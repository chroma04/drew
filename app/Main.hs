{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Monad (void, when)
import Data.Default (Default (..))
import Data.Function ((&))
import qualified Data.HashMap.Lazy as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Text.Read (readMaybe)
import Text.Regex.PCRE ((=~))
import UnliftIO (liftIO)

data Command = Command
  { cmdDesc :: T.Text, -- what does it do?
    cmdRing :: [UserId], -- who can run it?
    cmdBody :: Message -> T.Text -> DiscordHandler ()
    -- FIXME: currently, a cmdBody takes both the message itself for context and the prefix/cmdName-less content
    -- ideally, i'd want it to accept [T.Text] of args. {echo} uses the full text so i can't just `words` it
  }

instance Default Command where
  def = Command {cmdDesc = "let chroma know if you see this because it means they forgot to set a description for this command LMAO", cmdRing = [], cmdBody = \_ _ -> return ()}

type Ring = [UserId]

chroma :: Ring
chroma = [616443866907672737]

-- FIXME: this is just. horrible.
commands :: M.HashMap T.Text Command
commands =
  M.fromList
    [ ( "die",
        def
          { cmdDesc = "ET TU, USER? - kills the bot",
            cmdRing = chroma,
            cmdBody = \m _ -> do
              restCall $ R.CreateMessage (m & messageChannel) "EXTINGUISHING MY OWN LIGHT WITH UTMOST HASTE."
              stopDiscord
          }
      ),
      ( "e621",
        def
          { cmdDesc = "YOU FILTHY THING - grabs a random image from a particular website. ||not real||",
            cmdBody = \m _ -> void $ restCall $ R.CreateReaction (m & messageChannel, m & messageId) "eyebrow1:868175239606071376"
          }
      ),
      ( "echo",
        def
          { cmdDesc = "MINDLESSLY ECHO YOUR WORDS - pretend to talk as the bot",
            cmdRing = chroma,
            cmdBody = \m t -> do
              restCall $
                R.CreateMessageDetailed (m & messageChannel) $
                  def
                    { R.messageDetailedContent = t,
                      R.messageDetailedAllowedMentions = Just $ def {R.mentionEveryone = False, R.mentionRepliedUser = False},
                      R.messageDetailedReference = referencedMessage m >>= \r -> return $ MessageReference (Just $ r & messageId) (Just $ r & messageChannel) (r & messageGuild) False
                    }
              void $ restCall $ R.DeleteMessage (m & messageChannel, m & messageId)
          }
      ),
      ( "info",
        def
          { cmdDesc = "WHAT I AM AND AM NOT - greeting with info about commands; [`cmd`] specfic info about `cmd`",
            cmdBody = \m t -> void $
              restCall $
                R.CreateMessage (m & messageChannel) $ case T.words t of
                  [] ->
                    "I AM A COLD FACSIMILE OF A REAL PERSON PURPOSED TO NOT HAVE A PURPOSE CREATED BY `<@" <> T.pack (show (head chroma))
                      <> ">`. HOW HUMBLING. CALL ME DREW.\n\nAVAILABLE COMMANDS: (USE `info` WITH ARGUMENT FOR SPECIFICS)\n"
                      <> T.unwords (T.cons '`' . (`T.snoc` '`') <$> M.keys (M.filter (null . cmdRing) commands))
                  [a0] -> maybe "`ERR: LOOKUP RETURNED NOTHING`" cmdDesc $ M.lookup a0 commands
                  _ -> "`ERR: ARITY MISMATCH (TOO FEW/MANY ARGS)`"
          }
      ),
      ( "hug",
        def
          { cmdDesc = "NOT AFFECTION; PURELY PRAGMATIC - embrace drew!; [`user_id`] hug a specific user",
            cmdBody = \m t -> do
              Right user <- restCall R.GetCurrentUser
              void $
                restCall $
                  R.CreateMessage (m & messageChannel) $ case T.words t of
                    [] -> "<@" <> T.pack (show (m & messageAuthor & userId)) <> "> *gives drew a gentle hug. drew tries his best to hide that they enjoy it a little*"
                    ["816534200073060352"] -> "*drew is immediately uncomfortable and takes no action, slowly backing away if anything*" -- gltile
                    [a0] ->
                      -- why can't i use guards outside of a function definition Haskell??
                      if a0 == T.pack (show (user & userId))
                        then "*drew awkwardly wraps his arms around themselves with discontent, proving that love of the self is often the most difficult*"
                        else
                          if isJust (readMaybe $ T.unpack a0 :: Maybe Snowflake)
                            then
                              "*drew gives <@" <> a0 <> "> a distant yet desperate hug. it would be nice if they weren't digging his claws into you*\n(REQUESTED BY "
                                <> (m & messageAuthor & userName)
                                <> "#"
                                <> (m & messageAuthor & userDiscrim)
                                <> ")"
                            else
                              "`ERR: CAN'T PARSE "
                                <> if (== '`') `T.any` a0
                                  then "THAT AS SNOWFLAKE. NOW STOP FUCKING AROUND WITH INPUT.`"
                                  else T.pack (show a0) <> " AS SNOWFLAKE`"
                    _ -> "`ERR: ARITY MISMATCH (TOO FEW/MANY ARGS)`"
          }
      )
    ]

startHandler :: DiscordHandler ()
startHandler = do
  liftIO $ putStrLn "ALIVE AND WELL. RELATIVELY."

  Right user <- restCall R.GetCurrentUser
  sendCommand $
    UpdateStatus $
      UpdateStatusOpts
        { updateStatusOptsSince = Nothing,
          updateStatusOptsGame =
            Just $
              Activity
                { activityName = "@" <> (user & userName) <> "#" <> (user & userDiscrim) <> " info",
                  activityType = ActivityTypeGame,
                  activityUrl = Nothing
                },
          updateStatusOptsNewStatus = UpdateStatusDoNotDisturb,
          updateStatusOptsAFK = False
        }

eventHandler :: Event -> DiscordHandler ()
eventHandler e = do
  Right user <- restCall R.GetCurrentUser
  case e of
    MessageCreate m -> do
      let text = m & messageText
          author = m & messageAuthor
          authorId = author & userId
          (prefix, cmdName, cmdArgs) = T.unpack text =~ ("\\s+\\S+($|\\s+)" :: String) :: (String, String, String)

      when (not (author & userIsBot) && not (null cmdName) && prefix =~ ("<@!?" <> show (user & userId) <> ">")) $ do
        let channel = m & messageChannel
        case T.filter (/= ' ') (T.pack cmdName) `M.lookup` commands of
          Just cmd ->
            if not (null ring) && notElem authorId ring
              then void $ restCall $ R.CreateMessage channel "THE SHEER HUBRIS TO NOT ONLY DISCOVER BUT THINK YOU CAN RUN THAT COMMAND. YOU DON'T DESERVE IT."
              else cmdBody cmd m $ T.pack cmdArgs
            where
              ring = cmd & cmdRing
          Nothing -> void $ restCall $ R.CreateReaction (channel, m & messageId) "question"
    MessageReactionAdd r -> do
      let threshold = 15
          pinEmoji = "pushpin"
      when ((r & reactionEmoji & emojiName) == pinEmoji) $ do
        let ctx = (r & reactionChannelId, r & reactionMessageId)
        Right users <- restCall $ R.GetReactions ctx pinEmoji (threshold, R.LatestReaction)
        when (length users >= threshold) $ void $ restCall $ R.AddPinnedMessage ctx
    _ -> return ()

main :: IO ()
main = seq commands $ do
  -- plaintext file in drew/ named "token" with bot token
  token <- TIO.readFile "token"
  text <-
    runDiscord $
      def
        { discordToken = token,
          discordOnStart = startHandler,
          discordOnEvent = eventHandler,
          discordOnLog = TIO.putStrLn
        }
  TIO.putStrLn text
