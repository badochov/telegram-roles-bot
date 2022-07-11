-- TODO improve role_list
-- TODO /help@niepowazne_reakcje_bot
-- TODO print ppl in role after role_add / role_remove
-- TODO role_alias
-- TODO always mention(not only if its first and only word
-- TODO cooldown
-- TODO investigate perf issues
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative
import Control.Monad.Reader
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text
import Data.Text.Lazy (toStrict)
import GHC.Generics (Generic)
import Lib
import Secret (botKey)
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser
import Text.Pretty.Simple (pShowNoColor)

data Action
  = Help
  | AddRole (Text, [MessageEntity])
  | RemoveRole (Text, [MessageEntity])
  | DeleteRole Text
  | CreateRole Text
  | ListRoles
  | Mention (Text, MessageId)
  deriving (Show)

newtype ReplyMessageM = ReplyMessageM (Maybe ReplyMessage)

newtype TextM = TextM (Maybe Text)

instance GetAction ReplyMessageM a where
  getNextAction effect = getNextAction do
    (ReplyMessageM t) <- effect
    case t of
      Nothing -> pure ()
      Just m -> reply m

instance GetAction TextM a where
  getNextAction effect = getNextAction do
    (TextM t) <- effect
    case t of
      Nothing -> pure ()
      Just txt -> replyText txt

initialModel :: Model
initialModel =
  Model
    { roles = HashMap.empty,
      loadedFromDisk = False
    }

toInt :: UserId -> Int
toInt (UserId uid) = fromInteger uid

entities :: UpdateParser [MessageEntity]
entities = UpdateParser (updateMessage >=> messageEntities)

messageChatId :: UpdateParser ChatId
messageChatId = UpdateParser (updateMessage >=> (Just . chatId . messageChat))

command' :: Text -> UpdateParser (Text, [MessageEntity])
command' name = do
  t <- text
  ents <- entities
  case Data.Text.words t of
    (w : _)
      | w == "/" <> name ->
        pure (t, ents)
    _ -> fail "not that command"

messageId :: UpdateParser MessageId
messageId = UpdateParser (updateMessage >=> Just . messageMessageId)

mention :: UpdateParser (Text, MessageId)
mention = do
  t <- text
  mid <- messageId
  if "@" `Data.Text.isPrefixOf` t && length (Data.Text.words t) == 1
    then pure (t, mid)
    else fail "not mention"

addChatId :: UpdateParser a -> UpdateParser (ChatId, a)
addChatId m = m >>= (\a -> fmap (,a) messageChatId)

rolesBot :: BotApp (IO Model) (ChatId, Action)
rolesBot =
  BotApp
    { botInitialModel = return initialModel,
      botAction = flip updateToAction,
      botHandler = handleAction,
      botJobs = []
    }
  where
    updateToAction :: IO Model -> Update -> Maybe (ChatId, Action)
    updateToAction _ =
      parseUpdate $
        addChatId $
          Mention <$> mention
            <|> Help <$ command "help"
            <|> AddRole <$> command' "role_add"
            <|> RemoveRole <$> command' "role_remove"
            <|> CreateRole <$> command "role_create"
            <|> DeleteRole <$> command "role_delete"
            <|> ListRoles <$ command "role_list"

    handleAction :: (ChatId, Action) -> IO Model -> Eff (ChatId, Action) (IO Model)
    handleAction (chatId, action) model =
      let m = model >>= tryLoadFromDisk chatId
       in do
            case action of
              Help ->
                m <# replyText helpMessage
              AddRole (msg, ents) -> handleAddRole msg ents m
              RemoveRole (msg, ents) -> handleRemoveRole msg ents m
              CreateRole msg -> handleCreateRole msg m
              DeleteRole msg -> handleDeleteRole msg m
              ListRoles -> handleListRoles m
              Mention (t, mid) ->
                m <# do
                  handleMention t mid m
      where
        handleAddRole :: Text -> [MessageEntity] -> IO Model -> Eff (ChatId, Action) (IO Model)
        handleAddRole msg ents m = saveModel newModel <# botM
          where
            validation = validateAddRole msg <$> m
            newModel = validation >>= newModelF
            newModelF Nothing = addRole msg ents <$> m
            newModelF (Just _) = m
            botM = liftIO $ TextM <$> validation
        handleRemoveRole :: Text -> [MessageEntity] -> IO Model -> Eff (ChatId, Action) (IO Model)
        handleRemoveRole msg ents m = saveModel newModel <# botM
          where
            validation = validateRemoveRole msg <$> m
            newModel = validation >>= newModelF
            newModelF Nothing = removeRole msg ents <$> m
            newModelF (Just _) = m
            botM = liftIO $ TextM <$> validation
        handleCreateRole :: Text -> IO Model -> Eff (ChatId, Action) (IO Model)
        handleCreateRole msg m = saveModel newModel <# botM
          where
            validation = validateCreateRole msg <$> m
            newModel = validation >>= newModelF
            newModelF Nothing = createRole msg <$> m
            newModelF (Just _) = m
            botM = liftIO $ TextM <$> validation
        handleDeleteRole :: Text -> IO Model -> Eff (ChatId, Action) (IO Model)
        handleDeleteRole msg m = saveModel newModel <# botM
          where
            validation = validateDeleteRole msg <$> m
            newModel = validation >>= newModelF
            newModelF Nothing = deleteRole msg <$> m
            newModelF (Just _) = m
            botM = liftIO $ TextM <$> validation
        handleListRoles :: IO Model -> Eff (ChatId, Action) (IO Model)
        handleListRoles m = m <# liftIO listRoleMsg
          where
            listRoleMsg = Data.Text.Lazy.toStrict . pShowNoColor . roles <$> m
        saveModel :: IO Model -> IO Model
        saveModel m = m >>= saveToDisk chatId >> m
        helpMessage =
          Data.Text.unlines
            [ "This bot brings roles features to your Telegram chat!",
              "",
              "`/role_create <role_name>*` creates roles",
              "`/role_delete <role_name>*` deletes roles",
              "`/role_add <role_name> <mention>*` adds role to the mentioned users",
              "`/role_remove <role_name> <mention>*` removes role from the mentioned users",
              "`/role_list` list roles and assigned people"
            ]
        handleMention :: Text -> MessageId -> IO Model -> BotM ReplyMessageM
        handleMention t mid mM =
          liftIO $
            ( \m -> ReplyMessageM $ case HashMap.lookup (getMention t) (roles m) of
                Just users -> Just $ createMsg users
                Nothing -> Nothing
            )
              <$> mM
          where
            createMsg users =
              let (msgText, ents) = createMsgTextAndEntities users
               in ReplyMessage msgText Nothing (Just ents) Nothing Nothing Nothing (Just mid) Nothing Nothing
            createMsgTextAndEntities users = Set.foldl createMsg' (Data.Text.empty, []) users
            createMsg' :: (Text, [MessageEntity]) -> Mention -> (Text, [MessageEntity])
            createMsg' (txt, e) (Username username) = (addToBack txt ("@" `Data.Text.append` username), e)
            createMsg' (txt, e) u@(TelegramId _ name) =
              let offset = Data.Text.length txt + 1
                  nT = addToBack txt name
                  ent = createEntity offset u
               in (nT, ent : e)
            createEntity offset u@(TelegramId _ name) =
              let len = Data.Text.length name
               in MessageEntity MessageEntityTextMention offset len Nothing (Just $ createUser u) Nothing
            createUser (TelegramId telegramId name) = User telegramId False name Nothing Nothing Nothing Nothing Nothing Nothing
            addToBack start back = Data.Text.intercalate (Data.Text.pack " ") [start, back]
            getMention = Data.Text.drop 1 . head . Data.Text.words

validateAddRole :: Text -> Model -> Maybe Text
validateAddRole t Model {roles} =
  let role = getRole t
   in if HashMap.member role roles
        then Nothing
        else Just (Data.Text.intercalate Data.Text.empty [Data.Text.pack "Role: `", role, Data.Text.pack "` doesn't exist!"])

validateCreateRole :: Text -> Model -> Maybe Text
validateCreateRole t Model {roles} =
  let parts = Data.Text.words t
      checkIfExists acc name =
        if HashMap.member name roles
          then Data.Text.intercalate Data.Text.empty [Data.Text.pack "Role: `", name, Data.Text.pack "` exists!"] : acc
          else acc
      errors = foldl checkIfExists [] parts
   in case errors of
        [] -> Nothing
        _ -> Just $ Data.Text.unlines errors

validateDeleteRole :: Text -> Model -> Maybe Text
validateDeleteRole t Model {roles} =
  let parts = Data.Text.words t
      checkIfExists acc name =
        if HashMap.member name roles
          then acc
          else Data.Text.intercalate Data.Text.empty [Data.Text.pack "Role: `", name, Data.Text.pack "` doesn't exists!"] : acc
      errors = foldl checkIfExists [] parts
   in case errors of
        [] -> Nothing
        _ -> Just $ Data.Text.unlines errors

validateRemoveRole :: Text -> Model -> Maybe Text
validateRemoveRole = validateAddRole

getRole :: Text -> Text
getRole = head . tail . Data.Text.words

addRole :: Text -> [MessageEntity] -> Model -> Model
addRole msg ents =
  let users = parseMentions msg ents
      role = getRole msg
   in addToRole role users

removeRole :: Text -> [MessageEntity] -> Model -> Model
removeRole msg ents =
  let users = parseMentions msg ents
      role = getRole msg
   in removeFromRole role users

createRole :: Text -> Model -> Model
createRole rolesMsg model =
  let parts = Data.Text.words rolesMsg
      addPart m@Model {roles} name = m {roles = HashMap.insertWith (\_ x -> x) name Set.empty roles}
   in foldl addPart model parts

deleteRole :: Text -> Model -> Model
deleteRole rolesMsg model =
  let parts = Data.Text.words rolesMsg
      removePart m@Model {roles} name = m {roles = HashMap.delete name roles}
   in foldl removePart model parts

addToRole :: Role -> [Mention] -> Model -> Model
addToRole role users model@Model {roles} =
  case HashMap.lookup role roles of
    Nothing -> model
    Just prev ->
      let updated = foldl (flip Set.insert) prev users
       in model {roles = HashMap.insert role updated roles}

removeFromRole :: Role -> [Mention] -> Model -> Model
removeFromRole _ [] model = model
removeFromRole role (u : t) m@Model {roles} =
  removeFromRole role t model
  where
    model = m {roles = newRoles}
    newRoles = HashMap.alter alterFn role roles
    alterFn Nothing = Nothing
    alterFn (Just x) =
      let r = Set.delete u x
       in if Set.null r then Nothing else Just r

parseMentions :: Text -> [MessageEntity] -> [Mention]
parseMentions msg =
  foldl handleEnt []
  where
    handleEnt acc ent =
      case messageEntityType ent of
        MessageEntityMention -> Username username : acc
        MessageEntityTextMention ->
          case messageEntityUser ent of
            Nothing -> acc
            Just user -> TelegramId (userId user) username : acc
        _ -> acc
      where
        offset = messageEntityOffset ent + 1 -- +1 to not include @
        len = messageEntityLength ent
        username = substring offset len msg

substring :: Int -> Int -> Text -> Text
substring offset len = Data.Text.take len . Data.Text.drop offset

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId rolesBot) env

main :: IO ()
main = do
  createSerializedFolder
  run botKey
