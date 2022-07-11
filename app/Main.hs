{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad.Reader
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text
import GHC.Generics (Generic)
import Secret (botKey)
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

type Role = Text

data Mention
  = Username Text
  | TelegramId UserId Text
  deriving (Eq, Generic, Show)

instance Ord Mention where
  (<=) (Username _) (TelegramId _ _) = True
  (<=) (TelegramId _ _) (Username _) = False
  (<=) (Username a) (Username b) = a <= b
  (<=) (TelegramId (UserId a) _) (TelegramId (UserId b) _) = a <= b

newtype Model = Model
  { roles :: HashMap Role (Set Mention)
  }

initialModel :: Model
initialModel =
  Model
    { roles = HashMap.empty
    }

data Action
  = Help
  | AddRole (Text, [MessageEntity])
  | RemoveRole (Text, [MessageEntity])
  | CreateRole Text
  | ListRoles
  | Mention (Text,MessageId)
  deriving (Show)

toInt :: UserId -> Int
toInt (UserId uid) = fromInteger uid

entities :: UpdateParser [MessageEntity]
entities = UpdateParser (updateMessage >=> messageEntities)

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
messageId = UpdateParser (updateMessage >=> mmId)
  where
  mmId :: Message -> Maybe MessageId
  mmId = Just . messageMessageId

mention :: UpdateParser (Text, MessageId)
mention = do
  t <- text
  mid <- messageId
  if "@" `Data.Text.isPrefixOf` t && length (Data.Text.words t) == 1
    then pure (t, mid)
    else fail "not mention"

rolesBot :: BotApp Model Action
rolesBot =
  BotApp
    { botInitialModel = initialModel,
      botAction = flip updateToAction,
      botHandler = handleAction,
      botJobs = []
    }
  where
    updateToAction :: Model -> Update -> Maybe Action
    updateToAction _ =
      parseUpdate $
        Mention <$> mention
          <|> Help <$ command "help"
          <|> AddRole <$> command' "role_add"
          <|> RemoveRole <$> command' "role_remove"
          <|> CreateRole <$> command "role_create"
          <|> ListRoles <$ command "role_list"

    handleAction :: Action -> Model -> Eff Action Model
    handleAction action model = case action of
      Help ->
        model <# do
          replyText helpMessage
      AddRole (msg, ents) ->
        case validateAddRole msg model of
          Nothing -> addRole msg ents model <# pure ()
          Just err -> model <# replyText err
      RemoveRole (msg, ents) ->
        case validateRemoveRole msg model of
          Nothing -> removeRole msg ents model <# pure ()
          Just err -> model <# replyText err
      CreateRole msg ->
        case validateCreateRole msg model of
          Nothing -> createRole msg model <# pure ()
          Just err -> model <# replyText err
      ListRoles ->
        model <# do
          replyText $ Data.Text.pack (show $ roles model)
      Mention (t, mid) ->
        model <# do
          handleMention t mid model

    helpMessage =
      Data.Text.unlines
        []

handleMention :: Text -> MessageId -> Model -> BotM ()
handleMention t mid Model {roles} =
  case HashMap.lookup (getMention t) roles of
    Just users -> reply $ createMsg users
    Nothing -> pure ()
  where
    createMsg users =
      let (msgText, ents) = createMsgTextAndEntities users
       in ReplyMessage msgText Nothing (Just ents) Nothing Nothing Nothing (Just mid) Nothing Nothing
    createMsgTextAndEntities users = Set.foldl createMsg' (Data.Text.empty, []) users
    createMsg' :: (Text, [MessageEntity]) -> Mention -> (Text, [MessageEntity])
    createMsg' (txt, e) (Username username) = (addToBack txt username, e)
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
  in
    case errors of
      [] -> Nothing
      _ -> Just $ Data.Text.intercalate (Data.Text.pack "\n") errors

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
removeRole msg ents model =
  let users = parseMentions msg ents
      role = getRole msg
   in removeFromRole role users model

createRole :: Text -> Model -> Model
createRole rolesMsg model =
  let parts = Data.Text.words rolesMsg
      addPart Model {roles} name = Model {roles = HashMap.insertWith (\_ x -> x) name Set.empty roles}
   in foldl addPart model parts

addToRole :: Role -> [Mention] -> Model -> Model
addToRole role users model@Model {roles} =
  case HashMap.lookup role roles of
    Nothing -> model
    Just prev ->
      let updated = foldl (flip Set.insert) prev users
       in Model {roles = HashMap.insert role updated roles}

removeFromRole :: Role -> [Mention] -> Model -> Model
removeFromRole _ [] model = model
removeFromRole role (u : t) Model {roles} =
  removeFromRole role t model
  where
    model = Model {roles = HashMap.alter alterFn role roles}
    alterFn Nothing = Nothing
    alterFn (Just x) = Just $ Set.delete u x

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
        offset = messageEntityOffset ent
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
  run botKey