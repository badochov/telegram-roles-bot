{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.HashMap.Internal.Strict (HashMap)
import qualified Data.HashMap.Internal.Strict as HashMap
import Data.Serialize (Serialize, decode, encode)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import System.AtomicWrite.Writer.ByteString
import System.Directory
import Telegram.Bot.API
import System.FilePath (joinPath)

type Role = Text

type Roles = HashMap Role (Set Mention)

data Model = Model
  { roles :: Roles,
    loadedFromDisk :: Bool
  }
  deriving (Generic)

tryLoadFromDisk :: ChatId -> Model -> IO Model
tryLoadFromDisk chatId model@Model {loadedFromDisk} =
  if loadedFromDisk
    then return model
    else loadFromDisk chatId

data Mention
  = Username Text
  | TelegramId UserId Text
  deriving (Eq, Generic, Show)

instance Ord Mention where
  (<=) (Username _) (TelegramId _ _) = True
  (<=) (TelegramId _ _) (Username _) = False
  (<=) (Username a) (Username b) = a <= b
  (<=) (TelegramId (UserId a) _) (TelegramId (UserId b) _) = a <= b

saveToDisk :: ChatId -> Model -> IO ()
saveToDisk chat model = do
  let serialized = serialize model
  let name = getFilePath chat
  atomicWriteFile name serialized

newtype MentionSerialized
  = MentionSerialized (String, Maybe Integer)
  deriving (Serialize)

newtype SerializedRoles = SerializedRoles [(String, [MentionSerialized])] deriving (Serialize)

serialize :: Model -> ByteString
serialize Model {roles} = encode $ toSerializedRoles roles
  where
    toSerializedRoles :: Roles -> SerializedRoles
    toSerializedRoles rs = SerializedRoles $ map (Data.Bifunctor.bimap unpack serializeMentions) $ HashMap.toList rs
    serializeMentions :: Set Mention -> [MentionSerialized]
    serializeMentions mentions = map serializeMention $ Set.toList mentions
    serializeMention :: Mention -> MentionSerialized
    serializeMention (Username name) = MentionSerialized (Data.Text.unpack name, Nothing)
    serializeMention (TelegramId (UserId uid) name) = MentionSerialized (Data.Text.unpack name, Just uid)

deserialize :: ByteString -> Either String Model
deserialize serialized = fromSerializedRoles <$> decode serialized
  where
    fromSerializedRoles :: SerializedRoles -> Model
    fromSerializedRoles (SerializedRoles sr) = Model {roles = roles, loadedFromDisk = True}
      where
        roles = HashMap.fromList $ map (Data.Bifunctor.bimap pack deserializeMentions) sr
        deserializeMentions ms = Set.fromList $ map deserializeMention ms
        deserializeMention :: MentionSerialized -> Mention
        deserializeMention (MentionSerialized (name, Nothing)) = Username $ Data.Text.pack name
        deserializeMention (MentionSerialized (name, Just tid)) = TelegramId (UserId tid) (Data.Text.pack name)

defaultModel :: Model
defaultModel =
  Model
    { roles = HashMap.empty,
      loadedFromDisk = True
    }

loadFromDisk :: ChatId -> IO Model
loadFromDisk chat = do
  let name = getFilePath chat
  exists <- doesFileExist name
  if exists
    then do
      ByteString.readFile name
        >>= ( \case
                Left err -> putStrLn err >> return defaultModel
                Right m -> return m
            )
          . deserialize
    else return defaultModel

getFilePath :: ChatId -> String
getFilePath cid = joinPath [serializedFolderName, getFileName cid]

serializedFolderName :: String
serializedFolderName = "serialized"

getFileName :: ChatId -> String
getFileName (ChatId cid) = show cid
