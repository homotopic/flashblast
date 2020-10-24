{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
module FlashBlast.AnkiDB where

import           Path
import           Polysemy
import           Polysemy.Reader
import           RIO                     hiding (Reader, asks)
import           UnliftIO.Path.Directory

data AnkiDB m a where
  CopyToCollections :: [Path Rel File] -> AnkiDB m ()

makeSem ''AnkiDB

newtype UserProfile = UserProfile { unUserProfile :: Path Abs Dir }

interpretAnkiDBIO :: (Member (Embed IO) effs,
                      Member (Reader UserProfile) effs) => Sem (AnkiDB ': effs) a -> Sem effs a
interpretAnkiDBIO = interpret $ \case
    CopyToCollections xs -> forM_ xs $ \x -> do
                               f <- asks unUserProfile
                               copyFile x (f </> $(mkRelDir "collections.media") </> filename x)



type AnkiMediaMap = Map Int (Path Rel File)

data AnkiCollectionsDatabase = AnkiCollectionsDatabase ()
  deriving (Eq, Show, Generic)

data CollectionsPackage = CollectionsPackage {
  media       :: AnkiMediaMap
, collections :: AnkiCollectionsDatabase
, filecontent :: [ByteString]
} deriving (Eq, Show, Generic)
