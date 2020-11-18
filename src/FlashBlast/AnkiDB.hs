{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
module FlashBlast.AnkiDB where

import Data.Char
import Text.RawString.QQ
import RIO.ByteString.Lazy as LBS
import Data.Aeson
import Database.SQLite.Simple hiding (Error)
import           RIO
import GHC.TypeLits
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Codec.Archive.Zip
import Polysemy
import Polysemy.Error
import Polysemy.FS
import Path
import Data.List.Index
import qualified RIO.Map as Map
import Polysemy.Path
import Data.Aeson

data Note = Note {
  _id :: Int
, _guid :: Text
, _mid :: Int
, _mod :: Int
, _usn :: Int
, _tags :: Text
, _flds :: Text
, _sfld :: Text
, _csum :: Int
, _flags :: Int
, _data :: Text
} deriving (Eq, Show, Generic)

instance FromRow Note where
  fromRow = Note <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow Note where
  toRow (Note _id _guid _mid _mod _usn _tags _flds _sfld _csum _flags _data) =
    toRow (_id, _guid, _mid, _mod, _usn, _tags, _flds, _sfld, _csum, _flags, _data)


instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j, FromField k) =>
    FromRow (a,b,c,d,e,f,g,h,i,j,k ) where
    fromRow = (,,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field
                          <*> field <*> field <*> field <*> field <*> field <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j, FromField k, FromField l) =>
    FromRow (a,b,c,d,e,f,g,h,i,j,k,l) where
    fromRow = (,,,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field
                          <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j, FromField k, FromField l, FromField m) =>
    FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    fromRow = (,,,,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field
                          <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k) where
    toRow (a,b,c,d,e,f,g,h,i,j,k) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k, l) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l, ToField m)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l, toField m]

data Col = Col {
  _id :: Int
, _crt :: Int
, _mod :: Int
, _scm :: Int
, _ver :: Int
, _dty :: Int
, _usn :: Int
, _ls :: Int
, _conf :: Text
, _models :: Text
, _decks :: Text
, _dconf :: Text
, _tags :: Text
} deriving (Eq, Show, Generic)

instance FromRow Col where
  fromRow = Col <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow Col where
  toRow (Col _id _crt _mod _scm _ver _dty _usn _ls _conf _models _decks _dconf _tags) =
    toRow (_id, _crt, _mod, _scm, _ver, _dty, _usn, _ls, _conf, _models, _decks, _dconf, _tags)


data FSZip m a where
  ZipUp :: Path Rel Dir -> Path Rel File -> FSZip m ()

makeSem ''FSZip

runFSZip :: Members '[Embed IO] r => Sem (FSZip ': r) a -> Sem r a
runFSZip = interpret \case
  ZipUp d f -> embed $ createArchive (toFilePath f) (packDirRecur Deflate mkEntrySelector (toFilePath d))

writeMediaMapDir :: Members '[FSDir, Error SomeException, FSWrite, FSCopy] r => Path Rel Dir -> [Path Rel File] -> Sem r ()
writeMediaMapDir d f = do
  createDirectory d
  let a = toMediaLMap f
  forM_ (Map.toList a) $ \(k,v) -> do
    k' <- parseRelFile' $ show k
    copyFile v (d </> k')
  let a' = fmap filename a
  writeFileBS (d </> $(mkRelFile "media")) $ LBS.toStrict $ encode a

toMediaLMap :: [Path Rel File] -> Map Int (Path Rel File)
toMediaLMap xs = Map.fromList (imap (,) xs)

f :: Members '[Embed IO, FSZip, FSWrite, FSDir, FSCopy, Error SomeException] r
  => Path Rel Dir -> [Path Rel File] -> Path Rel File -> Sem r ()
f d xs k = do
  writeMediaMapDir d xs
  embed $ build d
  zipUp d k


data Field = Field {
  font  :: Text
, media :: [Text]
, name  :: Text
, ord   :: Int
, rtl   :: Bool
, size   :: Int
, sticky :: Bool
} deriving (Eq, Show, Generic)

createNotesSql = [r|CREATE TABLE IF NOT EXISTS notes
                    (id   INTEGER PRIMARY KEY
                    ,guid TEXT NOT NULL
                    )
                    |]

fieldSeparator :: Char
fieldSeparator = chr 0x1F

k = ['f', chr 0x1F, 'o', chr 0x1F, 'p']

ms = "{\"1590601318483\": {\"sortf\": 0, \"did\": 1605469695936, \"latexPre\": \"\\\\documentclass[12pt]{article}\\n\\\\special{papersize=3in,5in}\\n\\\\usepackage[utf8]{inputenc}\\n\\\\usepackage{amssymb,amsmath}\\n\\\\pagestyle{empty}\\n\\\\setlength{\\\\parindent}{0in}\\n\\\\begin{document}\\n\", \"latexPost\": \"\\\\end{document}\", \"mod\": 1605469976, \"usn\": 93, \"vers\": [], \"type\": 1, \"css\": \".card {\\n font-family: arial;\\n font-size: 20px;\\n text-align: center;\\n color: black;\\n background-color: white;\\n}\\n\\n.cloze {\\n font-weight: bold;\\n color: blue;\\n}\\n.nightMode .cloze {\\n color: lightblue;\\n}\", \"name\": \"MultiCloze\", \"flds\": [{\"name\": \"Text\", \"ord\": 0, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}, {\"name\": \"Extra\", \"ord\": 1, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}, {\"name\": \"Audio1\", \"ord\": 2, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}, {\"name\": \"Audio2\", \"ord\": 3, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}, {\"name\": \"Audio3\", \"ord\": 4, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}, {\"name\": \"Audio4\", \"ord\": 5, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}, {\"name\": \"Audio5\", \"ord\": 6, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}, {\"name\": \"Audio6\", \"ord\": 7, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}, {\"name\": \"Audio7\", \"ord\": 8, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}, {\"name\": \"Audio8\", \"ord\": 9, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}, {\"name\": \"Audio9\", \"ord\": 10, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}, {\"name\": \"Audio10\", \"ord\": 11, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}, {\"name\": \"Audio11\", \"ord\": 12, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}, {\"name\": \"Audio12\", \"ord\": 13, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}, {\"name\": \"Audio13\", \"ord\": 14, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}, {\"name\": \"Audio14\", \"ord\": 15, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}, {\"name\": \"Audio15\", \"ord\": 16, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}, {\"name\": \"Audio16\", \"ord\": 17, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}], \"tmpls\": [{\"name\": \"Cloze\", \"ord\": 0, \"qfmt\": \"{{cloze:Text}}\\n{{Extra}}\", \"afmt\": \"{{cloze:Text}}\\n\\n{{#c1}}{{Audio1}}{{/c1}}\\n{{#c2}}{{Audio2}}{{/c2}}\\n{{#c3}}{{Audio3}}{{/c3}}\\n{{#c4}}{{Audio4}}{{/c4}}\\n{{#c5}}{{Audio5}}{{/c5}}\\n{{#c6}}{{Audio6}}{{/c6}}\\n{{#c7}}{{Audio7}}{{/c7}}\\n{{#c8}}{{Audio8}}{{/c8}}\\n{{#c9}}{{Audio9}}{{/c9}}\", \"did\": null, \"bqfmt\": \"\", \"bafmt\": \"\"}], \"tags\": [], \"id\": \"1590601318483\", \"latexsvg\": false}, \"1601472169195\": {\"sortf\": 0, \"did\": 1, \"latexPre\": \"\\\\documentclass[12pt]{article}\\n\\\\special{papersize=3in,5in}\\n\\\\usepackage[utf8]{inputenc}\\n\\\\usepackage{amssymb,amsmath}\\n\\\\pagestyle{empty}\\n\\\\setlength{\\\\parindent}{0in}\\n\\\\begin{document}\\n\", \"latexPost\": \"\\\\end{document}\", \"mod\": 1605467930, \"usn\": 88, \"vers\": [], \"type\": 1, \"css\": \".card {\\n font-family: arial;\\n font-size: 20px;\\n text-align: center;\\n color: black;\\n background-color: white;\\n}\\n\\n.cloze {\\n font-weight: bold;\\n color: blue;\\n}\\n.nightMode .cloze {\\n color: lightblue;\\n}\", \"name\": \"Excerpt\", \"flds\": [{\"name\": \"Text\", \"ord\": 0, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}, {\"name\": \"Image\", \"ord\": 1, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}, {\"name\": \"Audio\", \"ord\": 2, \"sticky\": false, \"rtl\": false, \"font\": \"Liberation Sans\", \"size\": 20, \"media\": []}], \"tmpls\": [{\"name\": \"Cloze\", \"ord\": 0, \"qfmt\": \"{{cloze:Text}}\\n{{Image}}\", \"afmt\": \"{{clze:Text}}<br>\\n{{Image}}\\n{{Audio}}\", \"did\": null, \"bqfmt\": \"\", \"bafmt\": \"\"}], \"tags\": [], \"id\": \"1601472169195\"}}"

build :: Path Rel Dir -> IO ()
build d = do
  conn <- open $ toFilePath (d </> $(mkRelFile "collection.anki2"))
  execute_ conn "CREATE TABLE IF NOT EXISTS notes (id INTEGER PRIMARY KEY, guid TEXT NOT NULL, mid INTEGER NOT NULL, mod INTEGER NOT NULL, usn INTEGER NOT NULL, tags TEXT NOT NULL, flds TEXT NOT NULL, sfld INTEGER NOT NULL, csum INTEGER NOT NULL, flags INTEGER NOT NULL, data TEXT NOT NULL)"
  execute_ conn "CREATE TABLE IF NOT EXISTS col (id INTEGER PRIMARY KEY, crt INTEGER NOT NULL, mod INTEGER NOT NULL, scm INTEGER NOT NULL, ver INTEGER NOT NULL, dty INTEGER NOT NULL,usn INTEGER NOT NULL, ls INTEGER NOT NULL, conf TEXT NOT NULL, models TEXT NOT NULL, decks TEXT NOT NULL, dconf TEXT NOT NULL, tags TEXT NOT NULL)"
--  execute_ conn createNotesSql
  execute conn "INSERT INTO notes (id, guid, mid, mod, usn, tags, flds, sfld, csum, flags, data) VALUES (?,?,?,?,?,?,?,?,?,?,?)" (Note 1001 "fooo" 1601472169195 100 100 "ffs" k "f" 1432 2324 "sdas")
  execute conn "INSERT INTO col (id, crt, mod, scm, ver, dty, usn, ls, conf, models, decks, dconf, tags) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)" (Col 100 34234 3423423 43234 11 0 434 232 "{}" ms "{}" "{}" "{}")
  execute_ conn "CREATE TABLE IF NOT EXISTS cards (id INTEGER PRIMARY KEY, nid INTEGER NOT NULL, did INTEGER NOT NULL, ord INTEGER NOT NULL, mod INTEGER NOT NULL, usn INTEGER NOT NULL, type INTEGER NOT NULL, queue INTEGER NOT NULL, due INTEGER NOT NULL, ivl INTEGER NOT NULL, factor INTEGER NOT NULL, reps INTEGER NOT NULL, lapses INTEGER NOT NULL, left INTEGER NOT NULL, odue INTEGER NOT NULL, odid INTEGER NOT NULL, flags INTEGER NOT NULL, data TEXT NOT NULL)"
  close conn
