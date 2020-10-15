{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module FlashBlast.Conventions where

import Dhall hiding (maybe)
import Composite.Record
import Composite.TH
import Formatting
import qualified  RIO.Text as T
import qualified RIO.Text.Lazy as LT
import qualified RIO.Text.Partial as T
import RIO
import Path
import Path.Dhall()
import Path.Utils
import Lucid

data VF = Empty | Raw Text | Images [Path Rel File] | Audio (Path Rel File)
  deriving (Eq, Show ,Generic)

instance FromDhall VF

withLensesAndProxies [d|
  type FFront a     = "front" :-> a
  type FExtra a     = "extra" :-> a
  type FBack a      = "back"  :-> a
  type FFrom a      = "from" :-> a
  type FFromExtra a = "from-extra" :-> a
  type FTo a        = "to" :-> a
  type FToExtra a   = "to-extra" :-> a
  type FAudio1      = "audio1" :-> Maybe (Path Rel File)
  type FAudio2      = "audio2" :-> Maybe (Path Rel File)
  type FAudio3      = "audio3" :-> Maybe (Path Rel File)
  type FAudio4      = "audio4" :-> Maybe (Path Rel File)
  type FAudio5      = "audio5" :-> Maybe (Path Rel File)
  type FAudio6      = "audio6" :-> Maybe (Path Rel File)
  type FAudio7      = "audio7" :-> Maybe (Path Rel File)
  type FAudio8      = "audio8" :-> Maybe (Path Rel File)
  type FAudio9      = "audio9" :-> Maybe (Path Rel File)
  type FAudio10     = "audio10" :-> Maybe (Path Rel File)
  type FAudio11     = "audio11" :-> Maybe (Path Rel File)
  type FAudio12     = "audio12" :-> Maybe (Path Rel File)
  type FAudio13     = "audio13" :-> Maybe (Path Rel File)
  type FAudio14     = "audio14" :-> Maybe (Path Rel File)
  type FAudio15     = "audio15" :-> Maybe (Path Rel File)
  type FAudio16     = "audio16" :-> Maybe (Path Rel File)
  |]

type RBasicNote a b c = Record (FFront a : FExtra b : FBack c : '[])

type RBasicReversedNote a b c d = Record (FFrom a : FFromExtra b : FTo c : FToExtra d : '[])

type RMinimalNote a b = Record (FFrom a : FTo b : '[])

type RMultiAudioNote a b = Record (FFront a : FExtra b : FAudio1 : FAudio2 : FAudio3 : FAudio4 : FAudio5 : FAudio6 : FAudio7 : FAudio8 : FAudio9 : FAudio10 : FAudio11 : FAudio12 : FAudio13 : FAudio14 : FAudio15 : FAudio16 : '[])

type RExcerptNote = RBasicNote Text (Path Rel File) (Path Rel File)

type RForvoNote = RMultiAudioNote Text [Path Rel File]

type RMinimalNoteVF = RMinimalNote VF VF

type RBasicReversedNoteVF = RBasicReversedNote VF VF VF VF

forvoConvention :: MonadThrow m => Text -> Text -> m (Path Rel File)
forvoConvention locale word = parseRelFile . T.unpack $ sformat ("pronunciation_" % stext % "_" % stext % ".mp3") locale (T.replace " " "_" (T.toLower word))

ungroundedImage :: Path Rel File -> Html ()
ungroundedImage x = img_ [src_ $ toFilePathText x]

soundEmbed :: Path Rel File -> Text
soundEmbed = sformat ("[sound:" % stext % "]") . toFilePathText

-- TODO: do this as an interpretation
renderExcerptNote :: RExcerptNote -> Text
renderExcerptNote (a :*: b :*: c :*: RNil) = T.intercalate "\t" [a, LT.toStrict $ renderText $ ungroundedImage (filename b), soundEmbed c]

renderForvoNote :: RForvoNote -> Text
renderForvoNote (a :*: b :*: c :*: d :*: e :*: f :*: g :*: h :*: i :*: j :*: k :*: l :*: m :*: n :*: o :*: p :*: q :*: r :*: RNil)
  = T.intercalate "\t" $ [a, T.intercalate "\n" (LT.toStrict . renderText . ungroundedImage . filename <$> b)] ++ fmap (maybe "" soundEmbed) [c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r]

renderMinimalNoteVF :: RMinimalNoteVF -> Text
renderMinimalNoteVF (a :*: b :*: RNil) = T.intercalate "\t" $ renderVF <$> [a, b]

renderBasicReversedNoteVF :: RBasicReversedNoteVF -> Text
renderBasicReversedNoteVF (a :*: b :*: c :*: d :*: RNil) = T.intercalate "\t" $ renderVF <$> [a,b,c,d]

renderVF :: VF -> Text
renderVF Empty = ""
renderVF (Raw x) = x
renderVF (Images x) = T.intercalate "\n" $ LT.toStrict . renderText . ungroundedImage <$> x
renderVF (Audio x) = soundEmbed x
