{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module FlashBlast.Conventions where

import           Composite.Record
import           Composite.TH
import           Data.Align
import           FlashBlast.VF
import           Formatting
import           Lucid
import           Path
import           Path.Utils
import           RIO hiding (Const)
import           RIO.List.Partial
import qualified RIO.Text         as T
import qualified RIO.Text.Lazy    as LT
import Data.Vinyl.Functor hiding (Identity)
import Data.Vinyl
import Data.Vinyl.Class.Method

withLensesAndProxies [d|
  type FFront        = "front" :-> VF
  type FExtra        = "extra" :-> VFC
  type FBack         = "back"  :-> VF
  type FFrontExtra   = "front-extra" :-> VFC
  type FBackExtra    = "back-extra" :-> VFC
  type FAudio1       = "audio1" :-> VF
  type FAudio2       = "audio2" :-> VF
  type FAudio3       = "audio3" :-> VF
  type FAudio4       = "audio4" :-> VF
  type FAudio5       = "audio5" :-> VF
  type FAudio6       = "audio6" :-> VF
  type FAudio7       = "audio7" :-> VF
  type FAudio8       = "audio8" :-> VF
  type FAudio9       = "audio9" :-> VF
  type FAudio10      = "audio10" :-> VF
  type FAudio11      = "audio11" :-> VF
  type FAudio12      = "audio12" :-> VF
  type FAudio13      = "audio13" :-> VF
  type FAudio14      = "audio14" :-> VF
  type FAudio15      = "audio15" :-> VF
  type FAudio16      = "audio16" :-> VF
  |]

type RBasicNote = Record (FFront : FFrontExtra : FBack : FBackExtra : '[])

type RMinimalNote = Record (FFront : FBack : '[])

type RExcerptNote = Record (FFront : FExtra : FBack : '[] )

type RPronunciationNote = Record
  ( FFront   : FExtra
  : FAudio1  : FAudio2  : FAudio3  : FAudio4
  : FAudio5  : FAudio6  : FAudio7  : FAudio8
  : FAudio9  : FAudio10 : FAudio11 : FAudio12
  : FAudio13 : FAudio14 : FAudio15 : FAudio16
  : '[])

ungroundedImage :: Path Rel File -> Html ()
ungroundedImage x = img_ [src_ $ toFilePathText x]

soundEmbed :: Path Rel File -> Text
soundEmbed = sformat ("[sound:" % stext % "]") . toFilePathText

showFieldsCSV :: ( RecMapMethod RenderVF Identity ts
                 , RecordToList ts)
              => Record ts
              -> [Text]
showFieldsCSV = recordToList . rmapMethod @RenderVF aux
  where aux :: (RenderVF (PayloadType Identity a))
            => Identity a -> Const Text a
        aux (Identity x) = Const (renderVF x)

instance RenderVF a => RenderVF (s :-> a) where
  renderVF x = renderVF . getVal $ x

instance HasMedia a => HasMedia (s :-> a) where
  getMedia = getMedia . getVal

class RenderVF a where
  renderVF :: a -> Text

instance RenderVF VF where
  renderVF Blank       = ""
  renderVF (RawText x) = x
  renderVF (Image x)   = LT.toStrict . renderText . ungroundedImage $ x
  renderVF (Audio x)   = soundEmbed x
  renderVF (Video x)   = undefined

instance RenderVF VFC where
  renderVF (Single x) = renderVF x
  renderVF (Multi xs) = mconcat $ fmap renderVF xs

genForvos :: Text -> VFC -> [VF] -> RPronunciationNote
genForvos x zs ys' =
  let ys = lpadZipWith (const . fromMaybe Blank) ys' (replicate 16 ())
      ks = ys !! 0  :*: ys !! 1  :*: ys !! 2  :*: ys !! 3
       :*: ys !! 4  :*: ys !! 5  :*: ys !! 6  :*: ys !! 7
       :*: ys !! 8  :*: ys !! 9  :*: ys !! 10 :*: ys !! 11
       :*: ys !! 12 :*: ys !! 13 :*: ys !! 14 :*: ys !! 15
       :*: RNil
  in RawText x :*: zs :*: ks

renderNote :: ( RecMapMethod RenderVF Identity ts
              , RecordToList ts)
           => Record ts
           -> Text
renderNote = T.intercalate "\t" . showFieldsCSV

getRecordMedia :: ( RecMapMethod HasMedia Identity ts
                  , RecordToList ts)
               => Record ts
               -> [Path Rel File]
getRecordMedia = join . recordToList . rmapMethod @HasMedia aux where
  aux :: (HasMedia (PayloadType Identity a))
      => Identity a -> Const [Path Rel File] a
  aux (Identity x) = Const (getMedia x)

class HasMedia f where
  getMedia :: f -> [Path Rel File]

instance HasMedia VF where
  getMedia (Image x) = [x]
  getMedia (Audio x) = [x]
  getMedia (Video x) = [x]
  getMedia _ = []

instance HasMedia VFC where
  getMedia (Single x) = getMedia x
  getMedia (Multi xs) = foldMap getMedia xs
