{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module FlashBlast.Conventions where

import Composite.Record
import Composite.TH
import Data.Align
import Data.Vinyl
import Data.Vinyl.Class.Method
import Data.Vinyl.Functor hiding (Identity)
import FlashBlast.VF
import Formatting
import Lucid
import Media.VF
import Path
import Path.Utils
import RIO hiding (Const)
import RIO.List.Partial
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as LT

withLensesAndProxies
  [d|
    type FFront = "front" :-> VFRF

    type FExtra = "extra" :-> VFCRF

    type FBack = "back" :-> VFRF

    type FFrontExtra = "front-extra" :-> VFCRF

    type FBackExtra = "back-extra" :-> VFCRF

    type FAudio1 = "audio1" :-> VFRF

    type FAudio2 = "audio2" :-> VFRF

    type FAudio3 = "audio3" :-> VFRF

    type FAudio4 = "audio4" :-> VFRF

    type FAudio5 = "audio5" :-> VFRF

    type FAudio6 = "audio6" :-> VFRF

    type FAudio7 = "audio7" :-> VFRF

    type FAudio8 = "audio8" :-> VFRF

    type FAudio9 = "audio9" :-> VFRF

    type FAudio10 = "audio10" :-> VFRF

    type FAudio11 = "audio11" :-> VFRF

    type FAudio12 = "audio12" :-> VFRF

    type FAudio13 = "audio13" :-> VFRF

    type FAudio14 = "audio14" :-> VFRF

    type FAudio15 = "audio15" :-> VFRF

    type FAudio16 = "audio16" :-> VFRF
    |]

type RBasicNote = Record (FFront : FFrontExtra : FBack : FBackExtra : '[])

type RMinimalNote = Record (FFront : FBack : '[])

type RExcerptNote = Record (FFront : FExtra : FBack : '[])

type RPronunciationNote =
  Record
    ( FFront
        : FExtra
          : FAudio1
            : FAudio2
              : FAudio3
                : FAudio4
                  : FAudio5
                    : FAudio6
                      : FAudio7
                        : FAudio8
                          : FAudio9
                            : FAudio10
                              : FAudio11
                                : FAudio12
                                  : FAudio13
                                    : FAudio14
                                      : FAudio15
                                        : FAudio16
                                          : '[]
    )

ungroundedImage :: Path Rel File -> Html ()
ungroundedImage x = img_ [src_ $ toFilePathText x]

soundEmbed :: Path Rel File -> Text
soundEmbed = sformat ("[sound:" % stext % "]") . toFilePathText

showFieldsCSV ::
  ( RecMapMethod RenderVF Identity ts,
    RecordToList ts
  ) =>
  Record ts ->
  [Text]
showFieldsCSV = recordToList . rmapMethod @RenderVF aux
  where
    aux ::
      (RenderVF (PayloadType Identity a)) =>
      Identity a ->
      Const Text a
    aux (Identity x) = Const (renderVF x)

instance RenderVF a => RenderVF (s :-> a) where
  renderVF = renderVF . getVal

instance HasMedia x f => HasMedia x (s :-> f) where
  getMedia = getMedia . getVal

class RenderVF a where
  renderVF :: a -> Text

instance RenderVF VFRF where
  renderVF Blank = ""
  renderVF (RawText x) = x
  renderVF (Image x) = LT.toStrict . renderText . ungroundedImage $ x
  renderVF (Audio x) = soundEmbed x
  renderVF (Video x) = undefined

instance RenderVF VFCRF where
  renderVF (VFC xs) = mconcat $ fmap renderVF xs

genForvos :: Text -> VFCRF -> [VFRF] -> RPronunciationNote
genForvos x zs ys' =
  let ys = lpadZipWith (const . fromMaybe Blank) ys' (replicate 16 ())
      ks =
        ys !! 0 :*: ys !! 1 :*: ys !! 2 :*: ys !! 3
          :*: ys !! 4
          :*: ys !! 5
          :*: ys !! 6
          :*: ys !! 7
          :*: ys !! 8
          :*: ys !! 9
          :*: ys !! 10
          :*: ys !! 11
          :*: ys !! 12
          :*: ys !! 13
          :*: ys !! 14
          :*: ys !! 15
          :*: RNil
   in RawText x :*: zs :*: ks

renderNote ::
  ( RecMapMethod RenderVF Identity ts,
    RecordToList ts
  ) =>
  Record ts ->
  Text
renderNote = T.intercalate "\t" . showFieldsCSV

getRecordMedia ::
  forall x ts.
  ( RecMapMethod (HasMedia x) Identity ts,
    RecordToList ts
  ) =>
  Record ts ->
  [x]
getRecordMedia = join . recordToList . rmapMethod @(HasMedia x) aux
  where
    aux ::
      (HasMedia x (PayloadType Identity a)) =>
      Identity a ->
      Const [x] a
    aux (Identity x) = Const (getMedia x)

class HasMedia x f | f -> x where
  getMedia :: f -> [x]

instance HasMedia x (VF x) where
  getMedia (Image x) = [x]
  getMedia (Audio x) = [x]
  getMedia (Video x) = [x]
  getMedia _ = []

instance HasMedia x (VFC x) where
  getMedia (VFC xs) = foldMap getMedia xs
