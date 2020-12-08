module FlashBlast.Subtitles where

import qualified Dhall as D
import Text.Subtitles.SRT
import qualified Data.Attoparsec.Text as A
import Data.Either.Validation
import qualified Data.Text as T
import Techlab
import qualified Polysemy.Video as V

newtype SRT = SRT [Line]
  deriving (Eq, Show, Ord)

instance D.FromDhall SRT where
  autoWith options = srtDecoder options

srtDecoder :: D.InputNormalizer -> D.Decoder SRT
srtDecoder opts =
      D.Decoder
            { extract = extractSrt
            , expected = expectedSrt
            }
      where
        textDecoder :: D.Decoder Text
        textDecoder = D.autoWith opts

        extractSrt expression =
          case D.extract textDecoder expression of
              Success x -> case A.parseOnly parseSRT x of
                Left exception   -> D.extractError (T.pack $ show exception)
                Right path       -> Success (SRT path)
              Failure e        -> Failure e

        expectedSrt = D.expected textDecoder

fromTime :: Time -> V.Time
fromTime (Time h m s f) = V.Time h m s f

fromRange :: Range -> V.Range
fromRange (Range f t) = V.Range (fromTime f) (fromTime t)
