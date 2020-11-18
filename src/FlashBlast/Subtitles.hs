module FlashBlast.Subtitles where

import Dhall
import Text.Subtitles.SRT
import qualified Data.Attoparsec.Text as A
import RIO
import Data.Either.Validation
import qualified RIO.Text as T
import qualified Polysemy.Video as V

newtype SRT = SRT [Line]
  deriving (Eq, Show, Ord)

instance FromDhall SRT where
  autoWith options = srtDecoder options

srtDecoder :: InputNormalizer -> Decoder SRT
srtDecoder opts =
      Decoder
            { extract = extractSrt
            , expected = expectedSrt
            }
      where
        textDecoder :: Decoder Text
        textDecoder = autoWith opts

        extractSrt expression =
          case extract textDecoder expression of
              Success x -> case A.parseOnly parseSRT x of
                Left exception   -> Dhall.extractError (T.pack $ show exception)
                Right path       -> Success (SRT path)
              Failure e        -> Failure e

        expectedSrt = expected textDecoder

fromTime :: Time -> V.Time
fromTime (Time h m s f) = V.Time h m s f

fromRange :: Range -> V.Range
fromRange (Range f t) = V.Range (fromTime f) (fromTime t)
