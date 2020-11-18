module FlashBlast.Subtitles where

import Dhall
import Dhall.Deriving
import Text.Subtitles.SRT
import qualified Data.Attoparsec.Text as A
import RIO
import Data.Either.Validation
import qualified RIO.Text as T

data SRT = SRT [Line]
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
