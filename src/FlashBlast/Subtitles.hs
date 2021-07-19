module FlashBlast.Subtitles where

import qualified Data.Attoparsec.Text as A
import Data.Either.Validation
import Dhall
import Media.Subtitles.SRT
import Media.Subtitles.SRT.Attoparsec
import qualified Polysemy.Video as V
import RIO
import qualified RIO.Text as T

instance FromDhall SRT where
  autoWith options = srtDecoder options

srtDecoder :: InputNormalizer -> Decoder SRT
srtDecoder opts =
  Decoder {..}
  where
    textDecoder :: Decoder Text
    textDecoder = autoWith opts

    extract expression =
      case Dhall.extract textDecoder expression of
        Success x -> case A.parseOnly parseSRT x of
          Left exception -> Dhall.extractError (T.pack $ show exception)
          Right k -> Success k
        Failure e -> Failure e

    expected = Dhall.expected textDecoder
