{-# LANGUAGE TemplateHaskell #-}
module FlashBlast.YouTubeDL where

import Polysemy
import RIO
import Path
import qualified Turtle as S
import Path.Utils


data YouTubeDL m a where
  YouTubeDL' :: Text -> Path Rel File -> Text -> YouTubeDL m ()

makeSem ''YouTubeDL

interpretYouTubeDL :: Member (Embed IO) effs => Sem (YouTubeDL ': effs) a -> Sem effs a
interpretYouTubeDL = interpret \case
  YouTubeDL' x k f -> S.sh $ S.inproc "youtube-dl" [x, "-o", toFilePathText k, "-f", f] mempty
