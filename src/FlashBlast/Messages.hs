module FlashBlast.Messages where

import Techlab
import qualified Techlab.Formatting as F

msgBuildingDeck :: F.Format r' (Text -> r')
msgBuildingDeck = "Building Deck: " F.% F.stext

msgDeckComplete :: F.Format r' (Text -> r')
msgDeckComplete = "Deck Complete: " F.% F.stext
