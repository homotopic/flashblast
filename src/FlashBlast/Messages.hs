module FlashBlast.Messages where

import Formatting
import RIO
import RIO.Text

msgBuildingDeck :: Format r' (Text -> r')
msgBuildingDeck = "Building Deck: " % stext

msgDeckComplete :: Format r' (Text -> r')
msgDeckComplete = "Deck Complete: " % stext
