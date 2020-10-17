module FlashBlast.ClozeParse where

import RIO hiding (many)
import RIO.State
import qualified RIO.Text as T
import Replace.Megaparsec
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

bracevar :: ParsecT Void Text m Text
bracevar = between (string "{{") (string "}}") (T.pack <$> many (alphaNumChar <|> spaceChar <|> char '\'' <|> char '-' <|> char ','))

addClozeNumbers :: Text -> State Int Text
addClozeNumbers x = do
        i <- get
        let i' = i+1
        put i'
        pure $ "{{c" <> T.pack (show i') <> "::" <> x <> "}}"

genClozePhrase :: Text -> (Text, [Text])
genClozePhrase x = ( flip evalState 0 . streamEditT bracevar addClozeNumbers $ x
                   , fmap snd . rights . splitCap (match bracevar) $ x)
