module FlashBlast.ClozeParse where

import           Replace.Megaparsec
import           Techlab
import qualified Data.Text as T
import           Text.Megaparsec      hiding (State)
import           Text.Megaparsec.Char

bracevar :: ParsecT Void Text m Text
bracevar = between (string "{{") (string "}}") (T.pack <$> many (alphaNumChar <|> spaceChar <|> char '\'' <|> char '-' <|> char ','))

addClozeNumbers :: Text -> Sem '[State Int] Text
addClozeNumbers x = do
        i <- get
        let i' = i+1
        put i'
        pure $ "{{c" <> T.pack (show i') <> "::" <> x <> "}}"

genClozePhrase :: Text -> (Text, [Text])
genClozePhrase x = ( run $ evalState 0 . streamEditT bracevar addClozeNumbers $ x
                   , fmap snd . rights . splitCap (match bracevar) $ x)
