module Data  where

import Data.List.Split (keepDelimsL, oneOf, split)
import Text.Regex.Applicative (RE, Alternative(many), psym, sym) 

type Motif = String 

data Protein = Protein { 
    title :: String
  , aaseq :: String
  , motifs1 :: [Motif]
  , motifs2 :: [Motif]
}

-- pprint
instance Show Protein where
  show p = "Protein {\n"
            ++ "title = "     ++ show (title p) 
            ++ "\naaseq = "   ++ show (aaseq p) 
            ++ "\nmotifs1 = " ++ show (motifs1 p) 
            ++ "\nmotifs2 = " ++ show (motifs2 p) ++ "\n}"

splitProteins :: String -> [String]
splitProteins = split (keepDelimsL (oneOf ">"))

readTitle :: RE Char String
readTitle =  sym '>' *> many (psym (/= '\n')) <* sym '\n'

readSeq :: RE Char String
readSeq = many (psym (/= '>'))

parseProtein :: RE Char Protein
parseProtein = Protein <$> readTitle <*> readSeq <*> pure [] <*> pure []

cleanSeq :: Protein -> Protein
cleanSeq protein = protein { aaseq = filter (/= '\n') (aaseq protein) }

parseAndCleanProtein :: RE Char Protein
parseAndCleanProtein = cleanSeq <$> parseProtein

