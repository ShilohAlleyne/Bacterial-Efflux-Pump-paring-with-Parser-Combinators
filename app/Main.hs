module Main where

import Text.Regex.Applicative (match, RE)
import Data.Maybe (catMaybes)
import Data.List (isInfixOf, delete)
import Data (Protein (aaseq, motifs1, motifs2), Motif, splitProteins, parseAndCleanProtein)
import Type1 (type1Parsers)
import Type2 (type2Parsers)

import qualified Data.Set as S

parseMotifs' :: [RE Char Motif] -> String -> [Motif]
parseMotifs' combinators input = catMaybes [match c input | c <- combinators]

parseMotifs :: Protein -> [RE Char Motif] -> [RE Char Motif] -> Protein
parseMotifs protein parsers1 parsers2 = protein {
    motifs1 = S.toList . S.fromList $ parseMotifs' parsers1 (aaseq protein),
    motifs2 = S.toList . S.fromList $ parseMotifs' parsers2 (aaseq protein)}

-- filters protiens with motifs
filterProteins :: [Protein] -> [Protein]
filterProteins = filter (\p -> (not . null . motifs1) p || (not . null . motifs2) p)

filterMotifs :: [Motif] -> [Motif]
filterMotifs xs = filter (not. (`isSubset` xs)) xs 
    where
        isSubset motif motifs = any (\x -> motif `isInfixOf` x) $ delete motif motifs

-- filters subset motifs from proteins
bestMotifs :: Protein -> Protein
bestMotifs p = p {motifs1 = filterMotifs (motifs1 p), motifs2 = filterMotifs (motifs2 p)}

main :: IO ()
main = do

    contents <- readFile "C:/Users/yido6/Documents/Uni/Level 6/Final Project/Bacterial Efflux Pumps Bioinformatics Investigations/Proteomes/E.coli K12 Sample Proteome/uniprot-proteome_UP000000625.fasta" --"data.txt"
    let proteins = tail $ splitProteins contents
        cleanProteins = map (match parseAndCleanProtein) proteins
        searchedProteins = map (\x -> parseMotifs x type1Parsers type2Parsers) $ catMaybes cleanProteins
        filteredProteins = filterProteins searchedProteins
        filteredMotifs = map bestMotifs filteredProteins
    mapM_ print filteredMotifs
    print (length cleanProteins)
    print (length filteredMotifs)
