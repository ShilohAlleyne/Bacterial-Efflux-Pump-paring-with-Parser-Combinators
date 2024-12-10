module Type1 where

import Text.Regex.Applicative (Alternative(many), sym, anySym, RE, string)
import Data (Motif)

-- Type 1 motifs

fgfagr :: RE Char Motif
fgfagr = many anySym *> string "FGFAGR" <* many anySym

fxfaxr :: RE Char Motif
fxfaxr = 
    many anySym *> 
    ((\a b c d e -> a ++ b ++ c ++ d ++ e) <$> 
        (pure  <$> sym 'F') <*> 
        (pure  <$> anySym)  <*> 
        string "FA"         <*>
        (pure  <$> anySym)  <*> 
        (pure  <$> sym 'R'))
    <* many anySym

fgfxxr :: RE Char Motif
fgfxxr = 
    many anySym *>
    ((\a b c d -> a ++ b ++ c ++ d) <$>
        string "FGF"      <*>
        (pure <$> anySym) <*>
        (pure <$> anySym) <*>
        (pure <$> sym 'R'))
    <* many anySym

fgfxgx :: RE Char Motif
fgfxgx =
    many anySym *>
    ((\a b c d -> a ++ b ++ c ++ d) <$>
        string "FGF"       <*>
        (pure <$> anySym)  <*>
        (pure <$> sym 'G') <*>
        (pure <$> anySym))
    <* many anySym 

fgxxgr :: RE Char Motif
fgxxgr =
    many anySym *>
    ((\a b c d -> a ++ b ++ c ++ d) <$>
        string "FGF"      <*>
        (pure <$> anySym) <*>
        (pure <$> anySym) <*>
        string "GR")
    <* many anySym

fxfxxr :: RE Char Motif
fxfxxr =
    many anySym *> 
    ((\a b c d e f -> [a, b, c, d, e, f]) <$> 
        sym 'F' <*>
        anySym  <*>
        sym 'F' <*>
        anySym  <*>
        anySym  <*>
        sym 'R')
    <* many anySym

yxfaxr :: RE Char Motif
yxfaxr =
    many anySym *>
    ((\a b c d e -> a ++ b ++ c ++ d ++ e) <$>
        (pure <$> sym 'Y') <*>
        (pure <$> anySym)  <*>
        string "FA"        <*>
        (pure <$> anySym)  <*>
        (pure <$> sym 'R'))
    <* many anySym

ygfxxr :: RE Char Motif
ygfxxr =
    many anySym *>
    ((\a b c e -> a ++ b ++ c ++ e) <$>
        string "YGF"      <*>
        (pure <$> anySym) <*>
        (pure <$> anySym) <*>
        (pure <$> sym 'R'))
    <* many anySym

ygfxgx :: RE Char Motif
ygfxgx =
    many anySym *>
    ((\a b c d -> a ++ b ++ c ++ d) <$>
        string "YGF"       <*>
        (pure <$> anySym)  <*>
        (pure <$> sym 'G') <*>
        (pure <$> anySym))
    <* many anySym

ygxxgr :: RE Char Motif
ygxxgr =
    many anySym *>
    ((\a b c d -> a ++ b ++ c ++ d) <$>
        string "YG"        <*>
        (pure <$> anySym)  <*>
        (pure <$> anySym)  <*>
        string "GR")
    <* many anySym

yxfxxr :: RE Char Motif
yxfxxr =
    many anySym *>
    ((\a b c d e f -> [a, b, c, d, e, f]) <$>
        sym 'Y' <*>
        anySym  <*>
        sym 'F' <*>
        anySym  <*>
        anySym  <*>
        sym 'R')
    <* many anySym

fgyxxr :: RE Char Motif
fgyxxr =
    many anySym *>
    ((\a b c d -> a ++ b ++ c ++ d) <$>
        string "FGY" <*>
        (pure <$> anySym)  <*>
        (pure <$> anySym)  <*>
        (pure <$> sym 'R'))
    <* many anySym

fxyxxr :: RE Char Motif
fxyxxr =
    many anySym *>
    ((\a b c d e f -> [a, b, c, d, e, f]) <$>
        sym 'F'  <*>
        anySym   <*>
        sym 'Y'  <*>
        anySym   <*>
        anySym   <*>
        sym 'R')
    <* many anySym

type1Parsers :: [RE Char Motif]
type1Parsers = [fgfagr, fxfaxr, fgfxxr, fgfxgx, fgxxgr, fxfxxr, 
                yxfaxr, ygfxxr, ygfxgx, ygxxgr, yxfxxr, fgyxxr, fxyxxr]
