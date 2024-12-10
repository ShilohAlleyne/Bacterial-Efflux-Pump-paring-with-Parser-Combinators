module Type2 where

import Text.Regex.Applicative (Alternative(many), anySym, RE, string, psym)
import Data (Motif)

-- Type 2 parser

xh :: RE Char Char
xh = psym (`elem` "AILMFVPG")

xp :: RE Char Char
xp = psym (`elem` "QNHSTYC")

vekssssf :: RE Char Motif
vekssssf =
    many anySym *>
    string "VEKSSSSF"
    <* many anySym

ssss :: RE Char Motif
ssss =
    many anySym *>
    string "SSSS"
    <* many anySym

ekssss :: RE Char Motif
ekssss =
    many anySym *>
    string "EKSSSS"
    <* many anySym

xxxssssx :: RE Char Motif
xxxssssx =
    many anySym *> 
    ((\a b c d e -> a ++ b ++ c ++ d ++ e) <$>
        (pure <$> anySym)  <*>
        (pure <$> anySym)  <*>
        (pure <$> anySym)  <*>
        string "SSSS"      <*>
        (pure <$> anySym))
    <* many anySym
    
xHxxxssssxxH :: RE Char Motif
xHxxxssssxxH =
    many anySym *>
    ((\a b c -> a ++ b ++ c) <$>
        (pure <$> xh) <*>
        xxxssssx      <*>
        (pure <$> xh))
    <* many anySym

xXPXPssssx :: RE Char Motif
xXPXPssssx =
    many anySym *>
    ((\a b c d e -> a ++ b ++ c ++ d ++ e) <$>
        (pure <$> anySym)  <*>
        (pure <$> xp)      <*>
        (pure <$> xp)      <*>
        string "SSSS"      <*>
        (pure <$> anySym))
    <* many anySym

xHXPXPSSSS :: RE Char Motif
xHXPXPSSSS =
    many anySym *>
    ((\a b c d e -> a ++ b ++ c ++ d ++ e) <$>
        (pure <$> xh) <*>
        (pure <$> xp) <*>
        (pure <$> xp) <*>
        string "SSSS" <*>
        (pure <$> xh))
    <* many anySym 

type2Parsers :: [RE Char Motif]
type2Parsers = [vekssssf, ssss, ekssss, xxxssssx, xHxxxssssxxH, xXPXPssssx, xHXPXPSSSS]
