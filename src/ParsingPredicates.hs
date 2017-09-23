module ParsingPredicates
where
import Text.ParserCombinators.ReadP

vowel :: Char -> Bool
vowel = (`elem` "aouei")

digit :: Char -> Bool
digit = (`elem` ['0'..'9'])

nonDigit :: Char -> Bool
nonDigit = not . digit

eol = ('\n' ==)

nonEol :: Char -> Bool
nonEol = not . eol

digitOnLine :: Char -> Bool
digitOnLine x = digit x && not (eol x)

nonDigitOnLine :: Char -> Bool
nonDigitOnLine x = nonDigit x && not (eol x)

alphabeticUpper :: Char -> Bool
alphabeticUpper = (`elem` ['A'..'Z'])

alphabeticLower :: Char -> Bool
alphabeticLower = (`elem` ['a'..'z'])

alphabetic :: Char -> Bool
alphabetic x = alphabeticUpper x || alphabeticLower x

notAlphabetic :: Char -> Bool
notAlphabetic = not. alphabetic

floatComma :: Char -> Bool
floatComma x = (x `elem` ['0'..'9']) || (x `elem` ",Ee-+^")

floatDot :: Char -> Bool
floatDot x = (x `elem` ['0'..'9']) || (x `elem` ".Ee-+^")

isFloatDot = do
    s <- munch floatDot
    if null s then pfail
        else return s

nummeric :: Char -> Bool
nummeric x = digit x || (x `elem` ",.Ee-+^")

notNummeric :: Char -> Bool
notNummeric = not . nummeric

startNumber :: Char -> Bool
startNumber x = digit x || (x `elem` "-+")

nonStartNumber = not . startNumber

whiteSpace :: Char -> Bool
whiteSpace = (`elem` " \t")

pLine :: ReadP String
pLine = fmap (++) (munch nonEol) <*> readPCharToString (satisfy eol)
  where
    readPCharToString = fmap (:[])
