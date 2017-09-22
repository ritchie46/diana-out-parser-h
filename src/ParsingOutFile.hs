
module ParsingOutFile

where
import Text.ParserCombinators.ReadP
import qualified ParsingPredicates as PP
import Control.Monad
import Control.Applicative ((<|>))
import Data.List.Split (splitOn)
import Data.List (isInfixOf)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy (ByteString)
import Types

ioStringToJSON :: ToJSON a => IO a -> IO ByteString
ioStringToJSON x = do
  st <- x
  return $ encode st

outFile :: String -> OutFile
outFile st = fst . last $ readP_to_S pBlocks st

pBlocks :: ReadP OutFile
pBlocks = do
  ef <- header
  bl <- many singleBlock
  return $ OutFile ef bl

header :: ReadP ExternalForces
header =  fmap (head . splitOn "SUM OF EXTERNAL LOADS") look >>= string
    >> PP.pLine --  SUM OF EXTERNAL LOADS:
    >> PP.pLine --  ======================
    >> PP.pLine --  LOADSET POSITION  TR  X       TR  Y       TR  Z       RO  X       RO  Y       RO  Z
    >> do
        ldsets <- many externalLoads

        fmap (head . splitOn "INITIATED:") look >>= string
        return ldsets

externalLoads ::ReadP (LoadNumber, [ExternalForce])
externalLoads = do
    loadNumber <- munch1 PP.whiteSpace >> munch PP.digit
    tr <- many (munch1 PP.whiteSpace >> munch1 PP.floatDot)
    satisfy PP.eol
    return (read loadNumber, map read tr)


singleBlock :: ReadP Block
singleBlock = do
    st <- look
    if "LOAD INCREMENT" `isInfixOf` st then do
        -- splitOn "appel" "dit is appelmoes"
        -- ["dit is ","moes"]
        fmap (head . splitOn "LOAD INCREMENT") look >>= string
        increment <- pLoadFactor <++ (munch PP.nonDigit >> munch PP.floatDot)
        fmap (head. splitOn "STEP") look >>= string
        munch PP.nonDigit
        step <- munch PP.digit

        fmap (head . splitOn "TOTAL LOAD FACTOR:") look >>= string
        (ldNr, total) <- totalLoad
        plastLog <- nonLinearLog "PLASTICITY LOGGING"
        crackLog <- nonLinearLog "CRACKING LOGGING"
        cumf <- pCumForce

        return (Block ldNr total (read increment) (read step)
                      plastLog crackLog cumf)
    else pfail

-- call with:
-- - "PLASTICITY LOGGING" -> [plast, prvPl, critic, plastNew, prvPlNew, criticNew]
-- - "CRACKING LOGGING" -> [crack, open, closed, active, inacti, arises, re-opens, closes]
nonLinearLog :: String -> ReadP LogValues
nonLinearLog logtype = look >>= (\x -> if logtype `isInfixOf` x then return x else fail "uhoh")
                  >> fmap (head . splitOn "TOTAL MODEL") look >>= string
                  >> do
                      plast <- pLoggingValue
                      prvPl <- pLoggingValue
                      critic <- pLoggingValue
                      plastNew <- pLoggingValue
                      prvPlNew <- pLoggingValue
                      criticNew <- pLoggingValue
                      return $ map read [plast, prvPl, critic, plastNew, prvPlNew, criticNew]

pLoggingValue :: ReadP String
pLoggingValue = munch PP.nonDigit >> munch PP.digit

-- Parses the following line
pLoadFactor :: ReadP String
pLoadFactor = munch PP.nonDigitOnLine >> munch PP.floatDot >> munch PP.nonDigitOnLine >> PP.isFloatDot

totalLoad :: ReadP (Int, Double)
totalLoad = do
    -- load number
    ld <- munch PP.nonDigitOnLine >> munch PP.digit
    total <- munch PP.nonDigitOnLine >> PP.isFloatDot
    return (read ld, read total)

pCumForce :: ReadP CumForces
pCumForce = look >>= (\x -> if "CUMULATIVE REACTION" `isInfixOf` x then return x else fail "")
            >> fmap (head . splitOn "CUMULATIVE REACTION") look >>= string
            >> munch PP.nonStartNumber >> do
            fx <- isDianaFloat
            munch PP.nonStartNumber
            fy <- isDianaFloat
            munch PP.nonStartNumber
            fz <- isDianaFloat
            return $ CumForces (fx, fy, fz)


dianaFloat :: Char -> Bool
dianaFloat x = PP.digit x || (x `elem` ".EeD-+^")

isDianaFloat :: ReadP (Maybe Double)
isDianaFloat = let replace 'D' = 'E'
                   replace x = x
               in  fmap (Just . read . map replace) (munch1 dianaFloat) <++ return Nothing
