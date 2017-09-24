module Main where

import ParsingOutFile
import Types
import qualified Data.ByteString.Lazy as B
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)


-- main :: IO ()
main = readOutFile "char_gunstig.out"

readOutFile :: String -> IO OutFile
readOutFile fn = fmap outFile (readFile fn)

saveParseToJSON :: IO OutFile -> String -> IO ()
saveParseToJSON x fn = ioStringToJSON x >>= B.writeFile fn

-- increments :: LoadNumber -> OutFile -> [Double]
-- increments l f =

getBlocks :: LoadNumber -> OutFile -> [Block]
getBlocks n = filter (\x -> n == loadNumber x) . blocks

getIncrements :: LoadNumber -> OutFile -> [Double]
getIncrements n f = map increment $ getBlocks n f

getTotal :: LoadNumber -> OutFile -> [Double]
getTotal n f = map total $ getBlocks n f

getSteps :: LoadNumber -> OutFile -> [Int]
getSteps n f = map step $ getBlocks n f

getLoadNumbers :: OutFile -> [Int]
getLoadNumbers f = map loadNumber $ blocks f

unique :: (Ord a) => [a] -> [a]
unique = foldr (\x acc -> if null acc || x > head acc then x : acc else acc) []

plotOutFile n f = toFile def "mychart.svg" $ do
    let numbers = getLoadNumbers f

    layout_title .= "Out logger"
    setColors [opaque blue, opaque red]
    plot $ line "increments" [zip (getSteps n f) (getTotal n f)]
