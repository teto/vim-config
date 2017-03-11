{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import Options.Applicative
  -- (Parser, ParserInfo)
import qualified Data.Semigroup  as SG

import System.IO
import Data.Maybe (catMaybes)
import Data.List (isInfixOf)
-- import Options.Applicative
-- import Data.Semigroup ((<>))
-- import Control.Applicative
-- import Data.Csv
import qualified Data.Vector as V
-- bytestring
-- import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.UTF8 as UTF8
-- data Person = Person
--     { name   :: !String
--     , salary :: !Int
--     }
import qualified Data.Csv as Cassava

-- text
-- import Data.Text (Text)
-- import qualified Data.Text.Encoding as Text

-- vector
-- import Data.Vector (Vector)
import qualified Data.Vector as Vector


-- deriving implements automatically the obvious functions (show for Show , Eq for Eq etc)
data OptionRecord = OptionRecord { name :: !String, abbr :: !String, desc :: !String } deriving (Eq, Show)

-- To declare an instance of FromNamedRecord, we need to implement the parseNamedRecord function, which takes a map of names to fields and returns a parser of Item. The (.:) operator is a lookup operator, so m .: "Item" means that we look up a field with name Item in the map m. If there's such a field in the map, we use it as the first argument of the Item constructor, that is, we assign it to the itemName field
instance Cassava.FromNamedRecord OptionRecord where
    -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    -- .: is an accessor
    -- https://www.stackbuilders.com/tutorials/haskell/csv-encoding-decoding/
    parseNamedRecord r = OptionRecord <$> r Cassava..: "name" <*> r Cassava..: "abbr" <*> r Cassava..: "desc"


instance Cassava.ToNamedRecord OptionRecord where
  toNamedRecord OptionRecord{..} =
    Cassava.namedRecord
      [ "name" Cassava..= name
      , "abbr" Cassava..= abbr
      , "desc" Cassava..= desc
      ]

optionsHeader :: Cassava.Header
optionsHeader = Vector.fromList [ "name", "abbr", "desc"]
-- instance ToField OptionRecord where
--   toField Country = "International Country"
--   toField (Other otherType) = toField otherType

getFirstWord :: String -> String
getFirstWord line
  | null line = []
  | null $ words line = []
  | otherwise = head $ words line

-- here we could do filter (/='\'') str
stripTicks :: String -> String
stripTicks w
  | null w = []
  | otherwise = init $ tail w

dropLinesTill :: String -> [String] -> ([String], Bool)
dropLinesTill firstWord rlines
  | null rlines = ([], False)
  | h == firstWord = (rlines, True)
  | otherwise = dropLinesTill firstWord $ tail rlines
  where h = getFirstWord $ head rlines

parseLine :: String -> Maybe OptionRecord
parseLine line
    | null settings = Nothing
    | length settings < 3 = Nothing
    | otherwise = Just $ OptionRecord (stripTicks $ head settings) (stripTicks $ head $ tail settings) (unwords $ tail $ tail settings)
    where settings = words line

stringifyRecord :: OptionRecord -> String
stringifyRecord OptionRecord { name =n , abbr=b, desc=d} = n ++ "," ++ b ++ "," ++ d

-- -- Cassava.encodeByName
-- -- initial version
-- saveRecords :: Handle -> [OptionRecord] -> IO ()
-- saveRecords out records
--   | null records = return ()
--   | otherwise = do
--     hPutStrLn out (stringifyRecord $ head records)
--     saveRecords out ( tail records )

saveRecords :: Handle -> [OptionRecord] -> IO ()
saveRecords out records
  | null records = return ()
  | otherwise = do
    hPutStrLn out (stringifyRecord $ head records)
    saveRecords out ( tail records )

encode :: String -> String -> IO ()
encode from out = do
  fd <- Prelude.readFile from
  let start = dropLinesTill "'aleph'" ( lines fd )
  let res = dropLinesTill "'writedelay'" $ reverse $ fst start
  let relevantLines = fst res
  -- create entries 
  print $ show relevantLines
  let records = map parseLine relevantLines
  -- filter out nothing values
  -- http://stackoverflow.com/questions/40327699/filtering-nothing-and-unpack-just
  let cleanRecords = reverse $ catMaybes records
  -- let records = Just ( OptionRecord "name" "abbr" "desc" ++  records
  -- do
  let result = Cassava.encodeByName optionsHeader cleanRecords
  ByteString.writeFile out result
  -- outh <- openFile "out.csv" WriteMode
  -- hPutStrLn outh result
  -- saveRecords outh cleanRecords
  -- print records
  print "hello world"
  -- print $ "start=" ++ unlines start

data Sample = Sample
  { hello  :: String
  , quiet  :: Bool
  , repeat :: Int }

sample :: Parser Sample
sample = Sample
      <$> strOption
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "repeat"
         <> help "Repeats for greeting"
         <> showDefault
         <> value 1
         <> metavar "INT" )

opts :: ParserInfo Sample
opts = info (sample <**> helper)
  ( fullDesc
  <> progDesc "My first haskell program !!"
  <> header "update options.lua" )



-- generate a new file
-- addToOptions :: [String] -> OptionRecord -> [String]
-- addToOptions lines record
  -- todo append to another file
  -- |  name record `isInfixOf` head lines = head lines ++ "short_desc=" ++ (desc record)  ++ lines
  -- | null lines  = []
  -- | otherwise = head lines ++ addToOptions (tail lines) record

genShortDesc :: OptionRecord -> String
genShortDesc record = "      short_desc='" ++ desc record ++ "',"
-- todo use encode ?!

-- -- todo use concatMap
-- --name record `isInfixOf` head lines = 
-- insertSpecificDesc :: OptionRecord -> String -> [String]
-- insertSpecificDesc record line
--     -- concatMap (\ line -> if name record `isInfixOf` line then [line, genShortDesc record] else [line]) lines
--     | (ame record ++ "'") `isInfixOf` line = [line, genShortDesc record]
--     | otherwise = []

-- to be useed with traverse
insertAllDesc :: V.Vector OptionRecord -> String -> [String]
insertAllDesc options line
  -- concatMap (\ line -> if name record `isInfixOf` line then [line, genShortDesc record] else [line]) lines
  | null options = [line]
  | ("full_name='" ++ name record) `isInfixOf` line = [line, genShortDesc record]
  | otherwise = insertAllDesc (V.tail options) line
  where record = V.head options

decode :: String -> IO ()
decode from = do
    csvData <- ByteString.readFile from

    fd <- readFile "options.lua"
    let l = lines fd
    -- let towrite = lines
    -- forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
    -- backslash => lambda
    -- for each record
    putStrLn "hello"
    case Cassava.decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> putStrLn $ unlines full
          --
          -- (\ line v -> if name record `isInfixOf` line then [line, genShortDesc record] else [line])
          where full = concatMap (insertAllDesc v) l
        -- Right (_, v) -> let full = insertAllDesc v l; putStrLn full

-- return ()
-- concatMap or traverse or mapM
-- traverse (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
-- Data.Foldable forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
-- print v
-- V.forM_ v $ \ p ->
--    --
--    -- putStrLn $ name p ++ show (desc p)
--    let towrite = addToOptions towrite p

-- out <- Prelude.writeFile "options2.lua"

-- TODO add sthg like
-- http://hackage.haskell.org/package/optparse-applicative

-- main :: IO
main = do
    -- a priori options is of type Sample
    options <- execParser opts
    putStrLn $ hello options
    case hello options of
      "encode" -> encode "quickref.txt" "out.csv"
      "decode" -> decode "out.csv"
    -- case args
    putStrLn "End of the program"
