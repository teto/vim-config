{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import Options.Applicative
  -- (Parser, ParserInfo)
import qualified Data.Semigroup  as SG

import System.IO
import Data.Maybe (catMaybes)
-- import Options.Applicative
-- import Data.Semigroup ((<>))
-- import Control.Applicative
-- import Data.Csv
import qualified Data.Vector as V
-- bytestring
-- import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
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


data OptionRecord = OptionRecord { name :: !String, abbr :: !String, desc :: !String } deriving (Eq, Show)

instance  Cassava.FromNamedRecord OptionRecord where
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

csvHeader :: Cassava.Header
csvHeader = Vector.fromList [ "name", "abbr", "desc"]
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
  let cleanRecords = catMaybes records
  -- let records = Just ( OptionRecord "name" "abbr" "desc" ++  records
  outh <- openFile "out.csv" WriteMode
  -- do
  saveRecords outh cleanRecords
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


decode :: String -> IO ()
decode from = do 
    csvData <- ByteString.readFile from
    case Cassava.decodeByName csvData of
        Left err -> putStrLn err
        -- forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
        -- backslash => lambda
        Right (_, v) -> V.forM_ v $ \ p ->
            putStrLn $ name p ++ " earns " ++ show (desc p) ++ " dollars"



-- TODO add sthg like
-- http://hackage.haskell.org/package/optparse-applicative
main :: IO ()
main = do
    -- a priori options is of type Sample
    options <- execParser opts
    putStrLn $ hello options
    case hello options of
      "encode" -> encode "quickref.txt" "out.csv"
      "decode" -> decode "out.csv"
    -- case args
