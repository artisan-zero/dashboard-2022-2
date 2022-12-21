
---------------------------------------------------------------------
-- Module      : Commands
-- Author      : Willian A. Salles
---------------------------------------------------------------------
module Commands where

import Prelude                       hiding (error)

import System.Exit                          ( ExitCode(..) )
import System.Process                       ( system )

-- system-posix-redirect-1.1.0.1
import System.Posix.Redirect                ( redirectStderr
                                            , redirectStdout )

import Data.ByteString.Char8                ( unpack )
import Data.List.NonEmpty                   ( NonEmpty, (<|) )
import Text.Printf                          ( printf )

import Text.ParserCombinators.Parsec hiding ( try )
import Text.Parsec.Prim                     ( ParsecT, try )
import Text.Parsec.Char

import Control.Monad.Trans.Except



---------------------------------------------------------------------

-- General Parsers
line :: (Monad m) => ParsecT String u m String
line = manyTill anyChar endOfLine

numberP :: (Read a, Monad m) => ParsecT String u m a
numberP = read <$> (many1 digit)

percentP :: (Monad m) => ParsecT String u m String
percentP = many1 digit <* spaces <* char '%'

memPrefixP :: (Monad m) => ParsecT String u m String
memPrefixP = string "Mem:"

fileP :: (Monad m) => ParsecT String u m String
fileP = many1 alphaNum  <?> "File expected"

dirP :: (Monad m) => ParsecT String u m String
dirP = fileP <* many barP    <?> "Dir expected"

entryP :: (Monad m) => ParsecT String u m String
entryP = try dirP <|> fileP <?> "Entry expected"

barP :: (Monad m) => ParsecT String u m String
barP = string "/"

startPathP :: (Monad m) => ParsecT String u m String
startPathP
  = try $ (many1 $ char '/')
  <|> try (string ".." <* barP)
  <|> string "." <* barP
  <|> return "."


newtype Path = Path (NonEmpty String) deriving (Show)

pathP :: (Monad m) => ParsecT String u m Path
pathP = Path <$> do
  i <- startPathP
  rex (pure i) <|> return (pure i)
  where
    rex def = do
      e <- entryP
      let lasts = e <| def in
        try (rex lasts) <|> return lasts


---------------------------------------------------------------------

newtype Error = Error String deriving (Show)

error :: (Show a) => a -> Error
error = Error . show


class Usage a where
  total :: a -> Float
  used  :: a -> Float
  avail :: a -> Float
  usedRate :: a -> Float

  avail m = (total m) - (used m)
  usedRate m = (used m) / (total m)


-- MemoryUsage
data MemoryUsage = MemoryUsage Float Float

instance Usage MemoryUsage where
  total (MemoryUsage t _) = t
  used (MemoryUsage _ u) = u

instance Show MemoryUsage where
  show m = printf "[ total: %f | used: %f (%.2f%%) | avail: %f ]"
           (total m) (used m) (100 * usedRate m) (avail m)


cmdFreeP
  = line
  >> memPrefixP
  >> (MemoryUsage
      <$> (spaces *> numberP)
      <*> (spaces *> numberP))
  <?> "Error parsing command: free"


parseFree :: String -> Either ParseError MemoryUsage
parseFree = parse cmdFreeP "Parsing cmd: free"

getMemoryUsage :: ExceptT Error IO MemoryUsage
getMemoryUsage = cmdFree >>= (withExceptT error . except . parseFree)

-- Process Count
newtype ProcessCount = ProcessCount Int deriving (Show)

getProcessCount :: ExceptT Error IO ProcessCount
getProcessCount
  = cmdProcessCount
  >>= (withExceptT error . except . (fmap (ProcessCount . round)) . (parse numberP "Parsing process count"))
  
-- Disk Usage

--   Disk         Name  Total Used   Path
data DiskUsage = DiskUsage Path Float Float Path deriving (Show)
newtype DisksUsage = DisksUsage [DiskUsage] deriving (Show)


diskUsageRecordP
  = DiskUsage
  <$> pathP
  <*> (spaces *> numberP)
  <*> (spaces *> numberP)
  <*> (spaces *> percentP *> spaces *> pathP)

diskUsageP = line *> many diskUsageRecordP

parseDiskUsage :: String -> Either ParseError DisksUsage
parseDiskUsage = fmap DisksUsage . parse diskUsageP "Parsing cmd 'df -H'"

getDiskUsage = cmdDf >>= (withExceptT error . except . parseDiskUsage)

-- ps | wc -l
cmdProcessCount :: ExceptT Error IO String
cmdProcessCount = cmd "Error on command: 'ps aux | wc -l'" $ system "ps aux | wc -l"

-- free
cmdFree :: ExceptT Error IO String
cmdFree = cmd "Error on command 'free' " $ system "free"

-- df
cmdDf :: ExceptT Error IO String
cmdDf = cmd "Error on command 'df'" $ system "df"

-- Generic System Command : (System.Posix.Redirect)
cmd :: String -> IO ExitCode -> ExceptT Error IO String
cmd errMsg comm  = do
  ExceptT $ (g errMsg . snd) <$> ((redirectStderr . redirectStdout) $ comm)
  where
    g errMsg (_  , ExitFailure e) = Left  $ error $ errMsg ++ (show e)
    g _      (out, _            ) = Right $ unpack out
    
