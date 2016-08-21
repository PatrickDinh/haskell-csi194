{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Bits (xor)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret o m = do
    bs1 <- BS.readFile o
    bs2 <- BS.readFile m
    let diff = filter (\x -> x /= 0) (BS.zipWith xor bs1 bs2)
        result = BS.pack diff
    return result


-- Exercise 2 -----------------------------------------

decryptByteString :: ByteString -> ByteString -> ByteString
decryptByteString key "" = BS.empty
decryptByteString key encrypted = BS.concat[(BS.pack (BS.zipWith xor key takenEncrypted)), (decryptByteString key droppedEncrypted)]
  where takenEncrypted = BS.take (BS.length key) encrypted
        droppedEncrypted = BS.drop (BS.length key) encrypted

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey bs f = do
    bsEnc <- BS.readFile (f ++ ".enc")
    let result = decryptByteString bs bsEnc
    BS.writeFile f result

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile f = do
    bs <- BS.readFile f
    let r = Parser.decode bs
    return r

-- Exercise 4 -----------------------------------------
preMaybe :: Maybe [Transaction] -> Maybe [TId] -> Maybe [Transaction]
preMaybe maybeTrans maybeVictims = case maybeTrans of
                                  Nothing -> Nothing
                                  Just trans -> case maybeVictims of
                                                  Nothing -> Nothing
                                                  Just victims -> Just ([x | x <- trans, elem (tid x) victims])

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimPath transPath = do 
    victims <- parseFile victimPath :: IO (Maybe [TId])
    trans <- parseFile transPath :: IO (Maybe [Transaction])  
    let result = preMaybe trans victims
    return result

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = undefined

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = undefined

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

