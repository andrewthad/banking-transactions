{-# LANGUAGE OverloadedStrings #-}

module Main where

import Siphon (Siphon,SiphonError)
import Data.ByteString (ByteString)
import Data.Fixed (Fixed,E2)
import Colonnade (Headless,Headed)
import Text.Read (readMaybe)
import Streaming (Of,Stream)
import System.IO (Handle,withFile,IOMode(ReadMode))
import Control.Monad.Trans.Class (lift)
import Data.Map.Strict (Map)
import Control.Foldl (Fold(..))
import qualified Control.Foldl as F
import qualified Data.List as L
import qualified Siphon as SPN
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Streaming.Char8 as SBC8
import qualified Streaming.Prelude as SMP
import qualified Data.Map.Strict as M

data Currency = Dollar | Euro
  deriving (Show,Eq,Ord)

data Transaction = Transaction
  { identifier :: !Int
  , account :: !Int
  , amount :: !(Fixed E2)
  , currency :: !Currency
  } deriving (Show)

readIntExactly :: ByteString -> Maybe Int
readIntExactly bs = case BC.readInt bs of
  Nothing -> Nothing
  Just (i,bs') -> if BC.null bs'
    then Just i
    else Nothing

readAmountExactly :: ByteString -> Maybe (Fixed E2)
readAmountExactly bs = readMaybe (BC.unpack bs)

readCurrency :: ByteString -> Maybe Currency
readCurrency bs
  | bs == "USD" = Just Dollar
  | bs == "EUR" = Just Euro
  | otherwise = Nothing

siphon :: Siphon Headed ByteString Transaction
siphon = Transaction
  <$> SPN.headed "transaction" readIntExactly
  <*> SPN.headed "account" readIntExactly
  <*> SPN.headed "amount" readAmountExactly
  <*> SPN.headed "cur" readCurrency

buildStream :: Handle -> Stream (Of Transaction) IO ()
buildStream = id
  . (>>= maybe (return ()) (lift . fail . SPN.humanizeSiphonError))
  . SPN.decodeHeadedUtf8Csv siphon
  . SBC8.toChunks
  . SBC8.fromHandle

-- the map's key is the account number
netPerAccount :: Fold Transaction (Map Int (Map Currency (Fixed E2)))
netPerAccount = Fold step M.empty id where
  step :: Map Int (Map Currency (Fixed E2)) -> Transaction -> Map Int (Map Currency (Fixed E2))
  step m t = M.alter (\p -> case p of
      Nothing -> Just (M.singleton (currency t) (amount t))
      Just old -> Just (M.insertWith (+) (currency t) (amount t) old)
    ) (account t) m

displayNet :: Int -> Map Currency (Fixed E2) -> String
displayNet acctId m = L.concat
  [ "Account "
  , show acctId
  , " - "
  , L.intercalate ", " (M.foldMapWithKey (\cur amt -> [show cur ++ ": " ++ show amt]) m)
  ]

main :: IO ()
main = withFile "transactions.csv" ReadMode $ \h -> do
  putStrLn "Choose an action:"
  putStrLn "1) print transactions"
  putStrLn "2) net transaction of each kind of currency per account"
  putStrLn "3) count transactions bigger than $20"
  command <- getLine
  let s = buildStream h
  case command of
    "1" -> SMP.print s
    "2" -> do
      net <- F.purely SMP.fold_ netPerAccount s
      M.foldMapWithKey (\acctId m -> putStrLn (displayNet acctId m)) net
    "3" -> do
      count <- SMP.length_ (SMP.filter (\x -> amount x >= 20) s)
      putStrLn ("There were " ++ show count ++ " transactions for more than $20")
    _ -> putStrLn "Bad command given. No action will be taken."

