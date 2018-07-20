{- | reads config or any valid json file and parses it to a Haskell type 

arg: full or relative (to current directory) path to the json file  

@
import Data.Aeson
import GHC.Generics

data TestProp = TestProp {
        prop::Int
        } deriving (Generic,Eq,Show)

instance FromJSON TestProp            
@
-}
module Data.Conf.Json
        (readParse) where

import Control.Monad
import Data.Aeson as A
import Data.ByteString as B
import Data.ByteString.Lazy as L
import Prelude as P
import System.Directory as D
import System.IO as I


readParse::FromJSON conf =>
     FilePath -> IO (Either String conf)
readParse fullPath0 = do
     tbs1 <- readEntireFile fullPath0::IO (Either String B.ByteString)
     pure $ toLazy <$> tbs1 >>=
         eitherDecode'   --  ::Either String Config
     where toLazy::B.ByteString -> L.ByteString
           toLazy bs0 = L.fromChunks [bs0]


readEntireFile::FilePath -> IO (Either String B.ByteString)
readEntireFile path0 = do
    exists1 <- doesFileExist path0
    if exists1 then liftM Right $
        withBinaryFile path0 ReadMode B.hGetContents
    else pure $ Left $ "file n/a: " ++ path0
