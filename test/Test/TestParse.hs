module Test.TestParse where

import Test.Hspec
import Data.Aeson
import GHC.Generics
import Data.Conf.Json as J


main::IO()
main = hspec $ do
       describe "Test.TestParse" $ do
          it "case 1" $ do
            econf1 <- readParse "./test/Test/test-conf.json" 
            econf1 `shouldBe` (Right expect1)
        where expect1 = TestConf {
                a = 1,
                b = "val b",
                arr = [TestProp 10, TestProp 12]
                }
            

data TestProp = TestProp {
        prop::Int
        } deriving (Generic,Eq,Show)


data TestConf = TestConf {
    a::Int,
    b::String,
    arr::[TestProp]
    } deriving (Generic,Eq,Show)


instance FromJSON TestProp            
instance FromJSON TestConf            