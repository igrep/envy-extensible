{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE TypeOperators      #-}

module Data.Extensible.EnvySpec
  ( main
  , spec
  ) where

import           Data.Either          (isLeft)
import           Data.Extensible      (type (>:), (<:), (@=))
import qualified Data.Extensible      as Ex
import qualified System.Envy          as Env
import           Test.Hspec
import           Test.Main            (withEnv)

import           Data.Extensible.Envy


type SomeRecord = Ex.Record
 '[ "fooField" >: String
  , "bar" >: Bool
  , "baz" >: Int
  ]


main :: IO ()
main = hspec spec


spec :: Spec
spec = describe "recordFromEnvWith" $ do
  let fl2en = defaultFieldLabelToEnvName "EXAMPLE_RECORD"
  context "When all of the required environment variables are created" $
    it "can be created by System.Envy.runEnv" $ do
      let env =
            [ ("EXAMPLE_RECORD_FOO_FIELD", Just "string")
            , ("EXAMPLE_RECORD_BAR", Just "True")
            , ("EXAMPLE_RECORD_BAZ", Just "1230")
            ]
          expected :: SomeRecord
          expected =
                 #fooField @= "string"
              <: #bar @= True
              <: #baz @= 1230
              <: Ex.nil
      withEnv env $
        Env.runEnv (recordFromEnvWith fl2en) `shouldReturn` Right expected
  context "When some of the required environment variables are not created" $
    it "can't be created by System.Envy.runEnv" $ do
      let env =
            [ ("EXAMPLE_RECORD_FOO_FIELD", Just "string")
            , ("EXAMPLE_RECORD_BAR", Just "True")
            , ("EXAMPLE_RECORD_BAZ", Nothing)
            ]
      withEnv env $ do
        result <- Env.runEnv (recordFromEnvWith fl2en) :: IO (Either String SomeRecord)
        result `shouldSatisfy` isLeft
