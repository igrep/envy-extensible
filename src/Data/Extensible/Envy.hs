{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

--------------------------------------------------------------------------------
-- |
-- Module    : Data.Extensible.Envy
-- Copyright : (c) 2020 IIJ Innovation Institute, Inc.
-- License   : BSD3
-- Maintainer: YAMAMOTO Yuji <yuji-yamamoto@iij.ad.jp>
-- Stability : Experimental
--
-- Provides functions to create 'Ex.Record' from environment variable
-- using 'Env.Parser'.
--------------------------------------------------------------------------------

module Data.Extensible.Envy
  ( recordFromEnvWith
  , recordFromEnvWithDefault
  , recordFromEnv
  , FieldLabelToEnvName
  , defaultFieldLabelToEnvName
  , camelToUpperSnakeCase
  ) where


import           Control.Applicative   ((<|>))
import qualified Data.Char             as C
import           Data.Extensible       (Forall, Instance1)
import qualified Data.Extensible       as Ex
import           Data.Functor.Identity (Identity (Identity), runIdentity)
import           Data.Kind             (Type)
import           Data.Proxy            (Proxy (Proxy))
import           GHC.TypeLits          (KnownSymbol, Symbol)
import qualified System.Envy           as Env


-- | Function to convert field labels of 'Ex.Record' into the
--   name of environment variable.
--   Applied to each field label before passing it to 'Env.env'
type FieldLabelToEnvName = String -> String


-- | The default of 'FieldLabelToEnvName'.
--
-- If the first argument is empty, just convert the field label
-- (second argument) into @UPPER_SNAKE_CASE@.
--
-- >>> defaultFieldLabelToEnvName "" "thisIsATest"
-- "THIS_IS_A_TEST"
--
-- Otherwise, convert the field label into @UPPER_SNAKE_CASE@,
-- then prepend the first argument with an underscore @_@.
--
-- >>> defaultFieldLabelToEnvName "PREFIXED" "thisIsATest"
-- "PREFIXED_THIS_IS_A_TEST"
defaultFieldLabelToEnvName :: String -> FieldLabelToEnvName
defaultFieldLabelToEnvName ""     s = camelToUpperSnakeCase s
defaultFieldLabelToEnvName prefix s = prefix ++ "_" ++ camelToUpperSnakeCase s
{-# INLINE defaultFieldLabelToEnvName #-}


-- | Used internally in 'defaultFieldLabelToEnvName'.
--   Published for your convenience.
camelToUpperSnakeCase :: FieldLabelToEnvName
camelToUpperSnakeCase =
  foldMap (\c -> if C.isUpper c then '_' : [c] else [C.toUpper c])
{-# INLINE camelToUpperSnakeCase #-}


-- |
-- @
-- recordFromEnv = recordFromEnvWith $ defaultFieldLabelToEnvName ""
-- @
recordFromEnv
  :: forall (xs :: [Ex.Assoc Symbol Type]) h
   . Forall (Ex.KeyTargetAre KnownSymbol (Instance1 Env.Var h)) xs
  => Env.Parser (Ex.RecordOf h xs)
recordFromEnv = recordFromEnvWith $ defaultFieldLabelToEnvName ""
{-# INLINE recordFromEnv #-}


-- | 'Env.Parser' for 'Ex.Record'
recordFromEnvWith
  :: forall (xs :: [Ex.Assoc Symbol Type]) h
   . Forall (Ex.KeyTargetAre KnownSymbol (Instance1 Env.Var h)) xs
  => FieldLabelToEnvName
  -> Env.Parser (Ex.RecordOf h xs)
recordFromEnvWith fl2en =
  Ex.hgenerateFor
    (Proxy :: Proxy (Ex.KeyTargetAre KnownSymbol (Instance1 Env.Var h))) f
 where
  f membership = Ex.Field <$> Env.env (fl2en $ Ex.stringKeyOf membership)
{-# INLINE recordFromEnvWith #-}


-- | 'Env.Parser' for 'Ex.Record'.
-- This is necessary to implement the instance of 'Env.FromEnv' for
-- 'Ex.Record' correctly.
recordFromEnvWithDefault
  :: forall (xs :: [Ex.Assoc Symbol Type]) h
   . Forall (Ex.KeyTargetAre KnownSymbol (Instance1 Env.Var h)) xs
  => Ex.RecordOf h xs
  -> FieldLabelToEnvName
  -> Env.Parser (Ex.RecordOf h xs)
recordFromEnvWithDefault def fl2en =
  Ex.hgenerateFor
    (Proxy :: Proxy (Ex.KeyTargetAre KnownSymbol (Instance1 Env.Var h))) f
 where
  f membership =
    ( Ex.Field <$> Env.env (fl2en $ Ex.stringKeyOf membership)
    ) <|> pure (Ex.hlookup membership def)
{-# INLINE recordFromEnvWithDefault #-}


-- | Necessary to make a 'Env.Parser' for 'Ex.Record'.
--
-- TODO: Remove this after <https://github.com/dmjio/envy/pull/31> is released.
instance Env.Var a => Env.Var (Identity a) where
  toVar = Env.toVar . runIdentity
  {-# INLINE toVar #-}
  fromVar = fmap Identity . Env.fromVar
  {-# INLINE fromVar #-}
