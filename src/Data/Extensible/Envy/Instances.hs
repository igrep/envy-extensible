{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
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
-- Provides 'Env.FromEnv' instance for 'Ex.Record'.
--------------------------------------------------------------------------------

module Data.Extensible.Envy.Instances where


import           Data.Extensible      (Forall, Instance1)
import qualified Data.Extensible      as Ex
import           Data.Extensible.Envy
import           Data.Kind            (Type)
import           GHC.TypeLits         (KnownSymbol)
import qualified System.Envy          as Env


-- | Returns 'recordFromEnv' when the first argument of 'Env.fromEnv' is @Nothing@.
instance Forall (Ex.KeyTargetAre KnownSymbol (Instance1 Env.Var h)) xs
  => Env.FromEnv (Ex.RecordOf (h :: Type -> Type) xs)
 where
  fromEnv (Just r) = recordFromEnvWithDefault r (defaultFieldLabelToEnvName "")
  fromEnv Nothing  = recordFromEnv
  {-# INLINE fromEnv #-}
