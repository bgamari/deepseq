{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Control.DeepStrict (DeepStrict) where

import Data.Proxy
import GHC.Generics
import GHC.TypeLits (TypeError, ErrorMessage(..))

-- For instances
import Data.Word
import Data.Int
import Data.Int
import GHC.Fingerprint.Type (Fingerprint)

class GDeepStrict (f :: * -> *) where
 -- no methods


-- | The class of types which are statically known to be fully-strict. In order
-- to derive this you must also derive 'Generic'.
class DeepStrict (a :: *) where
  dummy :: Proxy a
  default dummy :: (Generic a, GDeepStrict (Rep a)) => Proxy a
  dummy = Proxy


-- Known-strict types from "base"
instance DeepStrict Int where dummy = Proxy
instance DeepStrict Word where dummy = Proxy
instance DeepStrict Integer where dummy = Proxy
instance DeepStrict Float where dummy = Proxy
instance DeepStrict Double where dummy = Proxy

instance DeepStrict Char where dummy = Proxy
instance DeepStrict Bool where dummy = Proxy
instance DeepStrict () where dummy = Proxy

instance DeepStrict Word8 where dummy = Proxy
instance DeepStrict Word16 where dummy = Proxy
instance DeepStrict Word32 where dummy = Proxy

instance DeepStrict Int8 where dummy = Proxy
instance DeepStrict Int16 where dummy = Proxy
instance DeepStrict Int32 where dummy = Proxy
instance DeepStrict Fingerprint where dummy = Proxy


instance GDeepStrict U1
instance GDeepStrict V1
instance GDeepStrict f => GDeepStrict (D1 m f)
instance GDeepStrict f => GDeepStrict (C1 m f)
instance (GDeepStrict f, GDeepStrict g) => GDeepStrict (f :+: g)
instance (GDeepStrict f, GDeepStrict g) => GDeepStrict (f :*: g)
instance GDeepStrict (Rec0 f)

-- This is where we detect strictness
instance GDeepStrict f => GDeepStrict (S1 ('MetaSel mb su ss 'DecidedStrict) (f :: * -> *))
instance GDeepStrict f => GDeepStrict (S1 ('MetaSel mb su ss 'DecidedUnpack) (f :: * -> *))

instance (TypeError ('Text "Cannot derive DeepStrict; lazy field found: " :<>: 'ShowType f), GDeepStrict f)
         => GDeepStrict (S1 ('MetaSel mb su ss DecidedLazy) f)


data ATree a = ATree !a
             deriving (Generic, DeepStrict)

data AnInt = AnInt !Int deriving (Generic, DeepStrict)
data AChar = AChar !Char deriving (Generic, DeepStrict)
