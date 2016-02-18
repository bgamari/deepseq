{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE DeriveGeneric #-}

module Control.DeepStrict (IsDeepStrict, AutoDeepSeq(..)) where

import Data.Proxy
import GHC.TypeLits
import GHC.Generics
import qualified Control.DeepSeq as NFData

type family IsDeepStrict (f :: * -> *) :: Bool where
    IsDeepStrict U1        = 'True
    IsDeepStrict V1        = 'True
    IsDeepStrict (S1 ('MetaSel _ _ _ 'DecidedLazy) _)   = 'False
    IsDeepStrict (S1 ('MetaSel _ _ _ 'DecidedStrict) x) = IsDeepStrict x
    IsDeepStrict (D1 m f)  = IsDeepStrict f
    IsDeepStrict (C1 m f)  = IsDeepStrict f
    IsDeepStrict (f :+: g) = IsDeepStrict f :&& IsDeepStrict g
    IsDeepStrict (f :*: g) = IsDeepStrict f :&& IsDeepStrict g
    -- GHC thinks the above are undecideable due to recursion, but we know there
    -- is finite depth
    IsDeepStrict (Rec0 f) = 'False -- FIXME: Is this correct? It seems like many
                                   -- instances are of this aren't actually recursive
    IsDeepStrict x         = TypeError (ShowType x)

type family (:&&) (a :: Bool) (b :: Bool) :: Bool where
    'True :&& 'True = 'True
    _     :&& _     = 'False

type family If (cond :: Bool) (a :: k) (b :: k) where
    If 'True  a _ = a
    If 'False _ b = b

data SeqType = Shallow | Deep

type family DeepSeq a :: SeqType where
    DeepSeq a = If (IsDeepStrict (Rep a)) Shallow Deep

class DoSeq' (deep :: Bool) a where
    seqIt' :: Proxy deep -> a -> b -> b

instance DoSeq' 'True a where
    seqIt' _ = seq

instance (NFData.NFData a) => DoSeq' 'False a where
    seqIt' _ x y = NFData.rnf x `seq` y

class AutoDeepSeq a where
    deepSeq :: a -> b -> b

instance (deep ~ IsDeepStrict (Rep a), DoSeq' deep a) => AutoDeepSeq a where
    deepSeq = seqIt' (Proxy :: Proxy deep)


data Hello = Hello !() deriving (Generic)
test :: (IsDeepStrict (Rep a) ~ 'True) => Proxy a -> ()
test _ = ()

main = print (test (Proxy :: Proxy Hello))
