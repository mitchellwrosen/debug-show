{-# language DataKinds            #-}
{-# language FlexibleContexts     #-}
{-# language FlexibleInstances    #-}
{-# language InstanceSigs         #-}
{-# language ScopedTypeVariables  #-}
{-# language TypeApplications     #-}
{-# language TypeFamilies         #-}
{-# language TypeOperators        #-}
{-# options_ghc -fno-warn-orphans #-}

module DebugShow
  ( debugShow
  , debugPrint
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy (Proxy(Proxy))
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Text.PrettyPrint
import Type.Reflection (Typeable, typeRep)

debugShow :: (Generic a, GDebugShow (Rep a)) => a -> String
debugShow =
  render . foldr ($+$) empty . gdebugShow False . from

debugPrint :: (Generic a, GDebugShow (Rep a), MonadIO m) => a -> m ()
debugPrint =
  liftIO . putStrLn . debugShow

class GDebugShow f where
  gdebugShow :: Bool -> f x -> [Doc]

instance GDebugShow f => GDebugShow (D1 m f) where
  gdebugShow :: Bool -> D1 m f x -> [Doc]
  gdebugShow b (M1 f) =
    gdebugShow b f

instance
  ( KnownSymbol n
  , GDebugShow f
  ) => GDebugShow (C1 ('MetaCons n i 'False) f) where
  gdebugShow :: Bool -> C1 ('MetaCons n i 'False) f x -> [Doc]
  gdebugShow b (M1 f) =
    [ (if b then parens else id)
        (text (symbolVal @n Proxy)
          <+> foldr ($+$) empty (gdebugShow True f))
    ]

instance
  ( KnownSymbol n
  , GDebugShow f
  ) => GDebugShow (C1 ('MetaCons n i 'True) f) where
  gdebugShow :: Bool -> C1 ('MetaCons n i 'True) f x -> [Doc]
  gdebugShow b (M1 f) =
    [ (if b then parens else id)
        (text (symbolVal @n Proxy)
          <+> text "{"
          <+> foldr ($+$) (nest (-2) (text "}")) (gdebugShow True f))
    ]

instance (GDebugShow f, GDebugShow g) => GDebugShow (f :*: g) where
  gdebugShow :: Bool -> (f :*: g) x -> [Doc]
  gdebugShow b (f :*: g) =
    gdebugShow b f ++ gdebugShow b g

instance (GDebugShow f, GDebugShow g) => GDebugShow (f :+: g) where
  gdebugShow :: Bool -> (f :+: g) x -> [Doc]
  gdebugShow b (L1 f) =
    gdebugShow b f
  gdebugShow b (R1 g) =
    gdebugShow b g

instance GDebugShow f => GDebugShow (S1 ('MetaSel 'Nothing su ss ds) f) where
  gdebugShow :: Bool -> S1 ('MetaSel 'Nothing su ss ds) f x -> [Doc]
  gdebugShow b (M1 f) =
    gdebugShow b f

instance
  ( KnownSymbol s
  , GDebugShow f
  ) => GDebugShow (S1 ('MetaSel ('Just s) su ss ds) f) where
  gdebugShow :: Bool -> S1 ('MetaSel ('Just s) su ss ds) f x -> [Doc]
  gdebugShow _ (M1 f) =
    [ text (symbolVal @s Proxy)
        <+> text "="
        <+> foldr ($+$) empty (gdebugShow False f)
    ]


instance (Generic a, GDebugShow (Rep a)) => GDebugShow (K1 R a) where
  gdebugShow :: Bool -> K1 R a x -> [Doc]
  gdebugShow b (K1 x) =
    gdebugShow b (from x)

instance GDebugShow U1 where
  gdebugShow :: Bool -> U1 x -> [Doc]
  gdebugShow _ _ =
    []

newtype Base a x
  = Base { unBase :: a }

instance Show a => GDebugShow (Base a) where
  gdebugShow :: Bool -> Base a x -> [Doc]
  gdebugShow _ (Base x) =
    [text (show x)]

instance Generic Char where
  type Rep Char = Base Char
  from = Base
  to = unBase

instance Generic Double where
  type Rep Double = Base Double
  from = Base
  to = unBase

instance Generic Float where
  type Rep Float = Base Float
  from = Base
  to = unBase

instance Generic Int where
  type Rep Int = Base Int
  from = Base
  to = unBase

instance Generic Integer where
  type Rep Integer = Base Integer
  from = Base
  to = unBase

instance Generic Word where
  type Rep Word = Base Word
  from = Base
  to = unBase

instance Generic (IO a) where
  type Rep (IO a) = Base (IO a)
  from = Base
  to = unBase

instance Generic (a -> b) where
  type Rep (a -> b) = Base (a -> b)
  from = Base
  to = unBase

instance {-# OVERLAPPABLE #-} Typeable a => Show a where
  show _ = "<" ++ show (typeRep @a) ++ ">"
