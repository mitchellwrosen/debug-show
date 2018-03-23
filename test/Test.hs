{-# language DeriveGeneric #-}

import DebugShow

import GHC.Generics (Generic)

newtype A
  = A Int
  deriving Generic

data B
  = B Int Int Int
  deriving Generic

data C = C
  { c1 :: Int
  , c2 :: Int
  , c3 :: Int
  } deriving Generic

data D
  = D A B
  deriving Generic

data E = E
  { e1 :: C
  , e2 :: D
  } deriving Generic

main :: IO ()
main = do
  -- Base types
  debugPrint True
  debugPrint (1 :: Int)
  debugPrint (1 :: Integer)
  debugPrint 'a'
  debugPrint (1.0 :: Float)
  debugPrint (1.0 :: Double)

  -- Functions
  debugPrint not

  -- IO operations
  debugPrint (print "hi")

  -- Newtypes
  debugPrint (A 1)

  -- Product types
  debugPrint (B 1 2 3)

  -- Record syntax
  debugPrint C
    { c1 = 1
    , c2 = 2
    , c3 = 3
    }

  -- Nested ADTs
  debugPrint (D (A 1) (B 2 3 4))

  -- Record syntax with nested ADTs
  debugPrint E
    { e1 = C
        { c1 = 1
        , c2 = 2
        , c3 = 3
        }
    , e2 = D (A 1) (B 2 3 4)
    }
