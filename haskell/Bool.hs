module Bool where

import Prelude hiding (Bool(..))
import GHC.TypeLits (Nat)
import Text.XHtml (base)

data Bool where
       True :: Bool
       False :: Bool 
    deriving (Eq, Show)    

ifthenelse :: Bool -> a -> a -> a 
ifthenelse True b c = b
ifthenelse False b c = c

 
  