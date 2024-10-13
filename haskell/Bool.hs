module Bool where

import Prelude hiding (Bool(..))

data Bool where
       True :: Bool
       False :: Bool 
    deriving (Eq, Show)    

ifthenelse :: Bool -> a -> a -> a 
ifthenelse True b c = b
ifthenelse False b c = c

 
  