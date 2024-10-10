module Nat where

import Prelude hiding
    (Num(..), (^))

data Nat where 
        O :: Nat
        S :: Nat -> Nat
    deriving (Eq, Show)

o, so, sso, ssso :: Nat
o = O
so = S o
sso = S so
ssso = S sso

plus :: Nat -> Nat -> Nat
plus n O = n
plus n (S m) = S (plus n m)

(+) :: Nat -> Nat -> Nat
(+) = plus

times :: Nat -> Nat -> Nat
times n O = O
times n (S m) = times n m + n

(*) :: Nat -> Nat -> Nat
(*) = times

(^) :: Nat -> Nat -> Nat 
n ^ O = S O
m ^ (S n) = (m ^ n) * m 

        
