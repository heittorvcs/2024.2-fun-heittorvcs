module Nat where

import Prelude hiding
    (Num(..), (^), pred, min, max)

data Nat where 
        O :: Nat
        S :: Nat -> Nat
    deriving (Eq, Show)

o, so, sso, ssso :: Nat
o = O
so = S o
sso = S so
ssso = S sso

pred :: Nat -> Nat
pred O = O
pred (S n) = n

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

double :: Nat -> Nat
double n = times n sso

fact :: Nat -> Nat
fact O = so
fact n = fact (pred n) * n

fib :: Nat -> Nat
fib O = S O
fib (S O) = S O
fib n = n + fib (pred n)

min :: (Nat,Nat) -> Nat
min (m,O) = O 
min (O,n) = O
min (S m,S n) = S (min(m, n)) 

max :: (Nat, Nat) -> Nat
max (m,O) = m
max (O,n) = n 
max (S m,S n) = S (max(m,n))



