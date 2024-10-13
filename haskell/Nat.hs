module Nat where

import Prelude hiding
    (Num(..), Bool(..), (^), (>), (>=), quot, pred, min, max, rem, lcm, gcd)

import Bool
data Nat where 
        O :: Nat
        S :: Nat -> Nat
    deriving (Eq, Show)

o, so, sso, ssso, sssso :: Nat
o = O
so = S o
sso = S so
ssso = S sso
sssso = S ssso

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
fib (S n) = fib(n) + fib (pred n)

min :: (Nat,Nat) -> Nat
min (m,O) = O 
min (O,n) = O
min (S m,S n) = S (min(m, n)) 

max :: (Nat, Nat) -> Nat
max (m,O) = m
max (O,n) = n 
max (S m,S n) = S (max(m,n))

monus :: Nat -> Nat -> Nat
monus O n = O
monus n O = n
monus m n = monus (pred m) (pred n)

(>) :: Nat -> Nat -> Bool
O > O = False
n > O = True
O > n = False
S n > S m = n > m
                    
quot :: (Nat, Nat) -> Nat
quot (O, n) = O
quot (n, O) = error "impossivel dividir por O"
quot (n, S O) = n
quot (m,n) = ifthenelse (n > m) O (S(quot(monus m n, n)))
                                            
rem :: (Nat, Nat) -> Nat
rem (O, n) = O
rem (n, O) = error "impossivel dividir por O"
rem (m, n) = monus(quot (m, n) * n) m

div :: (Nat, Nat) -> (Nat,Nat)  
div (m,n) = (quot(m,n), rem(m,n))

lcm :: (Nat, Nat) -> Nat
lcm(m,n) = quot(m * n, gcd(m,n) )

gcd :: (Nat, Nat) -> Nat
gcd(O, n) = n 
gcd(S O, n) = so
gcd(m, n) = gcd (n, rem(m,n))


