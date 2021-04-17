module Parsing where

import Brzozowski
import Data

mkEps :: RegularExpression -> Tree
mkEps EmptySet = error ("Conjunto vazio")
mkEps EmptyString = Empty
mkEps (Char _) = error ("Caso Char")
mkEps (Concat r s) = Pair (mkEps r, mkEps s)
mkEps (Union r s)
  | nullable r = Inl (mkEps r)
  | nullable s = Inr (mkEps s)
  | otherwise = error ("Não é nullable")
mkEps (Kleene _) = List []

injection :: RegularExpression -> Char -> Tree -> Tree
injection (Char c) a Empty
  | c == a = Character c
  | otherwise = error ("Constante diferente de char")
injection (Union r s) a (Inl t) = Inl (injection r a t)
injection (Union r s) a (Inr t) = Inr (injection s a t)
injection (Concat e1 e2) a (Pair (t1, t2)) = Pair (injection e1 a t1, t2)
injection (Concat e1 e2) a (Inl (Pair (t1, t2))) = Pair (injection e1 a t1, t2)
injection (Concat e1 e2) a (Inr t2) = Pair (mkEps e1, injection e2 a t2)
injection (Kleene e) a (Pair (x, List xs)) = List ((injection e a x) : xs)
injection EmptySet _ _ = error ("Conjunto vazio da RE")
injection EmptyString _ _ = error ("String vazia da RE")

parse :: RegularExpression -> String -> Tree
parse e [] = mkEps e
parse e (x : xs) = injection e x (parse (derive e x) xs)
