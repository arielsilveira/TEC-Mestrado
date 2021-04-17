module Brzozowski where

import Data

nullable :: RegularExpression -> Bool
nullable EmptySet = False
nullable EmptyString = True
nullable (Char a) = False
nullable (Union s r) = nullable s || nullable r
nullable (Concat s r) = nullable s && nullable r
nullable (Kleene s) = True

derive :: RegularExpression -> Char -> RegularExpression
derive EmptySet a = EmptySet
derive EmptyString a = EmptySet
derive (Char s) a
  | s == a = EmptyString
  | otherwise = EmptySet
derive (Union s r) a = Union (derive s a) (derive r a)
derive (Concat s r) a
  | nullable s = Union (Concat (derive s a) r) (derive r a)
  | otherwise = Concat (derive s a) r
derive (Kleene s) a = Concat (derive s a) (Kleene s)

match :: RegularExpression -> String -> Bool
match e [] = nullable e
match e (a : y) = match (derive e a) y