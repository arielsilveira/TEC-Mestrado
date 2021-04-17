module Data where

data RegularExpression
  = EmptySet
  | EmptyString
  | Char Char
  | Union RegularExpression RegularExpression
  | Concat RegularExpression RegularExpression
  | Kleene RegularExpression
  deriving (Show)

data Tree
  = Empty
  | Character Char
  | Inl Tree
  | Inr Tree
  | Pair (Tree, Tree)
  | List [Tree]
  deriving (Eq, Show)
