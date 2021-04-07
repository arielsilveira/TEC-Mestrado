data RegularExpression
  = EmptySet
  | EmptyString
  | Char Char
  | Union RegularExpression RegularExpression
  | Concat RegularExpression RegularExpression
  | Kleene RegularExpression
  | Not RegularExpression
  deriving (Show)

nullable :: RegularExpression -> Bool
nullable EmptySet = False
nullable EmptyString = True
nullable (Char a) = False
nullable (Union s r) = nullable s || nullable r
nullable (Concat s r) = nullable s && nullable r
nullable (Kleene s) = True
nullable (Not s) = not (nullable s)

derive :: RegularExpression -> Char -> RegularExpression
derive EmptySet a = EmptySet
derive EmptyString a = EmptyString
derive (Char s) a
  | s == a = EmptyString
  | otherwise = EmptySet
derive (Union s r) a = Union (derive s a) (derive r a)
derive (Concat s r) a
  | nullable s = Union (Concat (derive s a) r) (derive r a)
  | otherwise = Concat (derive s a) r
derive (Kleene s) a = Concat (derive s a) s
derive (Not s) a = Not (derive s a)
