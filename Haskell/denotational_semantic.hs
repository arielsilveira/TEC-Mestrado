data Exp
  = Const Bool
  | Exp :/\: Exp
  | Exp :\/: Exp
  | Exp :->: Exp
  deriving (Eq, Show)

denotational:: Exp -> Bool
denotational (Const e) = e
denotational (e1 :/\: e2) = denotational e1 && denotational e2
denotational (e1 :\/: e2) = denotational e1 || denotational e2
denotational (e1 :->: e2) = not (denotational e1) || denotational e2
