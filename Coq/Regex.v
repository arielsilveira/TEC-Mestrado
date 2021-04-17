Require Import Ascii String.

Inductive regex: Set :=
  | Empty: regex
  | Lambda: regex
  | Char: ascii -> regex
  | Union: regex -> regex -> regex
  | Concat: regex -> regex -> regex
  | Kleene: regex -> regex.

Inductive Tree: Set :=
  | Null: Tree
  | Const: ascii -> Tree
  | Inl: Tree -> Tree
  | Inr: Tree -> Tree
  | Pair: Tree -> Tree
  | List: Tree -> Tree. 


Fixpoint nullable (re: regex): bool :=
  match re with
  | Empty => false
  | Lambda => true
  | Char _ => false
  | Union a b => nullable a || nullable b
  | Concat a b => nullable a && nullable b
  | Kleene _ => true
end.

Compute nullable (Concat (Char "b") (Char "a")).

Fixpoint derive (re:regex) (s: ascii): regex :=
  match re with
  | Empty => Empty
  | Lambda => Empty
  | Char c => match (ascii_dec c s) with
    | left _ => Lambda
    | right _ => Empty
  end
  | Union a b => Union (derive a s) (derive b s)
  | Concat a b => match (nullable a) with
    | true => Union (Concat (derive a s) b) (derive b s)
    | false => (Concat (derive a s) b)
  end
  | Kleene k => Concat (derive k s) (Kleene k)
end.

Fixpoint matching (re: regex) (s: string): bool :=
  match s with
  | EmptyString => nullable re
  | String x xs => matching (derive re x) xs 
end.

Compute matching (Kleene (Union (Concat (Char "a") (Char "b")) (Char "c"))) "abb".

Inductive mkEps {t: Tree} (re: regex) : Tree -> Prop :=
  | mkEpsEmptyNull: mkEps Empty Null
  | mkEpsConcatPair: forall (a b: regex),
      mkEps (Concat a b) (Pair (mkEps a t) (mkEps b)) 
  | Union a b : match nullable a with
    | true => Inl (mkEps a)
    | false => match nullable b with
        | true => Inr (mkEps b)
        end
    end
  | Kleene k : List Empty.

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
