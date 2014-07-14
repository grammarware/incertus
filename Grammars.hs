module Grammars
where

infix 1 ::= 
type Grammar = [Rule]
data Rule = (::=) Name Expr
type Name = [Char]
data Expr
	= NT Name
	| T Name
	| Seq [Expr]
	| Dis [Expr] -- Expr -- list or binary?
	| Con [Expr] -- Expr
	| Neg Expr
	| Epsilon
	| Empty

pp :: Grammar -> [Char]
pp rs = intercalate "\n" (map ppr rs)

ppr :: Rule -> [Char]
ppr (lhs ::= rhs) = lhs ++ " ::= " ++ (ppe rhs)

ppe :: Expr -> [Char]
ppe (NT n) = n
ppe (T t) = "\"" ++ t ++ "\""
ppe (Seq es) = intercalate " " (map ppe es)
ppe (Dis es) = intercalate " | " (map ppe es)
ppe (Con es) = intercalate " & " (map ppe es)
ppe (Neg e) = "!" ++ (ppe e)
ppe Epsilon = "eps"
ppe Empty = "empty"

intercalate :: [Char] -> [[Char]] -> [Char]
intercalate sep xs = foldl (\ a x -> a ++ sep ++ x) (head xs) (tail xs)

eq :: Grammar -> Grammar -> Bool
eq _ _ = False
