module Grammars
where

type Grammar = [Rule]
data Rule = Prod Name Expr
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
