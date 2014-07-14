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

-- TODO
eqG :: Grammar -> Grammar -> Bool
eqG g1 g2
	= (ns == getNs g2)
	&& and (zipWith eqPs pps1 pps2)
	where
		ns = getNs g1
		pps1 = map (slicebyN g1) ns
		pps2 = map (slicebyN g2) ns

-- equality of production rules of one nonterminal
eqPs :: [Rule] -> [Rule] -> Bool
eqPs ps1 ps2 = eqEs (map rhs ps1) (map rhs ps2)

eqEs :: [Expr] -> [Expr] -> Bool
eqEs [] [] = True
eqEs _ [] = False
eqEs [] _ = False
eqEs (e1:es1) es2 = gote1 && (eqEs es1 restofes2) where (gote1,restofes2) = findremove e1 es2

findremove :: Expr -> [Expr] -> (Bool,[Expr])
findremove e [] = (False,[])
findremove e (x:xs) = if eqE e x then (True,xs) else (r,x:ys) where (r,ys) = findremove e xs

eqE :: Expr -> Expr -> Bool
eqE (NT n1) (NT n2) = n1 == n2
eqE (T t1) (T t2) = t1 == t2 -- TODO: should we add epsilon == ""?
eqE (Seq es1) (Seq es2) = and (zipWith eqE es1 es2)
eqE (Dis es1) (Dis es2) = eqEs es1 es2
eqE (Con es1) (Con es2) = eqEs es1 es2
eqE (Neg e1) (Neg e2) = eqE e1 e2
eqE Epsilon Epsilon = True
eqE Empty Empty = True
eqE _ _ = False

getNs :: Grammar -> [Name]
getNs rs = compress (map lhs rs)

compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = if elem x xs then (compress xs) else (x:compress xs)

slicebyN :: Grammar -> Name -> [Rule]
slicebyN rs n = filter (\ (x ::= _) -> x == n) rs

lhs :: Rule -> Name
lhs (n ::= _) = n

rhs :: Rule -> Expr
rhs (_ ::= e) = e

intercalate :: [Char] -> [[Char]] -> [Char]
intercalate sep xs = foldl (\ a x -> a ++ sep ++ x) (head xs) (tail xs)
