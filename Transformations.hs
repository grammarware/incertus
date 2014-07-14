module Transformations
where
import Grammars

data Step
	= RenameN Name Name
	| RenameT Name Name
	| Fold Name Expr

runT :: Step -> Grammar -> Grammar
runT s g1 = if (eqG g2 g1) then error "Vacuous!" else g2 where g2 = jrunT s g1

jrunT :: Step -> Grammar -> Grammar
jrunT (RenameN n1 n2) = map (renamingN n1 n2)
jrunT (RenameT t1 t2) = map (renamingT t1 t2)
jrunT _ = id

renamingN :: Name -> Name -> Rule -> Rule
renamingN n1 n2 (n3 ::= e) = (if n1 == n3 then n2 else n3) ::= replaceAtom (renamingNinE n1 n2) e

renamingT :: Name -> Name -> Rule -> Rule
renamingT t1 t2 (n ::= e) = n ::= replaceAtom (renamingTinE t1 t2) e

renamingNinE :: Name -> Name -> Expr -> Expr
renamingNinE n1 n2 (NT n3) = NT (if n1 == n3 then n2 else n3)
renamingNinE _ _ e = e

renamingTinE :: Name -> Name -> Expr -> Expr
renamingTinE t1 t2 (T t3) = T (if t1 == t3 then t2 else t3)
renamingTinE _ _ e = e

replaceAtom :: (Expr -> Expr) -> Expr -> Expr
replaceAtom f (Seq es) = Seq (map (replaceAtom f) es)
replaceAtom f (Dis es) = Dis (map (replaceAtom f) es)
replaceAtom f (Con es) = Con (map (replaceAtom f) es)
replaceAtom f (Neg e) = Neg (replaceAtom f e)
replaceAtom f e = f e
