module Transformations
where
import Grammars

data Step
	= RenameN Name Name
	| Fold Name Expr

runT :: Step -> Grammar -> Grammar
runT s g1 = if (eq g2 g1) then error "Vacuous!" else g2 where g2 = jrunT s g1

jrunT :: Step -> Grammar -> Grammar
jrunT (RenameN n1 n2) = map (renaming n1 n2)
jrunT _ = id

renaming :: Name -> Name -> Rule -> Rule
renaming n1 n2 (n3 ::= e) = (if n1 == n3 then n2 else n3) ::= renamingE n1 n2 e

renamingE :: Name -> Name -> Expr -> Expr
renamingE n1 n2 (NT n3) = NT (if n1 == n3 then n2 else n3)
renamingE n1 n2 (Seq es) = Seq (map (renamingE n1 n2) es)
renamingE n1 n2 (Dis es) = Dis (map (renamingE n1 n2) es)
renamingE n1 n2 (Con es) = Con (map (renamingE n1 n2) es)
renamingE n1 n2 (Neg e) = Neg (renamingE n1 n2 e)
renamingE _ _ e = e
