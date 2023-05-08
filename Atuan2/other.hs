
test' :: Exp -> IO ()
test' e =
    do (res, _) <- runTI (bu Set.empty e)
       case res of
         Left err -> putStrLn $ "error: " ++ err
         Right t  -> putStrLn $ show e ++ " :: " ++ show t

data Constraint = CEquivalent Type Type
                | CExplicitInstance Type Scheme
                | CImplicitInstance Type (Set.Set String) Type

instance Show Constraint where
    showsPrec _ x = shows (prConstraint x)

prConstraint :: Constraint -> PP.Doc
prConstraint (CEquivalent t1 t2) = PP.hsep [prType t1, PP.text "=", prType t2]
prConstraint (CExplicitInstance t s) =
    PP.hsep [prType t, PP.text "<~", prScheme s]
prConstraint (CImplicitInstance t1 m t2) =
    PP.hsep [prType t1,
             PP.text "<=" PP.<>
               PP.parens (PP.hcat (PP.punctuate PP.comma (map PP.text (Set.toList m)))),
             prType t2]

type Assum = [(String, Type)]
type CSet = [Constraint]

bu :: Set.Set String -> Exp -> TI (Assum, CSet, Type)
bu m (EVar n) = do b <- newTyVar "b"
                   return ([(n, b)], [], b)
bu m (ELit (LInt _)) = do b <- newTyVar "b"
                          return ([], [CEquivalent b TInt], b)
bu m (ELit (LBool _)) = do b <- newTyVar "b"
                           return ([], [CEquivalent b TBool], b)
bu m (EApp e1 e2) =
    do (a1, c1, t1) <- bu m e1
       (a2, c2, t2) <- bu m e2
       b <- newTyVar "b"
       return (a1 ++ a2, c1 ++ c2 ++ [CEquivalent t1 (TFun t2 b)],
               b)
bu m (EAbs x body) =
    do b@(TVar vn) <- newTyVar "b"
       (a, c, t) <- bu (vn `Set.insert` m) body
       return (a `removeAssum` x, c ++ [CEquivalent t' b | (x', t') <- a,
                                        x == x'], TFun b t)
bu m (ELet x e1 e2) =
    do (a1, c1, t1) <- bu m e1
       (a2, c2, t2) <- bu (x `Set.delete` m) e2
       return (a1 ++ removeAssum a2 x,
               c1 ++ c2 ++ [CImplicitInstance t' m t1 |
                            (x', t') <- a2, x' == x], t2)

removeAssum [] _ = []
removeAssum ((n', _) : as) n | n == n' = removeAssum as n
removeAssum (a:as) n = a : removeAssum as n

