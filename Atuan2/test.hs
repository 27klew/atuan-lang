

-- e17 = ELetRec "fun" (EAbs "x" 
--         (
--             EIf (ELit $ LBool True) 
--                 (EApp (EVar "fun") (ELit $ LBool True))
--                 (EVar "x")
--         )
--     ) 
--     (
--         EApp (EVar "fun") (ELit $ LInt 5)
--     )



e17 = let fun x = if True then fun 4 else x in fun 5


f x =
	if True then f 4 else x 


