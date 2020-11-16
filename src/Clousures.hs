import Lang

closureConvert :: Term -> StateT Int (Writer [IrDecl]) IrTm
closureConvert (V _ var)               = return $ IrVar (show var)
closureConvert (Const _ const)         = return $ IrConst const
closureConvert (Lam _ _ _ t)           = 
closureConvert (App _ f e)             = do MkClosure n ts <- closureConvert f
                                            return $ IrCall ts[0] ts -- donde va el e??? solo tenemos para pasarle esos 2 arg  
closureConvert (BinaryOp _ op t1 t2)   = do ct1 <- closureConvert t1
                                            ct2 <- closureConvert t2 
                                            return $ IrBinaryOp op ct1 ct2 
closureConvert (Fix _ _ _ _ _ t)       =  
closureConvert (IfZ _ c t1 t2)         = do cc <- closureConvert c
                                            ct1 <- closureConvert t1
                                            ct2 <- closureConvert t2
                                            return $ IrIfZ cc ct1 ct2 
closureConvert (LetIn _ _ _ t1 t2)     = 
--closureConvert x                     = 


runCC :: [Decl Term] -> [IrDecl]
runCC []         = []
runCC (d:ds)     = let (irtm,decls) = runWriter (closureConvert term)
                   in ((makeVal irtm):decls)
                         
                      
                      

--"__var" ++ show(cont)
