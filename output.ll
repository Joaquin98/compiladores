; ModuleID = 'pcfprog'


 


declare external ccc  i32* @pcf_mkclosure(i32* (i32*, i32*)*, i32, ...)    


declare external ccc  i32 @pcf_print(i32)    


define external ccc  i32* @__1(i32*  %__clo2, i32*  %__x0)    {
__1:
  %__r_1 = inttoptr i32 0 to i32* 
  %__r_2 = ptrtoint i32* %__clo2 to i32 
  %__r_3 = ptrtoint i32* %__r_1 to i32 
  %__r_4 = add   i32 %__r_2, %__r_3 
  %__fib3 = inttoptr i32 %__r_4 to i32* 
  br label %entry0 
entry0:
  %__r_6 = inttoptr i32 0 to i32* 
  %cond5 = icmp eq i32* %__x0, %__r_6 
  br i1 %cond5, label %then1, label %else2 
then1:
  br label %ifcont3 
else2:
  br label %entry4 
entry4:
  %__r_7 = inttoptr i32 1 to i32* 
  %__r_8 = ptrtoint i32* %__x0 to i32 
  %__r_9 = ptrtoint i32* %__r_7 to i32 
  %__r_10 = sub   i32 %__r_8, %__r_9 
  %reg8 = inttoptr i32 %__r_10 to i32* 
  %__r_12 = inttoptr i32 0 to i32* 
  %cond11 = icmp eq i32* %reg8, %__r_12 
  br i1 %cond11, label %then5, label %else6 
then5:
  br label %ifcont7 
else6:
  %__r_13 = inttoptr i32 0 to i32* 
  %__r_14 = ptrtoint i32* %__fib3 to i32 
  %__r_15 = ptrtoint i32* %__r_13 to i32 
  %__r_16 = add   i32 %__r_14, %__r_15 
  %__e4 = inttoptr i32 %__r_16 to i32* 
  %__r_18 = bitcast i32* %__e4 to i32** 
  %addr17 = getelementptr  i32*, i32** %__r_18, i32 0 
  %reg11 = load  i32*, i32** %addr17 
  %__r_19 = inttoptr i32 1 to i32* 
  %__r_20 = ptrtoint i32* %__x0 to i32 
  %__r_21 = ptrtoint i32* %__r_19 to i32 
  %__r_22 = sub   i32 %__r_20, %__r_21 
  %reg12 = inttoptr i32 %__r_22 to i32* 
  %fun23 = bitcast i32* %reg11 to i32* (i32*, i32*)* 
  %reg10 =  call ccc  i32*  %fun23(i32*  %__e4, i32*  %reg12)  
  %__r_24 = inttoptr i32 0 to i32* 
  %__r_25 = ptrtoint i32* %__fib3 to i32 
  %__r_26 = ptrtoint i32* %__r_24 to i32 
  %__r_27 = add   i32 %__r_25, %__r_26 
  %__e5 = inttoptr i32 %__r_27 to i32* 
  %__r_29 = bitcast i32* %__e5 to i32** 
  %addr28 = getelementptr  i32*, i32** %__r_29, i32 0 
  %reg14 = load  i32*, i32** %addr28 
  %__r_30 = inttoptr i32 1 to i32* 
  %__r_31 = ptrtoint i32* %__x0 to i32 
  %__r_32 = ptrtoint i32* %__r_30 to i32 
  %__r_33 = sub   i32 %__r_31, %__r_32 
  %reg16 = inttoptr i32 %__r_33 to i32* 
  %__r_34 = inttoptr i32 1 to i32* 
  %__r_35 = ptrtoint i32* %reg16 to i32 
  %__r_36 = ptrtoint i32* %__r_34 to i32 
  %__r_37 = sub   i32 %__r_35, %__r_36 
  %reg15 = inttoptr i32 %__r_37 to i32* 
  %fun38 = bitcast i32* %reg14 to i32* (i32*, i32*)* 
  %reg13 =  call ccc  i32*  %fun38(i32*  %__e5, i32*  %reg15)  
  %__r_39 = ptrtoint i32* %reg10 to i32 
  %__r_40 = ptrtoint i32* %reg13 to i32 
  %__r_41 = add   i32 %__r_39, %__r_40 
  %reg9 = inttoptr i32 %__r_41 to i32* 
  br label %ifcont7 
ifcont7:
  %__r_42 = inttoptr i32 1 to i32* 
  %regcont17 = phi i32* [%__r_42, %then5], [%reg9, %else6] 
  br label %ifcont3 
ifcont3:
  %__r_43 = inttoptr i32 1 to i32* 
  %regcont18 = phi i32* [%__r_43, %then1], [%regcont17, %else2] 
  ret i32* %regcont18 
}


@fib = internal   global i32* zeroinitializer


define external ccc  i32* @pcfmain()    {
pcfmain:
  %reg19 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__1, i32  0)  
  %__r_1 = inttoptr i32 0 to i32* 
  %__r_2 = ptrtoint i32* %reg19 to i32 
  %__r_3 = ptrtoint i32* %__r_1 to i32 
  %__r_4 = add   i32 %__r_2, %__r_3 
  %__r_5 = inttoptr i32 %__r_4 to i32* 
  store  i32* %__r_5, i32** @fib 
  ret i32* %reg19 
}