; ModuleID = 'pcfprog'


 


declare external ccc  i64* @pcf_mkclosure(i64* (i64*, i64*)*, i64, ...)    


declare external ccc  i64 @pcf_print(i64)    


define external ccc  i64* @__1(i64*  %__clo2, i64*  %__n0)    {
__1:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo2 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__countdown3 = inttoptr i64 %__r_4 to i64* 
  br label %entry0 
entry0:
  %__r_6 = inttoptr i64 0 to i64* 
  %cond5 = icmp eq i64* %__n0, %__r_6 
  br i1 %cond5, label %then1, label %else2 
then1:
  %__r_7 = inttoptr i64 0 to i64* 
  %__r_8 = inttoptr i64 0 to i64* 
  %__r_9 = ptrtoint i64* %__r_7 to i64 
  %__r_10 = ptrtoint i64* %__r_8 to i64 
  %__r_11 = add   i64 %__r_9, %__r_10 
  %reg4 = inttoptr i64 %__r_11 to i64* 
  br label %ifcont3 
else2:
  %__r_12 = inttoptr i64 0 to i64* 
  %__r_13 = ptrtoint i64* %__countdown3 to i64 
  %__r_14 = ptrtoint i64* %__r_12 to i64 
  %__r_15 = add   i64 %__r_13, %__r_14 
  %__e4 = inttoptr i64 %__r_15 to i64* 
  %__r_17 = bitcast i64* %__e4 to i64** 
  %addr16 = getelementptr  i64*, i64** %__r_17, i64 0 
  %reg6 = load  i64*, i64** %addr16 
  %__r_18 = inttoptr i64 1 to i64* 
  %__r_19 = inttoptr i64 0 to i64* 
  %__r_20 = ptrtoint i64* %__r_18 to i64 
  %__r_21 = ptrtoint i64* %__r_19 to i64 
  %__r_22 = add   i64 %__r_20, %__r_21 
  %reg8 = inttoptr i64 %__r_22 to i64* 
  %__r_23 = ptrtoint i64* %__n0 to i64 
  %__r_24 = ptrtoint i64* %reg8 to i64 
  %__r_25 = sub   i64 %__r_23, %__r_24 
  %__r_26 = icmp slt i64 0, %__r_25 
  %__r_27 = zext i1 %__r_26 to i64  
  %__r_28 = mul   i64 %__r_25, %__r_27 
  %reg7 = inttoptr i64 %__r_28 to i64* 
  %fun29 = bitcast i64* %reg6 to i64* (i64*, i64*)* 
  %reg5 =  call ccc  i64*  %fun29(i64*  %__e4, i64*  %reg7)  
  br label %ifcont3 
ifcont3:
  %regcont9 = phi i64* [%reg4, %then1], [%reg5, %else2] 
  ret i64* %regcont9 
}


@ans = internal   global i64* zeroinitializer


define external ccc  i64* @pcfmain()    {
pcfmain:
  %reg10 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__1, i64  0)  
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %reg10 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__e5 = inttoptr i64 %__r_4 to i64* 
  %__r_6 = bitcast i64* %__e5 to i64** 
  %addr5 = getelementptr  i64*, i64** %__r_6, i64 0 
  %reg12 = load  i64*, i64** %addr5 
  %__r_7 = inttoptr i64 345 to i64* 
  %__r_8 = inttoptr i64 0 to i64* 
  %__r_9 = ptrtoint i64* %__r_7 to i64 
  %__r_10 = ptrtoint i64* %__r_8 to i64 
  %__r_11 = add   i64 %__r_9, %__r_10 
  %reg13 = inttoptr i64 %__r_11 to i64* 
  %fun12 = bitcast i64* %reg12 to i64* (i64*, i64*)* 
  %reg11 =  call ccc  i64*  %fun12(i64*  %__e5, i64*  %reg13)  
  %__r_13 = ptrtoint i64* %reg11 to i64 
  %__r_14 =  call ccc  i64  @pcf_print(i64  %__r_13)  
  %reg14 = inttoptr i64 %__r_14 to i64* 
  %__r_15 = inttoptr i64 0 to i64* 
  %__r_16 = ptrtoint i64* %reg14 to i64 
  %__r_17 = ptrtoint i64* %__r_15 to i64 
  %__r_18 = add   i64 %__r_16, %__r_17 
  %__r_19 = inttoptr i64 %__r_18 to i64* 
  store  i64* %__r_19, i64** @ans 
  ret i64* %reg11 
}