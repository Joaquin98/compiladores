; ModuleID = 'pcfprog'


 


declare external ccc  i32* @pcf_mkclosure(i32* (i32*, i32*)*, i32, ...)    


declare external ccc  i32 @pcf_print(i32)    


define external ccc  i32* @__5(i32*  %__clo6, i32*  %__x4)    {
__5:
  %__r_1 = inttoptr i32 45 to i32* 
  %__r_2 = inttoptr i32 0 to i32* 
  %__r_3 = ptrtoint i32* %__r_1 to i32 
  %__r_4 = ptrtoint i32* %__r_2 to i32 
  %__r_5 = add   i32 %__r_3, %__r_4 
  %__y7 = inttoptr i32 %__r_5 to i32* 
  %__r_6 = load  i32*, i32** @f 
  %__r_7 = inttoptr i32 0 to i32* 
  %__r_8 = ptrtoint i32* %__r_6 to i32 
  %__r_9 = ptrtoint i32* %__r_7 to i32 
  %__r_10 = add   i32 %__r_8, %__r_9 
  %__e8 = inttoptr i32 %__r_10 to i32* 
  %__r_12 = load  i32*, i32** @__e8 
  %__r_13 = bitcast i32* %__r_12 to i32** 
  %addr11 = getelementptr  i32*, i32** %__r_13, i32 0 
  %reg1 = load  i32*, i32** %addr11 
  %fun14 = bitcast i32* %reg1 to i32* (i32*, i32*)* 
  %__r_15 = load  i32*, i32** @__e8 
  %__r_16 = load  i32*, i32** @__x4 
  %reg0 =  call ccc  i32*  %fun14(i32*  %__r_15, i32*  %__r_16)  
  ret i32* %reg0 
}


define external ccc  i32* @__1(i32*  %__clo2, i32*  %__x0)    {
__1:
  %__r_1 = load  i32*, i32** @__x0 
  %__r_2 = load  i32*, i32** @__x0 
  %__r_3 = ptrtoint i32* %__r_1 to i32 
  %__r_4 = ptrtoint i32* %__r_2 to i32 
  %__r_5 = add   i32 %__r_3, %__r_4 
  %reg2 = inttoptr i32 %__r_5 to i32* 
  ret i32* %reg2 
}


@g = internal   global i32* zeroinitializer


@z = internal   global i32* zeroinitializer


@f = internal   global i32* zeroinitializer


define external ccc  i32* @pcfmain()    {
pcfmain:
  %reg3 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__5, i32  0)  
  %__r_1 = inttoptr i32 0 to i32* 
  %__r_2 = ptrtoint i32* %reg3 to i32 
  %__r_3 = ptrtoint i32* %__r_1 to i32 
  %__r_4 = add   i32 %__r_2, %__r_3 
  %__r_5 = inttoptr i32 %__r_4 to i32* 
  store  i32* %__r_5, i32** @g 
  %__r_6 = load  i32*, i32** @f 
  %__r_7 = inttoptr i32 0 to i32* 
  %__r_8 = ptrtoint i32* %__r_6 to i32 
  %__r_9 = ptrtoint i32* %__r_7 to i32 
  %__r_10 = add   i32 %__r_8, %__r_9 
  %__e3 = inttoptr i32 %__r_10 to i32* 
  %__r_12 = load  i32*, i32** @__e3 
  %__r_13 = bitcast i32* %__r_12 to i32** 
  %addr11 = getelementptr  i32*, i32** %__r_13, i32 0 
  %reg5 = load  i32*, i32** %addr11 
  %fun14 = bitcast i32* %reg5 to i32* (i32*, i32*)* 
  %__r_15 = load  i32*, i32** @__e3 
  %__r_16 = inttoptr i32 1 to i32* 
  %reg4 =  call ccc  i32*  %fun14(i32*  %__r_15, i32*  %__r_16)  
  %__r_17 = inttoptr i32 0 to i32* 
  %__r_18 = ptrtoint i32* %reg4 to i32 
  %__r_19 = ptrtoint i32* %__r_17 to i32 
  %__r_20 = add   i32 %__r_18, %__r_19 
  %__r_21 = inttoptr i32 %__r_20 to i32* 
  store  i32* %__r_21, i32** @z 
  %reg6 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__1, i32  0)  
  %__r_22 = inttoptr i32 0 to i32* 
  %__r_23 = ptrtoint i32* %reg6 to i32 
  %__r_24 = ptrtoint i32* %__r_22 to i32 
  %__r_25 = add   i32 %__r_23, %__r_24 
  %__r_26 = inttoptr i32 %__r_25 to i32* 
  store  i32* %__r_26, i32** @f 
  ret i32* %reg6 
}