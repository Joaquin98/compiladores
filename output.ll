; ModuleID = 'pcfprog'


 


declare external ccc  i32* @pcf_mkclosure(i32* (i32*, i32*)*, i32, ...)    


declare external ccc  i32 @pcf_print(i32)    


define external ccc  i32* @__135(i32*  %__clo136, i32*  %__x134)    {
__135:
  %__r_1 = inttoptr i32 0 to i32* 
  %__r_2 = ptrtoint i32* %__clo136 to i32 
  %__r_3 = ptrtoint i32* %__r_1 to i32 
  %__r_4 = add   i32 %__r_2, %__r_3 
  %__suman137 = inttoptr i32 %__r_4 to i32* 
  br label %entry0 
entry0:
  %__r_6 = inttoptr i32 0 to i32* 
  %cond5 = icmp eq i32* %__x134, %__r_6 
  br i1 %cond5, label %then1, label %else2 
then1:
  %__r_7 = inttoptr i32 0 to i32* 
  %__r_8 = inttoptr i32 0 to i32* 
  %__r_9 = ptrtoint i32* %__r_7 to i32 
  %__r_10 = ptrtoint i32* %__r_8 to i32 
  %__r_11 = add   i32 %__r_9, %__r_10 
  %reg4 = inttoptr i32 %__r_11 to i32* 
  br label %ifcont3 
else2:
  %__r_12 = inttoptr i32 0 to i32* 
  %__r_13 = ptrtoint i32* %__suman137 to i32 
  %__r_14 = ptrtoint i32* %__r_12 to i32 
  %__r_15 = add   i32 %__r_13, %__r_14 
  %__e138 = inttoptr i32 %__r_15 to i32* 
  %__r_17 = bitcast i32* %__e138 to i32** 
  %addr16 = getelementptr  i32*, i32** %__r_17, i32 0 
  %reg7 = load  i32*, i32** %addr16 
  %__r_18 = inttoptr i32 1 to i32* 
  %__r_19 = inttoptr i32 0 to i32* 
  %__r_20 = ptrtoint i32* %__r_18 to i32 
  %__r_21 = ptrtoint i32* %__r_19 to i32 
  %__r_22 = add   i32 %__r_20, %__r_21 
  %reg9 = inttoptr i32 %__r_22 to i32* 
  %__r_23 = ptrtoint i32* %__x134 to i32 
  %__r_24 = ptrtoint i32* %reg9 to i32 
  %__r_25 = sub   i32 %__r_23, %__r_24 
  %reg8 = inttoptr i32 %__r_25 to i32* 
  %fun26 = bitcast i32* %reg7 to i32* (i32*, i32*)* 
  %reg6 =  call ccc  i32*  %fun26(i32*  %__e138, i32*  %reg8)  
  %__r_27 = ptrtoint i32* %__x134 to i32 
  %__r_28 = ptrtoint i32* %reg6 to i32 
  %__r_29 = add   i32 %__r_27, %__r_28 
  %reg5 = inttoptr i32 %__r_29 to i32* 
  br label %ifcont3 
ifcont3:
  %regcont10 = phi i32* [%reg4, %then1], [%reg5, %else2] 
  ret i32* %regcont10 
}


define external ccc  i32* @__132(i32*  %__clo133, i32*  %__x131)    {
__132:
  %__r_1 = ptrtoint i32* %__x131 to i32 
  %__r_2 = ptrtoint i32* %__x131 to i32 
  %__r_3 = add   i32 %__r_1, %__r_2 
  %reg11 = inttoptr i32 %__r_3 to i32* 
  ret i32* %reg11 
}


define external ccc  i32* @__122(i32*  %__clo123, i32*  %__x121)    {
__122:
  %reg12 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__125, i32  1, i32*  %__x121)  
  ret i32* %reg12 
}


define external ccc  i32* @__125(i32*  %__clo126, i32*  %__y124)    {
__125:
  %__r_2 = bitcast i32* %__clo126 to i32** 
  %addr1 = getelementptr  i32*, i32** %__r_2, i32 1 
  %reg13 = load  i32*, i32** %addr1 
  %__r_3 = inttoptr i32 0 to i32* 
  %__r_4 = ptrtoint i32* %reg13 to i32 
  %__r_5 = ptrtoint i32* %__r_3 to i32 
  %__r_6 = add   i32 %__r_4, %__r_5 
  %__x121 = inttoptr i32 %__r_6 to i32* 
  %reg14 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__128, i32  2, i32*  %__x121, i32*  %__y124)  
  ret i32* %reg14 
}


define external ccc  i32* @__128(i32*  %__clo129, i32*  %__f127)    {
__128:
  %__r_2 = bitcast i32* %__clo129 to i32** 
  %addr1 = getelementptr  i32*, i32** %__r_2, i32 1 
  %reg15 = load  i32*, i32** %addr1 
  %__r_3 = inttoptr i32 0 to i32* 
  %__r_4 = ptrtoint i32* %reg15 to i32 
  %__r_5 = ptrtoint i32* %__r_3 to i32 
  %__r_6 = add   i32 %__r_4, %__r_5 
  %__x121 = inttoptr i32 %__r_6 to i32* 
  %__r_8 = bitcast i32* %__clo129 to i32** 
  %addr7 = getelementptr  i32*, i32** %__r_8, i32 2 
  %reg16 = load  i32*, i32** %addr7 
  %__r_9 = inttoptr i32 0 to i32* 
  %__r_10 = ptrtoint i32* %reg16 to i32 
  %__r_11 = ptrtoint i32* %__r_9 to i32 
  %__r_12 = add   i32 %__r_10, %__r_11 
  %__y124 = inttoptr i32 %__r_12 to i32* 
  %__r_13 = inttoptr i32 0 to i32* 
  %__r_14 = ptrtoint i32* %__f127 to i32 
  %__r_15 = ptrtoint i32* %__r_13 to i32 
  %__r_16 = add   i32 %__r_14, %__r_15 
  %__e130 = inttoptr i32 %__r_16 to i32* 
  %__r_18 = bitcast i32* %__e130 to i32** 
  %addr17 = getelementptr  i32*, i32** %__r_18, i32 0 
  %reg18 = load  i32*, i32** %addr17 
  %__r_19 = ptrtoint i32* %__x121 to i32 
  %__r_20 = ptrtoint i32* %__y124 to i32 
  %__r_21 = add   i32 %__r_19, %__r_20 
  %reg19 = inttoptr i32 %__r_21 to i32* 
  %fun22 = bitcast i32* %reg18 to i32* (i32*, i32*)* 
  %reg17 =  call ccc  i32*  %fun22(i32*  %__e130, i32*  %reg19)  
  ret i32* %reg17 
}


define external ccc  i32* @__116(i32*  %__clo117, i32*  %__x115)    {
__116:
  %__r_1 = inttoptr i32 0 to i32* 
  %__r_2 = ptrtoint i32* %__clo117 to i32 
  %__r_3 = ptrtoint i32* %__r_1 to i32 
  %__r_4 = add   i32 %__r_2, %__r_3 
  %__fib118 = inttoptr i32 %__r_4 to i32* 
  br label %entry20 
entry20:
  %__r_6 = inttoptr i32 0 to i32* 
  %cond5 = icmp eq i32* %__x115, %__r_6 
  br i1 %cond5, label %then21, label %else22 
then21:
  %__r_7 = inttoptr i32 1 to i32* 
  %__r_8 = inttoptr i32 0 to i32* 
  %__r_9 = ptrtoint i32* %__r_7 to i32 
  %__r_10 = ptrtoint i32* %__r_8 to i32 
  %__r_11 = add   i32 %__r_9, %__r_10 
  %reg24 = inttoptr i32 %__r_11 to i32* 
  br label %ifcont23 
else22:
  br label %entry25 
entry25:
  %__r_12 = inttoptr i32 1 to i32* 
  %__r_13 = inttoptr i32 0 to i32* 
  %__r_14 = ptrtoint i32* %__r_12 to i32 
  %__r_15 = ptrtoint i32* %__r_13 to i32 
  %__r_16 = add   i32 %__r_14, %__r_15 
  %reg30 = inttoptr i32 %__r_16 to i32* 
  %__r_17 = ptrtoint i32* %__x115 to i32 
  %__r_18 = ptrtoint i32* %reg30 to i32 
  %__r_19 = sub   i32 %__r_17, %__r_18 
  %reg29 = inttoptr i32 %__r_19 to i32* 
  %__r_21 = inttoptr i32 0 to i32* 
  %cond20 = icmp eq i32* %reg29, %__r_21 
  br i1 %cond20, label %then26, label %else27 
then26:
  %__r_22 = inttoptr i32 1 to i32* 
  %__r_23 = inttoptr i32 0 to i32* 
  %__r_24 = ptrtoint i32* %__r_22 to i32 
  %__r_25 = ptrtoint i32* %__r_23 to i32 
  %__r_26 = add   i32 %__r_24, %__r_25 
  %reg31 = inttoptr i32 %__r_26 to i32* 
  br label %ifcont28 
else27:
  %__r_27 = inttoptr i32 0 to i32* 
  %__r_28 = ptrtoint i32* %__fib118 to i32 
  %__r_29 = ptrtoint i32* %__r_27 to i32 
  %__r_30 = add   i32 %__r_28, %__r_29 
  %__e119 = inttoptr i32 %__r_30 to i32* 
  %__r_32 = bitcast i32* %__e119 to i32** 
  %addr31 = getelementptr  i32*, i32** %__r_32, i32 0 
  %reg34 = load  i32*, i32** %addr31 
  %__r_33 = inttoptr i32 1 to i32* 
  %__r_34 = inttoptr i32 0 to i32* 
  %__r_35 = ptrtoint i32* %__r_33 to i32 
  %__r_36 = ptrtoint i32* %__r_34 to i32 
  %__r_37 = add   i32 %__r_35, %__r_36 
  %reg36 = inttoptr i32 %__r_37 to i32* 
  %__r_38 = ptrtoint i32* %__x115 to i32 
  %__r_39 = ptrtoint i32* %reg36 to i32 
  %__r_40 = sub   i32 %__r_38, %__r_39 
  %reg35 = inttoptr i32 %__r_40 to i32* 
  %fun41 = bitcast i32* %reg34 to i32* (i32*, i32*)* 
  %reg33 =  call ccc  i32*  %fun41(i32*  %__e119, i32*  %reg35)  
  %__r_42 = inttoptr i32 0 to i32* 
  %__r_43 = ptrtoint i32* %__fib118 to i32 
  %__r_44 = ptrtoint i32* %__r_42 to i32 
  %__r_45 = add   i32 %__r_43, %__r_44 
  %__e120 = inttoptr i32 %__r_45 to i32* 
  %__r_47 = bitcast i32* %__e120 to i32** 
  %addr46 = getelementptr  i32*, i32** %__r_47, i32 0 
  %reg38 = load  i32*, i32** %addr46 
  %__r_48 = inttoptr i32 1 to i32* 
  %__r_49 = inttoptr i32 0 to i32* 
  %__r_50 = ptrtoint i32* %__r_48 to i32 
  %__r_51 = ptrtoint i32* %__r_49 to i32 
  %__r_52 = add   i32 %__r_50, %__r_51 
  %reg41 = inttoptr i32 %__r_52 to i32* 
  %__r_53 = ptrtoint i32* %__x115 to i32 
  %__r_54 = ptrtoint i32* %reg41 to i32 
  %__r_55 = sub   i32 %__r_53, %__r_54 
  %reg40 = inttoptr i32 %__r_55 to i32* 
  %__r_56 = inttoptr i32 1 to i32* 
  %__r_57 = inttoptr i32 0 to i32* 
  %__r_58 = ptrtoint i32* %__r_56 to i32 
  %__r_59 = ptrtoint i32* %__r_57 to i32 
  %__r_60 = add   i32 %__r_58, %__r_59 
  %reg42 = inttoptr i32 %__r_60 to i32* 
  %__r_61 = ptrtoint i32* %reg40 to i32 
  %__r_62 = ptrtoint i32* %reg42 to i32 
  %__r_63 = sub   i32 %__r_61, %__r_62 
  %reg39 = inttoptr i32 %__r_63 to i32* 
  %fun64 = bitcast i32* %reg38 to i32* (i32*, i32*)* 
  %reg37 =  call ccc  i32*  %fun64(i32*  %__e120, i32*  %reg39)  
  %__r_65 = ptrtoint i32* %reg33 to i32 
  %__r_66 = ptrtoint i32* %reg37 to i32 
  %__r_67 = add   i32 %__r_65, %__r_66 
  %reg32 = inttoptr i32 %__r_67 to i32* 
  br label %ifcont28 
ifcont28:
  %regcont43 = phi i32* [%reg31, %then26], [%reg32, %else27] 
  br label %ifcont23 
ifcont23:
  %regcont44 = phi i32* [%reg24, %then21], [%regcont43, %ifcont28] 
  ret i32* %regcont44 
}


define external ccc  i32* @__107(i32*  %__clo108, i32*  %__x106)    {
__107:
  %__r_1 = inttoptr i32 0 to i32* 
  %__r_2 = ptrtoint i32* %__clo108 to i32 
  %__r_3 = ptrtoint i32* %__r_1 to i32 
  %__r_4 = add   i32 %__r_2, %__r_3 
  %__resta109 = inttoptr i32 %__r_4 to i32* 
  %reg45 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__111, i32  2, i32*  %__resta109, i32*  %__x106)  
  ret i32* %reg45 
}


define external ccc  i32* @__111(i32*  %__clo112, i32*  %__y110)    {
__111:
  %__r_2 = bitcast i32* %__clo112 to i32** 
  %addr1 = getelementptr  i32*, i32** %__r_2, i32 1 
  %reg46 = load  i32*, i32** %addr1 
  %__r_3 = inttoptr i32 0 to i32* 
  %__r_4 = ptrtoint i32* %reg46 to i32 
  %__r_5 = ptrtoint i32* %__r_3 to i32 
  %__r_6 = add   i32 %__r_4, %__r_5 
  %__resta109 = inttoptr i32 %__r_6 to i32* 
  %__r_8 = bitcast i32* %__clo112 to i32** 
  %addr7 = getelementptr  i32*, i32** %__r_8, i32 2 
  %reg47 = load  i32*, i32** %addr7 
  %__r_9 = inttoptr i32 0 to i32* 
  %__r_10 = ptrtoint i32* %reg47 to i32 
  %__r_11 = ptrtoint i32* %__r_9 to i32 
  %__r_12 = add   i32 %__r_10, %__r_11 
  %__x106 = inttoptr i32 %__r_12 to i32* 
  br label %entry48 
entry48:
  %__r_14 = inttoptr i32 0 to i32* 
  %cond13 = icmp eq i32* %__y110, %__r_14 
  br i1 %cond13, label %then49, label %else50 
then49:
  br label %ifcont51 
else50:
  br label %entry52 
entry52:
  %__r_16 = inttoptr i32 0 to i32* 
  %cond15 = icmp eq i32* %__x106, %__r_16 
  br i1 %cond15, label %then53, label %else54 
then53:
  %__r_17 = inttoptr i32 0 to i32* 
  %__r_18 = inttoptr i32 0 to i32* 
  %__r_19 = ptrtoint i32* %__r_17 to i32 
  %__r_20 = ptrtoint i32* %__r_18 to i32 
  %__r_21 = add   i32 %__r_19, %__r_20 
  %reg56 = inttoptr i32 %__r_21 to i32* 
  br label %ifcont55 
else54:
  %__r_22 = inttoptr i32 0 to i32* 
  %__r_23 = ptrtoint i32* %__resta109 to i32 
  %__r_24 = ptrtoint i32* %__r_22 to i32 
  %__r_25 = add   i32 %__r_23, %__r_24 
  %__e113 = inttoptr i32 %__r_25 to i32* 
  %__r_27 = bitcast i32* %__e113 to i32** 
  %addr26 = getelementptr  i32*, i32** %__r_27, i32 0 
  %reg58 = load  i32*, i32** %addr26 
  %__r_28 = inttoptr i32 1 to i32* 
  %__r_29 = inttoptr i32 0 to i32* 
  %__r_30 = ptrtoint i32* %__r_28 to i32 
  %__r_31 = ptrtoint i32* %__r_29 to i32 
  %__r_32 = add   i32 %__r_30, %__r_31 
  %reg60 = inttoptr i32 %__r_32 to i32* 
  %__r_33 = ptrtoint i32* %__x106 to i32 
  %__r_34 = ptrtoint i32* %reg60 to i32 
  %__r_35 = sub   i32 %__r_33, %__r_34 
  %reg59 = inttoptr i32 %__r_35 to i32* 
  %fun36 = bitcast i32* %reg58 to i32* (i32*, i32*)* 
  %reg57 =  call ccc  i32*  %fun36(i32*  %__e113, i32*  %reg59)  
  %__r_37 = inttoptr i32 0 to i32* 
  %__r_38 = ptrtoint i32* %reg57 to i32 
  %__r_39 = ptrtoint i32* %__r_37 to i32 
  %__r_40 = add   i32 %__r_38, %__r_39 
  %__e114 = inttoptr i32 %__r_40 to i32* 
  %__r_42 = bitcast i32* %__e114 to i32** 
  %addr41 = getelementptr  i32*, i32** %__r_42, i32 0 
  %reg62 = load  i32*, i32** %addr41 
  %__r_43 = inttoptr i32 1 to i32* 
  %__r_44 = inttoptr i32 0 to i32* 
  %__r_45 = ptrtoint i32* %__r_43 to i32 
  %__r_46 = ptrtoint i32* %__r_44 to i32 
  %__r_47 = add   i32 %__r_45, %__r_46 
  %reg64 = inttoptr i32 %__r_47 to i32* 
  %__r_48 = ptrtoint i32* %__y110 to i32 
  %__r_49 = ptrtoint i32* %reg64 to i32 
  %__r_50 = sub   i32 %__r_48, %__r_49 
  %reg63 = inttoptr i32 %__r_50 to i32* 
  %fun51 = bitcast i32* %reg62 to i32* (i32*, i32*)* 
  %reg61 =  call ccc  i32*  %fun51(i32*  %__e114, i32*  %reg63)  
  br label %ifcont55 
ifcont55:
  %regcont65 = phi i32* [%reg56, %then53], [%reg61, %else54] 
  br label %ifcont51 
ifcont51:
  %regcont66 = phi i32* [%__x106, %then49], [%regcont65, %ifcont55] 
  ret i32* %regcont66 
}


define external ccc  i32* @__98(i32*  %__clo99, i32*  %__x97)    {
__98:
  %__r_1 = inttoptr i32 0 to i32* 
  %__r_2 = ptrtoint i32* %__clo99 to i32 
  %__r_3 = ptrtoint i32* %__r_1 to i32 
  %__r_4 = add   i32 %__r_2, %__r_3 
  %__mult100 = inttoptr i32 %__r_4 to i32* 
  %reg67 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__102, i32  2, i32*  %__mult100, i32*  %__x97)  
  ret i32* %reg67 
}


define external ccc  i32* @__102(i32*  %__clo103, i32*  %__y101)    {
__102:
  %__r_2 = bitcast i32* %__clo103 to i32** 
  %addr1 = getelementptr  i32*, i32** %__r_2, i32 1 
  %reg68 = load  i32*, i32** %addr1 
  %__r_3 = inttoptr i32 0 to i32* 
  %__r_4 = ptrtoint i32* %reg68 to i32 
  %__r_5 = ptrtoint i32* %__r_3 to i32 
  %__r_6 = add   i32 %__r_4, %__r_5 
  %__mult100 = inttoptr i32 %__r_6 to i32* 
  %__r_8 = bitcast i32* %__clo103 to i32** 
  %addr7 = getelementptr  i32*, i32** %__r_8, i32 2 
  %reg69 = load  i32*, i32** %addr7 
  %__r_9 = inttoptr i32 0 to i32* 
  %__r_10 = ptrtoint i32* %reg69 to i32 
  %__r_11 = ptrtoint i32* %__r_9 to i32 
  %__r_12 = add   i32 %__r_10, %__r_11 
  %__x97 = inttoptr i32 %__r_12 to i32* 
  br label %entry70 
entry70:
  %__r_14 = inttoptr i32 0 to i32* 
  %cond13 = icmp eq i32* %__y101, %__r_14 
  br i1 %cond13, label %then71, label %else72 
then71:
  %__r_15 = inttoptr i32 0 to i32* 
  %__r_16 = inttoptr i32 0 to i32* 
  %__r_17 = ptrtoint i32* %__r_15 to i32 
  %__r_18 = ptrtoint i32* %__r_16 to i32 
  %__r_19 = add   i32 %__r_17, %__r_18 
  %reg74 = inttoptr i32 %__r_19 to i32* 
  br label %ifcont73 
else72:
  %__r_20 = inttoptr i32 0 to i32* 
  %__r_21 = ptrtoint i32* %__mult100 to i32 
  %__r_22 = ptrtoint i32* %__r_20 to i32 
  %__r_23 = add   i32 %__r_21, %__r_22 
  %__e104 = inttoptr i32 %__r_23 to i32* 
  %__r_25 = bitcast i32* %__e104 to i32** 
  %addr24 = getelementptr  i32*, i32** %__r_25, i32 0 
  %reg77 = load  i32*, i32** %addr24 
  %fun26 = bitcast i32* %reg77 to i32* (i32*, i32*)* 
  %reg76 =  call ccc  i32*  %fun26(i32*  %__e104, i32*  %__x97)  
  %__r_27 = inttoptr i32 0 to i32* 
  %__r_28 = ptrtoint i32* %reg76 to i32 
  %__r_29 = ptrtoint i32* %__r_27 to i32 
  %__r_30 = add   i32 %__r_28, %__r_29 
  %__e105 = inttoptr i32 %__r_30 to i32* 
  %__r_32 = bitcast i32* %__e105 to i32** 
  %addr31 = getelementptr  i32*, i32** %__r_32, i32 0 
  %reg79 = load  i32*, i32** %addr31 
  %__r_33 = inttoptr i32 1 to i32* 
  %__r_34 = inttoptr i32 0 to i32* 
  %__r_35 = ptrtoint i32* %__r_33 to i32 
  %__r_36 = ptrtoint i32* %__r_34 to i32 
  %__r_37 = add   i32 %__r_35, %__r_36 
  %reg81 = inttoptr i32 %__r_37 to i32* 
  %__r_38 = ptrtoint i32* %__y101 to i32 
  %__r_39 = ptrtoint i32* %reg81 to i32 
  %__r_40 = sub   i32 %__r_38, %__r_39 
  %reg80 = inttoptr i32 %__r_40 to i32* 
  %fun41 = bitcast i32* %reg79 to i32* (i32*, i32*)* 
  %reg78 =  call ccc  i32*  %fun41(i32*  %__e105, i32*  %reg80)  
  %__r_42 = ptrtoint i32* %__x97 to i32 
  %__r_43 = ptrtoint i32* %reg78 to i32 
  %__r_44 = add   i32 %__r_42, %__r_43 
  %reg75 = inttoptr i32 %__r_44 to i32* 
  br label %ifcont73 
ifcont73:
  %regcont82 = phi i32* [%reg74, %then71], [%reg75, %else72] 
  ret i32* %regcont82 
}


define external ccc  i32* @__87(i32*  %__clo88, i32*  %__x86)    {
__87:
  %__r_1 = inttoptr i32 0 to i32* 
  %__r_2 = ptrtoint i32* %__clo88 to i32 
  %__r_3 = ptrtoint i32* %__r_1 to i32 
  %__r_4 = add   i32 %__r_2, %__r_3 
  %__exp89 = inttoptr i32 %__r_4 to i32* 
  %reg83 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__91, i32  2, i32*  %__exp89, i32*  %__x86)  
  ret i32* %reg83 
}


define external ccc  i32* @__91(i32*  %__clo92, i32*  %__y90)    {
__91:
  %__r_2 = bitcast i32* %__clo92 to i32** 
  %addr1 = getelementptr  i32*, i32** %__r_2, i32 1 
  %reg84 = load  i32*, i32** %addr1 
  %__r_3 = inttoptr i32 0 to i32* 
  %__r_4 = ptrtoint i32* %reg84 to i32 
  %__r_5 = ptrtoint i32* %__r_3 to i32 
  %__r_6 = add   i32 %__r_4, %__r_5 
  %__exp89 = inttoptr i32 %__r_6 to i32* 
  %__r_8 = bitcast i32* %__clo92 to i32** 
  %addr7 = getelementptr  i32*, i32** %__r_8, i32 2 
  %reg85 = load  i32*, i32** %addr7 
  %__r_9 = inttoptr i32 0 to i32* 
  %__r_10 = ptrtoint i32* %reg85 to i32 
  %__r_11 = ptrtoint i32* %__r_9 to i32 
  %__r_12 = add   i32 %__r_10, %__r_11 
  %__x86 = inttoptr i32 %__r_12 to i32* 
  br label %entry86 
entry86:
  %__r_14 = inttoptr i32 0 to i32* 
  %cond13 = icmp eq i32* %__y90, %__r_14 
  br i1 %cond13, label %then87, label %else88 
then87:
  %__r_15 = inttoptr i32 1 to i32* 
  %__r_16 = inttoptr i32 0 to i32* 
  %__r_17 = ptrtoint i32* %__r_15 to i32 
  %__r_18 = ptrtoint i32* %__r_16 to i32 
  %__r_19 = add   i32 %__r_17, %__r_18 
  %reg90 = inttoptr i32 %__r_19 to i32* 
  br label %ifcont89 
else88:
  %__r_20 = load  i32*, i32** @mult 
  %__r_21 = inttoptr i32 0 to i32* 
  %__r_22 = ptrtoint i32* %__r_20 to i32 
  %__r_23 = ptrtoint i32* %__r_21 to i32 
  %__r_24 = add   i32 %__r_22, %__r_23 
  %__e93 = inttoptr i32 %__r_24 to i32* 
  %__r_26 = bitcast i32* %__e93 to i32** 
  %addr25 = getelementptr  i32*, i32** %__r_26, i32 0 
  %reg92 = load  i32*, i32** %addr25 
  %fun27 = bitcast i32* %reg92 to i32* (i32*, i32*)* 
  %reg91 =  call ccc  i32*  %fun27(i32*  %__e93, i32*  %__x86)  
  %__r_28 = inttoptr i32 0 to i32* 
  %__r_29 = ptrtoint i32* %reg91 to i32 
  %__r_30 = ptrtoint i32* %__r_28 to i32 
  %__r_31 = add   i32 %__r_29, %__r_30 
  %__e96 = inttoptr i32 %__r_31 to i32* 
  %__r_33 = bitcast i32* %__e96 to i32** 
  %addr32 = getelementptr  i32*, i32** %__r_33, i32 0 
  %reg94 = load  i32*, i32** %addr32 
  %__r_34 = inttoptr i32 0 to i32* 
  %__r_35 = ptrtoint i32* %__exp89 to i32 
  %__r_36 = ptrtoint i32* %__r_34 to i32 
  %__r_37 = add   i32 %__r_35, %__r_36 
  %__e94 = inttoptr i32 %__r_37 to i32* 
  %__r_39 = bitcast i32* %__e94 to i32** 
  %addr38 = getelementptr  i32*, i32** %__r_39, i32 0 
  %reg96 = load  i32*, i32** %addr38 
  %fun40 = bitcast i32* %reg96 to i32* (i32*, i32*)* 
  %reg95 =  call ccc  i32*  %fun40(i32*  %__e94, i32*  %__x86)  
  %__r_41 = inttoptr i32 0 to i32* 
  %__r_42 = ptrtoint i32* %reg95 to i32 
  %__r_43 = ptrtoint i32* %__r_41 to i32 
  %__r_44 = add   i32 %__r_42, %__r_43 
  %__e95 = inttoptr i32 %__r_44 to i32* 
  %__r_46 = bitcast i32* %__e95 to i32** 
  %addr45 = getelementptr  i32*, i32** %__r_46, i32 0 
  %reg98 = load  i32*, i32** %addr45 
  %__r_47 = inttoptr i32 1 to i32* 
  %__r_48 = inttoptr i32 0 to i32* 
  %__r_49 = ptrtoint i32* %__r_47 to i32 
  %__r_50 = ptrtoint i32* %__r_48 to i32 
  %__r_51 = add   i32 %__r_49, %__r_50 
  %reg100 = inttoptr i32 %__r_51 to i32* 
  %__r_52 = ptrtoint i32* %__y90 to i32 
  %__r_53 = ptrtoint i32* %reg100 to i32 
  %__r_54 = sub   i32 %__r_52, %__r_53 
  %reg99 = inttoptr i32 %__r_54 to i32* 
  %fun55 = bitcast i32* %reg98 to i32* (i32*, i32*)* 
  %reg97 =  call ccc  i32*  %fun55(i32*  %__e95, i32*  %reg99)  
  %fun56 = bitcast i32* %reg94 to i32* (i32*, i32*)* 
  %reg93 =  call ccc  i32*  %fun56(i32*  %__e96, i32*  %reg97)  
  br label %ifcont89 
ifcont89:
  %regcont101 = phi i32* [%reg90, %then87], [%reg93, %else88] 
  ret i32* %regcont101 
}


define external ccc  i32* @__80(i32*  %__clo81, i32*  %__x79)    {
__80:
  %__r_1 = inttoptr i32 0 to i32* 
  %__r_2 = ptrtoint i32* %__clo81 to i32 
  %__r_3 = ptrtoint i32* %__r_1 to i32 
  %__r_4 = add   i32 %__r_2, %__r_3 
  %__fact82 = inttoptr i32 %__r_4 to i32* 
  br label %entry102 
entry102:
  %__r_6 = inttoptr i32 0 to i32* 
  %cond5 = icmp eq i32* %__x79, %__r_6 
  br i1 %cond5, label %then103, label %else104 
then103:
  %__r_7 = inttoptr i32 1 to i32* 
  %__r_8 = inttoptr i32 0 to i32* 
  %__r_9 = ptrtoint i32* %__r_7 to i32 
  %__r_10 = ptrtoint i32* %__r_8 to i32 
  %__r_11 = add   i32 %__r_9, %__r_10 
  %reg106 = inttoptr i32 %__r_11 to i32* 
  br label %ifcont105 
else104:
  %__r_12 = load  i32*, i32** @mult 
  %__r_13 = inttoptr i32 0 to i32* 
  %__r_14 = ptrtoint i32* %__r_12 to i32 
  %__r_15 = ptrtoint i32* %__r_13 to i32 
  %__r_16 = add   i32 %__r_14, %__r_15 
  %__e83 = inttoptr i32 %__r_16 to i32* 
  %__r_18 = bitcast i32* %__e83 to i32** 
  %addr17 = getelementptr  i32*, i32** %__r_18, i32 0 
  %reg108 = load  i32*, i32** %addr17 
  %fun19 = bitcast i32* %reg108 to i32* (i32*, i32*)* 
  %reg107 =  call ccc  i32*  %fun19(i32*  %__e83, i32*  %__x79)  
  %__r_20 = inttoptr i32 0 to i32* 
  %__r_21 = ptrtoint i32* %reg107 to i32 
  %__r_22 = ptrtoint i32* %__r_20 to i32 
  %__r_23 = add   i32 %__r_21, %__r_22 
  %__e85 = inttoptr i32 %__r_23 to i32* 
  %__r_25 = bitcast i32* %__e85 to i32** 
  %addr24 = getelementptr  i32*, i32** %__r_25, i32 0 
  %reg110 = load  i32*, i32** %addr24 
  %__r_26 = inttoptr i32 0 to i32* 
  %__r_27 = ptrtoint i32* %__fact82 to i32 
  %__r_28 = ptrtoint i32* %__r_26 to i32 
  %__r_29 = add   i32 %__r_27, %__r_28 
  %__e84 = inttoptr i32 %__r_29 to i32* 
  %__r_31 = bitcast i32* %__e84 to i32** 
  %addr30 = getelementptr  i32*, i32** %__r_31, i32 0 
  %reg112 = load  i32*, i32** %addr30 
  %__r_32 = inttoptr i32 1 to i32* 
  %__r_33 = inttoptr i32 0 to i32* 
  %__r_34 = ptrtoint i32* %__r_32 to i32 
  %__r_35 = ptrtoint i32* %__r_33 to i32 
  %__r_36 = add   i32 %__r_34, %__r_35 
  %reg114 = inttoptr i32 %__r_36 to i32* 
  %__r_37 = ptrtoint i32* %__x79 to i32 
  %__r_38 = ptrtoint i32* %reg114 to i32 
  %__r_39 = sub   i32 %__r_37, %__r_38 
  %reg113 = inttoptr i32 %__r_39 to i32* 
  %fun40 = bitcast i32* %reg112 to i32* (i32*, i32*)* 
  %reg111 =  call ccc  i32*  %fun40(i32*  %__e84, i32*  %reg113)  
  %fun41 = bitcast i32* %reg110 to i32* (i32*, i32*)* 
  %reg109 =  call ccc  i32*  %fun41(i32*  %__e85, i32*  %reg111)  
  br label %ifcont105 
ifcont105:
  %regcont115 = phi i32* [%reg106, %then103], [%reg109, %else104] 
  ret i32* %regcont115 
}


define external ccc  i32* @__69(i32*  %__clo70, i32*  %__n68)    {
__69:
  %__r_1 = inttoptr i32 0 to i32* 
  %__r_2 = ptrtoint i32* %__clo70 to i32 
  %__r_3 = ptrtoint i32* %__r_1 to i32 
  %__r_4 = add   i32 %__r_2, %__r_3 
  %__gcd71 = inttoptr i32 %__r_4 to i32* 
  %reg116 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__73, i32  2, i32*  %__gcd71, i32*  %__n68)  
  ret i32* %reg116 
}


define external ccc  i32* @__73(i32*  %__clo74, i32*  %__m72)    {
__73:
  %__r_2 = bitcast i32* %__clo74 to i32** 
  %addr1 = getelementptr  i32*, i32** %__r_2, i32 1 
  %reg117 = load  i32*, i32** %addr1 
  %__r_3 = inttoptr i32 0 to i32* 
  %__r_4 = ptrtoint i32* %reg117 to i32 
  %__r_5 = ptrtoint i32* %__r_3 to i32 
  %__r_6 = add   i32 %__r_4, %__r_5 
  %__gcd71 = inttoptr i32 %__r_6 to i32* 
  %__r_8 = bitcast i32* %__clo74 to i32** 
  %addr7 = getelementptr  i32*, i32** %__r_8, i32 2 
  %reg118 = load  i32*, i32** %addr7 
  %__r_9 = inttoptr i32 0 to i32* 
  %__r_10 = ptrtoint i32* %reg118 to i32 
  %__r_11 = ptrtoint i32* %__r_9 to i32 
  %__r_12 = add   i32 %__r_10, %__r_11 
  %__n68 = inttoptr i32 %__r_12 to i32* 
  br label %entry119 
entry119:
  %__r_14 = inttoptr i32 0 to i32* 
  %cond13 = icmp eq i32* %__n68, %__r_14 
  br i1 %cond13, label %then120, label %else121 
then120:
  br label %ifcont122 
else121:
  br label %entry123 
entry123:
  %__r_16 = inttoptr i32 0 to i32* 
  %cond15 = icmp eq i32* %__m72, %__r_16 
  br i1 %cond15, label %then124, label %else125 
then124:
  br label %ifcont126 
else125:
  br label %entry127 
entry127:
  %__r_17 = ptrtoint i32* %__n68 to i32 
  %__r_18 = ptrtoint i32* %__m72 to i32 
  %__r_19 = sub   i32 %__r_17, %__r_18 
  %reg131 = inttoptr i32 %__r_19 to i32* 
  %__r_21 = inttoptr i32 0 to i32* 
  %cond20 = icmp eq i32* %reg131, %__r_21 
  br i1 %cond20, label %then128, label %else129 
then128:
  %__r_22 = inttoptr i32 0 to i32* 
  %__r_23 = ptrtoint i32* %__gcd71 to i32 
  %__r_24 = ptrtoint i32* %__r_22 to i32 
  %__r_25 = add   i32 %__r_23, %__r_24 
  %__e75 = inttoptr i32 %__r_25 to i32* 
  %__r_27 = bitcast i32* %__e75 to i32** 
  %addr26 = getelementptr  i32*, i32** %__r_27, i32 0 
  %reg133 = load  i32*, i32** %addr26 
  %__r_28 = ptrtoint i32* %__m72 to i32 
  %__r_29 = ptrtoint i32* %__n68 to i32 
  %__r_30 = sub   i32 %__r_28, %__r_29 
  %reg134 = inttoptr i32 %__r_30 to i32* 
  %fun31 = bitcast i32* %reg133 to i32* (i32*, i32*)* 
  %reg132 =  call ccc  i32*  %fun31(i32*  %__e75, i32*  %reg134)  
  %__r_32 = inttoptr i32 0 to i32* 
  %__r_33 = ptrtoint i32* %reg132 to i32 
  %__r_34 = ptrtoint i32* %__r_32 to i32 
  %__r_35 = add   i32 %__r_33, %__r_34 
  %__e76 = inttoptr i32 %__r_35 to i32* 
  %__r_37 = bitcast i32* %__e76 to i32** 
  %addr36 = getelementptr  i32*, i32** %__r_37, i32 0 
  %reg136 = load  i32*, i32** %addr36 
  %fun38 = bitcast i32* %reg136 to i32* (i32*, i32*)* 
  %reg135 =  call ccc  i32*  %fun38(i32*  %__e76, i32*  %__n68)  
  br label %ifcont130 
else129:
  %__r_39 = inttoptr i32 0 to i32* 
  %__r_40 = ptrtoint i32* %__gcd71 to i32 
  %__r_41 = ptrtoint i32* %__r_39 to i32 
  %__r_42 = add   i32 %__r_40, %__r_41 
  %__e77 = inttoptr i32 %__r_42 to i32* 
  %__r_44 = bitcast i32* %__e77 to i32** 
  %addr43 = getelementptr  i32*, i32** %__r_44, i32 0 
  %reg138 = load  i32*, i32** %addr43 
  %fun45 = bitcast i32* %reg138 to i32* (i32*, i32*)* 
  %reg137 =  call ccc  i32*  %fun45(i32*  %__e77, i32*  %__m72)  
  %__r_46 = inttoptr i32 0 to i32* 
  %__r_47 = ptrtoint i32* %reg137 to i32 
  %__r_48 = ptrtoint i32* %__r_46 to i32 
  %__r_49 = add   i32 %__r_47, %__r_48 
  %__e78 = inttoptr i32 %__r_49 to i32* 
  %__r_51 = bitcast i32* %__e78 to i32** 
  %addr50 = getelementptr  i32*, i32** %__r_51, i32 0 
  %reg140 = load  i32*, i32** %addr50 
  %__r_52 = ptrtoint i32* %__n68 to i32 
  %__r_53 = ptrtoint i32* %__m72 to i32 
  %__r_54 = sub   i32 %__r_52, %__r_53 
  %reg141 = inttoptr i32 %__r_54 to i32* 
  %fun55 = bitcast i32* %reg140 to i32* (i32*, i32*)* 
  %reg139 =  call ccc  i32*  %fun55(i32*  %__e78, i32*  %reg141)  
  br label %ifcont130 
ifcont130:
  %regcont142 = phi i32* [%reg135, %then128], [%reg139, %else129] 
  br label %ifcont126 
ifcont126:
  %regcont143 = phi i32* [%__n68, %then124], [%regcont142, %ifcont130] 
  br label %ifcont122 
ifcont122:
  %regcont144 = phi i32* [%__m72, %then120], [%regcont143, %ifcont126] 
  ret i32* %regcont144 
}


define external ccc  i32* @__52(i32*  %__clo53, i32*  %__n51)    {
__52:
  %__r_1 = inttoptr i32 0 to i32* 
  %__r_2 = ptrtoint i32* %__clo53 to i32 
  %__r_3 = ptrtoint i32* %__r_1 to i32 
  %__r_4 = add   i32 %__r_2, %__r_3 
  %__gcd254 = inttoptr i32 %__r_4 to i32* 
  %reg145 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__56, i32  2, i32*  %__gcd254, i32*  %__n51)  
  ret i32* %reg145 
}


define external ccc  i32* @__56(i32*  %__clo57, i32*  %__m55)    {
__56:
  %__r_2 = bitcast i32* %__clo57 to i32** 
  %addr1 = getelementptr  i32*, i32** %__r_2, i32 1 
  %reg146 = load  i32*, i32** %addr1 
  %__r_3 = inttoptr i32 0 to i32* 
  %__r_4 = ptrtoint i32* %reg146 to i32 
  %__r_5 = ptrtoint i32* %__r_3 to i32 
  %__r_6 = add   i32 %__r_4, %__r_5 
  %__gcd254 = inttoptr i32 %__r_6 to i32* 
  %__r_8 = bitcast i32* %__clo57 to i32** 
  %addr7 = getelementptr  i32*, i32** %__r_8, i32 2 
  %reg147 = load  i32*, i32** %addr7 
  %__r_9 = inttoptr i32 0 to i32* 
  %__r_10 = ptrtoint i32* %reg147 to i32 
  %__r_11 = ptrtoint i32* %__r_9 to i32 
  %__r_12 = add   i32 %__r_10, %__r_11 
  %__n51 = inttoptr i32 %__r_12 to i32* 
  br label %entry148 
entry148:
  %__r_14 = inttoptr i32 0 to i32* 
  %cond13 = icmp eq i32* %__n51, %__r_14 
  br i1 %cond13, label %then149, label %else150 
then149:
  br label %ifcont151 
else150:
  br label %entry152 
entry152:
  %__r_16 = inttoptr i32 0 to i32* 
  %cond15 = icmp eq i32* %__m55, %__r_16 
  br i1 %cond15, label %then153, label %else154 
then153:
  br label %ifcont155 
else154:
  br label %entry156 
entry156:
  %__r_17 = load  i32*, i32** @resta 
  %__r_18 = inttoptr i32 0 to i32* 
  %__r_19 = ptrtoint i32* %__r_17 to i32 
  %__r_20 = ptrtoint i32* %__r_18 to i32 
  %__r_21 = add   i32 %__r_19, %__r_20 
  %__e58 = inttoptr i32 %__r_21 to i32* 
  %__r_23 = bitcast i32* %__e58 to i32** 
  %addr22 = getelementptr  i32*, i32** %__r_23, i32 0 
  %reg161 = load  i32*, i32** %addr22 
  %fun24 = bitcast i32* %reg161 to i32* (i32*, i32*)* 
  %reg160 =  call ccc  i32*  %fun24(i32*  %__e58, i32*  %__n51)  
  %__r_25 = inttoptr i32 0 to i32* 
  %__r_26 = ptrtoint i32* %reg160 to i32 
  %__r_27 = ptrtoint i32* %__r_25 to i32 
  %__r_28 = add   i32 %__r_26, %__r_27 
  %__e59 = inttoptr i32 %__r_28 to i32* 
  %__r_30 = bitcast i32* %__e59 to i32** 
  %addr29 = getelementptr  i32*, i32** %__r_30, i32 0 
  %reg163 = load  i32*, i32** %addr29 
  %fun31 = bitcast i32* %reg163 to i32* (i32*, i32*)* 
  %reg162 =  call ccc  i32*  %fun31(i32*  %__e59, i32*  %__m55)  
  %__r_33 = inttoptr i32 0 to i32* 
  %cond32 = icmp eq i32* %reg162, %__r_33 
  br i1 %cond32, label %then157, label %else158 
then157:
  %__r_34 = inttoptr i32 0 to i32* 
  %__r_35 = ptrtoint i32* %__gcd254 to i32 
  %__r_36 = ptrtoint i32* %__r_34 to i32 
  %__r_37 = add   i32 %__r_35, %__r_36 
  %__e62 = inttoptr i32 %__r_37 to i32* 
  %__r_39 = bitcast i32* %__e62 to i32** 
  %addr38 = getelementptr  i32*, i32** %__r_39, i32 0 
  %reg165 = load  i32*, i32** %addr38 
  %__r_40 = load  i32*, i32** @resta 
  %__r_41 = inttoptr i32 0 to i32* 
  %__r_42 = ptrtoint i32* %__r_40 to i32 
  %__r_43 = ptrtoint i32* %__r_41 to i32 
  %__r_44 = add   i32 %__r_42, %__r_43 
  %__e60 = inttoptr i32 %__r_44 to i32* 
  %__r_46 = bitcast i32* %__e60 to i32** 
  %addr45 = getelementptr  i32*, i32** %__r_46, i32 0 
  %reg167 = load  i32*, i32** %addr45 
  %fun47 = bitcast i32* %reg167 to i32* (i32*, i32*)* 
  %reg166 =  call ccc  i32*  %fun47(i32*  %__e60, i32*  %__m55)  
  %__r_48 = inttoptr i32 0 to i32* 
  %__r_49 = ptrtoint i32* %reg166 to i32 
  %__r_50 = ptrtoint i32* %__r_48 to i32 
  %__r_51 = add   i32 %__r_49, %__r_50 
  %__e61 = inttoptr i32 %__r_51 to i32* 
  %__r_53 = bitcast i32* %__e61 to i32** 
  %addr52 = getelementptr  i32*, i32** %__r_53, i32 0 
  %reg169 = load  i32*, i32** %addr52 
  %fun54 = bitcast i32* %reg169 to i32* (i32*, i32*)* 
  %reg168 =  call ccc  i32*  %fun54(i32*  %__e61, i32*  %__n51)  
  %fun55 = bitcast i32* %reg165 to i32* (i32*, i32*)* 
  %reg164 =  call ccc  i32*  %fun55(i32*  %__e62, i32*  %reg168)  
  %__r_56 = inttoptr i32 0 to i32* 
  %__r_57 = ptrtoint i32* %reg164 to i32 
  %__r_58 = ptrtoint i32* %__r_56 to i32 
  %__r_59 = add   i32 %__r_57, %__r_58 
  %__e63 = inttoptr i32 %__r_59 to i32* 
  %__r_61 = bitcast i32* %__e63 to i32** 
  %addr60 = getelementptr  i32*, i32** %__r_61, i32 0 
  %reg171 = load  i32*, i32** %addr60 
  %fun62 = bitcast i32* %reg171 to i32* (i32*, i32*)* 
  %reg170 =  call ccc  i32*  %fun62(i32*  %__e63, i32*  %__n51)  
  br label %ifcont159 
else158:
  %__r_63 = inttoptr i32 0 to i32* 
  %__r_64 = ptrtoint i32* %__gcd254 to i32 
  %__r_65 = ptrtoint i32* %__r_63 to i32 
  %__r_66 = add   i32 %__r_64, %__r_65 
  %__e64 = inttoptr i32 %__r_66 to i32* 
  %__r_68 = bitcast i32* %__e64 to i32** 
  %addr67 = getelementptr  i32*, i32** %__r_68, i32 0 
  %reg173 = load  i32*, i32** %addr67 
  %fun69 = bitcast i32* %reg173 to i32* (i32*, i32*)* 
  %reg172 =  call ccc  i32*  %fun69(i32*  %__e64, i32*  %__m55)  
  %__r_70 = inttoptr i32 0 to i32* 
  %__r_71 = ptrtoint i32* %reg172 to i32 
  %__r_72 = ptrtoint i32* %__r_70 to i32 
  %__r_73 = add   i32 %__r_71, %__r_72 
  %__e67 = inttoptr i32 %__r_73 to i32* 
  %__r_75 = bitcast i32* %__e67 to i32** 
  %addr74 = getelementptr  i32*, i32** %__r_75, i32 0 
  %reg175 = load  i32*, i32** %addr74 
  %__r_76 = load  i32*, i32** @resta 
  %__r_77 = inttoptr i32 0 to i32* 
  %__r_78 = ptrtoint i32* %__r_76 to i32 
  %__r_79 = ptrtoint i32* %__r_77 to i32 
  %__r_80 = add   i32 %__r_78, %__r_79 
  %__e65 = inttoptr i32 %__r_80 to i32* 
  %__r_82 = bitcast i32* %__e65 to i32** 
  %addr81 = getelementptr  i32*, i32** %__r_82, i32 0 
  %reg177 = load  i32*, i32** %addr81 
  %fun83 = bitcast i32* %reg177 to i32* (i32*, i32*)* 
  %reg176 =  call ccc  i32*  %fun83(i32*  %__e65, i32*  %__n51)  
  %__r_84 = inttoptr i32 0 to i32* 
  %__r_85 = ptrtoint i32* %reg176 to i32 
  %__r_86 = ptrtoint i32* %__r_84 to i32 
  %__r_87 = add   i32 %__r_85, %__r_86 
  %__e66 = inttoptr i32 %__r_87 to i32* 
  %__r_89 = bitcast i32* %__e66 to i32** 
  %addr88 = getelementptr  i32*, i32** %__r_89, i32 0 
  %reg179 = load  i32*, i32** %addr88 
  %fun90 = bitcast i32* %reg179 to i32* (i32*, i32*)* 
  %reg178 =  call ccc  i32*  %fun90(i32*  %__e66, i32*  %__m55)  
  %fun91 = bitcast i32* %reg175 to i32* (i32*, i32*)* 
  %reg174 =  call ccc  i32*  %fun91(i32*  %__e67, i32*  %reg178)  
  br label %ifcont159 
ifcont159:
  %regcont180 = phi i32* [%reg170, %then157], [%reg174, %else158] 
  br label %ifcont155 
ifcont155:
  %regcont181 = phi i32* [%__n51, %then153], [%regcont180, %ifcont159] 
  br label %ifcont151 
ifcont151:
  %regcont182 = phi i32* [%__m55, %then149], [%regcont181, %ifcont155] 
  ret i32* %regcont182 
}


define external ccc  i32* @__38(i32*  %__clo39, i32*  %__a37)    {
__38:
  %__r_1 = inttoptr i32 0 to i32* 
  %__r_2 = ptrtoint i32* %__clo39 to i32 
  %__r_3 = ptrtoint i32* %__r_1 to i32 
  %__r_4 = add   i32 %__r_2, %__r_3 
  %__multC240 = inttoptr i32 %__r_4 to i32* 
  %reg183 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__42, i32  2, i32*  %__a37, i32*  %__multC240)  
  ret i32* %reg183 
}


define external ccc  i32* @__42(i32*  %__clo43, i32*  %__b41)    {
__42:
  %__r_2 = bitcast i32* %__clo43 to i32** 
  %addr1 = getelementptr  i32*, i32** %__r_2, i32 1 
  %reg184 = load  i32*, i32** %addr1 
  %__r_3 = inttoptr i32 0 to i32* 
  %__r_4 = ptrtoint i32* %reg184 to i32 
  %__r_5 = ptrtoint i32* %__r_3 to i32 
  %__r_6 = add   i32 %__r_4, %__r_5 
  %__a37 = inttoptr i32 %__r_6 to i32* 
  %__r_8 = bitcast i32* %__clo43 to i32** 
  %addr7 = getelementptr  i32*, i32** %__r_8, i32 2 
  %reg185 = load  i32*, i32** %addr7 
  %__r_9 = inttoptr i32 0 to i32* 
  %__r_10 = ptrtoint i32* %reg185 to i32 
  %__r_11 = ptrtoint i32* %__r_9 to i32 
  %__r_12 = add   i32 %__r_10, %__r_11 
  %__multC240 = inttoptr i32 %__r_12 to i32* 
  %reg186 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__45, i32  3, i32*  %__a37, i32*  %__b41, i32*  %__multC240)  
  ret i32* %reg186 
}


define external ccc  i32* @__45(i32*  %__clo46, i32*  %__ac44)    {
__45:
  %__r_2 = bitcast i32* %__clo46 to i32** 
  %addr1 = getelementptr  i32*, i32** %__r_2, i32 1 
  %reg187 = load  i32*, i32** %addr1 
  %__r_3 = inttoptr i32 0 to i32* 
  %__r_4 = ptrtoint i32* %reg187 to i32 
  %__r_5 = ptrtoint i32* %__r_3 to i32 
  %__r_6 = add   i32 %__r_4, %__r_5 
  %__a37 = inttoptr i32 %__r_6 to i32* 
  %__r_8 = bitcast i32* %__clo46 to i32** 
  %addr7 = getelementptr  i32*, i32** %__r_8, i32 2 
  %reg188 = load  i32*, i32** %addr7 
  %__r_9 = inttoptr i32 0 to i32* 
  %__r_10 = ptrtoint i32* %reg188 to i32 
  %__r_11 = ptrtoint i32* %__r_9 to i32 
  %__r_12 = add   i32 %__r_10, %__r_11 
  %__b41 = inttoptr i32 %__r_12 to i32* 
  %__r_14 = bitcast i32* %__clo46 to i32** 
  %addr13 = getelementptr  i32*, i32** %__r_14, i32 3 
  %reg189 = load  i32*, i32** %addr13 
  %__r_15 = inttoptr i32 0 to i32* 
  %__r_16 = ptrtoint i32* %reg189 to i32 
  %__r_17 = ptrtoint i32* %__r_15 to i32 
  %__r_18 = add   i32 %__r_16, %__r_17 
  %__multC240 = inttoptr i32 %__r_18 to i32* 
  br label %entry190 
entry190:
  %__r_20 = inttoptr i32 0 to i32* 
  %cond19 = icmp eq i32* %__b41, %__r_20 
  br i1 %cond19, label %then191, label %else192 
then191:
  br label %ifcont193 
else192:
  %__r_21 = inttoptr i32 0 to i32* 
  %__r_22 = ptrtoint i32* %__multC240 to i32 
  %__r_23 = ptrtoint i32* %__r_21 to i32 
  %__r_24 = add   i32 %__r_22, %__r_23 
  %__e47 = inttoptr i32 %__r_24 to i32* 
  %__r_26 = bitcast i32* %__e47 to i32** 
  %addr25 = getelementptr  i32*, i32** %__r_26, i32 0 
  %reg195 = load  i32*, i32** %addr25 
  %fun27 = bitcast i32* %reg195 to i32* (i32*, i32*)* 
  %reg194 =  call ccc  i32*  %fun27(i32*  %__e47, i32*  %__a37)  
  %__r_28 = inttoptr i32 0 to i32* 
  %__r_29 = ptrtoint i32* %reg194 to i32 
  %__r_30 = ptrtoint i32* %__r_28 to i32 
  %__r_31 = add   i32 %__r_29, %__r_30 
  %__e48 = inttoptr i32 %__r_31 to i32* 
  %__r_33 = bitcast i32* %__e48 to i32** 
  %addr32 = getelementptr  i32*, i32** %__r_33, i32 0 
  %reg197 = load  i32*, i32** %addr32 
  %__r_34 = inttoptr i32 1 to i32* 
  %__r_35 = inttoptr i32 0 to i32* 
  %__r_36 = ptrtoint i32* %__r_34 to i32 
  %__r_37 = ptrtoint i32* %__r_35 to i32 
  %__r_38 = add   i32 %__r_36, %__r_37 
  %reg199 = inttoptr i32 %__r_38 to i32* 
  %__r_39 = ptrtoint i32* %__b41 to i32 
  %__r_40 = ptrtoint i32* %reg199 to i32 
  %__r_41 = sub   i32 %__r_39, %__r_40 
  %reg198 = inttoptr i32 %__r_41 to i32* 
  %fun42 = bitcast i32* %reg197 to i32* (i32*, i32*)* 
  %reg196 =  call ccc  i32*  %fun42(i32*  %__e48, i32*  %reg198)  
  %__r_43 = inttoptr i32 0 to i32* 
  %__r_44 = ptrtoint i32* %reg196 to i32 
  %__r_45 = ptrtoint i32* %__r_43 to i32 
  %__r_46 = add   i32 %__r_44, %__r_45 
  %__e49 = inttoptr i32 %__r_46 to i32* 
  %__r_48 = bitcast i32* %__e49 to i32** 
  %addr47 = getelementptr  i32*, i32** %__r_48, i32 0 
  %reg201 = load  i32*, i32** %addr47 
  %__r_49 = ptrtoint i32* %__ac44 to i32 
  %__r_50 = ptrtoint i32* %__a37 to i32 
  %__r_51 = add   i32 %__r_49, %__r_50 
  %reg202 = inttoptr i32 %__r_51 to i32* 
  %fun52 = bitcast i32* %reg201 to i32* (i32*, i32*)* 
  %reg200 =  call ccc  i32*  %fun52(i32*  %__e49, i32*  %reg202)  
  br label %ifcont193 
ifcont193:
  %regcont203 = phi i32* [%__ac44, %then191], [%reg200, %else192] 
  ret i32* %regcont203 
}


define external ccc  i32* @__29(i32*  %__clo30, i32*  %__a28)    {
__29:
  %reg204 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__32, i32  1, i32*  %__a28)  
  ret i32* %reg204 
}


define external ccc  i32* @__32(i32*  %__clo33, i32*  %__b31)    {
__32:
  %__r_2 = bitcast i32* %__clo33 to i32** 
  %addr1 = getelementptr  i32*, i32** %__r_2, i32 1 
  %reg205 = load  i32*, i32** %addr1 
  %__r_3 = inttoptr i32 0 to i32* 
  %__r_4 = ptrtoint i32* %reg205 to i32 
  %__r_5 = ptrtoint i32* %__r_3 to i32 
  %__r_6 = add   i32 %__r_4, %__r_5 
  %__a28 = inttoptr i32 %__r_6 to i32* 
  %__r_7 = load  i32*, i32** @multC2 
  %__r_8 = inttoptr i32 0 to i32* 
  %__r_9 = ptrtoint i32* %__r_7 to i32 
  %__r_10 = ptrtoint i32* %__r_8 to i32 
  %__r_11 = add   i32 %__r_9, %__r_10 
  %__e34 = inttoptr i32 %__r_11 to i32* 
  %__r_13 = bitcast i32* %__e34 to i32** 
  %addr12 = getelementptr  i32*, i32** %__r_13, i32 0 
  %reg207 = load  i32*, i32** %addr12 
  %fun14 = bitcast i32* %reg207 to i32* (i32*, i32*)* 
  %reg206 =  call ccc  i32*  %fun14(i32*  %__e34, i32*  %__a28)  
  %__r_15 = inttoptr i32 0 to i32* 
  %__r_16 = ptrtoint i32* %reg206 to i32 
  %__r_17 = ptrtoint i32* %__r_15 to i32 
  %__r_18 = add   i32 %__r_16, %__r_17 
  %__e35 = inttoptr i32 %__r_18 to i32* 
  %__r_20 = bitcast i32* %__e35 to i32** 
  %addr19 = getelementptr  i32*, i32** %__r_20, i32 0 
  %reg209 = load  i32*, i32** %addr19 
  %fun21 = bitcast i32* %reg209 to i32* (i32*, i32*)* 
  %reg208 =  call ccc  i32*  %fun21(i32*  %__e35, i32*  %__b31)  
  %__r_22 = inttoptr i32 0 to i32* 
  %__r_23 = ptrtoint i32* %reg208 to i32 
  %__r_24 = ptrtoint i32* %__r_22 to i32 
  %__r_25 = add   i32 %__r_23, %__r_24 
  %__e36 = inttoptr i32 %__r_25 to i32* 
  %__r_27 = bitcast i32* %__e36 to i32** 
  %addr26 = getelementptr  i32*, i32** %__r_27, i32 0 
  %reg211 = load  i32*, i32** %addr26 
  %__r_28 = inttoptr i32 0 to i32* 
  %__r_29 = inttoptr i32 0 to i32* 
  %__r_30 = ptrtoint i32* %__r_28 to i32 
  %__r_31 = ptrtoint i32* %__r_29 to i32 
  %__r_32 = add   i32 %__r_30, %__r_31 
  %reg212 = inttoptr i32 %__r_32 to i32* 
  %fun33 = bitcast i32* %reg211 to i32* (i32*, i32*)* 
  %reg210 =  call ccc  i32*  %fun33(i32*  %__e36, i32*  %reg212)  
  ret i32* %reg210 
}


define external ccc  i32* @__14(i32*  %__clo15, i32*  %__a13)    {
__14:
  %__r_1 = inttoptr i32 0 to i32* 
  %__r_2 = ptrtoint i32* %__clo15 to i32 
  %__r_3 = ptrtoint i32* %__r_1 to i32 
  %__r_4 = add   i32 %__r_2, %__r_3 
  %__expC216 = inttoptr i32 %__r_4 to i32* 
  %reg213 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__18, i32  2, i32*  %__a13, i32*  %__expC216)  
  ret i32* %reg213 
}


define external ccc  i32* @__18(i32*  %__clo19, i32*  %__b17)    {
__18:
  %__r_2 = bitcast i32* %__clo19 to i32** 
  %addr1 = getelementptr  i32*, i32** %__r_2, i32 1 
  %reg214 = load  i32*, i32** %addr1 
  %__r_3 = inttoptr i32 0 to i32* 
  %__r_4 = ptrtoint i32* %reg214 to i32 
  %__r_5 = ptrtoint i32* %__r_3 to i32 
  %__r_6 = add   i32 %__r_4, %__r_5 
  %__a13 = inttoptr i32 %__r_6 to i32* 
  %__r_8 = bitcast i32* %__clo19 to i32** 
  %addr7 = getelementptr  i32*, i32** %__r_8, i32 2 
  %reg215 = load  i32*, i32** %addr7 
  %__r_9 = inttoptr i32 0 to i32* 
  %__r_10 = ptrtoint i32* %reg215 to i32 
  %__r_11 = ptrtoint i32* %__r_9 to i32 
  %__r_12 = add   i32 %__r_10, %__r_11 
  %__expC216 = inttoptr i32 %__r_12 to i32* 
  %reg216 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__21, i32  3, i32*  %__a13, i32*  %__b17, i32*  %__expC216)  
  ret i32* %reg216 
}


define external ccc  i32* @__21(i32*  %__clo22, i32*  %__ac20)    {
__21:
  %__r_2 = bitcast i32* %__clo22 to i32** 
  %addr1 = getelementptr  i32*, i32** %__r_2, i32 1 
  %reg217 = load  i32*, i32** %addr1 
  %__r_3 = inttoptr i32 0 to i32* 
  %__r_4 = ptrtoint i32* %reg217 to i32 
  %__r_5 = ptrtoint i32* %__r_3 to i32 
  %__r_6 = add   i32 %__r_4, %__r_5 
  %__a13 = inttoptr i32 %__r_6 to i32* 
  %__r_8 = bitcast i32* %__clo22 to i32** 
  %addr7 = getelementptr  i32*, i32** %__r_8, i32 2 
  %reg218 = load  i32*, i32** %addr7 
  %__r_9 = inttoptr i32 0 to i32* 
  %__r_10 = ptrtoint i32* %reg218 to i32 
  %__r_11 = ptrtoint i32* %__r_9 to i32 
  %__r_12 = add   i32 %__r_10, %__r_11 
  %__b17 = inttoptr i32 %__r_12 to i32* 
  %__r_14 = bitcast i32* %__clo22 to i32** 
  %addr13 = getelementptr  i32*, i32** %__r_14, i32 3 
  %reg219 = load  i32*, i32** %addr13 
  %__r_15 = inttoptr i32 0 to i32* 
  %__r_16 = ptrtoint i32* %reg219 to i32 
  %__r_17 = ptrtoint i32* %__r_15 to i32 
  %__r_18 = add   i32 %__r_16, %__r_17 
  %__expC216 = inttoptr i32 %__r_18 to i32* 
  br label %entry220 
entry220:
  %__r_20 = inttoptr i32 0 to i32* 
  %cond19 = icmp eq i32* %__b17, %__r_20 
  br i1 %cond19, label %then221, label %else222 
then221:
  br label %ifcont223 
else222:
  %__r_21 = inttoptr i32 0 to i32* 
  %__r_22 = ptrtoint i32* %__expC216 to i32 
  %__r_23 = ptrtoint i32* %__r_21 to i32 
  %__r_24 = add   i32 %__r_22, %__r_23 
  %__e23 = inttoptr i32 %__r_24 to i32* 
  %__r_26 = bitcast i32* %__e23 to i32** 
  %addr25 = getelementptr  i32*, i32** %__r_26, i32 0 
  %reg225 = load  i32*, i32** %addr25 
  %fun27 = bitcast i32* %reg225 to i32* (i32*, i32*)* 
  %reg224 =  call ccc  i32*  %fun27(i32*  %__e23, i32*  %__a13)  
  %__r_28 = inttoptr i32 0 to i32* 
  %__r_29 = ptrtoint i32* %reg224 to i32 
  %__r_30 = ptrtoint i32* %__r_28 to i32 
  %__r_31 = add   i32 %__r_29, %__r_30 
  %__e24 = inttoptr i32 %__r_31 to i32* 
  %__r_33 = bitcast i32* %__e24 to i32** 
  %addr32 = getelementptr  i32*, i32** %__r_33, i32 0 
  %reg227 = load  i32*, i32** %addr32 
  %__r_34 = inttoptr i32 1 to i32* 
  %__r_35 = inttoptr i32 0 to i32* 
  %__r_36 = ptrtoint i32* %__r_34 to i32 
  %__r_37 = ptrtoint i32* %__r_35 to i32 
  %__r_38 = add   i32 %__r_36, %__r_37 
  %reg229 = inttoptr i32 %__r_38 to i32* 
  %__r_39 = ptrtoint i32* %__b17 to i32 
  %__r_40 = ptrtoint i32* %reg229 to i32 
  %__r_41 = sub   i32 %__r_39, %__r_40 
  %reg228 = inttoptr i32 %__r_41 to i32* 
  %fun42 = bitcast i32* %reg227 to i32* (i32*, i32*)* 
  %reg226 =  call ccc  i32*  %fun42(i32*  %__e24, i32*  %reg228)  
  %__r_43 = inttoptr i32 0 to i32* 
  %__r_44 = ptrtoint i32* %reg226 to i32 
  %__r_45 = ptrtoint i32* %__r_43 to i32 
  %__r_46 = add   i32 %__r_44, %__r_45 
  %__e27 = inttoptr i32 %__r_46 to i32* 
  %__r_48 = bitcast i32* %__e27 to i32** 
  %addr47 = getelementptr  i32*, i32** %__r_48, i32 0 
  %reg231 = load  i32*, i32** %addr47 
  %__r_49 = load  i32*, i32** @multC 
  %__r_50 = inttoptr i32 0 to i32* 
  %__r_51 = ptrtoint i32* %__r_49 to i32 
  %__r_52 = ptrtoint i32* %__r_50 to i32 
  %__r_53 = add   i32 %__r_51, %__r_52 
  %__e25 = inttoptr i32 %__r_53 to i32* 
  %__r_55 = bitcast i32* %__e25 to i32** 
  %addr54 = getelementptr  i32*, i32** %__r_55, i32 0 
  %reg233 = load  i32*, i32** %addr54 
  %fun56 = bitcast i32* %reg233 to i32* (i32*, i32*)* 
  %reg232 =  call ccc  i32*  %fun56(i32*  %__e25, i32*  %__a13)  
  %__r_57 = inttoptr i32 0 to i32* 
  %__r_58 = ptrtoint i32* %reg232 to i32 
  %__r_59 = ptrtoint i32* %__r_57 to i32 
  %__r_60 = add   i32 %__r_58, %__r_59 
  %__e26 = inttoptr i32 %__r_60 to i32* 
  %__r_62 = bitcast i32* %__e26 to i32** 
  %addr61 = getelementptr  i32*, i32** %__r_62, i32 0 
  %reg235 = load  i32*, i32** %addr61 
  %fun63 = bitcast i32* %reg235 to i32* (i32*, i32*)* 
  %reg234 =  call ccc  i32*  %fun63(i32*  %__e26, i32*  %__ac20)  
  %fun64 = bitcast i32* %reg231 to i32* (i32*, i32*)* 
  %reg230 =  call ccc  i32*  %fun64(i32*  %__e27, i32*  %reg234)  
  br label %ifcont223 
ifcont223:
  %regcont236 = phi i32* [%__ac20, %then221], [%reg230, %else222] 
  ret i32* %regcont236 
}


define external ccc  i32* @__5(i32*  %__clo6, i32*  %__a4)    {
__5:
  %reg237 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__8, i32  1, i32*  %__a4)  
  ret i32* %reg237 
}


define external ccc  i32* @__8(i32*  %__clo9, i32*  %__b7)    {
__8:
  %__r_2 = bitcast i32* %__clo9 to i32** 
  %addr1 = getelementptr  i32*, i32** %__r_2, i32 1 
  %reg238 = load  i32*, i32** %addr1 
  %__r_3 = inttoptr i32 0 to i32* 
  %__r_4 = ptrtoint i32* %reg238 to i32 
  %__r_5 = ptrtoint i32* %__r_3 to i32 
  %__r_6 = add   i32 %__r_4, %__r_5 
  %__a4 = inttoptr i32 %__r_6 to i32* 
  %__r_7 = load  i32*, i32** @expC2 
  %__r_8 = inttoptr i32 0 to i32* 
  %__r_9 = ptrtoint i32* %__r_7 to i32 
  %__r_10 = ptrtoint i32* %__r_8 to i32 
  %__r_11 = add   i32 %__r_9, %__r_10 
  %__e10 = inttoptr i32 %__r_11 to i32* 
  %__r_13 = bitcast i32* %__e10 to i32** 
  %addr12 = getelementptr  i32*, i32** %__r_13, i32 0 
  %reg240 = load  i32*, i32** %addr12 
  %fun14 = bitcast i32* %reg240 to i32* (i32*, i32*)* 
  %reg239 =  call ccc  i32*  %fun14(i32*  %__e10, i32*  %__a4)  
  %__r_15 = inttoptr i32 0 to i32* 
  %__r_16 = ptrtoint i32* %reg239 to i32 
  %__r_17 = ptrtoint i32* %__r_15 to i32 
  %__r_18 = add   i32 %__r_16, %__r_17 
  %__e11 = inttoptr i32 %__r_18 to i32* 
  %__r_20 = bitcast i32* %__e11 to i32** 
  %addr19 = getelementptr  i32*, i32** %__r_20, i32 0 
  %reg242 = load  i32*, i32** %addr19 
  %fun21 = bitcast i32* %reg242 to i32* (i32*, i32*)* 
  %reg241 =  call ccc  i32*  %fun21(i32*  %__e11, i32*  %__b7)  
  %__r_22 = inttoptr i32 0 to i32* 
  %__r_23 = ptrtoint i32* %reg241 to i32 
  %__r_24 = ptrtoint i32* %__r_22 to i32 
  %__r_25 = add   i32 %__r_23, %__r_24 
  %__e12 = inttoptr i32 %__r_25 to i32* 
  %__r_27 = bitcast i32* %__e12 to i32** 
  %addr26 = getelementptr  i32*, i32** %__r_27, i32 0 
  %reg244 = load  i32*, i32** %addr26 
  %__r_28 = inttoptr i32 1 to i32* 
  %__r_29 = inttoptr i32 0 to i32* 
  %__r_30 = ptrtoint i32* %__r_28 to i32 
  %__r_31 = ptrtoint i32* %__r_29 to i32 
  %__r_32 = add   i32 %__r_30, %__r_31 
  %reg245 = inttoptr i32 %__r_32 to i32* 
  %fun33 = bitcast i32* %reg244 to i32* (i32*, i32*)* 
  %reg243 =  call ccc  i32*  %fun33(i32*  %__e12, i32*  %reg245)  
  ret i32* %reg243 
}


@suman = internal   global i32* zeroinitializer


@doble = internal   global i32* zeroinitializer


@sumard = internal   global i32* zeroinitializer


@fib = internal   global i32* zeroinitializer


@resta = internal   global i32* zeroinitializer


@mult = internal   global i32* zeroinitializer


@exp = internal   global i32* zeroinitializer


@fact = internal   global i32* zeroinitializer


@gcd = internal   global i32* zeroinitializer


@gcd2 = internal   global i32* zeroinitializer


@x = internal   global i32* zeroinitializer


@y = internal   global i32* zeroinitializer


@z = internal   global i32* zeroinitializer


@multC2 = internal   global i32* zeroinitializer


@multC = internal   global i32* zeroinitializer


@expC2 = internal   global i32* zeroinitializer


@expC = internal   global i32* zeroinitializer


@x2 = internal   global i32* zeroinitializer


@a = internal   global i32* zeroinitializer


define external ccc  i32* @pcfmain()    {
pcfmain:
  %reg246 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__135, i32  0)  
  %__r_1 = inttoptr i32 0 to i32* 
  %__r_2 = ptrtoint i32* %reg246 to i32 
  %__r_3 = ptrtoint i32* %__r_1 to i32 
  %__r_4 = add   i32 %__r_2, %__r_3 
  %__r_5 = inttoptr i32 %__r_4 to i32* 
  store  i32* %__r_5, i32** @suman 
  %reg247 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__132, i32  0)  
  %__r_6 = inttoptr i32 0 to i32* 
  %__r_7 = ptrtoint i32* %reg247 to i32 
  %__r_8 = ptrtoint i32* %__r_6 to i32 
  %__r_9 = add   i32 %__r_7, %__r_8 
  %__r_10 = inttoptr i32 %__r_9 to i32* 
  store  i32* %__r_10, i32** @doble 
  %reg248 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__122, i32  0)  
  %__r_11 = inttoptr i32 0 to i32* 
  %__r_12 = ptrtoint i32* %reg248 to i32 
  %__r_13 = ptrtoint i32* %__r_11 to i32 
  %__r_14 = add   i32 %__r_12, %__r_13 
  %__r_15 = inttoptr i32 %__r_14 to i32* 
  store  i32* %__r_15, i32** @sumard 
  %reg249 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__116, i32  0)  
  %__r_16 = inttoptr i32 0 to i32* 
  %__r_17 = ptrtoint i32* %reg249 to i32 
  %__r_18 = ptrtoint i32* %__r_16 to i32 
  %__r_19 = add   i32 %__r_17, %__r_18 
  %__r_20 = inttoptr i32 %__r_19 to i32* 
  store  i32* %__r_20, i32** @fib 
  %reg250 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__107, i32  0)  
  %__r_21 = inttoptr i32 0 to i32* 
  %__r_22 = ptrtoint i32* %reg250 to i32 
  %__r_23 = ptrtoint i32* %__r_21 to i32 
  %__r_24 = add   i32 %__r_22, %__r_23 
  %__r_25 = inttoptr i32 %__r_24 to i32* 
  store  i32* %__r_25, i32** @resta 
  %reg251 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__98, i32  0)  
  %__r_26 = inttoptr i32 0 to i32* 
  %__r_27 = ptrtoint i32* %reg251 to i32 
  %__r_28 = ptrtoint i32* %__r_26 to i32 
  %__r_29 = add   i32 %__r_27, %__r_28 
  %__r_30 = inttoptr i32 %__r_29 to i32* 
  store  i32* %__r_30, i32** @mult 
  %reg252 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__87, i32  0)  
  %__r_31 = inttoptr i32 0 to i32* 
  %__r_32 = ptrtoint i32* %reg252 to i32 
  %__r_33 = ptrtoint i32* %__r_31 to i32 
  %__r_34 = add   i32 %__r_32, %__r_33 
  %__r_35 = inttoptr i32 %__r_34 to i32* 
  store  i32* %__r_35, i32** @exp 
  %reg253 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__80, i32  0)  
  %__r_36 = inttoptr i32 0 to i32* 
  %__r_37 = ptrtoint i32* %reg253 to i32 
  %__r_38 = ptrtoint i32* %__r_36 to i32 
  %__r_39 = add   i32 %__r_37, %__r_38 
  %__r_40 = inttoptr i32 %__r_39 to i32* 
  store  i32* %__r_40, i32** @fact 
  %reg254 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__69, i32  0)  
  %__r_41 = inttoptr i32 0 to i32* 
  %__r_42 = ptrtoint i32* %reg254 to i32 
  %__r_43 = ptrtoint i32* %__r_41 to i32 
  %__r_44 = add   i32 %__r_42, %__r_43 
  %__r_45 = inttoptr i32 %__r_44 to i32* 
  store  i32* %__r_45, i32** @gcd 
  %reg255 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__52, i32  0)  
  %__r_46 = inttoptr i32 0 to i32* 
  %__r_47 = ptrtoint i32* %reg255 to i32 
  %__r_48 = ptrtoint i32* %__r_46 to i32 
  %__r_49 = add   i32 %__r_47, %__r_48 
  %__r_50 = inttoptr i32 %__r_49 to i32* 
  store  i32* %__r_50, i32** @gcd2 
  %__r_51 = inttoptr i32 3 to i32* 
  %__r_52 = inttoptr i32 0 to i32* 
  %__r_53 = ptrtoint i32* %__r_51 to i32 
  %__r_54 = ptrtoint i32* %__r_52 to i32 
  %__r_55 = add   i32 %__r_53, %__r_54 
  %reg256 = inttoptr i32 %__r_55 to i32* 
  %__r_56 = inttoptr i32 0 to i32* 
  %__r_57 = ptrtoint i32* %reg256 to i32 
  %__r_58 = ptrtoint i32* %__r_56 to i32 
  %__r_59 = add   i32 %__r_57, %__r_58 
  %__r_60 = inttoptr i32 %__r_59 to i32* 
  store  i32* %__r_60, i32** @x 
  %__r_61 = inttoptr i32 4 to i32* 
  %__r_62 = inttoptr i32 0 to i32* 
  %__r_63 = ptrtoint i32* %__r_61 to i32 
  %__r_64 = ptrtoint i32* %__r_62 to i32 
  %__r_65 = add   i32 %__r_63, %__r_64 
  %reg258 = inttoptr i32 %__r_65 to i32* 
  %__r_66 = load  i32*, i32** @x 
  %__r_67 = ptrtoint i32* %__r_66 to i32 
  %__r_68 = ptrtoint i32* %reg258 to i32 
  %__r_69 = add   i32 %__r_67, %__r_68 
  %reg257 = inttoptr i32 %__r_69 to i32* 
  %__r_70 = inttoptr i32 0 to i32* 
  %__r_71 = ptrtoint i32* %reg257 to i32 
  %__r_72 = ptrtoint i32* %__r_70 to i32 
  %__r_73 = add   i32 %__r_71, %__r_72 
  %__r_74 = inttoptr i32 %__r_73 to i32* 
  store  i32* %__r_74, i32** @y 
  %__r_75 = load  i32*, i32** @doble 
  %__r_76 = inttoptr i32 0 to i32* 
  %__r_77 = ptrtoint i32* %__r_75 to i32 
  %__r_78 = ptrtoint i32* %__r_76 to i32 
  %__r_79 = add   i32 %__r_77, %__r_78 
  %__e50 = inttoptr i32 %__r_79 to i32* 
  %__r_81 = bitcast i32* %__e50 to i32** 
  %addr80 = getelementptr  i32*, i32** %__r_81, i32 0 
  %reg260 = load  i32*, i32** %addr80 
  %fun82 = bitcast i32* %reg260 to i32* (i32*, i32*)* 
  %__r_83 = load  i32*, i32** @y 
  %reg259 =  call ccc  i32*  %fun82(i32*  %__e50, i32*  %__r_83)  
  %__r_84 = inttoptr i32 0 to i32* 
  %__r_85 = ptrtoint i32* %reg259 to i32 
  %__r_86 = ptrtoint i32* %__r_84 to i32 
  %__r_87 = add   i32 %__r_85, %__r_86 
  %__r_88 = inttoptr i32 %__r_87 to i32* 
  store  i32* %__r_88, i32** @z 
  %reg261 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__38, i32  0)  
  %__r_89 = inttoptr i32 0 to i32* 
  %__r_90 = ptrtoint i32* %reg261 to i32 
  %__r_91 = ptrtoint i32* %__r_89 to i32 
  %__r_92 = add   i32 %__r_90, %__r_91 
  %__r_93 = inttoptr i32 %__r_92 to i32* 
  store  i32* %__r_93, i32** @multC2 
  %reg262 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__29, i32  0)  
  %__r_94 = inttoptr i32 0 to i32* 
  %__r_95 = ptrtoint i32* %reg262 to i32 
  %__r_96 = ptrtoint i32* %__r_94 to i32 
  %__r_97 = add   i32 %__r_95, %__r_96 
  %__r_98 = inttoptr i32 %__r_97 to i32* 
  store  i32* %__r_98, i32** @multC 
  %reg263 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__14, i32  0)  
  %__r_99 = inttoptr i32 0 to i32* 
  %__r_100 = ptrtoint i32* %reg263 to i32 
  %__r_101 = ptrtoint i32* %__r_99 to i32 
  %__r_102 = add   i32 %__r_100, %__r_101 
  %__r_103 = inttoptr i32 %__r_102 to i32* 
  store  i32* %__r_103, i32** @expC2 
  %reg264 =  call ccc  i32* (i32* (i32*, i32*)*, i32, ...) @pcf_mkclosure(i32* (i32*, i32*)*  @__5, i32  0)  
  %__r_104 = inttoptr i32 0 to i32* 
  %__r_105 = ptrtoint i32* %reg264 to i32 
  %__r_106 = ptrtoint i32* %__r_104 to i32 
  %__r_107 = add   i32 %__r_105, %__r_106 
  %__r_108 = inttoptr i32 %__r_107 to i32* 
  store  i32* %__r_108, i32** @expC 
  %__r_109 = load  i32*, i32** @gcd2 
  %__r_110 = inttoptr i32 0 to i32* 
  %__r_111 = ptrtoint i32* %__r_109 to i32 
  %__r_112 = ptrtoint i32* %__r_110 to i32 
  %__r_113 = add   i32 %__r_111, %__r_112 
  %__e2 = inttoptr i32 %__r_113 to i32* 
  %__r_115 = bitcast i32* %__e2 to i32** 
  %addr114 = getelementptr  i32*, i32** %__r_115, i32 0 
  %reg266 = load  i32*, i32** %addr114 
  %__r_116 = inttoptr i32 12 to i32* 
  %__r_117 = inttoptr i32 0 to i32* 
  %__r_118 = ptrtoint i32* %__r_116 to i32 
  %__r_119 = ptrtoint i32* %__r_117 to i32 
  %__r_120 = add   i32 %__r_118, %__r_119 
  %reg267 = inttoptr i32 %__r_120 to i32* 
  %fun121 = bitcast i32* %reg266 to i32* (i32*, i32*)* 
  %reg265 =  call ccc  i32*  %fun121(i32*  %__e2, i32*  %reg267)  
  %__r_122 = inttoptr i32 0 to i32* 
  %__r_123 = ptrtoint i32* %reg265 to i32 
  %__r_124 = ptrtoint i32* %__r_122 to i32 
  %__r_125 = add   i32 %__r_123, %__r_124 
  %__e3 = inttoptr i32 %__r_125 to i32* 
  %__r_127 = bitcast i32* %__e3 to i32** 
  %addr126 = getelementptr  i32*, i32** %__r_127, i32 0 
  %reg269 = load  i32*, i32** %addr126 
  %__r_128 = inttoptr i32 144 to i32* 
  %__r_129 = inttoptr i32 0 to i32* 
  %__r_130 = ptrtoint i32* %__r_128 to i32 
  %__r_131 = ptrtoint i32* %__r_129 to i32 
  %__r_132 = add   i32 %__r_130, %__r_131 
  %reg270 = inttoptr i32 %__r_132 to i32* 
  %fun133 = bitcast i32* %reg269 to i32* (i32*, i32*)* 
  %reg268 =  call ccc  i32*  %fun133(i32*  %__e3, i32*  %reg270)  
  %__r_134 = inttoptr i32 0 to i32* 
  %__r_135 = ptrtoint i32* %reg268 to i32 
  %__r_136 = ptrtoint i32* %__r_134 to i32 
  %__r_137 = add   i32 %__r_135, %__r_136 
  %__r_138 = inttoptr i32 %__r_137 to i32* 
  store  i32* %__r_138, i32** @x2 
  %__r_139 = load  i32*, i32** @expC 
  %__r_140 = inttoptr i32 0 to i32* 
  %__r_141 = ptrtoint i32* %__r_139 to i32 
  %__r_142 = ptrtoint i32* %__r_140 to i32 
  %__r_143 = add   i32 %__r_141, %__r_142 
  %__e0 = inttoptr i32 %__r_143 to i32* 
  %__r_145 = bitcast i32* %__e0 to i32** 
  %addr144 = getelementptr  i32*, i32** %__r_145, i32 0 
  %reg272 = load  i32*, i32** %addr144 
  %__r_146 = inttoptr i32 2 to i32* 
  %__r_147 = inttoptr i32 0 to i32* 
  %__r_148 = ptrtoint i32* %__r_146 to i32 
  %__r_149 = ptrtoint i32* %__r_147 to i32 
  %__r_150 = add   i32 %__r_148, %__r_149 
  %reg273 = inttoptr i32 %__r_150 to i32* 
  %fun151 = bitcast i32* %reg272 to i32* (i32*, i32*)* 
  %reg271 =  call ccc  i32*  %fun151(i32*  %__e0, i32*  %reg273)  
  %__r_152 = inttoptr i32 0 to i32* 
  %__r_153 = ptrtoint i32* %reg271 to i32 
  %__r_154 = ptrtoint i32* %__r_152 to i32 
  %__r_155 = add   i32 %__r_153, %__r_154 
  %__e1 = inttoptr i32 %__r_155 to i32* 
  %__r_157 = bitcast i32* %__e1 to i32** 
  %addr156 = getelementptr  i32*, i32** %__r_157, i32 0 
  %reg275 = load  i32*, i32** %addr156 
  %__r_158 = inttoptr i32 4 to i32* 
  %__r_159 = inttoptr i32 0 to i32* 
  %__r_160 = ptrtoint i32* %__r_158 to i32 
  %__r_161 = ptrtoint i32* %__r_159 to i32 
  %__r_162 = add   i32 %__r_160, %__r_161 
  %reg276 = inttoptr i32 %__r_162 to i32* 
  %fun163 = bitcast i32* %reg275 to i32* (i32*, i32*)* 
  %reg274 =  call ccc  i32*  %fun163(i32*  %__e1, i32*  %reg276)  
  %__r_164 = ptrtoint i32* %reg274 to i32 
  %__r_165 =  call ccc  i32  @pcf_print(i32  %__r_164)  
  %reg277 = inttoptr i32 %__r_165 to i32* 
  %__r_166 = inttoptr i32 0 to i32* 
  %__r_167 = ptrtoint i32* %reg277 to i32 
  %__r_168 = ptrtoint i32* %__r_166 to i32 
  %__r_169 = add   i32 %__r_167, %__r_168 
  %__r_170 = inttoptr i32 %__r_169 to i32* 
  store  i32* %__r_170, i32** @a 
  ret i32* %reg274 
}