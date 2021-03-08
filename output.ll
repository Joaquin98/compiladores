; ModuleID = 'pcfprog'


 


declare external ccc  i64* @pcf_mkclosure(i64* (i64*, i64*)*, i64, ...)    


declare external ccc  i64 @pcf_print(i64)    


define external ccc  i64* @__161(i64*  %__clo162, i64*  %__x160)    {
__161:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo162 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__suman163 = inttoptr i64 %__r_4 to i64* 
  br label %entry0 
entry0:
  %__r_6 = inttoptr i64 0 to i64* 
  %cond5 = icmp eq i64* %__x160, %__r_6 
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
  %__r_13 = ptrtoint i64* %__suman163 to i64 
  %__r_14 = ptrtoint i64* %__r_12 to i64 
  %__r_15 = add   i64 %__r_13, %__r_14 
  %__e164 = inttoptr i64 %__r_15 to i64* 
  %__r_17 = bitcast i64* %__e164 to i64** 
  %addr16 = getelementptr  i64*, i64** %__r_17, i64 0 
  %reg7 = load  i64*, i64** %addr16 
  %__r_18 = inttoptr i64 1 to i64* 
  %__r_19 = inttoptr i64 0 to i64* 
  %__r_20 = ptrtoint i64* %__r_18 to i64 
  %__r_21 = ptrtoint i64* %__r_19 to i64 
  %__r_22 = add   i64 %__r_20, %__r_21 
  %reg9 = inttoptr i64 %__r_22 to i64* 
  %__r_23 = ptrtoint i64* %__x160 to i64 
  %__r_24 = ptrtoint i64* %reg9 to i64 
  %__r_25 = sub   i64 %__r_23, %__r_24 
  %__r_26 = icmp slt i64 0, %__r_25 
  %__r_27 = zext i1 %__r_26 to i64  
  %__r_28 = mul   i64 %__r_25, %__r_27 
  %reg8 = inttoptr i64 %__r_28 to i64* 
  %fun29 = bitcast i64* %reg7 to i64* (i64*, i64*)* 
  %reg6 =  call ccc  i64*  %fun29(i64*  %__e164, i64*  %reg8)  
  %__r_30 = ptrtoint i64* %__x160 to i64 
  %__r_31 = ptrtoint i64* %reg6 to i64 
  %__r_32 = add   i64 %__r_30, %__r_31 
  %reg5 = inttoptr i64 %__r_32 to i64* 
  br label %ifcont3 
ifcont3:
  %regcont10 = phi i64* [%reg4, %then1], [%reg5, %else2] 
  ret i64* %regcont10 
}


define external ccc  i64* @__158(i64*  %__clo159, i64*  %__x157)    {
__158:
  %__r_1 = ptrtoint i64* %__x157 to i64 
  %__r_2 = ptrtoint i64* %__x157 to i64 
  %__r_3 = add   i64 %__r_1, %__r_2 
  %reg11 = inttoptr i64 %__r_3 to i64* 
  ret i64* %reg11 
}


define external ccc  i64* @__148(i64*  %__clo149, i64*  %__x147)    {
__148:
  %reg12 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__151, i64  1, i64*  %__x147)  
  ret i64* %reg12 
}


define external ccc  i64* @__151(i64*  %__clo152, i64*  %__y150)    {
__151:
  %__r_2 = bitcast i64* %__clo152 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg13 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg13 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__x147 = inttoptr i64 %__r_6 to i64* 
  %reg14 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__154, i64  2, i64*  %__x147, i64*  %__y150)  
  ret i64* %reg14 
}


define external ccc  i64* @__154(i64*  %__clo155, i64*  %__f153)    {
__154:
  %__r_2 = bitcast i64* %__clo155 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg15 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg15 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__x147 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo155 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg16 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg16 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__y150 = inttoptr i64 %__r_12 to i64* 
  %__r_13 = inttoptr i64 0 to i64* 
  %__r_14 = ptrtoint i64* %__f153 to i64 
  %__r_15 = ptrtoint i64* %__r_13 to i64 
  %__r_16 = add   i64 %__r_14, %__r_15 
  %__e156 = inttoptr i64 %__r_16 to i64* 
  %__r_18 = bitcast i64* %__e156 to i64** 
  %addr17 = getelementptr  i64*, i64** %__r_18, i64 0 
  %reg18 = load  i64*, i64** %addr17 
  %__r_19 = ptrtoint i64* %__x147 to i64 
  %__r_20 = ptrtoint i64* %__y150 to i64 
  %__r_21 = add   i64 %__r_19, %__r_20 
  %reg19 = inttoptr i64 %__r_21 to i64* 
  %fun22 = bitcast i64* %reg18 to i64* (i64*, i64*)* 
  %reg17 =  call ccc  i64*  %fun22(i64*  %__e156, i64*  %reg19)  
  ret i64* %reg17 
}


define external ccc  i64* @__142(i64*  %__clo143, i64*  %__x141)    {
__142:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo143 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__fib144 = inttoptr i64 %__r_4 to i64* 
  br label %entry20 
entry20:
  %__r_6 = inttoptr i64 0 to i64* 
  %cond5 = icmp eq i64* %__x141, %__r_6 
  br i1 %cond5, label %then21, label %else22 
then21:
  %__r_7 = inttoptr i64 1 to i64* 
  %__r_8 = inttoptr i64 0 to i64* 
  %__r_9 = ptrtoint i64* %__r_7 to i64 
  %__r_10 = ptrtoint i64* %__r_8 to i64 
  %__r_11 = add   i64 %__r_9, %__r_10 
  %reg24 = inttoptr i64 %__r_11 to i64* 
  br label %ifcont23 
else22:
  br label %entry25 
entry25:
  %__r_12 = inttoptr i64 1 to i64* 
  %__r_13 = inttoptr i64 0 to i64* 
  %__r_14 = ptrtoint i64* %__r_12 to i64 
  %__r_15 = ptrtoint i64* %__r_13 to i64 
  %__r_16 = add   i64 %__r_14, %__r_15 
  %reg30 = inttoptr i64 %__r_16 to i64* 
  %__r_17 = ptrtoint i64* %__x141 to i64 
  %__r_18 = ptrtoint i64* %reg30 to i64 
  %__r_19 = sub   i64 %__r_17, %__r_18 
  %__r_20 = icmp slt i64 0, %__r_19 
  %__r_21 = zext i1 %__r_20 to i64  
  %__r_22 = mul   i64 %__r_19, %__r_21 
  %reg29 = inttoptr i64 %__r_22 to i64* 
  %__r_24 = inttoptr i64 0 to i64* 
  %cond23 = icmp eq i64* %reg29, %__r_24 
  br i1 %cond23, label %then26, label %else27 
then26:
  %__r_25 = inttoptr i64 1 to i64* 
  %__r_26 = inttoptr i64 0 to i64* 
  %__r_27 = ptrtoint i64* %__r_25 to i64 
  %__r_28 = ptrtoint i64* %__r_26 to i64 
  %__r_29 = add   i64 %__r_27, %__r_28 
  %reg31 = inttoptr i64 %__r_29 to i64* 
  br label %ifcont28 
else27:
  %__r_30 = inttoptr i64 0 to i64* 
  %__r_31 = ptrtoint i64* %__fib144 to i64 
  %__r_32 = ptrtoint i64* %__r_30 to i64 
  %__r_33 = add   i64 %__r_31, %__r_32 
  %__e145 = inttoptr i64 %__r_33 to i64* 
  %__r_35 = bitcast i64* %__e145 to i64** 
  %addr34 = getelementptr  i64*, i64** %__r_35, i64 0 
  %reg34 = load  i64*, i64** %addr34 
  %__r_36 = inttoptr i64 1 to i64* 
  %__r_37 = inttoptr i64 0 to i64* 
  %__r_38 = ptrtoint i64* %__r_36 to i64 
  %__r_39 = ptrtoint i64* %__r_37 to i64 
  %__r_40 = add   i64 %__r_38, %__r_39 
  %reg36 = inttoptr i64 %__r_40 to i64* 
  %__r_41 = ptrtoint i64* %__x141 to i64 
  %__r_42 = ptrtoint i64* %reg36 to i64 
  %__r_43 = sub   i64 %__r_41, %__r_42 
  %__r_44 = icmp slt i64 0, %__r_43 
  %__r_45 = zext i1 %__r_44 to i64  
  %__r_46 = mul   i64 %__r_43, %__r_45 
  %reg35 = inttoptr i64 %__r_46 to i64* 
  %fun47 = bitcast i64* %reg34 to i64* (i64*, i64*)* 
  %reg33 =  call ccc  i64*  %fun47(i64*  %__e145, i64*  %reg35)  
  %__r_48 = inttoptr i64 0 to i64* 
  %__r_49 = ptrtoint i64* %__fib144 to i64 
  %__r_50 = ptrtoint i64* %__r_48 to i64 
  %__r_51 = add   i64 %__r_49, %__r_50 
  %__e146 = inttoptr i64 %__r_51 to i64* 
  %__r_53 = bitcast i64* %__e146 to i64** 
  %addr52 = getelementptr  i64*, i64** %__r_53, i64 0 
  %reg38 = load  i64*, i64** %addr52 
  %__r_54 = inttoptr i64 1 to i64* 
  %__r_55 = inttoptr i64 0 to i64* 
  %__r_56 = ptrtoint i64* %__r_54 to i64 
  %__r_57 = ptrtoint i64* %__r_55 to i64 
  %__r_58 = add   i64 %__r_56, %__r_57 
  %reg41 = inttoptr i64 %__r_58 to i64* 
  %__r_59 = ptrtoint i64* %__x141 to i64 
  %__r_60 = ptrtoint i64* %reg41 to i64 
  %__r_61 = sub   i64 %__r_59, %__r_60 
  %__r_62 = icmp slt i64 0, %__r_61 
  %__r_63 = zext i1 %__r_62 to i64  
  %__r_64 = mul   i64 %__r_61, %__r_63 
  %reg40 = inttoptr i64 %__r_64 to i64* 
  %__r_65 = inttoptr i64 1 to i64* 
  %__r_66 = inttoptr i64 0 to i64* 
  %__r_67 = ptrtoint i64* %__r_65 to i64 
  %__r_68 = ptrtoint i64* %__r_66 to i64 
  %__r_69 = add   i64 %__r_67, %__r_68 
  %reg42 = inttoptr i64 %__r_69 to i64* 
  %__r_70 = ptrtoint i64* %reg40 to i64 
  %__r_71 = ptrtoint i64* %reg42 to i64 
  %__r_72 = sub   i64 %__r_70, %__r_71 
  %__r_73 = icmp slt i64 0, %__r_72 
  %__r_74 = zext i1 %__r_73 to i64  
  %__r_75 = mul   i64 %__r_72, %__r_74 
  %reg39 = inttoptr i64 %__r_75 to i64* 
  %fun76 = bitcast i64* %reg38 to i64* (i64*, i64*)* 
  %reg37 =  call ccc  i64*  %fun76(i64*  %__e146, i64*  %reg39)  
  %__r_77 = ptrtoint i64* %reg33 to i64 
  %__r_78 = ptrtoint i64* %reg37 to i64 
  %__r_79 = add   i64 %__r_77, %__r_78 
  %reg32 = inttoptr i64 %__r_79 to i64* 
  br label %ifcont28 
ifcont28:
  %regcont43 = phi i64* [%reg31, %then26], [%reg32, %else27] 
  br label %ifcont23 
ifcont23:
  %regcont44 = phi i64* [%reg24, %then21], [%regcont43, %ifcont28] 
  ret i64* %regcont44 
}


define external ccc  i64* @__133(i64*  %__clo134, i64*  %__x132)    {
__133:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo134 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__resta135 = inttoptr i64 %__r_4 to i64* 
  %reg45 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__137, i64  2, i64*  %__resta135, i64*  %__x132)  
  ret i64* %reg45 
}


define external ccc  i64* @__137(i64*  %__clo138, i64*  %__y136)    {
__137:
  %__r_2 = bitcast i64* %__clo138 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg46 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg46 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__resta135 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo138 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg47 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg47 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__x132 = inttoptr i64 %__r_12 to i64* 
  br label %entry48 
entry48:
  %__r_14 = inttoptr i64 0 to i64* 
  %cond13 = icmp eq i64* %__y136, %__r_14 
  br i1 %cond13, label %then49, label %else50 
then49:
  br label %ifcont51 
else50:
  br label %entry52 
entry52:
  %__r_16 = inttoptr i64 0 to i64* 
  %cond15 = icmp eq i64* %__x132, %__r_16 
  br i1 %cond15, label %then53, label %else54 
then53:
  %__r_17 = inttoptr i64 0 to i64* 
  %__r_18 = inttoptr i64 0 to i64* 
  %__r_19 = ptrtoint i64* %__r_17 to i64 
  %__r_20 = ptrtoint i64* %__r_18 to i64 
  %__r_21 = add   i64 %__r_19, %__r_20 
  %reg56 = inttoptr i64 %__r_21 to i64* 
  br label %ifcont55 
else54:
  %__r_22 = inttoptr i64 0 to i64* 
  %__r_23 = ptrtoint i64* %__resta135 to i64 
  %__r_24 = ptrtoint i64* %__r_22 to i64 
  %__r_25 = add   i64 %__r_23, %__r_24 
  %__e139 = inttoptr i64 %__r_25 to i64* 
  %__r_27 = bitcast i64* %__e139 to i64** 
  %addr26 = getelementptr  i64*, i64** %__r_27, i64 0 
  %reg58 = load  i64*, i64** %addr26 
  %__r_28 = inttoptr i64 1 to i64* 
  %__r_29 = inttoptr i64 0 to i64* 
  %__r_30 = ptrtoint i64* %__r_28 to i64 
  %__r_31 = ptrtoint i64* %__r_29 to i64 
  %__r_32 = add   i64 %__r_30, %__r_31 
  %reg60 = inttoptr i64 %__r_32 to i64* 
  %__r_33 = ptrtoint i64* %__x132 to i64 
  %__r_34 = ptrtoint i64* %reg60 to i64 
  %__r_35 = sub   i64 %__r_33, %__r_34 
  %__r_36 = icmp slt i64 0, %__r_35 
  %__r_37 = zext i1 %__r_36 to i64  
  %__r_38 = mul   i64 %__r_35, %__r_37 
  %reg59 = inttoptr i64 %__r_38 to i64* 
  %fun39 = bitcast i64* %reg58 to i64* (i64*, i64*)* 
  %reg57 =  call ccc  i64*  %fun39(i64*  %__e139, i64*  %reg59)  
  %__r_40 = inttoptr i64 0 to i64* 
  %__r_41 = ptrtoint i64* %reg57 to i64 
  %__r_42 = ptrtoint i64* %__r_40 to i64 
  %__r_43 = add   i64 %__r_41, %__r_42 
  %__e140 = inttoptr i64 %__r_43 to i64* 
  %__r_45 = bitcast i64* %__e140 to i64** 
  %addr44 = getelementptr  i64*, i64** %__r_45, i64 0 
  %reg62 = load  i64*, i64** %addr44 
  %__r_46 = inttoptr i64 1 to i64* 
  %__r_47 = inttoptr i64 0 to i64* 
  %__r_48 = ptrtoint i64* %__r_46 to i64 
  %__r_49 = ptrtoint i64* %__r_47 to i64 
  %__r_50 = add   i64 %__r_48, %__r_49 
  %reg64 = inttoptr i64 %__r_50 to i64* 
  %__r_51 = ptrtoint i64* %__y136 to i64 
  %__r_52 = ptrtoint i64* %reg64 to i64 
  %__r_53 = sub   i64 %__r_51, %__r_52 
  %__r_54 = icmp slt i64 0, %__r_53 
  %__r_55 = zext i1 %__r_54 to i64  
  %__r_56 = mul   i64 %__r_53, %__r_55 
  %reg63 = inttoptr i64 %__r_56 to i64* 
  %fun57 = bitcast i64* %reg62 to i64* (i64*, i64*)* 
  %reg61 =  call ccc  i64*  %fun57(i64*  %__e140, i64*  %reg63)  
  br label %ifcont55 
ifcont55:
  %regcont65 = phi i64* [%reg56, %then53], [%reg61, %else54] 
  br label %ifcont51 
ifcont51:
  %regcont66 = phi i64* [%__x132, %then49], [%regcont65, %ifcont55] 
  ret i64* %regcont66 
}


define external ccc  i64* @__124(i64*  %__clo125, i64*  %__x123)    {
__124:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo125 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__mult126 = inttoptr i64 %__r_4 to i64* 
  %reg67 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__128, i64  2, i64*  %__mult126, i64*  %__x123)  
  ret i64* %reg67 
}


define external ccc  i64* @__128(i64*  %__clo129, i64*  %__y127)    {
__128:
  %__r_2 = bitcast i64* %__clo129 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg68 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg68 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__mult126 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo129 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg69 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg69 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__x123 = inttoptr i64 %__r_12 to i64* 
  br label %entry70 
entry70:
  %__r_14 = inttoptr i64 0 to i64* 
  %cond13 = icmp eq i64* %__y127, %__r_14 
  br i1 %cond13, label %then71, label %else72 
then71:
  %__r_15 = inttoptr i64 0 to i64* 
  %__r_16 = inttoptr i64 0 to i64* 
  %__r_17 = ptrtoint i64* %__r_15 to i64 
  %__r_18 = ptrtoint i64* %__r_16 to i64 
  %__r_19 = add   i64 %__r_17, %__r_18 
  %reg74 = inttoptr i64 %__r_19 to i64* 
  br label %ifcont73 
else72:
  %__r_20 = inttoptr i64 0 to i64* 
  %__r_21 = ptrtoint i64* %__mult126 to i64 
  %__r_22 = ptrtoint i64* %__r_20 to i64 
  %__r_23 = add   i64 %__r_21, %__r_22 
  %__e130 = inttoptr i64 %__r_23 to i64* 
  %__r_25 = bitcast i64* %__e130 to i64** 
  %addr24 = getelementptr  i64*, i64** %__r_25, i64 0 
  %reg77 = load  i64*, i64** %addr24 
  %fun26 = bitcast i64* %reg77 to i64* (i64*, i64*)* 
  %reg76 =  call ccc  i64*  %fun26(i64*  %__e130, i64*  %__x123)  
  %__r_27 = inttoptr i64 0 to i64* 
  %__r_28 = ptrtoint i64* %reg76 to i64 
  %__r_29 = ptrtoint i64* %__r_27 to i64 
  %__r_30 = add   i64 %__r_28, %__r_29 
  %__e131 = inttoptr i64 %__r_30 to i64* 
  %__r_32 = bitcast i64* %__e131 to i64** 
  %addr31 = getelementptr  i64*, i64** %__r_32, i64 0 
  %reg79 = load  i64*, i64** %addr31 
  %__r_33 = inttoptr i64 1 to i64* 
  %__r_34 = inttoptr i64 0 to i64* 
  %__r_35 = ptrtoint i64* %__r_33 to i64 
  %__r_36 = ptrtoint i64* %__r_34 to i64 
  %__r_37 = add   i64 %__r_35, %__r_36 
  %reg81 = inttoptr i64 %__r_37 to i64* 
  %__r_38 = ptrtoint i64* %__y127 to i64 
  %__r_39 = ptrtoint i64* %reg81 to i64 
  %__r_40 = sub   i64 %__r_38, %__r_39 
  %__r_41 = icmp slt i64 0, %__r_40 
  %__r_42 = zext i1 %__r_41 to i64  
  %__r_43 = mul   i64 %__r_40, %__r_42 
  %reg80 = inttoptr i64 %__r_43 to i64* 
  %fun44 = bitcast i64* %reg79 to i64* (i64*, i64*)* 
  %reg78 =  call ccc  i64*  %fun44(i64*  %__e131, i64*  %reg80)  
  %__r_45 = ptrtoint i64* %__x123 to i64 
  %__r_46 = ptrtoint i64* %reg78 to i64 
  %__r_47 = add   i64 %__r_45, %__r_46 
  %reg75 = inttoptr i64 %__r_47 to i64* 
  br label %ifcont73 
ifcont73:
  %regcont82 = phi i64* [%reg74, %then71], [%reg75, %else72] 
  ret i64* %regcont82 
}


define external ccc  i64* @__113(i64*  %__clo114, i64*  %__x112)    {
__113:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo114 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__exp115 = inttoptr i64 %__r_4 to i64* 
  %reg83 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__117, i64  2, i64*  %__exp115, i64*  %__x112)  
  ret i64* %reg83 
}


define external ccc  i64* @__117(i64*  %__clo118, i64*  %__y116)    {
__117:
  %__r_2 = bitcast i64* %__clo118 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg84 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg84 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__exp115 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo118 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg85 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg85 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__x112 = inttoptr i64 %__r_12 to i64* 
  br label %entry86 
entry86:
  %__r_14 = inttoptr i64 0 to i64* 
  %cond13 = icmp eq i64* %__y116, %__r_14 
  br i1 %cond13, label %then87, label %else88 
then87:
  %__r_15 = inttoptr i64 1 to i64* 
  %__r_16 = inttoptr i64 0 to i64* 
  %__r_17 = ptrtoint i64* %__r_15 to i64 
  %__r_18 = ptrtoint i64* %__r_16 to i64 
  %__r_19 = add   i64 %__r_17, %__r_18 
  %reg90 = inttoptr i64 %__r_19 to i64* 
  br label %ifcont89 
else88:
  %__r_20 = load  i64*, i64** @mult 
  %__r_21 = inttoptr i64 0 to i64* 
  %__r_22 = ptrtoint i64* %__r_20 to i64 
  %__r_23 = ptrtoint i64* %__r_21 to i64 
  %__r_24 = add   i64 %__r_22, %__r_23 
  %__e119 = inttoptr i64 %__r_24 to i64* 
  %__r_26 = bitcast i64* %__e119 to i64** 
  %addr25 = getelementptr  i64*, i64** %__r_26, i64 0 
  %reg92 = load  i64*, i64** %addr25 
  %fun27 = bitcast i64* %reg92 to i64* (i64*, i64*)* 
  %reg91 =  call ccc  i64*  %fun27(i64*  %__e119, i64*  %__x112)  
  %__r_28 = inttoptr i64 0 to i64* 
  %__r_29 = ptrtoint i64* %reg91 to i64 
  %__r_30 = ptrtoint i64* %__r_28 to i64 
  %__r_31 = add   i64 %__r_29, %__r_30 
  %__e122 = inttoptr i64 %__r_31 to i64* 
  %__r_33 = bitcast i64* %__e122 to i64** 
  %addr32 = getelementptr  i64*, i64** %__r_33, i64 0 
  %reg94 = load  i64*, i64** %addr32 
  %__r_34 = inttoptr i64 0 to i64* 
  %__r_35 = ptrtoint i64* %__exp115 to i64 
  %__r_36 = ptrtoint i64* %__r_34 to i64 
  %__r_37 = add   i64 %__r_35, %__r_36 
  %__e120 = inttoptr i64 %__r_37 to i64* 
  %__r_39 = bitcast i64* %__e120 to i64** 
  %addr38 = getelementptr  i64*, i64** %__r_39, i64 0 
  %reg96 = load  i64*, i64** %addr38 
  %fun40 = bitcast i64* %reg96 to i64* (i64*, i64*)* 
  %reg95 =  call ccc  i64*  %fun40(i64*  %__e120, i64*  %__x112)  
  %__r_41 = inttoptr i64 0 to i64* 
  %__r_42 = ptrtoint i64* %reg95 to i64 
  %__r_43 = ptrtoint i64* %__r_41 to i64 
  %__r_44 = add   i64 %__r_42, %__r_43 
  %__e121 = inttoptr i64 %__r_44 to i64* 
  %__r_46 = bitcast i64* %__e121 to i64** 
  %addr45 = getelementptr  i64*, i64** %__r_46, i64 0 
  %reg98 = load  i64*, i64** %addr45 
  %__r_47 = inttoptr i64 1 to i64* 
  %__r_48 = inttoptr i64 0 to i64* 
  %__r_49 = ptrtoint i64* %__r_47 to i64 
  %__r_50 = ptrtoint i64* %__r_48 to i64 
  %__r_51 = add   i64 %__r_49, %__r_50 
  %reg100 = inttoptr i64 %__r_51 to i64* 
  %__r_52 = ptrtoint i64* %__y116 to i64 
  %__r_53 = ptrtoint i64* %reg100 to i64 
  %__r_54 = sub   i64 %__r_52, %__r_53 
  %__r_55 = icmp slt i64 0, %__r_54 
  %__r_56 = zext i1 %__r_55 to i64  
  %__r_57 = mul   i64 %__r_54, %__r_56 
  %reg99 = inttoptr i64 %__r_57 to i64* 
  %fun58 = bitcast i64* %reg98 to i64* (i64*, i64*)* 
  %reg97 =  call ccc  i64*  %fun58(i64*  %__e121, i64*  %reg99)  
  %fun59 = bitcast i64* %reg94 to i64* (i64*, i64*)* 
  %reg93 =  call ccc  i64*  %fun59(i64*  %__e122, i64*  %reg97)  
  br label %ifcont89 
ifcont89:
  %regcont101 = phi i64* [%reg90, %then87], [%reg93, %else88] 
  ret i64* %regcont101 
}


define external ccc  i64* @__106(i64*  %__clo107, i64*  %__x105)    {
__106:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo107 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__fact108 = inttoptr i64 %__r_4 to i64* 
  br label %entry102 
entry102:
  %__r_6 = inttoptr i64 0 to i64* 
  %cond5 = icmp eq i64* %__x105, %__r_6 
  br i1 %cond5, label %then103, label %else104 
then103:
  %__r_7 = inttoptr i64 1 to i64* 
  %__r_8 = inttoptr i64 0 to i64* 
  %__r_9 = ptrtoint i64* %__r_7 to i64 
  %__r_10 = ptrtoint i64* %__r_8 to i64 
  %__r_11 = add   i64 %__r_9, %__r_10 
  %reg106 = inttoptr i64 %__r_11 to i64* 
  br label %ifcont105 
else104:
  %__r_12 = load  i64*, i64** @mult 
  %__r_13 = inttoptr i64 0 to i64* 
  %__r_14 = ptrtoint i64* %__r_12 to i64 
  %__r_15 = ptrtoint i64* %__r_13 to i64 
  %__r_16 = add   i64 %__r_14, %__r_15 
  %__e109 = inttoptr i64 %__r_16 to i64* 
  %__r_18 = bitcast i64* %__e109 to i64** 
  %addr17 = getelementptr  i64*, i64** %__r_18, i64 0 
  %reg108 = load  i64*, i64** %addr17 
  %fun19 = bitcast i64* %reg108 to i64* (i64*, i64*)* 
  %reg107 =  call ccc  i64*  %fun19(i64*  %__e109, i64*  %__x105)  
  %__r_20 = inttoptr i64 0 to i64* 
  %__r_21 = ptrtoint i64* %reg107 to i64 
  %__r_22 = ptrtoint i64* %__r_20 to i64 
  %__r_23 = add   i64 %__r_21, %__r_22 
  %__e111 = inttoptr i64 %__r_23 to i64* 
  %__r_25 = bitcast i64* %__e111 to i64** 
  %addr24 = getelementptr  i64*, i64** %__r_25, i64 0 
  %reg110 = load  i64*, i64** %addr24 
  %__r_26 = inttoptr i64 0 to i64* 
  %__r_27 = ptrtoint i64* %__fact108 to i64 
  %__r_28 = ptrtoint i64* %__r_26 to i64 
  %__r_29 = add   i64 %__r_27, %__r_28 
  %__e110 = inttoptr i64 %__r_29 to i64* 
  %__r_31 = bitcast i64* %__e110 to i64** 
  %addr30 = getelementptr  i64*, i64** %__r_31, i64 0 
  %reg112 = load  i64*, i64** %addr30 
  %__r_32 = inttoptr i64 1 to i64* 
  %__r_33 = inttoptr i64 0 to i64* 
  %__r_34 = ptrtoint i64* %__r_32 to i64 
  %__r_35 = ptrtoint i64* %__r_33 to i64 
  %__r_36 = add   i64 %__r_34, %__r_35 
  %reg114 = inttoptr i64 %__r_36 to i64* 
  %__r_37 = ptrtoint i64* %__x105 to i64 
  %__r_38 = ptrtoint i64* %reg114 to i64 
  %__r_39 = sub   i64 %__r_37, %__r_38 
  %__r_40 = icmp slt i64 0, %__r_39 
  %__r_41 = zext i1 %__r_40 to i64  
  %__r_42 = mul   i64 %__r_39, %__r_41 
  %reg113 = inttoptr i64 %__r_42 to i64* 
  %fun43 = bitcast i64* %reg112 to i64* (i64*, i64*)* 
  %reg111 =  call ccc  i64*  %fun43(i64*  %__e110, i64*  %reg113)  
  %fun44 = bitcast i64* %reg110 to i64* (i64*, i64*)* 
  %reg109 =  call ccc  i64*  %fun44(i64*  %__e111, i64*  %reg111)  
  br label %ifcont105 
ifcont105:
  %regcont115 = phi i64* [%reg106, %then103], [%reg109, %else104] 
  ret i64* %regcont115 
}


define external ccc  i64* @__95(i64*  %__clo96, i64*  %__n94)    {
__95:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo96 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__gcd97 = inttoptr i64 %__r_4 to i64* 
  %reg116 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__99, i64  2, i64*  %__gcd97, i64*  %__n94)  
  ret i64* %reg116 
}


define external ccc  i64* @__99(i64*  %__clo100, i64*  %__m98)    {
__99:
  %__r_2 = bitcast i64* %__clo100 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg117 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg117 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__gcd97 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo100 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg118 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg118 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__n94 = inttoptr i64 %__r_12 to i64* 
  br label %entry119 
entry119:
  %__r_14 = inttoptr i64 0 to i64* 
  %cond13 = icmp eq i64* %__n94, %__r_14 
  br i1 %cond13, label %then120, label %else121 
then120:
  br label %ifcont122 
else121:
  br label %entry123 
entry123:
  %__r_16 = inttoptr i64 0 to i64* 
  %cond15 = icmp eq i64* %__m98, %__r_16 
  br i1 %cond15, label %then124, label %else125 
then124:
  br label %ifcont126 
else125:
  br label %entry127 
entry127:
  %__r_17 = ptrtoint i64* %__n94 to i64 
  %__r_18 = ptrtoint i64* %__m98 to i64 
  %__r_19 = sub   i64 %__r_17, %__r_18 
  %__r_20 = icmp slt i64 0, %__r_19 
  %__r_21 = zext i1 %__r_20 to i64  
  %__r_22 = mul   i64 %__r_19, %__r_21 
  %reg131 = inttoptr i64 %__r_22 to i64* 
  %__r_24 = inttoptr i64 0 to i64* 
  %cond23 = icmp eq i64* %reg131, %__r_24 
  br i1 %cond23, label %then128, label %else129 
then128:
  %__r_25 = inttoptr i64 0 to i64* 
  %__r_26 = ptrtoint i64* %__gcd97 to i64 
  %__r_27 = ptrtoint i64* %__r_25 to i64 
  %__r_28 = add   i64 %__r_26, %__r_27 
  %__e101 = inttoptr i64 %__r_28 to i64* 
  %__r_30 = bitcast i64* %__e101 to i64** 
  %addr29 = getelementptr  i64*, i64** %__r_30, i64 0 
  %reg133 = load  i64*, i64** %addr29 
  %__r_31 = ptrtoint i64* %__m98 to i64 
  %__r_32 = ptrtoint i64* %__n94 to i64 
  %__r_33 = sub   i64 %__r_31, %__r_32 
  %__r_34 = icmp slt i64 0, %__r_33 
  %__r_35 = zext i1 %__r_34 to i64  
  %__r_36 = mul   i64 %__r_33, %__r_35 
  %reg134 = inttoptr i64 %__r_36 to i64* 
  %fun37 = bitcast i64* %reg133 to i64* (i64*, i64*)* 
  %reg132 =  call ccc  i64*  %fun37(i64*  %__e101, i64*  %reg134)  
  %__r_38 = inttoptr i64 0 to i64* 
  %__r_39 = ptrtoint i64* %reg132 to i64 
  %__r_40 = ptrtoint i64* %__r_38 to i64 
  %__r_41 = add   i64 %__r_39, %__r_40 
  %__e102 = inttoptr i64 %__r_41 to i64* 
  %__r_43 = bitcast i64* %__e102 to i64** 
  %addr42 = getelementptr  i64*, i64** %__r_43, i64 0 
  %reg136 = load  i64*, i64** %addr42 
  %fun44 = bitcast i64* %reg136 to i64* (i64*, i64*)* 
  %reg135 =  call ccc  i64*  %fun44(i64*  %__e102, i64*  %__n94)  
  br label %ifcont130 
else129:
  %__r_45 = inttoptr i64 0 to i64* 
  %__r_46 = ptrtoint i64* %__gcd97 to i64 
  %__r_47 = ptrtoint i64* %__r_45 to i64 
  %__r_48 = add   i64 %__r_46, %__r_47 
  %__e103 = inttoptr i64 %__r_48 to i64* 
  %__r_50 = bitcast i64* %__e103 to i64** 
  %addr49 = getelementptr  i64*, i64** %__r_50, i64 0 
  %reg138 = load  i64*, i64** %addr49 
  %fun51 = bitcast i64* %reg138 to i64* (i64*, i64*)* 
  %reg137 =  call ccc  i64*  %fun51(i64*  %__e103, i64*  %__m98)  
  %__r_52 = inttoptr i64 0 to i64* 
  %__r_53 = ptrtoint i64* %reg137 to i64 
  %__r_54 = ptrtoint i64* %__r_52 to i64 
  %__r_55 = add   i64 %__r_53, %__r_54 
  %__e104 = inttoptr i64 %__r_55 to i64* 
  %__r_57 = bitcast i64* %__e104 to i64** 
  %addr56 = getelementptr  i64*, i64** %__r_57, i64 0 
  %reg140 = load  i64*, i64** %addr56 
  %__r_58 = ptrtoint i64* %__n94 to i64 
  %__r_59 = ptrtoint i64* %__m98 to i64 
  %__r_60 = sub   i64 %__r_58, %__r_59 
  %__r_61 = icmp slt i64 0, %__r_60 
  %__r_62 = zext i1 %__r_61 to i64  
  %__r_63 = mul   i64 %__r_60, %__r_62 
  %reg141 = inttoptr i64 %__r_63 to i64* 
  %fun64 = bitcast i64* %reg140 to i64* (i64*, i64*)* 
  %reg139 =  call ccc  i64*  %fun64(i64*  %__e104, i64*  %reg141)  
  br label %ifcont130 
ifcont130:
  %regcont142 = phi i64* [%reg135, %then128], [%reg139, %else129] 
  br label %ifcont126 
ifcont126:
  %regcont143 = phi i64* [%__n94, %then124], [%regcont142, %ifcont130] 
  br label %ifcont122 
ifcont122:
  %regcont144 = phi i64* [%__m98, %then120], [%regcont143, %ifcont126] 
  ret i64* %regcont144 
}


define external ccc  i64* @__78(i64*  %__clo79, i64*  %__n77)    {
__78:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo79 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__gcd280 = inttoptr i64 %__r_4 to i64* 
  %reg145 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__82, i64  2, i64*  %__gcd280, i64*  %__n77)  
  ret i64* %reg145 
}


define external ccc  i64* @__82(i64*  %__clo83, i64*  %__m81)    {
__82:
  %__r_2 = bitcast i64* %__clo83 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg146 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg146 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__gcd280 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo83 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg147 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg147 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__n77 = inttoptr i64 %__r_12 to i64* 
  br label %entry148 
entry148:
  %__r_14 = inttoptr i64 0 to i64* 
  %cond13 = icmp eq i64* %__n77, %__r_14 
  br i1 %cond13, label %then149, label %else150 
then149:
  br label %ifcont151 
else150:
  br label %entry152 
entry152:
  %__r_16 = inttoptr i64 0 to i64* 
  %cond15 = icmp eq i64* %__m81, %__r_16 
  br i1 %cond15, label %then153, label %else154 
then153:
  br label %ifcont155 
else154:
  br label %entry156 
entry156:
  %__r_17 = load  i64*, i64** @resta 
  %__r_18 = inttoptr i64 0 to i64* 
  %__r_19 = ptrtoint i64* %__r_17 to i64 
  %__r_20 = ptrtoint i64* %__r_18 to i64 
  %__r_21 = add   i64 %__r_19, %__r_20 
  %__e84 = inttoptr i64 %__r_21 to i64* 
  %__r_23 = bitcast i64* %__e84 to i64** 
  %addr22 = getelementptr  i64*, i64** %__r_23, i64 0 
  %reg161 = load  i64*, i64** %addr22 
  %fun24 = bitcast i64* %reg161 to i64* (i64*, i64*)* 
  %reg160 =  call ccc  i64*  %fun24(i64*  %__e84, i64*  %__n77)  
  %__r_25 = inttoptr i64 0 to i64* 
  %__r_26 = ptrtoint i64* %reg160 to i64 
  %__r_27 = ptrtoint i64* %__r_25 to i64 
  %__r_28 = add   i64 %__r_26, %__r_27 
  %__e85 = inttoptr i64 %__r_28 to i64* 
  %__r_30 = bitcast i64* %__e85 to i64** 
  %addr29 = getelementptr  i64*, i64** %__r_30, i64 0 
  %reg163 = load  i64*, i64** %addr29 
  %fun31 = bitcast i64* %reg163 to i64* (i64*, i64*)* 
  %reg162 =  call ccc  i64*  %fun31(i64*  %__e85, i64*  %__m81)  
  %__r_33 = inttoptr i64 0 to i64* 
  %cond32 = icmp eq i64* %reg162, %__r_33 
  br i1 %cond32, label %then157, label %else158 
then157:
  %__r_34 = inttoptr i64 0 to i64* 
  %__r_35 = ptrtoint i64* %__gcd280 to i64 
  %__r_36 = ptrtoint i64* %__r_34 to i64 
  %__r_37 = add   i64 %__r_35, %__r_36 
  %__e88 = inttoptr i64 %__r_37 to i64* 
  %__r_39 = bitcast i64* %__e88 to i64** 
  %addr38 = getelementptr  i64*, i64** %__r_39, i64 0 
  %reg165 = load  i64*, i64** %addr38 
  %__r_40 = load  i64*, i64** @resta 
  %__r_41 = inttoptr i64 0 to i64* 
  %__r_42 = ptrtoint i64* %__r_40 to i64 
  %__r_43 = ptrtoint i64* %__r_41 to i64 
  %__r_44 = add   i64 %__r_42, %__r_43 
  %__e86 = inttoptr i64 %__r_44 to i64* 
  %__r_46 = bitcast i64* %__e86 to i64** 
  %addr45 = getelementptr  i64*, i64** %__r_46, i64 0 
  %reg167 = load  i64*, i64** %addr45 
  %fun47 = bitcast i64* %reg167 to i64* (i64*, i64*)* 
  %reg166 =  call ccc  i64*  %fun47(i64*  %__e86, i64*  %__m81)  
  %__r_48 = inttoptr i64 0 to i64* 
  %__r_49 = ptrtoint i64* %reg166 to i64 
  %__r_50 = ptrtoint i64* %__r_48 to i64 
  %__r_51 = add   i64 %__r_49, %__r_50 
  %__e87 = inttoptr i64 %__r_51 to i64* 
  %__r_53 = bitcast i64* %__e87 to i64** 
  %addr52 = getelementptr  i64*, i64** %__r_53, i64 0 
  %reg169 = load  i64*, i64** %addr52 
  %fun54 = bitcast i64* %reg169 to i64* (i64*, i64*)* 
  %reg168 =  call ccc  i64*  %fun54(i64*  %__e87, i64*  %__n77)  
  %fun55 = bitcast i64* %reg165 to i64* (i64*, i64*)* 
  %reg164 =  call ccc  i64*  %fun55(i64*  %__e88, i64*  %reg168)  
  %__r_56 = inttoptr i64 0 to i64* 
  %__r_57 = ptrtoint i64* %reg164 to i64 
  %__r_58 = ptrtoint i64* %__r_56 to i64 
  %__r_59 = add   i64 %__r_57, %__r_58 
  %__e89 = inttoptr i64 %__r_59 to i64* 
  %__r_61 = bitcast i64* %__e89 to i64** 
  %addr60 = getelementptr  i64*, i64** %__r_61, i64 0 
  %reg171 = load  i64*, i64** %addr60 
  %fun62 = bitcast i64* %reg171 to i64* (i64*, i64*)* 
  %reg170 =  call ccc  i64*  %fun62(i64*  %__e89, i64*  %__n77)  
  br label %ifcont159 
else158:
  %__r_63 = inttoptr i64 0 to i64* 
  %__r_64 = ptrtoint i64* %__gcd280 to i64 
  %__r_65 = ptrtoint i64* %__r_63 to i64 
  %__r_66 = add   i64 %__r_64, %__r_65 
  %__e90 = inttoptr i64 %__r_66 to i64* 
  %__r_68 = bitcast i64* %__e90 to i64** 
  %addr67 = getelementptr  i64*, i64** %__r_68, i64 0 
  %reg173 = load  i64*, i64** %addr67 
  %fun69 = bitcast i64* %reg173 to i64* (i64*, i64*)* 
  %reg172 =  call ccc  i64*  %fun69(i64*  %__e90, i64*  %__m81)  
  %__r_70 = inttoptr i64 0 to i64* 
  %__r_71 = ptrtoint i64* %reg172 to i64 
  %__r_72 = ptrtoint i64* %__r_70 to i64 
  %__r_73 = add   i64 %__r_71, %__r_72 
  %__e93 = inttoptr i64 %__r_73 to i64* 
  %__r_75 = bitcast i64* %__e93 to i64** 
  %addr74 = getelementptr  i64*, i64** %__r_75, i64 0 
  %reg175 = load  i64*, i64** %addr74 
  %__r_76 = load  i64*, i64** @resta 
  %__r_77 = inttoptr i64 0 to i64* 
  %__r_78 = ptrtoint i64* %__r_76 to i64 
  %__r_79 = ptrtoint i64* %__r_77 to i64 
  %__r_80 = add   i64 %__r_78, %__r_79 
  %__e91 = inttoptr i64 %__r_80 to i64* 
  %__r_82 = bitcast i64* %__e91 to i64** 
  %addr81 = getelementptr  i64*, i64** %__r_82, i64 0 
  %reg177 = load  i64*, i64** %addr81 
  %fun83 = bitcast i64* %reg177 to i64* (i64*, i64*)* 
  %reg176 =  call ccc  i64*  %fun83(i64*  %__e91, i64*  %__n77)  
  %__r_84 = inttoptr i64 0 to i64* 
  %__r_85 = ptrtoint i64* %reg176 to i64 
  %__r_86 = ptrtoint i64* %__r_84 to i64 
  %__r_87 = add   i64 %__r_85, %__r_86 
  %__e92 = inttoptr i64 %__r_87 to i64* 
  %__r_89 = bitcast i64* %__e92 to i64** 
  %addr88 = getelementptr  i64*, i64** %__r_89, i64 0 
  %reg179 = load  i64*, i64** %addr88 
  %fun90 = bitcast i64* %reg179 to i64* (i64*, i64*)* 
  %reg178 =  call ccc  i64*  %fun90(i64*  %__e92, i64*  %__m81)  
  %fun91 = bitcast i64* %reg175 to i64* (i64*, i64*)* 
  %reg174 =  call ccc  i64*  %fun91(i64*  %__e93, i64*  %reg178)  
  br label %ifcont159 
ifcont159:
  %regcont180 = phi i64* [%reg170, %then157], [%reg174, %else158] 
  br label %ifcont155 
ifcont155:
  %regcont181 = phi i64* [%__n77, %then153], [%regcont180, %ifcont159] 
  br label %ifcont151 
ifcont151:
  %regcont182 = phi i64* [%__m81, %then149], [%regcont181, %ifcont155] 
  ret i64* %regcont182 
}


define external ccc  i64* @__64(i64*  %__clo65, i64*  %__a63)    {
__64:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo65 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__multC266 = inttoptr i64 %__r_4 to i64* 
  %reg183 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__68, i64  2, i64*  %__a63, i64*  %__multC266)  
  ret i64* %reg183 
}


define external ccc  i64* @__68(i64*  %__clo69, i64*  %__b67)    {
__68:
  %__r_2 = bitcast i64* %__clo69 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg184 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg184 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__a63 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo69 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg185 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg185 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__multC266 = inttoptr i64 %__r_12 to i64* 
  %reg186 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__71, i64  3, i64*  %__a63, i64*  %__b67, i64*  %__multC266)  
  ret i64* %reg186 
}


define external ccc  i64* @__71(i64*  %__clo72, i64*  %__ac70)    {
__71:
  %__r_2 = bitcast i64* %__clo72 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg187 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg187 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__a63 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo72 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg188 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg188 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__b67 = inttoptr i64 %__r_12 to i64* 
  %__r_14 = bitcast i64* %__clo72 to i64** 
  %addr13 = getelementptr  i64*, i64** %__r_14, i64 3 
  %reg189 = load  i64*, i64** %addr13 
  %__r_15 = inttoptr i64 0 to i64* 
  %__r_16 = ptrtoint i64* %reg189 to i64 
  %__r_17 = ptrtoint i64* %__r_15 to i64 
  %__r_18 = add   i64 %__r_16, %__r_17 
  %__multC266 = inttoptr i64 %__r_18 to i64* 
  br label %entry190 
entry190:
  %__r_20 = inttoptr i64 0 to i64* 
  %cond19 = icmp eq i64* %__b67, %__r_20 
  br i1 %cond19, label %then191, label %else192 
then191:
  br label %ifcont193 
else192:
  %__r_21 = inttoptr i64 0 to i64* 
  %__r_22 = ptrtoint i64* %__multC266 to i64 
  %__r_23 = ptrtoint i64* %__r_21 to i64 
  %__r_24 = add   i64 %__r_22, %__r_23 
  %__e73 = inttoptr i64 %__r_24 to i64* 
  %__r_26 = bitcast i64* %__e73 to i64** 
  %addr25 = getelementptr  i64*, i64** %__r_26, i64 0 
  %reg195 = load  i64*, i64** %addr25 
  %fun27 = bitcast i64* %reg195 to i64* (i64*, i64*)* 
  %reg194 =  call ccc  i64*  %fun27(i64*  %__e73, i64*  %__a63)  
  %__r_28 = inttoptr i64 0 to i64* 
  %__r_29 = ptrtoint i64* %reg194 to i64 
  %__r_30 = ptrtoint i64* %__r_28 to i64 
  %__r_31 = add   i64 %__r_29, %__r_30 
  %__e74 = inttoptr i64 %__r_31 to i64* 
  %__r_33 = bitcast i64* %__e74 to i64** 
  %addr32 = getelementptr  i64*, i64** %__r_33, i64 0 
  %reg197 = load  i64*, i64** %addr32 
  %__r_34 = inttoptr i64 1 to i64* 
  %__r_35 = inttoptr i64 0 to i64* 
  %__r_36 = ptrtoint i64* %__r_34 to i64 
  %__r_37 = ptrtoint i64* %__r_35 to i64 
  %__r_38 = add   i64 %__r_36, %__r_37 
  %reg199 = inttoptr i64 %__r_38 to i64* 
  %__r_39 = ptrtoint i64* %__b67 to i64 
  %__r_40 = ptrtoint i64* %reg199 to i64 
  %__r_41 = sub   i64 %__r_39, %__r_40 
  %__r_42 = icmp slt i64 0, %__r_41 
  %__r_43 = zext i1 %__r_42 to i64  
  %__r_44 = mul   i64 %__r_41, %__r_43 
  %reg198 = inttoptr i64 %__r_44 to i64* 
  %fun45 = bitcast i64* %reg197 to i64* (i64*, i64*)* 
  %reg196 =  call ccc  i64*  %fun45(i64*  %__e74, i64*  %reg198)  
  %__r_46 = inttoptr i64 0 to i64* 
  %__r_47 = ptrtoint i64* %reg196 to i64 
  %__r_48 = ptrtoint i64* %__r_46 to i64 
  %__r_49 = add   i64 %__r_47, %__r_48 
  %__e75 = inttoptr i64 %__r_49 to i64* 
  %__r_51 = bitcast i64* %__e75 to i64** 
  %addr50 = getelementptr  i64*, i64** %__r_51, i64 0 
  %reg201 = load  i64*, i64** %addr50 
  %__r_52 = ptrtoint i64* %__ac70 to i64 
  %__r_53 = ptrtoint i64* %__a63 to i64 
  %__r_54 = add   i64 %__r_52, %__r_53 
  %reg202 = inttoptr i64 %__r_54 to i64* 
  %fun55 = bitcast i64* %reg201 to i64* (i64*, i64*)* 
  %reg200 =  call ccc  i64*  %fun55(i64*  %__e75, i64*  %reg202)  
  br label %ifcont193 
ifcont193:
  %regcont203 = phi i64* [%__ac70, %then191], [%reg200, %else192] 
  ret i64* %regcont203 
}


define external ccc  i64* @__55(i64*  %__clo56, i64*  %__a54)    {
__55:
  %reg204 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__58, i64  1, i64*  %__a54)  
  ret i64* %reg204 
}


define external ccc  i64* @__58(i64*  %__clo59, i64*  %__b57)    {
__58:
  %__r_2 = bitcast i64* %__clo59 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg205 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg205 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__a54 = inttoptr i64 %__r_6 to i64* 
  %__r_7 = load  i64*, i64** @multC2 
  %__r_8 = inttoptr i64 0 to i64* 
  %__r_9 = ptrtoint i64* %__r_7 to i64 
  %__r_10 = ptrtoint i64* %__r_8 to i64 
  %__r_11 = add   i64 %__r_9, %__r_10 
  %__e60 = inttoptr i64 %__r_11 to i64* 
  %__r_13 = bitcast i64* %__e60 to i64** 
  %addr12 = getelementptr  i64*, i64** %__r_13, i64 0 
  %reg207 = load  i64*, i64** %addr12 
  %fun14 = bitcast i64* %reg207 to i64* (i64*, i64*)* 
  %reg206 =  call ccc  i64*  %fun14(i64*  %__e60, i64*  %__a54)  
  %__r_15 = inttoptr i64 0 to i64* 
  %__r_16 = ptrtoint i64* %reg206 to i64 
  %__r_17 = ptrtoint i64* %__r_15 to i64 
  %__r_18 = add   i64 %__r_16, %__r_17 
  %__e61 = inttoptr i64 %__r_18 to i64* 
  %__r_20 = bitcast i64* %__e61 to i64** 
  %addr19 = getelementptr  i64*, i64** %__r_20, i64 0 
  %reg209 = load  i64*, i64** %addr19 
  %fun21 = bitcast i64* %reg209 to i64* (i64*, i64*)* 
  %reg208 =  call ccc  i64*  %fun21(i64*  %__e61, i64*  %__b57)  
  %__r_22 = inttoptr i64 0 to i64* 
  %__r_23 = ptrtoint i64* %reg208 to i64 
  %__r_24 = ptrtoint i64* %__r_22 to i64 
  %__r_25 = add   i64 %__r_23, %__r_24 
  %__e62 = inttoptr i64 %__r_25 to i64* 
  %__r_27 = bitcast i64* %__e62 to i64** 
  %addr26 = getelementptr  i64*, i64** %__r_27, i64 0 
  %reg211 = load  i64*, i64** %addr26 
  %__r_28 = inttoptr i64 0 to i64* 
  %__r_29 = inttoptr i64 0 to i64* 
  %__r_30 = ptrtoint i64* %__r_28 to i64 
  %__r_31 = ptrtoint i64* %__r_29 to i64 
  %__r_32 = add   i64 %__r_30, %__r_31 
  %reg212 = inttoptr i64 %__r_32 to i64* 
  %fun33 = bitcast i64* %reg211 to i64* (i64*, i64*)* 
  %reg210 =  call ccc  i64*  %fun33(i64*  %__e62, i64*  %reg212)  
  ret i64* %reg210 
}


define external ccc  i64* @__40(i64*  %__clo41, i64*  %__a39)    {
__40:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo41 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__expC242 = inttoptr i64 %__r_4 to i64* 
  %reg213 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__44, i64  2, i64*  %__a39, i64*  %__expC242)  
  ret i64* %reg213 
}


define external ccc  i64* @__44(i64*  %__clo45, i64*  %__b43)    {
__44:
  %__r_2 = bitcast i64* %__clo45 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg214 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg214 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__a39 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo45 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg215 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg215 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__expC242 = inttoptr i64 %__r_12 to i64* 
  %reg216 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__47, i64  3, i64*  %__a39, i64*  %__b43, i64*  %__expC242)  
  ret i64* %reg216 
}


define external ccc  i64* @__47(i64*  %__clo48, i64*  %__ac46)    {
__47:
  %__r_2 = bitcast i64* %__clo48 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg217 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg217 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__a39 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo48 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg218 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg218 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__b43 = inttoptr i64 %__r_12 to i64* 
  %__r_14 = bitcast i64* %__clo48 to i64** 
  %addr13 = getelementptr  i64*, i64** %__r_14, i64 3 
  %reg219 = load  i64*, i64** %addr13 
  %__r_15 = inttoptr i64 0 to i64* 
  %__r_16 = ptrtoint i64* %reg219 to i64 
  %__r_17 = ptrtoint i64* %__r_15 to i64 
  %__r_18 = add   i64 %__r_16, %__r_17 
  %__expC242 = inttoptr i64 %__r_18 to i64* 
  br label %entry220 
entry220:
  %__r_20 = inttoptr i64 0 to i64* 
  %cond19 = icmp eq i64* %__b43, %__r_20 
  br i1 %cond19, label %then221, label %else222 
then221:
  br label %ifcont223 
else222:
  %__r_21 = inttoptr i64 0 to i64* 
  %__r_22 = ptrtoint i64* %__expC242 to i64 
  %__r_23 = ptrtoint i64* %__r_21 to i64 
  %__r_24 = add   i64 %__r_22, %__r_23 
  %__e49 = inttoptr i64 %__r_24 to i64* 
  %__r_26 = bitcast i64* %__e49 to i64** 
  %addr25 = getelementptr  i64*, i64** %__r_26, i64 0 
  %reg225 = load  i64*, i64** %addr25 
  %fun27 = bitcast i64* %reg225 to i64* (i64*, i64*)* 
  %reg224 =  call ccc  i64*  %fun27(i64*  %__e49, i64*  %__a39)  
  %__r_28 = inttoptr i64 0 to i64* 
  %__r_29 = ptrtoint i64* %reg224 to i64 
  %__r_30 = ptrtoint i64* %__r_28 to i64 
  %__r_31 = add   i64 %__r_29, %__r_30 
  %__e50 = inttoptr i64 %__r_31 to i64* 
  %__r_33 = bitcast i64* %__e50 to i64** 
  %addr32 = getelementptr  i64*, i64** %__r_33, i64 0 
  %reg227 = load  i64*, i64** %addr32 
  %__r_34 = inttoptr i64 1 to i64* 
  %__r_35 = inttoptr i64 0 to i64* 
  %__r_36 = ptrtoint i64* %__r_34 to i64 
  %__r_37 = ptrtoint i64* %__r_35 to i64 
  %__r_38 = add   i64 %__r_36, %__r_37 
  %reg229 = inttoptr i64 %__r_38 to i64* 
  %__r_39 = ptrtoint i64* %__b43 to i64 
  %__r_40 = ptrtoint i64* %reg229 to i64 
  %__r_41 = sub   i64 %__r_39, %__r_40 
  %__r_42 = icmp slt i64 0, %__r_41 
  %__r_43 = zext i1 %__r_42 to i64  
  %__r_44 = mul   i64 %__r_41, %__r_43 
  %reg228 = inttoptr i64 %__r_44 to i64* 
  %fun45 = bitcast i64* %reg227 to i64* (i64*, i64*)* 
  %reg226 =  call ccc  i64*  %fun45(i64*  %__e50, i64*  %reg228)  
  %__r_46 = inttoptr i64 0 to i64* 
  %__r_47 = ptrtoint i64* %reg226 to i64 
  %__r_48 = ptrtoint i64* %__r_46 to i64 
  %__r_49 = add   i64 %__r_47, %__r_48 
  %__e53 = inttoptr i64 %__r_49 to i64* 
  %__r_51 = bitcast i64* %__e53 to i64** 
  %addr50 = getelementptr  i64*, i64** %__r_51, i64 0 
  %reg231 = load  i64*, i64** %addr50 
  %__r_52 = load  i64*, i64** @multC 
  %__r_53 = inttoptr i64 0 to i64* 
  %__r_54 = ptrtoint i64* %__r_52 to i64 
  %__r_55 = ptrtoint i64* %__r_53 to i64 
  %__r_56 = add   i64 %__r_54, %__r_55 
  %__e51 = inttoptr i64 %__r_56 to i64* 
  %__r_58 = bitcast i64* %__e51 to i64** 
  %addr57 = getelementptr  i64*, i64** %__r_58, i64 0 
  %reg233 = load  i64*, i64** %addr57 
  %fun59 = bitcast i64* %reg233 to i64* (i64*, i64*)* 
  %reg232 =  call ccc  i64*  %fun59(i64*  %__e51, i64*  %__a39)  
  %__r_60 = inttoptr i64 0 to i64* 
  %__r_61 = ptrtoint i64* %reg232 to i64 
  %__r_62 = ptrtoint i64* %__r_60 to i64 
  %__r_63 = add   i64 %__r_61, %__r_62 
  %__e52 = inttoptr i64 %__r_63 to i64* 
  %__r_65 = bitcast i64* %__e52 to i64** 
  %addr64 = getelementptr  i64*, i64** %__r_65, i64 0 
  %reg235 = load  i64*, i64** %addr64 
  %fun66 = bitcast i64* %reg235 to i64* (i64*, i64*)* 
  %reg234 =  call ccc  i64*  %fun66(i64*  %__e52, i64*  %__ac46)  
  %fun67 = bitcast i64* %reg231 to i64* (i64*, i64*)* 
  %reg230 =  call ccc  i64*  %fun67(i64*  %__e53, i64*  %reg234)  
  br label %ifcont223 
ifcont223:
  %regcont236 = phi i64* [%__ac46, %then221], [%reg230, %else222] 
  ret i64* %regcont236 
}


define external ccc  i64* @__31(i64*  %__clo32, i64*  %__a30)    {
__31:
  %reg237 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__34, i64  1, i64*  %__a30)  
  ret i64* %reg237 
}


define external ccc  i64* @__34(i64*  %__clo35, i64*  %__b33)    {
__34:
  %__r_2 = bitcast i64* %__clo35 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg238 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg238 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__a30 = inttoptr i64 %__r_6 to i64* 
  %__r_7 = load  i64*, i64** @expC2 
  %__r_8 = inttoptr i64 0 to i64* 
  %__r_9 = ptrtoint i64* %__r_7 to i64 
  %__r_10 = ptrtoint i64* %__r_8 to i64 
  %__r_11 = add   i64 %__r_9, %__r_10 
  %__e36 = inttoptr i64 %__r_11 to i64* 
  %__r_13 = bitcast i64* %__e36 to i64** 
  %addr12 = getelementptr  i64*, i64** %__r_13, i64 0 
  %reg240 = load  i64*, i64** %addr12 
  %fun14 = bitcast i64* %reg240 to i64* (i64*, i64*)* 
  %reg239 =  call ccc  i64*  %fun14(i64*  %__e36, i64*  %__a30)  
  %__r_15 = inttoptr i64 0 to i64* 
  %__r_16 = ptrtoint i64* %reg239 to i64 
  %__r_17 = ptrtoint i64* %__r_15 to i64 
  %__r_18 = add   i64 %__r_16, %__r_17 
  %__e37 = inttoptr i64 %__r_18 to i64* 
  %__r_20 = bitcast i64* %__e37 to i64** 
  %addr19 = getelementptr  i64*, i64** %__r_20, i64 0 
  %reg242 = load  i64*, i64** %addr19 
  %fun21 = bitcast i64* %reg242 to i64* (i64*, i64*)* 
  %reg241 =  call ccc  i64*  %fun21(i64*  %__e37, i64*  %__b33)  
  %__r_22 = inttoptr i64 0 to i64* 
  %__r_23 = ptrtoint i64* %reg241 to i64 
  %__r_24 = ptrtoint i64* %__r_22 to i64 
  %__r_25 = add   i64 %__r_23, %__r_24 
  %__e38 = inttoptr i64 %__r_25 to i64* 
  %__r_27 = bitcast i64* %__e38 to i64** 
  %addr26 = getelementptr  i64*, i64** %__r_27, i64 0 
  %reg244 = load  i64*, i64** %addr26 
  %__r_28 = inttoptr i64 1 to i64* 
  %__r_29 = inttoptr i64 0 to i64* 
  %__r_30 = ptrtoint i64* %__r_28 to i64 
  %__r_31 = ptrtoint i64* %__r_29 to i64 
  %__r_32 = add   i64 %__r_30, %__r_31 
  %reg245 = inttoptr i64 %__r_32 to i64* 
  %fun33 = bitcast i64* %reg244 to i64* (i64*, i64*)* 
  %reg243 =  call ccc  i64*  %fun33(i64*  %__e38, i64*  %reg245)  
  ret i64* %reg243 
}


@suman = internal   global i64* zeroinitializer


@doble = internal   global i64* zeroinitializer


@sumard = internal   global i64* zeroinitializer


@fib = internal   global i64* zeroinitializer


@resta = internal   global i64* zeroinitializer


@mult = internal   global i64* zeroinitializer


@exp = internal   global i64* zeroinitializer


@fact = internal   global i64* zeroinitializer


@gcd = internal   global i64* zeroinitializer


@gcd2 = internal   global i64* zeroinitializer


@x = internal   global i64* zeroinitializer


@y = internal   global i64* zeroinitializer


@z = internal   global i64* zeroinitializer


@multC2 = internal   global i64* zeroinitializer


@multC = internal   global i64* zeroinitializer


@expC2 = internal   global i64* zeroinitializer


@expC = internal   global i64* zeroinitializer


@res1 = internal   global i64* zeroinitializer


@res2 = internal   global i64* zeroinitializer


@res3 = internal   global i64* zeroinitializer


@res4 = internal   global i64* zeroinitializer


@res5 = internal   global i64* zeroinitializer


@res6 = internal   global i64* zeroinitializer


@res7 = internal   global i64* zeroinitializer


@res8 = internal   global i64* zeroinitializer


@x4 = internal   global i64* zeroinitializer


@res = internal   global i64* zeroinitializer


@res55 = internal   global i64* zeroinitializer


define external ccc  i64* @pcfmain()    {
pcfmain:
  %reg246 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__161, i64  0)  
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %reg246 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__r_5 = inttoptr i64 %__r_4 to i64* 
  store  i64* %__r_5, i64** @suman 
  %reg247 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__158, i64  0)  
  %__r_6 = inttoptr i64 0 to i64* 
  %__r_7 = ptrtoint i64* %reg247 to i64 
  %__r_8 = ptrtoint i64* %__r_6 to i64 
  %__r_9 = add   i64 %__r_7, %__r_8 
  %__r_10 = inttoptr i64 %__r_9 to i64* 
  store  i64* %__r_10, i64** @doble 
  %reg248 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__148, i64  0)  
  %__r_11 = inttoptr i64 0 to i64* 
  %__r_12 = ptrtoint i64* %reg248 to i64 
  %__r_13 = ptrtoint i64* %__r_11 to i64 
  %__r_14 = add   i64 %__r_12, %__r_13 
  %__r_15 = inttoptr i64 %__r_14 to i64* 
  store  i64* %__r_15, i64** @sumard 
  %reg249 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__142, i64  0)  
  %__r_16 = inttoptr i64 0 to i64* 
  %__r_17 = ptrtoint i64* %reg249 to i64 
  %__r_18 = ptrtoint i64* %__r_16 to i64 
  %__r_19 = add   i64 %__r_17, %__r_18 
  %__r_20 = inttoptr i64 %__r_19 to i64* 
  store  i64* %__r_20, i64** @fib 
  %reg250 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__133, i64  0)  
  %__r_21 = inttoptr i64 0 to i64* 
  %__r_22 = ptrtoint i64* %reg250 to i64 
  %__r_23 = ptrtoint i64* %__r_21 to i64 
  %__r_24 = add   i64 %__r_22, %__r_23 
  %__r_25 = inttoptr i64 %__r_24 to i64* 
  store  i64* %__r_25, i64** @resta 
  %reg251 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__124, i64  0)  
  %__r_26 = inttoptr i64 0 to i64* 
  %__r_27 = ptrtoint i64* %reg251 to i64 
  %__r_28 = ptrtoint i64* %__r_26 to i64 
  %__r_29 = add   i64 %__r_27, %__r_28 
  %__r_30 = inttoptr i64 %__r_29 to i64* 
  store  i64* %__r_30, i64** @mult 
  %reg252 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__113, i64  0)  
  %__r_31 = inttoptr i64 0 to i64* 
  %__r_32 = ptrtoint i64* %reg252 to i64 
  %__r_33 = ptrtoint i64* %__r_31 to i64 
  %__r_34 = add   i64 %__r_32, %__r_33 
  %__r_35 = inttoptr i64 %__r_34 to i64* 
  store  i64* %__r_35, i64** @exp 
  %reg253 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__106, i64  0)  
  %__r_36 = inttoptr i64 0 to i64* 
  %__r_37 = ptrtoint i64* %reg253 to i64 
  %__r_38 = ptrtoint i64* %__r_36 to i64 
  %__r_39 = add   i64 %__r_37, %__r_38 
  %__r_40 = inttoptr i64 %__r_39 to i64* 
  store  i64* %__r_40, i64** @fact 
  %reg254 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__95, i64  0)  
  %__r_41 = inttoptr i64 0 to i64* 
  %__r_42 = ptrtoint i64* %reg254 to i64 
  %__r_43 = ptrtoint i64* %__r_41 to i64 
  %__r_44 = add   i64 %__r_42, %__r_43 
  %__r_45 = inttoptr i64 %__r_44 to i64* 
  store  i64* %__r_45, i64** @gcd 
  %reg255 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__78, i64  0)  
  %__r_46 = inttoptr i64 0 to i64* 
  %__r_47 = ptrtoint i64* %reg255 to i64 
  %__r_48 = ptrtoint i64* %__r_46 to i64 
  %__r_49 = add   i64 %__r_47, %__r_48 
  %__r_50 = inttoptr i64 %__r_49 to i64* 
  store  i64* %__r_50, i64** @gcd2 
  %__r_51 = inttoptr i64 3 to i64* 
  %__r_52 = inttoptr i64 0 to i64* 
  %__r_53 = ptrtoint i64* %__r_51 to i64 
  %__r_54 = ptrtoint i64* %__r_52 to i64 
  %__r_55 = add   i64 %__r_53, %__r_54 
  %reg256 = inttoptr i64 %__r_55 to i64* 
  %__r_56 = inttoptr i64 0 to i64* 
  %__r_57 = ptrtoint i64* %reg256 to i64 
  %__r_58 = ptrtoint i64* %__r_56 to i64 
  %__r_59 = add   i64 %__r_57, %__r_58 
  %__r_60 = inttoptr i64 %__r_59 to i64* 
  store  i64* %__r_60, i64** @x 
  %__r_61 = inttoptr i64 4 to i64* 
  %__r_62 = inttoptr i64 0 to i64* 
  %__r_63 = ptrtoint i64* %__r_61 to i64 
  %__r_64 = ptrtoint i64* %__r_62 to i64 
  %__r_65 = add   i64 %__r_63, %__r_64 
  %reg258 = inttoptr i64 %__r_65 to i64* 
  %__r_66 = load  i64*, i64** @x 
  %__r_67 = ptrtoint i64* %__r_66 to i64 
  %__r_68 = ptrtoint i64* %reg258 to i64 
  %__r_69 = add   i64 %__r_67, %__r_68 
  %reg257 = inttoptr i64 %__r_69 to i64* 
  %__r_70 = inttoptr i64 0 to i64* 
  %__r_71 = ptrtoint i64* %reg257 to i64 
  %__r_72 = ptrtoint i64* %__r_70 to i64 
  %__r_73 = add   i64 %__r_71, %__r_72 
  %__r_74 = inttoptr i64 %__r_73 to i64* 
  store  i64* %__r_74, i64** @y 
  %__r_75 = load  i64*, i64** @doble 
  %__r_76 = inttoptr i64 0 to i64* 
  %__r_77 = ptrtoint i64* %__r_75 to i64 
  %__r_78 = ptrtoint i64* %__r_76 to i64 
  %__r_79 = add   i64 %__r_77, %__r_78 
  %__e76 = inttoptr i64 %__r_79 to i64* 
  %__r_81 = bitcast i64* %__e76 to i64** 
  %addr80 = getelementptr  i64*, i64** %__r_81, i64 0 
  %reg260 = load  i64*, i64** %addr80 
  %fun82 = bitcast i64* %reg260 to i64* (i64*, i64*)* 
  %__r_83 = load  i64*, i64** @y 
  %reg259 =  call ccc  i64*  %fun82(i64*  %__e76, i64*  %__r_83)  
  %__r_84 = inttoptr i64 0 to i64* 
  %__r_85 = ptrtoint i64* %reg259 to i64 
  %__r_86 = ptrtoint i64* %__r_84 to i64 
  %__r_87 = add   i64 %__r_85, %__r_86 
  %__r_88 = inttoptr i64 %__r_87 to i64* 
  store  i64* %__r_88, i64** @z 
  %reg261 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__64, i64  0)  
  %__r_89 = inttoptr i64 0 to i64* 
  %__r_90 = ptrtoint i64* %reg261 to i64 
  %__r_91 = ptrtoint i64* %__r_89 to i64 
  %__r_92 = add   i64 %__r_90, %__r_91 
  %__r_93 = inttoptr i64 %__r_92 to i64* 
  store  i64* %__r_93, i64** @multC2 
  %reg262 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__55, i64  0)  
  %__r_94 = inttoptr i64 0 to i64* 
  %__r_95 = ptrtoint i64* %reg262 to i64 
  %__r_96 = ptrtoint i64* %__r_94 to i64 
  %__r_97 = add   i64 %__r_95, %__r_96 
  %__r_98 = inttoptr i64 %__r_97 to i64* 
  store  i64* %__r_98, i64** @multC 
  %reg263 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__40, i64  0)  
  %__r_99 = inttoptr i64 0 to i64* 
  %__r_100 = ptrtoint i64* %reg263 to i64 
  %__r_101 = ptrtoint i64* %__r_99 to i64 
  %__r_102 = add   i64 %__r_100, %__r_101 
  %__r_103 = inttoptr i64 %__r_102 to i64* 
  store  i64* %__r_103, i64** @expC2 
  %reg264 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__31, i64  0)  
  %__r_104 = inttoptr i64 0 to i64* 
  %__r_105 = ptrtoint i64* %reg264 to i64 
  %__r_106 = ptrtoint i64* %__r_104 to i64 
  %__r_107 = add   i64 %__r_105, %__r_106 
  %__r_108 = inttoptr i64 %__r_107 to i64* 
  store  i64* %__r_108, i64** @expC 
  %__r_109 = load  i64*, i64** @suman 
  %__r_110 = inttoptr i64 0 to i64* 
  %__r_111 = ptrtoint i64* %__r_109 to i64 
  %__r_112 = ptrtoint i64* %__r_110 to i64 
  %__r_113 = add   i64 %__r_111, %__r_112 
  %__e29 = inttoptr i64 %__r_113 to i64* 
  %__r_115 = bitcast i64* %__e29 to i64** 
  %addr114 = getelementptr  i64*, i64** %__r_115, i64 0 
  %reg266 = load  i64*, i64** %addr114 
  %__r_116 = inttoptr i64 10 to i64* 
  %__r_117 = inttoptr i64 0 to i64* 
  %__r_118 = ptrtoint i64* %__r_116 to i64 
  %__r_119 = ptrtoint i64* %__r_117 to i64 
  %__r_120 = add   i64 %__r_118, %__r_119 
  %reg267 = inttoptr i64 %__r_120 to i64* 
  %fun121 = bitcast i64* %reg266 to i64* (i64*, i64*)* 
  %reg265 =  call ccc  i64*  %fun121(i64*  %__e29, i64*  %reg267)  
  %__r_122 = inttoptr i64 0 to i64* 
  %__r_123 = ptrtoint i64* %reg265 to i64 
  %__r_124 = ptrtoint i64* %__r_122 to i64 
  %__r_125 = add   i64 %__r_123, %__r_124 
  %__r_126 = inttoptr i64 %__r_125 to i64* 
  store  i64* %__r_126, i64** @res1 
  %__r_127 = load  i64*, i64** @doble 
  %__r_128 = inttoptr i64 0 to i64* 
  %__r_129 = ptrtoint i64* %__r_127 to i64 
  %__r_130 = ptrtoint i64* %__r_128 to i64 
  %__r_131 = add   i64 %__r_129, %__r_130 
  %__e28 = inttoptr i64 %__r_131 to i64* 
  %__r_133 = bitcast i64* %__e28 to i64** 
  %addr132 = getelementptr  i64*, i64** %__r_133, i64 0 
  %reg269 = load  i64*, i64** %addr132 
  %__r_134 = inttoptr i64 10 to i64* 
  %__r_135 = inttoptr i64 0 to i64* 
  %__r_136 = ptrtoint i64* %__r_134 to i64 
  %__r_137 = ptrtoint i64* %__r_135 to i64 
  %__r_138 = add   i64 %__r_136, %__r_137 
  %reg270 = inttoptr i64 %__r_138 to i64* 
  %fun139 = bitcast i64* %reg269 to i64* (i64*, i64*)* 
  %reg268 =  call ccc  i64*  %fun139(i64*  %__e28, i64*  %reg270)  
  %__r_140 = inttoptr i64 0 to i64* 
  %__r_141 = ptrtoint i64* %reg268 to i64 
  %__r_142 = ptrtoint i64* %__r_140 to i64 
  %__r_143 = add   i64 %__r_141, %__r_142 
  %__r_144 = inttoptr i64 %__r_143 to i64* 
  store  i64* %__r_144, i64** @res2 
  %__r_145 = load  i64*, i64** @sumard 
  %__r_146 = inttoptr i64 0 to i64* 
  %__r_147 = ptrtoint i64* %__r_145 to i64 
  %__r_148 = ptrtoint i64* %__r_146 to i64 
  %__r_149 = add   i64 %__r_147, %__r_148 
  %__e25 = inttoptr i64 %__r_149 to i64* 
  %__r_151 = bitcast i64* %__e25 to i64** 
  %addr150 = getelementptr  i64*, i64** %__r_151, i64 0 
  %reg272 = load  i64*, i64** %addr150 
  %__r_152 = inttoptr i64 5 to i64* 
  %__r_153 = inttoptr i64 0 to i64* 
  %__r_154 = ptrtoint i64* %__r_152 to i64 
  %__r_155 = ptrtoint i64* %__r_153 to i64 
  %__r_156 = add   i64 %__r_154, %__r_155 
  %reg273 = inttoptr i64 %__r_156 to i64* 
  %fun157 = bitcast i64* %reg272 to i64* (i64*, i64*)* 
  %reg271 =  call ccc  i64*  %fun157(i64*  %__e25, i64*  %reg273)  
  %__r_158 = inttoptr i64 0 to i64* 
  %__r_159 = ptrtoint i64* %reg271 to i64 
  %__r_160 = ptrtoint i64* %__r_158 to i64 
  %__r_161 = add   i64 %__r_159, %__r_160 
  %__e26 = inttoptr i64 %__r_161 to i64* 
  %__r_163 = bitcast i64* %__e26 to i64** 
  %addr162 = getelementptr  i64*, i64** %__r_163, i64 0 
  %reg275 = load  i64*, i64** %addr162 
  %__r_164 = inttoptr i64 7 to i64* 
  %__r_165 = inttoptr i64 0 to i64* 
  %__r_166 = ptrtoint i64* %__r_164 to i64 
  %__r_167 = ptrtoint i64* %__r_165 to i64 
  %__r_168 = add   i64 %__r_166, %__r_167 
  %reg276 = inttoptr i64 %__r_168 to i64* 
  %fun169 = bitcast i64* %reg275 to i64* (i64*, i64*)* 
  %reg274 =  call ccc  i64*  %fun169(i64*  %__e26, i64*  %reg276)  
  %__r_170 = inttoptr i64 0 to i64* 
  %__r_171 = ptrtoint i64* %reg274 to i64 
  %__r_172 = ptrtoint i64* %__r_170 to i64 
  %__r_173 = add   i64 %__r_171, %__r_172 
  %__e27 = inttoptr i64 %__r_173 to i64* 
  %__r_175 = bitcast i64* %__e27 to i64** 
  %addr174 = getelementptr  i64*, i64** %__r_175, i64 0 
  %reg278 = load  i64*, i64** %addr174 
  %fun176 = bitcast i64* %reg278 to i64* (i64*, i64*)* 
  %__r_177 = load  i64*, i64** @doble 
  %reg277 =  call ccc  i64*  %fun176(i64*  %__e27, i64*  %__r_177)  
  %__r_178 = inttoptr i64 0 to i64* 
  %__r_179 = ptrtoint i64* %reg277 to i64 
  %__r_180 = ptrtoint i64* %__r_178 to i64 
  %__r_181 = add   i64 %__r_179, %__r_180 
  %__r_182 = inttoptr i64 %__r_181 to i64* 
  store  i64* %__r_182, i64** @res3 
  %__r_183 = load  i64*, i64** @fib 
  %__r_184 = inttoptr i64 0 to i64* 
  %__r_185 = ptrtoint i64* %__r_183 to i64 
  %__r_186 = ptrtoint i64* %__r_184 to i64 
  %__r_187 = add   i64 %__r_185, %__r_186 
  %__e24 = inttoptr i64 %__r_187 to i64* 
  %__r_189 = bitcast i64* %__e24 to i64** 
  %addr188 = getelementptr  i64*, i64** %__r_189, i64 0 
  %reg280 = load  i64*, i64** %addr188 
  %__r_190 = inttoptr i64 20 to i64* 
  %__r_191 = inttoptr i64 0 to i64* 
  %__r_192 = ptrtoint i64* %__r_190 to i64 
  %__r_193 = ptrtoint i64* %__r_191 to i64 
  %__r_194 = add   i64 %__r_192, %__r_193 
  %reg281 = inttoptr i64 %__r_194 to i64* 
  %fun195 = bitcast i64* %reg280 to i64* (i64*, i64*)* 
  %reg279 =  call ccc  i64*  %fun195(i64*  %__e24, i64*  %reg281)  
  %__r_196 = inttoptr i64 0 to i64* 
  %__r_197 = ptrtoint i64* %reg279 to i64 
  %__r_198 = ptrtoint i64* %__r_196 to i64 
  %__r_199 = add   i64 %__r_197, %__r_198 
  %__r_200 = inttoptr i64 %__r_199 to i64* 
  store  i64* %__r_200, i64** @res4 
  %__r_201 = load  i64*, i64** @resta 
  %__r_202 = inttoptr i64 0 to i64* 
  %__r_203 = ptrtoint i64* %__r_201 to i64 
  %__r_204 = ptrtoint i64* %__r_202 to i64 
  %__r_205 = add   i64 %__r_203, %__r_204 
  %__e22 = inttoptr i64 %__r_205 to i64* 
  %__r_207 = bitcast i64* %__e22 to i64** 
  %addr206 = getelementptr  i64*, i64** %__r_207, i64 0 
  %reg283 = load  i64*, i64** %addr206 
  %__r_208 = inttoptr i64 34 to i64* 
  %__r_209 = inttoptr i64 0 to i64* 
  %__r_210 = ptrtoint i64* %__r_208 to i64 
  %__r_211 = ptrtoint i64* %__r_209 to i64 
  %__r_212 = add   i64 %__r_210, %__r_211 
  %reg284 = inttoptr i64 %__r_212 to i64* 
  %fun213 = bitcast i64* %reg283 to i64* (i64*, i64*)* 
  %reg282 =  call ccc  i64*  %fun213(i64*  %__e22, i64*  %reg284)  
  %__r_214 = inttoptr i64 0 to i64* 
  %__r_215 = ptrtoint i64* %reg282 to i64 
  %__r_216 = ptrtoint i64* %__r_214 to i64 
  %__r_217 = add   i64 %__r_215, %__r_216 
  %__e23 = inttoptr i64 %__r_217 to i64* 
  %__r_219 = bitcast i64* %__e23 to i64** 
  %addr218 = getelementptr  i64*, i64** %__r_219, i64 0 
  %reg286 = load  i64*, i64** %addr218 
  %__r_220 = inttoptr i64 12 to i64* 
  %__r_221 = inttoptr i64 0 to i64* 
  %__r_222 = ptrtoint i64* %__r_220 to i64 
  %__r_223 = ptrtoint i64* %__r_221 to i64 
  %__r_224 = add   i64 %__r_222, %__r_223 
  %reg287 = inttoptr i64 %__r_224 to i64* 
  %fun225 = bitcast i64* %reg286 to i64* (i64*, i64*)* 
  %reg285 =  call ccc  i64*  %fun225(i64*  %__e23, i64*  %reg287)  
  %__r_226 = inttoptr i64 0 to i64* 
  %__r_227 = ptrtoint i64* %reg285 to i64 
  %__r_228 = ptrtoint i64* %__r_226 to i64 
  %__r_229 = add   i64 %__r_227, %__r_228 
  %__r_230 = inttoptr i64 %__r_229 to i64* 
  store  i64* %__r_230, i64** @res5 
  %__r_231 = load  i64*, i64** @mult 
  %__r_232 = inttoptr i64 0 to i64* 
  %__r_233 = ptrtoint i64* %__r_231 to i64 
  %__r_234 = ptrtoint i64* %__r_232 to i64 
  %__r_235 = add   i64 %__r_233, %__r_234 
  %__e20 = inttoptr i64 %__r_235 to i64* 
  %__r_237 = bitcast i64* %__e20 to i64** 
  %addr236 = getelementptr  i64*, i64** %__r_237, i64 0 
  %reg289 = load  i64*, i64** %addr236 
  %__r_238 = inttoptr i64 2 to i64* 
  %__r_239 = inttoptr i64 0 to i64* 
  %__r_240 = ptrtoint i64* %__r_238 to i64 
  %__r_241 = ptrtoint i64* %__r_239 to i64 
  %__r_242 = add   i64 %__r_240, %__r_241 
  %reg290 = inttoptr i64 %__r_242 to i64* 
  %fun243 = bitcast i64* %reg289 to i64* (i64*, i64*)* 
  %reg288 =  call ccc  i64*  %fun243(i64*  %__e20, i64*  %reg290)  
  %__r_244 = inttoptr i64 0 to i64* 
  %__r_245 = ptrtoint i64* %reg288 to i64 
  %__r_246 = ptrtoint i64* %__r_244 to i64 
  %__r_247 = add   i64 %__r_245, %__r_246 
  %__e21 = inttoptr i64 %__r_247 to i64* 
  %__r_249 = bitcast i64* %__e21 to i64** 
  %addr248 = getelementptr  i64*, i64** %__r_249, i64 0 
  %reg292 = load  i64*, i64** %addr248 
  %__r_250 = inttoptr i64 5 to i64* 
  %__r_251 = inttoptr i64 0 to i64* 
  %__r_252 = ptrtoint i64* %__r_250 to i64 
  %__r_253 = ptrtoint i64* %__r_251 to i64 
  %__r_254 = add   i64 %__r_252, %__r_253 
  %reg293 = inttoptr i64 %__r_254 to i64* 
  %fun255 = bitcast i64* %reg292 to i64* (i64*, i64*)* 
  %reg291 =  call ccc  i64*  %fun255(i64*  %__e21, i64*  %reg293)  
  %__r_256 = inttoptr i64 0 to i64* 
  %__r_257 = ptrtoint i64* %reg291 to i64 
  %__r_258 = ptrtoint i64* %__r_256 to i64 
  %__r_259 = add   i64 %__r_257, %__r_258 
  %__r_260 = inttoptr i64 %__r_259 to i64* 
  store  i64* %__r_260, i64** @res6 
  %__r_261 = load  i64*, i64** @exp 
  %__r_262 = inttoptr i64 0 to i64* 
  %__r_263 = ptrtoint i64* %__r_261 to i64 
  %__r_264 = ptrtoint i64* %__r_262 to i64 
  %__r_265 = add   i64 %__r_263, %__r_264 
  %__e18 = inttoptr i64 %__r_265 to i64* 
  %__r_267 = bitcast i64* %__e18 to i64** 
  %addr266 = getelementptr  i64*, i64** %__r_267, i64 0 
  %reg295 = load  i64*, i64** %addr266 
  %__r_268 = inttoptr i64 2 to i64* 
  %__r_269 = inttoptr i64 0 to i64* 
  %__r_270 = ptrtoint i64* %__r_268 to i64 
  %__r_271 = ptrtoint i64* %__r_269 to i64 
  %__r_272 = add   i64 %__r_270, %__r_271 
  %reg296 = inttoptr i64 %__r_272 to i64* 
  %fun273 = bitcast i64* %reg295 to i64* (i64*, i64*)* 
  %reg294 =  call ccc  i64*  %fun273(i64*  %__e18, i64*  %reg296)  
  %__r_274 = inttoptr i64 0 to i64* 
  %__r_275 = ptrtoint i64* %reg294 to i64 
  %__r_276 = ptrtoint i64* %__r_274 to i64 
  %__r_277 = add   i64 %__r_275, %__r_276 
  %__e19 = inttoptr i64 %__r_277 to i64* 
  %__r_279 = bitcast i64* %__e19 to i64** 
  %addr278 = getelementptr  i64*, i64** %__r_279, i64 0 
  %reg298 = load  i64*, i64** %addr278 
  %__r_280 = inttoptr i64 3 to i64* 
  %__r_281 = inttoptr i64 0 to i64* 
  %__r_282 = ptrtoint i64* %__r_280 to i64 
  %__r_283 = ptrtoint i64* %__r_281 to i64 
  %__r_284 = add   i64 %__r_282, %__r_283 
  %reg299 = inttoptr i64 %__r_284 to i64* 
  %fun285 = bitcast i64* %reg298 to i64* (i64*, i64*)* 
  %reg297 =  call ccc  i64*  %fun285(i64*  %__e19, i64*  %reg299)  
  %__r_286 = inttoptr i64 0 to i64* 
  %__r_287 = ptrtoint i64* %reg297 to i64 
  %__r_288 = ptrtoint i64* %__r_286 to i64 
  %__r_289 = add   i64 %__r_287, %__r_288 
  %__r_290 = inttoptr i64 %__r_289 to i64* 
  store  i64* %__r_290, i64** @res7 
  %__r_291 = load  i64*, i64** @fact 
  %__r_292 = inttoptr i64 0 to i64* 
  %__r_293 = ptrtoint i64* %__r_291 to i64 
  %__r_294 = ptrtoint i64* %__r_292 to i64 
  %__r_295 = add   i64 %__r_293, %__r_294 
  %__e17 = inttoptr i64 %__r_295 to i64* 
  %__r_297 = bitcast i64* %__e17 to i64** 
  %addr296 = getelementptr  i64*, i64** %__r_297, i64 0 
  %reg301 = load  i64*, i64** %addr296 
  %__r_298 = inttoptr i64 5 to i64* 
  %__r_299 = inttoptr i64 0 to i64* 
  %__r_300 = ptrtoint i64* %__r_298 to i64 
  %__r_301 = ptrtoint i64* %__r_299 to i64 
  %__r_302 = add   i64 %__r_300, %__r_301 
  %reg302 = inttoptr i64 %__r_302 to i64* 
  %fun303 = bitcast i64* %reg301 to i64* (i64*, i64*)* 
  %reg300 =  call ccc  i64*  %fun303(i64*  %__e17, i64*  %reg302)  
  %__r_304 = inttoptr i64 0 to i64* 
  %__r_305 = ptrtoint i64* %reg300 to i64 
  %__r_306 = ptrtoint i64* %__r_304 to i64 
  %__r_307 = add   i64 %__r_305, %__r_306 
  %__r_308 = inttoptr i64 %__r_307 to i64* 
  store  i64* %__r_308, i64** @res8 
  %__r_309 = inttoptr i64 343 to i64* 
  %__r_310 = inttoptr i64 0 to i64* 
  %__r_311 = ptrtoint i64* %__r_309 to i64 
  %__r_312 = ptrtoint i64* %__r_310 to i64 
  %__r_313 = add   i64 %__r_311, %__r_312 
  %reg303 = inttoptr i64 %__r_313 to i64* 
  %__r_314 = inttoptr i64 0 to i64* 
  %__r_315 = ptrtoint i64* %reg303 to i64 
  %__r_316 = ptrtoint i64* %__r_314 to i64 
  %__r_317 = add   i64 %__r_315, %__r_316 
  %__x216 = inttoptr i64 %__r_317 to i64* 
  %__r_318 = load  i64*, i64** @res2 
  %__r_319 = ptrtoint i64* %__x216 to i64 
  %__r_320 = ptrtoint i64* %__r_318 to i64 
  %__r_321 = add   i64 %__r_319, %__r_320 
  %reg305 = inttoptr i64 %__r_321 to i64* 
  %__r_322 = load  i64*, i64** @res1 
  %__r_323 = ptrtoint i64* %reg305 to i64 
  %__r_324 = ptrtoint i64* %__r_322 to i64 
  %__r_325 = add   i64 %__r_323, %__r_324 
  %reg304 = inttoptr i64 %__r_325 to i64* 
  %__r_326 = inttoptr i64 0 to i64* 
  %__r_327 = ptrtoint i64* %reg304 to i64 
  %__r_328 = ptrtoint i64* %__r_326 to i64 
  %__r_329 = add   i64 %__r_327, %__r_328 
  %__r_330 = inttoptr i64 %__r_329 to i64* 
  store  i64* %__r_330, i64** @x4 
  %__r_331 = load  i64*, i64** @doble 
  %__r_332 = inttoptr i64 0 to i64* 
  %__r_333 = ptrtoint i64* %__r_331 to i64 
  %__r_334 = ptrtoint i64* %__r_332 to i64 
  %__r_335 = add   i64 %__r_333, %__r_334 
  %__e12 = inttoptr i64 %__r_335 to i64* 
  %__r_337 = bitcast i64* %__e12 to i64** 
  %addr336 = getelementptr  i64*, i64** %__r_337, i64 0 
  %reg308 = load  i64*, i64** %addr336 
  %__r_338 = load  i64*, i64** @mult 
  %__r_339 = inttoptr i64 0 to i64* 
  %__r_340 = ptrtoint i64* %__r_338 to i64 
  %__r_341 = ptrtoint i64* %__r_339 to i64 
  %__r_342 = add   i64 %__r_340, %__r_341 
  %__e10 = inttoptr i64 %__r_342 to i64* 
  %__r_344 = bitcast i64* %__e10 to i64** 
  %addr343 = getelementptr  i64*, i64** %__r_344, i64 0 
  %reg310 = load  i64*, i64** %addr343 
  %__r_345 = inttoptr i64 3 to i64* 
  %__r_346 = inttoptr i64 0 to i64* 
  %__r_347 = ptrtoint i64* %__r_345 to i64 
  %__r_348 = ptrtoint i64* %__r_346 to i64 
  %__r_349 = add   i64 %__r_347, %__r_348 
  %reg311 = inttoptr i64 %__r_349 to i64* 
  %fun350 = bitcast i64* %reg310 to i64* (i64*, i64*)* 
  %reg309 =  call ccc  i64*  %fun350(i64*  %__e10, i64*  %reg311)  
  %__r_351 = inttoptr i64 0 to i64* 
  %__r_352 = ptrtoint i64* %reg309 to i64 
  %__r_353 = ptrtoint i64* %__r_351 to i64 
  %__r_354 = add   i64 %__r_352, %__r_353 
  %__e11 = inttoptr i64 %__r_354 to i64* 
  %__r_356 = bitcast i64* %__e11 to i64** 
  %addr355 = getelementptr  i64*, i64** %__r_356, i64 0 
  %reg313 = load  i64*, i64** %addr355 
  %__r_357 = inttoptr i64 2 to i64* 
  %__r_358 = inttoptr i64 0 to i64* 
  %__r_359 = ptrtoint i64* %__r_357 to i64 
  %__r_360 = ptrtoint i64* %__r_358 to i64 
  %__r_361 = add   i64 %__r_359, %__r_360 
  %reg314 = inttoptr i64 %__r_361 to i64* 
  %fun362 = bitcast i64* %reg313 to i64* (i64*, i64*)* 
  %reg312 =  call ccc  i64*  %fun362(i64*  %__e11, i64*  %reg314)  
  %fun363 = bitcast i64* %reg308 to i64* (i64*, i64*)* 
  %reg307 =  call ccc  i64*  %fun363(i64*  %__e12, i64*  %reg312)  
  %__r_364 = load  i64*, i64** @doble 
  %__r_365 = inttoptr i64 0 to i64* 
  %__r_366 = ptrtoint i64* %__r_364 to i64 
  %__r_367 = ptrtoint i64* %__r_365 to i64 
  %__r_368 = add   i64 %__r_366, %__r_367 
  %__e15 = inttoptr i64 %__r_368 to i64* 
  %__r_370 = bitcast i64* %__e15 to i64** 
  %addr369 = getelementptr  i64*, i64** %__r_370, i64 0 
  %reg316 = load  i64*, i64** %addr369 
  %__r_371 = load  i64*, i64** @mult 
  %__r_372 = inttoptr i64 0 to i64* 
  %__r_373 = ptrtoint i64* %__r_371 to i64 
  %__r_374 = ptrtoint i64* %__r_372 to i64 
  %__r_375 = add   i64 %__r_373, %__r_374 
  %__e13 = inttoptr i64 %__r_375 to i64* 
  %__r_377 = bitcast i64* %__e13 to i64** 
  %addr376 = getelementptr  i64*, i64** %__r_377, i64 0 
  %reg318 = load  i64*, i64** %addr376 
  %__r_378 = inttoptr i64 3 to i64* 
  %__r_379 = inttoptr i64 0 to i64* 
  %__r_380 = ptrtoint i64* %__r_378 to i64 
  %__r_381 = ptrtoint i64* %__r_379 to i64 
  %__r_382 = add   i64 %__r_380, %__r_381 
  %reg319 = inttoptr i64 %__r_382 to i64* 
  %fun383 = bitcast i64* %reg318 to i64* (i64*, i64*)* 
  %reg317 =  call ccc  i64*  %fun383(i64*  %__e13, i64*  %reg319)  
  %__r_384 = inttoptr i64 0 to i64* 
  %__r_385 = ptrtoint i64* %reg317 to i64 
  %__r_386 = ptrtoint i64* %__r_384 to i64 
  %__r_387 = add   i64 %__r_385, %__r_386 
  %__e14 = inttoptr i64 %__r_387 to i64* 
  %__r_389 = bitcast i64* %__e14 to i64** 
  %addr388 = getelementptr  i64*, i64** %__r_389, i64 0 
  %reg321 = load  i64*, i64** %addr388 
  %__r_390 = inttoptr i64 2 to i64* 
  %__r_391 = inttoptr i64 0 to i64* 
  %__r_392 = ptrtoint i64* %__r_390 to i64 
  %__r_393 = ptrtoint i64* %__r_391 to i64 
  %__r_394 = add   i64 %__r_392, %__r_393 
  %reg322 = inttoptr i64 %__r_394 to i64* 
  %fun395 = bitcast i64* %reg321 to i64* (i64*, i64*)* 
  %reg320 =  call ccc  i64*  %fun395(i64*  %__e14, i64*  %reg322)  
  %fun396 = bitcast i64* %reg316 to i64* (i64*, i64*)* 
  %reg315 =  call ccc  i64*  %fun396(i64*  %__e15, i64*  %reg320)  
  %__r_397 = ptrtoint i64* %reg307 to i64 
  %__r_398 = ptrtoint i64* %reg315 to i64 
  %__r_399 = add   i64 %__r_397, %__r_398 
  %reg306 = inttoptr i64 %__r_399 to i64* 
  %__r_400 = inttoptr i64 0 to i64* 
  %__r_401 = ptrtoint i64* %reg306 to i64 
  %__r_402 = ptrtoint i64* %__r_400 to i64 
  %__r_403 = add   i64 %__r_401, %__r_402 
  %__r_404 = inttoptr i64 %__r_403 to i64* 
  store  i64* %__r_404, i64** @res 
  %__r_405 = load  i64*, i64** @doble 
  %__r_406 = inttoptr i64 0 to i64* 
  %__r_407 = ptrtoint i64* %__r_405 to i64 
  %__r_408 = ptrtoint i64* %__r_406 to i64 
  %__r_409 = add   i64 %__r_407, %__r_408 
  %__e2 = inttoptr i64 %__r_409 to i64* 
  %__r_411 = bitcast i64* %__e2 to i64** 
  %addr410 = getelementptr  i64*, i64** %__r_411, i64 0 
  %reg326 = load  i64*, i64** %addr410 
  %__r_412 = load  i64*, i64** @mult 
  %__r_413 = inttoptr i64 0 to i64* 
  %__r_414 = ptrtoint i64* %__r_412 to i64 
  %__r_415 = ptrtoint i64* %__r_413 to i64 
  %__r_416 = add   i64 %__r_414, %__r_415 
  %__e0 = inttoptr i64 %__r_416 to i64* 
  %__r_418 = bitcast i64* %__e0 to i64** 
  %addr417 = getelementptr  i64*, i64** %__r_418, i64 0 
  %reg328 = load  i64*, i64** %addr417 
  %__r_419 = inttoptr i64 3 to i64* 
  %__r_420 = inttoptr i64 0 to i64* 
  %__r_421 = ptrtoint i64* %__r_419 to i64 
  %__r_422 = ptrtoint i64* %__r_420 to i64 
  %__r_423 = add   i64 %__r_421, %__r_422 
  %reg329 = inttoptr i64 %__r_423 to i64* 
  %fun424 = bitcast i64* %reg328 to i64* (i64*, i64*)* 
  %reg327 =  call ccc  i64*  %fun424(i64*  %__e0, i64*  %reg329)  
  %__r_425 = inttoptr i64 0 to i64* 
  %__r_426 = ptrtoint i64* %reg327 to i64 
  %__r_427 = ptrtoint i64* %__r_425 to i64 
  %__r_428 = add   i64 %__r_426, %__r_427 
  %__e1 = inttoptr i64 %__r_428 to i64* 
  %__r_430 = bitcast i64* %__e1 to i64** 
  %addr429 = getelementptr  i64*, i64** %__r_430, i64 0 
  %reg331 = load  i64*, i64** %addr429 
  %__r_431 = inttoptr i64 2 to i64* 
  %__r_432 = inttoptr i64 0 to i64* 
  %__r_433 = ptrtoint i64* %__r_431 to i64 
  %__r_434 = ptrtoint i64* %__r_432 to i64 
  %__r_435 = add   i64 %__r_433, %__r_434 
  %reg332 = inttoptr i64 %__r_435 to i64* 
  %fun436 = bitcast i64* %reg331 to i64* (i64*, i64*)* 
  %reg330 =  call ccc  i64*  %fun436(i64*  %__e1, i64*  %reg332)  
  %fun437 = bitcast i64* %reg326 to i64* (i64*, i64*)* 
  %reg325 =  call ccc  i64*  %fun437(i64*  %__e2, i64*  %reg330)  
  %__r_438 = load  i64*, i64** @doble 
  %__r_439 = inttoptr i64 0 to i64* 
  %__r_440 = ptrtoint i64* %__r_438 to i64 
  %__r_441 = ptrtoint i64* %__r_439 to i64 
  %__r_442 = add   i64 %__r_440, %__r_441 
  %__e5 = inttoptr i64 %__r_442 to i64* 
  %__r_444 = bitcast i64* %__e5 to i64** 
  %addr443 = getelementptr  i64*, i64** %__r_444, i64 0 
  %reg334 = load  i64*, i64** %addr443 
  %__r_445 = load  i64*, i64** @mult 
  %__r_446 = inttoptr i64 0 to i64* 
  %__r_447 = ptrtoint i64* %__r_445 to i64 
  %__r_448 = ptrtoint i64* %__r_446 to i64 
  %__r_449 = add   i64 %__r_447, %__r_448 
  %__e3 = inttoptr i64 %__r_449 to i64* 
  %__r_451 = bitcast i64* %__e3 to i64** 
  %addr450 = getelementptr  i64*, i64** %__r_451, i64 0 
  %reg336 = load  i64*, i64** %addr450 
  %__r_452 = inttoptr i64 3 to i64* 
  %__r_453 = inttoptr i64 0 to i64* 
  %__r_454 = ptrtoint i64* %__r_452 to i64 
  %__r_455 = ptrtoint i64* %__r_453 to i64 
  %__r_456 = add   i64 %__r_454, %__r_455 
  %reg337 = inttoptr i64 %__r_456 to i64* 
  %fun457 = bitcast i64* %reg336 to i64* (i64*, i64*)* 
  %reg335 =  call ccc  i64*  %fun457(i64*  %__e3, i64*  %reg337)  
  %__r_458 = inttoptr i64 0 to i64* 
  %__r_459 = ptrtoint i64* %reg335 to i64 
  %__r_460 = ptrtoint i64* %__r_458 to i64 
  %__r_461 = add   i64 %__r_459, %__r_460 
  %__e4 = inttoptr i64 %__r_461 to i64* 
  %__r_463 = bitcast i64* %__e4 to i64** 
  %addr462 = getelementptr  i64*, i64** %__r_463, i64 0 
  %reg339 = load  i64*, i64** %addr462 
  %__r_464 = inttoptr i64 2 to i64* 
  %__r_465 = inttoptr i64 0 to i64* 
  %__r_466 = ptrtoint i64* %__r_464 to i64 
  %__r_467 = ptrtoint i64* %__r_465 to i64 
  %__r_468 = add   i64 %__r_466, %__r_467 
  %reg340 = inttoptr i64 %__r_468 to i64* 
  %fun469 = bitcast i64* %reg339 to i64* (i64*, i64*)* 
  %reg338 =  call ccc  i64*  %fun469(i64*  %__e4, i64*  %reg340)  
  %fun470 = bitcast i64* %reg334 to i64* (i64*, i64*)* 
  %reg333 =  call ccc  i64*  %fun470(i64*  %__e5, i64*  %reg338)  
  %__r_471 = ptrtoint i64* %reg325 to i64 
  %__r_472 = ptrtoint i64* %reg333 to i64 
  %__r_473 = add   i64 %__r_471, %__r_472 
  %reg324 = inttoptr i64 %__r_473 to i64* 
  %__r_474 = load  i64*, i64** @exp 
  %__r_475 = inttoptr i64 0 to i64* 
  %__r_476 = ptrtoint i64* %__r_474 to i64 
  %__r_477 = ptrtoint i64* %__r_475 to i64 
  %__r_478 = add   i64 %__r_476, %__r_477 
  %__e6 = inttoptr i64 %__r_478 to i64* 
  %__r_480 = bitcast i64* %__e6 to i64** 
  %addr479 = getelementptr  i64*, i64** %__r_480, i64 0 
  %reg343 = load  i64*, i64** %addr479 
  %__r_481 = inttoptr i64 1 to i64* 
  %__r_482 = inttoptr i64 0 to i64* 
  %__r_483 = ptrtoint i64* %__r_481 to i64 
  %__r_484 = ptrtoint i64* %__r_482 to i64 
  %__r_485 = add   i64 %__r_483, %__r_484 
  %reg344 = inttoptr i64 %__r_485 to i64* 
  %fun486 = bitcast i64* %reg343 to i64* (i64*, i64*)* 
  %reg342 =  call ccc  i64*  %fun486(i64*  %__e6, i64*  %reg344)  
  %__r_487 = inttoptr i64 0 to i64* 
  %__r_488 = ptrtoint i64* %reg342 to i64 
  %__r_489 = ptrtoint i64* %__r_487 to i64 
  %__r_490 = add   i64 %__r_488, %__r_489 
  %__e7 = inttoptr i64 %__r_490 to i64* 
  %__r_492 = bitcast i64* %__e7 to i64** 
  %addr491 = getelementptr  i64*, i64** %__r_492, i64 0 
  %reg346 = load  i64*, i64** %addr491 
  %__r_493 = inttoptr i64 10000 to i64* 
  %__r_494 = inttoptr i64 0 to i64* 
  %__r_495 = ptrtoint i64* %__r_493 to i64 
  %__r_496 = ptrtoint i64* %__r_494 to i64 
  %__r_497 = add   i64 %__r_495, %__r_496 
  %reg347 = inttoptr i64 %__r_497 to i64* 
  %fun498 = bitcast i64* %reg346 to i64* (i64*, i64*)* 
  %reg345 =  call ccc  i64*  %fun498(i64*  %__e7, i64*  %reg347)  
  %__r_499 = load  i64*, i64** @gcd 
  %__r_500 = inttoptr i64 0 to i64* 
  %__r_501 = ptrtoint i64* %__r_499 to i64 
  %__r_502 = ptrtoint i64* %__r_500 to i64 
  %__r_503 = add   i64 %__r_501, %__r_502 
  %__e8 = inttoptr i64 %__r_503 to i64* 
  %__r_505 = bitcast i64* %__e8 to i64** 
  %addr504 = getelementptr  i64*, i64** %__r_505, i64 0 
  %reg349 = load  i64*, i64** %addr504 
  %__r_506 = inttoptr i64 9883 to i64* 
  %__r_507 = inttoptr i64 0 to i64* 
  %__r_508 = ptrtoint i64* %__r_506 to i64 
  %__r_509 = ptrtoint i64* %__r_507 to i64 
  %__r_510 = add   i64 %__r_508, %__r_509 
  %reg350 = inttoptr i64 %__r_510 to i64* 
  %fun511 = bitcast i64* %reg349 to i64* (i64*, i64*)* 
  %reg348 =  call ccc  i64*  %fun511(i64*  %__e8, i64*  %reg350)  
  %__r_512 = inttoptr i64 0 to i64* 
  %__r_513 = ptrtoint i64* %reg348 to i64 
  %__r_514 = ptrtoint i64* %__r_512 to i64 
  %__r_515 = add   i64 %__r_513, %__r_514 
  %__e9 = inttoptr i64 %__r_515 to i64* 
  %__r_517 = bitcast i64* %__e9 to i64** 
  %addr516 = getelementptr  i64*, i64** %__r_517, i64 0 
  %reg352 = load  i64*, i64** %addr516 
  %__r_518 = inttoptr i64 9887 to i64* 
  %__r_519 = inttoptr i64 0 to i64* 
  %__r_520 = ptrtoint i64* %__r_518 to i64 
  %__r_521 = ptrtoint i64* %__r_519 to i64 
  %__r_522 = add   i64 %__r_520, %__r_521 
  %reg353 = inttoptr i64 %__r_522 to i64* 
  %fun523 = bitcast i64* %reg352 to i64* (i64*, i64*)* 
  %reg351 =  call ccc  i64*  %fun523(i64*  %__e9, i64*  %reg353)  
  %__r_524 = ptrtoint i64* %reg345 to i64 
  %__r_525 = ptrtoint i64* %reg351 to i64 
  %__r_526 = add   i64 %__r_524, %__r_525 
  %reg341 = inttoptr i64 %__r_526 to i64* 
  %__r_527 = ptrtoint i64* %reg324 to i64 
  %__r_528 = ptrtoint i64* %reg341 to i64 
  %__r_529 = add   i64 %__r_527, %__r_528 
  %reg323 = inttoptr i64 %__r_529 to i64* 
  %__r_530 = ptrtoint i64* %reg323 to i64 
  %__r_531 =  call ccc  i64  @pcf_print(i64  %__r_530)  
  %reg354 = inttoptr i64 %__r_531 to i64* 
  %__r_532 = inttoptr i64 0 to i64* 
  %__r_533 = ptrtoint i64* %reg354 to i64 
  %__r_534 = ptrtoint i64* %__r_532 to i64 
  %__r_535 = add   i64 %__r_533, %__r_534 
  %__r_536 = inttoptr i64 %__r_535 to i64* 
  store  i64* %__r_536, i64** @res55 
  ret i64* %reg323 
}