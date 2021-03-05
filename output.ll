; ModuleID = 'pcfprog'


 


declare external ccc  i64* @pcf_mkclosure(i64* (i64*, i64*)*, i64, ...)    


declare external ccc  i64 @pcf_print(i64)    


define external ccc  i64* @__130(i64*  %__clo131, i64*  %__x129)    {
__130:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo131 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__suman132 = inttoptr i64 %__r_4 to i64* 
  br label %entry0 
entry0:
  %__r_6 = inttoptr i64 0 to i64* 
  %cond5 = icmp eq i64* %__x129, %__r_6 
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
  %__r_13 = ptrtoint i64* %__suman132 to i64 
  %__r_14 = ptrtoint i64* %__r_12 to i64 
  %__r_15 = add   i64 %__r_13, %__r_14 
  %__e133 = inttoptr i64 %__r_15 to i64* 
  %__r_17 = bitcast i64* %__e133 to i64** 
  %addr16 = getelementptr  i64*, i64** %__r_17, i64 0 
  %reg7 = load  i64*, i64** %addr16 
  %__r_18 = inttoptr i64 1 to i64* 
  %__r_19 = inttoptr i64 0 to i64* 
  %__r_20 = ptrtoint i64* %__r_18 to i64 
  %__r_21 = ptrtoint i64* %__r_19 to i64 
  %__r_22 = add   i64 %__r_20, %__r_21 
  %reg9 = inttoptr i64 %__r_22 to i64* 
  %__r_23 = ptrtoint i64* %__x129 to i64 
  %__r_24 = ptrtoint i64* %reg9 to i64 
  %__r_25 = sub   i64 %__r_23, %__r_24 
  %__r_26 = icmp slt i64 0, %__r_25 
  %__r_27 = zext i1 %__r_26 to i64  
  %__r_28 = mul   i64 %__r_25, %__r_27 
  %reg8 = inttoptr i64 %__r_28 to i64* 
  %fun29 = bitcast i64* %reg7 to i64* (i64*, i64*)* 
  %reg6 =  call ccc  i64*  %fun29(i64*  %__e133, i64*  %reg8)  
  %__r_30 = ptrtoint i64* %__x129 to i64 
  %__r_31 = ptrtoint i64* %reg6 to i64 
  %__r_32 = add   i64 %__r_30, %__r_31 
  %reg5 = inttoptr i64 %__r_32 to i64* 
  br label %ifcont3 
ifcont3:
  %regcont10 = phi i64* [%reg4, %then1], [%reg5, %else2] 
  ret i64* %regcont10 
}


define external ccc  i64* @__127(i64*  %__clo128, i64*  %__x126)    {
__127:
  %__r_1 = ptrtoint i64* %__x126 to i64 
  %__r_2 = ptrtoint i64* %__x126 to i64 
  %__r_3 = add   i64 %__r_1, %__r_2 
  %reg11 = inttoptr i64 %__r_3 to i64* 
  ret i64* %reg11 
}


define external ccc  i64* @__117(i64*  %__clo118, i64*  %__x116)    {
__117:
  %reg12 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__120, i64  1, i64*  %__x116)  
  ret i64* %reg12 
}


define external ccc  i64* @__120(i64*  %__clo121, i64*  %__y119)    {
__120:
  %__r_2 = bitcast i64* %__clo121 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg13 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg13 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__x116 = inttoptr i64 %__r_6 to i64* 
  %reg14 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__123, i64  2, i64*  %__x116, i64*  %__y119)  
  ret i64* %reg14 
}


define external ccc  i64* @__123(i64*  %__clo124, i64*  %__f122)    {
__123:
  %__r_2 = bitcast i64* %__clo124 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg15 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg15 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__x116 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo124 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg16 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg16 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__y119 = inttoptr i64 %__r_12 to i64* 
  %__r_13 = inttoptr i64 0 to i64* 
  %__r_14 = ptrtoint i64* %__f122 to i64 
  %__r_15 = ptrtoint i64* %__r_13 to i64 
  %__r_16 = add   i64 %__r_14, %__r_15 
  %__e125 = inttoptr i64 %__r_16 to i64* 
  %__r_18 = bitcast i64* %__e125 to i64** 
  %addr17 = getelementptr  i64*, i64** %__r_18, i64 0 
  %reg18 = load  i64*, i64** %addr17 
  %__r_19 = ptrtoint i64* %__x116 to i64 
  %__r_20 = ptrtoint i64* %__y119 to i64 
  %__r_21 = add   i64 %__r_19, %__r_20 
  %reg19 = inttoptr i64 %__r_21 to i64* 
  %fun22 = bitcast i64* %reg18 to i64* (i64*, i64*)* 
  %reg17 =  call ccc  i64*  %fun22(i64*  %__e125, i64*  %reg19)  
  ret i64* %reg17 
}


define external ccc  i64* @__111(i64*  %__clo112, i64*  %__x110)    {
__111:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo112 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__fib113 = inttoptr i64 %__r_4 to i64* 
  br label %entry20 
entry20:
  %__r_6 = inttoptr i64 0 to i64* 
  %cond5 = icmp eq i64* %__x110, %__r_6 
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
  %__r_17 = ptrtoint i64* %__x110 to i64 
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
  %__r_31 = ptrtoint i64* %__fib113 to i64 
  %__r_32 = ptrtoint i64* %__r_30 to i64 
  %__r_33 = add   i64 %__r_31, %__r_32 
  %__e114 = inttoptr i64 %__r_33 to i64* 
  %__r_35 = bitcast i64* %__e114 to i64** 
  %addr34 = getelementptr  i64*, i64** %__r_35, i64 0 
  %reg34 = load  i64*, i64** %addr34 
  %__r_36 = inttoptr i64 1 to i64* 
  %__r_37 = inttoptr i64 0 to i64* 
  %__r_38 = ptrtoint i64* %__r_36 to i64 
  %__r_39 = ptrtoint i64* %__r_37 to i64 
  %__r_40 = add   i64 %__r_38, %__r_39 
  %reg36 = inttoptr i64 %__r_40 to i64* 
  %__r_41 = ptrtoint i64* %__x110 to i64 
  %__r_42 = ptrtoint i64* %reg36 to i64 
  %__r_43 = sub   i64 %__r_41, %__r_42 
  %__r_44 = icmp slt i64 0, %__r_43 
  %__r_45 = zext i1 %__r_44 to i64  
  %__r_46 = mul   i64 %__r_43, %__r_45 
  %reg35 = inttoptr i64 %__r_46 to i64* 
  %fun47 = bitcast i64* %reg34 to i64* (i64*, i64*)* 
  %reg33 =  call ccc  i64*  %fun47(i64*  %__e114, i64*  %reg35)  
  %__r_48 = inttoptr i64 0 to i64* 
  %__r_49 = ptrtoint i64* %__fib113 to i64 
  %__r_50 = ptrtoint i64* %__r_48 to i64 
  %__r_51 = add   i64 %__r_49, %__r_50 
  %__e115 = inttoptr i64 %__r_51 to i64* 
  %__r_53 = bitcast i64* %__e115 to i64** 
  %addr52 = getelementptr  i64*, i64** %__r_53, i64 0 
  %reg38 = load  i64*, i64** %addr52 
  %__r_54 = inttoptr i64 1 to i64* 
  %__r_55 = inttoptr i64 0 to i64* 
  %__r_56 = ptrtoint i64* %__r_54 to i64 
  %__r_57 = ptrtoint i64* %__r_55 to i64 
  %__r_58 = add   i64 %__r_56, %__r_57 
  %reg41 = inttoptr i64 %__r_58 to i64* 
  %__r_59 = ptrtoint i64* %__x110 to i64 
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
  %reg37 =  call ccc  i64*  %fun76(i64*  %__e115, i64*  %reg39)  
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


define external ccc  i64* @__102(i64*  %__clo103, i64*  %__x101)    {
__102:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo103 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__resta104 = inttoptr i64 %__r_4 to i64* 
  %reg45 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__106, i64  2, i64*  %__resta104, i64*  %__x101)  
  ret i64* %reg45 
}


define external ccc  i64* @__106(i64*  %__clo107, i64*  %__y105)    {
__106:
  %__r_2 = bitcast i64* %__clo107 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg46 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg46 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__resta104 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo107 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg47 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg47 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__x101 = inttoptr i64 %__r_12 to i64* 
  br label %entry48 
entry48:
  %__r_14 = inttoptr i64 0 to i64* 
  %cond13 = icmp eq i64* %__y105, %__r_14 
  br i1 %cond13, label %then49, label %else50 
then49:
  br label %ifcont51 
else50:
  br label %entry52 
entry52:
  %__r_16 = inttoptr i64 0 to i64* 
  %cond15 = icmp eq i64* %__x101, %__r_16 
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
  %__r_23 = ptrtoint i64* %__resta104 to i64 
  %__r_24 = ptrtoint i64* %__r_22 to i64 
  %__r_25 = add   i64 %__r_23, %__r_24 
  %__e108 = inttoptr i64 %__r_25 to i64* 
  %__r_27 = bitcast i64* %__e108 to i64** 
  %addr26 = getelementptr  i64*, i64** %__r_27, i64 0 
  %reg58 = load  i64*, i64** %addr26 
  %__r_28 = inttoptr i64 1 to i64* 
  %__r_29 = inttoptr i64 0 to i64* 
  %__r_30 = ptrtoint i64* %__r_28 to i64 
  %__r_31 = ptrtoint i64* %__r_29 to i64 
  %__r_32 = add   i64 %__r_30, %__r_31 
  %reg60 = inttoptr i64 %__r_32 to i64* 
  %__r_33 = ptrtoint i64* %__x101 to i64 
  %__r_34 = ptrtoint i64* %reg60 to i64 
  %__r_35 = sub   i64 %__r_33, %__r_34 
  %__r_36 = icmp slt i64 0, %__r_35 
  %__r_37 = zext i1 %__r_36 to i64  
  %__r_38 = mul   i64 %__r_35, %__r_37 
  %reg59 = inttoptr i64 %__r_38 to i64* 
  %fun39 = bitcast i64* %reg58 to i64* (i64*, i64*)* 
  %reg57 =  call ccc  i64*  %fun39(i64*  %__e108, i64*  %reg59)  
  %__r_40 = inttoptr i64 0 to i64* 
  %__r_41 = ptrtoint i64* %reg57 to i64 
  %__r_42 = ptrtoint i64* %__r_40 to i64 
  %__r_43 = add   i64 %__r_41, %__r_42 
  %__e109 = inttoptr i64 %__r_43 to i64* 
  %__r_45 = bitcast i64* %__e109 to i64** 
  %addr44 = getelementptr  i64*, i64** %__r_45, i64 0 
  %reg62 = load  i64*, i64** %addr44 
  %__r_46 = inttoptr i64 1 to i64* 
  %__r_47 = inttoptr i64 0 to i64* 
  %__r_48 = ptrtoint i64* %__r_46 to i64 
  %__r_49 = ptrtoint i64* %__r_47 to i64 
  %__r_50 = add   i64 %__r_48, %__r_49 
  %reg64 = inttoptr i64 %__r_50 to i64* 
  %__r_51 = ptrtoint i64* %__y105 to i64 
  %__r_52 = ptrtoint i64* %reg64 to i64 
  %__r_53 = sub   i64 %__r_51, %__r_52 
  %__r_54 = icmp slt i64 0, %__r_53 
  %__r_55 = zext i1 %__r_54 to i64  
  %__r_56 = mul   i64 %__r_53, %__r_55 
  %reg63 = inttoptr i64 %__r_56 to i64* 
  %fun57 = bitcast i64* %reg62 to i64* (i64*, i64*)* 
  %reg61 =  call ccc  i64*  %fun57(i64*  %__e109, i64*  %reg63)  
  br label %ifcont55 
ifcont55:
  %regcont65 = phi i64* [%reg56, %then53], [%reg61, %else54] 
  br label %ifcont51 
ifcont51:
  %regcont66 = phi i64* [%__x101, %then49], [%regcont65, %ifcont55] 
  ret i64* %regcont66 
}


define external ccc  i64* @__93(i64*  %__clo94, i64*  %__x92)    {
__93:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo94 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__mult95 = inttoptr i64 %__r_4 to i64* 
  %reg67 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__97, i64  2, i64*  %__mult95, i64*  %__x92)  
  ret i64* %reg67 
}


define external ccc  i64* @__97(i64*  %__clo98, i64*  %__y96)    {
__97:
  %__r_2 = bitcast i64* %__clo98 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg68 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg68 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__mult95 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo98 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg69 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg69 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__x92 = inttoptr i64 %__r_12 to i64* 
  br label %entry70 
entry70:
  %__r_14 = inttoptr i64 0 to i64* 
  %cond13 = icmp eq i64* %__y96, %__r_14 
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
  %__r_21 = ptrtoint i64* %__mult95 to i64 
  %__r_22 = ptrtoint i64* %__r_20 to i64 
  %__r_23 = add   i64 %__r_21, %__r_22 
  %__e99 = inttoptr i64 %__r_23 to i64* 
  %__r_25 = bitcast i64* %__e99 to i64** 
  %addr24 = getelementptr  i64*, i64** %__r_25, i64 0 
  %reg77 = load  i64*, i64** %addr24 
  %fun26 = bitcast i64* %reg77 to i64* (i64*, i64*)* 
  %reg76 =  call ccc  i64*  %fun26(i64*  %__e99, i64*  %__x92)  
  %__r_27 = inttoptr i64 0 to i64* 
  %__r_28 = ptrtoint i64* %reg76 to i64 
  %__r_29 = ptrtoint i64* %__r_27 to i64 
  %__r_30 = add   i64 %__r_28, %__r_29 
  %__e100 = inttoptr i64 %__r_30 to i64* 
  %__r_32 = bitcast i64* %__e100 to i64** 
  %addr31 = getelementptr  i64*, i64** %__r_32, i64 0 
  %reg79 = load  i64*, i64** %addr31 
  %__r_33 = inttoptr i64 1 to i64* 
  %__r_34 = inttoptr i64 0 to i64* 
  %__r_35 = ptrtoint i64* %__r_33 to i64 
  %__r_36 = ptrtoint i64* %__r_34 to i64 
  %__r_37 = add   i64 %__r_35, %__r_36 
  %reg81 = inttoptr i64 %__r_37 to i64* 
  %__r_38 = ptrtoint i64* %__y96 to i64 
  %__r_39 = ptrtoint i64* %reg81 to i64 
  %__r_40 = sub   i64 %__r_38, %__r_39 
  %__r_41 = icmp slt i64 0, %__r_40 
  %__r_42 = zext i1 %__r_41 to i64  
  %__r_43 = mul   i64 %__r_40, %__r_42 
  %reg80 = inttoptr i64 %__r_43 to i64* 
  %fun44 = bitcast i64* %reg79 to i64* (i64*, i64*)* 
  %reg78 =  call ccc  i64*  %fun44(i64*  %__e100, i64*  %reg80)  
  %__r_45 = ptrtoint i64* %__x92 to i64 
  %__r_46 = ptrtoint i64* %reg78 to i64 
  %__r_47 = add   i64 %__r_45, %__r_46 
  %reg75 = inttoptr i64 %__r_47 to i64* 
  br label %ifcont73 
ifcont73:
  %regcont82 = phi i64* [%reg74, %then71], [%reg75, %else72] 
  ret i64* %regcont82 
}


define external ccc  i64* @__82(i64*  %__clo83, i64*  %__x81)    {
__82:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo83 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__exp84 = inttoptr i64 %__r_4 to i64* 
  %reg83 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__86, i64  2, i64*  %__exp84, i64*  %__x81)  
  ret i64* %reg83 
}


define external ccc  i64* @__86(i64*  %__clo87, i64*  %__y85)    {
__86:
  %__r_2 = bitcast i64* %__clo87 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg84 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg84 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__exp84 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo87 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg85 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg85 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__x81 = inttoptr i64 %__r_12 to i64* 
  br label %entry86 
entry86:
  %__r_14 = inttoptr i64 0 to i64* 
  %cond13 = icmp eq i64* %__y85, %__r_14 
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
  %__e88 = inttoptr i64 %__r_24 to i64* 
  %__r_26 = bitcast i64* %__e88 to i64** 
  %addr25 = getelementptr  i64*, i64** %__r_26, i64 0 
  %reg92 = load  i64*, i64** %addr25 
  %fun27 = bitcast i64* %reg92 to i64* (i64*, i64*)* 
  %reg91 =  call ccc  i64*  %fun27(i64*  %__e88, i64*  %__x81)  
  %__r_28 = inttoptr i64 0 to i64* 
  %__r_29 = ptrtoint i64* %reg91 to i64 
  %__r_30 = ptrtoint i64* %__r_28 to i64 
  %__r_31 = add   i64 %__r_29, %__r_30 
  %__e91 = inttoptr i64 %__r_31 to i64* 
  %__r_33 = bitcast i64* %__e91 to i64** 
  %addr32 = getelementptr  i64*, i64** %__r_33, i64 0 
  %reg94 = load  i64*, i64** %addr32 
  %__r_34 = inttoptr i64 0 to i64* 
  %__r_35 = ptrtoint i64* %__exp84 to i64 
  %__r_36 = ptrtoint i64* %__r_34 to i64 
  %__r_37 = add   i64 %__r_35, %__r_36 
  %__e89 = inttoptr i64 %__r_37 to i64* 
  %__r_39 = bitcast i64* %__e89 to i64** 
  %addr38 = getelementptr  i64*, i64** %__r_39, i64 0 
  %reg96 = load  i64*, i64** %addr38 
  %fun40 = bitcast i64* %reg96 to i64* (i64*, i64*)* 
  %reg95 =  call ccc  i64*  %fun40(i64*  %__e89, i64*  %__x81)  
  %__r_41 = inttoptr i64 0 to i64* 
  %__r_42 = ptrtoint i64* %reg95 to i64 
  %__r_43 = ptrtoint i64* %__r_41 to i64 
  %__r_44 = add   i64 %__r_42, %__r_43 
  %__e90 = inttoptr i64 %__r_44 to i64* 
  %__r_46 = bitcast i64* %__e90 to i64** 
  %addr45 = getelementptr  i64*, i64** %__r_46, i64 0 
  %reg98 = load  i64*, i64** %addr45 
  %__r_47 = inttoptr i64 1 to i64* 
  %__r_48 = inttoptr i64 0 to i64* 
  %__r_49 = ptrtoint i64* %__r_47 to i64 
  %__r_50 = ptrtoint i64* %__r_48 to i64 
  %__r_51 = add   i64 %__r_49, %__r_50 
  %reg100 = inttoptr i64 %__r_51 to i64* 
  %__r_52 = ptrtoint i64* %__y85 to i64 
  %__r_53 = ptrtoint i64* %reg100 to i64 
  %__r_54 = sub   i64 %__r_52, %__r_53 
  %__r_55 = icmp slt i64 0, %__r_54 
  %__r_56 = zext i1 %__r_55 to i64  
  %__r_57 = mul   i64 %__r_54, %__r_56 
  %reg99 = inttoptr i64 %__r_57 to i64* 
  %fun58 = bitcast i64* %reg98 to i64* (i64*, i64*)* 
  %reg97 =  call ccc  i64*  %fun58(i64*  %__e90, i64*  %reg99)  
  %fun59 = bitcast i64* %reg94 to i64* (i64*, i64*)* 
  %reg93 =  call ccc  i64*  %fun59(i64*  %__e91, i64*  %reg97)  
  br label %ifcont89 
ifcont89:
  %regcont101 = phi i64* [%reg90, %then87], [%reg93, %else88] 
  ret i64* %regcont101 
}


define external ccc  i64* @__75(i64*  %__clo76, i64*  %__x74)    {
__75:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo76 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__fact77 = inttoptr i64 %__r_4 to i64* 
  br label %entry102 
entry102:
  %__r_6 = inttoptr i64 0 to i64* 
  %cond5 = icmp eq i64* %__x74, %__r_6 
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
  %__e78 = inttoptr i64 %__r_16 to i64* 
  %__r_18 = bitcast i64* %__e78 to i64** 
  %addr17 = getelementptr  i64*, i64** %__r_18, i64 0 
  %reg108 = load  i64*, i64** %addr17 
  %fun19 = bitcast i64* %reg108 to i64* (i64*, i64*)* 
  %reg107 =  call ccc  i64*  %fun19(i64*  %__e78, i64*  %__x74)  
  %__r_20 = inttoptr i64 0 to i64* 
  %__r_21 = ptrtoint i64* %reg107 to i64 
  %__r_22 = ptrtoint i64* %__r_20 to i64 
  %__r_23 = add   i64 %__r_21, %__r_22 
  %__e80 = inttoptr i64 %__r_23 to i64* 
  %__r_25 = bitcast i64* %__e80 to i64** 
  %addr24 = getelementptr  i64*, i64** %__r_25, i64 0 
  %reg110 = load  i64*, i64** %addr24 
  %__r_26 = inttoptr i64 0 to i64* 
  %__r_27 = ptrtoint i64* %__fact77 to i64 
  %__r_28 = ptrtoint i64* %__r_26 to i64 
  %__r_29 = add   i64 %__r_27, %__r_28 
  %__e79 = inttoptr i64 %__r_29 to i64* 
  %__r_31 = bitcast i64* %__e79 to i64** 
  %addr30 = getelementptr  i64*, i64** %__r_31, i64 0 
  %reg112 = load  i64*, i64** %addr30 
  %__r_32 = inttoptr i64 1 to i64* 
  %__r_33 = inttoptr i64 0 to i64* 
  %__r_34 = ptrtoint i64* %__r_32 to i64 
  %__r_35 = ptrtoint i64* %__r_33 to i64 
  %__r_36 = add   i64 %__r_34, %__r_35 
  %reg114 = inttoptr i64 %__r_36 to i64* 
  %__r_37 = ptrtoint i64* %__x74 to i64 
  %__r_38 = ptrtoint i64* %reg114 to i64 
  %__r_39 = sub   i64 %__r_37, %__r_38 
  %__r_40 = icmp slt i64 0, %__r_39 
  %__r_41 = zext i1 %__r_40 to i64  
  %__r_42 = mul   i64 %__r_39, %__r_41 
  %reg113 = inttoptr i64 %__r_42 to i64* 
  %fun43 = bitcast i64* %reg112 to i64* (i64*, i64*)* 
  %reg111 =  call ccc  i64*  %fun43(i64*  %__e79, i64*  %reg113)  
  %fun44 = bitcast i64* %reg110 to i64* (i64*, i64*)* 
  %reg109 =  call ccc  i64*  %fun44(i64*  %__e80, i64*  %reg111)  
  br label %ifcont105 
ifcont105:
  %regcont115 = phi i64* [%reg106, %then103], [%reg109, %else104] 
  ret i64* %regcont115 
}


define external ccc  i64* @__64(i64*  %__clo65, i64*  %__n63)    {
__64:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo65 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__gcd66 = inttoptr i64 %__r_4 to i64* 
  %reg116 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__68, i64  2, i64*  %__gcd66, i64*  %__n63)  
  ret i64* %reg116 
}


define external ccc  i64* @__68(i64*  %__clo69, i64*  %__m67)    {
__68:
  %__r_2 = bitcast i64* %__clo69 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg117 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg117 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__gcd66 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo69 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg118 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg118 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__n63 = inttoptr i64 %__r_12 to i64* 
  br label %entry119 
entry119:
  %__r_14 = inttoptr i64 0 to i64* 
  %cond13 = icmp eq i64* %__n63, %__r_14 
  br i1 %cond13, label %then120, label %else121 
then120:
  br label %ifcont122 
else121:
  br label %entry123 
entry123:
  %__r_16 = inttoptr i64 0 to i64* 
  %cond15 = icmp eq i64* %__m67, %__r_16 
  br i1 %cond15, label %then124, label %else125 
then124:
  br label %ifcont126 
else125:
  br label %entry127 
entry127:
  %__r_17 = ptrtoint i64* %__n63 to i64 
  %__r_18 = ptrtoint i64* %__m67 to i64 
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
  %__r_26 = ptrtoint i64* %__gcd66 to i64 
  %__r_27 = ptrtoint i64* %__r_25 to i64 
  %__r_28 = add   i64 %__r_26, %__r_27 
  %__e70 = inttoptr i64 %__r_28 to i64* 
  %__r_30 = bitcast i64* %__e70 to i64** 
  %addr29 = getelementptr  i64*, i64** %__r_30, i64 0 
  %reg133 = load  i64*, i64** %addr29 
  %__r_31 = ptrtoint i64* %__m67 to i64 
  %__r_32 = ptrtoint i64* %__n63 to i64 
  %__r_33 = sub   i64 %__r_31, %__r_32 
  %__r_34 = icmp slt i64 0, %__r_33 
  %__r_35 = zext i1 %__r_34 to i64  
  %__r_36 = mul   i64 %__r_33, %__r_35 
  %reg134 = inttoptr i64 %__r_36 to i64* 
  %fun37 = bitcast i64* %reg133 to i64* (i64*, i64*)* 
  %reg132 =  call ccc  i64*  %fun37(i64*  %__e70, i64*  %reg134)  
  %__r_38 = inttoptr i64 0 to i64* 
  %__r_39 = ptrtoint i64* %reg132 to i64 
  %__r_40 = ptrtoint i64* %__r_38 to i64 
  %__r_41 = add   i64 %__r_39, %__r_40 
  %__e71 = inttoptr i64 %__r_41 to i64* 
  %__r_43 = bitcast i64* %__e71 to i64** 
  %addr42 = getelementptr  i64*, i64** %__r_43, i64 0 
  %reg136 = load  i64*, i64** %addr42 
  %fun44 = bitcast i64* %reg136 to i64* (i64*, i64*)* 
  %reg135 =  call ccc  i64*  %fun44(i64*  %__e71, i64*  %__n63)  
  br label %ifcont130 
else129:
  %__r_45 = inttoptr i64 0 to i64* 
  %__r_46 = ptrtoint i64* %__gcd66 to i64 
  %__r_47 = ptrtoint i64* %__r_45 to i64 
  %__r_48 = add   i64 %__r_46, %__r_47 
  %__e72 = inttoptr i64 %__r_48 to i64* 
  %__r_50 = bitcast i64* %__e72 to i64** 
  %addr49 = getelementptr  i64*, i64** %__r_50, i64 0 
  %reg138 = load  i64*, i64** %addr49 
  %fun51 = bitcast i64* %reg138 to i64* (i64*, i64*)* 
  %reg137 =  call ccc  i64*  %fun51(i64*  %__e72, i64*  %__m67)  
  %__r_52 = inttoptr i64 0 to i64* 
  %__r_53 = ptrtoint i64* %reg137 to i64 
  %__r_54 = ptrtoint i64* %__r_52 to i64 
  %__r_55 = add   i64 %__r_53, %__r_54 
  %__e73 = inttoptr i64 %__r_55 to i64* 
  %__r_57 = bitcast i64* %__e73 to i64** 
  %addr56 = getelementptr  i64*, i64** %__r_57, i64 0 
  %reg140 = load  i64*, i64** %addr56 
  %__r_58 = ptrtoint i64* %__n63 to i64 
  %__r_59 = ptrtoint i64* %__m67 to i64 
  %__r_60 = sub   i64 %__r_58, %__r_59 
  %__r_61 = icmp slt i64 0, %__r_60 
  %__r_62 = zext i1 %__r_61 to i64  
  %__r_63 = mul   i64 %__r_60, %__r_62 
  %reg141 = inttoptr i64 %__r_63 to i64* 
  %fun64 = bitcast i64* %reg140 to i64* (i64*, i64*)* 
  %reg139 =  call ccc  i64*  %fun64(i64*  %__e73, i64*  %reg141)  
  br label %ifcont130 
ifcont130:
  %regcont142 = phi i64* [%reg135, %then128], [%reg139, %else129] 
  br label %ifcont126 
ifcont126:
  %regcont143 = phi i64* [%__n63, %then124], [%regcont142, %ifcont130] 
  br label %ifcont122 
ifcont122:
  %regcont144 = phi i64* [%__m67, %then120], [%regcont143, %ifcont126] 
  ret i64* %regcont144 
}


define external ccc  i64* @__47(i64*  %__clo48, i64*  %__n46)    {
__47:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo48 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__gcd249 = inttoptr i64 %__r_4 to i64* 
  %reg145 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__51, i64  2, i64*  %__gcd249, i64*  %__n46)  
  ret i64* %reg145 
}


define external ccc  i64* @__51(i64*  %__clo52, i64*  %__m50)    {
__51:
  %__r_2 = bitcast i64* %__clo52 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg146 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg146 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__gcd249 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo52 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg147 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg147 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__n46 = inttoptr i64 %__r_12 to i64* 
  br label %entry148 
entry148:
  %__r_14 = inttoptr i64 0 to i64* 
  %cond13 = icmp eq i64* %__n46, %__r_14 
  br i1 %cond13, label %then149, label %else150 
then149:
  br label %ifcont151 
else150:
  br label %entry152 
entry152:
  %__r_16 = inttoptr i64 0 to i64* 
  %cond15 = icmp eq i64* %__m50, %__r_16 
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
  %__e53 = inttoptr i64 %__r_21 to i64* 
  %__r_23 = bitcast i64* %__e53 to i64** 
  %addr22 = getelementptr  i64*, i64** %__r_23, i64 0 
  %reg161 = load  i64*, i64** %addr22 
  %fun24 = bitcast i64* %reg161 to i64* (i64*, i64*)* 
  %reg160 =  call ccc  i64*  %fun24(i64*  %__e53, i64*  %__n46)  
  %__r_25 = inttoptr i64 0 to i64* 
  %__r_26 = ptrtoint i64* %reg160 to i64 
  %__r_27 = ptrtoint i64* %__r_25 to i64 
  %__r_28 = add   i64 %__r_26, %__r_27 
  %__e54 = inttoptr i64 %__r_28 to i64* 
  %__r_30 = bitcast i64* %__e54 to i64** 
  %addr29 = getelementptr  i64*, i64** %__r_30, i64 0 
  %reg163 = load  i64*, i64** %addr29 
  %fun31 = bitcast i64* %reg163 to i64* (i64*, i64*)* 
  %reg162 =  call ccc  i64*  %fun31(i64*  %__e54, i64*  %__m50)  
  %__r_33 = inttoptr i64 0 to i64* 
  %cond32 = icmp eq i64* %reg162, %__r_33 
  br i1 %cond32, label %then157, label %else158 
then157:
  %__r_34 = inttoptr i64 0 to i64* 
  %__r_35 = ptrtoint i64* %__gcd249 to i64 
  %__r_36 = ptrtoint i64* %__r_34 to i64 
  %__r_37 = add   i64 %__r_35, %__r_36 
  %__e57 = inttoptr i64 %__r_37 to i64* 
  %__r_39 = bitcast i64* %__e57 to i64** 
  %addr38 = getelementptr  i64*, i64** %__r_39, i64 0 
  %reg165 = load  i64*, i64** %addr38 
  %__r_40 = load  i64*, i64** @resta 
  %__r_41 = inttoptr i64 0 to i64* 
  %__r_42 = ptrtoint i64* %__r_40 to i64 
  %__r_43 = ptrtoint i64* %__r_41 to i64 
  %__r_44 = add   i64 %__r_42, %__r_43 
  %__e55 = inttoptr i64 %__r_44 to i64* 
  %__r_46 = bitcast i64* %__e55 to i64** 
  %addr45 = getelementptr  i64*, i64** %__r_46, i64 0 
  %reg167 = load  i64*, i64** %addr45 
  %fun47 = bitcast i64* %reg167 to i64* (i64*, i64*)* 
  %reg166 =  call ccc  i64*  %fun47(i64*  %__e55, i64*  %__m50)  
  %__r_48 = inttoptr i64 0 to i64* 
  %__r_49 = ptrtoint i64* %reg166 to i64 
  %__r_50 = ptrtoint i64* %__r_48 to i64 
  %__r_51 = add   i64 %__r_49, %__r_50 
  %__e56 = inttoptr i64 %__r_51 to i64* 
  %__r_53 = bitcast i64* %__e56 to i64** 
  %addr52 = getelementptr  i64*, i64** %__r_53, i64 0 
  %reg169 = load  i64*, i64** %addr52 
  %fun54 = bitcast i64* %reg169 to i64* (i64*, i64*)* 
  %reg168 =  call ccc  i64*  %fun54(i64*  %__e56, i64*  %__n46)  
  %fun55 = bitcast i64* %reg165 to i64* (i64*, i64*)* 
  %reg164 =  call ccc  i64*  %fun55(i64*  %__e57, i64*  %reg168)  
  %__r_56 = inttoptr i64 0 to i64* 
  %__r_57 = ptrtoint i64* %reg164 to i64 
  %__r_58 = ptrtoint i64* %__r_56 to i64 
  %__r_59 = add   i64 %__r_57, %__r_58 
  %__e58 = inttoptr i64 %__r_59 to i64* 
  %__r_61 = bitcast i64* %__e58 to i64** 
  %addr60 = getelementptr  i64*, i64** %__r_61, i64 0 
  %reg171 = load  i64*, i64** %addr60 
  %fun62 = bitcast i64* %reg171 to i64* (i64*, i64*)* 
  %reg170 =  call ccc  i64*  %fun62(i64*  %__e58, i64*  %__n46)  
  br label %ifcont159 
else158:
  %__r_63 = inttoptr i64 0 to i64* 
  %__r_64 = ptrtoint i64* %__gcd249 to i64 
  %__r_65 = ptrtoint i64* %__r_63 to i64 
  %__r_66 = add   i64 %__r_64, %__r_65 
  %__e59 = inttoptr i64 %__r_66 to i64* 
  %__r_68 = bitcast i64* %__e59 to i64** 
  %addr67 = getelementptr  i64*, i64** %__r_68, i64 0 
  %reg173 = load  i64*, i64** %addr67 
  %fun69 = bitcast i64* %reg173 to i64* (i64*, i64*)* 
  %reg172 =  call ccc  i64*  %fun69(i64*  %__e59, i64*  %__m50)  
  %__r_70 = inttoptr i64 0 to i64* 
  %__r_71 = ptrtoint i64* %reg172 to i64 
  %__r_72 = ptrtoint i64* %__r_70 to i64 
  %__r_73 = add   i64 %__r_71, %__r_72 
  %__e62 = inttoptr i64 %__r_73 to i64* 
  %__r_75 = bitcast i64* %__e62 to i64** 
  %addr74 = getelementptr  i64*, i64** %__r_75, i64 0 
  %reg175 = load  i64*, i64** %addr74 
  %__r_76 = load  i64*, i64** @resta 
  %__r_77 = inttoptr i64 0 to i64* 
  %__r_78 = ptrtoint i64* %__r_76 to i64 
  %__r_79 = ptrtoint i64* %__r_77 to i64 
  %__r_80 = add   i64 %__r_78, %__r_79 
  %__e60 = inttoptr i64 %__r_80 to i64* 
  %__r_82 = bitcast i64* %__e60 to i64** 
  %addr81 = getelementptr  i64*, i64** %__r_82, i64 0 
  %reg177 = load  i64*, i64** %addr81 
  %fun83 = bitcast i64* %reg177 to i64* (i64*, i64*)* 
  %reg176 =  call ccc  i64*  %fun83(i64*  %__e60, i64*  %__n46)  
  %__r_84 = inttoptr i64 0 to i64* 
  %__r_85 = ptrtoint i64* %reg176 to i64 
  %__r_86 = ptrtoint i64* %__r_84 to i64 
  %__r_87 = add   i64 %__r_85, %__r_86 
  %__e61 = inttoptr i64 %__r_87 to i64* 
  %__r_89 = bitcast i64* %__e61 to i64** 
  %addr88 = getelementptr  i64*, i64** %__r_89, i64 0 
  %reg179 = load  i64*, i64** %addr88 
  %fun90 = bitcast i64* %reg179 to i64* (i64*, i64*)* 
  %reg178 =  call ccc  i64*  %fun90(i64*  %__e61, i64*  %__m50)  
  %fun91 = bitcast i64* %reg175 to i64* (i64*, i64*)* 
  %reg174 =  call ccc  i64*  %fun91(i64*  %__e62, i64*  %reg178)  
  br label %ifcont159 
ifcont159:
  %regcont180 = phi i64* [%reg170, %then157], [%reg174, %else158] 
  br label %ifcont155 
ifcont155:
  %regcont181 = phi i64* [%__n46, %then153], [%regcont180, %ifcont159] 
  br label %ifcont151 
ifcont151:
  %regcont182 = phi i64* [%__m50, %then149], [%regcont181, %ifcont155] 
  ret i64* %regcont182 
}


define external ccc  i64* @__34(i64*  %__clo35, i64*  %__a33)    {
__34:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo35 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__multC236 = inttoptr i64 %__r_4 to i64* 
  %reg183 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__38, i64  2, i64*  %__a33, i64*  %__multC236)  
  ret i64* %reg183 
}


define external ccc  i64* @__38(i64*  %__clo39, i64*  %__b37)    {
__38:
  %__r_2 = bitcast i64* %__clo39 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg184 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg184 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__a33 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo39 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg185 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg185 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__multC236 = inttoptr i64 %__r_12 to i64* 
  %reg186 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__41, i64  3, i64*  %__a33, i64*  %__b37, i64*  %__multC236)  
  ret i64* %reg186 
}


define external ccc  i64* @__41(i64*  %__clo42, i64*  %__ac40)    {
__41:
  %__r_2 = bitcast i64* %__clo42 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg187 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg187 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__a33 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo42 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg188 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg188 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__b37 = inttoptr i64 %__r_12 to i64* 
  %__r_14 = bitcast i64* %__clo42 to i64** 
  %addr13 = getelementptr  i64*, i64** %__r_14, i64 3 
  %reg189 = load  i64*, i64** %addr13 
  %__r_15 = inttoptr i64 0 to i64* 
  %__r_16 = ptrtoint i64* %reg189 to i64 
  %__r_17 = ptrtoint i64* %__r_15 to i64 
  %__r_18 = add   i64 %__r_16, %__r_17 
  %__multC236 = inttoptr i64 %__r_18 to i64* 
  br label %entry190 
entry190:
  %__r_20 = inttoptr i64 0 to i64* 
  %cond19 = icmp eq i64* %__b37, %__r_20 
  br i1 %cond19, label %then191, label %else192 
then191:
  br label %ifcont193 
else192:
  %__r_21 = inttoptr i64 0 to i64* 
  %__r_22 = ptrtoint i64* %__multC236 to i64 
  %__r_23 = ptrtoint i64* %__r_21 to i64 
  %__r_24 = add   i64 %__r_22, %__r_23 
  %__e43 = inttoptr i64 %__r_24 to i64* 
  %__r_26 = bitcast i64* %__e43 to i64** 
  %addr25 = getelementptr  i64*, i64** %__r_26, i64 0 
  %reg195 = load  i64*, i64** %addr25 
  %fun27 = bitcast i64* %reg195 to i64* (i64*, i64*)* 
  %reg194 =  call ccc  i64*  %fun27(i64*  %__e43, i64*  %__a33)  
  %__r_28 = inttoptr i64 0 to i64* 
  %__r_29 = ptrtoint i64* %reg194 to i64 
  %__r_30 = ptrtoint i64* %__r_28 to i64 
  %__r_31 = add   i64 %__r_29, %__r_30 
  %__e44 = inttoptr i64 %__r_31 to i64* 
  %__r_33 = bitcast i64* %__e44 to i64** 
  %addr32 = getelementptr  i64*, i64** %__r_33, i64 0 
  %reg197 = load  i64*, i64** %addr32 
  %__r_34 = inttoptr i64 1 to i64* 
  %__r_35 = inttoptr i64 0 to i64* 
  %__r_36 = ptrtoint i64* %__r_34 to i64 
  %__r_37 = ptrtoint i64* %__r_35 to i64 
  %__r_38 = add   i64 %__r_36, %__r_37 
  %reg199 = inttoptr i64 %__r_38 to i64* 
  %__r_39 = ptrtoint i64* %__b37 to i64 
  %__r_40 = ptrtoint i64* %reg199 to i64 
  %__r_41 = sub   i64 %__r_39, %__r_40 
  %__r_42 = icmp slt i64 0, %__r_41 
  %__r_43 = zext i1 %__r_42 to i64  
  %__r_44 = mul   i64 %__r_41, %__r_43 
  %reg198 = inttoptr i64 %__r_44 to i64* 
  %fun45 = bitcast i64* %reg197 to i64* (i64*, i64*)* 
  %reg196 =  call ccc  i64*  %fun45(i64*  %__e44, i64*  %reg198)  
  %__r_46 = inttoptr i64 0 to i64* 
  %__r_47 = ptrtoint i64* %reg196 to i64 
  %__r_48 = ptrtoint i64* %__r_46 to i64 
  %__r_49 = add   i64 %__r_47, %__r_48 
  %__e45 = inttoptr i64 %__r_49 to i64* 
  %__r_51 = bitcast i64* %__e45 to i64** 
  %addr50 = getelementptr  i64*, i64** %__r_51, i64 0 
  %reg201 = load  i64*, i64** %addr50 
  %__r_52 = ptrtoint i64* %__ac40 to i64 
  %__r_53 = ptrtoint i64* %__a33 to i64 
  %__r_54 = add   i64 %__r_52, %__r_53 
  %reg202 = inttoptr i64 %__r_54 to i64* 
  %fun55 = bitcast i64* %reg201 to i64* (i64*, i64*)* 
  %reg200 =  call ccc  i64*  %fun55(i64*  %__e45, i64*  %reg202)  
  br label %ifcont193 
ifcont193:
  %regcont203 = phi i64* [%__ac40, %then191], [%reg200, %else192] 
  ret i64* %regcont203 
}


define external ccc  i64* @__25(i64*  %__clo26, i64*  %__a24)    {
__25:
  %reg204 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__28, i64  1, i64*  %__a24)  
  ret i64* %reg204 
}


define external ccc  i64* @__28(i64*  %__clo29, i64*  %__b27)    {
__28:
  %__r_2 = bitcast i64* %__clo29 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg205 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg205 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__a24 = inttoptr i64 %__r_6 to i64* 
  %__r_7 = load  i64*, i64** @multC2 
  %__r_8 = inttoptr i64 0 to i64* 
  %__r_9 = ptrtoint i64* %__r_7 to i64 
  %__r_10 = ptrtoint i64* %__r_8 to i64 
  %__r_11 = add   i64 %__r_9, %__r_10 
  %__e30 = inttoptr i64 %__r_11 to i64* 
  %__r_13 = bitcast i64* %__e30 to i64** 
  %addr12 = getelementptr  i64*, i64** %__r_13, i64 0 
  %reg207 = load  i64*, i64** %addr12 
  %fun14 = bitcast i64* %reg207 to i64* (i64*, i64*)* 
  %reg206 =  call ccc  i64*  %fun14(i64*  %__e30, i64*  %__a24)  
  %__r_15 = inttoptr i64 0 to i64* 
  %__r_16 = ptrtoint i64* %reg206 to i64 
  %__r_17 = ptrtoint i64* %__r_15 to i64 
  %__r_18 = add   i64 %__r_16, %__r_17 
  %__e31 = inttoptr i64 %__r_18 to i64* 
  %__r_20 = bitcast i64* %__e31 to i64** 
  %addr19 = getelementptr  i64*, i64** %__r_20, i64 0 
  %reg209 = load  i64*, i64** %addr19 
  %fun21 = bitcast i64* %reg209 to i64* (i64*, i64*)* 
  %reg208 =  call ccc  i64*  %fun21(i64*  %__e31, i64*  %__b27)  
  %__r_22 = inttoptr i64 0 to i64* 
  %__r_23 = ptrtoint i64* %reg208 to i64 
  %__r_24 = ptrtoint i64* %__r_22 to i64 
  %__r_25 = add   i64 %__r_23, %__r_24 
  %__e32 = inttoptr i64 %__r_25 to i64* 
  %__r_27 = bitcast i64* %__e32 to i64** 
  %addr26 = getelementptr  i64*, i64** %__r_27, i64 0 
  %reg211 = load  i64*, i64** %addr26 
  %__r_28 = inttoptr i64 0 to i64* 
  %__r_29 = inttoptr i64 0 to i64* 
  %__r_30 = ptrtoint i64* %__r_28 to i64 
  %__r_31 = ptrtoint i64* %__r_29 to i64 
  %__r_32 = add   i64 %__r_30, %__r_31 
  %reg212 = inttoptr i64 %__r_32 to i64* 
  %fun33 = bitcast i64* %reg211 to i64* (i64*, i64*)* 
  %reg210 =  call ccc  i64*  %fun33(i64*  %__e32, i64*  %reg212)  
  ret i64* %reg210 
}


define external ccc  i64* @__10(i64*  %__clo11, i64*  %__a9)    {
__10:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo11 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__expC212 = inttoptr i64 %__r_4 to i64* 
  %reg213 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__14, i64  2, i64*  %__a9, i64*  %__expC212)  
  ret i64* %reg213 
}


define external ccc  i64* @__14(i64*  %__clo15, i64*  %__b13)    {
__14:
  %__r_2 = bitcast i64* %__clo15 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg214 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg214 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__a9 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo15 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg215 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg215 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__expC212 = inttoptr i64 %__r_12 to i64* 
  %reg216 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__17, i64  3, i64*  %__a9, i64*  %__b13, i64*  %__expC212)  
  ret i64* %reg216 
}


define external ccc  i64* @__17(i64*  %__clo18, i64*  %__ac16)    {
__17:
  %__r_2 = bitcast i64* %__clo18 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg217 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg217 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__a9 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo18 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg218 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg218 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__b13 = inttoptr i64 %__r_12 to i64* 
  %__r_14 = bitcast i64* %__clo18 to i64** 
  %addr13 = getelementptr  i64*, i64** %__r_14, i64 3 
  %reg219 = load  i64*, i64** %addr13 
  %__r_15 = inttoptr i64 0 to i64* 
  %__r_16 = ptrtoint i64* %reg219 to i64 
  %__r_17 = ptrtoint i64* %__r_15 to i64 
  %__r_18 = add   i64 %__r_16, %__r_17 
  %__expC212 = inttoptr i64 %__r_18 to i64* 
  br label %entry220 
entry220:
  %__r_20 = inttoptr i64 0 to i64* 
  %cond19 = icmp eq i64* %__b13, %__r_20 
  br i1 %cond19, label %then221, label %else222 
then221:
  br label %ifcont223 
else222:
  %__r_21 = inttoptr i64 0 to i64* 
  %__r_22 = ptrtoint i64* %__expC212 to i64 
  %__r_23 = ptrtoint i64* %__r_21 to i64 
  %__r_24 = add   i64 %__r_22, %__r_23 
  %__e19 = inttoptr i64 %__r_24 to i64* 
  %__r_26 = bitcast i64* %__e19 to i64** 
  %addr25 = getelementptr  i64*, i64** %__r_26, i64 0 
  %reg225 = load  i64*, i64** %addr25 
  %fun27 = bitcast i64* %reg225 to i64* (i64*, i64*)* 
  %reg224 =  call ccc  i64*  %fun27(i64*  %__e19, i64*  %__a9)  
  %__r_28 = inttoptr i64 0 to i64* 
  %__r_29 = ptrtoint i64* %reg224 to i64 
  %__r_30 = ptrtoint i64* %__r_28 to i64 
  %__r_31 = add   i64 %__r_29, %__r_30 
  %__e20 = inttoptr i64 %__r_31 to i64* 
  %__r_33 = bitcast i64* %__e20 to i64** 
  %addr32 = getelementptr  i64*, i64** %__r_33, i64 0 
  %reg227 = load  i64*, i64** %addr32 
  %__r_34 = inttoptr i64 1 to i64* 
  %__r_35 = inttoptr i64 0 to i64* 
  %__r_36 = ptrtoint i64* %__r_34 to i64 
  %__r_37 = ptrtoint i64* %__r_35 to i64 
  %__r_38 = add   i64 %__r_36, %__r_37 
  %reg229 = inttoptr i64 %__r_38 to i64* 
  %__r_39 = ptrtoint i64* %__b13 to i64 
  %__r_40 = ptrtoint i64* %reg229 to i64 
  %__r_41 = sub   i64 %__r_39, %__r_40 
  %__r_42 = icmp slt i64 0, %__r_41 
  %__r_43 = zext i1 %__r_42 to i64  
  %__r_44 = mul   i64 %__r_41, %__r_43 
  %reg228 = inttoptr i64 %__r_44 to i64* 
  %fun45 = bitcast i64* %reg227 to i64* (i64*, i64*)* 
  %reg226 =  call ccc  i64*  %fun45(i64*  %__e20, i64*  %reg228)  
  %__r_46 = inttoptr i64 0 to i64* 
  %__r_47 = ptrtoint i64* %reg226 to i64 
  %__r_48 = ptrtoint i64* %__r_46 to i64 
  %__r_49 = add   i64 %__r_47, %__r_48 
  %__e23 = inttoptr i64 %__r_49 to i64* 
  %__r_51 = bitcast i64* %__e23 to i64** 
  %addr50 = getelementptr  i64*, i64** %__r_51, i64 0 
  %reg231 = load  i64*, i64** %addr50 
  %__r_52 = load  i64*, i64** @multC 
  %__r_53 = inttoptr i64 0 to i64* 
  %__r_54 = ptrtoint i64* %__r_52 to i64 
  %__r_55 = ptrtoint i64* %__r_53 to i64 
  %__r_56 = add   i64 %__r_54, %__r_55 
  %__e21 = inttoptr i64 %__r_56 to i64* 
  %__r_58 = bitcast i64* %__e21 to i64** 
  %addr57 = getelementptr  i64*, i64** %__r_58, i64 0 
  %reg233 = load  i64*, i64** %addr57 
  %fun59 = bitcast i64* %reg233 to i64* (i64*, i64*)* 
  %reg232 =  call ccc  i64*  %fun59(i64*  %__e21, i64*  %__a9)  
  %__r_60 = inttoptr i64 0 to i64* 
  %__r_61 = ptrtoint i64* %reg232 to i64 
  %__r_62 = ptrtoint i64* %__r_60 to i64 
  %__r_63 = add   i64 %__r_61, %__r_62 
  %__e22 = inttoptr i64 %__r_63 to i64* 
  %__r_65 = bitcast i64* %__e22 to i64** 
  %addr64 = getelementptr  i64*, i64** %__r_65, i64 0 
  %reg235 = load  i64*, i64** %addr64 
  %fun66 = bitcast i64* %reg235 to i64* (i64*, i64*)* 
  %reg234 =  call ccc  i64*  %fun66(i64*  %__e22, i64*  %__ac16)  
  %fun67 = bitcast i64* %reg231 to i64* (i64*, i64*)* 
  %reg230 =  call ccc  i64*  %fun67(i64*  %__e23, i64*  %reg234)  
  br label %ifcont223 
ifcont223:
  %regcont236 = phi i64* [%__ac16, %then221], [%reg230, %else222] 
  ret i64* %regcont236 
}


define external ccc  i64* @__1(i64*  %__clo2, i64*  %__a0)    {
__1:
  %reg237 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__4, i64  1, i64*  %__a0)  
  ret i64* %reg237 
}


define external ccc  i64* @__4(i64*  %__clo5, i64*  %__b3)    {
__4:
  %__r_2 = bitcast i64* %__clo5 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg238 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg238 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__a0 = inttoptr i64 %__r_6 to i64* 
  %__r_7 = load  i64*, i64** @expC2 
  %__r_8 = inttoptr i64 0 to i64* 
  %__r_9 = ptrtoint i64* %__r_7 to i64 
  %__r_10 = ptrtoint i64* %__r_8 to i64 
  %__r_11 = add   i64 %__r_9, %__r_10 
  %__e6 = inttoptr i64 %__r_11 to i64* 
  %__r_13 = bitcast i64* %__e6 to i64** 
  %addr12 = getelementptr  i64*, i64** %__r_13, i64 0 
  %reg240 = load  i64*, i64** %addr12 
  %fun14 = bitcast i64* %reg240 to i64* (i64*, i64*)* 
  %reg239 =  call ccc  i64*  %fun14(i64*  %__e6, i64*  %__a0)  
  %__r_15 = inttoptr i64 0 to i64* 
  %__r_16 = ptrtoint i64* %reg239 to i64 
  %__r_17 = ptrtoint i64* %__r_15 to i64 
  %__r_18 = add   i64 %__r_16, %__r_17 
  %__e7 = inttoptr i64 %__r_18 to i64* 
  %__r_20 = bitcast i64* %__e7 to i64** 
  %addr19 = getelementptr  i64*, i64** %__r_20, i64 0 
  %reg242 = load  i64*, i64** %addr19 
  %fun21 = bitcast i64* %reg242 to i64* (i64*, i64*)* 
  %reg241 =  call ccc  i64*  %fun21(i64*  %__e7, i64*  %__b3)  
  %__r_22 = inttoptr i64 0 to i64* 
  %__r_23 = ptrtoint i64* %reg241 to i64 
  %__r_24 = ptrtoint i64* %__r_22 to i64 
  %__r_25 = add   i64 %__r_23, %__r_24 
  %__e8 = inttoptr i64 %__r_25 to i64* 
  %__r_27 = bitcast i64* %__e8 to i64** 
  %addr26 = getelementptr  i64*, i64** %__r_27, i64 0 
  %reg244 = load  i64*, i64** %addr26 
  %__r_28 = inttoptr i64 1 to i64* 
  %__r_29 = inttoptr i64 0 to i64* 
  %__r_30 = ptrtoint i64* %__r_28 to i64 
  %__r_31 = ptrtoint i64* %__r_29 to i64 
  %__r_32 = add   i64 %__r_30, %__r_31 
  %reg245 = inttoptr i64 %__r_32 to i64* 
  %fun33 = bitcast i64* %reg244 to i64* (i64*, i64*)* 
  %reg243 =  call ccc  i64*  %fun33(i64*  %__e8, i64*  %reg245)  
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


@res6 = internal   global i64* zeroinitializer


define external ccc  i64* @pcfmain()    {
pcfmain:
  %reg246 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__130, i64  0)  
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %reg246 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__r_5 = inttoptr i64 %__r_4 to i64* 
  store  i64* %__r_5, i64** @suman 
  %reg247 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__127, i64  0)  
  %__r_6 = inttoptr i64 0 to i64* 
  %__r_7 = ptrtoint i64* %reg247 to i64 
  %__r_8 = ptrtoint i64* %__r_6 to i64 
  %__r_9 = add   i64 %__r_7, %__r_8 
  %__r_10 = inttoptr i64 %__r_9 to i64* 
  store  i64* %__r_10, i64** @doble 
  %reg248 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__117, i64  0)  
  %__r_11 = inttoptr i64 0 to i64* 
  %__r_12 = ptrtoint i64* %reg248 to i64 
  %__r_13 = ptrtoint i64* %__r_11 to i64 
  %__r_14 = add   i64 %__r_12, %__r_13 
  %__r_15 = inttoptr i64 %__r_14 to i64* 
  store  i64* %__r_15, i64** @sumard 
  %reg249 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__111, i64  0)  
  %__r_16 = inttoptr i64 0 to i64* 
  %__r_17 = ptrtoint i64* %reg249 to i64 
  %__r_18 = ptrtoint i64* %__r_16 to i64 
  %__r_19 = add   i64 %__r_17, %__r_18 
  %__r_20 = inttoptr i64 %__r_19 to i64* 
  store  i64* %__r_20, i64** @fib 
  %reg250 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__102, i64  0)  
  %__r_21 = inttoptr i64 0 to i64* 
  %__r_22 = ptrtoint i64* %reg250 to i64 
  %__r_23 = ptrtoint i64* %__r_21 to i64 
  %__r_24 = add   i64 %__r_22, %__r_23 
  %__r_25 = inttoptr i64 %__r_24 to i64* 
  store  i64* %__r_25, i64** @resta 
  %reg251 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__93, i64  0)  
  %__r_26 = inttoptr i64 0 to i64* 
  %__r_27 = ptrtoint i64* %reg251 to i64 
  %__r_28 = ptrtoint i64* %__r_26 to i64 
  %__r_29 = add   i64 %__r_27, %__r_28 
  %__r_30 = inttoptr i64 %__r_29 to i64* 
  store  i64* %__r_30, i64** @mult 
  %reg252 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__82, i64  0)  
  %__r_31 = inttoptr i64 0 to i64* 
  %__r_32 = ptrtoint i64* %reg252 to i64 
  %__r_33 = ptrtoint i64* %__r_31 to i64 
  %__r_34 = add   i64 %__r_32, %__r_33 
  %__r_35 = inttoptr i64 %__r_34 to i64* 
  store  i64* %__r_35, i64** @exp 
  %reg253 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__75, i64  0)  
  %__r_36 = inttoptr i64 0 to i64* 
  %__r_37 = ptrtoint i64* %reg253 to i64 
  %__r_38 = ptrtoint i64* %__r_36 to i64 
  %__r_39 = add   i64 %__r_37, %__r_38 
  %__r_40 = inttoptr i64 %__r_39 to i64* 
  store  i64* %__r_40, i64** @fact 
  %reg254 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__64, i64  0)  
  %__r_41 = inttoptr i64 0 to i64* 
  %__r_42 = ptrtoint i64* %reg254 to i64 
  %__r_43 = ptrtoint i64* %__r_41 to i64 
  %__r_44 = add   i64 %__r_42, %__r_43 
  %__r_45 = inttoptr i64 %__r_44 to i64* 
  store  i64* %__r_45, i64** @gcd 
  %reg255 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__47, i64  0)  
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
  %__r_61 = inttoptr i64 7 to i64* 
  %__r_62 = inttoptr i64 0 to i64* 
  %__r_63 = ptrtoint i64* %__r_61 to i64 
  %__r_64 = ptrtoint i64* %__r_62 to i64 
  %__r_65 = add   i64 %__r_63, %__r_64 
  %reg257 = inttoptr i64 %__r_65 to i64* 
  %__r_66 = inttoptr i64 0 to i64* 
  %__r_67 = ptrtoint i64* %reg257 to i64 
  %__r_68 = ptrtoint i64* %__r_66 to i64 
  %__r_69 = add   i64 %__r_67, %__r_68 
  %__r_70 = inttoptr i64 %__r_69 to i64* 
  store  i64* %__r_70, i64** @y 
  %__r_71 = inttoptr i64 14 to i64* 
  %__r_72 = inttoptr i64 0 to i64* 
  %__r_73 = ptrtoint i64* %__r_71 to i64 
  %__r_74 = ptrtoint i64* %__r_72 to i64 
  %__r_75 = add   i64 %__r_73, %__r_74 
  %reg258 = inttoptr i64 %__r_75 to i64* 
  %__r_76 = inttoptr i64 0 to i64* 
  %__r_77 = ptrtoint i64* %reg258 to i64 
  %__r_78 = ptrtoint i64* %__r_76 to i64 
  %__r_79 = add   i64 %__r_77, %__r_78 
  %__r_80 = inttoptr i64 %__r_79 to i64* 
  store  i64* %__r_80, i64** @z 
  %reg259 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__34, i64  0)  
  %__r_81 = inttoptr i64 0 to i64* 
  %__r_82 = ptrtoint i64* %reg259 to i64 
  %__r_83 = ptrtoint i64* %__r_81 to i64 
  %__r_84 = add   i64 %__r_82, %__r_83 
  %__r_85 = inttoptr i64 %__r_84 to i64* 
  store  i64* %__r_85, i64** @multC2 
  %reg260 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__25, i64  0)  
  %__r_86 = inttoptr i64 0 to i64* 
  %__r_87 = ptrtoint i64* %reg260 to i64 
  %__r_88 = ptrtoint i64* %__r_86 to i64 
  %__r_89 = add   i64 %__r_87, %__r_88 
  %__r_90 = inttoptr i64 %__r_89 to i64* 
  store  i64* %__r_90, i64** @multC 
  %reg261 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__10, i64  0)  
  %__r_91 = inttoptr i64 0 to i64* 
  %__r_92 = ptrtoint i64* %reg261 to i64 
  %__r_93 = ptrtoint i64* %__r_91 to i64 
  %__r_94 = add   i64 %__r_92, %__r_93 
  %__r_95 = inttoptr i64 %__r_94 to i64* 
  store  i64* %__r_95, i64** @expC2 
  %reg262 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__1, i64  0)  
  %__r_96 = inttoptr i64 0 to i64* 
  %__r_97 = ptrtoint i64* %reg262 to i64 
  %__r_98 = ptrtoint i64* %__r_96 to i64 
  %__r_99 = add   i64 %__r_97, %__r_98 
  %__r_100 = inttoptr i64 %__r_99 to i64* 
  store  i64* %__r_100, i64** @expC 
  %__r_101 = inttoptr i64 10 to i64* 
  %__r_102 = inttoptr i64 0 to i64* 
  %__r_103 = ptrtoint i64* %__r_101 to i64 
  %__r_104 = ptrtoint i64* %__r_102 to i64 
  %__r_105 = add   i64 %__r_103, %__r_104 
  %reg263 = inttoptr i64 %__r_105 to i64* 
  %__r_106 = ptrtoint i64* %reg263 to i64 
  %__r_107 =  call ccc  i64  @pcf_print(i64  %__r_106)  
  %reg264 = inttoptr i64 %__r_107 to i64* 
  %__r_108 = inttoptr i64 0 to i64* 
  %__r_109 = ptrtoint i64* %reg264 to i64 
  %__r_110 = ptrtoint i64* %__r_108 to i64 
  %__r_111 = add   i64 %__r_109, %__r_110 
  %__r_112 = inttoptr i64 %__r_111 to i64* 
  store  i64* %__r_112, i64** @res6 
  ret i64* %reg263 
}