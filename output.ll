; ModuleID = 'pcfprog'


 


declare external ccc  i64* @pcf_mkclosure(i64* (i64*, i64*)*, i64, ...)    


declare external ccc  i64 @pcf_print(i64)    


define external ccc  i64* @__136(i64*  %__clo137, i64*  %__x135)    {
__136:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo137 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__suman138 = inttoptr i64 %__r_4 to i64* 
  br label %entry0 
entry0:
  %__r_6 = inttoptr i64 0 to i64* 
  %cond5 = icmp eq i64* %__x135, %__r_6 
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
  %__r_13 = ptrtoint i64* %__suman138 to i64 
  %__r_14 = ptrtoint i64* %__r_12 to i64 
  %__r_15 = add   i64 %__r_13, %__r_14 
  %__e139 = inttoptr i64 %__r_15 to i64* 
  %__r_17 = bitcast i64* %__e139 to i64** 
  %addr16 = getelementptr  i64*, i64** %__r_17, i64 0 
  %reg7 = load  i64*, i64** %addr16 
  %__r_18 = inttoptr i64 1 to i64* 
  %__r_19 = inttoptr i64 0 to i64* 
  %__r_20 = ptrtoint i64* %__r_18 to i64 
  %__r_21 = ptrtoint i64* %__r_19 to i64 
  %__r_22 = add   i64 %__r_20, %__r_21 
  %reg9 = inttoptr i64 %__r_22 to i64* 
  %__r_23 = ptrtoint i64* %__x135 to i64 
  %__r_24 = ptrtoint i64* %reg9 to i64 
  %__r_25 = sub   i64 %__r_23, %__r_24 
  %__r_26 = icmp slt i64 0, %__r_25 
  %__r_27 = zext i1 %__r_26 to i64  
  %__r_28 = mul   i64 %__r_25, %__r_27 
  %reg8 = inttoptr i64 %__r_28 to i64* 
  %fun29 = bitcast i64* %reg7 to i64* (i64*, i64*)* 
  %reg6 =  call ccc  i64*  %fun29(i64*  %__e139, i64*  %reg8)  
  %__r_30 = ptrtoint i64* %__x135 to i64 
  %__r_31 = ptrtoint i64* %reg6 to i64 
  %__r_32 = add   i64 %__r_30, %__r_31 
  %reg5 = inttoptr i64 %__r_32 to i64* 
  br label %ifcont3 
ifcont3:
  %regcont10 = phi i64* [%reg4, %then1], [%reg5, %else2] 
  ret i64* %regcont10 
}


define external ccc  i64* @__133(i64*  %__clo134, i64*  %__x132)    {
__133:
  %__r_1 = ptrtoint i64* %__x132 to i64 
  %__r_2 = ptrtoint i64* %__x132 to i64 
  %__r_3 = add   i64 %__r_1, %__r_2 
  %reg11 = inttoptr i64 %__r_3 to i64* 
  ret i64* %reg11 
}


define external ccc  i64* @__123(i64*  %__clo124, i64*  %__x122)    {
__123:
  %reg12 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__126, i64  1, i64*  %__x122)  
  ret i64* %reg12 
}


define external ccc  i64* @__126(i64*  %__clo127, i64*  %__y125)    {
__126:
  %__r_2 = bitcast i64* %__clo127 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg13 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg13 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__x122 = inttoptr i64 %__r_6 to i64* 
  %reg14 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__129, i64  2, i64*  %__x122, i64*  %__y125)  
  ret i64* %reg14 
}


define external ccc  i64* @__129(i64*  %__clo130, i64*  %__f128)    {
__129:
  %__r_2 = bitcast i64* %__clo130 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg15 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg15 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__x122 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo130 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg16 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg16 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__y125 = inttoptr i64 %__r_12 to i64* 
  %__r_13 = inttoptr i64 0 to i64* 
  %__r_14 = ptrtoint i64* %__f128 to i64 
  %__r_15 = ptrtoint i64* %__r_13 to i64 
  %__r_16 = add   i64 %__r_14, %__r_15 
  %__e131 = inttoptr i64 %__r_16 to i64* 
  %__r_18 = bitcast i64* %__e131 to i64** 
  %addr17 = getelementptr  i64*, i64** %__r_18, i64 0 
  %reg18 = load  i64*, i64** %addr17 
  %__r_19 = ptrtoint i64* %__x122 to i64 
  %__r_20 = ptrtoint i64* %__y125 to i64 
  %__r_21 = add   i64 %__r_19, %__r_20 
  %reg19 = inttoptr i64 %__r_21 to i64* 
  %fun22 = bitcast i64* %reg18 to i64* (i64*, i64*)* 
  %reg17 =  call ccc  i64*  %fun22(i64*  %__e131, i64*  %reg19)  
  ret i64* %reg17 
}


define external ccc  i64* @__117(i64*  %__clo118, i64*  %__x116)    {
__117:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo118 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__fib119 = inttoptr i64 %__r_4 to i64* 
  br label %entry20 
entry20:
  %__r_6 = inttoptr i64 0 to i64* 
  %cond5 = icmp eq i64* %__x116, %__r_6 
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
  %__r_17 = ptrtoint i64* %__x116 to i64 
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
  %__r_31 = ptrtoint i64* %__fib119 to i64 
  %__r_32 = ptrtoint i64* %__r_30 to i64 
  %__r_33 = add   i64 %__r_31, %__r_32 
  %__e120 = inttoptr i64 %__r_33 to i64* 
  %__r_35 = bitcast i64* %__e120 to i64** 
  %addr34 = getelementptr  i64*, i64** %__r_35, i64 0 
  %reg34 = load  i64*, i64** %addr34 
  %__r_36 = inttoptr i64 1 to i64* 
  %__r_37 = inttoptr i64 0 to i64* 
  %__r_38 = ptrtoint i64* %__r_36 to i64 
  %__r_39 = ptrtoint i64* %__r_37 to i64 
  %__r_40 = add   i64 %__r_38, %__r_39 
  %reg36 = inttoptr i64 %__r_40 to i64* 
  %__r_41 = ptrtoint i64* %__x116 to i64 
  %__r_42 = ptrtoint i64* %reg36 to i64 
  %__r_43 = sub   i64 %__r_41, %__r_42 
  %__r_44 = icmp slt i64 0, %__r_43 
  %__r_45 = zext i1 %__r_44 to i64  
  %__r_46 = mul   i64 %__r_43, %__r_45 
  %reg35 = inttoptr i64 %__r_46 to i64* 
  %fun47 = bitcast i64* %reg34 to i64* (i64*, i64*)* 
  %reg33 =  call ccc  i64*  %fun47(i64*  %__e120, i64*  %reg35)  
  %__r_48 = inttoptr i64 0 to i64* 
  %__r_49 = ptrtoint i64* %__fib119 to i64 
  %__r_50 = ptrtoint i64* %__r_48 to i64 
  %__r_51 = add   i64 %__r_49, %__r_50 
  %__e121 = inttoptr i64 %__r_51 to i64* 
  %__r_53 = bitcast i64* %__e121 to i64** 
  %addr52 = getelementptr  i64*, i64** %__r_53, i64 0 
  %reg38 = load  i64*, i64** %addr52 
  %__r_54 = inttoptr i64 1 to i64* 
  %__r_55 = inttoptr i64 0 to i64* 
  %__r_56 = ptrtoint i64* %__r_54 to i64 
  %__r_57 = ptrtoint i64* %__r_55 to i64 
  %__r_58 = add   i64 %__r_56, %__r_57 
  %reg41 = inttoptr i64 %__r_58 to i64* 
  %__r_59 = ptrtoint i64* %__x116 to i64 
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
  %reg37 =  call ccc  i64*  %fun76(i64*  %__e121, i64*  %reg39)  
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


define external ccc  i64* @__108(i64*  %__clo109, i64*  %__x107)    {
__108:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo109 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__resta110 = inttoptr i64 %__r_4 to i64* 
  %reg45 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__112, i64  2, i64*  %__resta110, i64*  %__x107)  
  ret i64* %reg45 
}


define external ccc  i64* @__112(i64*  %__clo113, i64*  %__y111)    {
__112:
  %__r_2 = bitcast i64* %__clo113 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg46 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg46 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__resta110 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo113 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg47 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg47 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__x107 = inttoptr i64 %__r_12 to i64* 
  br label %entry48 
entry48:
  %__r_14 = inttoptr i64 0 to i64* 
  %cond13 = icmp eq i64* %__y111, %__r_14 
  br i1 %cond13, label %then49, label %else50 
then49:
  br label %ifcont51 
else50:
  br label %entry52 
entry52:
  %__r_16 = inttoptr i64 0 to i64* 
  %cond15 = icmp eq i64* %__x107, %__r_16 
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
  %__r_23 = ptrtoint i64* %__resta110 to i64 
  %__r_24 = ptrtoint i64* %__r_22 to i64 
  %__r_25 = add   i64 %__r_23, %__r_24 
  %__e114 = inttoptr i64 %__r_25 to i64* 
  %__r_27 = bitcast i64* %__e114 to i64** 
  %addr26 = getelementptr  i64*, i64** %__r_27, i64 0 
  %reg58 = load  i64*, i64** %addr26 
  %__r_28 = inttoptr i64 1 to i64* 
  %__r_29 = inttoptr i64 0 to i64* 
  %__r_30 = ptrtoint i64* %__r_28 to i64 
  %__r_31 = ptrtoint i64* %__r_29 to i64 
  %__r_32 = add   i64 %__r_30, %__r_31 
  %reg60 = inttoptr i64 %__r_32 to i64* 
  %__r_33 = ptrtoint i64* %__x107 to i64 
  %__r_34 = ptrtoint i64* %reg60 to i64 
  %__r_35 = sub   i64 %__r_33, %__r_34 
  %__r_36 = icmp slt i64 0, %__r_35 
  %__r_37 = zext i1 %__r_36 to i64  
  %__r_38 = mul   i64 %__r_35, %__r_37 
  %reg59 = inttoptr i64 %__r_38 to i64* 
  %fun39 = bitcast i64* %reg58 to i64* (i64*, i64*)* 
  %reg57 =  call ccc  i64*  %fun39(i64*  %__e114, i64*  %reg59)  
  %__r_40 = inttoptr i64 0 to i64* 
  %__r_41 = ptrtoint i64* %reg57 to i64 
  %__r_42 = ptrtoint i64* %__r_40 to i64 
  %__r_43 = add   i64 %__r_41, %__r_42 
  %__e115 = inttoptr i64 %__r_43 to i64* 
  %__r_45 = bitcast i64* %__e115 to i64** 
  %addr44 = getelementptr  i64*, i64** %__r_45, i64 0 
  %reg62 = load  i64*, i64** %addr44 
  %__r_46 = inttoptr i64 1 to i64* 
  %__r_47 = inttoptr i64 0 to i64* 
  %__r_48 = ptrtoint i64* %__r_46 to i64 
  %__r_49 = ptrtoint i64* %__r_47 to i64 
  %__r_50 = add   i64 %__r_48, %__r_49 
  %reg64 = inttoptr i64 %__r_50 to i64* 
  %__r_51 = ptrtoint i64* %__y111 to i64 
  %__r_52 = ptrtoint i64* %reg64 to i64 
  %__r_53 = sub   i64 %__r_51, %__r_52 
  %__r_54 = icmp slt i64 0, %__r_53 
  %__r_55 = zext i1 %__r_54 to i64  
  %__r_56 = mul   i64 %__r_53, %__r_55 
  %reg63 = inttoptr i64 %__r_56 to i64* 
  %fun57 = bitcast i64* %reg62 to i64* (i64*, i64*)* 
  %reg61 =  call ccc  i64*  %fun57(i64*  %__e115, i64*  %reg63)  
  br label %ifcont55 
ifcont55:
  %regcont65 = phi i64* [%reg56, %then53], [%reg61, %else54] 
  br label %ifcont51 
ifcont51:
  %regcont66 = phi i64* [%__x107, %then49], [%regcont65, %ifcont55] 
  ret i64* %regcont66 
}


define external ccc  i64* @__99(i64*  %__clo100, i64*  %__x98)    {
__99:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo100 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__mult101 = inttoptr i64 %__r_4 to i64* 
  %reg67 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__103, i64  2, i64*  %__mult101, i64*  %__x98)  
  ret i64* %reg67 
}


define external ccc  i64* @__103(i64*  %__clo104, i64*  %__y102)    {
__103:
  %__r_2 = bitcast i64* %__clo104 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg68 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg68 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__mult101 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo104 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg69 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg69 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__x98 = inttoptr i64 %__r_12 to i64* 
  br label %entry70 
entry70:
  %__r_14 = inttoptr i64 0 to i64* 
  %cond13 = icmp eq i64* %__y102, %__r_14 
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
  %__r_21 = ptrtoint i64* %__mult101 to i64 
  %__r_22 = ptrtoint i64* %__r_20 to i64 
  %__r_23 = add   i64 %__r_21, %__r_22 
  %__e105 = inttoptr i64 %__r_23 to i64* 
  %__r_25 = bitcast i64* %__e105 to i64** 
  %addr24 = getelementptr  i64*, i64** %__r_25, i64 0 
  %reg77 = load  i64*, i64** %addr24 
  %fun26 = bitcast i64* %reg77 to i64* (i64*, i64*)* 
  %reg76 =  call ccc  i64*  %fun26(i64*  %__e105, i64*  %__x98)  
  %__r_27 = inttoptr i64 0 to i64* 
  %__r_28 = ptrtoint i64* %reg76 to i64 
  %__r_29 = ptrtoint i64* %__r_27 to i64 
  %__r_30 = add   i64 %__r_28, %__r_29 
  %__e106 = inttoptr i64 %__r_30 to i64* 
  %__r_32 = bitcast i64* %__e106 to i64** 
  %addr31 = getelementptr  i64*, i64** %__r_32, i64 0 
  %reg79 = load  i64*, i64** %addr31 
  %__r_33 = inttoptr i64 1 to i64* 
  %__r_34 = inttoptr i64 0 to i64* 
  %__r_35 = ptrtoint i64* %__r_33 to i64 
  %__r_36 = ptrtoint i64* %__r_34 to i64 
  %__r_37 = add   i64 %__r_35, %__r_36 
  %reg81 = inttoptr i64 %__r_37 to i64* 
  %__r_38 = ptrtoint i64* %__y102 to i64 
  %__r_39 = ptrtoint i64* %reg81 to i64 
  %__r_40 = sub   i64 %__r_38, %__r_39 
  %__r_41 = icmp slt i64 0, %__r_40 
  %__r_42 = zext i1 %__r_41 to i64  
  %__r_43 = mul   i64 %__r_40, %__r_42 
  %reg80 = inttoptr i64 %__r_43 to i64* 
  %fun44 = bitcast i64* %reg79 to i64* (i64*, i64*)* 
  %reg78 =  call ccc  i64*  %fun44(i64*  %__e106, i64*  %reg80)  
  %__r_45 = ptrtoint i64* %__x98 to i64 
  %__r_46 = ptrtoint i64* %reg78 to i64 
  %__r_47 = add   i64 %__r_45, %__r_46 
  %reg75 = inttoptr i64 %__r_47 to i64* 
  br label %ifcont73 
ifcont73:
  %regcont82 = phi i64* [%reg74, %then71], [%reg75, %else72] 
  ret i64* %regcont82 
}


define external ccc  i64* @__88(i64*  %__clo89, i64*  %__x87)    {
__88:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo89 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__exp90 = inttoptr i64 %__r_4 to i64* 
  %reg83 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__92, i64  2, i64*  %__exp90, i64*  %__x87)  
  ret i64* %reg83 
}


define external ccc  i64* @__92(i64*  %__clo93, i64*  %__y91)    {
__92:
  %__r_2 = bitcast i64* %__clo93 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg84 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg84 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__exp90 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo93 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg85 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg85 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__x87 = inttoptr i64 %__r_12 to i64* 
  br label %entry86 
entry86:
  %__r_14 = inttoptr i64 0 to i64* 
  %cond13 = icmp eq i64* %__y91, %__r_14 
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
  %__e94 = inttoptr i64 %__r_24 to i64* 
  %__r_26 = bitcast i64* %__e94 to i64** 
  %addr25 = getelementptr  i64*, i64** %__r_26, i64 0 
  %reg92 = load  i64*, i64** %addr25 
  %fun27 = bitcast i64* %reg92 to i64* (i64*, i64*)* 
  %reg91 =  call ccc  i64*  %fun27(i64*  %__e94, i64*  %__x87)  
  %__r_28 = inttoptr i64 0 to i64* 
  %__r_29 = ptrtoint i64* %reg91 to i64 
  %__r_30 = ptrtoint i64* %__r_28 to i64 
  %__r_31 = add   i64 %__r_29, %__r_30 
  %__e97 = inttoptr i64 %__r_31 to i64* 
  %__r_33 = bitcast i64* %__e97 to i64** 
  %addr32 = getelementptr  i64*, i64** %__r_33, i64 0 
  %reg94 = load  i64*, i64** %addr32 
  %__r_34 = inttoptr i64 0 to i64* 
  %__r_35 = ptrtoint i64* %__exp90 to i64 
  %__r_36 = ptrtoint i64* %__r_34 to i64 
  %__r_37 = add   i64 %__r_35, %__r_36 
  %__e95 = inttoptr i64 %__r_37 to i64* 
  %__r_39 = bitcast i64* %__e95 to i64** 
  %addr38 = getelementptr  i64*, i64** %__r_39, i64 0 
  %reg96 = load  i64*, i64** %addr38 
  %fun40 = bitcast i64* %reg96 to i64* (i64*, i64*)* 
  %reg95 =  call ccc  i64*  %fun40(i64*  %__e95, i64*  %__x87)  
  %__r_41 = inttoptr i64 0 to i64* 
  %__r_42 = ptrtoint i64* %reg95 to i64 
  %__r_43 = ptrtoint i64* %__r_41 to i64 
  %__r_44 = add   i64 %__r_42, %__r_43 
  %__e96 = inttoptr i64 %__r_44 to i64* 
  %__r_46 = bitcast i64* %__e96 to i64** 
  %addr45 = getelementptr  i64*, i64** %__r_46, i64 0 
  %reg98 = load  i64*, i64** %addr45 
  %__r_47 = inttoptr i64 1 to i64* 
  %__r_48 = inttoptr i64 0 to i64* 
  %__r_49 = ptrtoint i64* %__r_47 to i64 
  %__r_50 = ptrtoint i64* %__r_48 to i64 
  %__r_51 = add   i64 %__r_49, %__r_50 
  %reg100 = inttoptr i64 %__r_51 to i64* 
  %__r_52 = ptrtoint i64* %__y91 to i64 
  %__r_53 = ptrtoint i64* %reg100 to i64 
  %__r_54 = sub   i64 %__r_52, %__r_53 
  %__r_55 = icmp slt i64 0, %__r_54 
  %__r_56 = zext i1 %__r_55 to i64  
  %__r_57 = mul   i64 %__r_54, %__r_56 
  %reg99 = inttoptr i64 %__r_57 to i64* 
  %fun58 = bitcast i64* %reg98 to i64* (i64*, i64*)* 
  %reg97 =  call ccc  i64*  %fun58(i64*  %__e96, i64*  %reg99)  
  %fun59 = bitcast i64* %reg94 to i64* (i64*, i64*)* 
  %reg93 =  call ccc  i64*  %fun59(i64*  %__e97, i64*  %reg97)  
  br label %ifcont89 
ifcont89:
  %regcont101 = phi i64* [%reg90, %then87], [%reg93, %else88] 
  ret i64* %regcont101 
}


define external ccc  i64* @__81(i64*  %__clo82, i64*  %__x80)    {
__81:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo82 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__fact83 = inttoptr i64 %__r_4 to i64* 
  br label %entry102 
entry102:
  %__r_6 = inttoptr i64 0 to i64* 
  %cond5 = icmp eq i64* %__x80, %__r_6 
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
  %__e84 = inttoptr i64 %__r_16 to i64* 
  %__r_18 = bitcast i64* %__e84 to i64** 
  %addr17 = getelementptr  i64*, i64** %__r_18, i64 0 
  %reg108 = load  i64*, i64** %addr17 
  %fun19 = bitcast i64* %reg108 to i64* (i64*, i64*)* 
  %reg107 =  call ccc  i64*  %fun19(i64*  %__e84, i64*  %__x80)  
  %__r_20 = inttoptr i64 0 to i64* 
  %__r_21 = ptrtoint i64* %reg107 to i64 
  %__r_22 = ptrtoint i64* %__r_20 to i64 
  %__r_23 = add   i64 %__r_21, %__r_22 
  %__e86 = inttoptr i64 %__r_23 to i64* 
  %__r_25 = bitcast i64* %__e86 to i64** 
  %addr24 = getelementptr  i64*, i64** %__r_25, i64 0 
  %reg110 = load  i64*, i64** %addr24 
  %__r_26 = inttoptr i64 0 to i64* 
  %__r_27 = ptrtoint i64* %__fact83 to i64 
  %__r_28 = ptrtoint i64* %__r_26 to i64 
  %__r_29 = add   i64 %__r_27, %__r_28 
  %__e85 = inttoptr i64 %__r_29 to i64* 
  %__r_31 = bitcast i64* %__e85 to i64** 
  %addr30 = getelementptr  i64*, i64** %__r_31, i64 0 
  %reg112 = load  i64*, i64** %addr30 
  %__r_32 = inttoptr i64 1 to i64* 
  %__r_33 = inttoptr i64 0 to i64* 
  %__r_34 = ptrtoint i64* %__r_32 to i64 
  %__r_35 = ptrtoint i64* %__r_33 to i64 
  %__r_36 = add   i64 %__r_34, %__r_35 
  %reg114 = inttoptr i64 %__r_36 to i64* 
  %__r_37 = ptrtoint i64* %__x80 to i64 
  %__r_38 = ptrtoint i64* %reg114 to i64 
  %__r_39 = sub   i64 %__r_37, %__r_38 
  %__r_40 = icmp slt i64 0, %__r_39 
  %__r_41 = zext i1 %__r_40 to i64  
  %__r_42 = mul   i64 %__r_39, %__r_41 
  %reg113 = inttoptr i64 %__r_42 to i64* 
  %fun43 = bitcast i64* %reg112 to i64* (i64*, i64*)* 
  %reg111 =  call ccc  i64*  %fun43(i64*  %__e85, i64*  %reg113)  
  %fun44 = bitcast i64* %reg110 to i64* (i64*, i64*)* 
  %reg109 =  call ccc  i64*  %fun44(i64*  %__e86, i64*  %reg111)  
  br label %ifcont105 
ifcont105:
  %regcont115 = phi i64* [%reg106, %then103], [%reg109, %else104] 
  ret i64* %regcont115 
}


define external ccc  i64* @__70(i64*  %__clo71, i64*  %__n69)    {
__70:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo71 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__gcd72 = inttoptr i64 %__r_4 to i64* 
  %reg116 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__74, i64  2, i64*  %__gcd72, i64*  %__n69)  
  ret i64* %reg116 
}


define external ccc  i64* @__74(i64*  %__clo75, i64*  %__m73)    {
__74:
  %__r_2 = bitcast i64* %__clo75 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg117 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg117 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__gcd72 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo75 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg118 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg118 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__n69 = inttoptr i64 %__r_12 to i64* 
  br label %entry119 
entry119:
  %__r_14 = inttoptr i64 0 to i64* 
  %cond13 = icmp eq i64* %__n69, %__r_14 
  br i1 %cond13, label %then120, label %else121 
then120:
  br label %ifcont122 
else121:
  br label %entry123 
entry123:
  %__r_16 = inttoptr i64 0 to i64* 
  %cond15 = icmp eq i64* %__m73, %__r_16 
  br i1 %cond15, label %then124, label %else125 
then124:
  br label %ifcont126 
else125:
  br label %entry127 
entry127:
  %__r_17 = ptrtoint i64* %__n69 to i64 
  %__r_18 = ptrtoint i64* %__m73 to i64 
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
  %__r_26 = ptrtoint i64* %__gcd72 to i64 
  %__r_27 = ptrtoint i64* %__r_25 to i64 
  %__r_28 = add   i64 %__r_26, %__r_27 
  %__e76 = inttoptr i64 %__r_28 to i64* 
  %__r_30 = bitcast i64* %__e76 to i64** 
  %addr29 = getelementptr  i64*, i64** %__r_30, i64 0 
  %reg133 = load  i64*, i64** %addr29 
  %__r_31 = ptrtoint i64* %__m73 to i64 
  %__r_32 = ptrtoint i64* %__n69 to i64 
  %__r_33 = sub   i64 %__r_31, %__r_32 
  %__r_34 = icmp slt i64 0, %__r_33 
  %__r_35 = zext i1 %__r_34 to i64  
  %__r_36 = mul   i64 %__r_33, %__r_35 
  %reg134 = inttoptr i64 %__r_36 to i64* 
  %fun37 = bitcast i64* %reg133 to i64* (i64*, i64*)* 
  %reg132 =  call ccc  i64*  %fun37(i64*  %__e76, i64*  %reg134)  
  %__r_38 = inttoptr i64 0 to i64* 
  %__r_39 = ptrtoint i64* %reg132 to i64 
  %__r_40 = ptrtoint i64* %__r_38 to i64 
  %__r_41 = add   i64 %__r_39, %__r_40 
  %__e77 = inttoptr i64 %__r_41 to i64* 
  %__r_43 = bitcast i64* %__e77 to i64** 
  %addr42 = getelementptr  i64*, i64** %__r_43, i64 0 
  %reg136 = load  i64*, i64** %addr42 
  %fun44 = bitcast i64* %reg136 to i64* (i64*, i64*)* 
  %reg135 =  call ccc  i64*  %fun44(i64*  %__e77, i64*  %__n69)  
  br label %ifcont130 
else129:
  %__r_45 = inttoptr i64 0 to i64* 
  %__r_46 = ptrtoint i64* %__gcd72 to i64 
  %__r_47 = ptrtoint i64* %__r_45 to i64 
  %__r_48 = add   i64 %__r_46, %__r_47 
  %__e78 = inttoptr i64 %__r_48 to i64* 
  %__r_50 = bitcast i64* %__e78 to i64** 
  %addr49 = getelementptr  i64*, i64** %__r_50, i64 0 
  %reg138 = load  i64*, i64** %addr49 
  %fun51 = bitcast i64* %reg138 to i64* (i64*, i64*)* 
  %reg137 =  call ccc  i64*  %fun51(i64*  %__e78, i64*  %__m73)  
  %__r_52 = inttoptr i64 0 to i64* 
  %__r_53 = ptrtoint i64* %reg137 to i64 
  %__r_54 = ptrtoint i64* %__r_52 to i64 
  %__r_55 = add   i64 %__r_53, %__r_54 
  %__e79 = inttoptr i64 %__r_55 to i64* 
  %__r_57 = bitcast i64* %__e79 to i64** 
  %addr56 = getelementptr  i64*, i64** %__r_57, i64 0 
  %reg140 = load  i64*, i64** %addr56 
  %__r_58 = ptrtoint i64* %__n69 to i64 
  %__r_59 = ptrtoint i64* %__m73 to i64 
  %__r_60 = sub   i64 %__r_58, %__r_59 
  %__r_61 = icmp slt i64 0, %__r_60 
  %__r_62 = zext i1 %__r_61 to i64  
  %__r_63 = mul   i64 %__r_60, %__r_62 
  %reg141 = inttoptr i64 %__r_63 to i64* 
  %fun64 = bitcast i64* %reg140 to i64* (i64*, i64*)* 
  %reg139 =  call ccc  i64*  %fun64(i64*  %__e79, i64*  %reg141)  
  br label %ifcont130 
ifcont130:
  %regcont142 = phi i64* [%reg135, %then128], [%reg139, %else129] 
  br label %ifcont126 
ifcont126:
  %regcont143 = phi i64* [%__n69, %then124], [%regcont142, %ifcont130] 
  br label %ifcont122 
ifcont122:
  %regcont144 = phi i64* [%__m73, %then120], [%regcont143, %ifcont126] 
  ret i64* %regcont144 
}


define external ccc  i64* @__53(i64*  %__clo54, i64*  %__n52)    {
__53:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo54 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__gcd255 = inttoptr i64 %__r_4 to i64* 
  %reg145 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__57, i64  2, i64*  %__gcd255, i64*  %__n52)  
  ret i64* %reg145 
}


define external ccc  i64* @__57(i64*  %__clo58, i64*  %__m56)    {
__57:
  %__r_2 = bitcast i64* %__clo58 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg146 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg146 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__gcd255 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo58 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg147 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg147 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__n52 = inttoptr i64 %__r_12 to i64* 
  br label %entry148 
entry148:
  %__r_14 = inttoptr i64 0 to i64* 
  %cond13 = icmp eq i64* %__n52, %__r_14 
  br i1 %cond13, label %then149, label %else150 
then149:
  br label %ifcont151 
else150:
  br label %entry152 
entry152:
  %__r_16 = inttoptr i64 0 to i64* 
  %cond15 = icmp eq i64* %__m56, %__r_16 
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
  %__e59 = inttoptr i64 %__r_21 to i64* 
  %__r_23 = bitcast i64* %__e59 to i64** 
  %addr22 = getelementptr  i64*, i64** %__r_23, i64 0 
  %reg161 = load  i64*, i64** %addr22 
  %fun24 = bitcast i64* %reg161 to i64* (i64*, i64*)* 
  %reg160 =  call ccc  i64*  %fun24(i64*  %__e59, i64*  %__n52)  
  %__r_25 = inttoptr i64 0 to i64* 
  %__r_26 = ptrtoint i64* %reg160 to i64 
  %__r_27 = ptrtoint i64* %__r_25 to i64 
  %__r_28 = add   i64 %__r_26, %__r_27 
  %__e60 = inttoptr i64 %__r_28 to i64* 
  %__r_30 = bitcast i64* %__e60 to i64** 
  %addr29 = getelementptr  i64*, i64** %__r_30, i64 0 
  %reg163 = load  i64*, i64** %addr29 
  %fun31 = bitcast i64* %reg163 to i64* (i64*, i64*)* 
  %reg162 =  call ccc  i64*  %fun31(i64*  %__e60, i64*  %__m56)  
  %__r_33 = inttoptr i64 0 to i64* 
  %cond32 = icmp eq i64* %reg162, %__r_33 
  br i1 %cond32, label %then157, label %else158 
then157:
  %__r_34 = inttoptr i64 0 to i64* 
  %__r_35 = ptrtoint i64* %__gcd255 to i64 
  %__r_36 = ptrtoint i64* %__r_34 to i64 
  %__r_37 = add   i64 %__r_35, %__r_36 
  %__e63 = inttoptr i64 %__r_37 to i64* 
  %__r_39 = bitcast i64* %__e63 to i64** 
  %addr38 = getelementptr  i64*, i64** %__r_39, i64 0 
  %reg165 = load  i64*, i64** %addr38 
  %__r_40 = load  i64*, i64** @resta 
  %__r_41 = inttoptr i64 0 to i64* 
  %__r_42 = ptrtoint i64* %__r_40 to i64 
  %__r_43 = ptrtoint i64* %__r_41 to i64 
  %__r_44 = add   i64 %__r_42, %__r_43 
  %__e61 = inttoptr i64 %__r_44 to i64* 
  %__r_46 = bitcast i64* %__e61 to i64** 
  %addr45 = getelementptr  i64*, i64** %__r_46, i64 0 
  %reg167 = load  i64*, i64** %addr45 
  %fun47 = bitcast i64* %reg167 to i64* (i64*, i64*)* 
  %reg166 =  call ccc  i64*  %fun47(i64*  %__e61, i64*  %__m56)  
  %__r_48 = inttoptr i64 0 to i64* 
  %__r_49 = ptrtoint i64* %reg166 to i64 
  %__r_50 = ptrtoint i64* %__r_48 to i64 
  %__r_51 = add   i64 %__r_49, %__r_50 
  %__e62 = inttoptr i64 %__r_51 to i64* 
  %__r_53 = bitcast i64* %__e62 to i64** 
  %addr52 = getelementptr  i64*, i64** %__r_53, i64 0 
  %reg169 = load  i64*, i64** %addr52 
  %fun54 = bitcast i64* %reg169 to i64* (i64*, i64*)* 
  %reg168 =  call ccc  i64*  %fun54(i64*  %__e62, i64*  %__n52)  
  %fun55 = bitcast i64* %reg165 to i64* (i64*, i64*)* 
  %reg164 =  call ccc  i64*  %fun55(i64*  %__e63, i64*  %reg168)  
  %__r_56 = inttoptr i64 0 to i64* 
  %__r_57 = ptrtoint i64* %reg164 to i64 
  %__r_58 = ptrtoint i64* %__r_56 to i64 
  %__r_59 = add   i64 %__r_57, %__r_58 
  %__e64 = inttoptr i64 %__r_59 to i64* 
  %__r_61 = bitcast i64* %__e64 to i64** 
  %addr60 = getelementptr  i64*, i64** %__r_61, i64 0 
  %reg171 = load  i64*, i64** %addr60 
  %fun62 = bitcast i64* %reg171 to i64* (i64*, i64*)* 
  %reg170 =  call ccc  i64*  %fun62(i64*  %__e64, i64*  %__n52)  
  br label %ifcont159 
else158:
  %__r_63 = inttoptr i64 0 to i64* 
  %__r_64 = ptrtoint i64* %__gcd255 to i64 
  %__r_65 = ptrtoint i64* %__r_63 to i64 
  %__r_66 = add   i64 %__r_64, %__r_65 
  %__e65 = inttoptr i64 %__r_66 to i64* 
  %__r_68 = bitcast i64* %__e65 to i64** 
  %addr67 = getelementptr  i64*, i64** %__r_68, i64 0 
  %reg173 = load  i64*, i64** %addr67 
  %fun69 = bitcast i64* %reg173 to i64* (i64*, i64*)* 
  %reg172 =  call ccc  i64*  %fun69(i64*  %__e65, i64*  %__m56)  
  %__r_70 = inttoptr i64 0 to i64* 
  %__r_71 = ptrtoint i64* %reg172 to i64 
  %__r_72 = ptrtoint i64* %__r_70 to i64 
  %__r_73 = add   i64 %__r_71, %__r_72 
  %__e68 = inttoptr i64 %__r_73 to i64* 
  %__r_75 = bitcast i64* %__e68 to i64** 
  %addr74 = getelementptr  i64*, i64** %__r_75, i64 0 
  %reg175 = load  i64*, i64** %addr74 
  %__r_76 = load  i64*, i64** @resta 
  %__r_77 = inttoptr i64 0 to i64* 
  %__r_78 = ptrtoint i64* %__r_76 to i64 
  %__r_79 = ptrtoint i64* %__r_77 to i64 
  %__r_80 = add   i64 %__r_78, %__r_79 
  %__e66 = inttoptr i64 %__r_80 to i64* 
  %__r_82 = bitcast i64* %__e66 to i64** 
  %addr81 = getelementptr  i64*, i64** %__r_82, i64 0 
  %reg177 = load  i64*, i64** %addr81 
  %fun83 = bitcast i64* %reg177 to i64* (i64*, i64*)* 
  %reg176 =  call ccc  i64*  %fun83(i64*  %__e66, i64*  %__n52)  
  %__r_84 = inttoptr i64 0 to i64* 
  %__r_85 = ptrtoint i64* %reg176 to i64 
  %__r_86 = ptrtoint i64* %__r_84 to i64 
  %__r_87 = add   i64 %__r_85, %__r_86 
  %__e67 = inttoptr i64 %__r_87 to i64* 
  %__r_89 = bitcast i64* %__e67 to i64** 
  %addr88 = getelementptr  i64*, i64** %__r_89, i64 0 
  %reg179 = load  i64*, i64** %addr88 
  %fun90 = bitcast i64* %reg179 to i64* (i64*, i64*)* 
  %reg178 =  call ccc  i64*  %fun90(i64*  %__e67, i64*  %__m56)  
  %fun91 = bitcast i64* %reg175 to i64* (i64*, i64*)* 
  %reg174 =  call ccc  i64*  %fun91(i64*  %__e68, i64*  %reg178)  
  br label %ifcont159 
ifcont159:
  %regcont180 = phi i64* [%reg170, %then157], [%reg174, %else158] 
  br label %ifcont155 
ifcont155:
  %regcont181 = phi i64* [%__n52, %then153], [%regcont180, %ifcont159] 
  br label %ifcont151 
ifcont151:
  %regcont182 = phi i64* [%__m56, %then149], [%regcont181, %ifcont155] 
  ret i64* %regcont182 
}


define external ccc  i64* @__39(i64*  %__clo40, i64*  %__a38)    {
__39:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo40 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__multC241 = inttoptr i64 %__r_4 to i64* 
  %reg183 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__43, i64  2, i64*  %__a38, i64*  %__multC241)  
  ret i64* %reg183 
}


define external ccc  i64* @__43(i64*  %__clo44, i64*  %__b42)    {
__43:
  %__r_2 = bitcast i64* %__clo44 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg184 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg184 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__a38 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo44 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg185 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg185 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__multC241 = inttoptr i64 %__r_12 to i64* 
  %reg186 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__46, i64  3, i64*  %__a38, i64*  %__b42, i64*  %__multC241)  
  ret i64* %reg186 
}


define external ccc  i64* @__46(i64*  %__clo47, i64*  %__ac45)    {
__46:
  %__r_2 = bitcast i64* %__clo47 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg187 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg187 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__a38 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo47 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg188 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg188 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__b42 = inttoptr i64 %__r_12 to i64* 
  %__r_14 = bitcast i64* %__clo47 to i64** 
  %addr13 = getelementptr  i64*, i64** %__r_14, i64 3 
  %reg189 = load  i64*, i64** %addr13 
  %__r_15 = inttoptr i64 0 to i64* 
  %__r_16 = ptrtoint i64* %reg189 to i64 
  %__r_17 = ptrtoint i64* %__r_15 to i64 
  %__r_18 = add   i64 %__r_16, %__r_17 
  %__multC241 = inttoptr i64 %__r_18 to i64* 
  br label %entry190 
entry190:
  %__r_20 = inttoptr i64 0 to i64* 
  %cond19 = icmp eq i64* %__b42, %__r_20 
  br i1 %cond19, label %then191, label %else192 
then191:
  br label %ifcont193 
else192:
  %__r_21 = inttoptr i64 0 to i64* 
  %__r_22 = ptrtoint i64* %__multC241 to i64 
  %__r_23 = ptrtoint i64* %__r_21 to i64 
  %__r_24 = add   i64 %__r_22, %__r_23 
  %__e48 = inttoptr i64 %__r_24 to i64* 
  %__r_26 = bitcast i64* %__e48 to i64** 
  %addr25 = getelementptr  i64*, i64** %__r_26, i64 0 
  %reg195 = load  i64*, i64** %addr25 
  %fun27 = bitcast i64* %reg195 to i64* (i64*, i64*)* 
  %reg194 =  call ccc  i64*  %fun27(i64*  %__e48, i64*  %__a38)  
  %__r_28 = inttoptr i64 0 to i64* 
  %__r_29 = ptrtoint i64* %reg194 to i64 
  %__r_30 = ptrtoint i64* %__r_28 to i64 
  %__r_31 = add   i64 %__r_29, %__r_30 
  %__e49 = inttoptr i64 %__r_31 to i64* 
  %__r_33 = bitcast i64* %__e49 to i64** 
  %addr32 = getelementptr  i64*, i64** %__r_33, i64 0 
  %reg197 = load  i64*, i64** %addr32 
  %__r_34 = inttoptr i64 1 to i64* 
  %__r_35 = inttoptr i64 0 to i64* 
  %__r_36 = ptrtoint i64* %__r_34 to i64 
  %__r_37 = ptrtoint i64* %__r_35 to i64 
  %__r_38 = add   i64 %__r_36, %__r_37 
  %reg199 = inttoptr i64 %__r_38 to i64* 
  %__r_39 = ptrtoint i64* %__b42 to i64 
  %__r_40 = ptrtoint i64* %reg199 to i64 
  %__r_41 = sub   i64 %__r_39, %__r_40 
  %__r_42 = icmp slt i64 0, %__r_41 
  %__r_43 = zext i1 %__r_42 to i64  
  %__r_44 = mul   i64 %__r_41, %__r_43 
  %reg198 = inttoptr i64 %__r_44 to i64* 
  %fun45 = bitcast i64* %reg197 to i64* (i64*, i64*)* 
  %reg196 =  call ccc  i64*  %fun45(i64*  %__e49, i64*  %reg198)  
  %__r_46 = inttoptr i64 0 to i64* 
  %__r_47 = ptrtoint i64* %reg196 to i64 
  %__r_48 = ptrtoint i64* %__r_46 to i64 
  %__r_49 = add   i64 %__r_47, %__r_48 
  %__e50 = inttoptr i64 %__r_49 to i64* 
  %__r_51 = bitcast i64* %__e50 to i64** 
  %addr50 = getelementptr  i64*, i64** %__r_51, i64 0 
  %reg201 = load  i64*, i64** %addr50 
  %__r_52 = ptrtoint i64* %__ac45 to i64 
  %__r_53 = ptrtoint i64* %__a38 to i64 
  %__r_54 = add   i64 %__r_52, %__r_53 
  %reg202 = inttoptr i64 %__r_54 to i64* 
  %fun55 = bitcast i64* %reg201 to i64* (i64*, i64*)* 
  %reg200 =  call ccc  i64*  %fun55(i64*  %__e50, i64*  %reg202)  
  br label %ifcont193 
ifcont193:
  %regcont203 = phi i64* [%__ac45, %then191], [%reg200, %else192] 
  ret i64* %regcont203 
}


define external ccc  i64* @__30(i64*  %__clo31, i64*  %__a29)    {
__30:
  %reg204 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__33, i64  1, i64*  %__a29)  
  ret i64* %reg204 
}


define external ccc  i64* @__33(i64*  %__clo34, i64*  %__b32)    {
__33:
  %__r_2 = bitcast i64* %__clo34 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg205 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg205 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__a29 = inttoptr i64 %__r_6 to i64* 
  %__r_7 = load  i64*, i64** @multC2 
  %__r_8 = inttoptr i64 0 to i64* 
  %__r_9 = ptrtoint i64* %__r_7 to i64 
  %__r_10 = ptrtoint i64* %__r_8 to i64 
  %__r_11 = add   i64 %__r_9, %__r_10 
  %__e35 = inttoptr i64 %__r_11 to i64* 
  %__r_13 = bitcast i64* %__e35 to i64** 
  %addr12 = getelementptr  i64*, i64** %__r_13, i64 0 
  %reg207 = load  i64*, i64** %addr12 
  %fun14 = bitcast i64* %reg207 to i64* (i64*, i64*)* 
  %reg206 =  call ccc  i64*  %fun14(i64*  %__e35, i64*  %__a29)  
  %__r_15 = inttoptr i64 0 to i64* 
  %__r_16 = ptrtoint i64* %reg206 to i64 
  %__r_17 = ptrtoint i64* %__r_15 to i64 
  %__r_18 = add   i64 %__r_16, %__r_17 
  %__e36 = inttoptr i64 %__r_18 to i64* 
  %__r_20 = bitcast i64* %__e36 to i64** 
  %addr19 = getelementptr  i64*, i64** %__r_20, i64 0 
  %reg209 = load  i64*, i64** %addr19 
  %fun21 = bitcast i64* %reg209 to i64* (i64*, i64*)* 
  %reg208 =  call ccc  i64*  %fun21(i64*  %__e36, i64*  %__b32)  
  %__r_22 = inttoptr i64 0 to i64* 
  %__r_23 = ptrtoint i64* %reg208 to i64 
  %__r_24 = ptrtoint i64* %__r_22 to i64 
  %__r_25 = add   i64 %__r_23, %__r_24 
  %__e37 = inttoptr i64 %__r_25 to i64* 
  %__r_27 = bitcast i64* %__e37 to i64** 
  %addr26 = getelementptr  i64*, i64** %__r_27, i64 0 
  %reg211 = load  i64*, i64** %addr26 
  %__r_28 = inttoptr i64 0 to i64* 
  %__r_29 = inttoptr i64 0 to i64* 
  %__r_30 = ptrtoint i64* %__r_28 to i64 
  %__r_31 = ptrtoint i64* %__r_29 to i64 
  %__r_32 = add   i64 %__r_30, %__r_31 
  %reg212 = inttoptr i64 %__r_32 to i64* 
  %fun33 = bitcast i64* %reg211 to i64* (i64*, i64*)* 
  %reg210 =  call ccc  i64*  %fun33(i64*  %__e37, i64*  %reg212)  
  ret i64* %reg210 
}


define external ccc  i64* @__15(i64*  %__clo16, i64*  %__a14)    {
__15:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo16 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__expC217 = inttoptr i64 %__r_4 to i64* 
  %reg213 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__19, i64  2, i64*  %__a14, i64*  %__expC217)  
  ret i64* %reg213 
}


define external ccc  i64* @__19(i64*  %__clo20, i64*  %__b18)    {
__19:
  %__r_2 = bitcast i64* %__clo20 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg214 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg214 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__a14 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo20 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg215 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg215 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__expC217 = inttoptr i64 %__r_12 to i64* 
  %reg216 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__22, i64  3, i64*  %__a14, i64*  %__b18, i64*  %__expC217)  
  ret i64* %reg216 
}


define external ccc  i64* @__22(i64*  %__clo23, i64*  %__ac21)    {
__22:
  %__r_2 = bitcast i64* %__clo23 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg217 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg217 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__a14 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo23 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg218 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg218 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__b18 = inttoptr i64 %__r_12 to i64* 
  %__r_14 = bitcast i64* %__clo23 to i64** 
  %addr13 = getelementptr  i64*, i64** %__r_14, i64 3 
  %reg219 = load  i64*, i64** %addr13 
  %__r_15 = inttoptr i64 0 to i64* 
  %__r_16 = ptrtoint i64* %reg219 to i64 
  %__r_17 = ptrtoint i64* %__r_15 to i64 
  %__r_18 = add   i64 %__r_16, %__r_17 
  %__expC217 = inttoptr i64 %__r_18 to i64* 
  br label %entry220 
entry220:
  %__r_20 = inttoptr i64 0 to i64* 
  %cond19 = icmp eq i64* %__b18, %__r_20 
  br i1 %cond19, label %then221, label %else222 
then221:
  br label %ifcont223 
else222:
  %__r_21 = inttoptr i64 0 to i64* 
  %__r_22 = ptrtoint i64* %__expC217 to i64 
  %__r_23 = ptrtoint i64* %__r_21 to i64 
  %__r_24 = add   i64 %__r_22, %__r_23 
  %__e24 = inttoptr i64 %__r_24 to i64* 
  %__r_26 = bitcast i64* %__e24 to i64** 
  %addr25 = getelementptr  i64*, i64** %__r_26, i64 0 
  %reg225 = load  i64*, i64** %addr25 
  %fun27 = bitcast i64* %reg225 to i64* (i64*, i64*)* 
  %reg224 =  call ccc  i64*  %fun27(i64*  %__e24, i64*  %__a14)  
  %__r_28 = inttoptr i64 0 to i64* 
  %__r_29 = ptrtoint i64* %reg224 to i64 
  %__r_30 = ptrtoint i64* %__r_28 to i64 
  %__r_31 = add   i64 %__r_29, %__r_30 
  %__e25 = inttoptr i64 %__r_31 to i64* 
  %__r_33 = bitcast i64* %__e25 to i64** 
  %addr32 = getelementptr  i64*, i64** %__r_33, i64 0 
  %reg227 = load  i64*, i64** %addr32 
  %__r_34 = inttoptr i64 1 to i64* 
  %__r_35 = inttoptr i64 0 to i64* 
  %__r_36 = ptrtoint i64* %__r_34 to i64 
  %__r_37 = ptrtoint i64* %__r_35 to i64 
  %__r_38 = add   i64 %__r_36, %__r_37 
  %reg229 = inttoptr i64 %__r_38 to i64* 
  %__r_39 = ptrtoint i64* %__b18 to i64 
  %__r_40 = ptrtoint i64* %reg229 to i64 
  %__r_41 = sub   i64 %__r_39, %__r_40 
  %__r_42 = icmp slt i64 0, %__r_41 
  %__r_43 = zext i1 %__r_42 to i64  
  %__r_44 = mul   i64 %__r_41, %__r_43 
  %reg228 = inttoptr i64 %__r_44 to i64* 
  %fun45 = bitcast i64* %reg227 to i64* (i64*, i64*)* 
  %reg226 =  call ccc  i64*  %fun45(i64*  %__e25, i64*  %reg228)  
  %__r_46 = inttoptr i64 0 to i64* 
  %__r_47 = ptrtoint i64* %reg226 to i64 
  %__r_48 = ptrtoint i64* %__r_46 to i64 
  %__r_49 = add   i64 %__r_47, %__r_48 
  %__e28 = inttoptr i64 %__r_49 to i64* 
  %__r_51 = bitcast i64* %__e28 to i64** 
  %addr50 = getelementptr  i64*, i64** %__r_51, i64 0 
  %reg231 = load  i64*, i64** %addr50 
  %__r_52 = load  i64*, i64** @multC 
  %__r_53 = inttoptr i64 0 to i64* 
  %__r_54 = ptrtoint i64* %__r_52 to i64 
  %__r_55 = ptrtoint i64* %__r_53 to i64 
  %__r_56 = add   i64 %__r_54, %__r_55 
  %__e26 = inttoptr i64 %__r_56 to i64* 
  %__r_58 = bitcast i64* %__e26 to i64** 
  %addr57 = getelementptr  i64*, i64** %__r_58, i64 0 
  %reg233 = load  i64*, i64** %addr57 
  %fun59 = bitcast i64* %reg233 to i64* (i64*, i64*)* 
  %reg232 =  call ccc  i64*  %fun59(i64*  %__e26, i64*  %__a14)  
  %__r_60 = inttoptr i64 0 to i64* 
  %__r_61 = ptrtoint i64* %reg232 to i64 
  %__r_62 = ptrtoint i64* %__r_60 to i64 
  %__r_63 = add   i64 %__r_61, %__r_62 
  %__e27 = inttoptr i64 %__r_63 to i64* 
  %__r_65 = bitcast i64* %__e27 to i64** 
  %addr64 = getelementptr  i64*, i64** %__r_65, i64 0 
  %reg235 = load  i64*, i64** %addr64 
  %fun66 = bitcast i64* %reg235 to i64* (i64*, i64*)* 
  %reg234 =  call ccc  i64*  %fun66(i64*  %__e27, i64*  %__ac21)  
  %fun67 = bitcast i64* %reg231 to i64* (i64*, i64*)* 
  %reg230 =  call ccc  i64*  %fun67(i64*  %__e28, i64*  %reg234)  
  br label %ifcont223 
ifcont223:
  %regcont236 = phi i64* [%__ac21, %then221], [%reg230, %else222] 
  ret i64* %regcont236 
}


define external ccc  i64* @__6(i64*  %__clo7, i64*  %__a5)    {
__6:
  %reg237 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__9, i64  1, i64*  %__a5)  
  ret i64* %reg237 
}


define external ccc  i64* @__9(i64*  %__clo10, i64*  %__b8)    {
__9:
  %__r_2 = bitcast i64* %__clo10 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg238 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg238 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__a5 = inttoptr i64 %__r_6 to i64* 
  %__r_7 = load  i64*, i64** @expC2 
  %__r_8 = inttoptr i64 0 to i64* 
  %__r_9 = ptrtoint i64* %__r_7 to i64 
  %__r_10 = ptrtoint i64* %__r_8 to i64 
  %__r_11 = add   i64 %__r_9, %__r_10 
  %__e11 = inttoptr i64 %__r_11 to i64* 
  %__r_13 = bitcast i64* %__e11 to i64** 
  %addr12 = getelementptr  i64*, i64** %__r_13, i64 0 
  %reg240 = load  i64*, i64** %addr12 
  %fun14 = bitcast i64* %reg240 to i64* (i64*, i64*)* 
  %reg239 =  call ccc  i64*  %fun14(i64*  %__e11, i64*  %__a5)  
  %__r_15 = inttoptr i64 0 to i64* 
  %__r_16 = ptrtoint i64* %reg239 to i64 
  %__r_17 = ptrtoint i64* %__r_15 to i64 
  %__r_18 = add   i64 %__r_16, %__r_17 
  %__e12 = inttoptr i64 %__r_18 to i64* 
  %__r_20 = bitcast i64* %__e12 to i64** 
  %addr19 = getelementptr  i64*, i64** %__r_20, i64 0 
  %reg242 = load  i64*, i64** %addr19 
  %fun21 = bitcast i64* %reg242 to i64* (i64*, i64*)* 
  %reg241 =  call ccc  i64*  %fun21(i64*  %__e12, i64*  %__b8)  
  %__r_22 = inttoptr i64 0 to i64* 
  %__r_23 = ptrtoint i64* %reg241 to i64 
  %__r_24 = ptrtoint i64* %__r_22 to i64 
  %__r_25 = add   i64 %__r_23, %__r_24 
  %__e13 = inttoptr i64 %__r_25 to i64* 
  %__r_27 = bitcast i64* %__e13 to i64** 
  %addr26 = getelementptr  i64*, i64** %__r_27, i64 0 
  %reg244 = load  i64*, i64** %addr26 
  %__r_28 = inttoptr i64 1 to i64* 
  %__r_29 = inttoptr i64 0 to i64* 
  %__r_30 = ptrtoint i64* %__r_28 to i64 
  %__r_31 = ptrtoint i64* %__r_29 to i64 
  %__r_32 = add   i64 %__r_30, %__r_31 
  %reg245 = inttoptr i64 %__r_32 to i64* 
  %fun33 = bitcast i64* %reg244 to i64* (i64*, i64*)* 
  %reg243 =  call ccc  i64*  %fun33(i64*  %__e13, i64*  %reg245)  
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


@x2 = internal   global i64* zeroinitializer


@x3 = internal   global i64* zeroinitializer


@x4 = internal   global i64* zeroinitializer


define external ccc  i64* @pcfmain()    {
pcfmain:
  %reg246 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__136, i64  0)  
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %reg246 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__r_5 = inttoptr i64 %__r_4 to i64* 
  store  i64* %__r_5, i64** @suman 
  %reg247 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__133, i64  0)  
  %__r_6 = inttoptr i64 0 to i64* 
  %__r_7 = ptrtoint i64* %reg247 to i64 
  %__r_8 = ptrtoint i64* %__r_6 to i64 
  %__r_9 = add   i64 %__r_7, %__r_8 
  %__r_10 = inttoptr i64 %__r_9 to i64* 
  store  i64* %__r_10, i64** @doble 
  %reg248 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__123, i64  0)  
  %__r_11 = inttoptr i64 0 to i64* 
  %__r_12 = ptrtoint i64* %reg248 to i64 
  %__r_13 = ptrtoint i64* %__r_11 to i64 
  %__r_14 = add   i64 %__r_12, %__r_13 
  %__r_15 = inttoptr i64 %__r_14 to i64* 
  store  i64* %__r_15, i64** @sumard 
  %reg249 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__117, i64  0)  
  %__r_16 = inttoptr i64 0 to i64* 
  %__r_17 = ptrtoint i64* %reg249 to i64 
  %__r_18 = ptrtoint i64* %__r_16 to i64 
  %__r_19 = add   i64 %__r_17, %__r_18 
  %__r_20 = inttoptr i64 %__r_19 to i64* 
  store  i64* %__r_20, i64** @fib 
  %reg250 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__108, i64  0)  
  %__r_21 = inttoptr i64 0 to i64* 
  %__r_22 = ptrtoint i64* %reg250 to i64 
  %__r_23 = ptrtoint i64* %__r_21 to i64 
  %__r_24 = add   i64 %__r_22, %__r_23 
  %__r_25 = inttoptr i64 %__r_24 to i64* 
  store  i64* %__r_25, i64** @resta 
  %reg251 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__99, i64  0)  
  %__r_26 = inttoptr i64 0 to i64* 
  %__r_27 = ptrtoint i64* %reg251 to i64 
  %__r_28 = ptrtoint i64* %__r_26 to i64 
  %__r_29 = add   i64 %__r_27, %__r_28 
  %__r_30 = inttoptr i64 %__r_29 to i64* 
  store  i64* %__r_30, i64** @mult 
  %reg252 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__88, i64  0)  
  %__r_31 = inttoptr i64 0 to i64* 
  %__r_32 = ptrtoint i64* %reg252 to i64 
  %__r_33 = ptrtoint i64* %__r_31 to i64 
  %__r_34 = add   i64 %__r_32, %__r_33 
  %__r_35 = inttoptr i64 %__r_34 to i64* 
  store  i64* %__r_35, i64** @exp 
  %reg253 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__81, i64  0)  
  %__r_36 = inttoptr i64 0 to i64* 
  %__r_37 = ptrtoint i64* %reg253 to i64 
  %__r_38 = ptrtoint i64* %__r_36 to i64 
  %__r_39 = add   i64 %__r_37, %__r_38 
  %__r_40 = inttoptr i64 %__r_39 to i64* 
  store  i64* %__r_40, i64** @fact 
  %reg254 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__70, i64  0)  
  %__r_41 = inttoptr i64 0 to i64* 
  %__r_42 = ptrtoint i64* %reg254 to i64 
  %__r_43 = ptrtoint i64* %__r_41 to i64 
  %__r_44 = add   i64 %__r_42, %__r_43 
  %__r_45 = inttoptr i64 %__r_44 to i64* 
  store  i64* %__r_45, i64** @gcd 
  %reg255 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__53, i64  0)  
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
  %__e51 = inttoptr i64 %__r_79 to i64* 
  %__r_81 = bitcast i64* %__e51 to i64** 
  %addr80 = getelementptr  i64*, i64** %__r_81, i64 0 
  %reg260 = load  i64*, i64** %addr80 
  %fun82 = bitcast i64* %reg260 to i64* (i64*, i64*)* 
  %__r_83 = load  i64*, i64** @y 
  %reg259 =  call ccc  i64*  %fun82(i64*  %__e51, i64*  %__r_83)  
  %__r_84 = inttoptr i64 0 to i64* 
  %__r_85 = ptrtoint i64* %reg259 to i64 
  %__r_86 = ptrtoint i64* %__r_84 to i64 
  %__r_87 = add   i64 %__r_85, %__r_86 
  %__r_88 = inttoptr i64 %__r_87 to i64* 
  store  i64* %__r_88, i64** @z 
  %reg261 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__39, i64  0)  
  %__r_89 = inttoptr i64 0 to i64* 
  %__r_90 = ptrtoint i64* %reg261 to i64 
  %__r_91 = ptrtoint i64* %__r_89 to i64 
  %__r_92 = add   i64 %__r_90, %__r_91 
  %__r_93 = inttoptr i64 %__r_92 to i64* 
  store  i64* %__r_93, i64** @multC2 
  %reg262 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__30, i64  0)  
  %__r_94 = inttoptr i64 0 to i64* 
  %__r_95 = ptrtoint i64* %reg262 to i64 
  %__r_96 = ptrtoint i64* %__r_94 to i64 
  %__r_97 = add   i64 %__r_95, %__r_96 
  %__r_98 = inttoptr i64 %__r_97 to i64* 
  store  i64* %__r_98, i64** @multC 
  %reg263 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__15, i64  0)  
  %__r_99 = inttoptr i64 0 to i64* 
  %__r_100 = ptrtoint i64* %reg263 to i64 
  %__r_101 = ptrtoint i64* %__r_99 to i64 
  %__r_102 = add   i64 %__r_100, %__r_101 
  %__r_103 = inttoptr i64 %__r_102 to i64* 
  store  i64* %__r_103, i64** @expC2 
  %reg264 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__6, i64  0)  
  %__r_104 = inttoptr i64 0 to i64* 
  %__r_105 = ptrtoint i64* %reg264 to i64 
  %__r_106 = ptrtoint i64* %__r_104 to i64 
  %__r_107 = add   i64 %__r_105, %__r_106 
  %__r_108 = inttoptr i64 %__r_107 to i64* 
  store  i64* %__r_108, i64** @expC 
  %__r_109 = load  i64*, i64** @gcd 
  %__r_110 = inttoptr i64 0 to i64* 
  %__r_111 = ptrtoint i64* %__r_109 to i64 
  %__r_112 = ptrtoint i64* %__r_110 to i64 
  %__r_113 = add   i64 %__r_111, %__r_112 
  %__e3 = inttoptr i64 %__r_113 to i64* 
  %__r_115 = bitcast i64* %__e3 to i64** 
  %addr114 = getelementptr  i64*, i64** %__r_115, i64 0 
  %reg266 = load  i64*, i64** %addr114 
  %__r_116 = inttoptr i64 12 to i64* 
  %__r_117 = inttoptr i64 0 to i64* 
  %__r_118 = ptrtoint i64* %__r_116 to i64 
  %__r_119 = ptrtoint i64* %__r_117 to i64 
  %__r_120 = add   i64 %__r_118, %__r_119 
  %reg267 = inttoptr i64 %__r_120 to i64* 
  %fun121 = bitcast i64* %reg266 to i64* (i64*, i64*)* 
  %reg265 =  call ccc  i64*  %fun121(i64*  %__e3, i64*  %reg267)  
  %__r_122 = inttoptr i64 0 to i64* 
  %__r_123 = ptrtoint i64* %reg265 to i64 
  %__r_124 = ptrtoint i64* %__r_122 to i64 
  %__r_125 = add   i64 %__r_123, %__r_124 
  %__e4 = inttoptr i64 %__r_125 to i64* 
  %__r_127 = bitcast i64* %__e4 to i64** 
  %addr126 = getelementptr  i64*, i64** %__r_127, i64 0 
  %reg269 = load  i64*, i64** %addr126 
  %__r_128 = inttoptr i64 144 to i64* 
  %__r_129 = inttoptr i64 0 to i64* 
  %__r_130 = ptrtoint i64* %__r_128 to i64 
  %__r_131 = ptrtoint i64* %__r_129 to i64 
  %__r_132 = add   i64 %__r_130, %__r_131 
  %reg270 = inttoptr i64 %__r_132 to i64* 
  %fun133 = bitcast i64* %reg269 to i64* (i64*, i64*)* 
  %reg268 =  call ccc  i64*  %fun133(i64*  %__e4, i64*  %reg270)  
  %__r_134 = inttoptr i64 0 to i64* 
  %__r_135 = ptrtoint i64* %reg268 to i64 
  %__r_136 = ptrtoint i64* %__r_134 to i64 
  %__r_137 = add   i64 %__r_135, %__r_136 
  %__r_138 = inttoptr i64 %__r_137 to i64* 
  store  i64* %__r_138, i64** @x2 
  %__r_139 = load  i64*, i64** @gcd 
  %__r_140 = inttoptr i64 0 to i64* 
  %__r_141 = ptrtoint i64* %__r_139 to i64 
  %__r_142 = ptrtoint i64* %__r_140 to i64 
  %__r_143 = add   i64 %__r_141, %__r_142 
  %__e1 = inttoptr i64 %__r_143 to i64* 
  %__r_145 = bitcast i64* %__e1 to i64** 
  %addr144 = getelementptr  i64*, i64** %__r_145, i64 0 
  %reg272 = load  i64*, i64** %addr144 
  %__r_146 = inttoptr i64 18 to i64* 
  %__r_147 = inttoptr i64 0 to i64* 
  %__r_148 = ptrtoint i64* %__r_146 to i64 
  %__r_149 = ptrtoint i64* %__r_147 to i64 
  %__r_150 = add   i64 %__r_148, %__r_149 
  %reg273 = inttoptr i64 %__r_150 to i64* 
  %fun151 = bitcast i64* %reg272 to i64* (i64*, i64*)* 
  %reg271 =  call ccc  i64*  %fun151(i64*  %__e1, i64*  %reg273)  
  %__r_152 = inttoptr i64 0 to i64* 
  %__r_153 = ptrtoint i64* %reg271 to i64 
  %__r_154 = ptrtoint i64* %__r_152 to i64 
  %__r_155 = add   i64 %__r_153, %__r_154 
  %__e2 = inttoptr i64 %__r_155 to i64* 
  %__r_157 = bitcast i64* %__e2 to i64** 
  %addr156 = getelementptr  i64*, i64** %__r_157, i64 0 
  %reg275 = load  i64*, i64** %addr156 
  %__r_158 = inttoptr i64 81 to i64* 
  %__r_159 = inttoptr i64 0 to i64* 
  %__r_160 = ptrtoint i64* %__r_158 to i64 
  %__r_161 = ptrtoint i64* %__r_159 to i64 
  %__r_162 = add   i64 %__r_160, %__r_161 
  %reg276 = inttoptr i64 %__r_162 to i64* 
  %fun163 = bitcast i64* %reg275 to i64* (i64*, i64*)* 
  %reg274 =  call ccc  i64*  %fun163(i64*  %__e2, i64*  %reg276)  
  %__r_164 = inttoptr i64 0 to i64* 
  %__r_165 = ptrtoint i64* %reg274 to i64 
  %__r_166 = ptrtoint i64* %__r_164 to i64 
  %__r_167 = add   i64 %__r_165, %__r_166 
  %__r_168 = inttoptr i64 %__r_167 to i64* 
  store  i64* %__r_168, i64** @x3 
  %__r_169 = inttoptr i64 343 to i64* 
  %__r_170 = inttoptr i64 0 to i64* 
  %__r_171 = ptrtoint i64* %__r_169 to i64 
  %__r_172 = ptrtoint i64* %__r_170 to i64 
  %__r_173 = add   i64 %__r_171, %__r_172 
  %reg277 = inttoptr i64 %__r_173 to i64* 
  %__r_174 = inttoptr i64 0 to i64* 
  %__r_175 = ptrtoint i64* %reg277 to i64 
  %__r_176 = ptrtoint i64* %__r_174 to i64 
  %__r_177 = add   i64 %__r_175, %__r_176 
  %__x20 = inttoptr i64 %__r_177 to i64* 
  %__r_178 = ptrtoint i64* %__x20 to i64 
  %__r_179 =  call ccc  i64  @pcf_print(i64  %__r_178)  
  %reg278 = inttoptr i64 %__r_179 to i64* 
  %__r_180 = inttoptr i64 0 to i64* 
  %__r_181 = ptrtoint i64* %reg278 to i64 
  %__r_182 = ptrtoint i64* %__r_180 to i64 
  %__r_183 = add   i64 %__r_181, %__r_182 
  %__r_184 = inttoptr i64 %__r_183 to i64* 
  store  i64* %__r_184, i64** @x4 
  ret i64* %__x20 
}