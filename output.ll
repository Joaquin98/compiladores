; ModuleID = 'pcfprog'


 


declare external ccc  i64* @pcf_mkclosure(i64* (i64*, i64*)*, i64, ...)    


declare external ccc  i64 @pcf_print(i64)    


define external ccc  i64* @__3(i64*  %__clo4, i64*  %__n2)    {
__3:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %__clo4 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__gcd5 = inttoptr i64 %__r_4 to i64* 
  %reg0 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__7, i64  2, i64*  %__gcd5, i64*  %__n2)  
  ret i64* %reg0 
}


define external ccc  i64* @__7(i64*  %__clo8, i64*  %__m6)    {
__7:
  %__r_2 = bitcast i64* %__clo8 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %reg1 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %reg1 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__gcd5 = inttoptr i64 %__r_6 to i64* 
  %__r_8 = bitcast i64* %__clo8 to i64** 
  %addr7 = getelementptr  i64*, i64** %__r_8, i64 2 
  %reg2 = load  i64*, i64** %addr7 
  %__r_9 = inttoptr i64 0 to i64* 
  %__r_10 = ptrtoint i64* %reg2 to i64 
  %__r_11 = ptrtoint i64* %__r_9 to i64 
  %__r_12 = add   i64 %__r_10, %__r_11 
  %__n2 = inttoptr i64 %__r_12 to i64* 
  br label %entry3 
entry3:
  %__r_14 = inttoptr i64 0 to i64* 
  %cond13 = icmp eq i64* %__n2, %__r_14 
  br i1 %cond13, label %then4, label %else5 
then4:
  br label %ifcont6 
else5:
  br label %entry7 
entry7:
  %__r_16 = inttoptr i64 0 to i64* 
  %cond15 = icmp eq i64* %__m6, %__r_16 
  br i1 %cond15, label %then8, label %else9 
then8:
  br label %ifcont10 
else9:
  br label %entry11 
entry11:
  %__r_17 = ptrtoint i64* %__n2 to i64 
  %__r_18 = ptrtoint i64* %__m6 to i64 
  %__r_19 = sub   i64 %__r_17, %__r_18 
  %__r_20 = icmp slt i64 0, %__r_19 
  %__r_21 = zext i1 %__r_20 to i64  
  %__r_22 = mul   i64 %__r_19, %__r_21 
  %reg15 = inttoptr i64 %__r_22 to i64* 
  %__r_24 = inttoptr i64 0 to i64* 
  %cond23 = icmp eq i64* %reg15, %__r_24 
  br i1 %cond23, label %then12, label %else13 
then12:
  %__r_25 = inttoptr i64 0 to i64* 
  %__r_26 = ptrtoint i64* %__gcd5 to i64 
  %__r_27 = ptrtoint i64* %__r_25 to i64 
  %__r_28 = add   i64 %__r_26, %__r_27 
  %__e9 = inttoptr i64 %__r_28 to i64* 
  %__r_30 = bitcast i64* %__e9 to i64** 
  %addr29 = getelementptr  i64*, i64** %__r_30, i64 0 
  %reg17 = load  i64*, i64** %addr29 
  %__r_31 = ptrtoint i64* %__m6 to i64 
  %__r_32 = ptrtoint i64* %__n2 to i64 
  %__r_33 = sub   i64 %__r_31, %__r_32 
  %__r_34 = icmp slt i64 0, %__r_33 
  %__r_35 = zext i1 %__r_34 to i64  
  %__r_36 = mul   i64 %__r_33, %__r_35 
  %reg18 = inttoptr i64 %__r_36 to i64* 
  %fun37 = bitcast i64* %reg17 to i64* (i64*, i64*)* 
  %reg16 =  call ccc  i64*  %fun37(i64*  %__e9, i64*  %reg18)  
  %__r_38 = inttoptr i64 0 to i64* 
  %__r_39 = ptrtoint i64* %reg16 to i64 
  %__r_40 = ptrtoint i64* %__r_38 to i64 
  %__r_41 = add   i64 %__r_39, %__r_40 
  %__e10 = inttoptr i64 %__r_41 to i64* 
  %__r_43 = bitcast i64* %__e10 to i64** 
  %addr42 = getelementptr  i64*, i64** %__r_43, i64 0 
  %reg20 = load  i64*, i64** %addr42 
  %fun44 = bitcast i64* %reg20 to i64* (i64*, i64*)* 
  %reg19 =  call ccc  i64*  %fun44(i64*  %__e10, i64*  %__n2)  
  br label %ifcont14 
else13:
  %__r_45 = inttoptr i64 0 to i64* 
  %__r_46 = ptrtoint i64* %__gcd5 to i64 
  %__r_47 = ptrtoint i64* %__r_45 to i64 
  %__r_48 = add   i64 %__r_46, %__r_47 
  %__e11 = inttoptr i64 %__r_48 to i64* 
  %__r_50 = bitcast i64* %__e11 to i64** 
  %addr49 = getelementptr  i64*, i64** %__r_50, i64 0 
  %reg22 = load  i64*, i64** %addr49 
  %fun51 = bitcast i64* %reg22 to i64* (i64*, i64*)* 
  %reg21 =  call ccc  i64*  %fun51(i64*  %__e11, i64*  %__m6)  
  %__r_52 = inttoptr i64 0 to i64* 
  %__r_53 = ptrtoint i64* %reg21 to i64 
  %__r_54 = ptrtoint i64* %__r_52 to i64 
  %__r_55 = add   i64 %__r_53, %__r_54 
  %__e12 = inttoptr i64 %__r_55 to i64* 
  %__r_57 = bitcast i64* %__e12 to i64** 
  %addr56 = getelementptr  i64*, i64** %__r_57, i64 0 
  %reg24 = load  i64*, i64** %addr56 
  %__r_58 = ptrtoint i64* %__n2 to i64 
  %__r_59 = ptrtoint i64* %__m6 to i64 
  %__r_60 = sub   i64 %__r_58, %__r_59 
  %__r_61 = icmp slt i64 0, %__r_60 
  %__r_62 = zext i1 %__r_61 to i64  
  %__r_63 = mul   i64 %__r_60, %__r_62 
  %reg25 = inttoptr i64 %__r_63 to i64* 
  %fun64 = bitcast i64* %reg24 to i64* (i64*, i64*)* 
  %reg23 =  call ccc  i64*  %fun64(i64*  %__e12, i64*  %reg25)  
  br label %ifcont14 
ifcont14:
  %regcont26 = phi i64* [%reg19, %then12], [%reg23, %else13] 
  br label %ifcont10 
ifcont10:
  %regcont27 = phi i64* [%__n2, %then8], [%regcont26, %ifcont14] 
  br label %ifcont6 
ifcont6:
  %regcont28 = phi i64* [%__m6, %then4], [%regcont27, %ifcont10] 
  ret i64* %regcont28 
}


@gcd = internal   global i64* zeroinitializer


@x3 = internal   global i64* zeroinitializer


define external ccc  i64* @pcfmain()    {
pcfmain:
  %reg29 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__3, i64  0)  
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = ptrtoint i64* %reg29 to i64 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = add   i64 %__r_2, %__r_3 
  %__r_5 = inttoptr i64 %__r_4 to i64* 
  store  i64* %__r_5, i64** @gcd 
  %__r_6 = load  i64*, i64** @gcd 
  %__r_7 = inttoptr i64 0 to i64* 
  %__r_8 = ptrtoint i64* %__r_6 to i64 
  %__r_9 = ptrtoint i64* %__r_7 to i64 
  %__r_10 = add   i64 %__r_8, %__r_9 
  %__e0 = inttoptr i64 %__r_10 to i64* 
  %__r_12 = bitcast i64* %__e0 to i64** 
  %addr11 = getelementptr  i64*, i64** %__r_12, i64 0 
  %reg31 = load  i64*, i64** %addr11 
  %__r_13 = inttoptr i64 18 to i64* 
  %__r_14 = inttoptr i64 0 to i64* 
  %__r_15 = ptrtoint i64* %__r_13 to i64 
  %__r_16 = ptrtoint i64* %__r_14 to i64 
  %__r_17 = add   i64 %__r_15, %__r_16 
  %reg32 = inttoptr i64 %__r_17 to i64* 
  %fun18 = bitcast i64* %reg31 to i64* (i64*, i64*)* 
  %reg30 =  call ccc  i64*  %fun18(i64*  %__e0, i64*  %reg32)  
  %__r_19 = inttoptr i64 0 to i64* 
  %__r_20 = ptrtoint i64* %reg30 to i64 
  %__r_21 = ptrtoint i64* %__r_19 to i64 
  %__r_22 = add   i64 %__r_20, %__r_21 
  %__e1 = inttoptr i64 %__r_22 to i64* 
  %__r_24 = bitcast i64* %__e1 to i64** 
  %addr23 = getelementptr  i64*, i64** %__r_24, i64 0 
  %reg34 = load  i64*, i64** %addr23 
  %__r_25 = inttoptr i64 81 to i64* 
  %__r_26 = inttoptr i64 0 to i64* 
  %__r_27 = ptrtoint i64* %__r_25 to i64 
  %__r_28 = ptrtoint i64* %__r_26 to i64 
  %__r_29 = add   i64 %__r_27, %__r_28 
  %reg35 = inttoptr i64 %__r_29 to i64* 
  %fun30 = bitcast i64* %reg34 to i64* (i64*, i64*)* 
  %reg33 =  call ccc  i64*  %fun30(i64*  %__e1, i64*  %reg35)  
  %__r_31 = ptrtoint i64* %reg33 to i64 
  %__r_32 =  call ccc  i64  @pcf_print(i64  %__r_31)  
  %reg36 = inttoptr i64 %__r_32 to i64* 
  %__r_33 = inttoptr i64 0 to i64* 
  %__r_34 = ptrtoint i64* %reg36 to i64 
  %__r_35 = ptrtoint i64* %__r_33 to i64 
  %__r_36 = add   i64 %__r_34, %__r_35 
  %__r_37 = inttoptr i64 %__r_36 to i64* 
  store  i64* %__r_37, i64** @x3 
  ret i64* %reg33 
}