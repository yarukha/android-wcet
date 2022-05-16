open Dvk


let catch i = 
  let l = String.split_on_char ' ' i in 
  let s = List.hd l in 
  let pos = int_of_string ("0x"^(String.sub s 0 4)) in 
  let q = List.tl l in 
  let args = List.tl q and instr = List.hd q in 
  let op = 
  match instr with 
  |"nop" -> Op0(Nop,Empty)
  |"move" -> Op1(Move(Empty),Empty)
  |"move/from16" -> Op1(Move(Empty),From16)
  |"move/16" -> Op1(Move(Empty),X16)
  |"move-wide" -> Op1(Move(Wide),Empty)
  |"move-wide/from16" -> Op1(Move(Wide),From16)
  |"move-wide/16" -> Op1(Move(Wide),X16)
  |"move-object" -> Op1(Move(Object),Empty)
  |"move-object/from16" -> Op1(Move(Object),From16)
  |"move-object/16" -> Op1(Move(Object),X16)
  |"move-result" -> Op1(MoveResult(Empty),Empty)
  |"move-result-wide" -> Op1(MoveResult(Wide),Empty)
  |"move-result-object" -> Op1(MoveResult(Object),Empty)
  |"move-exception" -> Op1(MoveException,Empty)
  |"return-void" -> Op0 (ReturnVoid,Empty)
  |"return" -> Op1(Return(Empty),Empty)
  |"return-wide" -> Op1(Return(Wide),Empty)
  |"return-object" -> Op1(Return(Empty),Empty)
  |"const/4" -> Op2(Const(Empty),X4)
  |"const/16" -> Op2(Const(Empty),X16)
  |"const" -> Op2(Const(Empty),Empty)
  |"const/high16" -> Op2(Const(Empty),High16)
  |"const-wide/16" -> Op2(Const(Wide),X16)
  |"const-wide/32" -> Op2(Const(Wide),X32)
  |"const-wide" -> Op2(Const(Wide),Empty)
  |"const-wide/high16" -> Op2(Const(Wide),High16)
  |"const-string" -> Op2(Const(String),Empty)
  |"const-string/jumbo" -> Op2(Const(String),Jumbo)
  |"const-class" -> Op2(Const(Class),Empty)
  |"monitor-enter" -> Op1(MoniterEnter,Empty)
  |"monitor-exit" -> Op1(MoniterExit,Empty)
  |"check-cast" -> Op2(CheckCast,Empty)
  |"instance-of" -> Op3(InstanceOf,Empty)
  |"array-length" -> Op2(ArrayLength,Empty)
  |"new-instance" -> Op2(NewInstance,Empty)
  |"new-array" -> Op3(NewArray,Empty)
  |"filled-new-array" -> Opn(FilledNewArray,Empty)
  |"filled-new-array/range" ->Opn(FilledNewArray,Range)
  |"fill-array-data" -> Opn(FillArrayData,Empty)
  |"throw" -> Op1(Throw,Empty)
  |"goto" -> Op1(Goto,Empty)
  |"goto/16" -> Op1(Goto,X16)
  |"goto/32"  -> Op1(Goto,X32)
  |"packed-switch-data" -> Opn(PackedSwitchData,Empty)
  |"packed-switch" -> Opn(PackedSwitch,Empty)
  |"sparse-switch-data" -> Opn(SparseSwitchData,Empty)
  |"sparse-switch" -> Opn(SparseSwitch,Empty)
  |"cmpl-float" -> Op3(Cmpl(Float),Empty)
  |"cmpg-float" -> Op3(Cmpg(Float),Empty)
  |"cmpl-double" -> Op3(Cmpl(Double),Empty)
  |"cmpg-double" -> Op3(Cmpg(Double),Empty)
  |"cmp-long" -> Op3(CmpLong,Empty)
  |"if-eq" -> Op3(If(Eq),Empty)
  |"if-ne" -> Op3(If(Ne),Empty)
  |"if-lt" -> Op3(If(Lt),Empty)
  |"if-ge" -> Op3(If(Ge),Empty)
  |"if-gt" -> Op3(If(Gt),Empty)
  |"if-le" -> Op3(If(Le),Empty)
  |"if-eqz" -> Op2(Ifz(Eq),Empty)
  |"if-nez" -> Op2(Ifz(Ne),Empty)
  |"if-ltz" -> Op2(Ifz(Lt),Empty)
  |"if-gez" -> Op2(Ifz(Ge),Empty)
  |"if-gtz" -> Op2(Ifz(Gt),Empty)
  |"if-lez" -> Op2(Ifz(Le),Empty)
  |"aget" -> Op3(Aget(Empty),Empty)
  |"aget-wide" -> Op3(Aget(Wide),Empty)
  |"aget-object" -> Op3(Aget(Object),Empty)
  |"aget-boolean" -> Op3(Aget(Boolean),Empty)
  |"aget-byte" -> Op3(Aget(Byte),Empty)
  |"aget-char" -> Op3(Aget(Char),Empty)
  |"aget-short" -> Op3(Aget(Short),Empty)
  |"aput" -> Op3(Aput(Empty),Empty)
  |"aput-wide" -> Op3(Aput(Wide),Empty)
  |"aput-object" -> Op3(Aput(Object),Empty)
  |"aput-boolean" -> Op3(Aput(Boolean),Empty)
  |"aput-byte" -> Op3(Aput(Byte),Empty)
  |"aput-char" -> Op3(Aput(Char),Empty)
  |"aput-short" -> Op3(Aput(Short),Empty)
  |"iget" -> Op3(Iget(Empty),Empty)
  |"iget-wide" -> Op3(Iget(Wide),Empty)
  |"iget-object" -> Op3(Iget(Object),Empty)
  |"iget-boolean" -> Op3(Iget(Boolean),Empty)
  |"iget-byte" -> Op3(Iget(Byte),Empty)
  |"iget-char" -> Op3(Iget(Char),Empty)
  |"iget-short" -> Op3(Iget(Short),Empty)
  |"iput" -> Op3(Iput(Empty),Empty)
  |"iput-wide" -> Op3(Iput(Wide),Empty)
  |"iput-object" -> Op3(Iput(Object),Empty)
  |"iput-boolean" -> Op3(Iput(Boolean),Empty)
  |"iput-byte" -> Op3(Iput(Byte),Empty)
  |"iput-char" -> Op3(Iput(Char),Empty)
  |"iput-short" -> Op3(Iput(Short),Empty)
  |"sget" -> Op2(Sget(Empty),Empty)
  |"sget-wide" -> Op2(Sget(Wide),Empty)
  |"sget-object" -> Op2(Sget(Object),Empty)
  |"sget-boolean" -> Op2(Sget(Boolean),Empty)
  |"sget-byte" -> Op2(Sget(Boolean),Empty)
  |"sget-char" -> Op2(Sget(Char),Empty)
  |"sget-short" -> Op2(Sget(Short),Empty)
  |"sput" -> Op2(Sput(Empty),Empty)
  |"sput-wide" -> Op2(Sput(Wide),Empty)
  |"sput-object" -> Op2(Sput(Object),Empty)
  |"sput-boolean" -> Op2(Sput(Boolean),Empty)
  |"sput-byte" -> Op2(Sput(Byte),Empty)
  |"sput-char" -> Op2(Sput(Char),Empty)
  |"sput-short" -> Op2(Sput(Short),Empty)
  |"invoke-virtual" -> Opn(Invoke(Virtual),Empty)
  |"invoke-super" -> Opn(Invoke(Super),Empty)
  |"invoke-direct" -> Opn(Invoke(Direct),Empty)
  |"invoke-static" -> Opn(Invoke(Static),Empty)
  |"invoke-interface" -> Opn(Invoke(Interface),Empty)
  |"invoke-virtual/range" -> Opn(Invoke(Virtual),Range)
  |"invoke-super/range" -> Opn(Invoke(Super),Range)
  |"invoke-direct/range" -> Opn(Invoke(Direct),Range)
  |"invoke-static/range" -> Opn(Invoke(Static),Range)
  |"invoke-interface/range" -> Opn(Invoke(Interface),Range)
  |"invoke-polymorphic"  -> Opn(Invoke(Polymorphic),Empty)
  |"invoke-polymorphic/range" -> Opn(Invoke(Polymorphic),Range)
  |"invoke-custom" -> Opn(Invoke(Custom),Empty)
  |"invoke-custom/range" -> Opn(Invoke(Custom),Range)
  |"neg-int" -> Op2(Neg(Int),Empty)
  |"not-int" -> Op2(Not(Int),Empty)
  |"neg-long" -> Op2(Neg(Long),Empty)
  |"not-long" -> Op2(Not(Long),Empty)
  |"neg-float" -> Op2(Neg(Float),Empty)
  |"not-float" -> Op2(Not(Float),Empty)
  |"neg-double" -> Op2(Neg(Double),Empty)
  |"int-to-long" -> Op2(To(Int,Long),Empty)
  |"int-to-float" -> Op2(To(Int,Float),Empty)
  |"int-to-double" -> Op2(To(Int,Double),Empty)
  |"long-to-int" -> Op2(To(Long,Int),Empty)
  |"long-to-float" -> Op2(To(Long,Float),Empty)
  |"long-to-double" -> Op2(To(Long,Double),Empty)
  |"float-to-int" -> Op2(To(Float,Int),Empty)
  |"float-to-long" -> Op2(To(Float,Long),Empty)
  |"float-to-double" -> Op2(To(Float,Double),Empty)
  |"double-to-int" -> Op2(To(Double,Int),Empty)
  |"double-to-long" -> Op2(To(Double,Long),Empty)
  |"double-to-float" -> Op2(To(Double,Float),Empty)
  |"int-to-byte" -> Op2(To(Int,Byte),Empty)
  |"int-to-char" -> Op2(To(Int,Char),Empty)
  |"int-to-short" -> Op2(To(Int,Short),Empty)
  |"add-int" -> Op3(Arithm(Add,Int),Empty)
  |"sub-int" -> Op3(Arithm(Sub,Int),Empty)
  |"mul-int" -> Op3(Arithm(Mul,Int),Empty)
  |"div-int" -> Op3(Arithm(Div,Int),Empty)
  |"rem-int" -> Op3(Arithm(Rem,Int),Empty)
  |"and-int" -> Op3(Arithm(And,Int),Empty)
  |"or-int" -> Op3(Arithm(Or,Int),Empty)
  |"xor-int" -> Op3(Arithm(Xor,Int),Empty)
  |"shl-int" -> Op3(Arithm(Shl,Int),Empty)
  |"shr-int" -> Op3(Arithm(Shr,Int),Empty)
  |"ushr-int" -> Op3(Arithm(Ushr,Int),Empty)
  |"add-long" -> Op3(Arithm(Add,Long),Empty)
  |"sub-long" -> Op3(Arithm(Sub,Long),Empty)
  |"mul-long" -> Op3(Arithm(Mul,Long),Empty)
  |"div-long" -> Op3(Arithm(Div,Long),Empty)
  |"rem-long" -> Op3(Arithm(Rem,Long),Empty)
  |"and-long" -> Op3(Arithm(And,Long),Empty)
  |"or-long" -> Op3(Arithm(Or,Long),Empty)
  |"xor-long" -> Op3(Arithm(Xor,Long),Empty)
  |"shl-long" -> Op3(Arithm(Shl,Long),Empty)
  |"shr-long" -> Op3(Arithm(Shr,Long),Empty)
  |"ushr-long" -> Op3(Arithm(Ushr,Long),Empty)
  |"add-float" -> Op3(Arithm(Add,Float),Empty)
  |"sub-float" -> Op3(Arithm(Sub,Float),Empty)
  |"mul-float" -> Op3(Arithm(Mul,Float),Empty)
  |"div-float" -> Op3(Arithm(Div,Float),Empty)
  |"rem-float" -> Op3(Arithm(Rem,Float),Empty)
  |"add-double" -> Op3(Arithm(Add,Double),Empty)
  |"sub-double" -> Op3(Arithm(Sub,Double),Empty)
  |"mul-double" -> Op3(Arithm(Mul,Double),Empty)
  |"div-double" -> Op3(Arithm(Div,Double),Empty)
  |"rem-double" -> Op3(Arithm(Rem,Double),Empty) 
  |"add-int/2addr" -> Op3(Arithm(Add,Int),X2Addr)
  |"sub-int/2addr" -> Op3(Arithm(Sub,Int),X2Addr)
  |"mul-int/2addr" -> Op3(Arithm(Mul,Int),X2Addr)
  |"div-int/2addr" -> Op3(Arithm(Div,Int),X2Addr)
  |"rem-int/2addr" -> Op3(Arithm(Rem,Int),X2Addr)
  |"and-int/2addr" -> Op3(Arithm(And,Int),X2Addr)
  |"or-int/2addr" -> Op3(Arithm(Or,Int),X2Addr)
  |"xor-int/2addr" -> Op3(Arithm(Xor,Int),X2Addr)
  |"shl-int/2addr" -> Op3(Arithm(Shl,Int),X2Addr)
  |"shr-int/2addr" -> Op3(Arithm(Shr,Int),X2Addr)
  |"ushr-int/2addr" -> Op3(Arithm(Ushr,Int),X2Addr)
  |"add-long/2addr" -> Op3(Arithm(Add,Long),X2Addr)
  |"sub-long/2addr" -> Op3(Arithm(Sub,Long),X2Addr)
  |"mul-long/2addr" -> Op3(Arithm(Mul,Long),X2Addr)
  |"div-long/2addr" -> Op3(Arithm(Div,Long),X2Addr)
  |"rem-long/2addr" -> Op3(Arithm(Rem,Long),X2Addr)
  |"and-long/2addr" -> Op3(Arithm(And,Long),X2Addr)
  |"or-long/2addr" -> Op3(Arithm(Or,Long),X2Addr)
  |"xor-long/2addr" -> Op3(Arithm(Xor,Long),X2Addr)
  |"shl-long/2addr" -> Op3(Arithm(Shl,Long),X2Addr)
  |"shr-long/2addr" -> Op3(Arithm(Shr,Long),X2Addr)
  |"ushr-long/2addr" -> Op3(Arithm(Ushr,Long),X2Addr)
  |"add-float/2addr" -> Op3(Arithm(Add,Float),X2Addr)
  |"sub-float/2addr" -> Op3(Arithm(Sub,Float),X2Addr)
  |"mul-float/2addr" -> Op3(Arithm(Mul,Float),X2Addr)
  |"div-float/2addr" -> Op3(Arithm(Div,Float),X2Addr)
  |"rem-float/2addr" -> Op3(Arithm(Rem,Float),X2Addr)
  |"add-double/2addr" -> Op3(Arithm(Add,Double),X2Addr)
  |"sub-double/2addr" -> Op3(Arithm(Sub,Double),X2Addr)
  |"mul-double/2addr" -> Op3(Arithm(Mul,Double),X2Addr)
  |"div-double/2addr" -> Op3(Arithm(Div,Double),X2Addr)
  |"rem-double/2addr" -> Op3(Arithm(Rem,Double),X2Addr)
  |"add-int/lit16" -> Op3(Arithm(Add,Int),Lit16)
  |"rsub-int" -> Op3(Arithm(Sub,Int),Lit16)
  |"mul-int/lit16" -> Op3(Arithm(Mul,Int),Lit16)
  |"div-int/lit16" -> Op3(Arithm(Div,Int),Lit16)
  |"rem-int/lit16" -> Op3(Arithm(Rem,Int),Lit16)
  |"and-int/lit16" -> Op3(Arithm(And,Int),Lit16)
  |"or-int/lit16" -> Op3(Arithm(Or,Int),Lit16)
  |"xor-int/lit16" -> Op3(Arithm(Xor,Int),Lit16)
  |"add-int/lit8" -> Op3(Arithm(Add,Int),Lit8)
  |"rsub-int/lit8" -> Op3(Arithm(Sub,Int),Lit8)
  |"mul-int/lit8" -> Op3(Arithm(Mul,Int),Lit8)
  |"div-int/lit8" -> Op3(Arithm(Div,Int),Lit8)
  |"rem-int/lit8" -> Op3(Arithm(Rem,Int),Lit8)
  |"and-int/lit8" -> Op3(Arithm(And,Int),Lit8)
  |"or-int/lit8" -> Op3(Arithm(Or,Int),Lit8)
  |"xor-int/lit8" -> Op3(Arithm(Xor,Int),Lit8)
  |"shl-int/lit8" -> Op3(Arithm(Shl,Int),Lit8)
  |"shr-int/lit8" -> Op3(Arithm(Shr,Int),Lit8)
  |"ushr-int/lit8" -> Op3(Arithm(Ushr,Int),Lit8)
  |"const-method-handle"
  |"const-method-type" -> raise(NotTranslated(instr))
  |"array-data" -> Opn(ArrayData,Empty)
  |_ -> raise(UnknownInstruction(instr))
  in {
    pc_pos = pos; 
    op = op;
    args = args
  }