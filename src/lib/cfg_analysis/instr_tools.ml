open Instructions



let instr_to_string i = 
  match i.op with 
  |Undefined -> Printf.sprintf "undefined"
  |Opn(Invoke(_),_) -> Printf.sprintf "invoke"
  |Op0(ReturnVoid,_)->Printf.sprintf "returnvoid"
  |Op1(Return(_),_)->Printf.sprintf "return"
  |Op0(_)->Printf.sprintf "op0"
  |Op1(_)->Printf.sprintf "op1"
  |Op2(_)->Printf.sprintf "op2"
  |Op3(_)->Printf.sprintf "op3"
  |Opn(_)->Printf.sprintf "other opn"

let invoke_type_to_string = function 
  |Virtual -> "Virtual"
  |Super -> "Super"
  |Direct -> "Direct"
  |Static->"Static"
  |Interface -> "Interface"
  |Polymorphic -> "Polymorphic"
  |Custom -> "Custom"

let catch_args op i =
   match op with 
   (*only the "interesting" arguments are catched, namely methods name in invoke
  and all modified registers*)
  |Opn(Invoke(_),_)-> 
    let buf = Buffer.create 20 in 
    let add = ref false in 
    String.iter (
      fun c -> if !add && (c!='\\') then Buffer.add_char buf c; if ((c='}'&& not !add)) then add := true; if ((c='/')&& !add) then add := false
    ) i;
    let b = Buffer.contents buf in
    let s =String.sub (b) 2 (String.length b -4 )in
    [s]
  |_ ->
    Str.split (Str.regexp {|,? #?\(int \|long \)?\+?|}) i 
    |>List.tl


let catch p i = 
  let l = String.split_on_char ' ' i in 
  let instr =  List.hd l in 
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
  |"filled-new-array/range" |"filled-new-array/range,"  ->Opn(FilledNewArray,Range)
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
  |"invoke-virtual/range," -> Opn(Invoke(Virtual),Range)
  |"invoke-super/range," -> Opn(Invoke(Super),Range)
  |"invoke-direct/range," -> Opn(Invoke(Direct),Range)
  |"invoke-static/range," -> Opn(Invoke(Static),Range)
  |"invoke-interface/range," -> Opn(Invoke(Interface),Range)
  |"invoke-polymorphic"  -> Opn(Invoke(Polymorphic),Empty)
  |"invoke-polymorphic/range," -> Opn(Invoke(Polymorphic),Range)
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
  |"const-method-handle"-> Op2(ConstMethodHandle,Empty)
  |"const-method-type" -> Op2(ConstMethodType,Empty)
  |"array-data" -> Opn(ArrayData,Empty)
  |_ -> raise(UnknownInstruction(instr))
  in let args = catch_args op i 
  in
  {
    pc= Pc(p);
    op = op;
    args = args
  }



let number_instructions = 224

let instruction_id i =
  match i.op with                      
  |Op0(Nop,Empty) -> 0                           
  |Op1(Move(Empty),Empty) -> 1
  |Op1(Move(Empty),From16) -> 2
  |Op1(Move(Empty),X16) -> 3
  |Op1(Move(Wide),Empty) -> 4
  |Op1(Move(Wide),From16) -> 5
  |Op1(Move(Wide),X16) -> 6
  |Op1(Move(Object),Empty) -> 7
  |Op1(Move(Object),From16) -> 8
  |Op1(Move(Object),X16) ->9
  |Op1(MoveResult(Empty),Empty) ->10
  |Op1(MoveResult(Wide),Empty) ->11
  |Op1(MoveResult(Object),Empty) ->12
  |Op1(MoveException,Empty) ->13
  |Op0(ReturnVoid,Empty) ->14
  |Op1(Return(Empty),Empty) ->15
  |Op1(Return(Wide),Empty) ->16
  |Op2(Const(Empty),X4) ->17
  |Op2(Const(Empty),X16) ->18
  |Op2(Const(Empty),Empty) ->19
  |Op2(Const(Empty),High16) ->20
  |Op2(Const(Wide),X16) ->21
  |Op2(Const(Wide),X32) ->22
  |Op2(Const(Wide),Empty) ->23
  |Op2(Const(Wide),High16) ->24
  |Op2(Const(String),Empty) ->25
  |Op2(Const(String),Jumbo) ->26
  |Op2(Const(Class),Empty) ->27
  |Op1(MoniterEnter,Empty) ->28
  |Op1(MoniterExit,Empty) ->29
  |Op2(CheckCast,Empty) ->30
  |Op3(InstanceOf,Empty) ->31
  |Op2(ArrayLength,Empty) ->32
  |Op2(NewInstance,Empty) ->33
  |Op3(NewArray,Empty) ->34
  |Opn(FilledNewArray,Empty) ->35
  |Opn(FilledNewArray,Range) ->36
  |Opn(FillArrayData,Empty) ->37
  |Op1(Throw,Empty) ->38
  |Op1(Goto,Empty) ->39
  |Op1(Goto,X16) ->40
  |Op1(Goto,X32) ->41
  |Opn(PackedSwitchData,Empty) ->42
  |Opn(PackedSwitch,Empty) ->43
  |Opn(SparseSwitchData,Empty) ->44
  |Opn(SparseSwitch,Empty) ->45
  |Op3(Cmpl(Float),Empty) ->46
  |Op3(Cmpg(Float),Empty) ->47
  |Op3(Cmpl(Double),Empty) ->48
  |Op3(Cmpg(Double),Empty) ->49
  |Op3(CmpLong,Empty) ->50
  |Op3(If(Eq),Empty) ->51
  |Op3(If(Ne),Empty) ->52
  |Op3(If(Lt),Empty) ->53
  |Op3(If(Ge),Empty) ->54
  |Op3(If(Gt),Empty) ->55
  |Op3(If(Le),Empty) ->56
  |Op2(Ifz(Eq),Empty) ->57
  |Op2(Ifz(Ne),Empty) ->58
  |Op2(Ifz(Lt),Empty) ->59
  |Op2(Ifz(Ge),Empty) ->60
  |Op2(Ifz(Gt),Empty) ->61
  |Op2(Ifz(Le),Empty) ->62
  |Op3(Aget(Empty),Empty) ->63
  |Op3(Aget(Wide),Empty) ->64
  |Op3(Aget(Object),Empty) ->65
  |Op3(Aget(Boolean),Empty) ->66
  |Op3(Aget(Byte),Empty) ->67
  |Op3(Aget(Char),Empty) ->68
  |Op3(Aget(Short),Empty) ->69
  |Op3(Aput(Empty),Empty) ->70
  |Op3(Aput(Wide),Empty) ->71
  |Op3(Aput(Object),Empty) ->72
  |Op3(Aput(Boolean),Empty) ->73
  |Op3(Aput(Byte),Empty) ->74
  |Op3(Aput(Char),Empty) ->75
  |Op3(Aput(Short),Empty) ->76
  |Op3(Iget(Empty),Empty) ->77
  |Op3(Iget(Wide),Empty) ->78
  |Op3(Iget(Object),Empty) ->79
  |Op3(Iget(Boolean),Empty) ->80
  |Op3(Iget(Byte),Empty) ->81
  |Op3(Iget(Char),Empty) ->82
  |Op3(Iget(Short),Empty) ->83
  |Op3(Iput(Empty),Empty) ->84
  |Op3(Iput(Wide),Empty) ->85
  |Op3(Iput(Object),Empty) ->86
  |Op3(Iput(Boolean),Empty) ->87
  |Op3(Iput(Byte),Empty) ->88
  |Op3(Iput(Char),Empty) ->89
  |Op3(Iput(Short),Empty) ->90
  |Op2(Sget(Empty),Empty) ->91
  |Op2(Sget(Wide),Empty) ->92
  |Op2(Sget(Object),Empty) ->93
  |Op2(Sget(Boolean),Empty) ->94
  |Op2(Sget(Char),Empty) ->95
  |Op2(Sget(Short),Empty) ->96
  |Op2(Sput(Empty),Empty) -> 97
  |Op2(Sput(Wide),Empty) -> 98
  |Op2(Sput(Object),Empty) -> 99
  |Op2(Sput(Boolean),Empty) -> 100
  |Op2(Sput(Byte),Empty) -> 101
  |Op2(Sput(Char),Empty) -> 102
  |Op2(Sput(Short),Empty) -> 103
  |Opn(Invoke(Virtual),Empty) -> 104
  |Opn(Invoke(Super),Empty) -> 105
  |Opn(Invoke(Direct),Empty) -> 106
  |Opn(Invoke(Static),Empty) -> 107
  |Opn(Invoke(Interface),Empty) -> 108
  |Opn(Invoke(Virtual),Range) -> 109
  |Opn(Invoke(Super),Range) -> 110
  |Opn(Invoke(Direct),Range) -> 111
  |Opn(Invoke(Static),Range) -> 112
  |Opn(Invoke(Interface),Range) -> 113
  |Opn(Invoke(Polymorphic),Empty) ->114
  |Opn(Invoke(Polymorphic),Range) ->115
  |Opn(Invoke(Custom),Empty) -> 116
  |Opn(Invoke(Custom),Range) -> 117
  |Op2(Neg(Int),Empty) -> 118
  |Op2(Not(Int),Empty) -> 119
  |Op2(Neg(Long),Empty) -> 120
  |Op2(Not(Long),Empty) -> 121
  |Op2(Neg(Float),Empty) -> 122
  |Op2(Not(Float),Empty) -> 123
  |Op2(Neg(Double),Empty) -> 124
  |Op2(To(Int,Long),Empty) -> 125
  |Op2(To(Int,Float),Empty) -> 126
  |Op2(To(Int,Double),Empty) -> 127
  |Op2(To(Long,Int),Empty) -> 128
  |Op2(To(Long,Float),Empty) -> 129
  |Op2(To(Long,Double),Empty) -> 130
  |Op2(To(Float,Int),Empty) -> 131
  |Op2(To(Float,Long),Empty) -> 132
  |Op2(To(Float,Double),Empty) -> 133
  |Op2(To(Double,Int),Empty) -> 134
  |Op2(To(Double,Long),Empty) -> 135
  |Op2(To(Double,Float),Empty) -> 136
  |Op2(To(Int,Byte),Empty) -> 137
  |Op2(To(Int,Char),Empty) -> 138
  |Op2(To(Int,Short),Empty) -> 139
  |Op3(Arithm(Add,Int),Empty) -> 140
  |Op3(Arithm(Sub,Int),Empty) -> 141
  |Op3(Arithm(Mul,Int),Empty) -> 142
  |Op3(Arithm(Div,Int),Empty) -> 143
  |Op3(Arithm(Rem,Int),Empty) -> 144
  |Op3(Arithm(And,Int),Empty) -> 145
  |Op3(Arithm(Or,Int),Empty) -> 146
  |Op3(Arithm(Xor,Int),Empty) -> 147
  |Op3(Arithm(Shl,Int),Empty) -> 148
  |Op3(Arithm(Shr,Int),Empty) -> 149
  |Op3(Arithm(Ushr,Int),Empty) -> 150
  |Op3(Arithm(Add,Long),Empty) -> 151
  |Op3(Arithm(Sub,Long),Empty) -> 152
  |Op3(Arithm(Mul,Long),Empty) -> 153
  |Op3(Arithm(Div,Long),Empty) -> 154
  |Op3(Arithm(Rem,Long),Empty) -> 155
  |Op3(Arithm(And,Long),Empty) -> 156
  |Op3(Arithm(Or,Long),Empty) -> 157
  |Op3(Arithm(Xor,Long),Empty) -> 158
  |Op3(Arithm(Shl,Long),Empty) -> 159
  |Op3(Arithm(Shr,Long),Empty) -> 160
  |Op3(Arithm(Ushr,Long),Empty) -> 161
  |Op3(Arithm(Add,Float),Empty) -> 162
  |Op3(Arithm(Sub,Float),Empty) -> 163
  |Op3(Arithm(Mul,Float),Empty) -> 164
  |Op3(Arithm(Div,Float),Empty) -> 165
  |Op3(Arithm(Rem,Float),Empty) -> 166
  |Op3(Arithm(Add,Double),Empty) -> 167
  |Op3(Arithm(Sub,Double),Empty) -> 168
  |Op3(Arithm(Mul,Double),Empty) -> 169
  |Op3(Arithm(Div,Double),Empty) -> 170
  |Op3(Arithm(Rem,Double),Empty)  ->171
  |Op3(Arithm(Add,Int),X2Addr) -> 172
  |Op3(Arithm(Sub,Int),X2Addr) -> 173
  |Op3(Arithm(Mul,Int),X2Addr) -> 174
  |Op3(Arithm(Div,Int),X2Addr) -> 175
  |Op3(Arithm(Rem,Int),X2Addr) -> 176
  |Op3(Arithm(And,Int),X2Addr) -> 177
  |Op3(Arithm(Or,Int),X2Addr) -> 178
  |Op3(Arithm(Xor,Int),X2Addr) -> 179
  |Op3(Arithm(Shl,Int),X2Addr) -> 180
  |Op3(Arithm(Shr,Int),X2Addr) -> 181
  |Op3(Arithm(Ushr,Int),X2Addr) -> 182
  |Op3(Arithm(Add,Long),X2Addr) -> 183
  |Op3(Arithm(Sub,Long),X2Addr) -> 184
  |Op3(Arithm(Mul,Long),X2Addr) -> 185
  |Op3(Arithm(Div,Long),X2Addr) -> 186
  |Op3(Arithm(Rem,Long),X2Addr) -> 187
  |Op3(Arithm(And,Long),X2Addr) -> 188
  |Op3(Arithm(Or,Long),X2Addr) -> 189
  |Op3(Arithm(Xor,Long),X2Addr) -> 190
  |Op3(Arithm(Shl,Long),X2Addr) -> 191
  |Op3(Arithm(Shr,Long),X2Addr) -> 192
  |Op3(Arithm(Ushr,Long),X2Addr) -> 193
  |Op3(Arithm(Add,Float),X2Addr) -> 194
  |Op3(Arithm(Sub,Float),X2Addr) -> 195
  |Op3(Arithm(Mul,Float),X2Addr) -> 196
  |Op3(Arithm(Div,Float),X2Addr) -> 197
  |Op3(Arithm(Rem,Float),X2Addr) -> 198
  |Op3(Arithm(Add,Double),X2Addr) ->199
  |Op3(Arithm(Sub,Double),X2Addr) ->200
  |Op3(Arithm(Mul,Double),X2Addr) ->201
  |Op3(Arithm(Div,Double),X2Addr) ->202
  |Op3(Arithm(Rem,Double),X2Addr) ->203
  |Op3(Arithm(Add,Int),Lit16) -> 204
  |Op3(Arithm(Sub,Int),Lit16) -> 205
  |Op3(Arithm(Mul,Int),Lit16) -> 206
  |Op3(Arithm(Div,Int),Lit16) -> 207
  |Op3(Arithm(Rem,Int),Lit16) -> 208
  |Op3(Arithm(And,Int),Lit16) -> 209
  |Op3(Arithm(Or,Int),Lit16) -> 210
  |Op3(Arithm(Xor,Int),Lit16) -> 211
  |Op3(Arithm(Add,Int),Lit8) -> 212
  |Op3(Arithm(Sub,Int),Lit8) -> 213
  |Op3(Arithm(Mul,Int),Lit8) -> 214
  |Op3(Arithm(Div,Int),Lit8) -> 215
  |Op3(Arithm(Rem,Int),Lit8) -> 216
  |Op3(Arithm(And,Int),Lit8) -> 217
  |Op3(Arithm(Or,Int),Lit8) -> 218
  |Op3(Arithm(Xor,Int),Lit8) -> 219
  |Op3(Arithm(Shl,Int),Lit8) -> 220
  |Op3(Arithm(Shr,Int),Lit8) -> 221
  |Op3(Arithm(Ushr,Int),Lit8) -> 222
  |Opn(ArrayData,Empty) -> 223
  |_ -> 256