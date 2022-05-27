exception UnknownInstruction of string
exception NotTranslated of string


type data_type = 
  |Empty| Wide |Boolean |Byte |Char |Short 
  |Int |Long |Float |Double |Object |String |Class
type param = 
  |From16 |High16 |X4 |X16 |X32 |X2Addr |Lit16 |Lit8 |Jumbo | Range| Empty
type args = string list
type cmp = 
  Eq | Ne |Lt |Ge |Gt |Le
type op0 = 
  |Nop |ReturnVoid
type op1 = 
  |Move of data_type
  |MoveResult of data_type
  |MoveException
  |Return of data_type
  |MoniterEnter |MoniterExit
  |Throw
  |Goto
type op2 = 
  |Move of data_type
  |Const of data_type
  |CheckCast
  |ArrayLength 
  |NewInstance
  |Sget of data_type |Sput of data_type
  |Neg of data_type |Not of data_type | To of data_type * data_type
  |Ifz of cmp
  |ConstMethodHandle 
  |ConstMethodType
type arithmetics = 
  |Add |Sub |Mul |Div |Rem |And |Or |Xor |Shl |Shr |Ushr 

type op3 = 
  |InstanceOf 
  |NewArray
  |Cmpl of data_type | Cmpg of data_type | CmpLong
  |If of cmp 
  |Aget of data_type |Aput of data_type
  |Iget of data_type |Iput of data_type
  |Arithm of  arithmetics *data_type 


type invoke_type = 
  |Virtual |Super |Direct |Static |Interface |Polymorphic |Custom

(*need to double check how arrays are built*)
type opn = 
  |FilledNewArray |FillArrayData |Invoke of invoke_type |ArrayData
  |PackedSwitch |SparseSwitch 
  |PackedSwitchData |SparseSwitchData 


type operator = 
  |Undefined 
  |Op0 of op0 * param
  |Op1 of op1 * param 
  |Op2 of op2 * param  
  |Op3 of op3 * param 
  |Opn of opn * param

type instruction = {
  pc_pos : int;
  op : operator;
  args : args
}


type class_descriptor =  Descriptor of string
type flags = Flags of int * (string list) 
type undefined_type = Unknown

type interface = Interface of string 
type value = 
  |Value_int of int | Value_string of string 
  |Value_Sci of string|Value_float of float
  |Value_bool of bool |Value_Big_int of string



type type_field = Empty_field |Field of {
  interface : interface;
  name : string; 
  type_name : string; 
  access : flags; 
  value : value
}

type type_code = Empty_code 
  |Code of {
  registers : int; 
  ins : int; 
  outs: int; 
  insns_size : string; 
  instructions : instruction list; 
}

type type_method = Empty_method | Method of {
  interface: interface;
  name: string; 
  type_name : string; 
  access : flags; 
  code : type_code
}



type type_class = Empty_class | C of {
  descriptor : class_descriptor; 
  access_flags : flags; 
  superclass : class_descriptor ;
  interfaces : interface list;
  static_fields : type_field list;  
  instance_fields : type_field list; 
  direct_methods : type_method list; 
  virtual_methods : type_method list; 
  source_file_idx : undefined_type;
}

type program = Empty_prog | Prog of (type_class list)


let number_instructions = 223
 
let instruction_id i =
  match i with                      
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
  |Opn(Invoke(Polymorphic),Empty) ->1114
  |Opn(Invoke(Polymorphic),Range) ->1115
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
  |Op3(Arithm(Rem,Double),Empty)  ->1171
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
  |Op3(Arithm(Add,Double),X2Addr) ->2199
  |Op3(Arithm(Sub,Double),X2Addr) ->2200
  |Op3(Arithm(Mul,Double),X2Addr) ->2201
  |Op3(Arithm(Div,Double),X2Addr) ->2202
  |Op3(Arithm(Rem,Double),X2Addr) ->2203
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
  |_ -> failwith "no id for this operat"