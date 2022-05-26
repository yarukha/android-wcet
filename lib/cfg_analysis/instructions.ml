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


type pc = Pc of int

type instruction = {
  pc: pc;
  op : operator;
  args : args
}

type block = instruction list


