exception UnknownInstruction of string
exception NotTranslated of string

type data_type = 
  |Empty| Wide |Boolean |Byte |Char |Short 
  |Int |Long |Float |Double |Object |String |Class
type param = 
  |From16 |High16 |X4 |X16 |X32 |X2Addr |Lit16 |Lit8 |Jumbo |Empty
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


(*need to double check how arrays are built*)
type weirdaf = 
  |FilledNewArray |FillArrayData |PackedSwitch |SparseSwitch |Invoke |ArrayData


type operator = 
  |Name of string
  |Undefined 
  |Op0 of op0 * param
  |Op1 of op1 * param 
  |Op2 of op2 * param  
  |Op3 of op3 * param 
  |Weird of weirdaf
  |Wtf of weirdaf

type instruction = operator * args


type class_descriptor =  Descriptor of string
type flags = Flags of int * (string list) 
type undefined_type = Unknown

type interface = Interface of string 
type value = 
  |Value_int of int | Value_string of string 
  |Value_Sci of string|Value_float of float
  |Value_bool of bool |Value_Big_int of string



type position = Pos of string
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