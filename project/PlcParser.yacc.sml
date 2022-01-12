functor PlcParserLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : PlcParser_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\039\000\004\000\129\000\005\000\038\000\006\000\037\000\
\\007\000\036\000\008\000\035\000\009\000\034\000\010\000\033\000\
\\011\000\032\000\012\000\031\000\013\000\030\000\014\000\029\000\
\\020\000\028\000\000\000\
\\001\000\001\000\039\000\005\000\038\000\006\000\037\000\007\000\036\000\
\\008\000\035\000\009\000\034\000\010\000\033\000\011\000\032\000\
\\012\000\031\000\013\000\030\000\014\000\029\000\020\000\028\000\
\\026\000\097\000\000\000\
\\001\000\001\000\039\000\005\000\038\000\006\000\037\000\007\000\036\000\
\\008\000\035\000\009\000\034\000\010\000\033\000\011\000\032\000\
\\012\000\031\000\013\000\030\000\014\000\029\000\020\000\028\000\
\\028\000\093\000\000\000\
\\001\000\001\000\039\000\005\000\038\000\006\000\037\000\007\000\036\000\
\\008\000\035\000\009\000\034\000\010\000\033\000\011\000\032\000\
\\012\000\031\000\013\000\030\000\014\000\029\000\020\000\028\000\
\\034\000\088\000\040\000\087\000\000\000\
\\001\000\001\000\039\000\005\000\038\000\006\000\037\000\007\000\036\000\
\\008\000\035\000\009\000\034\000\010\000\033\000\011\000\032\000\
\\012\000\031\000\013\000\030\000\014\000\029\000\020\000\028\000\
\\037\000\114\000\041\000\113\000\000\000\
\\001\000\001\000\039\000\005\000\038\000\006\000\037\000\007\000\036\000\
\\008\000\035\000\009\000\034\000\010\000\033\000\011\000\032\000\
\\012\000\031\000\013\000\030\000\014\000\029\000\020\000\028\000\
\\037\000\119\000\000\000\
\\001\000\001\000\040\000\000\000\
\\001\000\002\000\078\000\000\000\
\\001\000\003\000\025\000\012\000\024\000\016\000\023\000\017\000\022\000\
\\018\000\021\000\019\000\020\000\020\000\052\000\021\000\019\000\
\\027\000\016\000\029\000\015\000\031\000\014\000\033\000\051\000\
\\034\000\050\000\035\000\012\000\038\000\011\000\039\000\010\000\
\\043\000\009\000\045\000\049\000\046\000\048\000\047\000\047\000\000\000\
\\001\000\003\000\025\000\012\000\024\000\016\000\023\000\017\000\022\000\
\\018\000\021\000\019\000\020\000\021\000\019\000\022\000\018\000\
\\023\000\017\000\027\000\016\000\029\000\015\000\031\000\014\000\
\\033\000\013\000\035\000\012\000\038\000\011\000\039\000\010\000\
\\043\000\009\000\000\000\
\\001\000\003\000\025\000\012\000\024\000\016\000\023\000\017\000\022\000\
\\018\000\021\000\019\000\020\000\021\000\019\000\027\000\016\000\
\\029\000\015\000\031\000\014\000\033\000\013\000\035\000\012\000\
\\038\000\011\000\039\000\010\000\042\000\126\000\043\000\009\000\000\000\
\\001\000\003\000\025\000\012\000\024\000\016\000\023\000\017\000\022\000\
\\018\000\021\000\019\000\020\000\021\000\019\000\027\000\016\000\
\\029\000\015\000\031\000\014\000\033\000\013\000\035\000\012\000\
\\038\000\011\000\039\000\010\000\043\000\009\000\000\000\
\\001\000\006\000\096\000\000\000\
\\001\000\006\000\115\000\000\000\
\\001\000\006\000\134\000\036\000\084\000\000\000\
\\001\000\020\000\052\000\033\000\083\000\034\000\082\000\045\000\049\000\
\\046\000\048\000\047\000\047\000\000\000\
\\001\000\020\000\052\000\033\000\083\000\045\000\049\000\046\000\048\000\
\\047\000\047\000\000\000\
\\001\000\020\000\085\000\034\000\110\000\036\000\084\000\040\000\109\000\000\000\
\\001\000\020\000\085\000\036\000\084\000\000\000\
\\001\000\021\000\065\000\000\000\
\\001\000\024\000\057\000\043\000\056\000\000\000\
\\001\000\025\000\128\000\000\000\
\\001\000\030\000\098\000\000\000\
\\001\000\030\000\105\000\000\000\
\\001\000\030\000\111\000\036\000\084\000\000\000\
\\001\000\032\000\092\000\000\000\
\\001\000\033\000\042\000\000\000\
\\001\000\034\000\086\000\000\000\
\\001\000\034\000\102\000\000\000\
\\001\000\034\000\108\000\000\000\
\\001\000\034\000\110\000\036\000\084\000\040\000\109\000\000\000\
\\001\000\034\000\121\000\000\000\
\\001\000\036\000\084\000\043\000\100\000\000\000\
\\001\000\036\000\130\000\000\000\
\\001\000\037\000\114\000\041\000\113\000\000\000\
\\001\000\043\000\058\000\000\000\
\\001\000\043\000\095\000\000\000\
\\001\000\044\000\000\000\000\000\
\\138\000\000\000\
\\139\000\001\000\039\000\005\000\038\000\006\000\037\000\007\000\036\000\
\\008\000\035\000\009\000\034\000\010\000\033\000\011\000\032\000\
\\012\000\031\000\013\000\030\000\014\000\029\000\020\000\028\000\000\000\
\\140\000\000\000\
\\141\000\005\000\038\000\006\000\037\000\007\000\036\000\008\000\035\000\
\\009\000\034\000\010\000\033\000\011\000\032\000\012\000\031\000\
\\013\000\030\000\014\000\029\000\020\000\028\000\000\000\
\\142\000\005\000\038\000\006\000\037\000\007\000\036\000\008\000\035\000\
\\009\000\034\000\010\000\033\000\011\000\032\000\012\000\031\000\
\\013\000\030\000\014\000\029\000\020\000\028\000\000\000\
\\143\000\005\000\038\000\006\000\037\000\007\000\036\000\008\000\035\000\
\\009\000\034\000\010\000\033\000\011\000\032\000\012\000\031\000\
\\013\000\030\000\014\000\029\000\020\000\028\000\000\000\
\\144\000\021\000\019\000\031\000\014\000\033\000\013\000\035\000\012\000\
\\038\000\011\000\039\000\010\000\043\000\009\000\000\000\
\\145\000\021\000\019\000\031\000\014\000\033\000\013\000\035\000\012\000\
\\038\000\011\000\039\000\010\000\043\000\009\000\000\000\
\\146\000\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\014\000\029\000\020\000\028\000\000\000\
\\147\000\000\000\
\\148\000\001\000\039\000\005\000\038\000\006\000\037\000\007\000\036\000\
\\008\000\035\000\009\000\034\000\010\000\033\000\011\000\032\000\
\\012\000\031\000\013\000\030\000\014\000\029\000\020\000\028\000\000\000\
\\149\000\020\000\028\000\000\000\
\\150\000\020\000\028\000\000\000\
\\151\000\020\000\028\000\000\000\
\\152\000\020\000\028\000\000\000\
\\153\000\020\000\028\000\000\000\
\\154\000\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\014\000\029\000\020\000\028\000\000\000\
\\155\000\020\000\028\000\000\000\
\\156\000\020\000\028\000\000\000\
\\157\000\020\000\028\000\000\000\
\\158\000\020\000\028\000\000\000\
\\159\000\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\014\000\029\000\020\000\028\000\000\000\
\\160\000\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\014\000\029\000\020\000\028\000\000\000\
\\161\000\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\014\000\029\000\020\000\028\000\000\000\
\\162\000\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\014\000\029\000\020\000\028\000\000\000\
\\163\000\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\014\000\029\000\020\000\028\000\000\000\
\\164\000\001\000\039\000\005\000\038\000\006\000\037\000\007\000\036\000\
\\008\000\035\000\009\000\034\000\010\000\033\000\011\000\032\000\
\\012\000\031\000\013\000\030\000\014\000\029\000\020\000\028\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\001\000\039\000\005\000\038\000\006\000\037\000\007\000\036\000\
\\008\000\035\000\009\000\034\000\010\000\033\000\011\000\032\000\
\\012\000\031\000\013\000\030\000\014\000\029\000\020\000\028\000\
\\040\000\087\000\000\000\
\\180\000\000\000\
\\181\000\000\000\
\\182\000\000\000\
\\183\000\001\000\039\000\005\000\038\000\006\000\037\000\007\000\036\000\
\\008\000\035\000\009\000\034\000\010\000\033\000\011\000\032\000\
\\012\000\031\000\013\000\030\000\014\000\029\000\020\000\028\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\\186\000\000\000\
\\187\000\040\000\101\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\192\000\000\000\
\\193\000\036\000\084\000\000\000\
\\194\000\000\000\
\\195\000\000\000\
\\196\000\000\000\
\\197\000\000\000\
\\198\000\036\000\084\000\040\000\109\000\000\000\
\\199\000\000\000\
\"
val actionRowNumbers =
"\009\000\066\000\045\000\044\000\
\\039\000\006\000\038\000\067\000\
\\075\000\074\000\026\000\008\000\
\\009\000\011\000\011\000\020\000\
\\035\000\076\000\011\000\011\000\
\\011\000\011\000\011\000\011\000\
\\073\000\072\000\019\000\011\000\
\\011\000\011\000\011\000\011\000\
\\011\000\011\000\011\000\011\000\
\\011\000\011\000\009\000\007\000\
\\015\000\090\000\018\000\027\000\
\\003\000\094\000\095\000\096\000\
\\077\000\008\000\016\000\025\000\
\\048\000\002\000\026\000\036\000\
\\012\000\053\000\052\000\051\000\
\\050\000\049\000\001\000\022\000\
\\058\000\057\000\056\000\055\000\
\\063\000\062\000\061\000\060\000\
\\059\000\054\000\064\000\040\000\
\\011\000\032\000\087\000\028\000\
\\085\000\016\000\016\000\023\000\
\\070\000\011\000\069\000\029\000\
\\017\000\024\000\068\000\034\000\
\\013\000\026\000\011\000\011\000\
\\065\000\005\000\089\000\016\000\
\\086\000\030\000\093\000\031\000\
\\080\000\079\000\091\000\016\000\
\\097\000\092\000\047\000\010\000\
\\081\000\011\000\021\000\041\000\
\\000\000\071\000\088\000\078\000\
\\099\000\098\000\033\000\083\000\
\\084\000\042\000\016\000\011\000\
\\011\000\014\000\046\000\004\000\
\\011\000\082\000\043\000\037\000"
val gotoT =
"\
\\001\000\135\000\002\000\006\000\003\000\005\000\004\000\004\000\
\\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\000\000\
\\005\000\024\000\007\000\001\000\000\000\
\\005\000\025\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\039\000\000\000\
\\004\000\044\000\005\000\003\000\006\000\002\000\007\000\001\000\
\\008\000\043\000\014\000\042\000\015\000\041\000\000\000\
\\002\000\051\000\003\000\005\000\004\000\004\000\005\000\003\000\
\\006\000\002\000\007\000\001\000\000\000\
\\004\000\052\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\053\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\057\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\058\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\059\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\060\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\061\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\062\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\064\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\065\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\066\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\067\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\068\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\069\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\070\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\071\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\072\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\073\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\074\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\002\000\075\000\003\000\005\000\004\000\004\000\005\000\003\000\
\\006\000\002\000\007\000\001\000\000\000\
\\000\000\
\\012\000\079\000\013\000\078\000\014\000\077\000\015\000\041\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\044\000\005\000\003\000\006\000\002\000\007\000\001\000\
\\008\000\043\000\014\000\088\000\015\000\041\000\016\000\087\000\000\000\
\\014\000\089\000\015\000\041\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\092\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\097\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\101\000\015\000\041\000\016\000\087\000\000\000\
\\014\000\102\000\015\000\041\000\000\000\
\\000\000\
\\000\000\
\\004\000\105\000\005\000\003\000\006\000\002\000\007\000\001\000\
\\008\000\104\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\110\000\000\000\
\\000\000\
\\011\000\114\000\000\000\
\\004\000\115\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\116\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\118\000\013\000\078\000\014\000\077\000\015\000\041\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\121\000\015\000\041\000\016\000\120\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\123\000\005\000\003\000\006\000\002\000\007\000\001\000\
\\010\000\122\000\000\000\
\\000\000\
\\004\000\125\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\129\000\015\000\041\000\000\000\
\\004\000\130\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\131\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\009\000\133\000\000\000\
\\004\000\134\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 136
val numrules = 62
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
end
type svalue = MlyValue.svalue
type result = unit
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 43) => true | _ => false
val showTerminal =
fn (T 0) => "SEMICOLON"
  | (T 1) => "TARROW"
  | (T 2) => "IF"
  | (T 3) => "ELSE"
  | (T 4) => "AND"
  | (T 5) => "EQUAL"
  | (T 6) => "DIFFERENCE"
  | (T 7) => "LESS"
  | (T 8) => "LE"
  | (T 9) => "DOUBLECOLON"
  | (T 10) => "PLUS"
  | (T 11) => "MINUS"
  | (T 12) => "TIMES"
  | (T 13) => "DIVIDE"
  | (T 14) => "NOT"
  | (T 15) => "HD"
  | (T 16) => "TL"
  | (T 17) => "ISE"
  | (T 18) => "PRINT"
  | (T 19) => "LBRACKETS"
  | (T 20) => "NAT"
  | (T 21) => "VAR"
  | (T 22) => "FUN"
  | (T 23) => "REC"
  | (T 24) => "COLON"
  | (T 25) => "THEN"
  | (T 26) => "MATCH"
  | (T 27) => "WITH"
  | (T 28) => "EXCLAMATION"
  | (T 29) => "RBRACKETS"
  | (T 30) => "LBRACES"
  | (T 31) => "RBRACES"
  | (T 32) => "LPAREN"
  | (T 33) => "RPAREN"
  | (T 34) => "FN"
  | (T 35) => "FARROW"
  | (T 36) => "END"
  | (T 37) => "TRUE"
  | (T 38) => "FALSE"
  | (T 39) => "COMMA"
  | (T 40) => "PIPE"
  | (T 41) => "UNDERSCORE"
  | (T 42) => "NAME"
  | (T 43) => "EOF"
  | (T 44) => "INT"
  | (T 45) => "BOOL"
  | (T 46) => "NIL"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40)
 $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33)
 $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26)
 $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.ntVOID Prog1, Prog1left, Prog1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
Prog1 = Prog1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, Prog1left, Prog1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.ntVOID Expr1, Expr1left, Expr1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
Expr1 = Expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, Expr1left, Expr1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.ntVOID Prog1, _, Prog1right)) :: _ :: ( _, (
 MlyValue.ntVOID Decl1, Decl1left, _)) :: rest671)) => let val  result
 = MlyValue.ntVOID (fn _ => ( let val  Decl1 = Decl1 ()
 val  Prog1 = Prog1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, Decl1left, Prog1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.ntVOID Expr1, _, Expr1right)) :: _ :: _ :: (
 _, ( _, VAR1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  Expr1 = Expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, VAR1left, Expr1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.ntVOID Expr1, _, Expr1right)) :: _ :: ( _, (
 MlyValue.ntVOID Args1, _, _)) :: _ :: ( _, ( _, FUN1left, _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
Args1 = Args1 ()
 val  Expr1 = Expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, FUN1left, Expr1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ntVOID Expr1, _, Expr1right)) :: _ :: ( _, (
 MlyValue.ntVOID Type1, _, _)) :: _ :: ( _, ( MlyValue.ntVOID Args1, _
, _)) :: _ :: _ :: ( _, ( _, FUN1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  Args1 = Args1 ()
 val  Type1 = Type1 ()
 val  Expr1 = Expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, FUN1left, Expr1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.ntVOID AtomicExpr1, AtomicExpr1left, 
AtomicExpr1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  AtomicExpr1 = AtomicExpr1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, AtomicExpr1left, AtomicExpr1right), 
rest671)
end
|  ( 7, ( ( _, ( MlyValue.ntVOID AppExpr1, AppExpr1left, AppExpr1right
)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  AppExpr1 = AppExpr1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, AppExpr1left, AppExpr1right), rest671)

end
|  ( 8, ( ( _, ( MlyValue.ntVOID Expr3, _, Expr3right)) :: _ :: ( _, (
 MlyValue.ntVOID Expr2, _, _)) :: _ :: ( _, ( MlyValue.ntVOID Expr1, _
, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 val  Expr3 = Expr3 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, IF1left, Expr3right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ntVOID MatchExpr1, _, MatchExpr1right)) :: _
 :: ( _, ( MlyValue.ntVOID Expr1, _, _)) :: ( _, ( _, MATCH1left, _))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 Expr1 = Expr1 ()
 val  MatchExpr1 = MatchExpr1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, MATCH1left, MatchExpr1right), rest671)

end
|  ( 10, ( ( _, ( MlyValue.ntVOID Expr1, _, Expr1right)) :: ( _, ( _, 
EXCLAMATION1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  Expr1 = Expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, EXCLAMATION1left, Expr1right), rest671)

end
|  ( 11, ( ( _, ( MlyValue.ntVOID Expr1, _, Expr1right)) :: ( _, ( _, 
MINUS1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  Expr1 = Expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, MINUS1left, Expr1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.ntVOID Expr1, _, Expr1right)) :: ( _, ( _, 
HD1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  Expr1 = Expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, HD1left, Expr1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ntVOID Expr1, _, Expr1right)) :: ( _, ( _, 
TL1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  Expr1 = Expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, TL1left, Expr1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.ntVOID Expr1, _, Expr1right)) :: ( _, ( _, 
ISE1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  Expr1 = Expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, ISE1left, Expr1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.ntVOID Expr1, _, Expr1right)) :: ( _, ( _, 
PRINT1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  Expr1 = Expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, PRINT1left, Expr1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.ntVOID Expr2, _, Expr2right)) :: _ :: ( _, 
( MlyValue.ntVOID Expr1, Expr1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, Expr1left, Expr2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.ntVOID Expr2, _, Expr2right)) :: _ :: ( _, 
( MlyValue.ntVOID Expr1, Expr1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, Expr1left, Expr2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.ntVOID Expr2, _, Expr2right)) :: _ :: ( _, 
( MlyValue.ntVOID Expr1, Expr1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, Expr1left, Expr2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.ntVOID Expr2, _, Expr2right)) :: _ :: ( _, 
( MlyValue.ntVOID Expr1, Expr1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, Expr1left, Expr2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.ntVOID Expr2, _, Expr2right)) :: _ :: ( _, 
( MlyValue.ntVOID Expr1, Expr1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, Expr1left, Expr2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.ntVOID Expr2, _, Expr2right)) :: _ :: ( _, 
( MlyValue.ntVOID Expr1, Expr1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, Expr1left, Expr2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.ntVOID Expr2, _, Expr2right)) :: _ :: ( _, 
( MlyValue.ntVOID Expr1, Expr1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, Expr1left, Expr2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.ntVOID Expr2, _, Expr2right)) :: _ :: ( _, 
( MlyValue.ntVOID Expr1, Expr1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, Expr1left, Expr2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.ntVOID Expr2, _, Expr2right)) :: _ :: ( _, 
( MlyValue.ntVOID Expr1, Expr1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, Expr1left, Expr2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.ntVOID Expr2, _, Expr2right)) :: _ :: ( _, 
( MlyValue.ntVOID Expr1, Expr1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, Expr1left, Expr2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.ntVOID Expr2, _, Expr2right)) :: _ :: ( _, 
( MlyValue.ntVOID Expr1, Expr1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, Expr1left, Expr2right), rest671)
end
|  ( 27, ( ( _, ( _, _, RBRACKETS1right)) :: _ :: _ :: ( _, ( 
MlyValue.ntVOID Expr1, Expr1left, _)) :: rest671)) => let val  result
 = MlyValue.ntVOID (fn _ => ( let val  Expr1 = Expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, Expr1left, RBRACKETS1right), rest671)

end
|  ( 28, ( ( _, ( MlyValue.ntVOID Const1, Const1left, Const1right)) ::
 rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
Const1 = Const1 ()
 in ()
end; ()))
 in ( LrTable.NT 4, ( result, Const1left, Const1right), rest671)
end
|  ( 29, ( ( _, ( _, NAME1left, NAME1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 4, ( result, NAME1left, NAME1right), rest671)
end
|  ( 30, ( ( _, ( _, _, RBRACES1right)) :: ( _, ( MlyValue.ntVOID 
Prog1, _, _)) :: ( _, ( _, LBRACES1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  Prog1 = Prog1 ()
 in ()
end; ()))
 in ( LrTable.NT 4, ( result, LBRACES1left, RBRACES1right), rest671)

end
|  ( 31, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID Expr1
, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.ntVOID (fn _ => ( let val  Expr1 = Expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 4, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 32, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID 
Comps1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  Comps1 = Comps1 ()
 in ()
end; ()))
 in ( LrTable.NT 4, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 33, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.ntVOID Expr1, _
, _)) :: _ :: ( _, ( MlyValue.ntVOID Args1, _, _)) :: ( _, ( _, 
FN1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  Args1 = Args1 ()
 val  Expr1 = Expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 4, ( result, FN1left, END1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.ntVOID AtomicExpr2, _, AtomicExpr2right))
 :: ( _, ( MlyValue.ntVOID AtomicExpr1, AtomicExpr1left, _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
AtomicExpr1 = AtomicExpr1 ()
 val  AtomicExpr2 = AtomicExpr2 ()
 in ()
end; ()))
 in ( LrTable.NT 5, ( result, AtomicExpr1left, AtomicExpr2right), 
rest671)
end
|  ( 35, ( ( _, ( MlyValue.ntVOID AtomicExpr1, _, AtomicExpr1right))
 :: ( _, ( MlyValue.ntVOID AppExpr1, AppExpr1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  AppExpr1 = 
AppExpr1 ()
 val  AtomicExpr1 = AtomicExpr1 ()
 in ()
end; ()))
 in ( LrTable.NT 5, ( result, AppExpr1left, AtomicExpr1right), rest671
)
end
|  ( 36, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 6, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 37, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 6, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 38, ( ( _, ( _, NAT1left, NAT1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 6, ( result, NAT1left, NAT1right), rest671)
end
|  ( 39, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( _, LPAREN1left, _))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 6, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 40, ( ( _, ( _, _, RPAREN1right)) :: _ :: _ :: ( _, ( 
MlyValue.ntVOID Type1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671
)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  Type1 = 
Type1 ()
 in ()
end; ()))
 in ( LrTable.NT 6, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.ntVOID Expr2, _, Expr2right)) :: _ :: ( _, 
( MlyValue.ntVOID Expr1, Expr1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in ()
end; ()))
 in ( LrTable.NT 7, ( result, Expr1left, Expr2right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.ntVOID Comps1, _, Comps1right)) :: _ :: ( _
, ( MlyValue.ntVOID Expr1, Expr1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  Expr1 = Expr1 ()
 val  Comps1 = Comps1 ()
 in ()
end; ()))
 in ( LrTable.NT 7, ( result, Expr1left, Comps1right), rest671)
end
|  ( 43, ( ( _, ( _, END1left, END1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 8, ( result, END1left, END1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.ntVOID MatchExpr1, _, MatchExpr1right)) :: 
( _, ( MlyValue.ntVOID Expr1, _, _)) :: _ :: ( _, ( MlyValue.ntVOID 
CondExpr1, _, _)) :: ( _, ( _, PIPE1left, _)) :: rest671)) => let val 
 result = MlyValue.ntVOID (fn _ => ( let val  CondExpr1 = CondExpr1 ()
 val  Expr1 = Expr1 ()
 val  MatchExpr1 = MatchExpr1 ()
 in ()
end; ()))
 in ( LrTable.NT 8, ( result, PIPE1left, MatchExpr1right), rest671)

end
|  ( 45, ( ( _, ( MlyValue.ntVOID Expr1, Expr1left, Expr1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
Expr1 = Expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 9, ( result, Expr1left, Expr1right), rest671)
end
|  ( 46, ( ( _, ( _, UNDERSCORE1left, UNDERSCORE1right)) :: rest671))
 => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 9, ( result, UNDERSCORE1left, UNDERSCORE1right), 
rest671)
end
|  ( 47, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( _, LPAREN1left, _))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 10, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 48, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID 
Params1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val 
 result = MlyValue.ntVOID (fn _ => ( let val  Params1 = Params1 ()
 in ()
end; ()))
 in ( LrTable.NT 10, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 49, ( ( _, ( MlyValue.ntVOID TypedVar1, TypedVar1left, 
TypedVar1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  TypedVar1 = TypedVar1 ()
 in ()
end; ()))
 in ( LrTable.NT 11, ( result, TypedVar1left, TypedVar1right), rest671
)
end
|  ( 50, ( ( _, ( MlyValue.ntVOID Params1, _, Params1right)) :: _ :: (
 _, ( MlyValue.ntVOID TypedVar1, TypedVar1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  TypedVar1 = 
TypedVar1 ()
 val  Params1 = Params1 ()
 in ()
end; ()))
 in ( LrTable.NT 11, ( result, TypedVar1left, Params1right), rest671)

end
|  ( 51, ( ( _, ( _, _, NAME1right)) :: ( _, ( MlyValue.ntVOID Type1, 
Type1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  Type1 = Type1 ()
 in ()
end; ()))
 in ( LrTable.NT 12, ( result, Type1left, NAME1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.ntVOID AtomicType1, AtomicType1left, 
AtomicType1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  AtomicType1 = AtomicType1 ()
 in ()
end; ()))
 in ( LrTable.NT 13, ( result, AtomicType1left, AtomicType1right), 
rest671)
end
|  ( 53, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID 
Types1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  Types1 = Types1 ()
 in ()
end; ()))
 in ( LrTable.NT 13, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 54, ( ( _, ( _, _, RBRACKETS1right)) :: ( _, ( MlyValue.ntVOID 
Type1, _, _)) :: ( _, ( _, LBRACKETS1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  Type1 = Type1 ()
 in ()
end; ()))
 in ( LrTable.NT 13, ( result, LBRACKETS1left, RBRACKETS1right), 
rest671)
end
|  ( 55, ( ( _, ( MlyValue.ntVOID Type2, _, Type2right)) :: _ :: ( _, 
( MlyValue.ntVOID Type1, Type1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 in ()
end; ()))
 in ( LrTable.NT 13, ( result, Type1left, Type2right), rest671)
end
|  ( 56, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 14, ( result, NIL1left, NIL1right), rest671)
end
|  ( 57, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 14, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 58, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 14, ( result, INT1left, INT1right), rest671)
end
|  ( 59, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID Type1
, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.ntVOID (fn _ => ( let val  Type1 = Type1 ()
 in ()
end; ()))
 in ( LrTable.NT 14, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 60, ( ( _, ( MlyValue.ntVOID Type2, _, Type2right)) :: _ :: ( _, 
( MlyValue.ntVOID Type1, Type1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 in ()
end; ()))
 in ( LrTable.NT 15, ( result, Type1left, Type2right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.ntVOID Types1, _, Types1right)) :: _ :: ( _
, ( MlyValue.ntVOID Type1, Type1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  Type1 = Type1 ()
 val  Types1 = Types1 ()
 in ()
end; ()))
 in ( LrTable.NT 15, ( result, Type1left, Types1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : PlcParser_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun TARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun DIFFERENCE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LESS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun DOUBLECOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun HD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun ISE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACKETS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun NAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun REC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun MATCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun WITH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun EXCLAMATION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACKETS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun FARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun PIPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun UNDERSCORE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun NAME (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
end
end
