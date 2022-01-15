functor PlcLexerFun(structure Tokens: PlcParser_TOKENS)=
   struct
    structure UserDeclarations =
      struct
(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

fun identifier(s, lpos, rpos) = 
    case s of
       "var" => (Tokens.VAR(lpos, rpos))
     | "if" => (Tokens.IF(lpos, rpos))
     | "Bool" => (Tokens.BOOL(lpos,rpos))
     | "else" => (Tokens.ELSE(lpos,rpos))
     | "end" => (Tokens.END(lpos,rpos))
     | "false" => (Tokens.FALSE(lpos,rpos))
     | "fn" => (Tokens.FN(lpos,rpos))
     | "fun" => (Tokens.FUN(lpos,rpos))
     | "hd" => (Tokens.HD(lpos,rpos))
     | "Int" => (Tokens.INT(lpos,rpos))
     | "match" => (Tokens.MATCH(lpos,rpos))
     | "Nil" => (Tokens.NIL(lpos,rpos))
     | "print" => (Tokens.PRINT(lpos,rpos))
     | "rec" => (Tokens.REC(lpos,rpos))
     | "then" => (Tokens.THEN(lpos,rpos))
     | "tl" => (Tokens.TL(lpos,rpos))
     | "true" => (Tokens.TRUE(lpos,rpos))
     | "with" => (Tokens.WITH(lpos,rpos))
     | "ise" => (Tokens.ISE(lpos,rpos))
     | _ => (Tokens.NAME(s, lpos, rpos));

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

fun strToInt s = 
    case Int.fromString s of 
          SOME i => i
        | NONE => raise Fail ("Could not convert string to int")

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\000\000\000\000\000\000\000\000\000\030\031\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\030\028\000\000\000\000\026\000\025\024\023\022\021\019\000\018\
\\017\017\017\017\017\017\017\017\017\017\015\014\012\010\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\009\000\008\000\007\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\005\004\003\000\000\
\\000"
),
 (6, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (10, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\011\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (12, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (15, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\016\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (17, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\017\017\017\017\017\017\017\017\017\017\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (19, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\020\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (26, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\027\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (28, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (30, 
"\000\000\000\000\000\000\000\000\000\030\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\030\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 48)], trans = 0},
{fin = [(N 62)], trans = 0},
{fin = [(N 46)], trans = 0},
{fin = [(N 7)], trans = 6},
{fin = [(N 7),(N 64)], trans = 6},
{fin = [(N 44)], trans = 0},
{fin = [(N 42)], trans = 0},
{fin = [(N 14)], trans = 10},
{fin = [(N 55)], trans = 0},
{fin = [(N 34)], trans = 12},
{fin = [(N 37)], trans = 0},
{fin = [(N 12)], trans = 0},
{fin = [(N 16)], trans = 15},
{fin = [(N 40)], trans = 0},
{fin = [(N 10)], trans = 17},
{fin = [(N 29)], trans = 0},
{fin = [(N 20)], trans = 19},
{fin = [(N 58)], trans = 0},
{fin = [(N 60)], trans = 0},
{fin = [(N 25)], trans = 0},
{fin = [(N 27)], trans = 0},
{fin = [(N 52)], trans = 0},
{fin = [(N 50)], trans = 0},
{fin = [], trans = 26},
{fin = [(N 23)], trans = 0},
{fin = [(N 18)], trans = 28},
{fin = [(N 32)], trans = 0},
{fin = [(N 4)], trans = 30},
{fin = [(N 1)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput =
let	val yygone0=1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => (lineNumber := !lineNumber + 1; lex())
| 10 => let val yytext=yymktext() in Tokens.NAT(strToInt(yytext), !lineNumber, !lineNumber) end
| 12 => (Tokens.SEMICOLON(!lineNumber, !lineNumber))
| 14 => (Tokens.EQUAL(!lineNumber, !lineNumber))
| 16 => (Tokens.COLON(!lineNumber, !lineNumber))
| 18 => (Tokens.EXCLAMATION(!lineNumber, !lineNumber))
| 20 => (Tokens.MINUS(!lineNumber, !lineNumber))
| 23 => (Tokens.AND(!lineNumber, !lineNumber))
| 25 => (Tokens.PLUS(!lineNumber, !lineNumber))
| 27 => (Tokens.TIMES(!lineNumber, !lineNumber))
| 29 => (Tokens.DIVIDE(!lineNumber, !lineNumber))
| 32 => (Tokens.DIFFERENCE(!lineNumber, !lineNumber))
| 34 => (Tokens.LESS(!lineNumber, !lineNumber))
| 37 => (Tokens.LE(!lineNumber, !lineNumber))
| 4 => (lex())
| 40 => (Tokens.DOUBLECOLON(!lineNumber, !lineNumber))
| 42 => (Tokens.LBRACKETS(!lineNumber, !lineNumber))
| 44 => (Tokens.RBRACKETS(!lineNumber, !lineNumber))
| 46 => (Tokens.LBRACES(!lineNumber, !lineNumber))
| 48 => (Tokens.RBRACES(!lineNumber, !lineNumber))
| 50 => (Tokens.LPAREN(!lineNumber, !lineNumber))
| 52 => (Tokens.RPAREN(!lineNumber, !lineNumber))
| 55 => (Tokens.TARROW(!lineNumber, !lineNumber))
| 58 => (Tokens.FARROW(!lineNumber, !lineNumber))
| 60 => (Tokens.COMMA(!lineNumber, !lineNumber))
| 62 => (Tokens.PIPE(!lineNumber, !lineNumber))
| 64 => (Tokens.UNDERSCORE(!lineNumber, !lineNumber))
| 7 => let val yytext=yymktext() in identifier(yytext, !lineNumber, !lineNumber) end
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Unsafe.Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(Unsafe.CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(Unsafe.CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
