(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

fun keyword(s, lpos, rpos) = 
    case s of
         "var" => (Tokens.VAR(lpos, rpos))
     |   "if" => (Tokens.IF(lpos, rpos))
     |   "Bool" => (Tokens.BOOL(lpos,rpos))
     |   "else" => (Tokens.ELSE(lpos,rpos))
     |   "end" => (Tokens.END(lpos,rpos))
     |   "false" => (Tokens.FALSE(lpos,rpos))
     |   "fn" => (Tokens.FN(lpos,rpos))
     |   "fun" => (Tokens.FUN(lpos,rpos))
     |   "hd" => (Tokens.HD(lpos,rpos))
     |   "Int" => (Tokens.INT(lpos,rpos))
     |   "match" => (Tokens.MATCH(lpos,rpos))
     |   "Nil" => (Tokens.NIL(lpos,rpos))
     |   "print" => (Tokens.PRINT(lpos,rpos))
     |   "rec" => (Tokens.REC(lpos,rpos))
     |   "then" => (Tokens.THEN(lpos,rpos))
     |   "tl" => (Tokens.TL(lpos,rpos))
     |   "true" => (Tokens.TRUE(lpos,rpos))
     |   "with" => (Tokens.WITH(lpos,rpos))
     |   "ise" => (Tokens.ISE(lpos,rpos))

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

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
digit=[0-9];
ws = [\ \t];
str=[a-zA-Z_][a-zA-Z_0-9];
%%

\n          => (lineNumber := (!lineNumber) + 1; Tokens.EOF(!lineNumber, !lineNumber));
{ws}+       => (lex());
{str}*      => (keyword(yytext, !lineNumber, !lineNumber));
{digit}+    => (Tokens.NAT(valOf (Int.fromString yytext), !lineNumber, !lineNumber));

";"         => (Tokens.SEMICOLON(!lineNumber, !lineNumber));
"="         => (Tokens.EQUAL(!lineNumber, !lineNumber));
":"         => (Tokens.COLON(!lineNumber, !lineNumber));
"!"         => (Tokens.EXCLAMATION(!lineNumber, !lineNumber));
"-"         => (Tokens.MINUS(!lineNumber, !lineNumber));
"&&"        => (Tokens.AND(!lineNumber, !lineNumber));
"+"         => (Tokens.PLUS(!lineNumber, !lineNumber));
"*"         => (Tokens.TIMES(!lineNumber, !lineNumber));
"/"         => (Tokens.DIVIDE(!lineNumber, !lineNumber));
"!="        => (Tokens.DIFFERENCE(!lineNumber, !lineNumber));
"<"         => (Tokens.LESS(!lineNumber, !lineNumber));
"<="        => (Tokens.LE(!lineNumber, !lineNumber));
"::"        => (Tokens.DOUBLECOLON(!lineNumber, !lineNumber));
"["         => (Tokens.LBRACKETS(!lineNumber, !lineNumber));
"]"         => (Tokens.RBRACKETS(!lineNumber, !lineNumber));
"{"         => (Tokens.LBRACES(!lineNumber, !lineNumber));
"}"         => (Tokens.RBRACES(!lineNumber, !lineNumber));
"("         => (Tokens.LPAREN(!lineNumber, !lineNumber));
")"         => (Tokens.RPAREN(!lineNumber, !lineNumber));
"=>"        => (Tokens.TARROW(!lineNumber, !lineNumber));
"->"        => (Tokens.FARROW(!lineNumber, !lineNumber));
","         => (Tokens.COMMA(!lineNumber, !lineNumber));
"|"         => (Tokens.PIPE(!lineNumber, !lineNumber));
"_"         => (Tokens.UNDERSCORE(!lineNumber, !lineNumber));
