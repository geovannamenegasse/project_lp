(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

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
str=[a-zA-Z_][a-zA-Z_0-9];
%%

\n       => (lineNumber := (!lineNumber) + 1; Tokens.EOF(!lineNumber, !lineNumber));

{str}*      => (Tokens.NAME(getLineAsString(), !lineNumber, !lineNumber));
{digit}+    => (Tokens.NAT(valOf (Int.fromString yytext), !lineNumber, !lineNumber));

";"         => (Tokens.SEMICOLON(!lineNumber, !lineNumber));
"end"       => (Tokens.END(!lineNumber, !lineNumber));
"true"      => (Tokens.TRUE(!lineNumber, !lineNumber));
"false"     => (Tokens.FALSE(!lineNumber, !lineNumber));
"var"       => (Tokens.VAR(!lineNumber, !lineNumber));
"="         => (Tokens.EQUAL(!lineNumber, !lineNumber));
"fun"       => (Tokens.FUN(!lineNumber, !lineNumber));
"rec"       => (Tokens.REC(!lineNumber, !lineNumber));
":"         => (Tokens.COLON(!lineNumber, !lineNumber));
"if"        => (Tokens.IF(!lineNumber, !lineNumber));
"then"      => (Tokens.THEN(!lineNumber, !lineNumber));
"else"      => (Tokens.ELSE(!lineNumber, !lineNumber));
"match"     => (Tokens.MATCH(!lineNumber, !lineNumber));
"with"      => (Tokens.WITH(!lineNumber, !lineNumber));
"!"         => (Tokens.EXCLAMATION(!lineNumber, !lineNumber));
"-"         => (Tokens.MINUS(!lineNumber, !lineNumber));
"hd"        => (Tokens.HD(!lineNumber, !lineNumber));
"tl"        => (Tokens.TL(!lineNumber, !lineNumber));
"ise"       => (Tokens.ISE(!lineNumber, !lineNumber));
"print"     => (Tokens.PRINT(!lineNumber, !lineNumber));
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
"fn"        => (Tokens.FN(!lineNumber, !lineNumber));
"=>"        => (Tokens.TARROW(!lineNumber, !lineNumber));
"->"        => (Tokens.FARROW(!lineNumber, !lineNumber));
","         => (Tokens.COMMA(!lineNumber, !lineNumber));
"|"         => (Tokens.PIPE(!lineNumber, !lineNumber));
"_"         => (Tokens.UNDERSCORE(!lineNumber, !lineNumber));
