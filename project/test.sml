(* Infrastructure to run the Plc interpreter *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";
use "Parse.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;
use "PlcChecker.sml";
use "PlcInterp.sml";
use "Plc.sml";

use "testParserCases.sml";
(* 
   Avalia todos os testes presentes em testParserCases. 
   Os primeiros testes sao bem tipados e os últimos lancam excecoes.
   Para rodar, basta descomentar a linha testCases cases;.
*)
fun testCases c = map (fn (x,y) => let val v = run(fromString x) in print(x^"\n'"^v^"'\n\n"); v end) c; 
(* testCases cases; *)

(*
   Existem vários arquivos de exemplo na pasta tests. Para rodar todos eles, basta descomentar a linha 
   abaixo. Ela irá gerar uma lista com o resutado da avaliacao de todos os arquivos plc. A linha abaixo 
   dessa, serve para testar arquivo por arquivo, basta mudar o nome pro arquivo que quiser.
*)
(* List.tabulate(17, fn x => run(fromFile ("tests/t"^Int.toString(x)^".plc"))); *)
(* val abs = fromFile "tests/t13.plc"; *)
val abs = fromFile "example.plc";

(* val abs = fromString "fun f(Int x, Int b) = match b with | 2 -> {x + 1} | _ -> x end; f(3,2)"; *)
(* val abs = Letrec("f",BoolT,"x",BoolT,If (Var "x",ConI 11,ConI 22), Call (Var "f",ConB true)); *)

val v = run(abs);
(* val venv = []; *)
(* val v = eval abs venv; *)
(* val v = teval abs venv; *)
