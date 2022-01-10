%%

%name PlcParser

%pos int

%term 
PONTO_VIRGULA
VAR
IGUAL
FUN
REC
DOIS_PONTOS
IF
THEN
ELSE
MATCH
WITH
EXCLAMACAO
MENOS
HD
TL
ISE
PRINT
AND
PLUS
TIMES
DIV
DIFERENTE
MENOR
MENOR_IGUAL
DOIS_PONTOS_DUPLO
ABRE_COLCHETE
FECHA_COLCHETE
ABRE_CHAVE
FECHA_CHAVE
ABRE_PARENTESE
FECHA_PARENTESE
FN
SETA_GROSSA
SETA_FINA
END
TRUE
FALSE
VIRGULA
PIPE
ABRE_ASPAS_SIMPLES
UNDERSCORE
FECHA_ASPAS_SIMPLES
NAT
NAME

%nonterm 
  Prog
| Decl
| Expr
| AtomicExpr
| AppExpr
| Const
| Comps
| MatchExpr
| CondExpr
| Args
| Params
| TypedVar
| Type
| AtomicType
| Types

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr 
     | Decl PONTO_VIRGULA Prog

Decl : VAR NAME IGUAL Expr
     | FUN NAME Args IGUAL Expr
     | FUN REC NAME Args DOIS_PONTOS Type IGUAL Expr

Expr : AtomicExpr 
     | AppExpr 
     | IF Expr THEN Expr ELSE Expr 
     | MATCH Expr WITH MatchExpr
     | EXCLAMACAO Expr 
     | MENOS Expr
     | HD Expr
     | TL Expr
     | ISE Expr
     | PRINT Expr
     | Expr AND Expr 
     | Expr PLUS Expr
     | Expr MENOS Expr
     | Expr TIMES Expr
     | Expr DIV Expr
     | Expr IGUAL Expr
     | Expr DIFERENTE Expr
     | Expr MENOR Expr
     | Expr MENOR_IGUAL Expr
     | Expr DOIS_PONTOS_DUPLO Expr
     | Expr PONTO_VIRGULA Expr
     | Expr ABRE_COLCHETE NAT FECHA_COLCHETE

AtomicExpr : Const 
           | NAME 
           | ABRE_CHAVE Prog FECHA_CHAVE 
           | ABRE_PARENTESE Expr FECHA_PARENTESE
           | ABRE_PARENTESE Comps FECHA_PARENTESE 
           | FN Args SETA_GROSSA Expr END

AppExpr : AtomicExpr AtomicExpr
        | AppExpr AtomicExpr

Const : TRUE 
      | FALSE
      | NAT 
      | ABRE_PARENTESE FECHA_PARENTESE 
      | ABRE_PARENTESE Type ABRE_COLCHETE FECHA_COLCHETE FECHA_PARENTESE

Comps : Expr VIRGULA Expr
      | Expr VIRGULA Comps

MatchExpr : END
          | PIPE CondExpr SETA_FINA Expr MatchExpr

CondExpr : Expr
         | UNDERSCORE

Args : ABRE_PARENTESE FECHA_PARENTESE
     | ABRE_PARENTESE Params FECHA_PARENTESE

Params : TypedVar
       | TypedVar VIRGULA Params

TypedVar : Type NAME 

Type : AtomicType
     | ABRE_PARENTESE Types FECHA_PARENTESE 
     | ABRE_COLCHETE Type FECHA_COLCHETE
     | Type SETA_FINA Type 

AtomicType : Nil 
           | Bool 
           | Int 
           | ABRE_PARENTESE Type FECHA_PARENTESE

Types : Type VIRGULA Type
      | Type VIRGULA Types
