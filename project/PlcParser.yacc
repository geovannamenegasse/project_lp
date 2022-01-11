%%

%name PlcParser

%pos int

%term 
  SEMICOLON
| VAR
| EQUAL
| FUN
| REC
| COLON
| IF
| THEN
| ELSE
| MATCH
| WITH
| EXCLAMATION
| MINUS
| HD
| TL
| ISE
| PRINT
| AND
| PLUS
| TIMES
| DIVIDE
| DIFFERENCE
| LESS
| LE
| DOUBLECOLON
| LBRACKETS
| RBRACKETS
| LBRACES
| RBRACES
| LPAREN
| RPAREN
| FN
| TARROW
| FARROW
| END
| TRUE
| FALSE
| COMMA
| PIPE
| UNDERSCORE
| NAT
| NAME

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
     | Decl SEMICOLON Prog

Decl : VAR NAME EQUAL Expr
     | FUN NAME Args EQUAL Expr
     | FUN REC NAME Args COLONType EQUAL Expr

Expr : AtomicExpr 
     | AppExpr 
     | IF Expr THEN Expr ELSE Expr 
     | MATCH Expr WITH MatchExpr
     | EXCLAMATION Expr 
     | MINUS Expr
     | HD Expr
     | TL Expr
     | ISE Expr
     | PRINT Expr
     | Expr AND Expr 
     | Expr PLUS Expr
     | Expr MINUS Expr
     | Expr TIMES Expr
     | Expr DIVIDE Expr
     | Expr EQUAL Expr
     | Expr DIFFERENCE Expr
     | Expr LESS Expr
     | Expr LE Expr
     | Expr DOUBLECOLON Expr
     | Expr SEMICOLON Expr
     | Expr LBRACKETS NAT RBRACKETS

AtomicExpr : Const 
           | NAME 
           | LBRACES Prog RBRACES 
           | LPAREN Expr RPAREN
           | LPAREN Comps RPAREN 
           | FN Args TARROW Expr END

AppExpr : AtomicExpr AtomicExpr
        | AppExpr AtomicExpr

Const : TRUE 
      | FALSE
      | NAT 
      | LPAREN RPAREN 
      | LPAREN Type LBRACKETS RBRACKETS RPAREN

Comps : Expr COMMA Expr
      | Expr COMMA Comps

MatchExpr : END
          | PIPE CondExpr FARROW Expr MatchExpr

CondExpr : Expr
         | UNDERSCORE

Args : LPAREN RPAREN
     | LPAREN Params RPAREN

Params : TypedVar
       | TypedVar COMMA Params

TypedVar : Type NAME 

Type : AtomicType
     | LPAREN Types RPAREN 
     | LBRACKETS Type RBRACKETS
     | Type FARROW Type 

AtomicType : Nil 
           | Bool 
           | Int 
           | LPAREN Type RPAREN

Types : Type COMMA Type
      | Type COMMA Types
