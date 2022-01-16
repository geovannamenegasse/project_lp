%%

%name PlcParser

%pos int

%term SEMICOLON 
     | TARROW
     | IF
     | ELSE
     | AND
     | EQUAL
     | DIFFERENCE
     | LESS
     | LE
     | DOUBLECOLON
     | PLUS
     | MINUS
     | TIMES
     | DIVIDE
     | NOT
     | HD
     | TL
     | ISE
     | PRINT
     | LBRACKETS
     | NAT of int
     | VAR      
     | FUN
     | REC
     | COLON     
     | THEN     
     | MATCH
     | WITH
     | EXCLAMATION     
     | RBRACKETS
     | LBRACES
     | RBRACES
     | LPAREN
     | RPAREN
     | FN     
     | FARROW
     | END     
     | COMMA
     | PIPE
     | UNDERSCORE
     | NAME of string
     | TRUE
     | FALSE
     | EOF
     | INT
     | BOOL
     | NIL

%nonterm Start of expr
     | Prog of expr
     | Decl of expr
     | Expr of expr
     | AtomicExpr of expr
     | AppExpr of expr
     | Const of expr
     | Comps of expr list
     | MatchExpr of (expr option * expr) list
     | CondExpr of expr option
     | Args of (plcType * string) list
     | Params of (plcType * string) list
     | TypedVar of plcType * string
     | Type of plcType
     | AtomicType of plcType
     | Types of plcType list

%right SEMICOLON TARROW 
%nonassoc IF 
%left ELSE
%left AND
%left EQUAL DIFFERENCE
%left LESS LE

%right DOUBLECOLON

%left PLUS MINUS
%left TIMES DIVIDE

%nonassoc NOT HD TL ISE PRINT NAME
%left LBRACKETS

%eop EOF

%noshift EOF

%start Start

%%

Start : Prog                                                    (Prog)

Prog : Expr                                                     (Expr)
     | Decl                                                     (Decl)

Decl : VAR NAME EQUAL Expr SEMICOLON Prog                       (Let(NAME,Expr,Prog))
     | FUN NAME Args EQUAL Expr SEMICOLON Prog                  (Let(NAME, makeAnon(Args, Expr), Prog))
     | FUN REC NAME Args COLON Type EQUAL Expr SEMICOLON Prog   (makeFun(NAME, Args, Type, Expr, Prog))

Expr : AtomicExpr                                 (AtomicExpr)
     | AppExpr                                    (AppExpr)
     | IF Expr THEN Expr ELSE Expr                (If(Expr1, Expr2, Expr3))
     | MATCH Expr WITH MatchExpr                  (Match(Expr, MatchExpr))
     | EXCLAMATION Expr                           (Prim1("!", Expr))
     | MINUS Expr                                 (Prim1("-", Expr))
     | HD Expr                                    (Prim1("hd", Expr))
     | TL Expr                                    (Prim1("tl", Expr))
     | ISE Expr                                   (Prim1("ise", Expr))
     | PRINT Expr                                 (Prim1("print", Expr))
     | Expr AND Expr                              (Prim2("&&", Expr1, Expr2))
     | Expr PLUS Expr                             (Prim2("+", Expr1, Expr2))
     | Expr MINUS Expr                            (Prim2("-", Expr1, Expr2))
     | Expr TIMES Expr                            (Prim2("*", Expr1, Expr2))
     | Expr DIVIDE Expr                           (Prim2("/", Expr1, Expr2))
     | Expr EQUAL Expr                            (Prim2("=", Expr1, Expr2))
     | Expr DIFFERENCE Expr                       (Prim2("!=", Expr1, Expr2))
     | Expr LESS Expr                             (Prim2("<", Expr1, Expr2))
     | Expr LE Expr                               (Prim2("<=", Expr1, Expr2))
     | Expr DOUBLECOLON Expr                      (Prim2("::", Expr1, Expr2))
     | Expr SEMICOLON Expr                        (Prim2(";", Expr1, Expr2))
     | Expr LBRACKETS NAT RBRACKETS               (Item(NAT,Expr))

AtomicExpr : Const                                (Const)
           | NAME                                 (Var(NAME))
           | LBRACES Prog RBRACES                 (Prog)
           | LPAREN Expr RPAREN                   (Expr)
           | LPAREN Comps RPAREN                  (List Comps)
           | FN Args TARROW Expr END              (makeAnon(Args, Expr))

AppExpr : AtomicExpr AtomicExpr                   (Call(AtomicExpr1, AtomicExpr2))
        | AppExpr AtomicExpr                      (Call(AppExpr, AtomicExpr))

Const : TRUE                                      (ConB true)
      | FALSE                                     (ConB false)
      | NAT                                       (ConI NAT)
      | LPAREN RPAREN                             (List [])
      | LPAREN Type LBRACKETS RBRACKETS RPAREN    (ESeq(Type))

Comps : Expr COMMA Expr                           (Expr1::Expr2::[])
      | Expr COMMA Comps                          (Expr::Comps)

MatchExpr : END                                   ([])
          | PIPE CondExpr FARROW Expr MatchExpr   ((CondExpr, Expr)::MatchExpr)

CondExpr : Expr                                   (SOME(Expr))
         | UNDERSCORE                             (NONE)

Args : LPAREN RPAREN                              ([])
     | LPAREN Params RPAREN                       (Params)

Params : TypedVar                                 (TypedVar::[])
       | TypedVar COMMA Params                    (TypedVar::Params)

TypedVar : Type NAME                              ((Type, NAME))

Type : AtomicType                                 (AtomicType)
     | LPAREN Types RPAREN                        (ListT Types)
     | LBRACKETS Type RBRACKETS                   (SeqT Type)
     | Type FARROW Type                           (FunT(Type1, Type2))

AtomicType : NIL                                  (ListT [])
           | BOOL                                 (BoolT)
           | INT                                  (IntT)
           | LPAREN Type RPAREN                   (Type)

Types : Type COMMA Type                           (Type1::Type2::[])
      | Type COMMA Types                          (Type::Types)
