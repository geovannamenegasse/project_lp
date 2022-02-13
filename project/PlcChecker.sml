(* PlcChecker *)

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

fun teval (e:expr) (env: plcType env) : plcType =
	case e of
		  ConI i => IntT
		| ConB b => BoolT 
		| List [] => ListT []
		| List e => 
			let
			  val v = map (fn x => teval x env) e
			in
			  ListT v
			end
		(* | Item (i, e) => 
			case e of
				  List [] => raise ListOutOfRange
				| List l => if i = 0 then							
								teval (hd l) env 
						  	else 
								teval (Item (i-1, List (tl l))) env *)
		| Var x => lookup env x
		| Prim1(opr, e1) =>
				let
					val t1 = teval e1 env
				in
					case (opr, t1) of
						 ("print", _) => ListT []
						| _ => raise UnknownType
				end
		| Prim2(opr, e1, e2) =>
				let
					val t1 = teval e1 env
					val t2 = teval e2 env
				in
					case (opr, t1, t2) of
					  ("*" , IntT, IntT) => IntT
					| ("/" , IntT, IntT) => IntT
					| ("+" , IntT, IntT) => IntT
					| ("-" , IntT, IntT) => IntT
					| ("<" , IntT, IntT) => BoolT
					| ("<=" , IntT, IntT) => BoolT
					| ("&&" , BoolT, BoolT) => BoolT
					| ("!=" , IntT, IntT) => BoolT
					| ("!=" , BoolT, BoolT) => BoolT
					| ("=" , IntT, IntT) => BoolT
					| ("=" , BoolT, BoolT) => BoolT	
					| ("::" , _ , SeqT i2) => SeqT i2			
					| (";" , _ , _)    => t2
					| _   =>  raise UnknownType
				end
		| Let(x, e1, e2) =>
				let
					val t = teval e1 env
					val env' = (x,t)::env
				in
					teval e2 env'
				end
		| If(e1, e2, e3) => 
			let
			  val t1 = teval e1 env
			  val t2 = teval e2 env
			  val t3 = teval e3 env
			in
			  case t1 of
			    BoolT => if t2 = t3 then t2 else raise DiffBrTypes
			  | _ => raise IfCondNotBool
			end
		| Anon (t, s, e) => 
			let 				
				val env2 = (s,t)::env
			in
				FunT (t, teval e env2)
			end
		| Call (e2, e1) => 
			let
			  val t1 = teval e1 env
			  val t2 = teval e2 env
			in
			  t2 (* qual caso de erro tratar aqui? *)
			end
		(* | Letrec (f, t1, x, t, e1, c2) => eval cf ((f, Clos(f,p,ef,env))::env) *)
		| ESeq (SeqT t) => SeqT t
		| ESeq _ => raise EmptySeq 
		| _   =>  raise UnknownType
