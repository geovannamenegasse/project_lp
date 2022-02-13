(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval (e:expr) (env:plcVal env) : plcVal =
	case e of
		  ConI i => IntV i
		| ConB b => BoolV b
		| List [] => ListV []
		| List e => 
			let
			  val v = map (fn x => eval x env) e
			in
			  ListV v
			end
		(* | Item (i, e) => 
			case e of
				  List [] => raise ListOutOfRange
				| List l => if i = 0 then eval (hd l) env else eval (Item (i-1, List (tl l))) env
				| _ => raise OpNonList *)
		| Var x => lookup env x
		| Prim1(opr, e1) =>
				let
					val v1 = eval e1 env
				in
					case (opr, v1) of
							("-", IntV i) => IntV (~i)
						| ("print", _) =>
										let
											val s = val2string v1
										in
											print(s^"\n"); ListV []
										end
						| _   => raise Impossible
				end
		| Prim2(opr, e1, e2) =>
				let
					val v1 = eval e1 env
					val v2 = eval e2 env
				in
					case (opr, v1, v2) of
						  ("*" , IntV i1, IntV i2) => IntV (i1 * i2)
						| ("/" , IntV i1, IntV i2) => IntV (i1 div i2)
						| ("+" , IntV i1, IntV i2) => IntV (i1 + i2)
						| ("-" , IntV i1, IntV i2) => IntV (i1 - i2)
						| ("<" , IntV i1, IntV i2) => BoolV (i1 < i2)
						| ("<=" , IntV i1, IntV i2) => BoolV (i1 <= i2)
						| ("&&" , BoolV i1, BoolV i2) => BoolV (i1 andalso i2)
						| ("!=" , IntV i1, IntV i2) => BoolV (i1 <> i2)
						| ("!=" , BoolV i1, BoolV i2) => BoolV (i1 <> i2)
						| ("=" , IntV i1, IntV i2) => BoolV (i1 = i2)
						| ("=" , BoolV i1, BoolV i2) => BoolV (i1 = i2)	
						| ("::" , _ , SeqV i2) => SeqV ([v1] @ i2)					
						| (";" , _ , _ ) => v2
						| _ => raise Impossible
						end
		| Let(x, e1, e2) =>
			let
				val v = eval e1 env
				val env2 = (x,v) :: env
			in
				eval e2 env2
			end
		| If(e1, e2, e3) => 
			let
			  val v1 = eval e1 env
			in
			  case v1 of
			    BoolV true => eval e2 env
			  | BoolV false => eval e3 env
			  | _ => raise Impossible
			end
		| Anon (_, s, e) => Clos ("", s, e, env) 
		| Call (e1, e2) => 
			let
			  val c = eval e1 env
			in
				case c of 
					Clos(f, p, b, envf) =>
						let 
							val v = eval e2 env 
							val env2 = (p,v)::(f,c)::envf
						in
							eval b env2
						end
				| _ => raise NotAFunc
			end
		(* | Match (e1, l) => 
			let
			  
			in
			  
			end *)
		| Letrec (f, tf, p, tp, ef, cf) => eval cf ((f, Clos(f,p,ef,env))::env)
		| ESeq (SeqT t) => SeqV []
		| ESeq _ => raise EmptySeq 
		| _ => raise Impossible

