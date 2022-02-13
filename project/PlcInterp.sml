(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

(* fun makeList (x:plcVal list) = 
	case x of 
		  [] => []
		| y::xs => y::makeList(xs); *)

fun eval (e:expr) (env:plcVal env) : plcVal =
	case e of
		  ConI i => IntV i
		| ConB b => BoolV b
		| List [] => ListV []
		| List [e1, e2, e3, e4] => ListV[eval e1 env, eval e2 env, eval e3 env, eval e4 env]
		| List e => ListV [eval (hd e) env]
			(* let
				val vi = eval (hd e) env
				val vb = eval (List (tl e)) env
				val lista = makeList(vb);
			in
				ListV lista
			end *)
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
						| (";" , _ , _) => v2
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
			  val v2 = eval e2 env
			  val v3 = eval e3 env
			in
			  case v1 of
			    BoolV true => v2
			  | BoolV false => v3
			  | _ => raise Impossible
			end
		| Anon (_, s, e) => 
			let
				(* val env1 = (s,eval e env)::env *)
				(* val ev = eval e env1 *)
				(* val env2 = (s, ev)::env *)
			in
				Clos ("", s, e, env) 
			end
		| Call (e1, e2) => 
			let
			  val n = eval e1 env
			  val v = eval e2 env
			in
			  n
			end
		| Match (e1, l) => eval e1 env
		| Letrec (s, t, s2, t2, e1, e2) => eval e1 env
		| ESeq t => raise EmptySeq
		| _ => raise Impossible

