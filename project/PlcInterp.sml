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
		| _ => raise Impossible

