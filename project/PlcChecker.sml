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
		| Item(i, e1) =>
                let
                    val v = teval e1 env
                in
                    case v of
                         ListT ts =>
                            if 0 < i andalso i <= List.length ts then
                                List.nth(ts, i-1)
                            else
                                raise ListOutOfRange
                        | _ => raise OpNonList
                end
		| Var x => lookup env x
		| Prim1(opr, e1) =>
				let
					val t1 = teval e1 env
				in
					case (opr, t1) of
						  ("-", IntT) => IntT
						| ("!", BoolT) => BoolT
						| ("print", _) => ListT []
						| ("hd" , SeqT s) => s
						| ("tl" , SeqT s) => SeqT s
						| ("ise" , SeqT s) => BoolT
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
					| _ => raise UnknownType
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
			  (* val t2 = teval e2 env
			  val t3 = teval e3 env *)
			in
			  case t1 of
			    BoolT => 
					if teval e2 env = teval e3 env then 
						teval e3 env
					else 
						raise DiffBrTypes
			  | _ => raise IfCondNotBool
			end
		| Anon (t, s, e) => 
			let 				
				val env2 = (s,t)::env
			in
				FunT (t, teval e env2)
			end
		| Call (f, e) => 
			let
			  val te = teval e env
			  val tf = teval f env
			in
				case tf of
					FunT(x, y) => if x = te then y else raise CallTypeMisM
				  | _ => raise CallTypeMisM
			end
		| Letrec (f, t1, x, t, e1, e2) => 
			let
				val env2 = (f,FunT(t1,t))::(x,t1)::env
				val v1 = teval e1 env2
			in					
				if v1 = t then 
					teval e2 env2
				else 
					raise WrongRetType				
			end
		| ESeq (SeqT t) => SeqT t
		| ESeq _ => raise EmptySeq 
		(* | Match *)
		| _   =>  raise UnknownType
