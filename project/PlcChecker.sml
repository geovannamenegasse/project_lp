(* PlcChecker *)

exception EmptySeq
(* exception UnknownType *)
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
		| Var x => 
			lookupT env x
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

					| ("!=" , IntT, IntT) => if t1 = t2 then BoolT else raise NotEqTypes
					| ("=" , IntT, IntT) => if t1 = t2 then BoolT else raise NotEqTypes
					| ("!=" , BoolT, BoolT) => if t1 = t2 then BoolT else raise NotEqTypes
					| ("=" , BoolT, BoolT) => if t1 = t2 then BoolT else raise NotEqTypes	

					| ("!=" , ListT l1, ListT l2) => if t1 = t2 then BoolT else raise NotEqTypes
					| ("=" ,  ListT l1, ListT l2) => if t1 = t2 then BoolT else raise NotEqTypes
					| ("!=" , SeqT s1, SeqT s2)   => if t1 = t2 then BoolT else raise NotEqTypes
					| ("=" ,  SeqT s1, SeqT s2)   => if t1 = t2 then BoolT else raise NotEqTypes

					| ("::" , _ , SeqT i2) => if t1 = i2 then SeqT i2 else raise UnknownType
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
					FunT(x, y) => if x = te then 
									y 
								  else 
								  	raise CallTypeMisM
				  | _ => raise NotFunc
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
		| Match (e1, l) =>
			let
				fun check_x_param l1 e2 =
					case l1 of
					  [] => teval e2 env 
					| elem::rest =>
						case elem of 
						  (SOME (x), y) => 
							if teval x env = teval e2 env 
								then check_x_param rest e2
							else raise MatchCondTypesDiff
						| (NONE, y) => teval y env
				fun check_x l1 x1 =
					case l1 of
					  [] => teval x1 env 
					| elem::rest =>
						case elem of 
						  (SOME (x), y) => 
							if teval x env = teval x1 env 
								then  check_x rest x1
							else raise MatchCondTypesDiff
						| (NONE, y) => teval y env
				
				fun check_y l1 y1=
					case l1 of
					  [] => teval y1 env 
					| elem::rest =>
						case elem of 
						  (SOME (x), y) => 
							if teval y env = teval y1 env 
								then check_y rest y1
							else raise MatchResTypeDiff
						| (NONE, y) => if teval y env = teval y1 env 
										then teval y env
									   else raise MatchResTypeDiff
			in
				if l = [] then 
					raise NoMatchResults 
				else
					check_x_param l e1;

				case l of
					elem::rest =>
						case elem of 
						  (SOME (x), y) => check_x rest x
						| (NONE, y) => teval y env;

				case l of
					elem::rest =>
						case elem of 
						  (SOME (x), y) => check_y rest y
						| (NONE, y) => teval y env
			end
		(* | _   =>  raise UnknownType *)
