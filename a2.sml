(*
Evan Jacques
260413259
*)

(*Question 1 *)
fun findItem(lst, item) = 
	case lst of
		[] => false
		| h :: t => if item = h then true
				else findItem(t,item)

fun remDuplicates (lst) =
	case lst of
		[] => []
		| [h] => [h]
		| h::t => if findItem(t,h) then remDuplicates(t)
				else h::remDuplicates(t)

(*Question 2*)

fun absval(i:real) =
	if i < 0.0 then ~i
	else i

fun deriv_help (d, f, x) =
	(f(x + d)-f(x - d))/(2.0 * d)

fun newton (f, x, e) =
	let
		fun n_help (fh, xh, eh, count) =
			if (count < 1000) then
				let val app = absval (fh xh) 
					in 
						if (app < eh) then app
						else n_help (fh, (xh - (f(xh) / deriv_help (1.0E~8, fh, xh))), eh, (count + 1))
					end
			else
				let exception Diverge in
					raise Diverge
				end
	in
		n_help (f, x, e, 0)
	end
(*
(*Question 3*)
fun sum(f: real -> real, g: real -> real, a:real, b:real) =
	if (a > b) then 0
	else (f*g a) + sum(f,g,(a+1.0),b)
		

fun integral ( f, g, b:real, dy: real) = 
	fn n => dy * sum(f, g, 0.0, b)

---------- Couldnt figure out how to get the y variable implemented into the script -------------	
*)
(* Question 4 *)
datatype Mathexp = 
	Num of int
	| Var of string
	| Neg of Mathexp
	| Add of Mathexp * Mathexp
	| Mul of Mathexp * Mathexp

(*4.1*)
fun diff (m:Mathexp, x:string) =
	case m of
		Num i => Num(0)
		| Var m1 => if m1 = x then Num(1)
				else Num(0)
		| Neg m1 => Neg(diff(m1,x))
		| Add(m1,m2) => Add(diff(m1,x),diff(m2,x))
		| Mul(m1,m2) => if m1 = Var(x) andalso m2 = Var(x) then Mul(m1,diff(m2,x))
				else if m1 = Var(x) then Mul(diff(m1,x),m2)
				else if m2 = Var(x) then Mul(m1,diff(m2,x))
				else Mul(diff(m1,x),diff(m2,x))
(* 4.2*)
fun simplify(m:Mathexp) =
	case m of
		Num m1 => Num(m1)
		| Var m1 => Var(m1)
		| Neg m1 => if m1 = Num(0) then simplify(Num(0))
				else Neg(simplify(m1))
		| Add(m1,m2) => if m1 = Num(0) then simplify(m2)
				else if m2 = Num(0) then simplify(m1)
				else Add(simplify(m1), simplify(m2))
		| Mul(m1,m2) => if m1 = Num(0) then Num(0)
				else if m2 = Num(0) then Num(0)
				else if m1 = Num(1) then simplify(m2)
				else if m2 = Num(1) then simplify(m1)
				else Mul(simplify(m1), simplify(m2))
