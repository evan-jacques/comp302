(*Evan Jacques - 260413259*)
(*Question 1*)
datatype exp = Nat of int  | Bool of bool | Plus of exp * exp | Minus of exp * exp |
 Mult of exp * exp | If of exp * exp * exp |
 And of exp * exp | Not of exp | Eq of exp * exp |
 Var of string | Let of exp * (string * exp) | 
 Fun of string * string * exp |
 Apply of exp * exp

fun free_list (e : exp) : string list = 

    let

    fun cut(s, []) = []
		| cut(s, x::y) = if (s = x) then cut(s,y) else x::(cut(s,y))

    fun rem_dup ([]) = []
		| rem_dup (x::xs) = x::rem_dup(cut(x,xs))

	fun cases (Nat x) = []
	  | cases (Plus(a,b)) = cases(a)@cases(b)
	  | cases (Minus(a,b)) = cases(a)@cases(b)
	  | cases (Mult(a,b)) = cases(a)@cases(b)
	  | cases (If(a,b,exp3)) = cases(a)@cases(b)@cases(exp3)
	  | cases (Bool x) = []
	  | cases (And(a,b)) = cases(a)@cases(b)
	  | cases (Not a) = cases(a)
	  | cases (Eq(a,b)) =  cases(a)@cases(b)
	  | cases (Var string) = [string]
	  | cases (Let(a,(string,b))) = cases(a)@cut(string,cases(b))
	  | cases (Fun(string1,string2,exp)) = cut(string1, cut(string2, cases(exp)))
	  | cases (Apply(a,b)) =  cases(a)@cases(b)
    
    in
		rem_dup(cases(e))
    end
(*Question 2*)
(*Part 1*)
fun catalanfn (n, cn) = (2*(2*n + 1)*cn) div (n+2);

fun add (n) =
	let
		fun sum(i) = if (i<n) then i :: sum(i+1) else []
	in
		sum 0
	end

fun foldl(f, acc, lst) = 
 case lst of
 [] => acc
 | h::t => foldl(f, f(h,acc), t)

fun catalan x = foldl (catalanfn, 1, add x) ;

(*Part 2*)
datatype realSeq = Cons of real * (unit -> realSeq)

fun helperSeq n = Cons (2.0*(2.0 * real(n) + 1.0) / (real(n) + 2.0), fn () => helperSeq (n + 1));

fun take n s = case (n , s) of
  (0, Cons (x, f)) => []
| (n, Cons (x, f)) => x :: (take (n-1) (f ()))

val catalanSeq : realSeq =
	let
  		fun build cn (Cons (k, f)) = 
  			let
    				val cn' = k * cn
  			in
    				Cons (cn', fn () => build cn' (f ()))
  			end
	in
  		Cons(1.0, fn () => build 1.0 (helperSeq 0))
	end;


(*Question 3*)
exception NoData

datatype 'a instr = Put of 'a | Get | Restore


fun makeCell input =

	let
	  
	  val data = ref [input]
	  
	  fun select (Put n) = 
	  		let
	    			val l = !data
	  		in
	    			data := n::l;
	    			n
	  		end
	    | select (Get) =
	  		let
	    			val l = !data
	  		in
	    			hd l
	  		end
	    | select (Restore) =
	  		let
	    			val l = !data
	  		in
	    			if null l then (print "Nothing to restore" ; raise NoData)
	    			else let val (x::xs) = l in (data := xs ; x) end
	  		end
	in
	  select
	end

