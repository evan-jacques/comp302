(*Evan Jacques
260413259*)

Control.Print.printDepth:=100;

datatype 'a tree =
	Empty
	| Node of (int * string) * 'a tree * 'a tree
(*Question 1*)
fun lessFour x = if x < 4 then true else false
val tinytree = Node((3,"a"),Node((2,"c"),Empty,Empty),Node((4,"b"),Empty,Empty))

(*1.1*)
exception Found of (int*string) list

fun collect p t =
	case t of
		Empty => []
		| Node((i,s),L,R) => 
				(if (p i) then (raise Found [(i,s)])
				else (collect p L) )handle Found [(i,s)] => (collect p L) @ [(i,s)] @ (collect p R)
(*1.2*)
fun gather p t cont = 
	case t of 
		Empty => cont()
		| Node((i, s), L, R) => 
			if (p i) then (gather p L cont)@[(i,s)]@(gather p R cont)
			else gather p L (fn () => gather p R cont)

(*  Question 2  *)
(*
We want to prove that for all k, 
	fun pow(n, k) == fun pow_tl(n, k, 1)

Base Case: k = 0
	
	fun pow(n, 0) = 1 by definition

	fun pow_tl(n, 0, 1) = acc by definition
		acc = 1, so
		pow_tl(n, 0, 1) = 1

	fun pow(n, 0) == fun pow_tl(n, 0, 1)

Induction: 
	Hypothesis : fun pow(n,k) == fun pow_tl(n,k,1)
	Show that : fun pow(n,k+1) == fun pow_tl(n,k+1,1)

	Proof :
		fun pow(n,k+1) == n*pow(n,k)

		fun pow_tl(n,k+1,1) == pow_tl(n,k,n*1) == n*pow_tl(n,k,1)

	By our Induction Hypothesis that fun pow(n,k) == fun pow_tl(n,k,1), it is also valid that n*pow(n,k) == n*pow_tl(n,k,1) and therefore it is always true.

*)

(*  Question 3  *)

type 'a church = ('a -> 'a) * 'a -> 'a

val Z = fn (f : 'a -> 'a, x : 'a) => x
val O = fn (f : 'a -> 'a, x : 'a) => f(x)
val T = fn (f : 'a -> 'a, x : 'a) => f(f(x))

(* 3.1 *)


fun apply_n_times f n x =
	if n = 0 then x
	else f(apply_n_times f (n-1) x)


fun create n : 'a church =
	if n = 0 then fn (f, x) => x
	else fn (f, x) => apply_n_times f n x


(* 3.2 *)


fun churchToInt m : int=
	m((fn x => x + 1), 0)

(* 3.3 *)


fun SUCC j : 'a church =
	fn(f, x) => f(j (f, x))
