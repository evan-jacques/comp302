(*Evan Jacques
260413259*)

(* Question 1 *)
datatype 'a rlist = Empty | RCons of 'a * (('a rlist) ref)

fun insert (comp,item, lst) =
	case !lst of
	Empty => lst := RCons(item,ref Empty)
	| RCons (h,t) => 
		if (comp (item,h)) then (lst := RCons(item, ref(RCons(h,ref Empty))))
		else insert (comp,item,t)

(* Question 2 *)
datatype transactions = 
	Withdraw of int
	| Deposit of int
	| Check_balance

fun make__protected_account opening_balance password = 
	let
		val balance = ref opening_balance
		val pass = ref password
	in
		fn (trans: transactions,p: string) =>
			if p = !pass then
				case trans of
				Withdraw(a) 
					=> ((balance := !balance-a); !balance)
				| Deposit(a) 
					=> ((balance := !balance+a); !balance)
				| Check_balance
					=> (!balance)
			else (print ("Wrong password.\n"); 0)
	end

(* Question 4 *)

(*


(* reduce: 'a list * 'a * ('a * 'a -> 'a) -> 'a *)
fun reduce (nil, base, op) = base
 | reduce (h::t, base, op) = op (h, reduce(t, base, op)

(* reduce_tr: 'a list * 'a * ('a * 'a -> 'a) -> 'a *)
fun reduce_tr (nil, base, op) = base
 | reduce_tr (h::t, base, op) = reduce_tr(t, op(h,base), op)


Lemma:
For any h,l,n,op
op(h,reduce(l,n,op)) = reduce_tr(l,op(h,n),op)

Proof: 
reduce_tr(l,op(h,n),op) == op(h,reduce_tr(l,op(h,n),op) by property a
op(h,reduce_tr(l,op(h,n),op) == op(h,reduce_tr(l,n,op)) by property a

We can see at each iteration that the same operations are performed on l,n,op so the above must be equivalent.

Lemma:
For all h,l,n,op
reduce(l,n,op) = reduce_tr(l,h,op)

Base case: Given an empty list, h and op, we can see that the base case is returned for both functions due to the structure.

Induction hypothesis: Set list to size n + 1. Compared to the first lemma that we proved, only the size of the list changes when the element is removed. This does not cause the overall structure to change therefore concluding that the two are equivalent. If it is the final element that is being remove, then the base case is taken care of.
*)
