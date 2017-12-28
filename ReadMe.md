[Course Website](https://fos2015.github.io)

## Project 1: The NB Language

	t  ::= "true"                   terms
     | "false"
     | "if" t "then" t "else" t
     | numericLiteral
     | "succ" t
     | "pred" t
     | "iszero" t

	v  ::= "true"                   values
     | "false"
     | nv

	nv ::= 0                        numeric values
     | "succ" nv
     
Implement term parser that recognizes this language and Implement reduce method which performs one step of the evaluation

## Project 2: Untyped Lambda Calculus

	t ::= ident              terms
    | "\" ident "." t
    | t t
    | "(" t ")"

	v ::= "\" ident "." t    values
	
Evaluation Rules:

		t1 → t1'
	--------------
	t1 t2 → t1' t2
	
	   t2 → t2'
	--------------
	t1 t2 → t1 t2'
	
	   t1 → t1'
	----------------
	λx. t1 → λx. t1'
	
	(λx. t1) t2 → [x → t2] t1
	
Implement noral-order strategy and call-by-value evaluation strategy.

## Project 3: Simply Typed Lambda Calculus
Implement a type checker and a reducer for simply typed λ-terms.

## Project 4: STLC Extensions
Add sum type

	t ::= ...                                                terms
	    | "inl" t "as" T                                     inject left
	    | "inr" t "as" T                                     inject right
	    | "case" t "of" "inl" x "=>" t "|" "inr" x "=>" t    case
	
	v ::= ...                                                values
	    | "inl" v "as" T
	    | "inr" v "as" T
	
	T ::= ...                                                types
	    | T "+" T                                            sum type (right assoc.)

Add and Implement new evaluation rules to ensure progress and preservation holds.

## Project 5: STLC with Type Reconstruction
Implement a Hindley-Milner type inferencer for a modification of the simply typed λ-calculus with booleans, numbers and let-polymorphism.

Phase 1: Collecting constraints
	
	Γ ⊢ true : Bool | {}
	
	Γ ⊢ false : Bool | {}
	
	Γ ⊢ 0 : Nat | {}
	
	Γ ⊢ t : T | C    C' = C ∪ {T=Nat}
	---------------------------------
	    Γ ⊢ pred t : Nat | C'
	
	Γ ⊢ t : T | C    C' = C ∪ {T=Nat}
	---------------------------------
	    Γ ⊢ succ t : Nat | C'
	
	Γ ⊢ t : T | C    C' = C ∪ {T=Nat}
	---------------------------------
	   Γ ⊢ iszero t : Bool | C'
	
	 Γ ⊢ t1: T1 | C1    Γ ⊢ t2 : T2 | C2
	          Γ ⊢ t3 : T3 | C3
	  C = C1 ∪ C2 ∪ C3 ∪ {T1=Bool, T2=T3}
	-------------------------------------
	  Γ ⊢ if t1 then t2 else t3 : T2 | C
	
	   x : T ∈ Γ
	--------------
	Γ ⊢ x : T | {}
	
	   Γ, x : T1 ⊢ t : T2 | C
	-----------------------------
	Γ ⊢ λx: T1. t : T1 -> T2 | C
	
	 Γ, x : X ⊢ t : T2 | C
	      X is fresh
	------------------------
	Γ ⊢ λx. t : X -> T2 | C
	
	 Γ ⊢ t1 : T1 | C1    Γ ⊢ t2 : T2 | C2
	X is fresh, C = C1 ∪ C2 ∪ {T1=T2 -> X}
	--------------------------------------
	        Γ ⊢ t1 t2 : X | C
	        
Phase 2: Unification

The unification algorithm works by starting with an empty substitution [] and then going through the constraints one by one, extending the initial substitution (i.e. adding substitution rules) as it goes. 