###Project 1: The NB Language

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

###Project 2: Untyped Lambda Calculus

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

	
###Project 3: Simply Typed Lambda Calculus

###Project 4: STLC Extensions
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


###Project 5: STLC with Type Reconstruction
Implement a Hindley-Milner type inferencer for a modification of the simply typed $\lambda$-calculus with booleans, numbers and let: