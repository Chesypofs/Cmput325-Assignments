#|
Name: William Hodgson
ID: 1413104
Course: CMPUT 325
Section: B1
Assignment: #1
|#

#|
fl-interp is an interpreter for the fun programing language,
that takes an expression E and evaluates it on program P.

E can include built in functions:
	null, atom, eq, first, rest, cons, equal, isnumber,
	+, -, *, =, <, >, not, and, or, if
or user-defined functions that are included in P.
The user-defined function definitions must be of the form:
	f(X1,X2,...,Xn)=Exp
where f is the function name, the Xis are parameters and
Exp is the function body.
|#

(defun fl-interp (E P)
	(cond
		((atom E) E)
        (T
			(let ((f (car E)) (arg (cdr E)))
				(cond 
					; handle built-in functions
					((eq f 'null)
						(cond
							((null (fl-interp (car arg) P)) T)
							(T NIL)
						)
					)
					
					((eq f 'atom)
						(cond
							((atom (fl-interp (car arg) P)) T)
							(T NIL)
						)
					)
					
					((eq f 'eq)
						(cond
							((eq (fl-interp (car arg) P) (fl-interp (cadr arg) P)) T)
							(T NIL)
						)
					)
					
					((eq f 'first) (car (fl-interp (car arg) P)))
					
					((eq f 'rest) (cdr (fl-interp (car arg) P)))
					
					((eq f 'cons) (cons (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
					
					((eq f 'equal)
						(cond
							((equal (fl-interp (car arg) P) (fl-interp (cadr arg) P)) T)
							(T NIL)
						)
					)
					
					((eq f 'isnumber)
						(cond
							((numberp (fl-interp (car arg) P)) T)
							(T NIL)
						)
					)
					
					((eq f '+) (+ (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
					
					((eq f '-) (- (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
					
					((eq f '*) (* (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
					
					((eq f '>)
						(cond
							((> (fl-interp (car arg) P) (fl-interp (cadr arg) P)) T)
							(T NIL)
						)
					)
					
					((eq f '<)
						(cond
							((< (fl-interp (car arg) P) (fl-interp (cadr arg) P)) T)
							(T NIL)
						)
					)
					
					((eq f '=)
						(cond
							((= (fl-interp (car arg) P) (fl-interp (cadr arg) P)) T)
							(T NIL)
						)
					)
					
					((eq f 'not)
						(cond
							((fl-interp (car arg) P) NIL)
							(T T)
						)
					)
					
					((eq f 'and) 
						(cond
							; check first condition for and
							((fl-interp (car arg) P)
								(cond 
									; check second condition for and
									((fl-interp (cadr arg) P) T)
									(T NIL)
								)							
							)
							(T NIL)
						)
					)
					
					((eq f 'or)
						(cond
							; check conditions for or
							((fl-interp (car arg) P) T)
							((fl-interp (cadr arg) P) T)
							(T NIL)
						)
					)
					
					((eq f 'if)
						(cond
							; check the if condition
							((fl-interp (car arg) P) (fl-interp (cadr arg) P))
							; else clause
							(T (fl-interp (caddr arg) P))
						)
					)
					
					; handle user-defined functions
					((getuserfunc f arg P)
						(let ((reducedargs (reduceargs arg P)) (userfunction (getuserfunc f arg P)))
							(fl-interp (car (reduceandreplace (cdr userfunction) reducedargs)) P)
						)
					)
						
					; otherwise f is undefined; in this case,
					; E is returned as if it is quoted in lisp
					(T E)
				)
			)
		)
	)
)

(defun getuserfunc (f arg P)
	(cond
		((eq P NIL) NIL)
		((eq arg NIL) NIL)
		; check if function name and number of parameters match up
		((and (eq f (caar P)) (= (countargs (car P)) (countitems arg))) (car P))		
		(T (getuserfunc f arg (cdr P)))
	)
)

; counts the number of arguments in the function definition
(defun countargs (f)
	(cond
		((eq (car f) '=) -1) ;-1 here because the function definition starts with the function name
		(T (+ 1 (countargs (cdr f))))
	)
)

; counts the number of items in a list
(defun countitems (f)
	(cond
		((eq f NIL) 0)
		(T (+ 1 (countitems (cdr f))))
	)
)

(defun reduceargs (arg P)
	(cond
		((eq arg NIL) NIL)
		(T (cons (fl-interp (car arg) P) (reduceargs (cdr arg) P)))
	)
)

; returns the function definition with all parameters replaced by their arguments
(defun reduceandreplace (fdef arg)
	(cond
		((eq (car fdef) '=) (cdr fdef))
		(T (reduceandreplace (cdr (replaceparam fdef (car fdef) (car arg))) (cdr arg)))
	)
)

; replaces a single parameter with its associated argument
(defun replaceparam (fdef param arg)
	(cond
		((eq fdef NIL) NIL)
		((eq (car fdef) param) (cons arg (replaceparam (cdr fdef) param arg)))
		; check if the next element is a list itself, if so replace the params in that list aswell
		((not (atom (car fdef))) (cons (replaceparam (car fdef) param arg) (replaceparam (cdr fdef) param arg)))
		(T (cons (car fdef) (replaceparam (cdr fdef) param arg)))
	)
) 