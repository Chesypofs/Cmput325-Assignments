#|
Name: William Hodgson
ID: 1413104
Course: CMPUT 325
Section: B1
Assignment: #1
|#

#| Question 1.

The function xmember returns T if argument Y is a member of the 
argument list X and NIL otherwise. Both the list X and the argument 
Y may be NIL or lists containing NIL.
|#

(defun xmember (X Y)
	(cond ((null X) nil)
		((equal (car X) Y) T)
		(T (xmember (cdr X) Y ))
    )
)

#| Question 2.

The function flatten takes a list x and returns a list that contains
all the atoms appearing in x in the same order, but without sublists.

Test Cases:
(flatten '(a (b c) d)) => (a b c d)
(flatten '((((a))))) => (a)
(flatten '(a (b c) (d ((e)) f))) => (a b c d e f)
|#

(defun flatten (x)
    (cond ((null x) ())
		((atom (car x)) (cons (car x) (flatten (cdr x))))
		(T (flatten (append (car x) (cdr x))))
    )
)

#| Question 3.

The function mix takes elements from lists L1 and L2 by alternating
between them to form a single list consisting of all the elements of
both L1 and L2

Test Cases:
(mix '(d e f) '(a b c)) => (a d b e c f)
(mix '(a) '(1 2 (3))) => (1 a 2 (3))
(mix nil '(1 2 3)) => (1 2 3)
|#

(defun mix (L2 L1)
    (cond ((null L2) L1)
		((null L1) L2)
		((cons (car L1) (cons (car L2) (mix (cdr L2) (cdr L1)))))
    )
)

#| Question 4.

The function split takes a list L and splits it into two sublists (L1 L2)
by putting elements from L into L1 and L2 alternatingly.
NOTE: Uses helper functions getodd and geteven defined below.

Test Cases:
(split '(1 2 3 4 5 6)) => ((1 3 5) (2 4 6))
(split '(1 2 3 4 (5)))  => ((1 3 (5)) (2 4))
|#

(defun split (L)
    (cons (getodd L) (cons (geteven L) ()))
)

#|
The function getodd takes a list L and returns a list containing
all elements of L that are at odd indices (starting from 1).
|#

(defun getodd (L)
    (cond ((null L) ())
		((null (cdr L)) L)
		(T (cons (car L) (getodd (cddr L))))
    )
)

#|
The function geteven takes a list L and returns a list containing
all elements of L that are at even indices (starting from 1).
|#

(defun geteven (L)
    (cond ((null L) ())
		((null (cdr L)) ())
		(T (cons (cadr L) (geteven (cddr L))))
    )
)

#|
Question 5.1.

No it is not true that (split (mix L2 L1)) returns (L1 L2)
The result of (split (mix L2 L1)) is not (L1 L2) whenever L1
has less elements than L2, or L1 has atleast 2 elements more
than L2.

Ex:
(split (mix '(a1 a2) '(b1))) returns ((b1 a2) (a1)) not ((b1) (a1 a2))
(split (mix '(a1) '(b1 b2 b3))) returns ((b1 b2) (a1 b3)) not ((b1 b2 b3) (a1)) 


Question 5.2.

Yes it is true that (mix (cadr (split L)) (car (split L))) returns L

Proof:
Let the list L be (a_1 a_2 ... a_n) then
if n is even:
	(split L) returns ((a_1 a_3 ... a_n-1) (a_2 a_4 ... a_n))
	(cadr '((a_1 a_3 ... a_n-1) (a_2 a_4 ... a_n))) returns (a_2 a_4 ... a_n)
	(car '((a_1 a_3 ... a_n-1) (a_2 a_4 ... a_n))) returns (a_1 a_3 ... a_n-1)
	(mix (a_2 a_4 ... a_n) (a_1 a_3 ... a_n-1)) returns (a_1 a_2 a_3 ... a_n-1 a_n)

if n is odd:
	(split L) returns ((a_1 a_3 ... a_n) (a_2 a_4 ... a_n-1))
	(cadr '((a_1 a_3 ... a_n) (a_2 a_4 ... a_n-1))) returns (a_2 a_4 ... a_n-1)
	(car '((a_1 a_3 ... a_n) (a_2 a_4 ... a_n-1))) returns (a_1 a_3 ... a_n)
	(mix (a_2 a_4 ... a_n-1) (a_1 a_3 ... a_n)) returns (a_1 a_2 a_3 ... a_n-1 a_n)

In either case (mix (cadr (split L)) (car (split L))) returns L
|#

#| Question 6.

The function subsetsum takes a list L of positive integers and a 
positive integer sum S, and returns a subsetof L that sums up to 
S using numbers in L only once. If no such sum exists then the
function returns nil.
NOTE: Uses helper function sum defined below.

Test Cases:
(subsetsum 5 '(1 2 3)) => (2 3)
(subsetsum 2 '(1 5 3)) => nil
(subsetsum 29 '(1 16 2 8 4)) => (1 16 8 4)
|#

(defun subsetsum (S L)
    (cond ((< S 0) ())                       ; First exclude all the simple cases                      
		((null L) ())
		((= S (car L)) (cons (car L) ()))
		((= S (sum L)) L)
		((> S (sum L)) ())
				
		(T (let ((valueinsum (subsetsum (- S (car L)) (cdr L)))   ; Case 1: the first atom of L is in the sum
				(valuenotinsum (subsetsum S (cdr L)))            ; Case 2: the first atom of L is not in the sum
				)                                  
			(cond (valueinsum (cons (car L) valueinsum))       ; Check if either of the two cases was correct
				   (valuenotinsum valuenotinsum)
			)
			)
		)          
    )
)

#|
The function sum takes a list L of positive integers and returns
the sum of all its elements. If the list is empty then the function
returns 0.
|#

(defun sum (L)
    (cond ((null L) 0)
		(T (+ (car L) (sum (cdr L))))
    )
)