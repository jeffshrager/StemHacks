; (load "funsub.lisp")

#| 

Hi Jeff,

The procedure is always valid as long as you (1) Treat all fractions as 
if the denominators are positive (with possibly negative numerators); 
and (2) Always subtract the smaller numerator from the bigger one.

In your example the full procedure would go

5/30   ?  10/14
-5/16  ?  10/14
-15/2  ?  10/14
-15/2  ?  25/12
...
-15/2  ?  100/2

Once the denominators are equal you stop and declare the fraction with a 
smaller numerator to be smaller. In practice I always tell kids to stop 
as soon as the answer is obvious. I should have said in the book that 
one time things are obvious is when one number is negative and the other 
is positive.

To see formally that the procedure is valid, suppose we are comparing 
a/b and c/d and b > d > 0. The critical insight is to note that we can write

a/b = (d/b) (c/d) + ((b-d)/b) ( (a-c)/(b-d) )
        = w (c/d) + (1 - w) ( (a-c)/(b-d) ), where w=d/b \in (0, 1).

Because w is in (0,1), this equation is showing that a/b is a weighted 
average of two numbers: (c/d) and (a-c)/(b-d).

So now consider three cases:

Case 1: a/b > c/d
The fact that a/b is a weighted average of c/d and (a-c)/(b-d) and the 
first of these is smaller than a/b implies that the second (a-c)/(b-d) 
is bigger than a/b. And using transitivity and the assumption of Case 1 
(a-c)/(b-d) > a/b > c/d which slows that (a-c)/(b-d) has the same 
relation to (c/d) as did a/b. (It also shows that the fractions are 
getting farther apart with each step. This is why the comparisons tend 
to get obvious at some point.)

Case 2: a/b < c/d
Here the weighted average property implies that (a-c)/(b-d) < a/b and 
again transitivity gives (a-c)/(b-d) < a/b < c/d.

Case 3: a/b = c/d
Here the weighted average property implies that (a-c)/(b-d) = a/b = c/d.

When we treat all denominators as positive the denominators are 
guaranteed to keep decreasing throughout the process. (The denominators 
process is the "Euclidean algorithm" for finding the greatest common 
factor.) So the process will eventually always terminate at a point 
where the two denominators are equal (with both being equal to the 
greatest common factor of b and d).

The term "funny subtraction" is something I made up for the book. I 
assume this algorithm must have a real name, but I don't know what it 
is. In number theory you see people doing similar when working with 
"Farey sequences", but Farey's algorithm for generating such sequences 
is something different.

Glenn

|#


;;; Experiments with Glenn Ellison's "funny subtraction" (Hard Math
;;; for Elementary School, 2013)

(defparameter *lim* 60) ; plus or minus 
(defparameter *dstops* '(0 1 2 3 4 5))

(defun n () (random *lim*))

(defun rcomp (na da nb db)
  (let ((a (float (/ na da)))
	(b (float (/ nb db))))
    (if (> a b) ">"
      (if (< a b) "<"
	"="))))

(defun comp (na da nb db &optional (depth 0)) ;; nums and denoms are handled separately
  (format t "~a: ~a/~a ~a ~a/~a~%" depth na da (rcomp na da nb db) nb db)
  (unless (or (< na 0) (< nb 0) (= da db) (and (member da *dstops*) (member db *dstops*)) (> depth 20))
    (if (< db da)
	(comp (- na nb) (- da db) nb db (1+ depth))
      (comp na da (- nb na) (- db da) (1+ depth)))))
  
(loop for i below 100
      as (na da nb db) = `(,(n) ,(n) ,(n) ,(n))
      do (ignore-errors 
	  (format t "--------  ~a/~a ~a  ~a/~a  -------~%" na da (rcomp na da nb db) nb db)
	  (comp na da nb db)))

  