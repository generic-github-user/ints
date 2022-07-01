(defclass graph ()
  ((nodes :accessor nodes :initform (make-hash-table) :type hash-table)
   (edges :accessor edges :initform (list))
   (size :accessor size :initform 0 :type integer))
   (:documentation "A generic graph class")
)

(defclass node ()
  ((data :accessor data :initarg :data)
   (edges :accessor edges :initform (list))
   (adjacent :accessor adjacent :initform (list))
   (degree :accessor degree :initform 0)
   (info :accessor info :initform (make-hash-table)))
  (:documentation "A generic node class")
)

(defclass edge ()
  ((path :accessor path :initarg :path :type list)
   (data :accessor data :initarg :data :type T))
  (:documentation "A generic edge class")
)

(defun make-node (data)
	(make-instance 'node :data data))

(defun make-edge (data path)
	(make-instance 'edge :data data :path path))

(defmethod add-node ((G graph) (N node) &optional d)
  (let ((i (index G N)))
	(if (or d (not i))
	  (progn
	    ;(push N (nodes G))
	    (setf (gethash (size G) (nodes G)) N)
	    (incf (size G))
	  )
	i)))

(defmethod get-node ((G graph) (index integer))
	(gethash index (nodes G)))

(defmethod get-value ((G graph) (index integer))
	(data (get-node G index)))

;(defgeneric add-edge ((G graph)

(defmethod add-edge ((G graph) (e edge))
  (push e (edges G)))

(defgeneric geneq (a b))
(defmethod geneq ((a node) (b node))
  (equal (data a) (data b)))

(defmethod index ((G graph) (n node))
  ;(position n (nodes G) :test (lambda (key) (= (data a) (data b)))))
  (loop for key being the hash-key of (nodes G)
	when (geneq (gethash key (nodes G)) n) return key finally (return NIL)))

(describe 'graph)

(defvar num 30)
(defvar iterations 20)
(defvar verbose NIL)
(defparameter mg (make-instance 'graph))

; Initialize the database with a range of integers
(loop for i from 0 to num do (
  add-node mg (make-node i) T))

; Via https://bese.common-lisp.dev/docs/arnesi/html/api/function_005FIT.BESE.ARNESI_003A_003ANOOP.html
(defun noop (&rest args)
  "Do nothing."
  (declare (ignore args))
  (values))

(defun prime (n)
	(loop for x from 2 to (isqrt n)
		never (zerop (rem n x))
		finally (return T)))

; Based on https://stackoverflow.com/a/34897978
(defmethod print-object ((G graph) out)
  (with-slots (size) G
    (print-unreadable-object (G out :type t)
      (format out "size: ~a" (list size)))))

(defun check-pair (ai bi)
	(let (
			(a (get-node mg ai))
			(b (get-node mg bi))
		)
		(if (zerop (data b))
			(noop)
			(progn
			  	;(print-object mg *standard-output*) (terpri)
				;(format T "~d ~d~%" ai bi)
				(if verbose (format T "~d ~d~%" (data a) (data b)))
				;(terpri)

				(if verbose (progn (print "Checking divisibility") (terpri)))
				(if (zerop (rem (data a) (data b)))
					(add-edge mg (make-edge "divisible" (list ai bi))))
				(loop for op in (list '(+ "sum") '(* "product") '(- "difference") '(floor "quotient") '(mod "modulo")) do
					;(print op)
					;(print (funcall (car op) 2 4))
					(let* ((sum (make-node (funcall (car op) (data a) (data b)))))
						; (string (cdr op))
						(add-edge mg (make-edge (cdr op) (list ai bi (add-node mg sum T))))
					)
				)
				(if (prime (data a)) (add-edge mg (make-edge "prime" (list ai))))
				(loop for n in (list 2 3 4 5) do
					(add-edge mg (make-edge "exp" (list
						ai
						(add-node mg (make-node n) T)
						(add-node mg (make-node (expt (data a) n)) T)))))))))


; (setf *random-state* (make-random-state t))
; (loop repeat iterations do 
; 	(let* (
; 			(ai (- (size mg) (random num) 1))
; 			(bi (- (size mg) (random num) 1))
; 		)
; 		;(if (and (eql 'floor (car op)) (zerop (data b)))
; 		(check-pair ai bi)
; 	)
; )

(defun check-int (i) (loop for n from 0 to i do (check-pair i n)))
(loop for n from 0 to num do (check-int n))

; Compute summary properties
(maphash (lambda (key value)
	   (setf (gethash 'ndivisors (info value)) (count-if (lambda (e) (and (equal (data e) "divisible") (equal (car (path e)) (data value)))) (edges mg)))) (nodes mg))

;(maphash (lambda (key value) (print (data value))) (nodes mg))
(loop for n from 0 to num do
	(progn
	     (format T "**~A**~&" (get-value mg n))
	     (format T "Divisible by ~{~A~^, ~}~&" (sort (mapcar
			(lambda (e) (get-value mg (nth 1 (path e))))
			;(lambda (e) (cdr (path e)))
			(remove-if-not (lambda (e)
			(and
				(equal (data e) "divisible")
				(equal (get-value mg (car (path e))) n)))
			(edges mg))) #'<))
	     (format T "Analyzed ~a relationships ~&" (count-if (lambda (e) (some (lambda (p) (equal p n)) (path e))) (edges mg)))
	     (terpri)))

(defmethod print-object ((N node) out)
  (print-unreadable-object (N out :type t)
    (format out "~s" (data N))))

(defmethod write-graph (G)
  (with-open-file (outfile "intdata.lisp"
			 :direction :output
			 :if-exists :supersede)
    (print mg outfile)))

; (print-object (nth 20 (nodes mg)) *standard-output*)
; (print-node (nth 20 (nodes mg)))
