(defclass graph ()
  ((nodes
     :accessor nodes
     :initform (make-hash-table)
     :type hash-table)
   (edges
     :accessor edges
     :initform (list))
   (size
     :accessor size
     :initform 0
     :type integer))
   (:documentation "A generic graph class")
)

(defclass node ()
  ((data
     :accessor data
     :initarg :data)
   (edges
     :accessor edges
     :initform (list))
   (adjacent
     :accessor adjacent
     :initform (list))
   (degree
     :accessor degree
     :initform 0))
  (:documentation "A generic node class")
)

(defclass edge ()
  ((path
     :accessor path
     :initarg :path
     ;:initform (cons nil nil)
     ;:type cons)
     :type list)
   (data
     :accessor data
     :initarg :data
     :type T))
  (:documentation "A generic edge class")
)

(defmethod add-node ((G graph) (N node) &optional d)
  (let ((i (index G N)))
	(if (or d (not i))
	  (progn
	    ;(push N (nodes G))
	    (setf (gethash (size G) (nodes G)) N)
	    (incf (size G))
	  )
	i)))

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

(defvar num 100)
(defvar iterations 20)
(defparameter mg (make-instance 'graph))

; Initialize the database with a range of integers
(loop for i from 0 to num do (
  add-node mg (make-instance 'node :data i) T))

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
			(a (gethash ai (nodes mg)))
			(b (gethash bi (nodes mg)))
		)
		(if (zerop (data b))
			(noop)
			(progn
			  	(print-object mg *standard-output*) (terpri)
				(format T "~d ~d~%" ai bi)
				(format T "~d ~d~%" (data a) (data b))
				(terpri)
				(if (zerop (rem (data a) (data b)))
					(add-edge mg (make-instance 'edge :data "divisible" :path (list ai bi))))
				(loop for op in (list '(+ "sum") '(* "product") '(- "difference") '(floor "quotient") '(mod "modulo")) do
					;(print op)
					;(print (funcall (car op) 2 4))
					(let* ((sum (make-instance 'node :data (funcall (car op) (data a) (data b)))))
						; (string (cdr op))
						(add-edge mg (make-instance 'edge :data (cdr op) :path (list ai bi (add-node mg sum T))))
					)
				)
				(if (prime (data a)) (add-edge mg (make-instance 'edge :data "prime" :path (list ai))))
				(loop for n in (list 2 3 4 5) do
					(add-edge mg (make-instance 'edge :data "exp" :path (list
						ai
						(add-node mg (make-instance 'node :data n) T)
						(add-node mg (make-instance 'node :data (expt (data a) n)) T)))))))))

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

(defun check-int (i) (loop for n from 0 to 20 do (check-pair i n)))
(check-int 5)
;(loop for n from 0 to 10 do (check-int n))



(defmethod print-object ((N node) out)
  (print-unreadable-object (N out :type t)
    (format out "~s" (data N))))

(with-open-file (outfile "intdata.lisp"
			 :direction :output
			 :if-exists :supersede)
    (print mg outfile))

; (print-object (nth 20 (nodes mg)) *standard-output*)
; (print-node (nth 20 (nodes mg)))
