(defclass graph ()
  ((nodes
     :accessor nodes
     :initform (list)
     :type list)
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
     :initarg :data))
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
	    (push N (nodes G))
	    (incf (size G))
	  )
	i)))

;(defgeneric add-edge ((G graph)

(defmethod add-edge ((G graph) (e edge))
  (push e (edges G)))

(defmethod index ((G graph) (n node))
  (position n (nodes G) :test #'equal))

(describe 'graph)

(defvar num 100)
(defvar iterations 20)
(defparameter mg (make-instance 'graph))

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

(setf *random-state* (make-random-state t))
(loop repeat iterations do 
	(let* (
			(ai (- (size mg) (random num) 1))
			(bi (- (size mg) (random num) 1))
			(a (nth ai (nodes mg)))
			(b (nth bi (nodes mg)))
		)
		;(if (and (eql 'floor (car op)) (zerop (data b)))
		(if (zerop (data b))
			(noop)
			(progn
				(format T "~d ~d~%" (data a) (data b))
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
						(add-node mg (make-instance 'node :data (expt (data a) n)) T)
				     ))))
			)
		)
	)
)


