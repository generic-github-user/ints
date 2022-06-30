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

(defmethod add-node ((G graph) (N node))
  (push N (nodes G))
  (incf (size G))
)

(defmethod add-edge ((G graph) (e edge))
  (push e (edges G)))

(defmethod index ((G graph) (n node))
  (position n (nodes G) :test #'equal))

(describe 'graph)

(defvar num 100)
(defvar iterations 20)
(defparameter mg (make-instance 'graph))

(loop for i from 0 to num do (
  add-node mg (make-instance 'node :data i)))

; Via https://bese.common-lisp.dev/docs/arnesi/html/api/function_005FIT.BESE.ARNESI_003A_003ANOOP.html
(defun noop (&rest args)
  "Do nothing."
  (declare (ignore args))
  (values))


(defun prime (n)
	(loop for x from 2 to (isqrt n)
		never (zerop (rem n x))
		finally (return T)))
