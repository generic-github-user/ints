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
