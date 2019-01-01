;;; Default (initial) location of theuser
(defparameter *currentLocation* 'living-room)
;;; 'Node' association list (alist):
(defparameter *wizard-nodes* 
    '(
        (living-room (you are in the living room. a wizard is snoring loudly on the couch.))
        (garden (you are in a beautiful garden. there is a well in front of you.))
        (attic (you are in the attic. there is a giant welding torch in the corner))
    ))
;; 'Edge' alist [Data structure:  ([node] [direction] [method])  ]
(defparameter *wizard-edges*
    '(
        (living-room (garden west door) (attic upstairs ladder))
        (garden (living-room east door))
        (attic (living-room downstairs ladder))
    ))
;;; 'Objects':
(defparameter *objects*
    '(whiskey bucket frog chain) )
;;; location for the object
(defparameter *object-locations*
    '(
        (whiskey living-room)
        (bucket living-room)
        (chain garden)
        (frog garden)
    ))

;;;;;; Functions:

;;; Get the location description
(defun describe-location (location nodes)
    (cadr (assoc location nodes)))
;;;Get the edge path:
;; (not 'single quote' but 'backquote' here!!)
(defun describe-path (edge)
    `(there is a ,(caddr edge) going ,(cadr edge) from here.))
;;; Describe all the path at once:    
(defun describe-paths (location edges)
    (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))
;;; Return list of object where you are from:
(defun objects-at (loc objs obj-locs)
    (labels ((at-loc-p (obj)
                (eq (cadr (assoc obj obj-locs)) loc)))
        (remove-if-not #'at-loc-p objs) )) 

;;; View the objects at the location:
(defun describe-objects (loc objs obj-loc)
    (labels (
        (describe-obj (obj)
            `(you see a ,obj on the floor.)))
        (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc))) ))

;;; view the current location:
(defun look()
    (append (describe-location *currentLocation* *wizard-nodes*)
            (describe-paths *currentLocation* *wizard-edges*)
            (describe-objects *currentLocation* *objects* *object-locations*) ))

;;; walk around
(defun walk(direction)
    (let ((next (find direction
                      (cdr (assoc *currentLocation* *wizard-edges*))
                      :key #'cadr)))
        (if next
            (progn (setf *currentLocation* (car next)) 
                    (look))
            '(you cannot go that way.) )))
;;; picking up item(s)
(defun pickup(object) 
    (cond ((member object (objects-at *currentLocation* *objects* *object-locations*))
            ;; adding new list (xxx 'body) into object-locations:
            (push (list object 'body) *object-locations*)
            `(you are now carrying the ,object) )
        (t '(you cannot get that.)) ))
;;; check the items you have.
(defun inventory() 
    (cons 'items- (objects-at 'body *objects* *object-locations*)) )


