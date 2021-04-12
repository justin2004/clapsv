;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (ql:quickload '("cl-csv" "april" "alexandria")
                :silent t))

(defpackage :clapsv
  (:use :cl :april :cl-csv :cl-ppcre)
  (:export :toplevel *ui*)) ; TODO need ui?

(in-package :clapsv)


(defmacro custom (colname &rest funXs)
  "fun can be a string (apl) or a CL function"
  `(progn
     (april (:with (:state :in ((col_name ,colname)))) 
            "col_num←⊃⍸(⊂col_name)≡¨mat[1;]")
     ,@(loop :for funX
             :in  funXs
             :collect `(april (:with (:store-fun (f (wrap (typecase ,funX
                                                            (string (april ,funX))
                                                            (t ,funX)))))) 
                              "mat[1↓⍳⊃⍴mat;col_num]←f¨mat[1↓⍳⊃⍴mat;col_num]"))))

(defun string-em (lis) 
  "if one of the items in lis is a char turn it into a string"
  (mapcar #'(lambda (x)
              (typecase x
                (standard-char (string x))
                (t x)))
          lis))

(defun wrap (fun)
  "takes a function 'fun' and returns a function that does fun.
   we need this because april gives us a vector (possibly with standard-chars) but we want a list with strings."
  (lambda (arglis)
    (apply fun
           (typecase arglis
             (simple-vector (string-em (coerce arglis 'list)))
             (t (string-em (list arglis)))))))

; (defmacro junk ()
;  '(april (with (:state :in
;                 ((i (coerce (uiop:read-file-lines "/mnt/some.csv")
;                             'vector) ))))
;          "input←i") )

(defmacro run ()
  '(progn (april (with (:state :in
                        ((i (coerce (uiop:read-file-lines "./some.csv")
                                    'vector) ))))
                 "input←i")

          (april "mat←↑{(','≠⍵)/⍵}¨¨{(1,1↓(','=⍵))⊂⍵}¨input")

          (let ((cust-stream (make-string-input-stream (concatenate 'string
                                                                    (format nil "(in-package :clapsv)~%") ; TODO don't hardcode?
                                                                    (alexandria:read-file-into-string "1.lisp" )) )))
            (loop
              :for expr = (read cust-stream nil)
              :while (not (null expr))
              :do (progn
                    ; (format t "~A~%" expr)
                    (eval expr))))

          ; (identity *read-eval*) ;; ?
          ;look 
          ; (april-f "mat")

          ; 2d matrix to csv
          (cl-csv:write-csv (mapcar #'(lambda (x)
                                        (coerce x 'list)) 
                                    (coerce (april "↓mat")
                                            'list))
                            :stream *standard-output*)))


(defun toplevel ()
  (sb-ext:disable-debugger)
  (run))

