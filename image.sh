
sbcl --no-userinit --load "clapsv.lisp" \
     --eval "(sb-ext:save-lisp-and-die \"clapsv\"
                  :executable t
		  :save-runtime-options t
		  :toplevel 'clapsv:toplevel)"

# sbcl --no-userinit --load "j.lisp" \
#      --eval "(sb-ext:save-lisp-and-die \"junk\"
#                   :executable t
# 		  :save-runtime-options t
# 		  :toplevel 'junk:run)"
