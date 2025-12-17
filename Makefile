

sbcl:
	sbcl --eval '(progn (asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :inherit-configuration)) (asdf:load-system "cl-db") (asdf:make "cl-db/executable"))'
