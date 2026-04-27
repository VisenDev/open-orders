LISP?=sbcl

main:
	$(LISP) --eval '(load "init.lisp")' \
	--eval '(asdf:make "open-orders/executable")' \
	--eval '(uiop:quit)'

clean:
	find . -type f -name '*.fasl' -exec trash {} \;

