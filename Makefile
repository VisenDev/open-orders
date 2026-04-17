LISP?=sbcl

main:
	$(LISP) --eval '(load "init.lisp")' \
	--eval '(asdf:make "open-orders/executable")' \
	--eval '(uiop:quit)'

clean:
	trash **.fasl
	trash open-orders
	trash *.sqlite3
	trash *.database


