LISP?=sbcl

main:
	$(LISP) --no-userinit --no-sysinit --eval '(load "init.lisp")' \
	--eval '(sb-ext:save-lisp-and-die "main" :executable t :toplevel `cl-db.main:main)'

clean:
	trash **.fasl

.PHONY: main clean
