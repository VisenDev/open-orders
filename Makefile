ECL=tooling/ecl/build/bin/ecl
SBCL=tooling/sbcl/build/bin/sbcl

$(SBCL): $(ECL)
	cd tooling/sbcl && sh make.sh $(shell realpath "$(ECL) --norc")
	cd tooling/sbcl && mkdir -p build
	cd tooling/sbcl && sh install.sh --prefix=build

$(ECL):
	cd tooling/ecl && ./configure
	cd tooling/ecl && make

main: $(SBCL)
	$(SBCL) --no-userinit --no-sysinit --eval '(load "init.lisp")' --eval '(sb-ext:save-lisp-and-die "main" :executable t :toplevel `cl-db/main:main)'

clean:
#cd tooling/ecl && make clean
#cd tooling/sbcl && sh clean.sh

.PHONY: main clean
