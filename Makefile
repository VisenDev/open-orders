ECL=$(shell which ecl)
SBCL=$(shell which sbcl)
BUILD_ECL=0
BUILD_SBCL=0

ifeq ("", $(ECL))
	ECL=tooling/ecl/build/bin/ecl
	BUILD_ECL=1
endif

ifeq ("", $(SBCL))
	SBCL=tooling/sbcl/build/bin/sbcl
	BUILD_SBCL=1
endif

all: main

ifeq ($(BUILD_SBCL), 1)

$(SBCL): $(ECL)
	cd tooling/sbcl && sh make.sh "$(ECL) --norc"
	cd tooling/sbcl && mkdir -p build
	cd tooling/sbcl && sh install.sh --prefix=build

endif

ifeq ($(BUILD_ECL), 1)

$(ECL):	
	cd tooling/ecl && ./configure --config-cache
	cd tooling/ecl && make -j4

endif


main: $(SBCL)
	$(SBCL) --no-userinit --no-sysinit --eval '(load "init.lisp")' \
	--eval '(sb-ext:save-lisp-and-die "main" :executable t :toplevel `cl-db.main:main)'

clean:
	if [ -e **fasl ]; then rm **fasl; fi

clean-heavy: clean
	cd tooling/ecl && make clean
	cd tooling/sbcl && sh clean.sh


.PHONY: main clean
