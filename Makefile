LISP?=sbcl

open-orders-client:
	$(LISP) \
	--eval '(declaim (optimize (speed 3) (safety 1)))' \
	--eval '(load "open-orders.asd")' \
	--eval '(asdf:make "open-orders/client")' \
	--eval '(uiop:quit)'

open-orders-server:
	$(LISP) \
	--eval '(declaim (optimize (speed 3) (safety 1)))' \
	--eval '(load "open-orders.asd")' \
	--eval '(asdf:make "open-orders/server")' \
	--eval '(uiop:quit)'


clean:
	find . -type f -name '*.fasl' -exec trash {} \;
	if [ -e ./open-orders ]; then trash open-orders; fi
	if [ -e ./open-orders-server ]; then trash open-orders-server; fi
	if [ -e ./open-orders-client ]; then trash open-orders-client; fi

