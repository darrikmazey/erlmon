
OS = $(shell uname)

.PHONY: all clean test

all:
	$(MAKE) -f Makefile.$(OS)

clean:
	$(MAKE) -f Makefile.$(OS) clean

test:
	$(MAKE) -f Makefile.$(OS) test

runtests:
	erl -pa ebin -s lua_test test -noshell -s init stop

