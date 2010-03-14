ERL_LIB=/usr/lib/erlang/lib/erl_interface-3.6.3
CFLAGS=-Wall -I/usr/local/include -I$(ERL_LIB)/include -I/usr/lib/erlang/usr/include
LDFLAGS=-shared -L. -L$(ERL_LIB)/lib
LDLIBS=-llua
STATICLIBS=$(ERL_LIB)/lib/libei.a $(ERL_LIB)/lib/liberl_interface.a
GCC=/usr/bin/gcc
ERL=/usr/bin/erl

LIB=lib/liberlua.so

ERLMODS=$(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))
OBJECTS=$(patsubst c_src/%.c,build/%.o,$(wildcard c_src/*.c))

all: $(LIB) $(ERLMODS)

build/%.o: c_src/%.c
	$(GCC) $(CFLAGS) -o $@ -c $<

ebin/%.beam: src/%.erl
	$(ERL) -make

$(LIB): $(OBJECTS)
	$(GCC) $(CFLAGS) $(LDFLAGS) $(LDLIBS) -o $(LIB) $(OBJECTS) $(STATICLIBS)

clean:
	rm -f ebin/*.beam
	rm -f build/*.o
	rm -f lib/liberlua.so
