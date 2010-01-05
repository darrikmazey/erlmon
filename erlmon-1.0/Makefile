
ERL=erl
ERLC=erlc
ERLS=$(wildcard src/*.erl)
BEAMS=$(ERLS:.erl=.beam)
EBEAMS=$(BEAMS:src/%=ebin/%)

all: force

force: compile shell
compile: $(EBEAMS)

ebin/%.beam: src/%.erl
	@echo [$(ERLC)] $<
	@$(ERLC) -o ebin/ $<

clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump

shell:
	$(ERL) -pa ebin/ -s erlmon
