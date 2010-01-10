ERL=erl
ERLC=erlc
APP=amf

all: compile

compile:
	@$(ERL) -make

clean:
	@echo "removing:"
	@rm -fv ebin/*.beam

docs:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean-docs:
	-@rm -f doc/edoc-info doc/*.html doc/*.css doc/*.png

test: compile
	@$(ERL) -pa ebin -eval "eunit:test({application,$(APP)})" \
	-noshell -s init stop
