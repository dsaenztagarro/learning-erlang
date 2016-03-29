.PHONY: compile shell test

compile:
	erl -make

shell:
	erl -pa ebin/

test: compile
	erl -pa ebin/ -eval 'eunit:test(kitty_server2_test, [verbose])' -eval 'init:stop()'
