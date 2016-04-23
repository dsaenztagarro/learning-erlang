.PHONY: clean compile shell test version

clean:
	rm -rf ebin/; mkdir ebin/

compile: clean
	erl -make

shell:
	erl -pa ebin/

test: compile test/*_test.erl
	erl -pa ebin/ -eval 'eunit:test([kitty_gen_server_test, kitty_server2_test, trade_fsm_test], [verbose])' -eval 'init:stop()'

version:
	@erl -eval '{ok, Version} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), erlang:display(erlang:binary_to_list(Version)), halt().' -noshell
