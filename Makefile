

all:
	erlc -o ebin -I include/ src/*.erl test/*.erl

test: all
	erl -pa ebin/ -eval 'eunit:test(ess), init:stop().'