test:
	erlc test.erl stek.erl queue1.erl
	erl -s test run
test1:
	erlc stek.erl test.erl
	erl -s test run stek test
test2:
	erlc queue1.erl test.erl
	erl -s test run queue1 test

clean:
	-rm *.beam
