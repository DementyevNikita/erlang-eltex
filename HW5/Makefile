test:
	erlc test.erl fac.erl factail.erl lists1.erl
	erl -s test run
test1:
	erlc fac.erl test.erl
	erl -s test run fac test
test2:
	erlc factail.erl test.erl
	erl -s test run factail test
test3:
	erlc lists1.erl test.erl
	erl -s test run lists1 test

clean:
	-rm *.beam
