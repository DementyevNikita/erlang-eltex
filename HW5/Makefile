test:
	erlc test.erl fac.erl factail.erl
	erl -s test run
test1:
	erlc fac.erl test.erl
	erl -s test run fac test
test2:
	erlc factail.erl test.erl
	erl -s test run factail test

clean:
	-rm *.beam
