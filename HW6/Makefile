test_all: test runli runli1

test:
	erlc sorstu.erl test.erl
	erl -s test run sorstu test
	erl -noshell -eval 'io:format("Result: ~p~n",[sorstu:m_time(fun() -> sorstu:sor_on_student(sorstu:student(), name)end)])' -s init stop
	erl -noshell -eval 'io:format("Result: ~p~n",[sorstu:m_time(fun() -> sorstu:viv()end)])' -s init stop	

runli:
	erlc list2.erl
	erl -noshell -eval 'io:format("Result: ~p~n",[list2:m_time(fun() -> list2:sor_generate(list2:generate(1000,100))end)])' -s init stop

runli1:
	erl -noshell -eval 'io:format("Result: ~p~n",[list2:m_time(fun() -> lists:sort(list2:generate(1000,100))end)])' -s init stop

clean:
	-rm *.beam
