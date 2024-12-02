-module(my_bd).
-export([start/0, in_table/5,del_table/2, upd_table/3, find_table/2, sorted/0, stop/1]).

start() ->
	odbc:start(),
	{ok,Ref} = odbc:connect("DSN=pot123", []),
	Ref.

in_table(Id, Name, Age, Gender, Score) ->
	Ref = start(),
	odbc:param_query(Ref, "INSERT INTO student (id, name, age, gender, score) VALUES (?, ?, ?, ?, ?)",[{sql_integer, [Id]}, {{sql_varchar,10},[Name]},{sql_integer, [Age]}, {{sql_varchar,8},[Gender]}, {sql_integer, [Score]}]),
	stop(Ref).

del_table(Column,Value) ->
	Ref = start(),
	Query = io_lib:format("DELETE FROM student WHERE ~s = '~s'", [Column, Value]),
	Res = odbc:sql_query(Ref,Query),
	case Res of
		{updated, Count} when Count > 0 ->
			io:format("Удалено.~n");
		{updated, 0} ->
			io:format("Записи не существует.~n")
	end,
	stop(Ref).

upd_table(Column, Id, NewV) ->
	Ref = start(),
        Query = io_lib:format("UPDATE student SET ~s = '~s' WHERE id = '~s'", [Column, NewV, Id]),
        Res = odbc:sql_query(Ref,Query),
	case Res of
                {updated, Count} when Count > 0 ->
                        io:format("Изменено ~p.~n",[Count]);
                {updated, 0} ->
                        io:format("Запись не обновлена.~n")
	end,
	stop(Ref).

find_table(Column,Value) ->
        Ref = start(),
        Query = io_lib:format("SELECT * FROM student WHERE ~s = '~s'", [Column,Value]),
	{selected,_,Rows} = odbc:sql_query(Ref, Query),
	case Rows of
		[] ->
			io:format("Запись не найдена ~s = '~s' .~n", [Column,Value]);
		_ ->
			lists:foreach(fun({Id, Name, Age, Gender, Score}) -> io:format("Id: ~p, Name: ~p, Age: ~p, Gender: ~p, Score: ~p~n", [Id, Name, Age, Gender, Score]) end, Rows)
	end,
	stop(Ref).

sorted() ->
	Ref = start(),
	{selected, _, Rows}=odbc:sql_query(Ref,"SELECT *  FROM student ORDER BY id ASC"),
	lists:foreach(fun({Id, Name, Age, Gender, Score}) -> io:format("Id: ~p, Name: ~p, Age: ~p, Gender: ~p, Score: ~p~n", [Id, Name, Age, Gender, Score]) end, Rows),
	stop(Ref).

stop(Ref) ->
	odbc:disconnect(Ref).

