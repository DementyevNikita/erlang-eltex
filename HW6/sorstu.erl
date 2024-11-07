-module(sorstu).
-export([student/0,sor_age/2, sor_name/2, sor_gender/2, sor_score/2, sor_fast/2, sor_on_student/2,viv/0, m_time/1]).
-record(student, {id, name, gender, age, score}).

student() ->
	[
	 #student{id = 5,name = "Sasha",gender = male,age = 23,score = 5},
	 #student{id = 2,name = "Misha",gender = male,age =  27,score =  4},
	 #student{id = 4,name = "Lida",gender =  female,age =  35,score = 5},
	 #student{id = 3,name = "Bob",gender =  male,age =  19,score = 3},
	 #student{id = 1,name = "Lera",gender =  female,age = 21,score = 3}
	].

m_time(Fun) ->
        StartTime = erlang:monotonic_time(),
        Result = Fun(),
        EndTime = erlang:monotonic_time(),
        ElapsedTime = erlang:convert_time_unit(EndTime - StartTime,native,microsecond),
        io:format("Exe time: ~p microsec ~n",[ElapsedTime]),
        Result.

sor_age(Stu1,Stu2) ->
	Stu1#student.age < Stu2#student.age.
sor_name(Stu1,Stu2) ->
        Stu1#student.name < Stu2#student.name.
sor_gender(Stu1,Stu2) ->
        Stu1#student.gender < Stu2#student.gender.
sor_score(Stu1,Stu2) ->
        Stu1#student.score < Stu2#student.score.


sor_fast([], _Sor)  -> [];
sor_fast([Pivot|Rest], Sor) ->
	sor_fast([X||X <- Rest, Sor(X, Pivot)], Sor) ++
	[Pivot]++
	sor_fast([X ||X <- Rest,not Sor(X, Pivot)], Sor).



sor_on_student(Stu, Key) ->
	case Key of
		age -> sor_fast(Stu, fun sor_age/2);
		name -> sor_fast(Stu, fun sor_name/2);
		gender -> sor_fast(Stu, fun sor_gender/2);
		score -> sor_fast(Stu, fun sor_score/2)
	end.

viv() ->
	Student = student(),
	Sorlist = lists:sort(fun(Stu1, Stu2) -> sor_name(Stu1,Stu2)end,Student),
	io:format("Sortirovka: ~p~n", [Sorlist]),
	ok.



