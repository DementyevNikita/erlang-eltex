-module(sorstu).
-export([student/0, sor_id/2, sor_age/2, sor_name/2, sor_gender/2, sor_score/2, sor_fast/2, sor_on_student/2,viv/0, m_time/1]).
-include_lib("eunit/include/eunit.hrl").
-record(student, {id, name, gender, age, score}).

-spec student() -> list.
student() ->
	[
	 #student{id = 5,name = "Sasha",gender = male,age = 23,score = 5},
	 #student{id = 2,name = "Misha",gender = male,age =  27,score =  4},
	 #student{id = 4,name = "Lida",gender =  female,age =  35,score = 5},
	 #student{id = 3,name = "Bob",gender =  male,age =  19,score = 3},
	 #student{id = 1,name = "Lera",gender =  female,age = 21,score = 3}
	].
-spec m_time(Fun :: fun()) -> int.
m_time(Fun) ->
        StartTime = erlang:monotonic_time(),
        Result = Fun(),
        EndTime = erlang:monotonic_time(),
        ElapsedTime = erlang:convert_time_unit(EndTime - StartTime,native,microsecond),
        io:format("Exe time: ~p microsec ~n",[ElapsedTime]),
        Result.

sor_id(Stu1,Stu2) ->
	Stu1#student.id < Stu2#student.id.
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


-spec sor_on_student(Stu::list, Key :: atom) -> list.
sor_on_student(Stu, Key) ->
	case Key of
		id -> sor_fast(Stu, fun sor_id/2);
		age -> sor_fast(Stu, fun sor_age/2);
		name -> sor_fast(Stu, fun sor_name/2);
		gender -> sor_fast(Stu, fun sor_gender/2);
		score -> sor_fast(Stu, fun sor_score/2)
	end.
-spec viv() -> list.
viv() ->
	Student = student(),
	Sorlist = lists:sort(fun(Stu1, Stu2) -> sor_name(Stu1,Stu2)end,Student),
	io:format("Otvet~p~n",[Sorlist]),
	ok.

sor_on_student_test() ->
	S = student(),
	?assertEqual([{student,3,"Bob",male,19,3},
		      {student,1,"Lera",female,21,3},
		      {student,4,"Lida",female,35,5},
		      {student,2,"Misha",male,27,4},
		      {student,5,"Sasha",male,23,5}]
		   ,sor_on_student(S,name)),
	?assertEqual([{student,3,"Bob",male,19,3},
 		      {student,1,"Lera",female,21,3},
		      {student,5,"Sasha",male,23,5},
		      {student,2,"Misha",male,27,4},
		      {student,4,"Lida",female,35,5}],sor_on_student(S,age)),
	?assertEqual([{student,3,"Bob",male,19,3},
		      {student,1,"Lera",female,21,3},
		      {student,2,"Misha",male,27,4},
		      {student,5,"Sasha",male,23,5},
		      {student,4,"Lida",female,35,5}],sor_on_student(S,score)),
	?assertEqual( [{student,4,"Lida",female,35,5},
 		       {student,1,"Lera",female,21,3},
		       {student,5,"Sasha",male,23,5},
 		       {student,2,"Misha",male,27,4},
 		       {student,3,"Bob",male,19,3}],sor_on_student(S,gender)),
        ?assertEqual([{student,1,"Lera",female,21,3},
 		      {student,2,"Misha",male,27,4},
 		      {student,3,"Bob",male,19,3},
 		      {student,4,"Lida",female,35,5},
 		      {student,5,"Sasha",male,23,5}],sor_on_student(S,id)),
        ok.


%viv_test() ->
%	?assertEqual("Otvet",[{student,3,"Bob",male,19,3},
%                      {student,1,"Lera",female,21,3},
 %                     {student,4,"Lida",female,35,5},
  %                    {student,2,"Misha",male,27,4},
   %                   {student,5,"Sasha",male,23,5}]
    %               ,viv()),
%	ok.
