-module(records).
-export([create_persons/0,create_rec/0, main/1]).
-record(person, {id, name, age, gender}).

main (_) ->
  Persons = create_persons(),
  io:fwrite("Persons: ~p~2n",[Persons]),
  io:fwrite("Zadanie 1, Punkt 1: ~p~2n",[[First | Rest] = Persons]), % с помощью сложения кортежей делим список так, чтобы левая часть была равна правой, в следствии чего получается переменная First с первым кортежем из списка и Rest с оставшимися кортежами%
  io:fwrite("First: ~p~2n",[First]),
  io:fwrite("Rest: ~p~2n",[Rest]),
  io:fwrite("Zadanie 1, Punkt 2: ~p~2n",[[Second | Rest1] = Rest]), % с помощью сложения кортежей делим список так, чтобы левая часть была равна правой, в следствии чего получается переменная Second с первым кортежем из списка и уже Rest1 с оставшимися кортежами от Persons% 
  io:fwrite("Second: ~p~2n",[Second]),
  io:fwrite("Rest1: ~p~2n",[Rest1]),
  io:fwrite("Zadanie 1, Punkt 3: ~p~2n",[[Third, Fourth | Rest2] = Rest1]), % по аналогии с остальными пуктами получаем последнии два кортежа и оставшийся список, так как данные закончились и мы присвоили всем кортежам переменные, то список получается пустым% 
  io:fwrite("Third: ~p~2n",[Third]),
  io:fwrite("Fourth: ~p~2n",[Fourth]),
  io:fwrite("Rest2: ~p~2n",[Rest2]),
  
  Persons1 = create_rec(),


  io:fwrite("Zadanie 2: ~p~2n",[Persons1]),
  io:fwrite("Zadanie 2, Punkt 1: ~p~2n",[[FirstPerson | _] = Persons1]), % Выносим из списка первый кортеж, а нижнее подчеркивание позволяет нам не определять новую переменную для оставшихся кортежей%
  io:fwrite("FirstPerson: ~p~2n",[FirstPerson]),
  io:fwrite("Zadanie 2, Punkt 2: ~p~2n",[[_, SecondPerson, _, _] = Persons1]), % В данном случае выносим 2 кортеж через запятые и присваиваем ему значение, а оставшиеся мы определяем как подчеркивание для того чтобы не задавать переменные%
  io:fwrite("SecondPerson: ~p~2n",[SecondPerson]),
  %io:fwrite("Zadanie 2, Punkt 3: ~p~2n",[[_, _, SecondPerson,_] = Persons1]), определяется как ошибка, из-зправойа того, что SecondPerson уже задан и является вторым в списке кортежей, здесь же мы его задаем как 3, поэтому левая сторона выражения не равна% 
  io:fwrite("Zadanie 2, Punkt 4 - SecondName: ~p~2n",[SecondName = SecondPerson#person.name]),
  io:fwrite("Zadanie 2, Punkt 4 - SecondAge: ~p~2n",[SecondAge = SecondPerson#person.age]),
  io:fwrite("Zadanie 2, Punkt 5: ~p~2n",[Persons1]),
  io:fwrite("Zadanie 2, Punkt 6 - SecondAge: ~p~2n",[SecondPerson#person{age = 21}]),
  io:fwrite("Slovo: ~p~2n", ["Night"]),
  io:fwrite("Slovo2: ~p~2n", [binary_to_list(term_to_binary("Night"))]),
  io:fwrite("Slovo4: ~p~2n", [<<78,105,103,104,116>>]). % - list_to_tuple("Night") получил числа с помощью функции%
create_persons() ->
  [
    {person, 1, "Bob", 23, male},
    {person, 2, "Kate", 20, female},
    {person, 3, "Jack", 34, male},
    {person, 4, "Nata", 54, female}
  ].
create_rec() ->
  [
    #person{id = 1, name = "Bob", age = 23, gender = male}, 
    #person{id = 2, name = "Kate", age = 20, gender = female},
    #person{id = 3, name = "Jack", age = 34, gender = male},
    #person{id = 4, name = "Nata", age = 54, gender = female}
  ].

