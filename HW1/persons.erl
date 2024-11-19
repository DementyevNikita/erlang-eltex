-module(persons).
-export([create_persons/0,create_NewPerson/0, main/1]).

main (_) ->
  Persons = create_persons(),
  io:fwrite("Persons: ~p~n",[Persons]),

  NewPerson = create_NewPerson(),
  io:fwrite("NewPerson: ~p~n",[NewPerson]),

  Suma = [Persons | [NewPerson]], %- складывает список из кортежей и список из одного кортежа и получает список состоящий из списка и кортежа% 
  io:fwrite("Suma: ~p~n",[Suma]),

  Suma1 = [NewPerson | Persons], % - складывает кортеж и список из кортежей, и получает один список состоящий из кортежей%
  io:fwrite("Suma1: ~p~n",[Suma1]),
  
  %Suma2 = NewPerson ++ Persons,  - не может сложить кортеж и список таким способом
  %io:fwrite("Suma2: ~p~n",[Suma2]),%

  Suma3 = [NewPerson] ++ Persons,  % - складывает список с кортежем и список из нескольких кортежей, и получает результат аналогично Suma1%
  io:fwrite("Suma3: ~p~n",[Suma3]),
  
  %Suma4 = [NewPerson] + Persons, - нельзя сложить два саиска таким способом, потому что знак + используется только для чисел%
  %io:fwrite("Suma4: ~p~n",[Suma4]),%

  Suma5 = Persons, % - проверка списка %
  io:fwrite("Suma5: ~p~n",[Suma5]),

  io:fwrite("Itog: ~p~n", [+10]),

  io:fwrite("Itog1: ~p~n", [-10]),

  io:fwrite("Itog2: ~p~n", [1+11]),

  io:fwrite("Itog3: ~p~n", [8/2]),

  io:fwrite("Itog4: ~p~n", [9 div 2]),

  io:fwrite("Itog5: ~p~n", [7 rem 2]),

  io:fwrite("Itog6: ~p~n", [2#10 band 2#01]),

  io:fwrite("Itog7: ~p~n", [2#10 bor 2#01]),

  %io:fwrite("Itog8: ~p~n", [a + 10]), - не может сложить атом и число%

  %io:fwrite("Itog9: ~p~n", [1 bsl(1 bsl 64)]), - bsl двигает знак влево на аргумент, в данном случае сначала на 64 знака, а потом 1 двигает на полученное число, так как число меньше 1 он не может его подвинуть%
  
  io:fwrite("Slovo: ~p~n", ["Night"]),
  
  io:fwrite("Slovo2: ~p~n", [binary_to_list(term_to_binary("Night"))]),
  
  io:fwrite("Slovo1: ~p~n", [<<78,105,103,104,116>>]). % - list_to_tuple("Night") получил числа с помощью функции%
  


create_persons() ->
  [
    {person, 1, "Bob", 23, male},
    {person, 2, "Kate", 20, female},
    {person, 3, "Jack", 34, male},
    {person, 4, "Nata", 54, female}
  ].

create_NewPerson() ->
  {person, 5, "Mike", 13, male}.


