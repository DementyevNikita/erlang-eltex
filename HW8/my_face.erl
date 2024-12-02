-module(my_face).
-export([main/0]).


main() ->
	io:format("Добро пожаловать в БД \"student\" !~n"),
	menu().

menu() ->
	io:format("Меню: ~n"),
	io:format("1. Добавить ~n"),
	io:format("2. Поиск ~n"),
	io:format("3. Удалить ~n"),
	io:format("4. Редактировать ~n"),
	io:format("5. БД ~n"),
	io:format("6. Выйти ~n"),
	case io:get_line("") of
		"1\n" -> in_db(),
menu();
		"2\n" -> fn_db(),
menu();
		"3\n" -> del_db(),
menu();
		"4\n" -> upd_db(),
menu();
		"5\n" -> sor_db(),
menu();
		"6\n" -> 
			io:format("Выход ~n"),
			ok;
		_ ->
			io:format("Неверный выбор ~n"),
			menu()
	end.

in_db() ->
	io:format("Добавление: ~n"),
	{Id,[]} = i_int("Введите Id: "),
	Name = i_str("Введите имя: "),
	{Age,[]} = i_int("Введите возраст: "),
	Gender = i_str("Введите пол(male/female): "),
	{Score,[]} = i_int("Введите балл(1-5): "),
	my_bd:in_table(Id,Name,Age,Gender,Score),
	io:format("Запись добавлена ~n").

fn_db() ->
	io:format("Поиск: ~n"),
	io:format("1.по Id ~n"),
	io:format("2.по имени ~n"),
	io:format("3.по полу ~n"),
	io:format("4.по баллу ~n"),
	io:format("Введите номер: ~n"),
	ColCho = io:get_line(""),
	case string:trim(ColCho) of
		"1" ->
			Id = i_str("Введите Id: "),
			my_bd:find_table("id", Id);
		"2" ->
			Name = i_str("Введите имя: "),
			my_bd:find_table("name", Name);
		"3" ->
			Gender = i_str("Введите пол(male/female): "),
			my_bd:find_table("gender", Gender);
		"4" ->
			Score = i_str("Введите балл(1-5): "),
			my_bd:find_table("score", Score);
		_ ->
			io:format("Неверный выбор ~n")
	end.

del_db() ->
	io:format("Удаление: ~n"),
        io:format("1.по Id ~n"),
        io:format("2.по имени ~n"),
        io:format("3.по полу ~n"),
        io:format("4.по баллу ~n"),
        io:format("Введите номер: ~n"),
        ColCho = io:get_line(""),
        case string:trim(ColCho) of
                "1" ->
                        Id = i_str("Введите Id: "),
                        my_bd:del_table("id", Id);
                "2" ->
                        Name = i_str("Введите имя: "),
                        my_bd:del_table("name", Name);
                "3" ->
                        Gender = i_str("Введите пол(male/female): "),
                        my_bd:del_table("gender", Gender);
                "4" ->
                        Score = i_str("Введите балл(1-5): "),
                        my_bd:del_table("score", Score);
                _ ->
                        io:format("Неверный выбор ~n")
        end.

upd_db() ->
	io:format("Редактирование: ~n"),
	Id = i_str("Введите Id записи: "),
	io:format("1.изменить имя ~n"),
        io:format("2.изменить возраст ~n"),
        io:format("3.изменить пол ~n"),
        io:format("4.изменить балл ~n"),
	UpdCho = io:get_line(""),
	case string:trim(UpdCho) of
                "1" ->
                        Name = i_str("Введите новое имя: "),
                	my_bd:upd_table("name", Id, Name);
		"2" ->
                        Age = i_str("Введите новый возраст: "),
                        my_bd:upd_table("age", Id, Age);
                "3" ->
                        Gender = i_str("Введите новый пол(male/female): "),
                        my_bd:upd_table("gender",Id, Gender);
                "4" ->
                        Score = i_str("Введите новый балл(1-5): "),
                        my_bd:upd_table("score",Id, Score);
                _ ->
                        io:format("Неверный выбор ~n")
        end.

sor_db() -> 
	my_bd:sorted().

i_int(Message) ->
	io:format(Message),
	Line = string:trim(io:get_line("")),
	io:format("Ввод: ~p~n", [Line]),
	case string:to_integer(Line) of
		{Int,[]} -> 
			{Int,[]};
		_ ->
			io:format("Ошибка ввода ~n"),
			i_int(Message)
	end.

i_str(Message) ->
	io:format(Message),
	string:trim(io:get_line("")).



