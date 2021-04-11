% Program lets you set a calorie goal, and your current intake of food and gives a summary
% of the macro-nutrients you have consumed. The user can then ask questions in natural Englsih
% to perform a query on the food database.

%Planned Queries:

% What should/can I eat?
% EFFECT: returns all foods that fit withiin calorie budget

% How many/much [NUTRIENT] are in [FOOD]?
% EFFECT: returns the amount of specified nutrient

% What [FOODTYPE] should I eat?
% EFFECT: returns the foods that fit within calorie budget filetered by FOODTYPE

:- ensure_loaded(foodAPI).

q :- write("What is your calorie goal?: "), flush_output(current_output),
  readln(Goal),
  write("What did you eat today?: "), flush_output(current_output),
  readln(Diet),
  write("How much in grams did you eat?: "), flush_output(current_output),
  readln(Grams),
  printSummary(Diet, Goal, Grams).
  %%write("\nWhat would you like to know?: "), flush_output(current_output),
  %%readln(Query).
  % Process the query

% determines which command user wants
check_command(['print', 'summary' | _]) :- q, !.
check_command(['commands' | _]) :- commands, !.
check_command(_) :- write("invalid command\n"), !.

% commands
commands :- write("type 'print summary' to show the nutrients you have consumed today\n").

% checks if user wants to quit
check_quit(['quit' | _ ]) :- !.
check_quit(X) :- check_command(X), start.

% Point of entry, enter start. to run the program.
start:-
  write("type 'commands' to get a list of commands or 'quit' to exit\n"),
  flush_output(current_output),
  readln(X),
  check_quit(X).


printSummary(Diet, Goal, [Grams]) :-
  %maplist(upcase_atom, Diet, Cased_Diet),
  list_string_concat(Diet, R),
  search(R, Prot, Cal, Carb, F),
  Scaling is Grams/100,
  Protein is Scaling*Prot,
  Cals is Scaling*Cal,
  Carbs is Scaling*Carb,
  Fat is Scaling*F,
  %nutrient_total(Cased_Diet, "calorie", Cals),
  %nutrient_total(Cased_Diet, "fat", Fat),
  %nutrient_total(Cased_Diet, "carb", Carbs),
  %nutrient_total(Cased_Diet, "protein", Protein),
  write("\nTOTAL CALORIES:\t\t\t"), write(Cals), write("\n"), flush_output(current_output),
  write("TOTAL FAT:\t\t\t"), write(Fat), write("g\n"), flush_output(current_output),
  write("TOTAL CARBOHYDRATES:\t\t"), write(Carbs), write("g\n"), flush_output(current_output),
  write("TOTAL PROTEIN:\t\t\t"), write(Protein), write("g\n"), flush_output(current_output),
  assert(ate(Diet)),
  assert(goal(Goal)),
  calc_remaining(Cals, Goal).

calc_remaining(Cals, Goal) :-
  Cals > Goal,
  D is Cals-Goal,
  write("\nYou have exceeded your goal by "), write(D), write(" calories.\n"), flush_output(current_output).
calc_remaining(Cals, Goal) :-
  Cals =< Goal,
  D is Goal-Cals,
  write("\nYou have "), write(D), write(" calories remaining for today.\n"), flush_output(current_output).

% Given a list of foods as a list of atoms and a nutrient, find the sum of the specified nutrient
nutrient_total([], _, 0).
nutrient_total([H|T], "calorie", Sum) :-
  food(H, _, Cal, _, _, _),
  nutrient_total(T, "calorie", Rest),
  Sum is Cal + Rest.

nutrient_total([H|T], "fat", Sum) :-
  food(H, _, _, Fat, _, _),
  nutrient_total(T, "fat", Rest),
  Sum is Fat + Rest.

nutrient_total([H|T], "carb", Sum) :-
  food(H, _, _, _, Carb, _),
  nutrient_total(T, "carb", Rest),
  Sum is Carb + Rest.

nutrient_total([H|T], "protein", Sum) :-
  food(H, _, _, _, _, Protein),
  nutrient_total(T, "protein", Rest),
  Sum is Protein + Rest.

% Return all foods less than the calorie count.
budget_query(Budget) :-
  budget_search(Budget, Food),
  atom_concat("You can eat: ", Food, Res),
  print(Res), flush_output(current_output).

budget_search(Budget, Food) :-
  food(Name, _, Cal, _, _, _),
  Cal =< Budget,
  Food = Name.

% contatenates a list of strings into a single string
list_string_concat([], '').
list_string_concat([H|T], R) :- list_string_concat(T, R1), string_concat(H, ' ', R2), string_concat(R2, R1, R).

% database of foods. Fields are in the format of:
% food(NAME, FOODTYPE, CALORIES, FAT, CARBS, PROTEIN)
% available FOODTYPE: FRUIT, MEAT, SEAFOOD, VEGETABLE, JUNK, DAIRY, GRAIN
food('APPLE', ["FRUIT"], 95, 0.3, 25, 0.5).
food('ORANGE', ["FRUIT"], 62, 0.2, 15, 1.2).
