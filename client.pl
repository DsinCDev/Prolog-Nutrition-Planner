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

q:-
  write("What is your calorie goal?: "), flush_output(current_output),
  readln(Goal),
  write("What did you eat today?: "), flush_output(current_output),
  readln(Diet),
  printSummary(Diet, Goal),
  write("\nWhat would you like to know?: "), flush_output(current_output),
  readln(Query).
  % Process the query


% Point of entry, enter start. to run the program.
start:-
  not(q),
  write("Invalid input, please try again").

printSummary(Diet, Goal) :-
  maplist(upcase_atom, Diet, Cased_Diet),
  nutrient_total(Cased_Diet, "calorie", Cals),
  nutrient_total(Cased_Diet, "fat", Fat),
  nutrient_total(Cased_Diet, "carb", Carbs),
  nutrient_total(Cased_Diet, "protein", Protein),
  write("\nTOTAL CALORIES:\t\t\t"), write(Cals), write("\n"), flush_output(current_output),
  write("TOTAL FAT:\t\t\t"), write(Fat), write("g\n"), flush_output(current_output),
  write("TOTAL CARBOHYDRATES:\t\t"), write(Carbs), write("g\n"), flush_output(current_output),
  write("TOTAL PROTEIN:\t\t\t"), write(Protein), write("g\n"), flush_output(current_output),
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


% database of foods. Fields are in the format of:
% food(NAME, FOODTYPE, CALORIES, FAT, CARBS, PROTEIN)
% available FOODTYPE: FRUIT, MEAT, SEAFOOD, VEGETABLE, JUNK, DAIRY, GRAIN
food('APPLE', ["FRUIT"], 95, 0.3, 25, 0.5).
food('ORANGE', ["FRUIT"], 62, 0.2, 15, 1.2).
