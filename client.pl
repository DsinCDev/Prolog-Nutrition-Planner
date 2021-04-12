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

:- dynamic goal/1.
:- dynamic calories_eaten/1.
:- dynamic protein_eaten/1.
:- dynamic fat_eaten/1.
:- dynamic carbohydrate_eaten/1.


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

% if already have a calorie goal, then change it
set_goal(Goal) :- 
  retract(goal(X)),
  assert(goal(Goal)),
  write("Calorie goal set as: "),
  write(Goal),
  write("\n").

% if don't have a calorie goal, then just assert a new goal
set_goal(Goal) :- 
  assert(goal(Goal)),
  write("Calorie goal set as: "),
  write(Goal),
  write("\n").


% if calories eaten has been set, then calculate remaining calories
check_goal :-
  goal(Goal),
  calories_eaten(Cal_eaten),
  number(Goal),
  number(Cal_eaten),
  write("Calorie goal: "),
  write(Goal),
  write("\n"),
  Remaining_cals is Goal-Cal_eaten,
  write("Calories remaining: "),
  write(Remaining_cals),
  write("\n").

% if calories eaten has not been set, then just use the goal as remaining calories
check_goal :-
  goal(Goal),
  number(Goal),
  write("Calorie goal: "),
  write(Goal),
  write("\n"),
  write("Calories remaining: "),
  write(Goal),
  write("\n").


% adds a meal and updates the calories/protein/carbohydrates/fat eaten for the day
add_meal :-
  protein_eaten(Prev_protein),
  calories_eaten(Prev_calories),
  carbohydrate_eaten(Prev_carbs),
  fat_eaten(Prev_fat),
  write("What did you eat today?: "), flush_output(current_output),
  readln(Meal),
  list_string_concat(Meal, Food),
  write("How much in grams did you eat?: "), flush_output(current_output),
  readln([Grams]),
  search(Food, Prot, Cal, Carb, F),
  Scaling is Grams/100,
  Protein is Scaling*Prot,
  Cals is Scaling*Cal,
  Carbs is Scaling*Carb,
  Fat is Scaling*F,
  New_protein is Prev_protein + Protein,
  New_calories is Prev_calories + Cals,
  New_carbs is Prev_carbs + Carbs,
  New_fat is Prev_fat + Fat,
  retract(protein_eaten(Prev_protein)),
  retract(calories_eaten(Prev_calories)),
  retract(carbohydrate_eaten(Prev_carbs)),
  retract(fat_eaten(Prev_fat)),
  assert(protein_eaten(New_protein)),
  assert(calories_eaten(New_calories)),
  assert(carbohydrate_eaten(New_carbs)),
  assert(fat_eaten(New_fat)),
  write("\n"), write(Food), write("Nutrition Data:\n\n"),
  write("CALORES:\t\t\t"), write(Cals), write("\n"), flush_output(current_output),
  write("FAT:\t\t\t\t"), write(Fat), write("\n"), flush_output(current_output),
  write("CARBOHYDRATES:\t\t\t"), write(Carbs), write("\n"), flush_output(current_output),
  write("PROTEIN:\t\t\t"), write(Protein), write("\n"), flush_output(current_output),
  write("\nUpdated macronutrients:\n\n"),
  write("\nTOTAL CALORIES:\t\t\t"), write(New_calories), write("\n"), flush_output(current_output),
  write("TOTAL FAT:\t\t\t"), write(New_fat), write("g\n"), flush_output(current_output),
  write("TOTAL CARBOHYDRATES:\t\t"), write(New_carbs), write("g\n"), flush_output(current_output),
  write("TOTAL PROTEIN:\t\t\t"), write(New_protein), write("g\n"), flush_output(current_output).

% if no previous value for macronutrients, then just assert the new ones
add_meal :-
  write("What did you eat today?: "), flush_output(current_output),
  readln(Meal),
  list_string_concat(Meal, Food),
  write("How much in grams did you eat?: "), flush_output(current_output),
  readln([Grams]),
  search(Food, Prot, Cal, Carb, F),
  Scaling is Grams/100,
  Protein is Scaling*Prot,
  Cals is Scaling*Cal,
  Carbs is Scaling*Carb,
  Fat is Scaling*F,
  assert(protein_eaten(Protein)),
  assert(calories_eaten(Cals)),
  assert(carbohydrate_eaten(Carbs)),
  assert(fat_eaten(Fat)),
  write("\n"), write(Food), write("Nutrition Data:\n\n"),
  write("CALORES:\t\t\t"), write(Cals), write("\n"), flush_output(current_output),
  write("FAT:\t\t\t\t"), write(Fat), write("\n"), flush_output(current_output),
  write("CARBOHYDRATES:\t\t\t"), write(Carbs), write("\n"), flush_output(current_output),
  write("PROTEIN:\t\t\t"), write(Protein), write("\n"), flush_output(current_output),
  write("\nUpdated macronutrients:\n\n"),
  write("\nTOTAL CALORIES:\t\t\t"), write(Cals), write("\n"), flush_output(current_output),
  write("TOTAL FAT:\t\t\t"), write(Fat), write("g\n"), flush_output(current_output),
  write("TOTAL CARBOHYDRATES:\t\t"), write(Carbs), write("g\n"), flush_output(current_output),
  write("TOTAL PROTEIN:\t\t\t"), write(Protein), write("g\n"), flush_output(current_output).


% determines which command user wants
check_command(['print', 'summary' | _]) :- printSummary, !.
check_command(['commands' | _]) :- commands, !.
check_command(['set', 'goal', X]) :- set_goal(X), !.
check_command(['check', 'goal']) :- check_goal, !.
check_command(['check', 'goal']) :- \+check_goal, write("Set a goal first!\n"), !. % if no calorie goal has been set
check_command(['add', 'meal']) :- add_meal, !.
check_command(_) :- write("invalid command\n"), !.

% commands
commands :-
  write("Type 'print summary' to show the nutrients you have consumed today.\n"),
  write("Type 'set goal <calories>' to set your calorie goal.\n"),
  write("Type 'check goal' to see your calorie goal and remaining calories.\n"),
  write("Type 'add meal' to add a meal that you have eaten.\n").


% checks if user wants to quit
check_quit(['quit' | _ ]) :- !.
check_quit(X) :- check_command(X), start.

% Point of entry, enter start. to run the program.
start:-
  write("type 'commands' to get a list of commands or 'quit' to exit\n"),
  flush_output(current_output),
  readln(X),
  check_quit(X).


% if all a goal and macronutrients eaten have been set, then print them
printSummary :-
  goal(Goal),
  calories_eaten(Cals),
  protein_eaten(Protein),
  carbohydrate_eaten(Carbs),
  fat_eaten(Fat),

  %nutrient_total(Cased_Diet, "calorie", Cals),
  %nutrient_total(Cased_Diet, "fat", Fat),
  %nutrient_total(Cased_Diet, "carb", Carbs),
  %nutrient_total(Cased_Diet, "protein", Protein),
  write("CALORIE GOAL:\t\t\t\t"), write(Goal), write(" calories\n"), flush_output(current_output),
  write("TOTAL CALORIES CONSUMED:\t\t"), write(Cals), write(" calories\n"), flush_output(current_output),
  write("TOTAL FAT CONSUMED:\t\t\t"), write(Fat), write("g\n"), flush_output(current_output),
  write("TOTAL CARBOHYDRATES CONSUMED:\t\t"), write(Carbs), write("g\n"), flush_output(current_output),
  write("TOTAL PROTEIN CONSUMED:\t\t\t"), write(Protein), write("g\n"), flush_output(current_output).
  %assert(ate(Diet)),
  %assert(goal(Goal)),
  %calc_remaining(Cals, Goal).

% if no goal has been set but macronutrients have been eaten
printSummary :-
  calories_eaten(Cals),
  protein_eaten(Protein),
  carbohydrate_eaten(Carbs),
  fat_eaten(Fat),
  write("CALORIE GOAL:\t\t\t\t"), write("not set"), write("\n"), flush_output(current_output),
  write("TOTAL CALORIES CONSUMED:\t\t"), write(Cals), write(" calories\n"), flush_output(current_output),
  write("TOTAL FAT CONSUMED:\t\t\t"), write(Fat), write("g\n"), flush_output(current_output),
  write("TOTAL CARBOHYDRATES CONSUMED:\t\t"), write(Carbs), write("g\n"), flush_output(current_output),
  write("TOTAL PROTEIN CONSUMED:\t\t\t"), write(Protein), write("g\n"), flush_output(current_output).

% if only a goal has been set
printSummary :-
  goal(Goal),
  write("CALORIE GOAL:\t\t\t\t"), write(Goal), write(" calories\n"), flush_output(current_output),
  write("TOTAL CALORIES CONSUMED:\t\t"), write(0), write(" calories\n"), flush_output(current_output),
  write("TOTAL FAT CONSUMED:\t\t\t"), write(0), write("g\n"), flush_output(current_output),
  write("TOTAL CARBOHYDRATES CONSUMED:\t\t"), write(0), write("g\n"), flush_output(current_output),
  write("TOTAL PROTEIN CONSUMED:\t\t\t"), write(0), write("g\n"), flush_output(current_output).

% if nothing else has been set, then just print 0s
printSummary :-
  write("CALORIE GOAL:\t\t\t\t"), write("not set"), write("\n"), flush_output(current_output),
  write("TOTAL CALORIES CONSUMED:\t\t"), write(0), write(" calories\n"), flush_output(current_output),
  write("TOTAL FAT CONSUMED:\t\t\t"), write(0), write("g\n"), flush_output(current_output),
  write("TOTAL CARBOHYDRATES CONSUMED:\t\t"), write(0), write("g\n"), flush_output(current_output),
  write("TOTAL PROTEIN CONSUMED:\t\t\t"), write(0), write("g\n"), flush_output(current_output).

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
