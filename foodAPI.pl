:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

:- ensure_loaded(secret_key). % load api key

% nutrientId's from FDC
calories(1008).
protein(1003).
carbohydrates(1005).
fat(1004).


%test :- key(X), string_concat('https://api.nal.usda.gov/fdc/v1/food/1621366?api_key=', X, URL), http_get(URL, In, []), write(In).

% search for the food and return the nutrients
search(Q, Protein, Calories, Carbs, Fat) :- 
    key(X),
    string_concat('https://api.nal.usda.gov/fdc/v1/foods/search?pageSize=1&query=',Q, URL1),
    string_concat(URL1, '&api_key=', URL2), string_concat(URL2, X, URL), 
    http_get(URL, In,[request_header('Accept'='application/json')]),
    atom_json_dict(In, D, []),
    [D1] = D.get('foods'),
    D2 = D1.get('foodNutrients'),
    get_protein(D2, Protein),
    get_calories(D2, Calories),
    get_carbohydrates(D2, Carbs),
    get_fat(D2, Fat).



get_protein([H|T], Value) :- protein(X), H.get('nutrientId') =:= X, Value = H.get('value').
get_protein([H|T], Value) :- get_protein(T, Value).

get_calories([H|T], Value) :- calories(X), H.get('nutrientId') =:= X, Value = H.get('value').
get_calories([H|T], Value) :- get_calories(T, Value).

get_carbohydrates([H|T], Value) :- carbohydrates(X), H.get('nutrientId') =:= X, Value = H.get('value').
get_carbohydrates([H|T], Value) :- get_carbohydrates(T, Value).

get_fat([H|T], Value) :- fat(X), H.get('nutrientId') =:= X, Value = H.get('value').
get_fat([H|T], Value) :- get_fat(T, Value).
