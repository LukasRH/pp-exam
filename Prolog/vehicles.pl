/* Programming Paradigms Mini Project 3
 * 
 * Author: Lukas Rønsholt
 * Student No.: 20166653
 * Email: lransh16@student.aau.dk
 *
 */

/* What is a car, a car is a car, but a taxi is also a car */
is_car(car).
is_car(taxi).

/* Define vehicle types */
is_vehicle(bicycle).
is_vehicle(bus).
is_vehicle(X) :-
    is_car(X).

/* availble_vehicles
Get the type of vehicles that can be used for by a group
 */
available_vehicles(Size, Res) :-
    (Size == 1,
     findall(X, is_vehicle(X), Res),!;
     Size =< 4,
     findall(X, (is_vehicle(X), X \== bicycle), Res),!;
     Size > 4,
     findall(X, (is_vehicle(X), X == bus), Res),!
     ).

/* vehicles that are allowed on a motorway */
allowed_on_motorway(X) :-
    is_vehicle(X),
    not(X = bicycle).

/* vehicles that are allowed on a pavedroad */
allowed_on_pavedroads(X) :-
    is_vehicle(X).

/* vehicles that are allowed on a gravelroad */
allowed_on_gravelroads(X) :-
    not(X = bus),
    is_vehicle(X).

/* allowed_on_road
check if a vehicle is allowed on a road
 */
allowed_on_road(X, [H|_]) :- allowed_on_road(X, H).
allowed_on_road(X, [H|T]) :-
    not(allowed_on_road(X, H)),
    allowed_on_road(X, T).
allowed_on_road(motorway, X) :- allowed_on_motorway(X).
allowed_on_road(pavedroad, X) :- allowed_on_pavedroads(X).
allowed_on_road(gravelroad, X) :- allowed_on_gravelroads(X).

/* get all vehicles that is allowed on a road */
get_allowed_vehicles(X, Res) :- 
    findall(V, (is_vehicle(V), allowed_on_road(X,V)), Res).
