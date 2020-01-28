/* Programming Paradigms Mini Project 3
 * 
 * Author: Lukas Rønsholt
 * Student No.: 20166653
 * Email: lransh16@student.aau.dk
 *
 */

/* load in vehicles rules and facts */
:- [vehicles].

/* traveler
A traveler consist of an identifer, and a list of vehicles.
 */
traveler(_,[]).
traveler(_,[V]) :- is_vehicle(V),!.
traveler(_,[H|T]) :- 
    is_vehicle(H),
    not(member(H,T)),
    traveler(_,T),!.

/* count_group
Get the number of travelers in a group of travelers
 */
count_group(List, Count) :- count_group(List,0,Count).
count_group([], Count, Count).
count_group([_|T],N, Count) :- 
    M is N + 1,
    count_group(T, M, Count).

/* get_traveler_vehicles
Get the vehicles that a traveler have connectet to it
 */
get_traveler_vehicles(traveler(_, List), List).

/* get_vehicles_from_group
Get a list of what vehicle options that a group of travelers have
 */
get_vehicles_from_group([H|T], Res) :-
    get_traveler_vehicles(H, Vehicles),
    get_vehicles_from_group(T,Vehicles,Res).
get_vehicles_from_group([H|T], Vehicles, Res) :-
    get_traveler_vehicles(H, V),
    append(Vehicles, V, R),
    get_vehicles_from_group(T, R, Res).
get_vehicles_from_group([], Vehicles, Res) :-
    sort(Vehicles, Res).