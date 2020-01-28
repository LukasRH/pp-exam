/* Programming Paradigms Mini Project 3
 * 
 * Author: Lukas Rønsholt
 * Student No.: 20166653
 * Email: lransh16@student.aau.dk
 *
 */

/* Load in our roads, traveler and vehicles rules and facts */
:- [travelers, roads].

/* can_travel 
rules to se if there is a road that can be used, and opholds a condition.
Also finds the vehicles that are allowed to use the road.
*/
% Traveler is allowed to use the road
can_travel(traveler(_,V),A,B,Time,Length,Vehicles) :- 
    (road(A,B,Type,Length,Time) ; road(B,A,Type,Length,Time)),
    allowed_on_road(Type, V),
    get_allowed_vehicles(Type, Vehicles).
% The road type matches one of the given types
can_travel(Types,A,B,Time,Length,Vehicles) :-
    (road(A,B,Type,Length,Time) ; road(B,A,Type,Length,Time)),
    member(Type, Types),
    get_allowed_vehicles(Type, Vehicles).
% Only accept roads which one of the given vehicles can use
can_travel(Vehicles,A,B,Time,Length,AVehicles) :-
    (road(A,B,Type,Length,Time) ; road(B,A,Type,Length,Time)),
    allowed_on_road(Type, Vehicles),
    get_allowed_vehicles(Type, AVehicles).
% No condition, there just have to be a road
can_travel(none,A,B,Time,Length,Vehicles) :-
    (road(A,B,Type,Length,Time) ; road(B,A,Type,Length,Time)),
    get_allowed_vehicles(Type,Vehicles).

/* path
Finds paths from A to B, using can_travel on T to decide which roads can be taken.
Finds all paths from A to B, its lenght, time, and which vehicles that is required to 
take the path.
 */
path(T,A,B,P,[B|P],Time,Length,Vehicles) :- 
    can_travel(T,A,B,Time,Length,Vehicles).
path(T,A,B,V,Path,Time,Length,Vehicles) :-
    can_travel(T,A,C,HTime,HLenght,HVehicles),
    not(C == B), % C is not B
    not(member(C,V)), % Not a city we havnt visited yet
    path(T,C,B,[C|V],Path,TTime,TLenght,TVhicles),
    Time is HTime + TTime,
    Length is HLenght + TLenght,
    append(HVehicles,TVhicles,Temp), % combine vehicles from can_travel, append gives combo, intersection gives direct
    sort(Temp, Vehicles). % Only needed with append.

/* minimal_path & minimal
finds the shortes path bases on a wheight.
 */
minimal_path([H|T], Minimal) :- minimal(T,H,Minimal).

minimal([[NPath,NMin]|T],[_,Min],Minimal) :- % Case of new minimal
    NMin < Min,
    minimal(T,[NPath,NMin],Minimal).
minimal([_|T], Min, Minimal) :- % Case of head not new minimal
    minimal(T,Min,Minimal).
minimal([],Minimal,Minimal).

/* find_path
Find the paths from A to B, optionally get time and lenght with
 */
find_path(A,B,Path) :- find_path(A,B,Path,_,_).
find_path(A,B,Path,Time,Length) :-
    path(none,A,B,[A],Q,Time,Length,_), 
    reverse(Q,Path).

/* find_traveler_path
Find paths that a traveler can take from A to B
 */
find_traveler_path(T,A,B,Path) :-
    path(T,A,B,[A],Q,_,_,_), 
    reverse(Q,Path).

/* find_shortest_path
Find the shortest path from A to B
 */
find_shortest_path(A,B,Path,Length) :-
    setof([P, L], find_path(A,B,P,_,L), Paths), % Find all paths
    member([_,_],Paths), % Make sure there is at least one path
    minimal_path(Paths, [Path, Length]),!. % Get the shortest path from the found paths

/* find_quickest_path
Find the fastest path from A to B
 */
find_quickest_path(A,B,Path,Time) :-
    setof([P, Time], find_path(A,B,P,Time,_), Paths),
    member([_,_],Paths),
    minimal_path(Paths, [Path, Time]),!.

/* find_path_with_stops
Find paths that a traveler can take from A to B, which goes though a list of stops
 */   
find_path_with_stops(T,A,B,Path,Stops) :-
    path(T,A,B,[A],Q,_,_,_),
    subset(Stops, Q),
    reverse(Q,Path).

/* find_path_using_types
Find paths from A to B, that only uses the provided road types
 */
find_path_using_types(A,B,Path,Types) :-
    path(Types,A,B,[A],Q,_,_,_),
    reverse(Q, Path).

/* find_group_path
find paths form A to B, that a group of travelers can take, using the vehicles they have
 */
find_group_path(travelers, A, B, Path) :-
    count_group(travelers, Count),
    available_vehicles(Count, AVehicles),
    get_vehicles_from_group(travelers, OVehicles),
    intersection(AVehicles, OVehicles, Vehicles),
    path(Vehicles,A,B,[A],Q,_,_,_),
    reverse(Q,Path).

/* find_transportation
Find the vehicles that are required to travel from A to B, 
or which of a travelers vehicles that is requires to take the paths
 */
find_transportation(A,B,Path,Vehicles) :-
    path(none,A,B,[A],Q,_,_,Vehicles), 
    reverse(Q,Path).
find_transportation(traveler(_,Options),A,B,Path,Vehicles) :-
    path(traveler(_,Options),A,B,[A],Q,_,_,V), 
    reverse(Q,Path),
    intersection(V,Options,Vehicles).