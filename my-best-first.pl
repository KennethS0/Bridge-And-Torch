/*
 * Initializes the problem and solves it
 *   Problem: name of the problem.
 *   Moves: moves needed to solve the problem.
 */
test_best_search(Problem,Moves) :-
   initial_state(Problem,State),                        % Obtains the initial state from the problem
   value(State,Value),                                  % Assigns a heuristic to the state.
   solve_best([punto(State,[],Value)],[State],Moves).   

/*
    Solves the problem by checking if it has reached a final state,
    if not then it will keep searching for a solution.
*/

solve_best([punto(State,Path,_)|_],_,Moves) :-
    final_state(State),reverse(Path,Moves).


solve_best([punto(State,Path,_)|Frontier],History,FinalPath) :-
    findall(M,move(State,M),Moves),     % Obtains the moves of the best state
    updates(Moves,Path,State,States),   % Obtains the new states using the moves
    legals(States,States1),             % Choses the new states that are legal
    news(States1,History,States2),      % Removes the new states that are in the history
    evaluates(States2,Values),          % Assigns a heuristic to all the new states
    inserts(Values,Frontier,Frontier1), % Inserts in order the new points of the frontier
    solve_best(Frontier1,[State|History],FinalPath).

/*
 * updates(Moves,Path,State,States)
 *   States es la lista de posibles estados accesables a partir
 *   de State usando la lista de posibles movidas (Moves).
 *   Path es la ruta de movidas que llevan del estado inicial a State.
 *   Las rutas de los nuevos estados se agregan al inicio su respectiva movida
 *   a la ruta de State.
 *   States es una lista de pares (NuevoEstado, NuevaRuta).
 */

updates([M|Ms],Path,S,[(S1,[M|Path])|Ss]) :-
    update(S,M,S1),         % obtiene el estado al que se llega por una movida
    updates(Ms,Path,S,Ss).  % procesa recursivamente las siguientes movidas
updates([],_,_,[]).


/*
 * legasls(States,States1)
 *   States1 es el subconjunto de la lista State que son estados legales.
 *   Maneja pares (Estado,Ruta).
 */

% el primer estado es legal, incluirlo en la nueva lista
legals([(S,P)|States],[(S,P)|States1]) :-
    legal(S),
    legals(States,States1).
    
% primer estado ilegal, excluirlo de la nueva lista
legals([(S,_)|States],States1) :-
    not(legal(S)),
    legals(States,States1).
    
legals([],[]).


/*
 * news(States,History,States1)
 *   States1 es el subconjunto de la lista States que consiste de estados
 *   que no aparecen en el historial.
 *   Maneja pares (Estado,Ruta).
 */

% primer estado ya aparece en historial, excluirlo de la nueva lista
news([(S,_)|States],History,States1) :-
    member(S,History),
    news(States,History,States1).

% primer estado no aparece en historial, incluirlo en nueva lista
news([(S,P)|States],History,[(S,P)|States1]) :-
    not(member(S,History)),
    news(States,History,States1).
    
news([],_,[]).


/*
 * evaluates(States,Values)
 *   Calcula el valor heur�stico de los estados en la lista States.
 *   Values is la lista resultante con los estados junto con sus valores.
 *   La lista State consiste de pares (Estado,Ruta).
 *   La lista Values consiste de estructuras punto(Estado,Ruta,Valor).
 */

evaluates([(S,P)|States],[punto(S,P,V)|Values]) :-
    value(S,V),                % calcula valor heur�stico del estado S
    evaluates(States,Values).  % procesa resto de estados
evaluates([],[]).


/*
 * inserts(Points,Frontier,Frontier1)
 *   Frontier1 es el resultado de insertar una lista de puntos (Points)
 *   en una frontera anterior (Frontier).
 *   Los puntos son insertados preservando el orden descendente en el
 *   valor heur�stico.
 */

inserts([Punto|Puntos],Frontier,Frontier1) :-
    insertPoint(Punto,Frontier,Frontier0),  % inserta primer punto
    inserts(Puntos,Frontier0,Frontier1).    % recursivamente inserta los dem�s puntos
inserts([],Frontier,Frontier).


/*
 * insertPoint(Point,Frontier,Frontier1)
 *   Frontier1 es el resultado de insertar el punto Points en
 *   su posici�n correcta dentro de Frontier de acuerdo con el orden
 *   del valor heur�stico.
 *
 */
insertPoint(Point,[],[Point]).

% nuevo punto es mejor que el primero de la frontera,
% va de primero en nueva frontera
insertPoint(Point,[Point1|Points],[Point1,Point|Points]) :-
    less_than(Point1,Point).

% nuevo punto es igual al primero de la frontera,
% nuevo punto se ignora y se deja la frontera sin cambios
insertPoint(Point,[Point1|Points],[Point|Points]) :-
    equals(Point,Point1).

% nuevo punto es peor que el primero de la frontera,
% el primero de la frontera de deja en el primer lugar y
% el nuevo punto se inserta recursivamente dentro del resto de la frontera
insertPoint(Point,[Point1|Points],[Point1|Points1]) :-
    less_than(Point,Point1),
    insertPoint(Point,Points,Points1).

% nuevo punto no es igual a primero de la frontera pero tiene
% el mismo valor heur�stico, se pone al nuevo punto como primero
% en la nueva frontera
insertPoint(Point,[Point1|Points],[Point,Point1|Points]) :-
    same(Point,Point1).


/*
 * relaciones de comparaci�n de puntos
 *
 * no se puede dar el caso de que dos puntos tengan el mismo estado
 * pero diferente valor
 */

% dos puntos son iguales si contienen el mismo estado y tienen el mismo valor;
% se ignoran las rutas: no importa c�mo se haya llegado al mismo estado
equals(punto(S,_,V),punto(S,_,V)).

% un punto es menor que otro, si contienen diferentes estados y si el
% valor del primero es menor que el valor del segundo;
% las rutas son ignoradas
less_than(punto(S1,_,V1),punto(S2,_,V2)) :- S1 \= S2, V1 < V2.

% dos puntos tienen el mismo valor si contienen diferentes estados
% y si sus valores son iguales
same(punto(S1,_,V1),punto(S2,_,V2)) :- S1 \= S2, V1 = V2.





/*  ==========================================================
    Relationships needed to solve the Bridge and Torch Problem
    ========================================================== */

/*    
    Defining the Initial State and Final State of Bridge and Torch (BT)
        1. Starting on the left side.
        2. List of people on one side.
        3. Empty side.
        4. Amount of people that can pass at a time.
        5. Max time that it takes to cross the bridge 
*/

% Initial State
initial_state(bt, bt(left, [a,b,c,d,e], [], 4, 28)).

% Final State
final_state(bt(right, [], _, _, _)).


/*
    Defining how every single move will be obtained.
        Input (1):
            1. Starting Point
            2. Left list
            3. _
            4. Amount of people that need to cross
            5. _
        Output (2):
            1. Starting Point
            2. Possible Combination
            3. Time it takes for them to cross
*/
move(bt(left, Xs, _, N, _), (left, Xs, Time)) :-
    length(Xs, L),
    L < N,
    max_crossing_time(Xs, Time).

move(bt(left, Xs, _, N, _), (left, Comb, Time)) :-
    combinations(Xs, N, Comb),
    max_crossing_time(Comb, Time).

move(bt(right, _, Ys, _, _), (right, Selected, Time)) :-
    get_one(Ys, Selected, Time).


/*
    Defining a relation that obtains all the ways to get 'N' members from the list
    1: Input list
    2: Size of output
    3: Output list
*/

% Sets Comb to be []
combinations(_, 0, []).

% Appends the removed head to the combination
combinations([X | Xs], N, [X | Ys]) :- 
    N > 0,
    N1 is N-1,
    combinations(Xs, N1, Ys).

% Takes the head out of the initial list.
combinations([_|Xs], N, Ys) :-
    N > 0,
    combinations(Xs, N, Ys).


/*  
    Update:
        1: Previous state
        2: Movement
        3: Updated state
*/
update(bt(left, Ls, Rs, Pc, Mt), (left, Ms, CT), bt(right, Nl, Nr, Pc, Ntc)) :-
    remove_from(Ls, Ms, Nl),   % Removes the items of one list from another list
    append(Rs, Ms, Nr1),       % Adds the removed items 
    sort(Nr1, Nr),             % Sorts the result
    Ntc is Mt - CT.            % Adds the crossing time to the total time

update(bt(right, [L | Ls], Rs, Pc, Mt), (right, Ms, CT), bt(left, Nl, Nr, Pc, Ntc)) :-
    remove_from(Rs, Ms, Nr),   % Removes the items of one list from another list
    append([L | Ls], Ms, Nl1),       % Adds the removed items
    sort(Nl1, Nl),             % Sorts the result
    Ntc is Mt - CT.            % Adds the crossing time to the total time


% Removes elements from one list in another list.
remove_from([], _, []).

remove_from([X|Tail], L2, Result):- 
    member(X, L2), !, 
    remove_from(Tail, L2, Result). 

remove_from([X|Tail], L2, [X|Result]):- 
    remove_from(Tail, L2, Result).


/*
    Legal: Checks if the move can be done
*/
legal(bt(_, _, _, _, Mt)) :-
    0 =< Mt.

/*
    Time relationships

    time : Defines how long it takes for one person to pass.

    max_crossing_time : Obtains the MAX time of the combination of people crossing.
    min_crossing_time : Obtains the person with the lowest time
*/
time(a, 1).
time(b, 2).
time(c, 5).
time(d, 10).
time(e, 15).
time(j, 20).

% No one is crossing
max_crossing_time([], 0).

% Obtains the time of only one person crossing
max_crossing_time([Person], Time) :- time(Person, Time). 

% Obtains the max time if two people are crossing
max_crossing_time([P1, P2], T0) :-
    time(P1, T0),
    time(P2, T1),
    T1 =< T0.

max_crossing_time([P1, P2], T1) :-
    time(P1, T0),
    time(P2, T1),
    T0 =< T1.

% Obtains the max time if more three or more people are crossing
max_crossing_time([P1, P2, P3 | Ps], T1) :- 
    time(P1, T1),
    time(P2, T2),
    time(P3, T3),
    max_crossing_time(Ps, T4),
    T1 >= T2, T1 >= T3, T1 >= T4.

max_crossing_time([P1, P2, P3 | Ps], T2) :- 
    time(P1, T1),
    time(P2, T2),
    time(P3, T3),
    max_crossing_time(Ps, T4),
    T2 >= T1, T2 >= T3, T2 >= T4.

max_crossing_time([P1, P2, P3 | Ps], T3) :- 
    time(P1, T1),
    time(P2, T2),
    time(P3, T3),
    max_crossing_time(Ps, T4),
    T3 >= T2, T3 >= T1, T3 >= T4.

max_crossing_time([P1, P2, P3 | Ps], T4) :- 
    time(P1, T1),
    time(P2, T2),
    time(P3, T3),
    max_crossing_time(Ps, T4),
    T4 >= T2, T4 >= T3, T4 >= T1.

% Obtains one person with their corresponding time
get_one(Xs, [X], T) :-
    member(X, Xs),
    time(X, T).

% Sums the time of all the people
timeSum([X|Xs], Sum) :- 
    time(X, Tiempo),
    timeSum(Xs, N), 
    Sum is Tiempo + N.

timeSum([X], Tiempo):-
    time(X, Tiempo).


/* ============
    Heuristics
   ============ */
value(bt(left, Ls, _, _, _), Val) :-
    timeSum(Ls, X),
    Val is -X.

value(bt(right, _, Rs, _, _), Val) :-
    timeSum(Rs, X),
    Val is X.