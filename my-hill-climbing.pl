/*
 * solve_hill_climb(State,History,Moves)
 *   Moves es la secuencia de movidas requeridas para
 *   alcanzar un estado final deseado a partir de State.
 *   History contiene los estados previamente visitados.
 */

% Si el Estado actual es un estado final, no hay que moverse.
solve_hill_climb(State,_,[]) :-
    final_state(State).

/*
 * Si el Estado actual no es un estado final, genera una movida
 * para desplazarse a un nuevo estado, y continua la b�squeda a
 * partir de ese nuevo estado.
 * Las movidas son intentadas en el orden establecido por la heur�stica
 * que eval�a la "bondad" de los estados que se alcanzan para cada movida.
 */
solve_hill_climb(State,History,[Move|Moves]) :-
    hill_climb(State,Move),      % generar una nueva Movida en el orden heur�stico
    update(State,Move,State1),   % calcula nuevo estado usando Movida
    legal(State1),               % nuevo estado debe ser legal
    not(member(State1,History)), % debe ser primera vez que se llega al nuevo estado
    solve_hill_climb(State1,[State1|History],Moves).   % continuar a partir de nuevo estado

/*
 *  A partir de un Estado devuelve una Movida.
 *  Primero genera todas las movidas, luego las eval�a usando una heur�stica,
 *  y finalmente las va usando en orden decreciente de la evaluaci�n obtenida.
 */
hill_climb(State,Move) :-
    findall(M,move(State,M),Moves),         % Encuentra todas las movidas posibles
    evaluate_and_order(Moves,State,[],MVs), % Eval�a con la heur�stica todas las movidas y las ordena.
    member((Move,_),MVs).                   % Escoge movidas en orden de heur�stica


/*
 * evaluate_and_order(Movidas,Estado,AcumuladorParcial,MovidasOrdenadas)
 *   Todas las Movidas del Estado actual
 *   son evaluadas y almacenadas en orden en MovidasOrdenadas
 *   Acumulador es donde se van acumulando parcialmente las movidas evaluadas.
 */

% Caso: procesar la primera movida y continuar recursivamente
evaluate_and_order([Move|Moves],State,MVs,OrderedMVs) :-
    update(State,Move,State1),         % obtiene nuevo estado usando movida
    value(State1,Value),               % calcula el valor heur�sico del nuevo estado
    insertPair((Move,Value),MVs,MVs1), % inserta en orden el par (movida,valor) en lista de movidas
    evaluate_and_order(Moves,State,MVs1,OrderedMVs).  % procesa recursivamente el resto de movidas
    
% Caso base: no hay m�s movidas que evaluar. Se retorna el acumulador como resultado.
evaluate_and_order([],_,MVs,MVs).

insertPair(MV,[],[MV]).
insertPair((M,V),[(M1,V1)|MVs],[(M,V),(M1,V1)|MVs]) :-
    V >= V1.
insertPair((M,V),[(M1,V1)|MVs],[(M1,V1)|MVs1]) :-
    V < V1,insertPair((M,V),MVs,MVs1).

/*
 * Inicializa un problema y lo resuelve.
 *   Problem: nombre del problema.
 *   Moves: movidas requeridas para resolver el problema.
 */
test_hill_climb(Problem,Moves) :-
   initial_state(Problem,State),           % obtener un Estado inicial dado Problema
   solve_hill_climb(State,[State],Moves).  % inicia resoluci�n desde Estado


/*  ==========================================================
    Relationships needed to solve the Bridge and Torch Problem
    ========================================================== */

/*    
    Defining the Initial State and Final State of Bridge and Torch (BT)
        1. Starting on the left side.
        2. List of people on one side.
        3. Empty side.
        4. Amount of people that can pass at a time.
        6. Max time that it takes to cross the bridge 
*/

% Initial State
initial_state(bt, bt(left, [a,b,c,d,e,j], [], 3, 30)).

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