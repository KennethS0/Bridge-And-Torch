/*
 * Initializes the problem and solves it
 *   Problem: name of the problem.
 *   Moves: moves needed to solve the problem.
 */
test_dfs(Problem, Moves) :-
      initial_state(Problem, State),        % Obtains the initial state from the problem.
      solve_dfs(State, [State], Moves).     % Solves the problem.


/*
    Solves the problem by checking if it has reached a final state,
    if not then it will solve the problem using the Depth First search.
*/
solve_dfs(State, _, []) :- final_state(State).

solve_dfs(State, History, [Move|Moves]) :-
      move(State, Move),                % Generates a new move
      update(State, Move, State2),      % Generates a new state based on that move
      legal(State2),                    % Checks if the new state is legal
      not(member(State2, History)),     % Checks if the state has already been visited
      solve_dfs(State2, [State2|History], Moves).


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