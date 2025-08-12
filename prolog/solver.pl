/* =========================================================
   8-PUZZLE SOLVER - Knowledge Base Prolog
   =========================================================
   
   Implementazione di algoritmi di ricerca per l'8-puzzle:
   - A* con diverse euristiche (Manhattan, Misplaced, Linear Conflict)
   - IDA* (Iterative Deepening A*)
   - BFS (Breadth-First Search)
   - Greedy Best-First Search
   
   Autore: [Il tuo nome]
   Corso: Ingegneria della Conoscenza
   ========================================================= */

:- module(solver, [
    solve_astar_manhattan/5,
    solve_astar_misplaced/5,
    solve_astar_linear/5,
    solve_idastar/5,
    solve_bfs/5,
    solve_greedy/5,
    generate_random_puzzle/2,
    is_solvable/1
]).

:- use_module('heuristics.pl').

% Predicati dinamici per memorizzazione stati
:- dynamic stato_visitato/1.
:- dynamic soluzione_memorizzata/1.
:- dynamic nodes_explored/1.
:- dynamic nodes_frontier/1.

% =========================================================
% STATO OBIETTIVO E UTILITÀ
% =========================================================

% Stato obiettivo 8-puzzle
goal_state([1,2,3,4,5,6,7,8,0]).

% Inizializza contatori
init_counters :-
    retractall(nodes_explored(_)),
    retractall(nodes_frontier(_)),
    assert(nodes_explored(0)),
    assert(nodes_frontier(0)).

% Incrementa nodi esplorati
increment_explored :-
    retract(nodes_explored(N)),
    N1 is N + 1,
    assert(nodes_explored(N1)).

% Aggiorna nodi in frontiera
update_frontier(Size) :-
    retract(nodes_frontier(_)),
    assert(nodes_frontier(Size)).

% =========================================================
% MOVIMENTI E TRANSIZIONI
% =========================================================

% Trova la posizione dello spazio vuoto (0)
find_blank(State, Pos) :-
    nth0(Pos, State, 0).

% Genera tutti i possibili successori di uno stato
successors(State, Successors) :-
    findall(NextState, move(State, NextState), Successors).

% Applica una mossa valida
move(State, NextState) :-
    find_blank(State, BlankPos),
    valid_move(BlankPos, NewBlankPos),
    swap(State, BlankPos, NewBlankPos, NextState).

% Mosse valide basate sulla posizione dello spazio vuoto
valid_move(Pos, NewPos) :-
    Row is Pos // 3,
    Col is Pos mod 3,
    (   % Movimento verso alto
        Row > 0,
        NewPos is Pos - 3
    ;   % Movimento verso il basso
        Row < 2,
        NewPos is Pos + 3
    ;   % Movimento verso sinistra
        Col > 0,
        NewPos is Pos - 1
    ;   % Movimento verso destra
        Col < 2,
        NewPos is Pos + 1
    ).

% Scambia due elementi in una lista
swap(List, I, J, Result) :-
    nth0(I, List, ElemI),
    nth0(J, List, ElemJ),
    replace_nth(List, I, ElemJ, Temp),
    replace_nth(Temp, J, ElemI, Result).

% Sostituisce n-esimo elemento di una lista
replace_nth(List, N, Elem, Result) :-
    nth0(N, List, _, Rest),
    nth0(N, Result, Elem, Rest).

% =========================================================
% A* CON DISTANZA DI MANHATTAN
% =========================================================

solve_astar_manhattan(Initial, Path, NodesExplored, NodesFrontier, Time) :-
    get_time(StartTime),
    init_counters,
    retractall(stato_visitato(_)),
    
    manhattan_distance(Initial, H),
    astar_search([node(Initial, [Initial], 0, H, H)], [], Path),
    
    nodes_explored(NodesExplored),
    nodes_frontier(NodesFrontier),
    get_time(EndTime),
    Time is EndTime - StartTime.

% Ricerca A* generica
astar_search([node(State, Path, _, _, _)|_], _, Path) :-
    goal_state(State),
    !.

astar_search([node(State, PathSoFar, G, _, _)|Rest], Visited, Solution) :-
    increment_explored,
    successors(State, Successors),
    expand_astar_nodes(Successors, PathSoFar, G, Visited, NewNodes),
    append(Rest, NewNodes, OpenList),
    sort_by_f(OpenList, SortedOpen),
    length(SortedOpen, FrontierSize),
    update_frontier(FrontierSize),
    astar_search(SortedOpen, [State|Visited], Solution).

% Espande i nodi per A*
expand_astar_nodes([], _, _, _, []).
expand_astar_nodes([State|States], PathSoFar, G, Visited, Nodes) :-
    (   member(State, Visited)
    ->  expand_astar_nodes(States, PathSoFar, G, Visited, Nodes)
    ;   member(State, PathSoFar)
    ->  expand_astar_nodes(States, PathSoFar, G, Visited, Nodes)
    ;   NewG is G + 1,
        manhattan_distance(State, H),
        F is NewG + H,
        append(PathSoFar, [State], NewPath),
        expand_astar_nodes(States, PathSoFar, G, Visited, RestNodes),
        Nodes = [node(State, NewPath, NewG, H, F)|RestNodes]
    ).

% Ordina nodi per f-score
sort_by_f(Nodes, Sorted) :-
    predsort(compare_f, Nodes, Sorted).

compare_f(<, node(_, _, _, _, F1), node(_, _, _, _, F2)) :- F1 < F2, !.
compare_f(>, _, _).

% =========================================================
% A* CON TESSERE FUORI POSTO
% =========================================================

solve_astar_misplaced(Initial, Path, NodesExplored, NodesFrontier, Time) :-
    get_time(StartTime),
    init_counters,
    retractall(stato_visitato(_)),
    
    misplaced_tiles(Initial, H),
    astar_search_misplaced([node(Initial, [Initial], 0, H, H)], [], Path),
    
    nodes_explored(NodesExplored),
    nodes_frontier(NodesFrontier),
    get_time(EndTime),
    Time is EndTime - StartTime.

astar_search_misplaced([node(State, Path, _, _, _)|_], _, Path) :-
    goal_state(State),
    !.

astar_search_misplaced([node(State, PathSoFar, G, _, _)|Rest], Visited, Solution) :-
    increment_explored,
    successors(State, Successors),
    expand_astar_nodes_misplaced(Successors, PathSoFar, G, Visited, NewNodes),
    append(Rest, NewNodes, OpenList),
    sort_by_f(OpenList, SortedOpen),
    length(SortedOpen, FrontierSize),
    update_frontier(FrontierSize),
    astar_search_misplaced(SortedOpen, [State|Visited], Solution).

expand_astar_nodes_misplaced([], _, _, _, []).
expand_astar_nodes_misplaced([State|States], PathSoFar, G, Visited, Nodes) :-
    (   member(State, Visited)
    ->  expand_astar_nodes_misplaced(States, PathSoFar, G, Visited, Nodes)
    ;   member(State, PathSoFar)
    ->  expand_astar_nodes_misplaced(States, PathSoFar, G, Visited, Nodes)
    ;   NewG is G + 1,
        misplaced_tiles(State, H),
        F is NewG + H,
        append(PathSoFar, [State], NewPath),
        expand_astar_nodes_misplaced(States, PathSoFar, G, Visited, RestNodes),
        Nodes = [node(State, NewPath, NewG, H, F)|RestNodes]
    ).

% =========================================================
% A* CON LINEAR CONFLICT
% =========================================================

solve_astar_linear(Initial, Path, NodesExplored, NodesFrontier, Time) :-
    get_time(StartTime),
    init_counters,
    retractall(stato_visitato(_)),
    
    linear_conflict(Initial, H),
    astar_search_linear([node(Initial, [Initial], 0, H, H)], [], Path),
    
    nodes_explored(NodesExplored),
    nodes_frontier(NodesFrontier),
    get_time(EndTime),
    Time is EndTime - StartTime.

astar_search_linear([node(State, Path, _, _, _)|_], _, Path) :-
    goal_state(State),
    !.

astar_search_linear([node(State, PathSoFar, G, _, _)|Rest], Visited, Solution) :-
    increment_explored,
    successors(State, Successors),
    expand_astar_nodes_linear(Successors, PathSoFar, G, Visited, NewNodes),
    append(Rest, NewNodes, OpenList),
    sort_by_f(OpenList, SortedOpen),
    length(SortedOpen, FrontierSize),
    update_frontier(FrontierSize),
    astar_search_linear(SortedOpen, [State|Visited], Solution).

expand_astar_nodes_linear([], _, _, _, []).
expand_astar_nodes_linear([State|States], PathSoFar, G, Visited, Nodes) :-
    (   member(State, Visited)
    ->  expand_astar_nodes_linear(States, PathSoFar, G, Visited, Nodes)
    ;   member(State, PathSoFar)
    ->  expand_astar_nodes_linear(States, PathSoFar, G, Visited, Nodes)
    ;   NewG is G + 1,
        linear_conflict(State, H),
        F is NewG + H,
        append(PathSoFar, [State], NewPath),
        expand_astar_nodes_linear(States, PathSoFar, G, Visited, RestNodes),
        Nodes = [node(State, NewPath, NewG, H, F)|RestNodes]
    ).

% =========================================================
% IDA* (ITERATIVE DEEPENING A*)
% =========================================================

solve_idastar(Initial, Path, NodesExplored, NodesFrontier, Time) :-
    get_time(StartTime),
    init_counters,
    
    manhattan_distance(Initial, H),
    idastar_search(Initial, H, Path),
    
    nodes_explored(NodesExplored),
    nodes_frontier(NodesFrontier),
    get_time(EndTime),
    Time is EndTime - StartTime.

idastar_search(Initial, Bound, Path) :-
    idastar_iteration(Initial, [Initial], 0, Bound, Path, NextBound),
    (   Path \= []
    ->  true
    ;   NextBound = inf
    ->  Path = []
    ;   idastar_search(Initial, NextBound, Path)
    ).

idastar_iteration(State, PathSoFar, G, Bound, Path, NextBound) :-
    goal_state(State),
    !,
    Path = PathSoFar,
    NextBound = 0.

idastar_iteration(State, PathSoFar, G, Bound, Path, NextBound) :-
    manhattan_distance(State, H),
    F is G + H,
    (   F > Bound
    ->  Path = [],
        NextBound = F
    ;   increment_explored,
        successors(State, Successors),
        explore_successors_ida(Successors, PathSoFar, G, Bound, Path, NextBound)
    ).

explore_successors_ida([], _, _, _, [], inf).
explore_successors_ida([State|States], PathSoFar, G, Bound, Path, NextBound) :-
    (   member(State, PathSoFar)
    ->  explore_successors_ida(States, PathSoFar, G, Bound, Path, NextBound)
    ;   NewG is G + 1,
        append(PathSoFar, [State], NewPath),
        idastar_iteration(State, NewPath, NewG, Bound, PathTemp, NBTemp),
        (   PathTemp \= []
        ->  Path = PathTemp,
            NextBound = NBTemp
        ;   explore_successors_ida(States, PathSoFar, G, Bound, PathRest, NBRest),
            (   PathRest \= []
            ->  Path = PathRest,
                NextBound = NBRest
            ;   Path = [],
                NextBound is min(NBTemp, NBRest)
            )
        )
    ).

% =========================================================
% BFS (BREADTH-FIRST SEARCH)
% =========================================================

solve_bfs(Initial, Path, NodesExplored, NodesFrontier, Time) :-
    get_time(StartTime),
    init_counters,
    retractall(stato_visitato(_)),
    
    bfs_search([[Initial]], Path),
    
    nodes_explored(NodesExplored),
    nodes_frontier(NodesFrontier),
    get_time(EndTime),
    Time is EndTime - StartTime.

bfs_search([Path|_], Path) :-
    Path = [State|_],
    goal_state(State),
    !.

bfs_search([CurrentPath|RestPaths], Solution) :-
    CurrentPath = [State|_],
    increment_explored,
    successors(State, Successors),
    expand_bfs_paths(Successors, CurrentPath, NewPaths),
    append(RestPaths, NewPaths, AllPaths),
    length(AllPaths, FrontierSize),
    update_frontier(FrontierSize),
    bfs_search(AllPaths, Solution).

expand_bfs_paths([], _, []).
expand_bfs_paths([State|States], PathSoFar, Paths) :-
    (   member(State, PathSoFar)
    ->  expand_bfs_paths(States, PathSoFar, Paths)
    ;   stato_visitato(State)
    ->  expand_bfs_paths(States, PathSoFar, Paths)
    ;   assert(stato_visitato(State)),
        Paths = [[State|PathSoFar]|RestPaths],
        expand_bfs_paths(States, PathSoFar, RestPaths)
    ).

% =========================================================
% GREEDY BEST-FIRST SEARCH
% =========================================================

solve_greedy(Initial, Path, NodesExplored, NodesFrontier, Time) :-
    get_time(StartTime),
    init_counters,
    retractall(stato_visitato(_)),
    
    manhattan_distance(Initial, H),
    greedy_search([node(Initial, [Initial], H)], [], Path),
    
    nodes_explored(NodesExplored),
    nodes_frontier(NodesFrontier),
    get_time(EndTime),
    Time is EndTime - StartTime.

greedy_search([node(State, Path, _)|_], _, Path) :-
    goal_state(State),
    !.

greedy_search([node(State, PathSoFar, _)|Rest], Visited, Solution) :-
    increment_explored,
    successors(State, Successors),
    expand_greedy_nodes(Successors, PathSoFar, Visited, NewNodes),
    append(Rest, NewNodes, OpenList),
    sort_by_h_greedy(OpenList, SortedOpen),
    length(SortedOpen, FrontierSize),
    update_frontier(FrontierSize),
    greedy_search(SortedOpen, [State|Visited], Solution).

expand_greedy_nodes([], _, _, []).
expand_greedy_nodes([State|States], PathSoFar, Visited, Nodes) :-
    (   member(State, Visited)
    ->  expand_greedy_nodes(States, PathSoFar, Visited, Nodes)
    ;   member(State, PathSoFar)
    ->  expand_greedy_nodes(States, PathSoFar, Visited, Nodes)
    ;   manhattan_distance(State, H),
        append(PathSoFar, [State], NewPath),
        expand_greedy_nodes(States, PathSoFar, Visited, RestNodes),
        Nodes = [node(State, NewPath, H)|RestNodes]
    ).

sort_by_h_greedy(Nodes, Sorted) :-
    predsort(compare_h_greedy, Nodes, Sorted).

compare_h_greedy(<, node(_, _, H1), node(_, _, H2)) :- H1 < H2, !.
compare_h_greedy(>, _, _).

% =========================================================
% UTILITÀ PER GENERAZIONE E VALIDAZIONE
% =========================================================

% Genera un puzzle casuale risolvibile
generate_random_puzzle(Difficulty, Puzzle) :-
    goal_state(Goal),
    random_moves(Goal, Difficulty, Puzzle).

random_moves(State, 0, State) :- !.
random_moves(State, N, Final) :-
    N > 0,
    successors(State, Successors),
    random_member(NextState, Successors),
    N1 is N - 1,
    random_moves(NextState, N1, Final).

% Verifica se un puzzle è risolvibile
is_solvable(State) :-
    count_inversions(State, Count),
    Count mod 2 =:= 0.

count_inversions([], 0).
count_inversions([H|T], Count) :-
    (   H = 0
    ->  count_inversions(T, Count)
    ;   count_inversions_for_element(H, T, ElementCount),
        count_inversions(T, RestCount),
        Count is ElementCount + RestCount
    ).

count_inversions_for_element(_, [], 0).
count_inversions_for_element(X, [H|T], Count) :-
    (   H = 0
    ->  count_inversions_for_element(X, T, Count)
    ;   H < X
    ->  count_inversions_for_element(X, T, RestCount),
        Count is RestCount + 1
    ;   count_inversions_for_element(X, T, Count)
    ).

% =========================================================
% PREDICATI HELPER
% =========================================================

% Trova elemento casuale in una lista
random_member(X, List) :-
    length(List, Len),
    Len > 0,
    random(0, Len, Index),
    nth0(Index, List, X).