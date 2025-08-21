/** <module> 8-Puzzle Solver - Knowledge Base Prolog
 *
 * Implementazione di algoritmi di ricerca per l'8-puzzle.
 * Questo modulo fornisce tre algoritmi principali per la risoluzione:
 * - A* con euristica combinata
 * - BFS (Breadth-First Search)
 * - Greedy Best-First Search
 *
 * @author Marco Ferrara
 * @version 1.0
 * @license MIT
 */

:- module(solver, [
    solve_astar/5,
    solve_bfs/5,
    solve_greedy/5,
    generate_random_puzzle/2,
    is_solvable/1,
    cleanup_prolog/0,
    optimal_cost/1,
    goal_state/1

]).

:- use_module('heuristics.pl').

%! stato_visitato(?State:list) is nondet
% Predicato dinamico per memorizzare gli stati già visitati durante la ricerca.
% @arg State Lista di 9 elementi rappresentante uno stato del puzzle
:- dynamic stato_visitato/1.

%! nodes_explored(?Count:int) is det
% Contatore dei nodi esplorati durante la ricerca.
% @arg Count Numero di nodi esplorati
:- dynamic nodes_explored/1.

%! nodes_frontier(?Count:int) is det
% Contatore dei nodi in frontiera durante la ricerca.
% @arg Count Numero di nodi in frontiera
:- dynamic nodes_frontier/1.

%! optimal_cost(?Cost:number) is det
% Costo ottimale trovato finora.
% @arg Cost Costo della soluzione ottimale (inf se non trovata)
:- dynamic optimal_cost/1.

% =========================================================
% CLEANUP E GESTIONE MEMORIA
% =========================================================

%! cleanup_prolog is det
% Pulisce tutti i predicati dinamici per liberare memoria.
% Deve essere chiamato alla fine di ogni sessione di risoluzione.
cleanup_prolog :-
    retractall(stato_visitato(_)),
    retractall(nodes_explored(_)),
    retractall(nodes_frontier(_)),
    retractall(optimal_cost(_)),
    !.

% =========================================================
% STATO OBIETTIVO E UTILITÀ
% =========================================================

%! goal_state(?State:list) is det
% Definisce lo stato obiettivo del 8-puzzle.
% @arg State Lista [1,2,3,4,5,6,7,8,0] rappresentante lo stato goal
goal_state([1,2,3,4,5,6,7,8,0]).

%! init_counters is det
% Inizializza i contatori per le statistiche di ricerca.
% Resetta nodes_explored, nodes_frontier e optimal_cost.
init_counters :-
    retractall(nodes_explored(_)),
    retractall(nodes_frontier(_)),
    retractall(optimal_cost(_)),
    assert(nodes_explored(0)),
    assert(nodes_frontier(0)),
    assert(optimal_cost(inf)).

%! increment_explored is det
% Incrementa il contatore dei nodi esplorati di 1.
increment_explored :-
    retract(nodes_explored(N)),
    N1 is N + 1,
    assert(nodes_explored(N1)).

%! update_frontier(+Size:int) is det
% Aggiorna il numero di nodi in frontiera.
% @arg Size Nuovo numero di nodi in frontiera
update_frontier(Size) :-
    retract(nodes_frontier(_)),
    assert(nodes_frontier(Size)).

%! update_optimal_cost(+Cost:number) is det
% Aggiorna il costo ottimale se il nuovo costo è migliore.
% @arg Cost Nuovo costo da confrontare con quello esistente
update_optimal_cost(Cost) :-
    ( retract(optimal_cost(OldCost))
    -> NewCost is min(Cost, OldCost)
    ; NewCost = Cost
    ),
    assert(optimal_cost(NewCost)).

% =========================================================
% MOVIMENTI E TRANSIZIONI
% =========================================================

%! find_blank(+State:list, -Pos:int) is det
% Trova la posizione dello spazio vuoto (0) nello stato.
% @arg State Stato corrente del puzzle
% @arg Pos Posizione (0-8) dello spazio vuoto
find_blank(State, Pos) :-
    nth0(Pos, State, 0).

%! successors(+State:list, -Successors:list) is det
% Genera tutti i possibili stati successori di uno stato dato.
% @arg State Stato corrente
% @arg Successors Lista di stati successori validi
successors(State, Successors) :-
    findall(NextState, move(State, NextState), Successors).

%! move(+State:list, -NextState:list) is nondet
% Applica una mossa valida allo stato corrente.
% @arg State Stato corrente
% @arg NextState Stato risultante dopo la mossa
move(State, NextState) :-
    find_blank(State, BlankPos),
    valid_move(BlankPos, NewBlankPos),
    swap(State, BlankPos, NewBlankPos, NextState).

%! valid_move(+Pos:int, -NewPos:int) is nondet
% Determina le mosse valide basate sulla posizione dello spazio vuoto.
% @arg Pos Posizione corrente dello spazio vuoto
% @arg NewPos Nuova posizione dopo una mossa valida
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

%! swap(+List:list, +I:int, +J:int, -Result:list) is det
% Scambia due elementi in una lista.
% @arg List Lista originale
% @arg I Indice del primo elemento
% @arg J Indice del secondo elemento
% @arg Result Lista con elementi scambiati
swap(List, I, J, Result) :-
    nth0(I, List, ElemI),
    nth0(J, List, ElemJ),
    replace_nth(List, I, ElemJ, Temp),
    replace_nth(Temp, J, ElemI, Result).

%! replace_nth(+List:list, +N:int, +Elem:any, -Result:list) is det
% Sostituisce n-esimo elemento di una lista.
% @arg List Lista originale
% @arg N Indice elemento da sostituire (0-based)
% @arg Elem Nuovo elemento
% @arg Result Lista con elemento sostituito
replace_nth(List, N, Elem, Result) :-
    nth0(N, List, _, Rest),
    nth0(N, Result, Elem, Rest).

% =========================================================
% A* CON EURISTICA COMBINATA
% =========================================================

%! solve_astar(+Initial:list, -Path:list, -NodesExplored:int, -NodesFrontier:int, -Time:float) is det
% Risolve 8-puzzle usando algoritmo A* con euristica combinata.
%
% @arg Initial Stato iniziale del puzzle
% @arg Path Percorso soluzione (lista di stati)
% @arg NodesExplored Numero di nodi esplorati
% @arg NodesFrontier Numero massimo di nodi in frontiera
% @arg Time Tempo di esecuzione in secondi
solve_astar(Initial, Path, NodesExplored, NodesFrontier, Time) :-
    get_time(StartTime),
    init_counters,
    retractall(stato_visitato(_)),
    
    combined_heuristic(Initial, H),
    astar_search([node(Initial, [Initial], 0, H, H)], [], Path, FinalCost),
    update_optimal_cost(FinalCost),
    
    nodes_explored(NodesExplored),
    nodes_frontier(NodesFrontier),
    get_time(EndTime),
    Time is EndTime - StartTime.

%! astar_search(+OpenList:list, +Visited:list, -Path:list, -Cost:int) is det
% Implementazione ricorsiva algoritmo A*.
% @arg OpenList Lista dei nodi da esplorare ordinata per f-score
% @arg Visited Lista degli stati già visitati
% @arg Path Percorso soluzione trovato
% @arg Cost Costo totale del percorso
astar_search([node(State, Path, Cost, _, _)|_], _, Path, Cost) :-
    goal_state(State),
    !.

astar_search([node(State, PathSoFar, G, _, _)|Rest], Visited, Solution, FinalCost) :-
    increment_explored,
    successors(State, Successors),
    expand_astar_nodes(Successors, PathSoFar, G, Visited, NewNodes),
    append(Rest, NewNodes, OpenList),
    sort_by_f(OpenList, SortedOpen),
    length(SortedOpen, FrontierSize),
    update_frontier(FrontierSize),
    astar_search(SortedOpen, [State|Visited], Solution, FinalCost).

%! expand_astar_nodes(+States:list, +PathSoFar:list, +G:int, +Visited:list, -Nodes:list) is det
% Espande i nodi successori per algoritmo A*.
% @arg States Lista di stati successori
% @arg PathSoFar Percorso corrente
% @arg G Costo g corrente
% @arg Visited Stati già visitati
% @arg Nodes Lista di nodi espansi
expand_astar_nodes([], _, _, _, []).
expand_astar_nodes([State|States], PathSoFar, G, Visited, Nodes) :-
    (   member(State, Visited)
    ->  expand_astar_nodes(States, PathSoFar, G, Visited, Nodes)
    ;   member(State, PathSoFar)
    ->  expand_astar_nodes(States, PathSoFar, G, Visited, Nodes)
    ;   NewG is G + 1,
        combined_heuristic(State, H),
        F is NewG + H,
        append(PathSoFar, [State], NewPath),
        expand_astar_nodes(States, PathSoFar, G, Visited, RestNodes),
        Nodes = [node(State, NewPath, NewG, H, F)|RestNodes]
    ).

%! sort_by_f(+Nodes:list, -Sorted:list) is det
% Ordina i nodi per f-score crescente.
% @arg Nodes Lista di nodi da ordinare
% @arg Sorted Lista ordinata per f-score
sort_by_f(Nodes, Sorted) :-
    predsort(compare_f, Nodes, Sorted).

%! compare_f(-Delta, +Node1:term, +Node2:term) is det
% Predicato di confronto per ordinamento per f-score.
% @arg Delta Risultato del confronto (<, >, =)
% @arg Node1 Primo nodo
% @arg Node2 Secondo nodo
compare_f(<, node(_, _, _, _, F1), node(_, _, _, _, F2)) :- F1 < F2, !.
compare_f(>, _, _).

% =========================================================
% BFS (BREADTH-FIRST SEARCH)
% =========================================================

%! solve_bfs(+Initial:list, -Path:list, -NodesExplored:int, -NodesFrontier:int, -Time:float) is det
% Risolve 8-puzzle usando Breadth-First Search.
%
% @arg Initial Stato iniziale del puzzle
% @arg Path Percorso soluzione (lista di stati)
% @arg NodesExplored Numero di nodi esplorati
% @arg NodesFrontier Numero massimo di nodi in frontiera
% @arg Time Tempo di esecuzione in secondi
solve_bfs(Initial, Path, NodesExplored, NodesFrontier, Time) :-
    get_time(StartTime),
    init_counters,
    retractall(stato_visitato(_)),
    
    bfs_search([[Initial]], ReversedPath, FinalCost),
    reverse(ReversedPath, Path),
    update_optimal_cost(FinalCost),
    
    nodes_explored(NodesExplored),
    nodes_frontier(NodesFrontier),
    get_time(EndTime),
    Time is EndTime - StartTime.

%! bfs_search(+Queue:list, -Path:list, -Cost:int) is det
% Implementazione ricorsiva di BFS.
% @arg Queue Coda di percorsi da esplorare
% @arg Path Percorso soluzione trovato
% @arg Cost Costo del percorso
bfs_search([Path|_], Path, Cost) :-
    Path = [State|_],
    goal_state(State),
    length(Path, Len),
    Cost is Len - 1,
    !.

bfs_search([CurrentPath|RestPaths], Solution, FinalCost) :-
    CurrentPath = [State|_],
    increment_explored,
    successors(State, Successors),
    expand_bfs_paths(Successors, CurrentPath, NewPaths),
    append(RestPaths, NewPaths, AllPaths),
    length(AllPaths, FrontierSize),
    update_frontier(FrontierSize),
    bfs_search(AllPaths, Solution, FinalCost).

%! expand_bfs_paths(+States:list, +PathSoFar:list, -Paths:list) is det
% Espande i percorsi per BFS.
% @arg States Stati successori
% @arg PathSoFar Percorso corrente
% @arg Paths Nuovi percorsi generati
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

%! solve_greedy(+Initial:list, -Path:list, -NodesExplored:int, -NodesFrontier:int, -Time:float) is det
% Risolve 8-puzzle usando Greedy Best-First Search.
%
% @arg Initial Stato iniziale del puzzle
% @arg Path Percorso soluzione (lista di stati)
% @arg NodesExplored Numero di nodi esplorati
% @arg NodesFrontier Numero massimo di nodi in frontiera
% @arg Time Tempo di esecuzione in secondi
solve_greedy(Initial, Path, NodesExplored, NodesFrontier, Time) :-
    get_time(StartTime),
    init_counters,
    retractall(stato_visitato(_)),
    
    combined_heuristic(Initial, H),
    greedy_search([node(Initial, [Initial], H)], [], Path, FinalCost),
    update_optimal_cost(FinalCost),
    
    nodes_explored(NodesExplored),
    nodes_frontier(NodesFrontier),
    get_time(EndTime),
    Time is EndTime - StartTime.

%! greedy_search(+OpenList:list, +Visited:list, -Path:list, -Cost:int) is det
% Implementazione ricorsiva di Greedy Best-First Search.
% @arg OpenList Lista ordinata per euristica
% @arg Visited Stati visitati
% @arg Path Percorso soluzione
% @arg Cost Costo del percorso
greedy_search([node(State, Path, _)|_], _, Path, Cost) :-
    goal_state(State),
    length(Path, Len),
    Cost is Len - 1,
    !.

greedy_search([node(State, PathSoFar, _)|Rest], Visited, Solution, FinalCost) :-
    increment_explored,
    successors(State, Successors),
    expand_greedy_nodes(Successors, PathSoFar, Visited, NewNodes),
    append(Rest, NewNodes, OpenList),
    sort_by_h_greedy(OpenList, SortedOpen),
    length(SortedOpen, FrontierSize),
    update_frontier(FrontierSize),
    greedy_search(SortedOpen, [State|Visited], Solution, FinalCost).

%! expand_greedy_nodes(+States:list, +PathSoFar:list, +Visited:list, -Nodes:list) is det
% Espande i nodi per Greedy Search.
% @arg States Stati successori
% @arg PathSoFar Percorso corrente
% @arg Visited Stati visitati
% @arg Nodes Nodi espansi
expand_greedy_nodes([], _, _, []).
expand_greedy_nodes([State|States], PathSoFar, Visited, Nodes) :-
    (   member(State, Visited)
    ->  expand_greedy_nodes(States, PathSoFar, Visited, Nodes)
    ;   member(State, PathSoFar)
    ->  expand_greedy_nodes(States, PathSoFar, Visited, Nodes)
    ;   combined_heuristic(State, H),
        append(PathSoFar, [State], NewPath),
        expand_greedy_nodes(States, PathSoFar, Visited, RestNodes),
        Nodes = [node(State, NewPath, H)|RestNodes]
    ).

%! sort_by_h_greedy(+Nodes:list, -Sorted:list) is det
% Ordina i nodi per valore euristico.
% @arg Nodes Lista di nodi
% @arg Sorted Lista ordinata per euristica
sort_by_h_greedy(Nodes, Sorted) :-
    predsort(compare_h_greedy, Nodes, Sorted).

%! compare_h_greedy(-Delta, +Node1:term, +Node2:term) is det
% Confronta nodi per valore euristico.
% @arg Delta Risultato confronto
% @arg Node1 Primo nodo
% @arg Node2 Secondo nodo
compare_h_greedy(<, node(_, _, H1), node(_, _, H2)) :- H1 < H2, !.
compare_h_greedy(>, _, _).

% =========================================================
% UTILITÀ PER GENERAZIONE E VALIDAZIONE
% =========================================================

%! generate_random_puzzle(+Difficulty:int, -Puzzle:list) is det
% Genera un puzzle casuale risolvibile.
% @arg Difficulty Numero di mosse casuali da applicare (10-100)
% @arg Puzzle Stato del puzzle generato
generate_random_puzzle(Difficulty, Puzzle) :-
    goal_state(Goal),
    random_moves(Goal, Difficulty, Puzzle).

%! random_moves(+State:list, +N:int, -Final:list) is det
% Applica N mosse casuali a uno stato.
% @arg State Stato iniziale
% @arg N Numero di mosse da applicare
% @arg Final Stato finale dopo N mosse
random_moves(State, 0, State) :- !.
random_moves(State, N, Final) :-
    N > 0,
    successors(State, Successors),
    random_member(NextState, Successors),
    N1 is N - 1,
    random_moves(NextState, N1, Final).

%! is_solvable(+State:list) is semidet
% Verifica se un puzzle è risolvibile controllando il numero di inversioni.
% @arg State Stato del puzzle da verificare
is_solvable(State) :-
    count_inversions(State, Count),
    Count mod 2 =:= 0.

%! count_inversions(+State:list, -Count:int) is det
% Conta il numero di inversioni in uno stato.
% @arg State Stato del puzzle
% @arg Count Numero di inversioni
count_inversions([], 0).
count_inversions([H|T], Count) :-
    (   H = 0
    ->  count_inversions(T, Count)
    ;   count_inversions_for_element(H, T, ElementCount),
        count_inversions(T, RestCount),
        Count is ElementCount + RestCount
    ).

%! count_inversions_for_element(+X:int, +List:list, -Count:int) is det
% Conta le inversioni per un singolo elemento.
% @arg X Elemento da confrontare
% @arg List Lista rimanente
% @arg Count Numero di inversioni per X
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

%! random_member(-X:any, +List:list) is det
% Seleziona un elemento casuale da una lista.
% @arg X Elemento selezionato
% @arg List Lista da cui selezionare
random_member(X, List) :-
    length(List, Len),
    Len > 0,
    random(0, Len, Index),
    nth0(Index, List, X).