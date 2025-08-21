/** <module> Heuristics Module - Euristiche per l'8-Puzzle
 *
 * Questo modulo implementa diverse funzioni euristiche per guidare 
 * gli algoritmi di ricerca informata nell'8-puzzle.
 * 
 * Le euristiche implementate sono:
 * - Manhattan Distance: somma delle distanze Manhattan di ogni tessera
 * - Misplaced Tiles: numero di tessere fuori posto
 * - Combined Heuristic: combinazione pesata delle precedenti
 *
 * Il sistema usa l'euristica combinata che integra Manhattan Distance 
 * e Misplaced Tiles per massima efficienza mantenendo l'ammissibilità.
 *
 * @author Marco Ferrara
 * @version 1.0
 * @license MIT
 */

:- module(heuristics, [
    manhattan_distance/2,
    misplaced_tiles/2,
    combined_heuristic/2,
    print_state/1,
    print_tile/1
]).

% =========================================================
% MANHATTAN DISTANCE
% =========================================================

%! manhattan_distance(+State:list, -Distance:int) is det
% Calcola la distanza di Manhattan totale per uno stato del puzzle.
% La distanza di Manhattan è la somma delle distanze di ogni tessera
% dalla sua posizione obiettivo, calcolata come |x1-x2| + |y1-y2|.
%
% @arg State Lista di 9 elementi rappresentante lo stato del puzzle
% @arg Distance Distanza di Manhattan totale
%
manhattan_distance(State, Distance) :-
    goal_positions(GoalPos),
    manhattan_distance_acc(State, 0, GoalPos, 0, Distance).

%! manhattan_distance_acc(+State:list, +Pos:int, +GoalPos:list, +Acc:int, -Distance:int) is det
% Predicato accumulatore per il calcolo della distanza di Manhattan.
%
% @arg State Lista rimanente delle tessere
% @arg Pos Posizione corrente (0-8)
% @arg GoalPos Posizioni obiettivo di tutte le tessere
% @arg Acc Accumulatore della distanza
% @arg Distance Distanza totale finale
manhattan_distance_acc([], _, _, Acc, Acc).
manhattan_distance_acc([Tile|Rest], Pos, GoalPos, Acc, Distance) :-
    (   Tile = 0
    ->  NewAcc = Acc
    ;   nth0(GoalIndex, GoalPos, Tile),
        tile_manhattan(Pos, GoalIndex, TileDist),
        NewAcc is Acc + TileDist
    ),
    NextPos is Pos + 1,
    manhattan_distance_acc(Rest, NextPos, GoalPos, NewAcc, Distance).

%! tile_manhattan(+Pos1:int, +Pos2:int, -Distance:int) is det
% Calcola la distanza di Manhattan tra due posizioni nella griglia 3x3.
%
% @arg Pos1 Prima posizione (0-8)
% @arg Pos2 Seconda posizione (0-8)
% @arg Distance Distanza di Manhattan tra le posizioni
%
tile_manhattan(Pos1, Pos2, Distance) :-
    Row1 is Pos1 // 3,
    Col1 is Pos1 mod 3,
    Row2 is Pos2 // 3,
    Col2 is Pos2 mod 3,
    Distance is abs(Row1 - Row2) + abs(Col1 - Col2).

%! goal_positions(-Positions:list) is det
% Restituisce le posizioni obiettivo per ogni tessera.
% Lo stato obiettivo è [1,2,3,4,5,6,7,8,0].
%
% @arg Positions Lista delle posizioni obiettivo
goal_positions([1,2,3,4,5,6,7,8,0]).

% =========================================================
% MISPLACED TILES
% =========================================================

%! misplaced_tiles(+State:list, -Count:int) is det
% Conta il numero di tessere fuori posto rispetto allo stato obiettivo.
% Non conta lo spazio vuoto (0) come tessera fuori posto.
%
% @arg State Stato corrente del puzzle
% @arg Count Numero di tessere fuori posto
misplaced_tiles(State, Count) :-
    goal_state(Goal),
    misplaced_tiles_acc(State, Goal, 0, Count).

%! misplaced_tiles_acc(+State:list, +Goal:list, +Acc:int, -Count:int) is det
% Predicato accumulatore per contare le tessere fuori posto.
%
% @arg State Lista rimanente dello stato corrente
% @arg Goal Lista rimanente dello stato obiettivo
% @arg Acc Accumulatore del conteggio
% @arg Count Conteggio finale
misplaced_tiles_acc([], [], Acc, Acc).
misplaced_tiles_acc([Tile|RestState], [GoalTile|RestGoal], Acc, Count) :-
    (   Tile = 0
    ->  NewAcc = Acc
    ;   Tile = GoalTile
    ->  NewAcc = Acc
    ;   NewAcc is Acc + 1
    ),
    misplaced_tiles_acc(RestState, RestGoal, NewAcc, Count).

%! goal_state(-State:list) is det
% Importa lo stato obiettivo 8-puzzle.
%
% 
:- use_module('solver.pl', [goal_state/1]).

% =========================================================
% EURISTICA COMBINATA
% =========================================================

%! combined_heuristic(+State:list, -Value:int) is det
% Calcola euristica combinata che integra Manhattan Distance e Misplaced Tiles.
% Questa euristica è ammissibile (non sovrastima mai il costo reale) e
% più informativa delle singole euristiche.
%
% La formula utilizzata è:
% H = max(Manhattan, Manhattan + MisplacedTiles/2)
%
% @arg State Stato corrente del puzzle
% @arg Value Valore euristica combinata
combined_heuristic(State, Value) :-
    manhattan_distance(State, Manhattan),
    misplaced_tiles(State, Misplaced),
    % Combina le euristiche: usa il massimo tra Manhattan e (Manhattan + Misplaced/2)
    % Questo garantisce ammissibilità mantenendo informatività
    MisplacedWeight is Misplaced // 2,
    Combined is Manhattan + MisplacedWeight,
    % Prendi il massimo tra Manhattan e la combinazione pesata
    (   Combined > Manhattan
    ->  Value = Combined
    ;   Value = Manhattan
    ).

% =========================================================
% UTILITÀ PER DEBUG
% =========================================================

%! print_state(+State:list) is det
% Stampa lo stato del puzzle in formato leggibile come griglia 3x3.
% Utile per debug e visualizzazione durante lo sviluppo.
%
% @arg State Lista di 9 elementi rappresentante lo stato
print_state(State) :-
    State = [T0, T1, T2, T3, T4, T5, T6, T7, T8],
    format('~n+---+---+---+~n', []),
    print_tile(T0), write('|'), print_tile(T1), write('|'), print_tile(T2), write('|'), nl,
    format('+---+---+---+~n', []),
    print_tile(T3), write('|'), print_tile(T4), write('|'), print_tile(T5), write('|'), nl,
    format('+---+---+---+~n', []),
    print_tile(T6), write('|'), print_tile(T7), write('|'), print_tile(T8), write('|'), nl,
    format('+---+---+---+~n', []).

%! print_tile(+N:int) is det
% Stampa una singola tessera formattata per la griglia.
% Lo spazio vuoto (0) viene stampato come tre spazi.
%
% @arg N Numero della tessera (0-8) dove 0 è lo spazio vuoto
print_tile(0) :- write('   '), !.
print_tile(N) :- format(' ~w ', [N]).