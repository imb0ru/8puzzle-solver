/* =========================================================
   HEURISTICS MODULE - Euristiche per l'8-Puzzle
   =========================================================
   
   Questo modulo implementa diverse funzioni euristiche
   per guidare gli algoritmi di ricerca informata.
   
   Euristiche implementate:
   - Manhattan Distance
   - Misplaced Tiles
   - Combined
   
   ========================================================= */

:- module(heuristics, [
    manhattan_distance/2,
    misplaced_tiles/2,
    combined_heuristic/2
]).

% =========================================================
% MANHATTAN DISTANCE
% =========================================================

% Calcola la distanza di Manhattan totale
manhattan_distance(State, Distance) :-
    goal_positions(GoalPos),
    manhattan_distance_acc(State, 0, GoalPos, 0, Distance).

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

% Calcola la distanza di Manhattan tra due posizioni
tile_manhattan(Pos1, Pos2, Distance) :-
    Row1 is Pos1 // 3,
    Col1 is Pos1 mod 3,
    Row2 is Pos2 // 3,
    Col2 is Pos2 mod 3,
    Distance is abs(Row1 - Row2) + abs(Col1 - Col2).

% Posizioni obiettivo per ogni tessera
goal_positions([1,2,3,4,5,6,7,8,0]).

% =========================================================
% MISPLACED TILES
% =========================================================

% Conta il numero di tessere fuori posto
misplaced_tiles(State, Count) :-
    goal_state(Goal),
    misplaced_tiles_acc(State, Goal, 0, Count).

misplaced_tiles_acc([], [], Acc, Acc).
misplaced_tiles_acc([Tile|RestState], [GoalTile|RestGoal], Acc, Count) :-
    (   Tile = 0
    ->  NewAcc = Acc
    ;   Tile = GoalTile
    ->  NewAcc = Acc
    ;   NewAcc is Acc + 1
    ),
    misplaced_tiles_acc(RestState, RestGoal, NewAcc, Count).

goal_state([1,2,3,4,5,6,7,8,0]).

% =========================================================
% EURISTICA COMBINATA
% =========================================================

% Combina più euristiche con pesi
combined_heuristic(State, Value) :-
    manhattan_distance(State, Manhattan),
    misplaced_tiles(State, Misplaced),
    Value is max(Manhattan + Misplaced // 2).

% =========================================================
% EURISTICHE AVANZATE (OPZIONALI)
% =========================================================

% Walking distance: considera le mosse minime per ogni riga/colonna
walking_distance(State, Distance) :-
    % Implementazione semplificata
    manhattan_distance(State, Distance).

% Last moves heuristic: penalizza mosse che annullano le precedenti
last_moves_penalty(LastMove, CurrentMove, Penalty) :-
    (   opposite_moves(LastMove, CurrentMove)
    ->  Penalty = 2
    ;   Penalty = 0
    ).

opposite_moves(up, down).
opposite_moves(down, up).
opposite_moves(left, right).
opposite_moves(right, left).

% =========================================================
% UTILITÀ PER DEBUG
% =========================================================

% Stampa lo stato del puzzle in formato leggibile
print_state(State) :-
    State = [T0, T1, T2, T3, T4, T5, T6, T7, T8],
    format('~n+---+---+---+~n', []),
    print_tile(T0), write('|'), print_tile(T1), write('|'), print_tile(T2), write('|'), nl,
    format('+---+---+---+~n', []),
    print_tile(T3), write('|'), print_tile(T4), write('|'), print_tile(T5), write('|'), nl,
    format('+---+---+---+~n', []),
    print_tile(T6), write('|'), print_tile(T7), write('|'), print_tile(T8), write('|'), nl,
    format('+---+---+---+~n', []).

print_tile(0) :- write('   '), !.
print_tile(N) :- format(' ~w ', [N]).

% Valuta tutte le euristiche per uno stato
evaluate_all_heuristics(State) :-
    manhattan_distance(State, Manhattan),
    misplaced_tiles(State, Misplaced),
    pattern_database(State, Pattern),
    combined_heuristic(State, Combined),
    
    format('~nEuristiche per lo stato:~n', []),
    print_state(State),
    format('Manhattan Distance: ~w~n', [Manhattan]),
    format('Misplaced Tiles: ~w~n', [Misplaced]),
    format('Pattern Database: ~w~n', [Pattern]),
    format('Combined: ~w~n', [Combined]).