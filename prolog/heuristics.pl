/* =========================================================
   HEURISTICS MODULE - Euristiche per l'8-Puzzle
   =========================================================
   
   Questo modulo implementa diverse funzioni euristiche
   per guidare gli algoritmi di ricerca informata.
   
   Euristiche implementate:
   - Manhattan Distance
   - Misplaced Tiles
   - Linear Conflict
   - Pattern Database (semplificato)
   
   ========================================================= */

:- module(heuristics, [
    manhattan_distance/2,
    misplaced_tiles/2,
    linear_conflict/2,
    pattern_database/2,
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
% LINEAR CONFLICT
% =========================================================

% Linear Conflict = Manhattan Distance + 2 * numero di conflitti lineari
linear_conflict(State, Value) :-
    manhattan_distance(State, Manhattan),
    count_linear_conflicts(State, Conflicts),
    Value is Manhattan + 2 * Conflicts.

% Conta i conflitti lineari totali
count_linear_conflicts(State, Total) :-
    count_row_conflicts(State, RowConflicts),
    count_col_conflicts(State, ColConflicts),
    Total is RowConflicts + ColConflicts.

% Conta i conflitti nelle righe
count_row_conflicts(State, Total) :-
    extract_row(State, 0, Row0),
    extract_row(State, 1, Row1),
    extract_row(State, 2, Row2),
    row_conflicts(Row0, 0, C0),
    row_conflicts(Row1, 1, C1),
    row_conflicts(Row2, 2, C2),
    Total is C0 + C1 + C2.

% Estrae una riga dallo stato
extract_row(State, RowNum, Row) :-
    Start is RowNum * 3,
    End is Start + 3,
    extract_range(State, Start, End, Row).

extract_range(_, Start, End, []) :- Start >= End, !.
extract_range(State, Start, End, [Elem|Rest]) :-
    nth0(Start, State, Elem),
    NextStart is Start + 1,
    extract_range(State, NextStart, End, Rest).

% Conta conflitti in una riga specifica
row_conflicts([], _, 0).
row_conflicts([Tile|Rest], RowNum, Conflicts) :-
    (   Tile = 0
    ->  row_conflicts(Rest, RowNum, Conflicts)
    ;   goal_row(Tile, GoalRow),
        GoalRow = RowNum
    ->  count_conflicts_with_rest(Tile, Rest, RowNum, PartialConflicts),
        row_conflicts(Rest, RowNum, RestConflicts),
        Conflicts is PartialConflicts + RestConflicts
    ;   row_conflicts(Rest, RowNum, Conflicts)
    ).

% Conta conflitti di una tessera con le successive nella stessa riga
count_conflicts_with_rest(_, [], _, 0).
count_conflicts_with_rest(Tile1, [Tile2|Rest], RowNum, Conflicts) :-
    (   Tile2 = 0
    ->  count_conflicts_with_rest(Tile1, Rest, RowNum, Conflicts)
    ;   goal_row(Tile2, GoalRow2),
        GoalRow2 = RowNum,
        goal_col(Tile1, Col1),
        goal_col(Tile2, Col2),
        Col1 > Col2
    ->  count_conflicts_with_rest(Tile1, Rest, RowNum, RestConflicts),
        Conflicts is RestConflicts + 1
    ;   count_conflicts_with_rest(Tile1, Rest, RowNum, Conflicts)
    ).

% Conta i conflitti nelle colonne
count_col_conflicts(State, Total) :-
    extract_col(State, 0, Col0),
    extract_col(State, 1, Col1),
    extract_col(State, 2, Col2),
    col_conflicts(Col0, 0, C0),
    col_conflicts(Col1, 1, C1),
    col_conflicts(Col2, 2, C2),
    Total is C0 + C1 + C2.

% Estrae una colonna dallo stato
extract_col(State, ColNum, Col) :-
    Pos0 is ColNum,
    Pos1 is ColNum + 3,
    Pos2 is ColNum + 6,
    nth0(Pos0, State, Elem0),
    nth0(Pos1, State, Elem1),
    nth0(Pos2, State, Elem2),
    Col = [Elem0, Elem1, Elem2].

% Conta conflitti in una colonna specifica
col_conflicts([], _, 0).
col_conflicts([Tile|Rest], ColNum, Conflicts) :-
    (   Tile = 0
    ->  col_conflicts(Rest, ColNum, Conflicts)
    ;   goal_col(Tile, GoalCol),
        GoalCol = ColNum
    ->  count_col_conflicts_with_rest(Tile, Rest, ColNum, PartialConflicts),
        col_conflicts(Rest, ColNum, RestConflicts),
        Conflicts is PartialConflicts + RestConflicts
    ;   col_conflicts(Rest, ColNum, Conflicts)
    ).

% Conta conflitti di una tessera con le successive nella stessa colonna
count_col_conflicts_with_rest(_, [], _, 0).
count_col_conflicts_with_rest(Tile1, [Tile2|Rest], ColNum, Conflicts) :-
    (   Tile2 = 0
    ->  count_col_conflicts_with_rest(Tile1, Rest, ColNum, Conflicts)
    ;   goal_col(Tile2, GoalCol2),
        GoalCol2 = ColNum,
        goal_row(Tile1, Row1),
        goal_row(Tile2, Row2),
        Row1 > Row2
    ->  count_col_conflicts_with_rest(Tile1, Rest, ColNum, RestConflicts),
        Conflicts is RestConflicts + 1
    ;   count_col_conflicts_with_rest(Tile1, Rest, ColNum, Conflicts)
    ).

% Posizione obiettivo di una tessera
goal_row(Tile, Row) :-
    Tile > 0,
    Pos is Tile - 1,
    Row is Pos // 3.

goal_col(Tile, Col) :-
    Tile > 0,
    Pos is Tile - 1,
    Col is Pos mod 3.

% =========================================================
% PATTERN DATABASE (SEMPLIFICATO)
% =========================================================

% Pattern database semplificato: somma delle distanze dei corner
pattern_database(State, Value) :-
    % Considera solo le tessere negli angoli (1, 3, 7, 8)
    nth0(0, State, Tile0),
    nth0(2, State, Tile2),
    nth0(6, State, Tile6),
    nth0(8, State, Tile8),
    
    corner_distance(Tile0, 0, 1, D0),
    corner_distance(Tile2, 2, 3, D2),
    corner_distance(Tile6, 6, 7, D6),
    corner_distance(Tile8, 8, 0, D8),  % 0 dovrebbe essere in posizione 8
    
    Value is D0 + D2 + D6 + D8.

corner_distance(Tile, CurrentPos, ExpectedTile, Distance) :-
    (   Tile = ExpectedTile
    ->  Distance = 0
    ;   Tile = 0, ExpectedTile = 0
    ->  Distance = 0
    ;   tile_manhattan(CurrentPos, ExpectedPos, Distance),
        (ExpectedTile = 0 -> ExpectedPos = 8 ; ExpectedPos is ExpectedTile - 1)
    ).

% =========================================================
% EURISTICA COMBINATA
% =========================================================

% Combina più euristiche con pesi
combined_heuristic(State, Value) :-
    manhattan_distance(State, Manhattan),
    misplaced_tiles(State, Misplaced),
    linear_conflict(State, Linear),
    
    % Usa euristica più informativa (linear conflict)
    % ma considera anche le altre per tie-breaking
    Value is max(Linear, Manhattan + Misplaced // 2).

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
    linear_conflict(State, Linear),
    pattern_database(State, Pattern),
    combined_heuristic(State, Combined),
    
    format('~nEuristiche per lo stato:~n', []),
    print_state(State),
    format('Manhattan Distance: ~w~n', [Manhattan]),
    format('Misplaced Tiles: ~w~n', [Misplaced]),
    format('Linear Conflict: ~w~n', [Linear]),
    format('Pattern Database: ~w~n', [Pattern]),
    format('Combined: ~w~n', [Combined]).