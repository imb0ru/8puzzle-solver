"""
puzzle_logic.py - Logica di business per l'8-Puzzle Solver
===========================================================
Interfaccia tra Python e Prolog per la risoluzione del puzzle.
"""

import os
import sys
import random
import time
from pyswip import Prolog
from typing import List, Dict, Optional, Tuple


class PuzzleLogic:
    """
    Gestisce la logica del puzzle e l'interfacciamento con Prolog.
    """
    
    def __init__(self, debug=False):
        """
        Inizializza la logica del puzzle.
        
        Args:
            debug (bool): Modalità debug
        """
        self.debug = debug
        self.prolog = None
        self._initialize_prolog()
        
    def _initialize_prolog(self):
        """Inizializza l'interprete Prolog e carica la KB."""
        try:
            if self.debug:
                print("🔧 Inizializzazione Prolog...")
            
            # Cambia directory per pyswip
            prolog_dir = os.path.join(
                os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
                "prolog"
            )

            if not os.path.exists(prolog_dir):
                raise FileNotFoundError(f"Cartella Prolog non trovata: {prolog_dir}")
            
            # Carica i file Prolog
            solver_file = os.path.join(prolog_dir, "solver.pl")
            heuristics_file = os.path.join(prolog_dir, "heuristics.pl")

            if not os.path.exists(solver_file) or not os.path.exists(heuristics_file):
                raise FileNotFoundError("File Prolog mancanti.")

            self.prolog = Prolog()
            self.prolog.consult(solver_file)
            self.prolog.consult(heuristics_file)
            if self.debug:
                print("✅ Prolog inizializzato correttamente")
                
        except Exception as e:
            print(f"❌ Errore inizializzazione Prolog: {e}")
            raise
    
    def cleanup(self):
        """Pulisce le risorse."""
        if self.prolog:
            try:
                # Retract di tutti i fatti dinamici
                list(self.prolog.query("retractall(stato_visitato(_))"))
                list(self.prolog.query("retractall(soluzione_memorizzata(_))"))
            except:
                pass
    
    def is_solvable(self, state: List[int]) -> bool:
        """
        Verifica se una configurazione è risolvibile.
        
        L'8-puzzle è risolvibile se e solo se il numero di inversioni è pari.
        
        Args:
            state: Lista di 9 interi (0-8) rappresentante lo stato
            
        Returns:
            bool: True se risolvibile, False altrimenti
        """
        inversions = 0
        state_without_zero = [x for x in state if x != 0]
        
        for i in range(len(state_without_zero)):
            for j in range(i + 1, len(state_without_zero)):
                if state_without_zero[i] > state_without_zero[j]:
                    inversions += 1
        
        return inversions % 2 == 0
    
    def generate_random_puzzle(self, difficulty: str = "medium") -> List[int]:
        """
        Genera un puzzle casuale risolvibile.
        
        Args:
            difficulty: "easy", "medium", o "hard"
            
        Returns:
            Lista rappresentante lo stato del puzzle
        """
        # Mapping difficoltà -> numero di mosse casuali
        difficulty_map = {
            "easy": random.randint(10, 20),
            "medium": random.randint(20, 40),
            "hard": random.randint(40, 80)
        }
        
        moves = difficulty_map.get(difficulty, 30)
        
        # Parti dallo stato obiettivo
        state = [1, 2, 3, 4, 5, 6, 7, 8, 0]
        
        # Applica mosse casuali
        for _ in range(moves):
            empty_pos = state.index(0)
            valid_moves = self._get_valid_moves(state, empty_pos)
            
            if valid_moves:
                move = random.choice(valid_moves)
                new_pos = self._apply_move(empty_pos, move)
                state[empty_pos], state[new_pos] = state[new_pos], state[empty_pos]
        
        return state
    
    def _get_valid_moves(self, state: List[int], empty_pos: int) -> List[str]:
        """
        Ottiene le mosse valide per lo spazio vuoto.
        
        Args:
            state: Stato corrente
            empty_pos: Posizione dello spazio vuoto
            
        Returns:
            Lista di mosse valide ("up", "down", "left", "right")
        """
        moves = []
        row, col = empty_pos // 3, empty_pos % 3
        
        if row > 0:
            moves.append("up")
        if row < 2:
            moves.append("down")
        if col > 0:
            moves.append("left")
        if col < 2:
            moves.append("right")
        
        return moves
    
    def _apply_move(self, empty_pos: int, move: str) -> int:
        """
        Calcola la nuova posizione dopo una mossa.
        
        Args:
            empty_pos: Posizione corrente dello spazio vuoto
            move: Direzione della mossa
            
        Returns:
            Nuova posizione
        """
        row, col = empty_pos // 3, empty_pos % 3
        
        if move == "up":
            row -= 1
        elif move == "down":
            row += 1
        elif move == "left":
            col -= 1
        elif move == "right":
            col += 1
        
        return row * 3 + col
    
    def solve(self, initial_state: List[int], algorithm: str) -> Dict:
        """
        Risolve il puzzle usando l'algoritmo specificato.
        
        Args:
            initial_state: Stato iniziale del puzzle
            algorithm: Nome dell'algoritmo da usare
            
        Returns:
            Dizionario con risultati della risoluzione
        """
        # Verifica risolvibilità
        if not self.is_solvable(initial_state):
            return {
                'success': False,
                'message': 'Configurazione non risolvibile'
            }
        
        # Converti stato in formato Prolog
        state_str = self._state_to_prolog(initial_state)
        
        # Mappa algoritmi a predicati Prolog
        algorithm_map = {
            'astar_manhattan': 'solve_astar_manhattan',
            'astar_misplaced': 'solve_astar_misplaced',
            'astar_linear': 'solve_astar_linear',
            'idastar': 'solve_idastar',
            'bfs': 'solve_bfs',
            'greedy': 'solve_greedy',
        }
        
        predicate = algorithm_map.get(algorithm, 'solve_astar_manhattan')
        
        try:
            # Pulisci stati precedenti
            list(self.prolog.query("retractall(stato_visitato(_))"))
            list(self.prolog.query("retractall(soluzione_memorizzata(_))"))
            
            # Query Prolog
            query = f"{predicate}({state_str}, Path, NodesExplored, NodesFrontier, Time)"
            
            if self.debug:
                print(f"🔍 Query: {query}")
            
            start_time = time.time()
            solutions = list(self.prolog.query(query))
            elapsed_time = time.time() - start_time
            
            if solutions:
                solution = solutions[0]
                path = self._parse_prolog_path(solution['Path'])
                
                # Calcola memoria utilizzata (approssimativa)
                memory = (solution['NodesExplored'] + solution['NodesFrontier']) * 9 * 4
                
                # Determina se l'algoritmo è ottimale
                #todo non cé calcolo sull'oyttimalitá
                optimal_algorithms = ['astar_manhattan', 'astar_misplaced', 'astar_linear', 
                                    'idastar', 'bfs']
                is_optimal = algorithm in optimal_algorithms
                
                return {
                    'success': True,
                    'path': path,
                    'nodes_explored': solution['NodesExplored'],
                    'nodes_frontier': solution['NodesFrontier'],
                    'time': elapsed_time,
                    'memory': memory,
                    'optimal': is_optimal
                }
            else:
                return {
                    'success': False,
                    'message': 'Nessuna soluzione trovata'
                }
                
        except Exception as e:
            if self.debug:
                print(f"❌ Errore solver: {e}")
            
            return {
                'success': False,
                'message': f'Errore: {str(e)}'
            }
    
    def _state_to_prolog(self, state: List[int]) -> str:
        """
        Converte uno stato Python in formato Prolog.
        
        Args:
            state: Lista di 9 interi
            
        Returns:
            Stringa nel formato Prolog [1,2,3,4,5,6,7,8,0]
        """
        return str(state).replace(' ', '')
    
    def _parse_prolog_path(self, prolog_path) -> List[List[int]]:
        """
        Converte un percorso Prolog in lista Python.
        
        Args:
            prolog_path: Path restituito da Prolog
            
        Returns:
            Lista di stati (ogni stato è una lista di 9 interi)
        """
        path = []
        
        # Se il path è una stringa, parsala
        if isinstance(prolog_path, str):
            # Rimuovi le parentesi quadre esterne e dividi per stati
            prolog_path = prolog_path.strip('[]')
            states = prolog_path.split('],[')
            
            for state_str in states:
                state_str = state_str.strip('[]')
                if state_str:
                    state = [int(x) for x in state_str.split(',')]
                    path.append(state)
        else:
            # Se è già una lista di liste
            for state in prolog_path:
                if isinstance(state, list):
                    path.append(state)
                else:
                    # Prova a convertire
                    state_list = str(state).strip('[]').split(',')
                    path.append([int(x) for x in state_list])
        
        return path
    
    def get_manhattan_distance(self, state: List[int]) -> int:
        """
        Calcola la distanza di Manhattan per uno stato.
        
        Args:
            state: Stato del puzzle
            
        Returns:
            Distanza di Manhattan totale
        """
        distance = 0
        
        for i in range(9):
            if state[i] != 0:
                current_row, current_col = i // 3, i % 3
                goal_pos = state[i] - 1
                goal_row, goal_col = goal_pos // 3, goal_pos % 3
                distance += abs(current_row - goal_row) + abs(current_col - goal_col)
        
        return distance
    
    def get_misplaced_tiles(self, state: List[int]) -> int:
        """
        Conta le tessere fuori posto.
        
        Args:
            state: Stato del puzzle
            
        Returns:
            Numero di tessere fuori posto
        """
        goal = [1, 2, 3, 4, 5, 6, 7, 8, 0]
        count = 0
        
        for i in range(9):
            if state[i] != 0 and state[i] != goal[i]:
                count += 1
        
        return count
    
    def get_linear_conflict(self, state: List[int]) -> int:
        """
        Calcola il linear conflict (Manhattan + conflitti lineari).
        
        Args:
            state: Stato del puzzle
            
        Returns:
            Euristica linear conflict
        """
        manhattan = self.get_manhattan_distance(state)
        conflicts = 0
        
        # Controlla conflitti per ogni riga
        for row in range(3):
            for col1 in range(3):
                pos1 = row * 3 + col1
                if state[pos1] == 0:
                    continue
                    
                goal_row1 = (state[pos1] - 1) // 3
                if goal_row1 == row:  # La tessera appartiene a questa riga
                    for col2 in range(col1 + 1, 3):
                        pos2 = row * 3 + col2
                        if state[pos2] == 0:
                            continue
                            
                        goal_row2 = (state[pos2] - 1) // 3
                        goal_col2 = (state[pos2] - 1) % 3
                        goal_col1 = (state[pos1] - 1) % 3
                        
                        # Conflitto se entrambe appartengono alla riga e sono invertite
                        if goal_row2 == row and goal_col1 > goal_col2:
                            conflicts += 2
        
        # Controlla conflitti per ogni colonna
        for col in range(3):
            for row1 in range(3):
                pos1 = row1 * 3 + col
                if state[pos1] == 0:
                    continue
                    
                goal_col1 = (state[pos1] - 1) % 3
                if goal_col1 == col:  # La tessera appartiene a questa colonna
                    for row2 in range(row1 + 1, 3):
                        pos2 = row2 * 3 + col
                        if state[pos2] == 0:
                            continue
                            
                        goal_col2 = (state[pos2] - 1) % 3
                        goal_row2 = (state[pos2] - 1) // 3
                        goal_row1 = (state[pos1] - 1) // 3
                        
                        # Conflitto se entrambe appartengono alla colonna e sono invertite
                        if goal_col2 == col and goal_row1 > goal_row2:
                            conflicts += 2
        
        return manhattan + conflicts
    
    def validate_state(self, state: List[int]) -> Tuple[bool, str]:
        """
        Valida uno stato del puzzle.
        
        Args:
            state: Stato da validare
            
        Returns:
            Tupla (valido, messaggio_errore)
        """
        # Controlla lunghezza
        if len(state) != 9:
            return False, "Lo stato deve contenere esattamente 9 elementi"
        
        # Controlla che ci siano tutti i numeri da 0 a 8
        if sorted(state) != list(range(9)):
            return False, "Lo stato deve contenere i numeri da 0 a 8 senza ripetizioni"
        
        # Controlla risolvibilità
        if not self.is_solvable(state):
            return False, "Configurazione non risolvibile (numero dispari di inversioni)"
        
        return True, "Stato valido"
    
    def get_move_sequence(self, path: List[List[int]]) -> List[str]:
        """
        Converte un percorso di stati in sequenza di mosse.
        
        Args:
            path: Lista di stati
            
        Returns:
            Lista di mosse ("UP", "DOWN", "LEFT", "RIGHT")
        """
        if len(path) < 2:
            return []
        
        moves = []
        
        for i in range(len(path) - 1):
            current = path[i]
            next_state = path[i + 1]
            
            # Trova posizioni dello spazio vuoto
            current_empty = current.index(0)
            next_empty = next_state.index(0)
            
            # Determina la mossa
            current_row, current_col = current_empty // 3, current_empty % 3
            next_row, next_col = next_empty // 3, next_empty % 3
            
            if next_row < current_row:
                moves.append("UP")
            elif next_row > current_row:
                moves.append("DOWN")
            elif next_col < current_col:
                moves.append("LEFT")
            elif next_col > current_col:
                moves.append("RIGHT")
        
        return moves
    
    def benchmark_algorithms(self, test_states: List[List[int]], 
                           algorithms: List[str] = None) -> Dict:
        """
        Esegue benchmark su più algoritmi.
        
        Args:
            test_states: Lista di stati da testare
            algorithms: Lista di algoritmi (default: tutti)
            
        Returns:
            Dizionario con risultati del benchmark
        """
        if algorithms is None:
            algorithms = ['astar_manhattan', 'astar_misplaced', 'astar_linear',
                         'idastar', 'bfs', 'greedy']
        
        results = {algo: {
            'total_time': 0,
            'total_nodes': 0,
            'total_moves': 0,
            'solved': 0,
            'failed': 0
        } for algo in algorithms}
        
        for state in test_states:
            for algo in algorithms:
                result = self.solve(state, algo)
                
                if result['success']:
                    results[algo]['solved'] += 1
                    results[algo]['total_time'] += result['time']
                    results[algo]['total_nodes'] += result['nodes_explored']
                    results[algo]['total_moves'] += len(result['path']) - 1
                else:
                    results[algo]['failed'] += 1
        
        # Calcola medie
        for algo in algorithms:
            if results[algo]['solved'] > 0:
                n = results[algo]['solved']
                results[algo]['avg_time'] = results[algo]['total_time'] / n
                results[algo]['avg_nodes'] = results[algo]['total_nodes'] / n
                results[algo]['avg_moves'] = results[algo]['total_moves'] / n
            else:
                results[algo]['avg_time'] = float('inf')
                results[algo]['avg_nodes'] = float('inf')
                results[algo]['avg_moves'] = float('inf')
        
        return results