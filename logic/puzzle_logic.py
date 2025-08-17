"""
puzzle_logic.py - Logica di business per l'8-Puzzle Solver
===========================================================
Interfaccia tra Python e Prolog per la risoluzione del puzzle.
"""

import os
import sys
import random
import time
import atexit
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
        # Registra cleanup per evitare warning
        atexit.register(self.cleanup)
        
    def _initialize_prolog(self):
        """
        Inizializza l'interprete Prolog e carica la knowledge base.
        
        Configura SWI-Prolog, carica i file solver.pl e heuristics.pl,
        e sopprime i warning per un output pulito. Verifica l'esistenza
        dei file necessari e gestisce errori di configurazione.
        
        Raises:
            FileNotFoundError: Se i file Prolog non vengono trovati
            Exception: Per errori di inizializzazione di SWI-Prolog
        """
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
            
            # Sopprime warning di SWI-Prolog
            self.prolog.query("set_prolog_flag(verbose, silent)").close()
            
            self.prolog.consult(solver_file)
            self.prolog.consult(heuristics_file)
            
            if self.debug:
                print("✅ Prolog inizializzato correttamente")
                
        except Exception as e:
            print(f"❌ Errore inizializzazione Prolog: {e}")
            raise
    
    def cleanup(self):
        """
        Pulisce le risorse Prolog e previene warning di terminazione.
        
        Chiama la funzione di cleanup definita in Prolog per rimuovere
        stati memorizzati e query pendenti, poi termina l'interprete
        in modo pulito per evitare warning di sistema.
        """
        if self.prolog:
            try:
                # Chiama la funzione di cleanup definita in Prolog
                query = self.prolog.query("cleanup_prolog")
                list(query)
                query.close()
                
                # Pulisce query pendenti
                self.prolog.query("halt").close()
            except:
                pass
            finally:
                self.prolog = None
    
    def __del__(self):
        """
        Destructor per pulire risorse.
        
        Non esegue operazioni dirette per evitare problemi durante
        la garbage collection. Il cleanup è gestito dal sistema atexit.
        """
        # Non fare nulla qui, lascia che atexit gestisca il cleanup
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
        Ottiene le mosse valide per lo spazio vuoto nella griglia 3x3.
        
        Args:
            state: Stato corrente del puzzle
            empty_pos (int): Posizione dello spazio vuoto (0-8)
            
        Returns:
            List[str]: Lista di direzioni valide ("up", "down", "left", "right")
            
        Note:
            Le mosse sono limitate dai bordi della griglia:
            - "up" non disponibile per riga 0
            - "down" non disponibile per riga 2
            - "left" non disponibile per colonna 0
            - "right" non disponibile per colonna 2
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
        Calcola la nuova posizione dello spazio vuoto dopo una mossa.
        
        Args:
            empty_pos (int): Posizione corrente dello spazio vuoto (0-8)
            move (str): Direzione della mossa ("up", "down", "left", "right")
            
        Returns:
            int: Nuova posizione dello spazio vuoto
            
        Note:
            Le mosse sono relative allo spazio vuoto:
            - "up": spazio vuoto si muove verso l'alto (riga-1)
            - "down": spazio vuoto si muove verso il basso (riga+1)
            - "left": spazio vuoto si muove a sinistra (col-1)
            - "right": spazio vuoto si muove a destra (col+1)
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
            'astar': 'solve_astar',
            'bfs': 'solve_bfs',
            'greedy': 'solve_greedy',
        }

        predicate = algorithm_map.get(algorithm, 'solve_astar')
        
        try:
            # Pulisci stati precedenti
            self.prolog.query("retractall(stato_visitato(_))").close()
            self.prolog.query("retractall(soluzione_memorizzata(_))").close()
            self.prolog.query("retractall(optimal_cost(_))").close()
            
            # Query Prolog
            query_str = f"{predicate}({state_str}, Path, NodesExplored, NodesFrontier, Time)"
            
            if self.debug:
                print(f"🔍 Query: {query_str}")
            
            start_time = time.time()
            query = self.prolog.query(query_str)
            solutions = list(query)
            query.close()
            elapsed_time = time.time() - start_time
            
            if solutions:
                solution = solutions[0]
                path = self._parse_prolog_path(solution['Path'])
                path_length = len(path) - 1
                
                # Calcola memoria utilizzata (approssimativa)
                memory = (solution['NodesExplored'] + solution['NodesFrontier']) * 9 * 4
                
                return {
                    'success': True,
                    'path': path,
                    'path_length': path_length,
                    'nodes_explored': solution['NodesExplored'],
                    'nodes_frontier': solution['NodesFrontier'],
                    'time': elapsed_time,
                    'memory': memory,
                    'algorithm': algorithm
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
        Converte uno stato Python in formato compatibile con Prolog.
        
        Args:
            state (List[int]): Lista di 9 interi rappresentanti lo stato
            
        Returns:
            str: Stringa nel formato Prolog [1,2,3,4,5,6,7,8,0]
            
        Note:
            Rimuove gli spazi dalla rappresentazione per garantire
            compatibilità con il parser Prolog.
        """
        return str(state).replace(' ', '')
    
    def _parse_prolog_path(self, prolog_path) -> List[List[int]]:
        """
        Converte un percorso Prolog in lista Python.
        
        Args:
            prolog_path: Percorso restituito da Prolog (stringa o lista)
            
        Returns:
            List[List[int]]: Lista di stati, ognuno rappresentato come lista di 9 interi
            
        Note:
            Gestisce sia format stringa che lista nativa di Prolog.
            Ogni stato nel percorso rappresenta una configurazione del puzzle
            dalla situazione iniziale alla soluzione.
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
    
    def test_algorithms(self, test_states: List[List[int]], 
                           algorithms: List[str] = None) -> Dict:
        """
        Esegue test su più algoritmi.

        Args:
            test_states: Lista di stati da testare
            algorithms: Lista di algoritmi (default: tutti)
            
        Returns:
            Dizionario con risultati del test
        """
        if algorithms is None:
            algorithms = ['astar', 'bfs', 'greedy']
        
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
                    results[algo]['total_moves'] += result['path_length']
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