"""
test.py - Test per confrontare gli algoritmi
=============================================================
Esegue test sistematici su tutti gli algoritmi implementati.
"""

import sys
import os
import time
import pandas as pd
from typing import List, Dict
import json
import argparse
from datetime import datetime

# Aggiungi path del progetto
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from logic.puzzle_logic import PuzzleLogic


class PuzzleTest:
    """
    Classe per eseguire test sistematici sugli algoritmi dell'8-puzzle.
    
    Gestisce l'esecuzione di test comparativi tra diversi algoritmi di ricerca,
    la generazione di puzzle di varie difficoltà, la raccolta di statistiche
    e l'esportazione dei risultati in formati multipli (CSV, JSON, Markdown).
    """
    
    def __init__(self):
        """
        Inizializza il sistema di test.
        
        Configura la logica del puzzle, prepara le strutture dati per
        i risultati e imposta la cartella di destinazione per i file
        di output.
        """
        self.logic = PuzzleLogic(debug=False)
        self.results = []
        self.results_dir = os.path.join(
            os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 
            'results'
        )
        # Non creiamo più la cartella qui, sarà creata al momento dell'esecuzione
    
    def create_run_directory(self, timestamp: str) -> str:
        """
        Crea una cartella specifica per questa esecuzione del test.
        
        Args:
            timestamp: Timestamp per il nome della cartella
            
        Returns:
            Percorso completo della cartella creata
        """
        # Crea prima la cartella principale results se non esiste
        try:
            os.makedirs(self.results_dir, exist_ok=True)
        except Exception as e:
            print(f"⚠️ Errore nella creazione della cartella results: {e}")
            self.results_dir = os.getcwd()
            print(f"📁 Userò la cartella corrente: {self.results_dir}")
        
        # Crea la cartella per questa esecuzione
        run_dir = os.path.join(self.results_dir, f"test_{timestamp}")
        
        try:
            os.makedirs(run_dir, exist_ok=True)
            print(f"📁 Cartella risultati per questa esecuzione: {run_dir}")
            
            # Test di scrittura
            test_file = os.path.join(run_dir, '.test_write')
            with open(test_file, 'w') as f:
                f.write('test')
            os.remove(test_file)
            
            return run_dir
            
        except Exception as e:
            print(f"⚠️ Errore nella creazione della cartella: {e}")
            print(f"📁 Userò la cartella principale: {self.results_dir}")
            return self.results_dir
    
    def generate_test_puzzles(self, count: int, difficulty: str) -> List[List[int]]:
        """
        Genera puzzle di test per una specifica difficoltà.
        
        Args:
            count: Numero di puzzle da generare
            difficulty: "easy", "medium", "hard", o "mixed"
            
        Returns:
            Lista di puzzle
        """
        puzzles = []
        
        if difficulty == "mixed":
            # Per mixed, genera 1/3 di ogni difficoltà
            easy_count = count // 3
            medium_count = count // 3
            hard_count = count - easy_count - medium_count
            
            for _ in range(easy_count):
                puzzles.append(self.logic.generate_random_puzzle("easy"))
            for _ in range(medium_count):
                puzzles.append(self.logic.generate_random_puzzle("medium"))
            for _ in range(hard_count):
                puzzles.append(self.logic.generate_random_puzzle("hard"))
        else:
            # Genera tutti della stessa difficoltà
            for _ in range(count):
                puzzles.append(self.logic.generate_random_puzzle(difficulty))
        
        return puzzles
    
    def run_single_test(self, puzzles: List[List[int]], 
                           difficulty_label: str,
                           algorithms: List[str] = None) -> pd.DataFrame:
        """
        Esegue il test su un set di puzzle.
        
        Args:
            puzzles: Lista di puzzle da testare
            difficulty_label: Etichetta della difficoltà per il report
            algorithms: Algoritmi da testare (default: tutti)
            
        Returns:
            DataFrame con i risultati
        """
        if algorithms is None:
            algorithms = [
                'astar',
                'bfs',
                'greedy',
            ]
        
        print(f"\n🎯 Testing {difficulty_label}: {len(puzzles)} puzzle × {len(algorithms)} algoritmi")
        print("-" * 60)
        
        results = []
        total_tests = len(puzzles) * len(algorithms)
        current_test = 0
        
        for puzzle_idx, puzzle in enumerate(puzzles):
            # Calcola difficoltà del puzzle
            manhattan = self.logic.get_manhattan_distance(puzzle)
            
            for algo in algorithms:
                current_test += 1
                progress = (current_test / total_tests) * 100
                print(f"\r  Progresso: {progress:.1f}%", end="", flush=True)
                
                try:
                    start_time = time.time()
                    result = self.logic.solve(puzzle, algo)
                    solve_time = time.time() - start_time
                    
                    if result['success']:
                        results.append({
                            'difficulty_category': difficulty_label,
                            'puzzle_id': puzzle_idx,
                            'algorithm': algo,
                            'success': True,
                            'time': solve_time,
                            'path_length': result.get('path_length', len(result['path']) - 1),
                            'nodes_explored': result.get('nodes_explored', 0),
                            'nodes_frontier': result.get('nodes_frontier', 0),
                            'memory': result.get('memory', 0),
                            'puzzle_manhattan': manhattan
                        })
                    else:
                        results.append({
                            'difficulty_category': difficulty_label,
                            'puzzle_id': puzzle_idx,
                            'algorithm': algo,
                            'success': False,
                            'time': solve_time,
                            'path_length': None,
                            'nodes_explored': None,
                            'nodes_frontier': None,
                            'memory': None,
                            'puzzle_manhattan': manhattan
                        })
                            
                except Exception as e:
                    results.append({
                        'difficulty_category': difficulty_label,
                        'puzzle_id': puzzle_idx,
                        'algorithm': algo,
                        'success': False,
                        'error': str(e),
                        'puzzle_manhattan': manhattan
                    })
        
        print() 
        return pd.DataFrame(results)
    
    def run_complete_test(self, n_puzzles: int) -> pd.DataFrame:
        """
        Esegue test completo su tutte le difficoltà.
        
        Args:
            n_puzzles: Numero di puzzle per ogni difficoltà
            
        Returns:
            DataFrame con tutti i risultati
        """
        print("\n" + "=" * 60)
        print("🚀 AVVIO TEST COMPLETO")
        print("=" * 60)
        print(f"Configurazione:")
        print(f"  • Puzzle per difficoltà: {n_puzzles}")
        print(f"  • Difficoltà testate: Easy, Medium, Hard, Mixed")
        print(f"  • Test totali: {n_puzzles * 4} puzzle × 3 algoritmi = {n_puzzles * 4 * 3} test")
        print("=" * 60)
        
        all_results = []
        
        # Test per ogni difficoltà
        difficulties = ['easy', 'medium', 'hard', 'mixed']
        
        for difficulty in difficulties:
            # Genera puzzle per questa difficoltà
            puzzles = self.generate_test_puzzles(n_puzzles, difficulty)
            
            # Esegui test
            df_difficulty = self.run_single_test(
                puzzles, 
                difficulty.capitalize()
            )
            
            all_results.append(df_difficulty)
        
        # Combina tutti i risultati
        final_df = pd.concat(all_results, ignore_index=True)
        
        print("\n" + "=" * 60)
        print("✅ TEST COMPLETATO!")
        print("=" * 60)
        
        return final_df
    
    def analyze_results(self, df: pd.DataFrame) -> Dict:
        """
        Analizza i risultati del test.
        
        Args:
            df: DataFrame con i risultati
            
        Returns:
            Dizionario con statistiche aggregate
        """
        print("\n📊 ANALISI RISULTATI")
        print("=" * 60)
        
        # Statistiche globali
        print(f"\n📈 STATISTICHE GLOBALI:")
        print(f"  • Test totali: {len(df)}")
        print(f"  • Test riusciti: {df['success'].sum()} ({df['success'].mean()*100:.1f}%)")
        print(f"  • Puzzle unici testati: {df['puzzle_id'].nunique() * 4}") 
        
        # Filtra solo i successi per le statistiche
        success_df = df[df['success'] == True].copy()
        
        # Statistiche per algoritmo (globali)
        stats = {}
        
        print("\n📊 PRESTAZIONI PER ALGORITMO (tutte le difficoltà):")
        print("-" * 80)
        print(f"{'Algoritmo':<20} {'Success%':<10} {'Avg Time':<12} {'Avg Moves':<10} {'Avg Nodes':<12}")
        print("-" * 80)
        
        for algo in df['algorithm'].unique():
            algo_df = df[df['algorithm'] == algo]
            algo_success = success_df[success_df['algorithm'] == algo]
            
            total = len(algo_df)
            successes = len(algo_success)
            success_rate = (successes / total * 100) if total > 0 else 0
            
            if len(algo_success) > 0:
                avg_time = algo_success['time'].mean()
                avg_moves = algo_success['path_length'].mean()
                avg_nodes = algo_success['nodes_explored'].mean()
                
                stats[algo] = {
                    'success_rate': success_rate,
                    'avg_time': avg_time,
                    'avg_moves': avg_moves,
                    'avg_nodes': avg_nodes,
                    'total_tests': total,
                    'successful_tests': successes
                }
                
                print(f"{algo:<20} {success_rate:>8.1f}% "
                     f"{avg_time:>10.3f}s "
                     f"{avg_moves:>9.1f} "
                     f"{avg_nodes:>11.0f}")
            else:
                stats[algo] = {
                    'success_rate': 0,
                    'avg_time': float('inf'),
                    'avg_moves': float('inf'),
                    'avg_nodes': float('inf'),
                    'total_tests': total,
                    'successful_tests': 0
                }
                
                print(f"{algo:<20} {success_rate:>8.1f}% "
                     f"{'N/A':>10} "
                     f"{'N/A':>9} "
                     f"{'N/A':>11}")
        
        # Statistiche per difficoltà
        print("\n📊 PRESTAZIONI PER DIFFICOLTÀ:")
        print("-" * 80)
        
        for difficulty in df['difficulty_category'].unique():
            diff_df = success_df[success_df['difficulty_category'] == difficulty]
            if len(diff_df) > 0:
                print(f"\n{difficulty}:")
                for algo in df['algorithm'].unique():
                    algo_diff = diff_df[diff_df['algorithm'] == algo]
                    if len(algo_diff) > 0:
                        print(f"  {algo:<20} - Time: {algo_diff['time'].mean():.3f}s, "
                             f"Moves: {algo_diff['path_length'].mean():.1f}, "
                             f"Nodes: {algo_diff['nodes_explored'].mean():.0f}")
        
        # Trova il migliore
        print("\n🏆 MIGLIORI PRESTAZIONI:")
        print("-" * 40)
        
        if success_df.empty:
            print("Nessun algoritmo ha risolto i puzzle!")
        else:
            valid_stats = {k: v for k, v in stats.items() if v['avg_time'] != float('inf')}
            if valid_stats:
                fastest = min(valid_stats.items(), key=lambda x: x[1]['avg_time'])
                most_efficient = min(valid_stats.items(), key=lambda x: x[1]['avg_nodes'])
                
                print(f"⚡ Più veloce: {fastest[0]} ({fastest[1]['avg_time']:.3f}s)")
                print(f"🎯 Più efficiente: {most_efficient[0]} ({most_efficient[1]['avg_nodes']:.0f} nodi)")
            
            # Algoritmi ottimali
            optimal_algos = ['astar', 'bfs']
            print(f"✅ Algoritmi con garanzia di ottimalità: {', '.join(optimal_algos)}")
        
        return stats
    
    def export_results(self, df: pd.DataFrame, stats: Dict, timestamp: str, run_dir: str = None):
        """
        Esporta tutti i risultati in vari formati.
        
        Args:
            df: DataFrame con i risultati
            stats: Statistiche aggregate
            timestamp: Timestamp per i nomi dei file
            run_dir: Cartella dove salvare i file (se None, usa self.results_dir)
        """
        try:
            print("\n💾 Esportazione risultati...")
            
            # Usa la cartella specificata o quella di default
            export_dir = run_dir if run_dir is not None else self.results_dir
            
            base_filename = f"test_{timestamp}"
            
            # 1. Esporta CSV con dati grezzi
            csv_file = os.path.join(export_dir, f"{base_filename}.csv")
            df.to_csv(csv_file, index=False, encoding='utf-8-sig')
            print(f"   ✅ CSV dati grezzi: {csv_file}")
            
            # 2. Esporta statistiche aggregate in JSON
            json_file = os.path.join(export_dir, f"{base_filename}_stats.json")
            
            # Converti infinity in stringa per JSON
            json_stats = {}
            for algo, algo_stats in stats.items():
                json_stats[algo] = {}
                for key, value in algo_stats.items():
                    if value == float('inf'):
                        json_stats[algo][key] = "infinity"
                    elif pd.isna(value):
                        json_stats[algo][key] = None
                    else:
                        json_stats[algo][key] = value
            
            with open(json_file, 'w', encoding='utf-8') as f:
                json.dump(json_stats, f, indent=2, default=str)
            print(f"   ✅ JSON statistiche: {json_file}")
            
            # 3. Crea report Markdown dettagliato
            md_file = os.path.join(export_dir, f"{base_filename}_report.md")
            self._create_markdown_report(df, stats, md_file, timestamp)
            print(f"   ✅ Report Markdown: {md_file}")
            
        except Exception as e:
            print(f"❌ Errore nell'esportazione: {e}")
            import traceback
            traceback.print_exc()
    
    def _create_markdown_report(self, df: pd.DataFrame, stats: Dict, 
                               filename: str, timestamp: str):
        """
        Crea un report dettagliato in formato Markdown.
        
        Args:
            df (pd.DataFrame): DataFrame con i risultati del test
            stats (Dict): Statistiche aggregate per algoritmo
            filename (str): Percorso del file Markdown da creare
            timestamp (str): Timestamp dell'esecuzione del test
            
        Genera un report completo con:
        - Sommario esecutivo con overview dei test
        - Tabelle comparative delle prestazioni
        - Analisi dettagliata per difficoltà
        - Conclusioni con identificazione del migliore algoritmo
        """
        with open(filename, 'w', encoding='utf-8') as f:
            f.write("# 🧩 8-Puzzle Test Report\n\n")
            f.write(f"**Data Esecuzione**: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
            f.write(f"**ID Report**: {timestamp}\n\n")
            
            # Sommario
            f.write("## 📊 Sommario Esecutivo\n\n")
            f.write(f"- **Puzzle testati per difficoltà**: {df['puzzle_id'].nunique()}\n")
            f.write(f"- **Difficoltà testate**: {', '.join(df['difficulty_category'].unique())}\n")
            f.write(f"- **Algoritmi testati**: {df['algorithm'].nunique()}\n")
            f.write(f"- **Test totali eseguiti**: {len(df)}\n")
            f.write(f"- **Test completati con successo**: {df['success'].sum()} ")
            f.write(f"({df['success'].mean()*100:.1f}%)\n\n")
            
            # Risultati per algoritmo
            f.write("## 🏆 Risultati Globali per Algoritmo\n\n")
            f.write("| Algoritmo | Success Rate | Tempo Medio | Mosse Medie | Nodi Esplorati |\n")
            f.write("|-----------|-------------|-------------|-------------|----------------|\n")
            
            for algo, stat in sorted(stats.items(), 
                                    key=lambda x: x[1]['avg_time'] if x[1]['avg_time'] != float('inf') else float('inf')):
                avg_time = "N/A" if stat['avg_time'] == float('inf') else f"{stat['avg_time']:.3f}s"
                avg_moves = "N/A" if stat['avg_moves'] == float('inf') else f"{stat['avg_moves']:.1f}"
                avg_nodes = "N/A" if stat['avg_nodes'] == float('inf') else f"{stat['avg_nodes']:.0f}"
                
                f.write(f"| **{algo}** | {stat['success_rate']:.1f}% | ")
                f.write(f"{avg_time} | {avg_moves} | {avg_nodes} |\n")
            
            # Analisi per difficoltà
            f.write("\n## 📈 Analisi per Difficoltà\n\n")
            
            success_df = df[df['success'] == True]
            
            for difficulty in df['difficulty_category'].unique():
                f.write(f"### {difficulty}\n\n")
                
                diff_df = df[df['difficulty_category'] == difficulty]
                diff_success = success_df[success_df['difficulty_category'] == difficulty]
                
                f.write(f"- Puzzle testati: {diff_df['puzzle_id'].nunique()}\n")
                f.write(f"- Test totali: {len(diff_df)}\n")
                f.write(f"- Success rate: {diff_df['success'].mean()*100:.1f}%\n\n")
                
                if len(diff_success) > 0:
                    f.write("| Algoritmo | Tempo Medio | Mosse | Nodi |\n")
                    f.write("|-----------|------------|-------|------|\n")
                    
                    for algo in df['algorithm'].unique():
                        algo_diff = diff_success[diff_success['algorithm'] == algo]
                        if len(algo_diff) > 0:
                            f.write(f"| {algo} | {algo_diff['time'].mean():.3f}s | ")
                            f.write(f"{algo_diff['path_length'].mean():.1f} | ")
                            f.write(f"{algo_diff['nodes_explored'].mean():.0f} |\n")
                
                f.write("\n")
            
            # Conclusioni
            f.write("## 🎯 Conclusioni\n\n")
            
            valid_stats = {k: v for k, v in stats.items() if v['avg_time'] != float('inf')}
            
            if valid_stats:
                fastest = min(valid_stats.items(), key=lambda x: x[1]['avg_time'])
                most_efficient = min(valid_stats.items(), key=lambda x: x[1]['avg_nodes'])
                
                f.write("### Migliori Prestazioni\n\n")
                f.write(f"- **⚡ Algoritmo più veloce**: {fastest[0]} ")
                f.write(f"(tempo medio: {fastest[1]['avg_time']:.3f}s)\n")
                f.write(f"- **🎯 Algoritmo più efficiente**: {most_efficient[0]} ")
                f.write(f"(nodi esplorati: {most_efficient[1]['avg_nodes']:.0f})\n")
            
            f.write("\n---\n")
            f.write("*Report generato automaticamente dal sistema di test 8-Puzzle*\n")


def main():
    """
    Funzione principale per eseguire il test completo degli algoritmi.
    
    Configura e avvia il sistema di test con parametri da command line,
    esegue il test completo su tutte le difficoltà, analizza i risultati
    e genera tutti i file di output (CSV, JSON, Markdown).
    
    Returns:
        Tuple[pd.DataFrame, Dict]: DataFrame dei risultati e statistiche aggregate,
                                  oppure (None, None) in caso di errore
    
    Note:
        Il test include un minimo di 10 puzzle per difficoltà per garantire
        significatività statistica dei risultati.
    """
    parser = argparse.ArgumentParser(
        description='🧩 Test completo per algoritmi dell\'8-Puzzle',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Il test testerà automaticamente:
  • N puzzle Easy
  • N puzzle Medium  
  • N puzzle Hard
  • N puzzle Mixed
  
Dove N è il numero specificato (default: 10, minimo: 10)

Tutti i risultati vengono automaticamente:
  • Salvati in CSV, JSON e Markdown
  • Analizzati con statistiche complete
        """
    )
    
    parser.add_argument(
        '--puzzles', '-p',
        type=int,
        default=10,
        help='Numero di puzzle da testare per ogni difficoltà (min: 10, default: 10)'
    )
    
    args = parser.parse_args()
    
    # Applica il minimo di 10 puzzle
    n_puzzles = max(10, args.puzzles)
    
    if args.puzzles < 10:
        print(f"⚠️ Numero minimo di puzzle è 10. Usando 10 invece di {args.puzzles}")
    
    print("=" * 60)
    print("🧩 8-PUZZLE test SUITE v2.0")
    print("=" * 60)
    print(f"📋 Configurazione:")
    print(f"  • Puzzle per difficoltà: {n_puzzles}")
    print(f"  • Difficoltà: Easy, Medium, Hard, Mixed")
    print(f"  • Totale puzzle: {n_puzzles * 4}")
    print(f"  • Algoritmi: A*, BFS, Greedy")
    print(f"  • Test totali: {n_puzzles * 4 * 3}")
    print("=" * 60)
    
    try:
        # Crea istanza del test
        test = PuzzleTest()
        
        # Timestamp per questa esecuzione
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        # Crea cartella specifica per questa esecuzione
        run_dir = test.create_run_directory(timestamp)
        
        # Esegui test completo
        results_df = test.run_complete_test(n_puzzles)
        
        # Analizza risultati
        stats = test.analyze_results(results_df)
        
        # Esporta sempre tutti i risultati
        test.export_results(results_df, stats, timestamp, run_dir)
        
        # Riepilogo finale
        print("\n" + "=" * 60)
        print("✅ TEST COMPLETATO CON SUCCESSO!")
        print("=" * 60)
        
        print(f"\n📁 Tutti i risultati sono stati salvati in:")
        print(f"   {run_dir}")
        
        print(f"\n📄 File generati:")
        files_created = [
            f"test_{timestamp}.csv",
            f"test_{timestamp}_stats.json",
            f"test_{timestamp}_report.md"
        ]
        
        for filename in files_created:
            filepath = os.path.join(run_dir, filename)
            if os.path.exists(filepath):
                size = os.path.getsize(filepath)
                print(f"   ✓ {filename} ({size:,} bytes)")
            else:
                print(f"   ✗ {filename} (non trovato)")
        
        # Mostra il best performer
        print("\n🏆 RISULTATO FINALE:")
        print("-" * 40)
        
        if stats:
            valid_stats = {k: v for k, v in stats.items() if v['avg_time'] != float('inf')}
            if valid_stats:
                best = min(valid_stats.items(), key=lambda x: x[1]['avg_time'])
                print(f"Miglior algoritmo overall: {best[0]}")
                print(f"  • Tempo medio: {best[1]['avg_time']:.3f}s")
                print(f"  • Mosse medie: {best[1]['avg_moves']:.1f}")
                print(f"  • Nodi esplorati: {best[1]['avg_nodes']:.0f}")
                print(f"  • Success rate: {best[1]['success_rate']:.1f}%")
        
        print("\n" + "=" * 60)
        print("📊 Il report completo è disponibile in:")
        print(f"   {os.path.join(run_dir, f'test_{timestamp}_report.md')}")
        print("=" * 60)
        
        return results_df, stats
        
    except KeyboardInterrupt:
        print("\n\n⚠️ Test interrotto dall'utente")
        return None, None
        
    except Exception as e:
        print(f"\n❌ Errore durante il test: {e}")
        import traceback
        traceback.print_exc()
        return None, None


if __name__ == "__main__":
    # Esegui il test
    df, stats = main()