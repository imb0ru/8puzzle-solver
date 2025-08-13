"""
benchmark.py - Test e benchmark per confrontare gli algoritmi
=============================================================
Esegue test sistematici su tutti gli algoritmi implementati.
"""

import sys
import os
import time
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from typing import List, Dict
import json
import argparse
from datetime import datetime

# Aggiungi path del progetto
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from logic.puzzle_logic import PuzzleLogic


class PuzzleBenchmark:
    """Classe per eseguire benchmark sugli algoritmi dell'8-puzzle."""
    
    def __init__(self, verbose=False):
        """
        Inizializza il benchmark.
        
        Args:
            verbose (bool): Output dettagliato
        """
        self.verbose = verbose
        self.logic = PuzzleLogic(debug=False)
        self.results = []
        
        # Crea cartella results se non esiste
        self.results_dir = os.path.join(
            os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 
            'results'
        )
        if not os.path.exists(self.results_dir):
            os.makedirs(self.results_dir)
        
    def generate_test_puzzles(self, count: int, difficulty: str = "mixed") -> List[List[int]]:
        """
        Genera puzzle di test.
        
        Args:
            count: Numero di puzzle da generare
            difficulty: "easy", "medium", "hard", o "mixed"
            
        Returns:
            Lista di puzzle
        """
        puzzles = []
        
        if difficulty == "mixed":
            # 1/3 per ogni difficoltà
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
            for _ in range(count):
                puzzles.append(self.logic.generate_random_puzzle(difficulty))
        
        return puzzles
    
    def run_benchmark(self, puzzles: List[List[int]], 
                     algorithms: List[str] = None) -> pd.DataFrame:
        """
        Esegue il benchmark su un set di puzzle.
        
        Args:
            puzzles: Lista di puzzle da testare
            algorithms: Algoritmi da testare (default: tutti)
            
        Returns:
            DataFrame con i risultati
        """
        if algorithms is None:
            algorithms = [
                'astar_manhattan',
                'astar_misplaced',
                'astar_combined',
                'bfs',
                'greedy',
            ]
        
        print(f"🚀 Avvio benchmark su {len(puzzles)} puzzle con {len(algorithms)} algoritmi")
        print("=" * 60)
        
        results = []
        total_tests = len(puzzles) * len(algorithms)
        current_test = 0
        
        for puzzle_idx, puzzle in enumerate(puzzles):
            # Calcola difficoltà del puzzle
            manhattan = self.logic.get_manhattan_distance(puzzle)
            
            if self.verbose:
                print(f"\n📊 Puzzle {puzzle_idx + 1}/{len(puzzles)} (Manhattan: {manhattan})")
            
            for algo in algorithms:
                current_test += 1
                progress = (current_test / total_tests) * 100
                
                if not self.verbose:
                    print(f"\rProgresso: {progress:.1f}%", end="", flush=True)
                
                try:
                    start_time = time.time()
                    result = self.logic.solve(puzzle, algo)
                    solve_time = time.time() - start_time
                    
                    if result['success']:
                        results.append({
                            'puzzle_id': puzzle_idx,
                            'algorithm': algo,
                            'success': True,
                            'time': solve_time,
                            'path_length': result.get('path_length', len(result['path']) - 1),
                            'nodes_explored': result.get('nodes_explored', 0),
                            'nodes_frontier': result.get('nodes_frontier', 0),
                            'memory': result.get('memory', 0),
                            'optimal': result.get('optimal', False),
                            'puzzle_difficulty': manhattan
                        })
                        
                        if self.verbose:
                            print(f"  ✅ {algo:20} | Time: {solve_time:.3f}s | "
                                 f"Moves: {result.get('path_length', len(result['path'])-1)} | "
                                 f"Nodes: {result.get('nodes_explored', 0)}")
                    else:
                        results.append({
                            'puzzle_id': puzzle_idx,
                            'algorithm': algo,
                            'success': False,
                            'time': solve_time,
                            'path_length': None,
                            'nodes_explored': None,
                            'nodes_frontier': None,
                            'memory': None,
                            'optimal': None,
                            'puzzle_difficulty': manhattan
                        })
                        
                        if self.verbose:
                            print(f"  ❌ {algo:20} | FAILED")
                            
                except Exception as e:
                    results.append({
                        'puzzle_id': puzzle_idx,
                        'algorithm': algo,
                        'success': False,
                        'error': str(e),
                        'puzzle_difficulty': manhattan
                    })
                    
                    if self.verbose:
                        print(f"  💥 {algo:20} | ERROR: {e}")
        
        if not self.verbose:
            print()  # Nuova riga dopo la progress bar
        
        print("\n" + "=" * 60)
        print("✅ Benchmark completato!")
        
        return pd.DataFrame(results)
    
    def analyze_results(self, df: pd.DataFrame) -> Dict:
        """
        Analizza i risultati del benchmark.
        
        Args:
            df: DataFrame con i risultati
            
        Returns:
            Dizionario con statistiche aggregate
        """
        print("\n📊 ANALISI RISULTATI")
        print("=" * 60)
        
        # Filtra solo i successi per le statistiche
        success_df = df[df['success'] == True].copy()
        
        # Statistiche per algoritmo
        stats = {}
        
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
                avg_memory = algo_success['memory'].mean() if 'memory' in algo_success else 0
                optimal_rate = (algo_success['optimal'].sum() / len(algo_success) * 100) if 'optimal' in algo_success.columns else 0
                
                stats[algo] = {
                    'success_rate': success_rate,
                    'avg_time': avg_time,
                    'avg_moves': avg_moves,
                    'avg_nodes': avg_nodes,
                    'avg_memory': avg_memory,
                    'optimal_rate': optimal_rate,
                    'min_time': algo_success['time'].min(),
                    'max_time': algo_success['time'].max(),
                    'std_time': algo_success['time'].std()
                }
            else:
                stats[algo] = {
                    'success_rate': 0,
                    'avg_time': float('inf'),
                    'avg_moves': float('inf'),
                    'avg_nodes': float('inf'),
                    'avg_memory': float('inf'),
                    'optimal_rate': 0
                }
        
        # Stampa tabella riassuntiva
        print("\n📈 STATISTICHE PER ALGORITMO:")
        print("-" * 90)
        print(f"{'Algoritmo':<20} {'Success%':<10} {'Avg Time':<12} {'Avg Moves':<10} {'Avg Nodes':<12} {'Optimal%':<10}")
        print("-" * 90)
        
        for algo, stat in sorted(stats.items(), key=lambda x: x[1]['avg_time']):
            print(f"{algo:<20} {stat['success_rate']:>8.1f}% "
                 f"{stat['avg_time']:>10.3f}s "
                 f"{stat['avg_moves']:>9.1f} "
                 f"{stat['avg_nodes']:>11.0f} "
                 f"{stat['optimal_rate']:>8.1f}%")
        
        # Trova il migliore per ogni metrica
        print("\n🏆 MIGLIORI PRESTAZIONI:")
        print("-" * 40)
        
        if success_df.empty:
            print("Nessun algoritmo ha risolto i puzzle!")
        else:
            fastest = min(stats.items(), key=lambda x: x[1]['avg_time'] if x[1]['avg_time'] != float('inf') else float('inf'))
            print(f"⚡ Più veloce: {fastest[0]} ({fastest[1]['avg_time']:.3f}s)")
            
            most_efficient = min(stats.items(), key=lambda x: x[1]['avg_nodes'] if x[1]['avg_nodes'] != float('inf') else float('inf'))
            print(f"🎯 Più efficiente: {most_efficient[0]} ({most_efficient[1]['avg_nodes']:.0f} nodi)")
            
            # Algoritmi ottimali
            optimal_algos = [algo for algo, stat in stats.items() 
                           if stat['optimal_rate'] >= 99]  # Considera ottimali se >= 99%
            if optimal_algos:
                print(f"✅ Ottimali: {', '.join(optimal_algos)}")
        
        return stats
    
    def plot_results(self, df: pd.DataFrame, save_path: str = None):
        """
        Crea grafici dei risultati.
        
        Args:
            df: DataFrame con i risultati
            save_path: Path dove salvare i grafici (se None, salva in results/)
        """
        # Imposta stile
        sns.set_style("whitegrid")
        plt.rcParams['figure.figsize'] = (15, 10)
        
        # Se non specificato, salva nella cartella results
        if save_path is None:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            save_path = os.path.join(self.results_dir, f"benchmark_plots_{timestamp}.png")
        
        # Crea subplots
        fig, axes = plt.subplots(2, 3, figsize=(18, 12))
        fig.suptitle('8-Puzzle Algorithm Benchmark Results', fontsize=16, fontweight='bold')
        
        # Filtra solo successi
        success_df = df[df['success'] == True].copy()
        
        if success_df.empty:
            print("⚠️ Nessun dato da visualizzare")
            plt.close()
            return
        
        # Colori per algoritmi
        algo_colors = {
            'astar_manhattan': '#2196F3',
            'astar_misplaced': '#4CAF50',
            'astar_linear': '#FF9800',
            'astar_combined': '#9C27B0',
            'bfs': '#F44336',
            'greedy': '#795548'
        }
        
        # 1. Tempo di esecuzione per algoritmo
        ax1 = axes[0, 0]
        sns.boxplot(data=success_df, x='algorithm', y='time', ax=ax1,
                   palette=[algo_colors.get(a, '#333') for a in success_df['algorithm'].unique()])
        ax1.set_title('Tempo di Esecuzione', fontsize=12, fontweight='bold')
        ax1.set_xlabel('Algoritmo')
        ax1.set_ylabel('Tempo (s)')
        ax1.tick_params(axis='x', rotation=45)
        
        # 2. Lunghezza del percorso
        ax2 = axes[0, 1]
        sns.boxplot(data=success_df, x='algorithm', y='path_length', ax=ax2,
                   palette=[algo_colors.get(a, '#333') for a in success_df['algorithm'].unique()])
        ax2.set_title('Lunghezza del Percorso', fontsize=12, fontweight='bold')
        ax2.set_xlabel('Algoritmo')
        ax2.set_ylabel('Numero di Mosse')
        ax2.tick_params(axis='x', rotation=45)
        
        # 3. Nodi esplorati (scala logaritmica)
        ax3 = axes[0, 2]
        sns.boxplot(data=success_df, x='algorithm', y='nodes_explored', ax=ax3,
                   palette=[algo_colors.get(a, '#333') for a in success_df['algorithm'].unique()])
        ax3.set_title('Nodi Esplorati (scala log)', fontsize=12, fontweight='bold')
        ax3.set_xlabel('Algoritmo')
        ax3.set_ylabel('Numero di Nodi (log)')
        ax3.tick_params(axis='x', rotation=45)
        ax3.set_yscale('log')
        
        # 4. Success rate
        ax4 = axes[1, 0]
        success_rates = df.groupby('algorithm')['success'].mean() * 100
        
        x_pos = range(len(success_rates))
        bars = ax4.bar(x_pos, success_rates.values, 
                      color=[algo_colors.get(a, '#333') for a in success_rates.index],
                      alpha=0.8)
        
        ax4.set_title('Tasso di Successo', fontsize=12, fontweight='bold')
        ax4.set_xlabel('Algoritmo')
        ax4.set_ylabel('Percentuale (%)')
        ax4.set_xticks(x_pos)
        ax4.set_xticklabels(success_rates.index, rotation=45)
        ax4.axhline(y=100, color='r', linestyle='--', alpha=0.3)
        
        # Aggiungi valori sopra le barre
        for bar, val in zip(bars, success_rates.values):
            ax4.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 1,
                    f'{val:.1f}%', ha='center', va='bottom', fontsize=8)
        
        # 5. Tempo vs Difficoltà
        ax5 = axes[1, 1]
        for algo in success_df['algorithm'].unique():
            algo_data = success_df[success_df['algorithm'] == algo]
            ax5.scatter(algo_data['puzzle_difficulty'], algo_data['time'], 
                       label=algo, alpha=0.6, color=algo_colors.get(algo, '#333'))
        ax5.set_title('Tempo vs Difficoltà del Puzzle', fontsize=12, fontweight='bold')
        ax5.set_xlabel('Difficoltà (Manhattan Distance iniziale)')
        ax5.set_ylabel('Tempo (s)')
        ax5.legend(bbox_to_anchor=(1.05, 1), loc='upper left', fontsize=8)
        
        # 6. Efficienza (Nodi/Mossa)
        ax6 = axes[1, 2]
        success_df['efficiency'] = success_df['nodes_explored'] / success_df['path_length']
        efficiency_means = success_df.groupby('algorithm')['efficiency'].mean()
        
        bars = ax6.bar(range(len(efficiency_means)), efficiency_means.values,
                      color=[algo_colors.get(a, '#333') for a in efficiency_means.index])
        ax6.set_title('Efficienza Media (Nodi per Mossa)', fontsize=12, fontweight='bold')
        ax6.set_xlabel('Algoritmo')
        ax6.set_ylabel('Nodi / Mossa')
        ax6.set_xticks(range(len(efficiency_means)))
        ax6.set_xticklabels(efficiency_means.index, rotation=45)
        
        # Aggiungi valori sopra le barre
        for i, (bar, val) in enumerate(zip(bars, efficiency_means.values)):
            ax6.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.5,
                    f'{val:.1f}', ha='center', va='bottom', fontsize=8)
        
        plt.tight_layout()
        
        # Salva il grafico
        try:
            plt.savefig(save_path, dpi=150, bbox_inches='tight')
            print(f"📊 Grafici salvati in: {save_path}")
        except Exception as e:
            print(f"❌ Errore nel salvare i grafici: {e}")
        
        plt.show()
    
    def export_results(self, df: pd.DataFrame, stats: Dict, 
                      base_filename: str = None):
        """
        Esporta i risultati in vari formati nella cartella results.
        
        Args:
            df: DataFrame con i risultati
            stats: Statistiche aggregate
            base_filename: Nome base per i file (se None, usa timestamp)
        """
        # Genera nome file con timestamp se non specificato
        if base_filename is None:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            base_filename = f"benchmark_{timestamp}"
        
        # Path completi nella cartella results
        csv_file = os.path.join(self.results_dir, f"{base_filename}.csv")
        json_file = os.path.join(self.results_dir, f"{base_filename}_stats.json")
        md_file = os.path.join(self.results_dir, f"{base_filename}_report.md")
        
        # Esporta CSV dettagliato (gestisci encoding per caratteri speciali)
        df_export = df.copy()
        # Sostituisci valori booleani con stringhe per compatibilità
        if 'optimal' in df_export.columns:
            df_export['optimal'] = df_export['optimal'].map({True: 'SI', False: 'NO', None: 'N/A'})
        df_export.to_csv(csv_file, index=False, encoding='utf-8-sig')
        print(f"💾 Risultati dettagliati salvati in: {csv_file}")
        
        # Esporta statistiche in JSON
        with open(json_file, 'w', encoding='utf-8') as f:
            json.dump(stats, f, indent=2, default=str)
        print(f"💾 Statistiche salvate in: {json_file}")
        
        # Crea report markdown
        self._create_markdown_report(df, stats, md_file)
        print(f"💾 Report salvato in: {md_file}")
    
    def _create_markdown_report(self, df: pd.DataFrame, stats: Dict, filename: str):
        """Crea un report in formato Markdown."""
        with open(filename, 'w', encoding='utf-8') as f:
            f.write("# 8-Puzzle Benchmark Report\n\n")
            f.write(f"**Data**: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
            
            f.write("## Sommario\n\n")
            f.write(f"- **Puzzle testati**: {df['puzzle_id'].nunique()}\n")
            f.write(f"- **Algoritmi testati**: {df['algorithm'].nunique()}\n")
            f.write(f"- **Test totali**: {len(df)}\n")
            f.write(f"- **Test riusciti**: {df['success'].sum()}\n")
            f.write(f"- **Tasso successo globale**: {df['success'].mean()*100:.1f}%\n\n")
            
            f.write("## Risultati per Algoritmo\n\n")
            f.write("| Algoritmo | Success% | Avg Time (s) | Avg Moves | Avg Nodes | Optimal% |\n")
            f.write("|-----------|----------|--------------|-----------|-----------|----------|\n")
            
            for algo, stat in sorted(stats.items(), key=lambda x: x[1]['avg_time']):
                f.write(f"| {algo} | {stat['success_rate']:.1f}% | "
                       f"{stat['avg_time']:.3f} | {stat['avg_moves']:.1f} | "
                       f"{stat['avg_nodes']:.0f} | {stat.get('optimal_rate', 0):.1f}% |\n")
            
            f.write("\n## Analisi Dettagliata\n\n")
            
            # Analisi per difficoltà
            if 'puzzle_difficulty' in df.columns:
                f.write("### Prestazioni per Difficoltà\n\n")
                difficulty_groups = df.groupby('puzzle_difficulty')
                f.write("| Manhattan Distance | Puzzle Count | Avg Success Rate |\n")
                f.write("|-------------------|--------------|------------------|\n")
                for diff, group in difficulty_groups:
                    success_rate = group['success'].mean() * 100
                    f.write(f"| {diff} | {len(group)} | {success_rate:.1f}% |\n")
            
            f.write("\n## Conclusioni\n\n")
            
            # Trova migliori
            if stats:
                fastest = min(stats.items(), 
                            key=lambda x: x[1]['avg_time'] if x[1]['avg_time'] != float('inf') else float('inf'))
                most_efficient = min(stats.items(), 
                                   key=lambda x: x[1]['avg_nodes'] if x[1]['avg_nodes'] != float('inf') else float('inf'))
                
                f.write(f"- **Algoritmo più veloce**: {fastest[0]} ({fastest[1]['avg_time']:.3f}s)\n")
                f.write(f"- **Algoritmo più efficiente**: {most_efficient[0]} ")
                f.write(f"({most_efficient[1]['avg_nodes']:.0f} nodi esplorati)\n")
                
                # Algoritmi ottimali
                optimal_algos = [algo for algo, stat in stats.items() 
                               if stat.get('optimal_rate', 0) >= 99]
                if optimal_algos:
                    f.write(f"- **Algoritmi ottimali**: {', '.join(optimal_algos)}\n")
            


def main():
    """Funzione principale per eseguire il benchmark."""
    parser = argparse.ArgumentParser(
        description='Benchmark per algoritmi dell\'8-Puzzle',
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    
    parser.add_argument(
        '--puzzles', '-p',
        type=int,
        default=10,
        help='Numero di puzzle da testare (default: 10)'
    )
    
    parser.add_argument(
        '--difficulty', '-d',
        choices=['easy', 'medium', 'hard', 'mixed'],
        default='mixed',
        help='Difficoltà dei puzzle (default: mixed)'
    )
    
    parser.add_argument(
        '--algorithms', '-a',
        nargs='+',
        choices=['astar_manhattan', 'astar_misplaced',
                'astar_combined', 'bfs', 'greedy', 'all'],
        default=['all'],
        help='Algoritmi da testare (default: all)'
    )
    
    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Output dettagliato'
    )
    
    parser.add_argument(
        '--export', '-e',
        action='store_true',
        help='Esporta risultati in file nella cartella results/'
    )
    
    parser.add_argument(
        '--plot', 
        action='store_true',
        help='Mostra grafici dei risultati'
    )
    
    parser.add_argument(
        '--iterations', '-i',
        type=int,
        default=1,
        help='Numero di iterazioni per puzzle (default: 1)'
    )
    
    args = parser.parse_args()
    
    # Gestisci algoritmi
    if 'all' in args.algorithms:
        algorithms = None  # Usa tutti
    else:
        algorithms = args.algorithms
    
    print("=" * 60)
    print("🧩 8-PUZZLE BENCHMARK SUITE")
    print("=" * 60)
    print(f"Configurazione:")
    print(f"  • Puzzle: {args.puzzles}")
    print(f"  • Difficoltà: {args.difficulty}")
    print(f"  • Algoritmi: {algorithms if algorithms else 'Tutti'}")
    print(f"  • Iterazioni: {args.iterations}")
    print("=" * 60)
    
    # Crea benchmark
    benchmark = PuzzleBenchmark(verbose=args.verbose)
    
    # Genera puzzle
    print("\n🎲 Generazione puzzle di test...")
    puzzles = benchmark.generate_test_puzzles(args.puzzles, args.difficulty)
    
    # Esegui test multipli se richiesto
    all_results = []
    
    for iteration in range(args.iterations):
        if args.iterations > 1:
            print(f"\n🔄 Iterazione {iteration + 1}/{args.iterations}")
        
        # Esegui benchmark
        results_df = benchmark.run_benchmark(puzzles, algorithms)
        results_df['iteration'] = iteration
        all_results.append(results_df)
    
    # Combina risultati
    final_df = pd.concat(all_results, ignore_index=True)
    
    # Analizza risultati
    stats = benchmark.analyze_results(final_df)
    
    # Visualizza grafici
    if args.plot:
        print("\n📊 Generazione grafici...")
        benchmark.plot_results(final_df)
    
    # Esporta risultati (sempre nella cartella results)
    if args.export:
        print("\n💾 Esportazione risultati nella cartella results/...")
        benchmark.export_results(final_df, stats)
    
    print("\n" + "=" * 60)
    print("✅ Benchmark completato con successo!")
    print(f"📁 I risultati sono disponibili nella cartella: {benchmark.results_dir}")
    print("=" * 60)
    
    return final_df, stats


if __name__ == "__main__":
    try:
        df, stats = main()
    except KeyboardInterrupt:
        print("\n\n⚠️ Benchmark interrotto dall'utente")
    except Exception as e:
        print(f"\n❌ Errore: {e}")
        import traceback
        traceback.print_exc()