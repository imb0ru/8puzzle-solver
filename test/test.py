"""
test.py - Test e test per confrontare gli algoritmi
=============================================================
Esegue test sistematici su tutti gli algoritmi implementati.
Esporta sempre risultati e grafici automaticamente.
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


class PuzzleTest:
    """Classe per eseguire test sugli algoritmi dell'8-puzzle."""
    
    def __init__(self):
        """Inizializza il test."""
        self.logic = PuzzleLogic(debug=False)
        self.results = []
        
        # Crea cartella results con percorso assoluto
        self.results_dir = os.path.join(
            os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 
            'results'
        )
        
        # Crea la directory se non esiste
        try:
            os.makedirs(self.results_dir, exist_ok=True)
            print(f"📁 Cartella risultati: {self.results_dir}")
            
            # Test di scrittura
            test_file = os.path.join(self.results_dir, '.test_write')
            with open(test_file, 'w') as f:
                f.write('test')
            os.remove(test_file)
            
        except Exception as e:
            print(f"⚠️ Errore con la cartella results: {e}")
            self.results_dir = os.getcwd()
            print(f"📁 Usando cartella corrente: {self.results_dir}")
    
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
                'astar_manhattan',
                'astar_misplaced',
                'astar_combined',
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
        
        print()  # Nuova riga dopo la progress bar
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
        print(f"  • Test totali: {n_puzzles * 4} puzzle × 5 algoritmi = {n_puzzles * 4 * 5} test")
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
        print(f"  • Puzzle unici testati: {df['puzzle_id'].nunique() * 4}")  # 4 difficoltà
        
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
            optimal_algos = ['astar_manhattan', 'astar_misplaced', 'astar_combined', 'bfs']
            print(f"✅ Algoritmi con garanzia di ottimalità: {', '.join(optimal_algos)}")
        
        return stats
    
    def create_plots(self, df: pd.DataFrame, timestamp: str):
        """
        Crea e salva tutti i grafici dei risultati.
        
        Args:
            df: DataFrame con i risultati
            timestamp: Timestamp per il nome del file
        """
        try:
            print("\n📊 Generazione grafici...")
            
            # Imposta stile
            sns.set_style("whitegrid")
            
            # Crea figura con subplots
            fig = plt.figure(figsize=(20, 12))
            
            # Titolo principale
            fig.suptitle('8-Puzzle Test Results - Analisi Completa', 
                        fontsize=18, fontweight='bold')
            
            # Filtra solo successi per alcuni grafici
            success_df = df[df['success'] == True].copy()
            
            if success_df.empty:
                print("⚠️ Nessun dato da visualizzare")
                plt.close()
                return
            
            # Colori per algoritmi
            algo_colors = {
                'astar_manhattan': '#2196F3',
                'astar_misplaced': '#4CAF50',
                'astar_combined': '#9C27B0',
                'bfs': '#F44336',
                'greedy': '#FFA726'
            }
            
            # 1. Tempo medio per algoritmo e difficoltà
            ax1 = plt.subplot(2, 3, 1)
            pivot_time = success_df.pivot_table(
                values='time', 
                index='algorithm', 
                columns='difficulty_category', 
                aggfunc='mean'
            )
            pivot_time.plot(kind='bar', ax=ax1, color=['#81C784', '#FFB74D', '#E57373', '#9575CD'])
            ax1.set_title('Tempo Medio per Algoritmo e Difficoltà', fontweight='bold')
            ax1.set_xlabel('Algoritmo')
            ax1.set_ylabel('Tempo (s)')
            ax1.legend(title='Difficoltà', loc='upper right')
            ax1.grid(True, alpha=0.3)
            plt.setp(ax1.xaxis.get_majorticklabels(), rotation=45, ha='right')
            
            # 2. Distribuzione mosse per difficoltà
            ax2 = plt.subplot(2, 3, 2)
            sns.boxplot(data=success_df, x='difficulty_category', y='path_length', 
                       hue='algorithm', ax=ax2, palette=algo_colors)
            ax2.set_title('Distribuzione Mosse per Difficoltà', fontweight='bold')
            ax2.set_xlabel('Difficoltà')
            ax2.set_ylabel('Numero di Mosse')
            ax2.legend(title='Algoritmo', bbox_to_anchor=(1.05, 1), loc='upper left')
            
            # 3. Nodi esplorati (scala log)
            ax3 = plt.subplot(2, 3, 3)
            for algo in success_df['algorithm'].unique():
                algo_data = success_df[success_df['algorithm'] == algo]
                ax3.scatter(algo_data['puzzle_manhattan'], algo_data['nodes_explored'],
                          label=algo, alpha=0.6, color=algo_colors.get(algo, '#333'),
                          s=30)
            ax3.set_title('Nodi Esplorati vs Complessità Puzzle', fontweight='bold')
            ax3.set_xlabel('Manhattan Distance Iniziale')
            ax3.set_ylabel('Nodi Esplorati (scala log)')
            ax3.set_yscale('log')
            ax3.legend(loc='upper left')
            ax3.grid(True, alpha=0.3)
            
            # 4. Success rate per difficoltà
            ax4 = plt.subplot(2, 3, 4)
            success_rates = df.pivot_table(
                values='success',
                index='algorithm',
                columns='difficulty_category',
                aggfunc='mean'
            ) * 100
            
            x = range(len(success_rates.index))
            width = 0.2
            difficulties = success_rates.columns
            
            for i, diff in enumerate(difficulties):
                offset = (i - len(difficulties)/2) * width + width/2
                ax4.bar([xi + offset for xi in x], success_rates[diff].values,
                       width, label=diff, alpha=0.8)
            
            ax4.set_title('Tasso di Successo per Algoritmo e Difficoltà', fontweight='bold')
            ax4.set_xlabel('Algoritmo')
            ax4.set_ylabel('Success Rate (%)')
            ax4.set_xticks(x)
            ax4.set_xticklabels(success_rates.index, rotation=45, ha='right')
            ax4.legend(title='Difficoltà')
            ax4.axhline(y=100, color='r', linestyle='--', alpha=0.3)
            ax4.grid(True, alpha=0.3)
            
            # 5. Efficienza (Tempo/Mossa)
            ax5 = plt.subplot(2, 3, 5)
            success_df['efficiency'] = success_df['time'] / success_df['path_length']
            efficiency_pivot = success_df.pivot_table(
                values='efficiency',
                index='algorithm',
                columns='difficulty_category',
                aggfunc='mean'
            )
            efficiency_pivot.plot(kind='bar', ax=ax5, 
                                 color=['#81C784', '#FFB74D', '#E57373', '#9575CD'])
            ax5.set_title('Efficienza Temporale (Tempo per Mossa)', fontweight='bold')
            ax5.set_xlabel('Algoritmo')
            ax5.set_ylabel('Tempo/Mossa (s)')
            ax5.legend(title='Difficoltà')
            ax5.grid(True, alpha=0.3)
            plt.setp(ax5.xaxis.get_majorticklabels(), rotation=45, ha='right')
            
            # 6. Confronto complessivo (radar chart simulato con bar)
            ax6 = plt.subplot(2, 3, 6)
            
            # Calcola metriche normalizzate per ogni algoritmo
            metrics_data = []
            for algo in success_df['algorithm'].unique():
                algo_data = success_df[success_df['algorithm'] == algo]
                metrics_data.append({
                    'Algorithm': algo,
                    'Speed': 1 / (algo_data['time'].mean() + 0.001),  # Inverso per avere più alto = meglio
                    'Optimality': 1 / (algo_data['path_length'].mean() + 1),
                    'Efficiency': 1 / (algo_data['nodes_explored'].mean() + 1)
                })
            
            metrics_df = pd.DataFrame(metrics_data)
            metrics_df.set_index('Algorithm')[['Speed', 'Optimality', 'Efficiency']].plot(
                kind='bar', ax=ax6, color=['#4CAF50', '#2196F3', '#FF9800']
            )
            ax6.set_title('Confronto Metriche Normalizzate', fontweight='bold')
            ax6.set_xlabel('Algoritmo')
            ax6.set_ylabel('Score (più alto = meglio)')
            ax6.legend(title='Metrica')
            ax6.grid(True, alpha=0.3)
            plt.setp(ax6.xaxis.get_majorticklabels(), rotation=45, ha='right')
            
            plt.tight_layout()
            
            # Salva il grafico
            plot_path = os.path.join(self.results_dir, f"test_plots_{timestamp}.png")
            plt.savefig(plot_path, dpi=150, bbox_inches='tight')
            print(f"   ✅ Grafici salvati in: {plot_path}")
            
            # Chiudi la figura per liberare memoria
            plt.close()
            
        except Exception as e:
            print(f"❌ Errore nella creazione dei grafici: {e}")
            import traceback
            traceback.print_exc()
    
    def export_results(self, df: pd.DataFrame, stats: Dict, timestamp: str):
        """
        Esporta tutti i risultati in vari formati.
        
        Args:
            df: DataFrame con i risultati
            stats: Statistiche aggregate
            timestamp: Timestamp per i nomi dei file
        """
        try:
            print("\n💾 Esportazione risultati...")
            
            base_filename = f"test_{timestamp}"
            
            # 1. Esporta CSV con dati grezzi
            csv_file = os.path.join(self.results_dir, f"{base_filename}.csv")
            df.to_csv(csv_file, index=False, encoding='utf-8-sig')
            print(f"   ✅ CSV dati grezzi: {csv_file}")
            
            # 2. Esporta statistiche aggregate in JSON
            json_file = os.path.join(self.results_dir, f"{base_filename}_stats.json")
            
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
            md_file = os.path.join(self.results_dir, f"{base_filename}_report.md")
            self._create_markdown_report(df, stats, md_file, timestamp)
            print(f"   ✅ Report Markdown: {md_file}")
            
            # 4. Esporta tabella riassuntiva per difficoltà
            summary_file = os.path.join(self.results_dir, f"{base_filename}_summary.csv")
            summary_df = df.groupby(['algorithm', 'difficulty_category']).agg({
                'success': 'mean',
                'time': 'mean',
                'path_length': 'mean',
                'nodes_explored': 'mean'
            }).round(3)
            summary_df.to_csv(summary_file)
            print(f"   ✅ CSV riassuntivo: {summary_file}")
            
        except Exception as e:
            print(f"❌ Errore nell'esportazione: {e}")
            import traceback
            traceback.print_exc()
    
    def _create_markdown_report(self, df: pd.DataFrame, stats: Dict, 
                               filename: str, timestamp: str):
        """Crea un report dettagliato in formato Markdown."""
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
                
                # Raccomandazioni
                f.write("\n### 💡 Raccomandazioni\n\n")
                
                optimal_algos = ['astar_manhattan', 'astar_misplaced', 'astar_combined', 'bfs']
                f.write("- **Per soluzioni ottimali garantite**: ")
                f.write(f"{', '.join([a for a in optimal_algos if a in valid_stats])}\n")
                
                if 'greedy' in valid_stats:
                    f.write("- **Per velocità massima** (ma senza garanzia di ottimalità): Greedy\n")
                
                if 'astar_combined' in valid_stats:
                    f.write("- **Miglior compromesso generale**: A* Combined\n")
            
            f.write("\n---\n")
            f.write("*Report generato automaticamente dal sistema di test 8-Puzzle*\n")


def main():
    """Funzione principale per eseguire il test."""
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
  • Visualizzati in grafici dettagliati
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
    print("🧩 8-PUZZLE TEST SUITE v2.0")
    print("=" * 60)
    print(f"📋 Configurazione:")
    print(f"  • Puzzle per difficoltà: {n_puzzles}")
    print(f"  • Difficoltà: Easy, Medium, Hard, Mixed")
    print(f"  • Totale puzzle: {n_puzzles * 4}")
    print(f"  • Algoritmi: A* Manhattan, A* Misplaced, A* Combined, BFS, Greedy")
    print(f"  • Test totali: {n_puzzles * 4 * 5}")
    print("=" * 60)
    
    # Timestamp per questa esecuzione
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    
    try:
        # Crea istanza del test
        test = PuzzleTest()

        # Esegui test completo
        results_df = test.run_complete_test(n_puzzles)
        
        # Analizza risultati
        stats = test.analyze_results(results_df)
        
        # Esporta sempre tutti i risultati
        test.export_results(results_df, stats, timestamp)
        
        # Crea sempre i grafici
        test.create_plots(results_df, timestamp)
        
        # Riepilogo finale
        print("\n" + "=" * 60)
        print("✅ TEST COMPLETATO CON SUCCESSO!")
        print("=" * 60)
        
        print(f"\n📁 Tutti i risultati sono stati salvati in:")
        print(f"   {test.results_dir}")

        print(f"\n📄 File generati:")
        files_created = [
            f"test_{timestamp}.csv",
            f"test_{timestamp}_stats.json",
            f"test_{timestamp}_report.md",
            f"test_{timestamp}_summary.csv",
            f"test_plots_{timestamp}.png"
        ]
        
        for filename in files_created:
            filepath = os.path.join(test.results_dir, filename)
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
        print("📊 Consulta il report completo in:")
        print(f"   {os.path.join(test.results_dir, f'test_{timestamp}_report.md')}")
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