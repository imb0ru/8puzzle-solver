"""
puzzle_gui.py - Interfaccia grafica per l'8-Puzzle Solver
==========================================================
GUI interattiva con animazioni, statistiche e confronto algoritmi.
"""

import tkinter as tk
from tkinter import ttk, messagebox, scrolledtext
import time
import threading
from collections import deque
import random


class PuzzleGUI:
    """Interfaccia grafica principale per l'8-Puzzle Solver."""
    
    # Costanti grafiche
    TILE_SIZE = 100
    TILE_FONT = ("Arial", 36, "bold")
    TILE_COLORS = {
        'bg': "#2196F3",
        'fg': "white",
        'empty': "#E0E0E0",
        'hover': "#1976D2",
        'solving': "#4CAF50",
        'path': "#FFC107"
    }
    
    def __init__(self, master, logic, debug=False):
        """
        Inizializza la GUI.
        
        Args:
            master: Finestra Tkinter principale
            logic: Istanza di PuzzleLogic
            debug: Modalità debug
        """
        self.master = master
        self.logic = logic
        self.debug = debug
        
        # Stato del puzzle
        self.current_state = [1, 2, 3, 4, 5, 6, 7, 8, 0]
        self.goal_state = [1, 2, 3, 4, 5, 6, 7, 8, 0]
        self.tiles = {}
        self.solution_path = []
        self.is_solving = False
        self.animation_speed = 500  # ms
        
        # Statistiche
        self.stats = {
            'moves': 0,
            'time': 0,
            'nodes_explored': 0,
            'nodes_frontier': 0,
            'memory': 0
        }
        
        self._setup_ui()
        self._create_puzzle_board()
        self._bind_events()
        self.update_display()
        
    def _setup_ui(self):
        """Costruisce l'interfaccia utente."""
        # Stile
        style = ttk.Style()
        style.theme_use('clam')
        
        # Frame principale con layout a 3 colonne
        main_frame = ttk.Frame(self.master, padding="10")
        main_frame.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))
        
        # Configura peso delle colonne
        self.master.columnconfigure(0, weight=1)
        self.master.rowconfigure(0, weight=1)
        main_frame.columnconfigure(1, weight=3)  # Colonna centrale più grande
        
        # ========== COLONNA SINISTRA: Controlli ==========
        left_frame = ttk.LabelFrame(main_frame, text="🎮 Controlli", padding="10")
        left_frame.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S), padx=(0, 5))
        
        # Sezione configurazione
        ttk.Label(left_frame, text="📝 Configurazione:", font=("Arial", 10, "bold")).pack(anchor="w", pady=(0, 5))
        
        config_frame = ttk.Frame(left_frame)
        config_frame.pack(fill="x", pady=5)
        
        ttk.Button(config_frame, text="🎲 Random", 
                  command=self.shuffle_puzzle).pack(side="left", padx=2)
        ttk.Button(config_frame, text="🎯 Goal", 
                  command=self.set_goal_state).pack(side="left", padx=2)
        ttk.Button(config_frame, text="✏️ Modifica", 
                  command=self.enable_edit_mode).pack(side="left", padx=2)
        
        ttk.Separator(left_frame, orient="horizontal").pack(fill="x", pady=10)
        
        # Sezione algoritmi
        ttk.Label(left_frame, text="🧠 Algoritmo:", font=("Arial", 10, "bold")).pack(anchor="w", pady=(0, 5))
        
        self.algorithm_var = tk.StringVar(value="astar_manhattan")
        algorithms = [
            ("A* (Manhattan)", "astar_manhattan"),
            ("A* (Misplaced)", "astar_misplaced"),
            ("A* (Linear Conflict)", "astar_linear"),
            ("BFS", "bfs"),
            ("Greedy Best-First", "greedy")
        ]
        
        for text, value in algorithms:
            ttk.Radiobutton(left_frame, text=text, variable=self.algorithm_var, 
                           value=value).pack(anchor="w", padx=20)
        
        ttk.Separator(left_frame, orient="horizontal").pack(fill="x", pady=10)
        
        # Sezione azioni
        ttk.Label(left_frame, text="🚀 Azioni:", font=("Arial", 10, "bold")).pack(anchor="w", pady=(0, 5))
        
        self.solve_button = ttk.Button(left_frame, text="🔍 Risolvi", 
                                      command=self.solve_puzzle)
        self.solve_button.pack(fill="x", pady=2)
        
        self.stop_button = ttk.Button(left_frame, text="⏹️ Stop", 
                                     command=self.stop_solving, state="disabled")
        self.stop_button.pack(fill="x", pady=2)
        
        ttk.Button(left_frame, text="📊 Confronta Algoritmi", 
                  command=self.compare_algorithms).pack(fill="x", pady=2)
        
        ttk.Separator(left_frame, orient="horizontal").pack(fill="x", pady=10)
        
        # Sezione velocità animazione
        ttk.Label(left_frame, text="⚡ Velocità animazione:", font=("Arial", 10, "bold")).pack(anchor="w", pady=(0, 5))
        
        self.speed_var = tk.IntVar(value=500)
        speed_frame = ttk.Frame(left_frame)
        speed_frame.pack(fill="x", pady=5)
        
        ttk.Scale(speed_frame, from_=100, to=2000, variable=self.speed_var,
                 orient="horizontal", command=self.update_speed).pack(side="left", fill="x", expand=True)
        self.speed_label = ttk.Label(speed_frame, text="500 ms")
        self.speed_label.pack(side="right", padx=5)
        
        # ========== COLONNA CENTRALE: Puzzle Board ==========
        center_frame = ttk.LabelFrame(main_frame, text="🧩 8-Puzzle", padding="20")
        center_frame.grid(row=0, column=1, sticky=(tk.W, tk.E, tk.N, tk.S))
        
        # Canvas per il puzzle
        self.canvas = tk.Canvas(center_frame, width=320, height=320, bg="white", 
                               highlightthickness=2, highlightbackground="#333")
        self.canvas.pack()
        
        # Frame per info sotto il puzzle
        info_frame = ttk.Frame(center_frame)
        info_frame.pack(fill="x", pady=10)
        
        self.info_label = ttk.Label(info_frame, text="Pronto per iniziare!", 
                                   font=("Arial", 11))
        self.info_label.pack()
        
        # Progress bar
        self.progress = ttk.Progressbar(center_frame, mode='indeterminate')
        self.progress.pack(fill="x", pady=5)
        
        # ========== COLONNA DESTRA: Statistiche e Log ==========
        right_frame = ttk.Frame(main_frame)
        right_frame.grid(row=0, column=2, sticky=(tk.W, tk.E, tk.N, tk.S), padx=(5, 0))
        
        # Frame statistiche
        stats_frame = ttk.LabelFrame(right_frame, text="📊 Statistiche", padding="10")
        stats_frame.pack(fill="x", pady=(0, 5))
        
        self.stats_labels = {}
        stats_items = [
            ("Mosse:", "moves"),
            ("Tempo:", "time"),
            ("Nodi esplorati:", "nodes_explored"),
            ("Nodi in frontiera:", "nodes_frontier"),
            ("Memoria:", "memory")
        ]
        
        for i, (label, key) in enumerate(stats_items):
            ttk.Label(stats_frame, text=label, font=("Arial", 9)).grid(row=i, column=0, sticky="w", padx=5, pady=2)
            self.stats_labels[key] = ttk.Label(stats_frame, text="0", font=("Courier", 9))
            self.stats_labels[key].grid(row=i, column=1, sticky="e", padx=5, pady=2)
        
        # Console log
        log_frame = ttk.LabelFrame(right_frame, text="📝 Log", padding="5")
        log_frame.pack(fill="both", expand=True)
        
        self.console = scrolledtext.ScrolledText(log_frame, height=20, width=40, 
                                                font=("Courier", 9), 
                                                bg="#1e1e1e", fg="#00ff00")
        self.console.pack(fill="both", expand=True)
        
        self.log("🧩 8-Puzzle AI Solver avviato")
        self.log("Seleziona un algoritmo e clicca 'Risolvi'")
    
    def _create_puzzle_board(self):
        """Crea le tessere del puzzle sulla canvas."""
        for i in range(9):
            row = i // 3
            col = i % 3
            x = col * self.TILE_SIZE + 10
            y = row * self.TILE_SIZE + 10
            
            # Crea rettangolo per la tessera
            tile_id = self.canvas.create_rectangle(
                x, y, x + self.TILE_SIZE, y + self.TILE_SIZE,
                fill=self.TILE_COLORS['bg'], outline="white", width=2
            )
            
            # Crea testo per il numero
            text_id = self.canvas.create_text(
                x + self.TILE_SIZE // 2, y + self.TILE_SIZE // 2,
                text="", font=self.TILE_FONT, fill=self.TILE_COLORS['fg']
            )
            
            self.tiles[i] = {'rect': tile_id, 'text': text_id, 'pos': i}
    
    def _bind_events(self):
        """Associa gli eventi della GUI."""
        # Keyboard shortcuts
        self.master.bind('<F1>', lambda e: self.show_help())
        self.master.bind('<Escape>', lambda e: self.master.quit())
        self.master.bind('<r>', lambda e: self.shuffle_puzzle())
        self.master.bind('<s>', lambda e: self.solve_puzzle())
        self.master.bind('<space>', lambda e: self.solve_puzzle())
        
        # Mouse events sul canvas
        self.canvas.bind('<Button-1>', self.on_tile_click)
        self.canvas.bind('<Motion>', self.on_mouse_motion)
    
    def update_display(self):
        """Aggiorna la visualizzazione del puzzle."""
        for i in range(9):
            value = self.current_state[i]
            tile = self.tiles[i]
            
            if value == 0:
                # Tessera vuota
                self.canvas.itemconfig(tile['rect'], fill=self.TILE_COLORS['empty'])
                self.canvas.itemconfig(tile['text'], text="")
            else:
                # Tessera numerata
                self.canvas.itemconfig(tile['rect'], fill=self.TILE_COLORS['bg'])
                self.canvas.itemconfig(tile['text'], text=str(value))
        
        self.canvas.update()
    
    def shuffle_puzzle(self):
        """Mescola il puzzle in modo casuale ma risolvibile."""
        if self.is_solving:
            return
        
        self.log("🎲 Mescolamento puzzle...")
        self.current_state = self.logic.generate_random_puzzle()
        self.update_display()
        self.reset_stats()
        self.log(f"✅ Puzzle mescolato: {self._format_state(self.current_state)}")
    
    def set_goal_state(self):
        """Imposta il puzzle nello stato obiettivo."""
        if self.is_solving:
            return
        
        self.current_state = self.goal_state.copy()
        self.update_display()
        self.reset_stats()
        self.log("🎯 Puzzle impostato allo stato obiettivo")
    
    def enable_edit_mode(self):
        """Abilita la modalità di modifica manuale."""
        if self.is_solving:
            return
        
        # Implementazione semplificata - usa un dialog per input
        dialog = EditDialog(self.master, self.current_state)
        self.master.wait_window(dialog.top)
        
        if dialog.result:
            self.current_state = dialog.result
            self.update_display()
            self.log("✏️ Configurazione modificata manualmente")
    
    def on_tile_click(self, event):
        """Gestisce il click su una tessera."""
        if self.is_solving:
            return
        
        # Trova quale tessera è stata cliccata
        col = (event.x - 10) // self.TILE_SIZE
        row = (event.y - 10) // self.TILE_SIZE
        
        if 0 <= row < 3 and 0 <= col < 3:
            clicked_pos = row * 3 + col
            
            # Trova posizione dello spazio vuoto
            empty_pos = self.current_state.index(0)
            
            # Verifica se la mossa è valida
            if self.is_adjacent(clicked_pos, empty_pos):
                # Scambia le tessere
                self.current_state[empty_pos], self.current_state[clicked_pos] = \
                    self.current_state[clicked_pos], self.current_state[empty_pos]
                
                self.update_display()
                self.stats['moves'] += 1
                self.update_stats_display()
                
                # Verifica se il puzzle è risolto
                if self.current_state == self.goal_state:
                    self.log("🎉 Puzzle risolto manualmente!")
                    messagebox.showinfo("Complimenti!", 
                                      f"Hai risolto il puzzle in {self.stats['moves']} mosse!")
    
    def on_mouse_motion(self, event):
        """Gestisce il movimento del mouse sul canvas."""
        if self.is_solving:
            return
        
        # Evidenzia la tessera sotto il mouse
        col = (event.x - 10) // self.TILE_SIZE
        row = (event.y - 10) // self.TILE_SIZE
        
        if 0 <= row < 3 and 0 <= col < 3:
            pos = row * 3 + col
            empty_pos = self.current_state.index(0)
            
            # Reset colori
            for i in range(9):
                if self.current_state[i] != 0:
                    self.canvas.itemconfig(self.tiles[i]['rect'], 
                                         fill=self.TILE_COLORS['bg'])
            
            # Evidenzia se adiacente allo spazio vuoto
            if self.is_adjacent(pos, empty_pos) and self.current_state[pos] != 0:
                self.canvas.itemconfig(self.tiles[pos]['rect'], 
                                     fill=self.TILE_COLORS['hover'])
    
    def is_adjacent(self, pos1, pos2):
        """Verifica se due posizioni sono adiacenti."""
        row1, col1 = pos1 // 3, pos1 % 3
        row2, col2 = pos2 // 3, pos2 % 3
        
        return (abs(row1 - row2) == 1 and col1 == col2) or \
               (abs(col1 - col2) == 1 and row1 == row2)
    
    def solve_puzzle(self):
        """Avvia la risoluzione del puzzle."""
        if self.is_solving:
            return
        
        if self.current_state == self.goal_state:
            self.log("ℹ️ Il puzzle è già risolto!")
            return
        
        self.is_solving = True
        self.solve_button.config(state="disabled")
        self.stop_button.config(state="normal")
        self.progress.start()
        
        algorithm = self.algorithm_var.get()
        self.log(f"🔍 Avvio risoluzione con {algorithm.upper()}...")
        
        # Avvia thread per la risoluzione
        thread = threading.Thread(target=self._solve_thread, args=(algorithm,))
        thread.daemon = True
        thread.start()
    
    def _solve_thread(self, algorithm):
        """Thread per la risoluzione del puzzle."""
        try:
            start_time = time.time()
            
            # Chiama il solver Prolog
            result = self.logic.solve(self.current_state, algorithm)
            
            elapsed_time = time.time() - start_time
            
            if result and result['success']:
                self.solution_path = result['path']
                self.stats['time'] = elapsed_time
                self.stats['nodes_explored'] = result.get('nodes_explored', 0)
                self.stats['nodes_frontier'] = result.get('nodes_frontier', 0)
                self.stats['memory'] = result.get('memory', 0)
                
                self.master.after(0, self._on_solution_found)
            else:
                self.master.after(0, self._on_solution_not_found)
                
        except Exception as e:
            self.master.after(0, lambda: self._on_solver_error(str(e)))
    
    def _on_solution_found(self):
        """Callback quando una soluzione è trovata."""
        self.progress.stop()
        self.log(f"✅ Soluzione trovata! {len(self.solution_path)} mosse")
        self.update_stats_display()
        
        # Chiedi se animare
        if messagebox.askyesno("Soluzione trovata", 
                              f"Trovata soluzione in {len(self.solution_path)} mosse.\n"
                              f"Vuoi visualizzare l'animazione?"):
            self.animate_solution()
        else:
            self.is_solving = False
            self.solve_button.config(state="normal")
            self.stop_button.config(state="disabled")
    
    def _on_solution_not_found(self):
        """Callback quando nessuna soluzione è trovata."""
        self.progress.stop()
        self.is_solving = False
        self.solve_button.config(state="normal")
        self.stop_button.config(state="disabled")
        self.log("❌ Nessuna soluzione trovata")
        messagebox.showwarning("Nessuna soluzione", 
                             "Non è stata trovata una soluzione per questa configurazione.")
    
    def _on_solver_error(self, error):
        """Callback per errori del solver."""
        self.progress.stop()
        self.is_solving = False
        self.solve_button.config(state="normal")
        self.stop_button.config(state="disabled")
        self.log(f"❌ Errore: {error}")
        messagebox.showerror("Errore", f"Errore durante la risoluzione:\n{error}")
    
    def animate_solution(self):
        """Anima la soluzione passo passo."""
        if not self.solution_path:
            return
        
        self.log("🎬 Animazione soluzione in corso...")
        self._animate_step(0)
    
    def _animate_step(self, step_index):
        """Anima un singolo passo della soluzione."""
        if step_index >= len(self.solution_path) or not self.is_solving:
            self.is_solving = False
            self.solve_button.config(state="normal")
            self.stop_button.config(state="disabled")
            self.log("✅ Animazione completata")
            return
        
        # Applica il prossimo stato
        self.current_state = self.solution_path[step_index]
        self.update_display()
        
        # Evidenzia le tessere mosse
        if step_index > 0:
            prev_state = self.solution_path[step_index - 1]
            # Trova quale tessera si è mossa
            for i in range(9):
                if prev_state[i] != self.current_state[i] and self.current_state[i] != 0:
                    self.canvas.itemconfig(self.tiles[i]['rect'], 
                                         fill=self.TILE_COLORS['path'])
        
        # Prossimo step
        self.master.after(self.animation_speed, 
                         lambda: self._animate_step(step_index + 1))
    
    def stop_solving(self):
        """Interrompe la risoluzione."""
        self.is_solving = False
        self.progress.stop()
        self.solve_button.config(state="normal")
        self.stop_button.config(state="disabled")
        self.log("⏹️ Risoluzione interrotta")
    
    def compare_algorithms(self):
        """Confronta tutti gli algoritmi."""
        if self.is_solving:
            return
        
        CompareDialog(self.master, self.logic, self.current_state)
    
    def update_speed(self, value):
        """Aggiorna la velocità di animazione."""
        self.animation_speed = int(float(value))
        self.speed_label.config(text=f"{self.animation_speed} ms")
    
    def update_stats_display(self):
        """Aggiorna la visualizzazione delle statistiche."""
        self.stats_labels['moves'].config(text=str(self.stats['moves']))
        self.stats_labels['time'].config(text=f"{self.stats['time']:.3f}s")
        self.stats_labels['nodes_explored'].config(text=str(self.stats['nodes_explored']))
        self.stats_labels['nodes_frontier'].config(text=str(self.stats['nodes_frontier']))
        
        # Formatta memoria
        memory = self.stats['memory']
        if memory < 1024:
            mem_str = f"{memory} B"
        elif memory < 1024 * 1024:
            mem_str = f"{memory / 1024:.1f} KB"
        else:
            mem_str = f"{memory / (1024 * 1024):.1f} MB"
        self.stats_labels['memory'].config(text=mem_str)
    
    def reset_stats(self):
        """Reset delle statistiche."""
        self.stats = {
            'moves': 0,
            'time': 0,
            'nodes_explored': 0,
            'nodes_frontier': 0,
            'memory': 0
        }
        self.update_stats_display()
    
    def log(self, message):
        """Aggiunge un messaggio al log."""
        timestamp = time.strftime("%H:%M:%S")
        self.console.insert(tk.END, f"[{timestamp}] {message}\n")
        self.console.see(tk.END)
    
    def show_help(self):
        """Mostra la finestra di aiuto."""
        help_text = """
        🧩 8-PUZZLE AI SOLVER - GUIDA
        ================================
        
        COMANDI TASTIERA:
        • F1: Mostra questo aiuto
        • ESC: Esci dall'applicazione
        • R: Mescola puzzle
        • S/SPACE: Risolvi puzzle
        
        COME GIOCARE:
        1. Clicca su una tessera adiacente allo spazio vuoto per spostarla
        2. Oppure usa 'Risolvi' per far risolvere all'AI
        
        ALGORITMI:
        • A*: Ottimale, usa euristiche
        • BFS: Trova soluzione ottimale
        • Greedy: Velocissimo ma non ottimale
        """
        messagebox.showinfo("Aiuto", help_text)
    
    def _format_state(self, state):
        """Formatta uno stato per il log."""
        return "[" + " ".join(str(x) if x != 0 else "_" for x in state) + "]"


class EditDialog:
    """Dialog per modificare manualmente la configurazione del puzzle."""
    
    def __init__(self, parent, current_state):
        self.result = None
        
        self.top = tk.Toplevel(parent)
        self.top.title("Modifica Configurazione")
        self.top.geometry("300x350")
        self.top.transient(parent)
        self.top.grab_set()
        
        ttk.Label(self.top, text="Inserisci i numeri da 0 a 8:").pack(pady=10)
        ttk.Label(self.top, text="(0 = spazio vuoto)").pack()
        
        # Griglia 3x3 di entry
        frame = ttk.Frame(self.top)
        frame.pack(pady=20)
        
        self.entries = []
        for i in range(9):
            row = i // 3
            col = i % 3
            
            entry = ttk.Entry(frame, width=5, justify="center")
            entry.grid(row=row, column=col, padx=5, pady=5)
            entry.insert(0, str(current_state[i]))
            self.entries.append(entry)
        
        # Bottoni
        button_frame = ttk.Frame(self.top)
        button_frame.pack(pady=10)
        
        ttk.Button(button_frame, text="OK", command=self.ok).pack(side="left", padx=5)
        ttk.Button(button_frame, text="Annulla", command=self.cancel).pack(side="left", padx=5)
    
    def ok(self):
        try:
            values = []
            for entry in self.entries:
                val = int(entry.get())
                if val < 0 or val > 8:
                    raise ValueError("Valore fuori range")
                values.append(val)
            
            # Verifica che ci siano tutti i numeri da 0 a 8
            if sorted(values) != list(range(9)):
                raise ValueError("Numeri mancanti o duplicati")
            
            self.result = values
            self.top.destroy()
            
        except ValueError as e:
            messagebox.showerror("Errore", f"Configurazione non valida:\n{e}")
    
    def cancel(self):
        self.top.destroy()


class CompareDialog:
    """Dialog per confrontare gli algoritmi."""
    
    def __init__(self, parent, logic, initial_state):
        self.logic = logic
        self.initial_state = initial_state
        
        self.window = tk.Toplevel(parent)
        self.window.title("📊 Confronto Algoritmi")
        self.window.geometry("800x600")
        self.window.transient(parent)
        
        # Header
        header = ttk.Label(self.window, text="CONFRONTO PRESTAZIONI ALGORITMI", 
                          font=("Arial", 14, "bold"))
        header.pack(pady=10)
        
        # Treeview per risultati
        columns = ('Algoritmo', 'Tempo (s)', 'Mosse', 'Nodi Esplorati', 'Memoria', 'Ottimale')
        self.tree = ttk.Treeview(self.window, columns=columns, show='headings', height=10)
        
        for col in columns:
            self.tree.heading(col, text=col)
            self.tree.column(col, width=120)
        
        self.tree.pack(pady=20, padx=20, fill="both", expand=True)
        
        # Progress
        self.progress = ttk.Progressbar(self.window, mode='determinate')
        self.progress.pack(fill="x", padx=20, pady=5)
        
        # Log
        self.log_text = scrolledtext.ScrolledText(self.window, height=8, width=80)
        self.log_text.pack(padx=20, pady=10, fill="both", expand=True)
        
        # Bottoni
        button_frame = ttk.Frame(self.window)
        button_frame.pack(pady=10)
        
        ttk.Button(button_frame, text="▶️ Avvia Confronto", 
                  command=self.run_comparison).pack(side="left", padx=5)
        ttk.Button(button_frame, text="💾 Esporta CSV", 
                  command=self.export_csv).pack(side="left", padx=5)
        ttk.Button(button_frame, text="❌ Chiudi", 
                  command=self.window.destroy).pack(side="left", padx=5)
    
    def run_comparison(self):
        """Esegue il confronto degli algoritmi."""
        # Clear previous results
        for item in self.tree.get_children():
            self.tree.delete(item)
        
        algorithms = [
            'astar_manhattan', 'astar_misplaced', 'astar_linear',
            'bfs', 'greedy'
        ]
        
        self.progress['maximum'] = len(algorithms)
        self.progress['value'] = 0
        
        self.log("Avvio confronto algoritmi...\n")
        
        for i, algo in enumerate(algorithms):
            self.log(f"Testing {algo}...")
            
            try:
                start_time = time.time()
                result = self.logic.solve(self.initial_state, algo)
                elapsed = time.time() - start_time
                
                if result and result['success']:
                    self.tree.insert('', 'end', values=(
                        algo,
                        f"{elapsed:.3f}",
                        len(result['path']) - 1,
                        result.get('nodes_explored', 'N/A'),
                        self._format_memory(result.get('memory', 0)),
                        '✅' if result.get('optimal', False) else '❌'
                    ))
                    self.log(f"  ✅ Completato in {elapsed:.3f}s\n")
                else:
                    self.tree.insert('', 'end', values=(
                        algo, "FAIL", "-", "-", "-", "-"
                    ))
                    self.log(f"  ❌ Nessuna soluzione\n")
                    
            except Exception as e:
                self.log(f"  ❌ Errore: {e}\n")
            
            self.progress['value'] = i + 1
            self.window.update()
        
        self.log("\n✅ Confronto completato!")
    
    def export_csv(self):
        """Esporta i risultati in CSV."""
        from tkinter import filedialog
        import csv
        
        filename = filedialog.asksaveasfilename(
            defaultextension=".csv",
            filetypes=[("CSV files", "*.csv"), ("All files", "*.*")]
        )
        
        if filename:
            with open(filename, 'w', newline='') as f:
                writer = csv.writer(f)
                
                # Header
                writer.writerow(['Algoritmo', 'Tempo (s)', 'Mosse', 
                               'Nodi Esplorati', 'Memoria', 'Ottimale'])
                
                # Data
                for item in self.tree.get_children():
                    writer.writerow(self.tree.item(item)['values'])
            
            self.log(f"\n💾 Esportato in: {filename}")
    
    def log(self, message):
        """Aggiunge messaggio al log."""
        self.log_text.insert(tk.END, message)
        self.log_text.see(tk.END)
        self.window.update()
    
    def _format_memory(self, memory):
        """Formatta la memoria."""
        if memory == 0:
            return "N/A"
        elif memory < 1024:
            return f"{memory} B"
        elif memory < 1024 * 1024:
            return f"{memory / 1024:.1f} KB"
        else:
            return f"{memory / (1024 * 1024):.1f} MB"