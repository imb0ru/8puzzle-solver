"""
puzzle_gui.py - Interfaccia grafica per l'8-Puzzle Solver
==========================================================
GUI interattiva con animazioni, statistiche e confronto algoritmi.
"""

import tkinter as tk
from tkinter import ttk, messagebox, scrolledtext
import time
from collections import deque
import random
import os
from datetime import datetime


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
        
        # Crea cartella results se non esiste
        self.results_dir = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'results')
        if not os.path.exists(self.results_dir):
            os.makedirs(self.results_dir)
        
        self._setup_ui()
        self._create_puzzle_board()
        self.update_display()
        
    def _setup_ui(self):
        """
        Costruisce l'interfaccia utente completa.
        
        Crea tutti i componenti dell'interfaccia grafica:
        - Frame dei controlli con algoritmi e configurazioni
        - Canvas del puzzle 3x3 interattivo  
        - Pannello delle statistiche in tempo reale
        - Console di logging con output colorato
        """
        # Stile
        style = ttk.Style()
        style.theme_use('clam')
        
        # Frame principale verticale
        main_container = ttk.Frame(self.master)
        main_container.pack(fill="both", expand=True, padx=10, pady=10)
        
        # Frame superiore (puzzle e controlli)
        upper_frame = ttk.Frame(main_container)
        upper_frame.pack(fill="both", expand=True)
        
        # ========== COLONNA SINISTRA: Controlli ==========
        left_frame = ttk.LabelFrame(upper_frame, text="🎮 Controlli", padding="10")
        left_frame.pack(side="left", fill="both", padx=(0, 5))
        
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
        
        self.algorithm_var = tk.StringVar(value="astar")
        algorithms = [
            ("A*", "astar"),
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
        
        ttk.Button(left_frame, text="📊 Confronta Algoritmi", 
                  command=self.compare_algorithms).pack(fill="x", pady=2)
    
        ttk.Button(left_frame, text="❓ Aiuto", 
                  command=self.show_help).pack(fill="x", pady=2)
        
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
        center_frame = ttk.LabelFrame(upper_frame, text="🧩 8-Puzzle", padding="20")
        center_frame.pack(side="left", fill="both", expand=True)
        
        # Canvas per il puzzle - DIMENSIONE CORRETTA (3x3 tiles + spacing)
        canvas_width = 3 * self.TILE_SIZE + 20  # 320 pixels
        canvas_height = 3 * self.TILE_SIZE + 20  # 320 pixels
        self.canvas = tk.Canvas(center_frame, width=canvas_width, height=canvas_height, 
                               bg="white", highlightthickness=2, highlightbackground="#333")
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
        
        # ========== COLONNA DESTRA: Statistiche ==========
        right_frame = ttk.Frame(upper_frame)
        right_frame.pack(side="left", fill="both", padx=(5, 0))
        
        # Frame statistiche
        stats_frame = ttk.LabelFrame(right_frame, text="📊 Statistiche", padding="10")
        stats_frame.pack(fill="x")
        
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
        
        # ========== FRAME INFERIORE: Console/Log combinata ==========
        self.console_frame = ttk.LabelFrame(main_container, text="📝 Console", padding="5")
        self.console_frame.pack(fill="both", expand=True, pady=(10, 0))
        
        # Console con scrollbar
        console_container = ttk.Frame(self.console_frame)
        console_container.pack(fill="both", expand=True)
        
        # Scrollbar verticale
        v_scrollbar = ttk.Scrollbar(console_container, orient="vertical")
        v_scrollbar.pack(side="right", fill="y")
        
        # Console text widget - READ ONLY
        self.console = tk.Text(console_container, 
                              height=8,
                              font=("Courier", 9), 
                              bg="#1e1e1e", 
                              fg="#00ff00",
                              wrap="word",
                              yscrollcommand=v_scrollbar.set,
                              state="disabled")  # Inizialmente disabilitata per read-only
        self.console.pack(side="left", fill="both", expand=True)
        
        v_scrollbar.config(command=self.console.yview)
        
        self.console.tag_config("info", foreground="#00ff00")
        self.console.tag_config("warning", foreground="#ffaa00")
        self.console.tag_config("error", foreground="#ff0000")
        self.console.tag_config("success", foreground="#00ffff")
        
        # Salva l'altezza originale della finestra
        self.master.update_idletasks()
        self.original_window_height = self.master.winfo_height()
        self.console_visible = True
        
        self.log("🧩 8-Puzzle AI Solver avviato", "info")
        self.log("Seleziona un algoritmo e clicca 'Risolvi'", "info")
    
    def _create_puzzle_board(self):
        """
        Crea le tessere del puzzle sulla canvas.
        
        Disegna una griglia 3x3 di tessere interattive:
        - Ogni tessera è un rettangolo con testo numerico
        - Le tessere sono cliccabili per le mosse manuali
        - Lo spazio vuoto (0) viene visualizzato diversamente
        - Memorizza gli ID degli oggetti canvas per aggiornamenti
        """
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
    
    def _center_window(self, window, width, height):
        """
        Centra una finestra sullo schermo.
        
        Args:
            window: Finestra Tkinter da centrare
            width (int): Larghezza desiderata della finestra  
            height (int): Altezza desiderata della finestra
        """
        window.update_idletasks()  # Assicura che le dimensioni siano aggiornate
        screen_width = window.winfo_screenwidth()
        screen_height = window.winfo_screenheight()
        x = (screen_width - width) // 2
        y = (screen_height - height) // 2
        window.geometry(f"{width}x{height}+{x}+{y}")
    
    def update_display(self):
        """
        Aggiorna la visualizzazione del puzzle.
        
        Sincronizza lo stato interno con la rappresentazione grafica:
        - Aggiorna i colori delle tessere
        - Mostra/nasconde i numeri in base al valore
        - Applica stili diversi per lo spazio vuoto
        """
        for i in range(9):
            value = self.current_state[i]
            
            if value == 0:
                # Spazio vuoto
                self.canvas.itemconfig(self.tiles[i]['rect'], 
                                     fill=self.TILE_COLORS['empty'])
                self.canvas.itemconfig(self.tiles[i]['text'], text="")
            else:
                # Tessera numerata
                self.canvas.itemconfig(self.tiles[i]['rect'], 
                                     fill=self.TILE_COLORS['bg'])
                self.canvas.itemconfig(self.tiles[i]['text'], 
                                     text=str(value))
    
    def update_stats(self):
        """
        Aggiorna le statistiche visualizzate nel pannello destro.
        
        Formatta e mostra:
        - Numero di mosse effettuate
        - Tempo di esecuzione dell'algoritmo
        - Nodi esplorati durante la ricerca
        - Nodi in frontiera
        - Utilizzo di memoria (con unità appropriate)
        """
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
    
    def update_speed(self, value):
        """
        Aggiorna la velocità di animazione delle soluzioni.
        
        Args:
            value: Nuovo valore di velocità in millisecondi per frame
        """
        self.animation_speed = int(float(value))
        self.speed_label.config(text=f"{self.animation_speed} ms")

    
    def log(self, message, level="info"):
        """
        Aggiunge un messaggio alla console con timestamp e colore.
        
        Args:
            message (str): Messaggio da visualizzare
            level (str): Livello del messaggio ("info", "warning", "error", "success")
        """
        timestamp = datetime.now().strftime("%H:%M:%S")
        formatted_message = f"[{timestamp}] {message}\n"
        
        # Solo se la console esiste e è visibile
        if hasattr(self, 'console'):
            self.console.config(state="normal")
            self.console.insert("end", formatted_message, level)
            self.console.see("end")
            self.console.config(state="disabled")
        
        # Se la console è nascosta, stampa anche su stdout per debug
        if hasattr(self, 'console_visible') and not self.console_visible:
            print(f"[LOG] {message}")
    
    def on_tile_click(self, event):
        """
        Gestisce il click su una tessera per il movimento manuale.
        
        Args:
            event: Evento di click del mouse con coordinate
            
        Verifica se la tessera cliccata è adiacente allo spazio vuoto
        e in caso positivo esegue lo scambio, aggiornando la visualizzazione.
        """
        if self.is_solving:
            return
        
        # Trova quale tessera è stata cliccata
        x = (event.x - 10) // self.TILE_SIZE
        y = (event.y - 10) // self.TILE_SIZE
        
        if 0 <= x < 3 and 0 <= y < 3:
            clicked_pos = y * 3 + x
            
            # Trova posizione dello spazio vuoto
            empty_pos = self.current_state.index(0)
            
            # Verifica se la mossa è valida
            if self.is_adjacent(clicked_pos, empty_pos):
                # Scambia
                self.current_state[clicked_pos], self.current_state[empty_pos] = \
                    self.current_state[empty_pos], self.current_state[clicked_pos]
                
                self.update_display()
                
                # Verifica vittoria
                if self.current_state == self.goal_state:
                    self.info_label.config(text="🎉 Puzzle risolto!")
                    messagebox.showinfo("Vittoria!", "Hai risolto il puzzle!")
    
    def is_adjacent(self, pos1, pos2):
        """
        Verifica se due posizioni sono adiacenti nella griglia 3x3.
        
        Args:
            pos1 (int): Prima posizione (0-8)
            pos2 (int): Seconda posizione (0-8)
            
        Returns:
            bool: True se le posizioni sono adiacenti (Manhattan distance = 1)
        """
        row1, col1 = pos1 // 3, pos1 % 3
        row2, col2 = pos2 // 3, pos2 % 3
        
        return abs(row1 - row2) + abs(col1 - col2) == 1
    
    def shuffle_puzzle(self):
        """
        Mescola il puzzle in modo casuale ma garantendo la risolvibilità.
        
        Genera una nuova configurazione partendo dallo stato obiettivo
        e applicando mosse casuali valide. La difficoltà viene scelta
        casualmente tra easy, medium e hard.
        """
        if self.is_solving:
            return
        
        difficulty = random.choice(["easy", "medium", "hard"])
        self.current_state = self.logic.generate_random_puzzle(difficulty)
        self.update_display()
        self.info_label.config(text=f"Puzzle mescolato ({difficulty})")
        self.log(f"🎲 Nuovo puzzle generato (difficoltà: {difficulty})", "info")
    
    def set_goal_state(self):
        """
        Imposta lo stato obiettivo standard del puzzle.
        
        Configura il puzzle nella disposizione finale:
        1 2 3
        4 5 6  
        7 8 □
        """
        if self.is_solving:
            return
        
        self.current_state = [1, 2, 3, 4, 5, 6, 7, 8, 0]
        self.update_display()
        self.info_label.config(text="Stato obiettivo impostato")
        self.log("🎯 Impostato stato obiettivo", "info")
    
    def enable_edit_mode(self):
        """
        Abilita la modalità di modifica manuale del puzzle.
        
        Apre una finestra di dialogo con una griglia 3x3 di campi di input
        dove l'utente può inserire manualmente i numeri per configurare
        il puzzle. Include validazione dell'input e controllo di risolvibilità.
        """
        dialog = tk.Toplevel(self.master)
        dialog.title("✏️ Configurazione Manuale")
        dialog.resizable(False, False)
        
        # Centra la finestra di dialog
        self._center_window(dialog, 400, 500)
        
        # Rendi il dialog modale
        dialog.transient(self.master)
        dialog.grab_set()
        
        # Istruzioni
        ttk.Label(dialog, text="Inserisci i numeri da 0 a 8\n(0 = spazio vuoto)", font=("Arial", 11)).pack(pady=10)

        # Frame per la griglia
        grid_frame = ttk.Frame(dialog)
        grid_frame.pack(pady=10)
        
        # Entry per ogni posizione
        entries = []
        for i in range(9):
            row = i // 3
            col = i % 3
            
            entry = ttk.Entry(grid_frame, width=5, font=("Arial", 16), 
                             justify="center")
            entry.grid(row=row, column=col, padx=5, pady=5)
            entry.insert(0, str(self.current_state[i]))
            entries.append(entry)
        
        # Frame per i bottoni
        button_frame = ttk.Frame(dialog)
        button_frame.pack(pady=20)
        
        
        
        def apply_config():
            try:
                # Leggi i valori
                new_state = []
                for entry in entries:
                    value = int(entry.get())
                    if value < 0 or value > 8:
                        raise ValueError("I valori devono essere tra 0 e 8")
                    new_state.append(value)
                
                # Valida lo stato
                valid, msg = self.logic.validate_state(new_state)
                
                if valid:
                    self.current_state = new_state
                    self.update_display()
                    self.log("✏️ Configurazione manuale applicata", "info")
                    dialog.destroy()
                else:
                    messagebox.showerror("Errore", msg)
                    
            except ValueError as e:
                messagebox.showerror("Errore", f"Input non valido: {e}")
        
        ttk.Button(button_frame, text="✅ Applica", 
                  command=apply_config).pack(side="left", padx=5)
        ttk.Button(button_frame, text="❌ Annulla", 
                  command=dialog.destroy).pack(side="left", padx=5)
        
        # Suggerimenti
        tips_frame = ttk.LabelFrame(dialog, text="💡 Suggerimenti", padding="10")
        tips_frame.pack(fill="x", padx=10, pady=10)
        
        tips_text = """• Usa 0 per lo spazio vuoto
• Ogni numero da 1 a 8 deve apparire una volta
• Non tutte le configurazioni sono risolvibili
• Il sistema verificherà automaticamente la validità"""
        
        ttk.Label(tips_frame, text=tips_text, font=("Arial", 9)).pack()
    
    def solve_puzzle(self):
        """
        Risolve il puzzle utilizzando l'algoritmo selezionato.
        
        Esegue la risoluzione in un thread separato per non bloccare l'UI.
        Gestisce l'interfaccia utente durante la risoluzione (progress bar,
        disabilitazione controlli) e al completamento chiede se animare
        la soluzione trovata.
        """
        if self.is_solving:
            self.log("⚠️ Risoluzione già in corso", "warning")
            return
        
        if self.current_state == self.goal_state:
            self.log("ℹ️ Il puzzle è già risolto!")
            return
        
        
        self.is_solving = True
        self.solve_button.config(state="disabled")
        self.progress.start(100)

        algorithm = self.algorithm_var.get()
        self.log(f"🔍 Risoluzione con {algorithm}...", "info")
        self.info_label.config(text=f"Risolvendo con {algorithm}...")
        self.master.update()
        
        # Esegui in thread separato
        def solve_thread():
            try:
                result = self.logic.solve(self.current_state, algorithm)

                if result and result['success']:
                    self.solution_path = result['path']
                    
                    # Aggiorna statistiche
                    self.stats['time'] = result['time']
                    self.stats['nodes_explored'] = result['nodes_explored']
                    self.stats['nodes_frontier'] = result['nodes_frontier']
                    self.stats['memory'] = result['memory']
                    
                    self.update_stats()
                    
                    # Log risultati
                    self.log(f"✅ Soluzione trovata: {result['path_length']} mosse", "success")
                    self.log(f"⏱️ Tempo: {result['time']:.3f}s", "info")
                    self.log(f"🔍 Nodi esplorati: {result['nodes_explored']}", "info")
                    
                    # Anima soluzione nel thread principale
                    self.master.after(100, lambda: self._on_solution_found())
                else:
                    self.log(f"❌ {result.get('message', 'Errore sconosciuto')}", "error")
                    self.info_label.config(text="Risoluzione fallita")
                    self.master.after(0, self._on_solution_not_found)

            except Exception as e:
                self.log(f"❌ Errore: {e}", "error")
                if self.debug:
                    import traceback
                    self.log(traceback.format_exc(), "error")
            finally:
                self.progress.stop()
        
        # Avvia thread
        import threading
        thread = threading.Thread(target=solve_thread, daemon=True)
        thread.start()
        
    def _on_solution_found(self):
        """
        Callback eseguita quando una soluzione è stata trovata.
        
        Ferma la progress bar, logga i risultati e chiede all'utente
        se desidera visualizzare l'animazione della soluzione.
        """
        self.progress.stop()
        self.log(f"✅ Soluzione trovata! {len(self.solution_path)} mosse")
        
        
        # Chiedi se animare
        if messagebox.askyesno("Soluzione trovata", 
                              f"Trovata soluzione in {len(self.solution_path)} mosse.\n"
                              f"Vuoi visualizzare l'animazione?"):
            self.animate_solution(self.solution_path)
        else:
            self.is_solving = False
            self.solve_button.config(state="normal")
    
    def _on_solution_not_found(self, message=None):
        """
        Callback eseguita quando nessuna soluzione è stata trovata.
        
        Args:
            message (str, optional): Messaggio di errore specifico
            
        Ferma la progress bar, riabilita i controlli e mostra
        un avviso all'utente.
        """
        self.progress.stop()
        self.is_solving = False
        self.solve_button.config(state="normal")
        self.log("❌ Nessuna soluzione trovata")
        messagebox.showwarning("Nessuna soluzione", 
                             "Non è stata trovata una soluzione per questa configurazione.")
    
    def animate_solution(self, path):
        """
        Anima la soluzione passo per passo con evidenziazione delle mosse.
        
        Args:
            path: Lista di stati che rappresentano la soluzione
            
        Mostra ogni mossa della soluzione con:
        - Aggiornamento dello stato del puzzle
        - Evidenziazione temporanea della tessera mossa
        - Aggiornamento del contatore mosse
        - Velocità controllabile dall'utente
        """
        if not path or len(path) < 2:
            self.log("⚠️ Nessun percorso da animare", "warning")
            return
        
        self.is_solving = True
        self.solve_button.config(state="disabled")
        
        # Reset contatore mosse per l'animazione
        self.stats['moves'] = 0
        self.update_stats()
        
        def animate_step(index):
            if not self.is_solving or index >= len(path):
                self.is_solving = False
                self.solve_button.config(state="normal")
                self.info_label.config(text="✅ Soluzione completata!")
                self.log(f"✅ Animazione completata: {len(path)-1} mosse", "success")
                return
            
            # Aggiorna stato corrente
            self.current_state = path[index]
            self.update_display()
            
            # Aggiorna contatore mosse durante l'animazione
            if index > 0:
                self.stats['moves'] = index
                self.update_stats()
            
            # Evidenzia tessera mossa
            if index > 0:
                prev_state = path[index - 1]
                # Trova quale tessera si è mossa
                for i in range(9):
                    if prev_state[i] != self.current_state[i] and self.current_state[i] != 0:
                        # Evidenzia temporaneamente
                        self.canvas.itemconfig(self.tiles[i]['rect'], 
                                             fill=self.TILE_COLORS['path'])
                        self.master.after(self.animation_speed // 2, 
                                        lambda: self.canvas.itemconfig(
                                            self.tiles[i]['rect'], 
                                            fill=self.TILE_COLORS['bg']))
                        break
            
            # Prossimo step
            self.master.after(self.animation_speed, lambda: animate_step(index + 1))
        
        # Inizia animazione
        animate_step(0)
    
    def compare_algorithms(self):
        """
        Confronta le prestazioni di tutti gli algoritmi disponibili.
        
        Apre una finestra con una tabella che mostra i risultati del
        confronto sistematico tra A*, BFS e Greedy sullo stato corrente.
        Include metriche di tempo, mosse, nodi esplorati e identifica
        il migliore per ogni categoria.
        """
        # Crea finestra di confronto
        compare_window = tk.Toplevel(self.master)
        compare_window.title("📊 Confronto Algoritmi")
        compare_window.resizable(False, False)
        
        # Centra la finestra
        self._center_window(compare_window, 800, 600)
        
        # Rendi modale
        compare_window.transient(self.master)
        compare_window.grab_set()
        
        # Frame principale
        main_frame = ttk.Frame(compare_window, padding="10")
        main_frame.pack(fill="both", expand=True)
        
        # Titolo
        title = ttk.Label(main_frame, 
                         text="Confronto Prestazioni Algoritmi",
                         font=("Arial", 14, "bold"))
        title.pack(pady=10)
        
        # Info stato corrente
        info_text = f"Stato corrente: {self.current_state}"
        ttk.Label(main_frame, text=info_text, font=("Arial", 10)).pack()
        
        # Treeview per risultati
        columns = ('Algorithm', 'Time (s)', 'Moves', 'Nodes')
        tree = ttk.Treeview(main_frame, columns=columns, show='headings', height=8)
        
        # Definisci colonne
        tree.heading('Algorithm', text='Algoritmo')
        tree.heading('Time (s)', text='Tempo (s)')
        tree.heading('Moves', text='Mosse')
        tree.heading('Nodes', text='Nodi Esplorati')
        
        tree.column('Algorithm', width=200)
        tree.column('Time (s)', width=150)
        tree.column('Moves', width=150)
        tree.column('Nodes', width=200)
        
        tree.pack(pady=20)
        
        # Progress bar
        progress = ttk.Progressbar(main_frame, mode='determinate')
        progress.pack(fill="x", pady=10)
        
        # Label stato
        status_label = ttk.Label(main_frame, text="Pronto per iniziare il test")
        status_label.pack()
        
        # Frame bottoni
        button_frame = ttk.Frame(main_frame)
        button_frame.pack(pady=20)
        
        results = {}
        
        def run_comparison():
            # Clear previous results
            for item in tree.get_children():
                tree.delete(item)
            
            algorithms = [
                ('A*', 'astar'),
                ('BFS', 'bfs'),
                ('Greedy', 'greedy')
            ]
            
            progress['maximum'] = len(algorithms)
            progress['value'] = 0
            
            for display_name, algo_name in algorithms:
                status_label.config(text=f"Testing {display_name}...")
                compare_window.update()
                
                try:
                    result = self.logic.solve(self.current_state, algo_name)
                    
                    if result['success']:
                        results[display_name] = result
                        
                        # Aggiungi alla tabella
                        tree.insert('', 'end', values=(
                            display_name,
                            f"{result['time']:.3f}",
                            result['path_length'],
                            result['nodes_explored']
                        ))
                    else:
                        tree.insert('', 'end', values=(
                            display_name,
                            "N/A",
                            "N/A",
                            "N/A"
                        ))
                except Exception as e:
                    tree.insert('', 'end', values=(
                        display_name,
                        "ERROR",
                        "ERROR",
                        "ERROR"
                    ))
                    print(f"Errore con {algo_name}: {e}")
                
                progress['value'] += 1
                compare_window.update()
            
            status_label.config(text="✅ Confronto completato!")
            
            # Trova il migliore
            if results:
                best_time = min(results.items(), key=lambda x: x[1]['time'])
                best_moves = min(results.items(), key=lambda x: x[1]['path_length'])
                best_nodes = min(results.items(), key=lambda x: x[1]['nodes_explored'])
                
                summary = f"\n🏆 Migliori prestazioni:\n"
                summary += f"⚡ Più veloce: {best_time[0]} ({best_time[1]['time']:.3f}s)\n"
                summary += f"🎯 Meno mosse: {best_moves[0]} ({best_moves[1]['path_length']} mosse)\n"
                summary += f"🔍 Meno nodi: {best_nodes[0]} ({best_nodes[1]['nodes_explored']} nodi)"
                
                summary_label = ttk.Label(main_frame, text=summary, 
                                        font=("Arial", 10), justify="left")
                summary_label.pack(pady=10)
        
        ttk.Button(button_frame, text="▶️ Avvia Confronto", 
                  command=run_comparison).pack(side="left", padx=5)
        ttk.Button(button_frame, text="❌ Chiudi", 
                  command=compare_window.destroy).pack(side="left", padx=5)
    
    def show_help(self):
        """
        Mostra la finestra di aiuto con informazioni complete sull'applicazione.
        
        Crea una finestra con tab multiple contenenti:
        - Come giocare: regole e controlli del puzzle
        - Algoritmi: descrizione dettagliata degli algoritmi AI
        - Statistiche: spiegazione delle metriche visualizzate
        """
        help_window = tk.Toplevel(self.master)
        help_window.title("❓ Aiuto - 8-Puzzle Solver")
        help_window.resizable(False, False)
        
        # Centra la finestra
        self._center_window(help_window, 600, 500)
        
        # Rendi modale
        help_window.transient(self.master)
        help_window.grab_set()
        
        # Notebook per organizzare l'aiuto
        notebook = ttk.Notebook(help_window)
        notebook.pack(fill="both", expand=True, padx=10, pady=10)
        
        # Tab 1: Come giocare
        tab1 = ttk.Frame(notebook)
        notebook.add(tab1, text="🎮 Come Giocare")
        
        play_text = """
8-PUZZLE: IL GIOCO

L'8-puzzle è un rompicapo a scorrimento dove devi riordinare
8 tessere numerate in una griglia 3x3.

OBIETTIVO:
Disporre le tessere nell'ordine:
    1 2 3
    4 5 6
    7 8 □
    
(dove □ rappresenta lo spazio vuoto)

COME MUOVERE:
• Clicca su una tessera adiacente allo spazio vuoto
• La tessera scivolerà nello spazio vuoto
• Puoi muovere solo in orizzontale o verticale

CONTROLLI:
• 🎲 Random: Genera un nuovo puzzle casuale
• 🎯 Goal: Imposta lo stato obiettivo
• ✏️ Modifica: Configura manualmente il puzzle
• 🔍 Risolvi: Trova la soluzione con l'algoritmo scelto

SHORTCUTS:
• F1: Mostra questo aiuto
• R: Genera puzzle casuale
• S: Risolvi puzzle
• C: Mostra/Nascondi console
• ESC: Ferma animazione
        """
        
        text1 = tk.Text(tab1, wrap="word", font=("Arial", 10), padx=10, pady=10)
        text1.insert("1.0", play_text)
        text1.config(state="disabled")
        text1.pack(fill="both", expand=True)
        
        # Tab 2: Algoritmi
        tab2 = ttk.Frame(notebook)
        notebook.add(tab2, text="🧠 Algoritmi")
        
        algo_text = """
ALGORITMI DISPONIBILI:

A* (ALGORITMO A-STAR)
• Utilizza euristica combinata (Manhattan + Misplaced)
• Garantisce sempre la soluzione ottimale
• Eccellente bilanciamento tra velocità e qualità
• Esplora intelligentemente lo spazio di ricerca

BFS (BREADTH-FIRST SEARCH)
• Esplora livello per livello
• Garantisce soluzione ottimale
• Usa molta memoria per stati complessi
• Affidabile ma può essere lento

GREEDY BEST-FIRST
• Segue sempre l'euristica migliore locale
• Molto veloce nell'esecuzione
• NON garantisce ottimalità
• Buono per soluzioni rapide

QUALE SCEGLIERE?
• Per la migliore soluzione: A*
• Per garanzia assoluta: BFS
• Per velocità massima: Greedy
        """
        
        text2 = tk.Text(tab2, wrap="word", font=("Arial", 10), padx=10, pady=10)
        text2.insert("1.0", algo_text)
        text2.config(state="disabled")
        text2.pack(fill="both", expand=True)
        
        # Tab 3: Statistiche
        tab3 = ttk.Frame(notebook)
        notebook.add(tab3, text="📊 Statistiche")
        
        stats_text = """
INTERPRETARE LE STATISTICHE:

MOSSE
• Numero di spostamenti per risolvere
• Meno mosse = soluzione più efficiente

TEMPO
• Tempo di calcolo della soluzione
• Dipende dalla complessità e dall'algoritmo

NODI ESPLORATI
• Stati del puzzle analizzati
• Indica l'efficienza della ricerca

NODI IN FRONTIERA
• Stati in attesa di essere esplorati
• Indica l'uso di memoria

MEMORIA
• Stima della memoria utilizzata
• Importante per puzzle complessi
        """
        
        text3 = tk.Text(tab3, wrap="word", font=("Arial", 10), padx=10, pady=10)
        text3.insert("1.0", stats_text)
        text3.config(state="disabled")
        text3.pack(fill="both", expand=True)