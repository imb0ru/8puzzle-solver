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
        self._bind_events()
        self.update_display()
        
    def _setup_ui(self):
        """Costruisce l'interfaccia utente."""
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
        
        # Sezione algoritmi (aggiornata con A* Combined)
        ttk.Label(left_frame, text="🧠 Algoritmo:", font=("Arial", 10, "bold")).pack(anchor="w", pady=(0, 5))
        
        self.algorithm_var = tk.StringVar(value="astar_manhattan")
        algorithms = [
            ("A* (Manhattan)", "astar_manhattan"),
            ("A* (Misplaced)", "astar_misplaced"),
            ("A* (Combined)", "astar_combined"),
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
        center_frame = ttk.LabelFrame(upper_frame, text="🧩 8-Puzzle", padding="20")
        center_frame.pack(side="left", fill="both", expand=True)
        
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
        console_frame = ttk.LabelFrame(main_container, text="📝 Console", padding="5")
        console_frame.pack(fill="both", expand=True, pady=(10, 0))
        
        # Console con scrollbar
        console_container = ttk.Frame(console_frame)
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
        
        # Aggiungi tag per colorare i messaggi
        self.console.tag_config("info", foreground="#00ff00")
        self.console.tag_config("warning", foreground="#ffaa00")
        self.console.tag_config("error", foreground="#ff0000")
        self.console.tag_config("success", foreground="#00ffff")
        
        self.log("🧩 8-Puzzle AI Solver avviato", "info")
        self.log("Seleziona un algoritmo e clicca 'Risolvi'", "info")