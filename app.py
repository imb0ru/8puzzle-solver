"""
8-Puzzle AI Solver
==================
Entry point dell'applicazione per il risolutore automatico dell'8-puzzle.
Integra algoritmi di ricerca AI con interfaccia grafica interattiva.

Autore: [Il tuo nome]
Corso: Ingegneria della Conoscenza - Prof. Nicola Fanizzi
Università degli Studi di Bari "Aldo Moro"
"""

import tkinter as tk
from tkinter import messagebox
import sys
import os
import argparse
from datetime import datetime

# Aggiungi il percorso del progetto al PYTHONPATH
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from gui.puzzle_gui import PuzzleGUI
from logic.puzzle_logic import PuzzleLogic


class EightPuzzleApp:
    """
    Classe principale dell'applicazione 8-Puzzle Solver.
    Gestisce l'inizializzazione e il ciclo di vita dell'applicazione.
    """
    
    def __init__(self, debug=False, verbose=False):
        """
        Inizializza l'applicazione.
        
        Args:
            debug (bool): Abilita modalità debug
            verbose (bool): Abilita output verboso
        """
        self.debug = debug
        self.verbose = verbose
        self.root = None
        self.gui = None
        self.logic = None
        
        self._print_header()
        self._check_dependencies()
        
    def _print_header(self):
        """Stampa l'header dell'applicazione."""
        print("=" * 60)
        print("🧩 8-PUZZLE SOLVER v1.0")
        print("=" * 60)
        print("Ingegneria della Conoscenza - A.A. 2024-2025")
        print("Università degli Studi di Bari 'Aldo Moro'")
        print("=" * 60)
        
        if self.debug:
            print("🔧 Modalità DEBUG attiva")
        if self.verbose:
            print("📝 Output VERBOSE attivo")
        
        print(f"⏰ Avvio: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print("-" * 60)
    
    def _check_dependencies(self):
        """Verifica che tutte le dipendenze siano installate."""
        dependencies_ok = True
        
        # Verifica Python version
        if sys.version_info < (3, 9):
            print("❌ Python 3.9+ richiesto")
            dependencies_ok = False
        else:
            print("✅ Python version OK")
        
        # Verifica pyswip
        try:
            import pyswip
            print("✅ pyswip installato")
        except ImportError:
            print("❌ pyswip non trovato. Installa con: pip install pyswip")
            dependencies_ok = False
        
        # Verifica SWI-Prolog
        try:
            from pyswip import Prolog
            prolog = Prolog()
            prolog.assertz("test(ok)")
            list(prolog.query("test(ok)"))
            print("✅ SWI-Prolog funzionante")
        except Exception as e:
            print(f"❌ SWI-Prolog non configurato: {e}")
            dependencies_ok = False
        
        # Verifica altre librerie
        required_libs = ['pandas']
        for lib in required_libs:
            try:
                __import__(lib)
                print(f"✅ {lib} installato")
            except ImportError:
                print(f"⚠️ {lib} non trovato (opzionale)")
        
        if not dependencies_ok:
            print("\n❌ Alcune dipendenze mancano. Controlla l'installazione.")
            if not self.debug:
                sys.exit(1)
        else:
            print("\n✅ Tutte le dipendenze sono soddisfatte!")
        
        print("-" * 60)
    
    def run(self):
        """Avvia l'applicazione."""
        try:
            print("🚀 Inizializzazione GUI...")
            
            # Crea finestra principale
            self.root = tk.Tk()
            self.root.title("🧩 8-Puzzle AI Solver")
            
            # Centra la finestra
            self._center_window(1200, 800)
            
            # Inizializza logica
            print("🧠 Caricamento motore logico...")
            self.logic = PuzzleLogic(debug=self.debug)
            
            # Inizializza GUI
            print("🎨 Creazione interfaccia...")
            self.gui = PuzzleGUI(self.root, self.logic, debug=self.debug)
            
            # Gestione chiusura
            self.root.protocol("WM_DELETE_WINDOW", self._on_closing)
            
            print("✅ Applicazione pronta!")
            print("-" * 60)
            
            # Avvia loop principale
            self.root.mainloop()
            
        except Exception as e:
            print(f"❌ Errore critico: {e}")
            if self.debug:
                import traceback
                traceback.print_exc()
            sys.exit(1)
    
    def _center_window(self, width, height):
        """Centra la finestra sullo schermo."""
        screen_width = self.root.winfo_screenwidth()
        screen_height = self.root.winfo_screenheight()
        
        x = (screen_width - width) // 2
        y = (screen_height - height) // 2
        
        self.root.geometry(f"{width}x{height}+{x}+{y}")
        self.root.minsize(800, 600)
    
    def _on_closing(self):
        """Gestisce la chiusura dell'applicazione."""
        if messagebox.askokcancel("Conferma uscita", 
                                 "Vuoi davvero uscire dall'8-Puzzle Solver?"):
            print("\n" + "=" * 60)
            print("👋 Grazie per aver usato 8-Puzzle AI Solver!")
            print("=" * 60)
            
            if self.logic:
                self.logic.cleanup()
            
            self.root.destroy()
            sys.exit(0)


def main():
    """Entry point principale."""
    parser = argparse.ArgumentParser(
        description='8-Puzzle AI Solver - Risolvi il puzzle con l\'intelligenza artificiale',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Esempi di utilizzo:
  python app.py                    # Avvia normalmente
  python app.py --debug            # Modalità debug
  python app.py --verbose          # Output dettagliato
  python app.py --debug --verbose  # Debug + verbose
        """
    )
    
    parser.add_argument(
        '--debug', '-d',
        action='store_true',
        help='Abilita modalità debug con output diagnostico'
    )
    
    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Abilita output verboso durante l\'esecuzione'
    )
    
    parser.add_argument(
        '--version',
        action='version',
        version='8-Puzzle AI Solver v1.0'
    )
    
    args = parser.parse_args()
    
    # Crea e avvia applicazione
    app = EightPuzzleApp(debug=args.debug, verbose=args.verbose)
    app.run()


if __name__ == "__main__":
    main()