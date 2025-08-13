# 🧩 8-Puzzle AI Solver – Un viaggio nell'Intelligenza Artificiale

Questo progetto è stato realizzato per il corso di **Ingegneria della Conoscenza**, tenuto dal **Prof. Nicola Fanizzi**, all'interno del **corso di laurea triennale in Informatica** presso l'**Università degli Studi di Bari Aldo Moro**.

---

## 🎯 Finalità del progetto

Il progetto propone un sistema completo per la risoluzione automatica del celebre **8-puzzle**, un rompicapo a scorrimento che ha affascinato generazioni di appassionati e che rappresenta un caso di studio fondamentale nell'ambito dell'Intelligenza Artificiale.

L'obiettivo didattico è applicare i concetti fondamentali dell'**Ingegneria della Conoscenza**, combinando:

- La **rappresentazione della conoscenza** tramite stati, transizioni e regole logiche
- La **ricerca euristica** in spazi di stato complessi
- Il **ragionamento simbolico** su configurazioni e mosse valide
- L'**ottimizzazione** dei percorsi di soluzione
- L'integrazione con interfacce grafiche moderne e intuitive

---

## 📚 Contenuti e metodi

Il progetto affronta il problema dell'8-puzzle implementando e confrontando diversi algoritmi di ricerca classici dell'AI:

### 🔍 Algoritmi implementati

| Algoritmo | Tipo | Caratteristiche | Complessità |
|-----------|------|-----------------|-------------|
| **BFS** | Non informata | Trova soluzione ottimale, alta memoria | O(b^d) spazio |
| **A\*** | Informata | Ottimale con euristica ammissibile | O(b^d) |
| **Greedy Best-First** | Informata | Veloce ma non ottimale | O(b^d) |

### 🧮 Euristiche utilizzate

- **Manhattan Distance**: Somma delle distanze Manhattan di ogni tessera dalla sua posizione obiettivo
- **Misplaced Tiles**: Numero di tessere in posizione errata
- **Combined Heuristic**: Combinazione pesata delle euristiche precedenti per massima informatività

---

## ⚙️ Componenti del progetto

| Componente | Descrizione |
|------------|-------------|
| `prolog/solver.pl` | Base di conoscenza Prolog con algoritmi di ricerca e euristiche |
| `prolog/heuristics.pl` | Modulo dedicato alle funzioni euristiche |
| `gui/puzzle_gui.py` | Interfaccia grafica interattiva con animazioni e statistiche |
| `logic/puzzle_logic.py` | Logica e interfacciamento Python-Prolog |
| `test/test.py` | Suite di test per confronto prestazioni algoritmi |
| `docs/` | Documentazione tecnica completa in LaTeX |
| `results/` | Report di ogni test effettuato |

---

## 🔬 Dettagli tecnici

### Rappresentazione dello stato
- **Prolog**: Lista di 9 elementi `[1,2,3,4,0,6,7,5,8]` dove 0 rappresenta lo spazio vuoto
- **Python**: Matrice 3x3 NumPy per operazioni vettorizzate
- **Hash table** per stati visitati (controllo duplicati O(1))

### Ottimizzazioni implementate
- **Pruning**: Eliminazione mosse che annullano la precedente
- **Memoization**: Cache delle euristiche calcolate
- **Incremental heuristic**: Aggiornamento incrementale invece di ricalcolo
- **Symmetry reduction**: Riconoscimento configurazioni simmetriche

---

## 📦 Tecnologie utilizzate

- **SWI-Prolog 9.x** – Motore di inferenza e algoritmi di ricerca
- **Python 3.11+** – Orchestrazione, GUI e analisi dati
- **pyswip** – Bridge Python-Prolog bidirezionale
- **Tkinter + CustomTkinter** – GUI moderna e responsive
- **NumPy** – Operazioni matriciali ottimizzate
- **Matplotlib + Plotly** – Visualizzazioni interattive
- **Pandas** – Analisi statistica dei risultati

---

## 🛠️ Istruzioni per l'installazione

### 1. Prerequisiti

```bash
# Verifica versioni
python --version  # >= 3.9
swipl --version   # >= 8.4
```

### 2. Clonazione repository

```bash
git clone https://github.com/imb0ru/8puzzle-solver.git
cd 8puzzle-solver
```

### 3. Ambiente virtuale

```bash
python -m venv venv
source venv/bin/activate       # Linux/macOS
venv\Scripts\activate.bat       # Windows
```

### 4. Installazione dipendenze

```bash
pip install -r requirements.txt
```

---

## ▶️ Esecuzione del progetto

### 🎮 Interfaccia grafica principale

```bash
python app.py --help
```

### 🧪 Suite di test

```bash
python test/test.py --help
```

---

## 🎯 Possibili estensioni future

- [ ] Supporto per N-puzzle (15-puzzle, 24-puzzle)
- [ ] Machine Learning per apprendimento euristiche

---

## 👨‍💻 Autore

**Marco Ferrara**  
Matricola: 782407

Corso di Laurea Triennale in Informatica  
Università degli Studi di Bari "Aldo Moro"  
Anno Accademico 2024-2025

📧 Email: m.ferrara62@studenti.uniba.it 
