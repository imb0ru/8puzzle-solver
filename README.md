# 🧩 8-Puzzle AI Solver
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

Il progetto affronta il problema dell'8-puzzle implementando e confrontando diversi algoritmi di ricerca classici:

### 🔍 Algoritmi implementati

| Algoritmo | Tipo | Caratteristiche | Complessità |
|-----------|------|-----------------|-------------|
| **A\*** | Informata | Ottimale, euristica combinata avanzata | O(b^d) |
| **BFS** | Non informata | Trova soluzione ottimale, alta memoria | O(b^d) spazio |
| **Greedy Best-First** | Informata | Veloce ma non ottimale | O(b^d) |

### 🧮 Euristica A* Combinata

L'algoritmo A* utilizza un'**euristica combinata** che integra:
- **Manhattan Distance**: Somma delle distanze Manhattan di ogni tessera dalla sua posizione obiettivo
- **Misplaced Tiles**: Numero di tessere in posizione errata

La combinazione di queste euristiche garantisce:
- ✅ **Ammissibilità**: non sovrastima mai il costo reale
- ✅ **Informatività**: guida efficacemente la ricerca
- ✅ **Ottimalità**: trova sempre la soluzione migliore

---

## 🔬 Dettagli tecnici

### Rappresentazione dello stato
- **Prolog**: Lista di 9 elementi `[1,2,3,4,0,6,7,5,8]` dove 0 rappresenta lo spazio vuoto
- **Python**: Matrice 3x3 NumPy per operazioni vettorizzate
- **Hash table** per stati visitati (controllo duplicati O(1))

### Ottimizzazioni implementate
- **Pruning**: Eliminazione mosse che annullano la precedente
- **Memoization**: Cache delle euristiche calcolate

---

## 📦 Tecnologie utilizzate

- **SWI-Prolog 9.x** – Motore di inferenza e algoritmi di ricerca
- **Python 3.11+** – Orchestrazione, GUI e analisi dati
- **pyswip** – Bridge Python-Prolog bidirezionale
- **Tkinter** – GUI moderna e responsive
- **Pandas** – Analisi statistica dei risultati

---

## 📂 Struttura del Progetto

```
8-puzzle-solver/
│
├── app.py                # Entry point principale
├── requirements.txt      # Dipendenze Python
├── README.md             # Documentazione
├── .gitignore            # Elenco dei file e cartelle da escludere dal versionamento
│
├── docs/                 
│   └──  8puzzle.pdf      # Documentazione progetto
├── gui/
│   └── puzzle_gui.py     # Interfaccia grafica Tkinter
│
├── logic/
│   └── puzzle_logic.py   # Logica di business e bridge Python-Prolog
│
├── prolog/
│   ├── solver.pl         # Implementazione algoritmi in Prolog
│   └── heuristics.pl     # Funzioni euristiche
│
├── test/
│   └── test.py      # Suite di test
│
└── results/              # Output di test
```

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
python app.py
```

Si consiglia per il primo utilizzo di eseguire:

```bash
python app.py --help
```

### 🧪 Suite di test

```bash
python test/test.py
```

Si consiglia per il primo utilizzo di eseguire:

```bash
python test/test.py --help
```

---

## 🎯 Possibili estensioni future

- [ ] Supporto per puzzle NxN generalizzato
- [ ] Machine Learning per predizione mosse

---

## 👨‍💻 Autore

**Marco Ferrara**  
Matricola: 782407

Corso di Laurea Triennale in Informatica  
Università degli Studi di Bari "Aldo Moro"  
Anno Accademico 2024-2025

📧 Email: m.ferrara62@studenti.uniba.it 
