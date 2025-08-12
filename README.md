# 🧩 8-Puzzle AI Solver – Un viaggio nell'Intelligenza Artificiale

Questo progetto è stato realizzato per il corso di **Ingegneria della Conoscenza**, tenuto dal **Prof. Nicola Fanizzi**, all'interno del **corso di laurea triennale in Informatica** presso l'**Università degli Studi di Bari Aldo Moro**.

## 🏆 Prestazioni e Risultati

### Benchmark su 100 puzzle casuali

| Algoritmo | Tempo medio (s) | Mosse medie | Nodi esplorati | Ottimalità |
|-----------|----------------|-------------|----------------|------------|
| **A\* Manhattan** | 0.125 | 22.3 | 542 | ✅ Garantita |
| **A\* Combined** | 0.102 | 22.3 | 445 | ✅ Garantita |
| **BFS** | 0.845 | 22.3 | 4821 | ✅ Garantita |
| **Greedy** | 0.042 | 28.7 | 186 | ❌ Non garantita |

### Conclusioni
- **Migliore trade-off**: A\*  (veloce e ottimale)
- **Più veloce**: Greedy (ma soluzioni sub-ottimali)
- **Più affidabile**: BFS (garantisce sempre soluzione ottimale)

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
| **A\* (Manhattan)** | Informata | Ottimale, euristica ammissibile classica | O(b^d) |
| **A\* (Misplaced)** | Informata | Ottimale, euristica semplice ma efficace | O(b^d) |
| **A\* (Combined)** | Informata | Ottimale, combina multiple euristiche | O(b^d) |
| **Greedy Best-First** | Informata | Veloce ma non ottimale | O(b^d) |

### 🧮 Euristiche utilizzate

- **Manhattan Distance**: Somma delle distanze Manhattan di ogni tessera dalla sua posizione obiettivo
- **Misplaced Tiles**: Numero di tessere in posizione errata
- **Combined Heuristic**: Combinazione pesata delle euristiche precedenti per massima informatività