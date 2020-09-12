# Dokumentation

## Start

Die Gui läst sich einfach mit stack run starten. Es ist auch möglich stack build && stack exec sudoku-exe zu verwenden oder für vscode erstellte tasks

Der Build wurde unter Windows erstellt und ich kann nicht garantieren, dass das Program auch unter Linux 100% funktioniert.
Damit das Program unter Windows funktioniert musste ich regex-posix < 0.96.0.0 (es scheint eine Verknüpfung zu einer .h ist fehlerhaft als .96 verwendet habe) einschränken und lts-14.27(ghc 8.6.5) verwenden. Falls dies zu Problemen unter Linux führt kann diese Einschränkung einfach entfernt werden.

## GUI

Wurde mit dem 3penny Framework erstellt. Es wurde mit Absicht minimal gehalten.

Der next Button erstellt das nächste Sudoku. Es wird an die Seite unten hinzugefügt.

Der Solve Button löst im moment nur das erste Sudoku und wurde für the restlichen deaktiviert
Der Solver ist im moment leider nicht in der Lage andere zu lösen.

### Intern

Es wird mit einer internen IORef variable gespeichert wie oft next aufgerufen wurde um sicher zu stellen das ein neues Sudoku geladen wird
Das Spiel wird ebenfalls mit IORef variablen spielbar gemacht

# GameField

Stellt die Typklassen für das Sudoku zur Verfügung.

- Digit
- Sudoku
- SudokuField

Ebenfalls stellt es einige simple Funktionen zur Verfügung.

GameField ist mehr optimiert für einfache Handhabung, deswegen verwende ich ein anderes Format für den Solver.

# Utility

Stellt Konvertierung-Funktionen zur Verfügung.

- Digit <-> Char
- [Digit]<->Sudoku
- String <-> Sudoku

Import und Export zu einer txt Datei.

Verschieden Sudoku zum debuggen

Die meisten der Funktionen sind für die GUI

## Solver

Stellt eine Funktion zur Verfügung, welche ein Sudoku löst.

### SolverUtil

Stellt Hilfsfunktionen für den Solver zur Verfügung.

- [] <-> Vektor Sudoku
- findet leere Felder

## Quellcode Dokumentation

Der Quellcode ist mit Kommentaren versehen, die die Funktionsweise erklären.

### Erklärung für >>> Kommentare

`-- >>> completeSudokuValid (vecSudoku initSudokuField3) -- True`

Kommentare wie diese sind dokumentierte Beispiele der Funktion.(man kann aus diesen auch leicht Test erstellen)
In VSCode kann man mit der Haskell Extension diese auch ausführen.
