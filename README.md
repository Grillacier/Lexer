interpréteur pour les automates à pile déterministes
analyse lexicale réalisée avec ocamllex
analyse grammaticale réalisée avec menhir

pour compiler le code : make
pour "nettoyer" les fichiers : make clean
pour executer le code ./parser "mot à tester" < "fichier automate"
(exemple : ./parser abcba < exemples_phase1/palindrome)
