module Prefix where


prefix :: (A -> A -> A) -> A -> [A] -> [A]
prefix f a b | (length b > 0) = (a:[])
             | True           = (iterate f (a:b) ((length b) - 1))
--Ich mag die Parameter nicht so...
iterate :: (A -> A -> A) -> [A] -> Num -> [A]
iterate f a b = | (b == 0) = a
                | True     = (iterate f (take b a):(step f (take (b + 1) a)):(drop (b + 1) a) (b-1))
--Der iteriert jetzt die Funktion step
--über alle Elemente
--von hinten nach vorne
--außer das erste Element
step :: (A -> A -> A) -> [A] -> [A]
step f a | ((length a) == 1) = a
         | True              = (step f ((f (head a) (head (tail a))):(tail a)))
--Berechnet nach der in der Aufgabe gegebenen Funktion
--den Wert für eine beliebige Stelle
--(Er erwartet eine Liste der Elemente bis zu dem berechnenden Element
--und gibt eine einelementige Liste mit der Kösung wieder)
