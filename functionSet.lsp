(defun quadrat (n)
    (* n n))

(defun suma3 (n1 n2 n3) 
            (+ n1 n2 n3))

(defun tercerF (l)
    (car (cddr l)))

(defun darrer (l)
    (cond ((eq (cdr l) nil) (car l)) 
        (t (darrer (cdr l)))))

(defun long (l)
    (cond ((null l) 0)
    (t (+ (long (cdr l)) 1))))

(defun pertany (a l)
    (cond ((null l) nil)
          ((eq a (car l)) t)
          (t (pertany a (cdr l)))))

(defun exp (n e)
    (cond ((= e 1) n)
          (t (* n (exp n (- e 1))))))

(defun dividir (n d)
    (cond ((< (- n d) d) 1)
          (t (+ (dividir (- n d) d) 1))))


(defun parell (n)
    (cond ((= (* 2 (dividir n 2)) n) t) ;; 4 / 2 = 2; 2 * 2 == 4 PAR
          (t nil)))                     ;; 3 / 2 = 1; 1 * 2 = 2 != 3 IMPAR
         

(defun senars (l)
        (cond   ((null l) l)
                ((eq (parell (car l)) nil) (cons (car l) (senars (cdr l))))
                (t (senars(cdr l)))))


;; Definir la funció borra per eliminar la primera
;; aparició d’un element dins una llista
(defun borra (el l)
        (cond   ((null l) l)
                ((eq el (car l)) (cdr l))
                (t (cons (car l) (borra el (cdr l))))))

;; Com es modificaria la funció borra si en volem
;; eliminar totes les aparicions ?

(defun borraTot (el l)
        (cond   ((null l) l)
                ((eq el (car l)) (borraTot el (cdr l)))
                (t (cons (car l) (borraTot el (cdr l))))))

;; Definir la funció rdc que torna tots els elements
;; d’una llista menys el darrer     
(defun rdc (l)
    (cond   ((null (cdr l)) ())
            (t (cons (car l) (rdc (cdr l))))))    

;; Definir la funció snoc que afegeix un element al
;; final d’una llista       

(defun snoc (el l)
    (cond   ((null l) (list el))
            (t (cons (car l) (snoc el (cdr l))))))

; Definir la funció escala per multiplicar tots els
; elements d’una llista per un número

(defun escala (n l)
     (cond  ((null l) nil)
            (t (cons (* n (car l)) (escala n (cdr l))))))    


;; Definir les funcions màxim i mínim d’una llista de
;; números

(defun maxim (l)
    (cond   ((null (cdr l)) (car l))
            ((>= (car l) (maxim (cdr l))) (car l))
            (t (maxim (cdr l)))))

(defun minim (l)
    (cond   ((null (cdr l)) (car l))
            ((< (car l) (minim (cdr l))) (car l))
            (t (minim (cdr l)))))


;; Escriure una funció per ordenar una llista de
; números amb el mètode de selecció directa (trobar
; el mínim a cada passa i posar-ho al principi)

(defun ordena (l)
    (cond   ((null l) ())
            (t (cons (minim l) (ordena (borra (minim l) l))))))


; Escriure la funció invertir que donada una llista la
; gira al revés (sense utilitzar reverse)

(defun invertir (l)
    (cond   ((null l) nil)
            (t (snoc (car l)  (invertir(cdr l))))))

;;  Escriure una funció per borrar l’enèsim element
;; d’una llista

(defun borrarN (n l)
    (cond   ((null l) nil)
            ((= n 1) (cdr l))
            (t (cons (car l) (borrarN (- n 1) (cdr l))))))


; Escriure una funció que compti el número de
; vegades que una expressió apareix a una llista

(defun vegades (el l)
    (cond   ((null l) 0)
            ((equal el (car l)) (+ (vegades el (cdr l)) 1))
            (t (vegades el (cdr l)))))


; Escriure una funció que compti el número total
; d’àtoms (element individual) que hi ha dins una llista

(defun atoms (l)
    (cond   ((null l) 0)
            ((listp (car l)) (+ (atoms (car l))
            (atoms (cdr l))))
            (t (+ 1 (atoms (cdr l))))))


;; Escriure una funció que donada una llista retorni la
;; mateixa llista sense els n primers elements

(defun treuPrimers (n l)
    (cond   ((null l) nil)
            ((= n 1) (cdr l))
            (t (treuPrimers (- n 1) (cdr l)))))


; Escriure una funció que donada una llista torni els
; primers n elements

(defun tornaPrimers (n l)
    (cond   ((null l) nil)
            ((= n 0) nil)
            (t (cons (car l) (tornaPrimers (- n 1) (cdr l))))))


;; Escriure una funció per insertar un element a la
;; posició enèsima d’una llista