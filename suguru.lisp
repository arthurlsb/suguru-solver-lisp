(defun main ()
    
    ;;Escolher o largura do tabuleiro
    (setf largura 8)

    ;;Digitar os valores do tabuleiro (Se e posição estiver vazia é colocado o valor 0)
    (setf valores (make-array (list largura largura)
        :INITIAL-CONTENTS '((0 0 0 3 0 0 2 0) (4 0 0 0 0 0 0 0) (0 2 0 0 0 0 0 0) (0 1 5 0 0 1 5 0) (0 2 0 0 0 0 0 0) (0 0 0 0 4 0 0 4) (0 0 0 0 0 3 0 0) (0 5 0 0 0 5 0 0)))
    )    
    
    ;;Digitar a área de cada um dos valores
    (setf areas (make-array (list largura largura)
        :INITIAL-CONTENTS '((0 0 0 1 2 2 3 3) (0 4 4 1 2 2 3 3) (4 4 1 1 2 7 7 3) (4 5 5 1 6 6 7 7) (8 8 5 5 6 11 11 7) (9 8 8 5 6 12 11 11) (9 9 8 10 6 12 12 11) (9 9 10 10 10 10 12 12)))
    ) 
    
;;----------------------------------------------------------------------------------    
    
;;          MATRIZ DE VALORES:                            MATRIZ DE ÁREAS:

;;          0  0  0  3  0  0  2  0                        0  0  0  1  2  2  3  3    
;;          4  0  0  0  0  0  0  0                        0  4  4  1  2  2  3  3
;;          0  2  0  0  0  0  0  0                        4  4  1  1  2  7  7  3 
;;          0  1  5  0  0  1  5  0                        4  5  5  1  6  6  7  7 
;;          0  2  0  0  0  0  0  0                        8  8  5  5  6  11 11 7 
;;          0  0  0  0  4  0  0  4                        9  8  8  5  6  12 11 11
;;          0  0  0  0  0  3  0  0                        9  9  8  10 6  12 12 11
;;          0  5  0  0  0  5  0  0                        9  9  10 10 10 10 12 12

;;----------------------------------------------------------------------------------  
    
    (terpri)
    (write-line "Tabuleiro Inicial:")
    
    ;;Escreve no console o tabuleiro inicial
    (imprimirTabuleiro valores)

    (terpri)
    (write-line "Soluções:")
    
    ;;Chama a função resolvedor para resolver o tabuleiro
    (resolvedor valores areas)
    
    (terpri)
    (write-line "Fim!")
)

;;Função para escrever tabuleiro no console
(defun imprimirTabuleiro (valores)
    (dotimes (y largura)
        (terpri )
        (dotimes (x largura)
            (format T " ~D " (aref valores y x))))
    (terpri )
)

;;Função que de fato resolve o tabuleiro.
(defun resolvedor (valores areas)
    (if
        (block resolvedor-block
            (dotimes (y largura)
                (dotimes (x largura)
                    (if (= (aref valores y x) 0)
                        (progn
                            (dotimes (valor (numElementosArea areas y x))
                                (if (verificador valores areas y x (+ valor 1))
                                    (progn
                                        (setf (aref valores y x) (+ valor 1))
                                        (resolvedor valores areas)
                                        (setf (aref valores y x) 0)
                                    )
                                )
                            )
                            (return-from resolvedor-block NIL)
                        )
                    )
                )
            )
            T
        )
        (imprimirTabuleiro valores)
    )
)

;;Função chamada na função resolvedor que serve para saber o número de elementos (casas) de uma determinada área.
(defun numElementosArea (areas y x)
    (setf numMax 0)
    (dotimes (n largura)
        (dotimes (m largura)
            (if (= (aref areas y x) (aref areas n m))
                (setf numMax (+ 1 numMax))
            )
        )
    )    
    numMax
)

;;Função chamada na função resolvedor que verifica se um determinado valor pode ser escrito em uma posição (y,x) do tabuleiro.
;; Se puder retorna true (T), caso contrário retorna false (NIL).
(defun verificador (valores areas y x valor)
    (block outer-block
        
        ;;Verifica se o valor já está na área na qual a posição (y, x) faz parte.
        (loop for a from 0 to (- largura 1) 
            do
            (loop for b from 0 to (- largura 1) 
                do
                (if (and (= (aref areas y x) (aref areas a b)) (= valor (aref valores a b)))
                    (return-from outer-block NIL)
                )
            )
        )
    
        ;;Verifica se na posiçao à esquerda da posição (y, x) já existe esse valor, caso exista retorna NIL.
        (if (/= x 0)
            (if (= valor (aref valores y (- x 1)))
                (return-from outer-block NIL)
            )
        )   

        ;;Verifica se na posiçao em cima da posição (y, x) já existe esse valor, caso exista retorna NIL.
        (if (/= y 0)
            (if (= valor (aref valores (- y 1) x))
                (return-from outer-block NIL)
            )
        ) 

        ;;Verifica se na posiçao embaixo da posição (y, x) já existe esse valor, caso exista retorna NIL.
        (if (/= (- largura 1) y)
            (if (= valor (aref valores (+ y 1) x))
                (return-from outer-block NIL)
            )
        ) 

        ;;Verifica se na posiçao à direita da posição (y, x) já existe esse valor, caso exista retorna NIL.
        (if (/= (- largura 1) x)
            (if (= valor (aref valores y  (+ 1 x)))
                (return-from outer-block NIL)
            )
        ) 

        ;;Verifica se na posição à diagonal esquerda superior da posição (y, x) já existe esse valor, caso exista retorna NIL.
        (if (not (or (= x 0) (= y 0)))
            (if (= valor (aref valores (- y 1)  (- x 1)))
                (return-from outer-block NIL)
            )
        )

        ;;Verifica se na posição à diagonal direita superior da posição (y, x) já existe esse valor, caso exista retorna NIL.
        (if (not (or (= x (- largura 1)) (= y 0)))
            (if (= valor (aref valores (- y 1)  (+ x 1)))
                (return-from outer-block NIL)
            )
        )

        ;;Verifica se na posição à diagonal esquerda inferior da posição (y, x) já existe esse valor, caso exista retorna NIL.
        (if (not (or (= x 0) (= y (- largura 1))))
            (if (= valor (aref valores (+ y 1)  (- x 1)))
                (return-from outer-block NIL)
            )
        )

        ;;Verifica se na posição à diagonal direita inferior da posição (y, x) já existe esse valor, caso exista retorna NIL.
        (if (not (or (= x (- largura 1)) (= y (- largura 1))))
            (if (= valor (aref valores (+ y 1)  (+ x 1)))
                (return-from outer-block NIL)
            )
        )
        T
    )
)

(main)