(in-package :user)

(compile-file "procura.lisp")
(load "procura")
(load "turnos-teste")

(defconstant +max-tempo-execucao+ 5)

(defconstant +duracao-max-turno+ 480)
(defconstant +duracao-min-turno+ 360)
(defconstant +duracao-pausa+ 40)
(defconstant +duracao-antes-refeicao+ 240)
(defconstant +local-inicial+ 'L1)

(defconstant +temperatura-minima+ 40E-20)

(defvar *tempo-execucao-inicial* (get-internal-run-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Funcoes auxiliares
;;;

(defun remove-nth (n list)

	"Remove o n-esimo elemento de uma lista.

	 Argumentos:
	 * n -- o indice do elemento a remover.
	 * list -- a lista a modificar.
	 Returno:
	 * A lista sem o n-esimo elemento"

  (declare
    (type (integer 0) n)
    (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))


(defun calcula-tempo-execucao ()

	"Calcula o tempo de execucao do programa.

	 Retorno:
	 * O tempo de execucao desde o inicio do programa em segundos."

	(float (/ (- (get-internal-run-time) *tempo-execucao-inicial*) internal-time-units-per-second)))


(defun gera-numero-decimal ()

	"Gera um numero decimal entre 0 e 1 com 3 casas decimais.

	 Retorno:
	 * Um numero decimal entre 0 e 1 com 3 casas decimais."

	(float (/ (random (1+ 10E10)) 10E10)))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Definicao da estrutura que representa um turno
;;; e funcoes auxiliares de acesso a elementos da estrutura
;;; e criacao da mesma
;;;

(defstruct turno
	tarefas
	duracao-total)


(defun cria-turno (tarefas
					&key (duracao-total NIL))
	(if (null duracao-total)
		(let ((primeira-tarefa (car tarefas))
		       (ultima-tarefa (car (last tarefas)))
		       (tempoi-primeira-tarefa (nth 2 (car tarefas)))
		       (tempof-ultima-tarefa (nth 3 (car (last tarefas)))))
		    
		    ;;verifica se existem deslocacoes inicial e final a contabilizar para a duracao
		    (if (not (eq (nth 0 primeira-tarefa) +local-inicial+)) ;inicial
		        (setf tempoi-primeira-tarefa (- tempoi-primeira-tarefa +duracao-pausa+)))
		    (if (not (eq (nth 1 ultima-tarefa) +local-inicial+)) ;final
		        (setf tempof-ultima-tarefa (+ tempof-ultima-tarefa +duracao-pausa+)))

		    (setf duracao-total (max +duracao-min-turno+ (-  tempof-ultima-tarefa tempoi-primeira-tarefa)))))

	(make-turno
		:tarefas tarefas
		:duracao-total duracao-total))

(defun last-turno (turno)

	"A funcao last, quando aplicada a um turno, devolve uma lista com a tarefa. 
	 Esta funcao last modificada devolve apenas a tarefa, em vez de uma lista 
	 com uma tarefa.
	 Argumentos:
	 * turno -- o turno do qual sera obtida a ultima tarefa.
	 Retorno:
	 * Uma lista de 4 elementos, correspondente a uma tarefa."

	 (car (last (turno-tarefas turno))))

(defun tem-vaga-t1 (turno1 t2f)
	;(format t "tem-vaga-t1 IN ~%")
	;(format t "~A ~%" turno1)
  (let* ((turno (reverse (turno-tarefas turno1)))
         (primeiro (first turno))
         (resto (rest turno))
         (segundo (first resto))
         (pausas 0))
        
    (loop
      (if (null segundo)
      	(return nil))
      (when (> (- t2f (third primeiro)) +duracao-antes-refeicao+)
      	;(format t "tem-vaga-t1 OUT ~%")
          (return nil))     
      (if (equal (first primeiro) (second segundo))
          (setf pausas (+ pausas +duracao-pausa+))
        (setf pausas (+ pausas (* 2 +duracao-pausa+))))
      (when (>= (- (third primeiro) (fourth segundo) pausas) 0)
      ;	(format t "tem-vaga-t1 OUT ~%")
          (return T))
        
      (setf primeiro (first resto))
      (setf resto (rest resto))
      (setf segundo (first resto))
      (setf pausas 0))))


(defun tem-vaga-t2 (turno t1i)
  ;(format t "tem-vaga-t2 IN ~%")
  (let* ((turno (turno-tarefas turno))
  	     (primeiro (first turno))
         (resto (rest turno))
         (segundo (first resto))
         (pausas 0))
    
    (loop
      (if (null segundo)
      	(return nil))
      (if (equal (second primeiro) (first segundo))
          (setf pausas (+ pausas +duracao-pausa+))
        (setf pausas (+ pausas (* 2 +duracao-pausa+))))
      (when (> (- (fourth primeiro) t1i) (- +duracao-antes-refeicao+ pausas))
      	;(format t "tem-vaga-t2 OUT ~%")
          (return nil))
      (when (>= (- (third segundo) (fourth primeiro) pausas) 0)
      	;(format t "tem-vaga-t2 OUT ~%")
          (return T))
      
      (setf primeiro (first resto))
      (setf resto (rest resto))
      (setf segundo (first resto))
      (setf pausas 0))))


(defun turnos-unificaveis-p (turno1 turno2)

	"Une dois turnos, tendo em conta as restricoes impostas.
	 Argumentos:
	 * turno1 -- Lista de tarefas.
	 * turno2 -- Lista de tarefas.
	 Retorno:
	 * Novo turno gerado de unir turno1 e turno2 se puderem ser unidos,
	   NIL caso nao seja possível."
	   ;(format t "turnos-unificaveis-p IN ~%")
	(let* ((primeira-tarefa-t1 (car (turno-tarefas turno1)))
		   (ultima-tarefa-t1 (last-turno  turno1))
		   (primeira-tarefa-t2 (car (turno-tarefas turno2)))
		   (ultima-tarefa-t2 (last-turno turno2))

		   (tempo-inicio-t1 (nth 2 primeira-tarefa-t1))
		   (tempo-inicio-t2 (nth 2 primeira-tarefa-t2))
		   (tempo-fim-t1 (nth 3 ultima-tarefa-t1))
		   (tempo-fim-t2 (nth 3 ultima-tarefa-t2))

     	   (local-inicio-t1 (nth 0 primeira-tarefa-t1))
		   (local-inicio-t2 (nth 0 primeira-tarefa-t2))
		   (local-fim-t1 (nth 1 ultima-tarefa-t1))
		   (local-fim-t2 (nth 1 ultima-tarefa-t2))

		   (tempo-total-t1 (- tempo-fim-t1 tempo-inicio-t1))
		   (tempo-total-t2 (- tempo-fim-t2 tempo-inicio-t2)))

		(when (not (eq local-inicio-t1 +local-inicial+)) 
			(setf tempo-inicio-t1 (- tempo-inicio-t1 +duracao-pausa+))
			(setf tempo-total-t1 (+ tempo-total-t1 +duracao-pausa+)))

		(when (not (eq local-fim-t1 +local-inicial+)) 
			(setf tempo-total-t1 (+ tempo-total-t1 +duracao-pausa+)))
	  
	  	(when (not (eq local-fim-t2 +local-inicial+)) 
			(setf tempo-fim-t2 (+ tempo-fim-t2 +duracao-pausa+))
			(setf tempo-total-t2 (+ tempo-total-t2 +duracao-pausa+)))

	  	(when (not (eq local-inicio-t2 +local-inicial+)) 
			(setf tempo-total-t2 (+ tempo-total-t2 +duracao-pausa+)))

	  	(let ((une? NIL)
	  		  (duracao-uniao (- tempo-fim-t2 tempo-inicio-t1)))
	  		;(format t "t1 = ~A  t2 = ~A ~%" turno1 turno2)
	  		;(format t "sobrepostas? ~A ~A ? =~A ~%" ultima-tarefa-t1 primeira-tarefa-t2 (tarefas-sobrepostas-p ultima-tarefa-t1 primeira-tarefa-t2))
	  		;(format t "duracao= ~A ~%" duracao-uniao)
	  		;(format t "t1 refeicao? = ~A ~%" (<= tempo-total-t1 +duracao-antes-refeicao+))
	  		;(format t "t2 refeicao? = ~A ~%" (<= tempo-total-t2 +duracao-antes-refeicao+))
			(if (and (<=  duracao-uniao +duracao-max-turno+) ;tempototal < duracao max turno
					 (not (tarefas-sobrepostas-p ultima-tarefa-t1 primeira-tarefa-t2))
			         (or (and (> tempo-total-t1 +duracao-antes-refeicao+) ;tempo(t1) > 240
						      (<= tempo-total-t2 +duracao-antes-refeicao+)) ; tempo(t1) > 240 AND tempo(t2) <= 240 

						 (and (> tempo-total-t2 +duracao-antes-refeicao+) ; tempo(t2) > 240
			            	  (<= tempo-total-t1 +duracao-antes-refeicao+)) ; tempo(t1) <= 240 AND tempo(t2) > 240

	 					 (and (<= tempo-total-t1 +duracao-antes-refeicao+)
						      (<= tempo-total-t2 +duracao-antes-refeicao+)
						      (or (<= duracao-uniao +duracao-antes-refeicao+);tempo(t1) <= 240 AND tempo(t2) <= 240 AND tempo(t1) + tempo(t2) <= 240
								  (and (> duracao-uniao +duracao-antes-refeicao+) ;tempo(t1) <= 240 AND tempo(t2) <= 240 AND tempo(t1) + tempo(t2) > 240
								       (or (and (>= (- tempo-inicio-t2 tempo-fim-t1) +duracao-pausa+)
										  	    (eq local-inicio-t2 local-fim-t1)) ;existe espaco entre os turnos para refeicao?

										   (and (>= (- tempo-inicio-t2 tempo-fim-t1) (* 2 +duracao-pausa+))
										        (not (eq local-inicio-t2 local-fim-t1))) ;existe espaco entre os turnos para refeicao e transporte?

										   (and (> (length (turno-tarefas turno1)) 1) 
										   		   (tem-vaga-t1 turno1 tempo-fim-t2))
										   (and (> (length (turno-tarefas turno2)) 1)    
										   		   (tem-vaga-t2 turno2 tempo-inicio-t1))))))))
						(setf une? T))
			;(format t "turnos-unificaveis-p OUT ~%")
			une?)))


(defun junta-turnos (estado turno1 index1 turno2 index2)


  (let ((novo-estado (copy-list estado)))
    (cond ((turnos-unificaveis-p turno1 turno2)
    	(let* ((novas-tarefas (turno-tarefas (nth index1 novo-estado))))
    		;(format t "~% ################### ~%")

			(setf novas-tarefas (append novas-tarefas (turno-tarefas (nth index2 novo-estado))))

			(setf (nth index1 novo-estado) (cria-turno novas-tarefas ))

			(setf novo-estado (remove-nth index2 novo-estado)))

			novo-estado))))


(defun conta-pausas-turno (turno)

	"Calcula a quantidade de tempo gasta fora de tarefas num turno
	 Argumentos:
	 * turno -- struct que representa um turno.
	 Retorno:
	 * Um inteiro correspondente ao numero de tarefas."

  (let ((primeiro (first (turno-tarefas turno)))
        (resto (rest (turno-tarefas turno)))
        (resultado 0))



    (loop 
      (if (null resto) (return-from conta-pausas-turno resultado))
      (setf resultado (+ resultado (- (third (first resto)) (fourth primeiro))))
      (setf primeiro (first resto))
      (setf resto (rest resto)))resultado))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Funcoes que operam sobre tarefas
;;;

(defun tarefa-calcula-duracao (tarefa)

	"Calcula a duracao de uma tarefa.
	 Argumentos:
	 * tarefa -- Lista de 4 elementos que representa uma tarefa.
	 Retorno:
	 * Um inteiro que representa a duracao da tarefa em minutos."
	(- (nth 3 tarefa) (nth 2 tarefa)))


(defun tarefas-sobrepostas-p (tarefa1 tarefa2)

	"Predicado que verifica se duas tarefas se sobrepoem.
	 Argumentos:
	 * tarefa1 -- Lista de 4 elementos.
	 * tarefa2 -- Lista de 4 elementos.
	 Retorno:
	 * T se se sobrepuserem, NIL caso contrario."
	 ;(format t "~A ~A ~%" tarefa1 tarefa2)
	;;se for necessaria uma pausa para deslocacao entre as duas tarefas 
	;;deve ser contabilizado esse tempo extra
	(let ((pausa? 0)) ;0 ou 1 em vez de booleano para poder ser usado na multiplicacao
		(if (not (eq (nth 1 tarefa1) (nth 0 tarefa2)))
			(setf pausa? 1))

		(< (nth 2 tarefa2) (+ (nth 3 tarefa1) (* pausa? +duracao-pausa+)))))


;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Funcoes que operam sobre estados
;;;

(defun operador (estado)

	"Gera a lista de sucessores dado um estado, em que cada sucessor
	 consiste em 1 uniao de 2 turnos, mantendo-se os outros turnos iguais.
	 Argumentos:
	 * estado -- lista de turnos.
	 Retorno:
	 * Lista de estados que representa os estados gerados."

	(let ((primeiro (first estado))
		  (resto (rest estado))
		  (resto-aux (rest estado))
		  (i 0)
		  (j 1)
		  (resultado '())
		  (juntou? NIL))
    	(loop 
    		;(format t "RESULTADO! ~A ~%" resultado)
      		(if (null resto) (return))
      		(progn
        		(setf juntou? (junta-turnos estado primeiro i (first resto-aux) j)) 
        		;(format t "PODE JUNTAR ~A ~A  = ~A ~%" primeiro (first resto-aux) juntou?)
    			(if juntou?
        			(setf resultado (append resultado (list juntou?))))
        		(setf resto-aux (rest resto-aux))
        		(setf j (1+ j))
        (cond ((null resto-aux) 
               (setf primeiro (first resto))
               (setf i (1+ i))
               (setf resto (rest resto))
               (setf resto-aux resto)
               (setf j (1+ i))))))resultado))


(defun operador-contrario (estado)

	"Gera a lista de antecessores dado um estado, em que cada antecessor
	 consiste numa separacao de 1 turno em 2, mantendo-se os outros turnos iguais.

	 Argumentos:
	 * estado -- lista de turnos.
	 Retorno:
	 * Lista de estados que representa os antecessores."

	(let ((antecessores)
		  (prefixo)
		  (sufixo (cdr estado))
		  (novo-estado))

		(dolist (turno estado)
			(dotimes (n (1- (length (turno-tarefas turno))))
				(setf novo-estado (append prefixo (list (cria-turno (subseq (turno-tarefas turno) 0 (1+ n))))))
				(when (> (length (turno-tarefas turno)) 1)
					(setf novo-estado (append novo-estado (list (cria-turno (subseq (turno-tarefas turno) (1+ n) (length (turno-tarefas turno))))) sufixo)))
				(setf antecessores (append antecessores (list novo-estado))))
			(setf prefixo (append prefixo (list turno)))
			(setf sufixo (cdr sufixo)))

		antecessores))

(defun n-turnos (estado)

	"Calcula o numero de turnos num estado.
	 Argumentos:
	 * estado -- lista de turnos.
	 Retorno:
	 * Inteiro que representa o numero de turnos."

 	(length estado))


(defun objectivo-p (estado)

	"Testa se um estado e objectivo. Um estado e objectivo 
	 se nao puder gerar sucessores.
	 Argumentos:
	 * estado -- lista de turnos.
	 Retorno:
	 * T se for objectivo, NIL caso contrario."
	(or (null (operador estado))
		(>= (calcula-tempo-execucao) +max-tempo-execucao+)))


(defun custo-estado (estado)
	
	"Calcula um custo de um estado, correspondente
	 a soma dos custos de cada turno.
	 Argumentos:
	 * estado -- lista de turnos.
	 Retorno:
	 * Um turno que representa o custo do estado"

	;(format t " cost ~A ~%" estado)
	 (let ((duracao 0))
	 	(dolist (turno estado)
	 		(setf duracao (+ duracao (turno-duracao-total turno))))
	 	duracao))


(defun heuristica-alternativa (estado)
  (let ((resultado 0))
    (dolist (turno estado) (setf resultado (+ resultado (conta-pausas turno))))
    (setf resultado (+ (list-length estado) (/ resultado 1000)))))

(defun n-turnos-curtos (estado)
  (let ((numero-turnos-curtos 0))
    (dolist (turno estado)
      (if (= (turno-duracao-total turno) +duracao-min-turno+)
          (incf numero-turnos-curtos)))
    numero-turnos-curtos))


(defun le-estado-inicial (input)

	"Le um problema na sua representacao externa e transforma-o para a sua
	 representacao interna, transformando cada tarefa num turno.
	 Argumentos:
	 * input-- lista de tarefas correspondente a 
	               representacao externa de um problema a resolver.
	 Retorno:
	 * Lista de turnos correspondente ao estado inicial de um problema."

  (let ((problema '()))
    (dolist (i (sort input #'< :key #'third))
		(let ((novo-turno (cria-turno (list i))))

      		(setf problema (append problema (list novo-turno)))))
  	problema))

(defun conta-pausas-estado (estado)
	(let ((total 0))
		(dolist (turno estado)
			(setf total (+ total (conta-pausas-turno turno))))
		;(format t "~A  ~A ~%"  estado (+ total (custo-estado estado)))
	total))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Estrategias de procura implementadas
;;;


(defun sondagem-iterativa (problema)

	"Utiliza a estrategia de songagem iterativa para resolver um problema de procura.
	 Argumentos:
	 * problema -- struct de representacao de um problema.
	 Retorno:
	 * O estado final obtido."

	(let* ((estado-inicial (problema-estado-inicial problema))
		  (estado-actual estado-inicial)
		  (melhor-solucao estado-inicial)
		  (operador (car (problema-operadores problema))) ;podemos fazer car, so ha 1 operador..
		  (objectivo? (problema-objectivo? problema))
		  (sucessores '())
		  (nos-expandidos 1)
		  (nos-gerados 0))
		(loop
			(loop
				(when (funcall objectivo? estado-actual)
					(if (< (custo-estado estado-actual) (custo-estado melhor-solucao))
						(setf melhor-solucao estado-actual))
					(return))
				(setf sucessores (funcall operador estado-actual))
				(setf nos-gerados (+ nos-gerados (length sucessores)))
				(setf nos-expandidos (1+ nos-expandidos))
				(setf estado-actual (nth (random (length sucessores)) sucessores))
			)
			(if (>= (calcula-tempo-execucao) +max-tempo-execucao+)
				(return-from sondagem-iterativa (append (list melhor-solucao) (list nos-expandidos) (list nos-gerados))))

			(setf estado-actual estado-inicial))))


(defun ilds (estado)
  (let* ((tempoi (get-universal-time))
         (sucessores (operador estado))
         (melhor estado)
         (customelhor (custo-estado melhor))
         (actual)
         (custoactual)
         (caminho estado)
         (nos-expandidos 1)
         (nos-gerados 0))
    
    (if (null sucessores)
        (return-from ilds melhor))
    
    (setf caminho (append (list (first sucessores)) (last sucessores) caminho))
    (setf actual (first caminho))
    (setf caminho (rest caminho))
    (setf nos-gerados (length sucessores))
    (incf nos-expandidos)                         
                              
    (loop
      (if (or (> (- (get-universal-time) tempoi) 60)
              (equal caminho estado))
          (return-from ilds (append (list melhor) (list nos-gerados) (list nos-expandidos))))
      
      (if (objectivo-p actual)
          (progn
            (setf custoactual (custo-estado actual))
            (if (< custoactual customelhor)
                (progn
                  (setf melhor actual)
                  (setf customelhor custoactual)))))
      
      (setf sucessores (operador actual))
      (if sucessores
          (setf caminho (append (list (first sucessores)) (last sucessores) caminho)))
      
      (setf actual (first caminho))
      (setf caminho (rest caminho))
      (setf nos-gerados (+ nos-gerados (length sucessores)))
      (incf nos-expandidos))
    
    (append (list melhor) (list nos-gerados) (list nos-expandidos))))



;;;
;;; 	Tempera Simulada
;;;

(defun tempera-simulada (problema)

	"Utiliza tempera simulada para resolver um problema de procura.
	 Argumentos:
	 * problema -- struct de representacao de um problema.
	 Retorno:
	 * O estado final obtido."

	 (let* ((melhor-solucao (problema-estado-inicial problema))
	 	   (operadores (problema-operadores problema)) 
		   (i 1)
		   (estado-actual melhor-solucao)
		   (proximo-estado)
		   (temperatura)
		   (sucessores)
		   (custo-melhor-solucao (custo-estado melhor-solucao))
		   (custo-proximo-estado)
		   (usa-funcao-temp? T)
		   (custo-actual (custo-estado estado-actual))
		   (nos-expandidos 1)
		   (nos-gerados 0))


	 	(flet ((temperatura-exp (k)
	 				;(format t "~A ~%" k)
	 				(expt 0.99 k)))
	 	(loop
	 		(if usa-funcao-temp?
	 			(progn
	 				(setf temperatura (temperatura-exp i))
	 				(when (< (temperatura-exp i) +temperatura-minima+)
	 					(setf usa-funcao-temp? NIL)
	 					(setf temperatura +temperatura-minima+)))
	 			(setf temperatura +temperatura-minima+))

	 		(dolist (operador operadores)
      			(setf sucessores (append (funcall operador estado-actual) sucessores)))
	 		;(format t "~A ~%" (length sucessores))
	 		(setf nos-gerados (+ nos-gerados (length sucessores)))
	 		;(format t "~A ~%" estado-actual)
	 		;(format t "~% $$$$$$$$$$$$$$$ ~%")
	 		;(format t "~A  ~%"  (operador estado-actual))
	 		;(format t " ~A  ~A ~%" temperatura i)
	 		;(format t "~% ############### ~%")
	 		;(format t "~A  ~%"  (operador-contrario estado-actual))
	 		;(format t "~% !!!!!!!!!!!!!!!! ~%")
	 		(setf proximo-estado (nth (random (length sucessores)) sucessores))

	 		(setf custo-proximo-estado (custo-estado proximo-estado)) 
	 	

	 		(if (< custo-proximo-estado custo-actual)
		 		(progn
		 			(setf estado-actual proximo-estado)

		 			;;custo-proximo-estado = custo do estado actual devido a linha de cima..
		 			(when (< custo-proximo-estado custo-melhor-solucao) 
		 				(setf melhor-solucao estado-actual)
		 				(setf nos-expandidos (1+ nos-expandidos))
		 				(setf custo-melhor-solucao custo-proximo-estado)))

		 		(if (< (gera-numero-decimal) temperatura)
		 			(setf nos-expandidos (1+ nos-expandidos))
		 			(setf estado-actual proximo-estado)))

	 		(setf custo-actual (custo-estado estado-actual))

			(if (>= (calcula-tempo-execucao) +max-tempo-execucao+)
				(return-from tempera-simulada (append (list melhor-solucao) (list nos-expandidos) (list nos-gerados))))
			(setf sucessores NIL)
	 		(setf i (1+ i))))))



;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;
;;;		Funcao Principal
;;;

(defun faz-afectacao (problema estrategia)

	"Resolve um problema de afectacao de recursos.
	 Argumentos:
	 * problema -- representacao externa de um problema, uma lista de tarefas.
	 * estrategia -- uma string que representa o nome da estrategia a usar.
	 Retorno:
	 * Melhor distribuicao de tarefas obtida."

	;;comecar por transformar a lista de tarefas numa lista de turnos

	(setf *tempo-execucao-inicial* (get-internal-run-time))
	(let ((solucao NIL)
		  (tempo 0)
		  (funcao-heuristica NIL)
		  (usa-procura? NIL)
		  (estado-final NIL)
		  (nos-expandidos)
		  (nos-gerados))
		(cond 
			((equal estrategia "melhor.abordagem")
				NIl)
			((equal estrategia "a*.melhor.heuristica")
				(progn
					(setf funcao-heuristica #'conta-pausas-estado)
					(setf estrategia "a*")
					(setf usa-procura? T)))
			((equal estrategia "a*.melhor.heuristica.alternativa")
				(progn
					(setf funcao-heuristica #'heuristica-alternativa)
					(setf estrategia "a*")
					(setf usa-procura? T)))
			((equal estrategia "sondagem.iterativa")
				(progn 
					(setf problema (cria-problema (le-estado-inicial problema)
						  (list #'operador)
						  :objectivo? #'objectivo-p
						  :custo #'custo-estado
						  :heuristica #'n-turnos))
				(setf solucao (sondagem-iterativa problema))
				(setf estado-final (car solucao))
				(nos-expandidos (nth 1 solucao))
				(nos-gerados (nth 2 solucao))))
			((equal estrategia "ILDS")
				(progn 
					(setf solucao (ilds (le-estado-inicial problema)))))
			((equal estrategia "abordagem.alternativa")
				(progn 
					(setf problema (cria-problema (le-estado-inicial problema)
						  (list #'operador #'operador-contrario)
						  :objectivo? #'objectivo-p
						  :custo #'custo-estado
						  :heuristica #'n-turnos))
				(setf solucao (tempera-simulada problema))
				(setf estado-final  (car solucao))
				(setf nos-expandidos (nth 1 solucao))
				(setf nos-gerados (nth 2 solucao)))))
		(when usa-procura?
			(setf problema (cria-problema (le-estado-inicial problema)
									  (list #'operador)
									  :objectivo? #'objectivo-p
									  :heuristica funcao-heuristica))
			(setf solucao (procura problema estrategia))

			(setf estado-final (car (last (car solucao))))
			(setf nos-expandidos (nth 2 solucao))
			(setf nos-gerados (nth 3 solucao)))

		;;Se tiver sido resolvido usando procura.lisp
		;;e necessario obter o ultimo elemento do caminho devolvido
		;(format t "~A ~%" solucao)
		(setf tempo (calcula-tempo-execucao))
		(append (list estado-final) (list (n-turnos solucao)) (list tempo) (list nos-expandidos) (list nos-gerados))))

		#|	(setf prob1 (cria-problema (le-estado-inicial p1)
									  (list #'operador)
									  :objectivo? #'objectivo-p
									  :custo #'custo-estado
									  :heuristica "lh2"))|#



