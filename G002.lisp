(in-package :user)

(compile-file "procura.lisp")
(load "procura")
(load "turnos-teste")

(defconstant +max-tempo-execucao+ 295) ;5segundos de seguranca.. (295 default)
(defconstant +duracao-max-turno+ 480)
(defconstant +duracao-min-turno+ 360)
(defconstant +duracao-pausa+ 40)
(defconstant +duracao-antes-refeicao+ 240)
(defconstant +local-inicial+ 'L1)

(defvar *tempo-execucao-inicial* (get-internal-run-time))


(defstruct turno
	tarefas
	duracao-total
	duracao-conducao)


(defun cria-turno (tarefas
					&key (duracao-total NIL)
					(duracao-conducao NIL))
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

	(if (null duracao-conducao)
		(progn
			(setf duracao-conducao 0)
			(dolist (tarefa tarefas)
				(setf duracao-conducao (+ duracao-conducao (tarefa-calcula-duracao tarefa))))))
	(make-turno
		:tarefas tarefas
		:duracao-total duracao-total
		:duracao-conducao duracao-conducao))

(defun last-turno (turno)

	"A funcao last, quando aplicada a um turno, devolve uma lista com a tarefa. 
	 Esta funcao last modificada devolve apenas a tarefa, em vez de uma lista 
	 com uma tarefa.
	 Argumentos:
	 * turno -- o turno do qual sera obtida a ultima tarefa.
	 Retorno:
	 * Uma lista de 4 elementos, correspondente a uma tarefa."

	 (car (last (turno-tarefas turno))))


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


(defun tem-vaga-p (turno contrario? limite)

	"Verifica se um turno tem espaco para refeicao.
	 Argumentos:
	 * turno -- lista de tarefas.
	 * contrario? -- T o início ao fim, NIL caso contrario
	 * limite -- 
	 Retorno:
	 * T se existir espaco para refeicao, NIl caso contrario."

	(setf turno (turno-tarefas turno))
	(let ((tamanho (1- (length turno))))
		(dotimes (i tamanho)
			(if contrario? 
				(setf i (1- (- tamanho i))))
			(let ((t1 (nth i turno))
				  (t2 (nth (1+ i) turno)))
				;(format t "t1= ~A  t2= ~A ~%" t1 t2)
			    ;(format t "aux-vaga= ~A  ~%" (and (eq (nth 0 t2) (nth 1 t1))
							 					  ;(>= (- (nth 2 t2) (nth 3 t1)) +duracao-pausa+)))
				(if (or (and contrario?
						     (> (- limite (nth 2 t1)) +duracao-antes-refeicao+))
						(and (not contrario?)
							 (or (and (not (eq (nth 1 t1) (nth 0 t2)))
							 		  (> (- (nth 3 t1) limite) (- +duracao-antes-refeicao+ (* 2 +duracao-pausa+)))) 
							 	 (and (eq (nth 1 t1) (nth 0 t2))
							 	 	  (> (- (nth 3 t1) limite) (- +duracao-antes-refeicao+ +duracao-pausa+))))))
					(return-from tem-vaga-p NIL))

				(if (or (and (eq (nth 0 t2) (nth 1 t1))
							 (>= (- (nth 2 t2) (nth 3 t1)) +duracao-pausa+));nao e necessario um transporte.
						(and (not (eq (nth 0 t2) (nth 1 t1)))
							 (>= (- (nth 2 t2) (nth 3 t1)) (* 2 +duracao-pausa+)))) ;e necessario fazer um transporte? 
					(return-from tem-vaga-p T)))))
	NIL)


(defun tem-vaga-t1 (turno1 t2f)
  (let* ((turno (reverse (turno-tarefas turno1)))
         (primeiro (first turno))
         (resto (rest turno))
         (segundo (first resto))
         (pausas 0))
        
    (loop
      
      (if (> (- t2f (third primeiro)) +duracao-antes-refeicao+)
          (return nil))     
      (if (equal (first primeiro) (second segundo))
          (setf pausas (+ pausas +duracao-pausa+))
        (setf pausas (+ pausas (* 2 +duracao-pausa+))))
      (if (>= (- (third primeiro) (fourth segundo) pausas) 0)
          (return T))
        
      (setf primeiro (first resto))
      (setf resto (rest resto))
      (setf segundo (first resto))
      (setf pausas 0))))


(defun tem-vaga-t2 (turno t1i)
  (let* ((turno (turno-tarefas turno))
  	     (primeiro (first turno))
         (resto (rest turno))
         (segundo (first resto))
         (pausas 0))
    
    (loop
      
      (if (equal (second primeiro) (first segundo))
          (setf pausas (+ pausas +duracao-pausa+))
        (setf pausas (+ pausas (* 2 +duracao-pausa+))))
      (if (> (- (fourth primeiro) t1i) (- +duracao-antes-refeicao+ pausas))
          (return nil))
      (if (>= (- (third segundo) (fourth primeiro) pausas) 0)
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

	(let* (
		   (primeira-tarefa-t1 (car (turno-tarefas turno1)))
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

		   (tempo-conducao-t1 (turno-duracao-conducao turno1))
		   (tempo-conducao-t2 (turno-duracao-conducao turno2))

		   (tempo-conducao-uniao (+ tempo-conducao-t1 tempo-conducao-t2)))


		(if (not (eq local-inicio-t1 +local-inicial+)) 
			(setf tempo-inicio-t1 (- tempo-inicio-t1 +duracao-pausa+)))
	  
	  	(if (not (eq local-fim-t2 +local-inicial+)) 
			(setf tempo-fim-t2 (+ tempo-fim-t2 +duracao-pausa+)))

	  	(let ((une? NIL)
	  		  (duracao-uniao (- tempo-fim-t2 tempo-inicio-t1)))
	  		;(format t "sobrepostas? ~A ~%" (tarefas-sobrepostas-p ultima-tarefa-t1 primeira-tarefa-t2))
	  		;(format t "duracao= ~A ~%" (- tempo-fim-t2 tempo-inicio-t1))
	  		;(format t "aux = ~A ~%" (<= tempo-conducao-t1 +duracao-antes-refeicao+))
			(if (and (<=  duracao-uniao +duracao-max-turno+) ;tempototal < duracao max turno
					 (not (tarefas-sobrepostas-p ultima-tarefa-t1 primeira-tarefa-t2))
			         (or (and (> tempo-conducao-t1 +duracao-antes-refeicao+) ;tempo(t1) > 240
						      (<= tempo-conducao-t2 +duracao-antes-refeicao+)) ; tempo(t1) > 240 AND tempo(t2) <= 240 

						 (and (> tempo-conducao-t2 +duracao-antes-refeicao+) ; tempo(t2) > 240
			            	  (<= tempo-conducao-t1 +duracao-antes-refeicao+)) ; tempo(t1) <= 240 AND tempo(t2) > 240

	 					 (and (<= tempo-conducao-t1 +duracao-antes-refeicao+)
						      (<= tempo-conducao-t2 +duracao-antes-refeicao+)
						      (or (<= tempo-conducao-uniao +duracao-antes-refeicao+);tempo(t1) <= 240 AND tempo(t2) <= 240 AND tempo(t1) + tempo(t2) <= 240
								  (and (> tempo-conducao-uniao +duracao-antes-refeicao+) ;tempo(t1) <= 240 AND tempo(t2) <= 240 AND tempo(t1) + tempo(t2) > 240
								       (or (and (>= (- tempo-inicio-t2 tempo-fim-t1) +duracao-pausa+)
										  	    (eq local-inicio-t2 local-fim-t1)) ;existe espaco entre os turnos para refeicao?

										   (and (>= (- tempo-inicio-t2 tempo-fim-t1) (* 2 +duracao-pausa+))
										        (not (eq local-inicio-t2 local-fim-t1))) ;existe espaco entre os turnos para refeicao e transporte?

										   (tem-vaga-t1 turno1 tempo-fim-t2) 
										   (tem-vaga-t2 turno2 tempo-inicio-t1)))))))
						(setf une? T))
			
			une?)))


(defun junta-turnos (estado turno1 index1 turno2 index2)


  (let ((novo-estado (copy-list estado)))
    (cond ((turnos-unificaveis-p turno1 turno2)
    	(let* ((novas-tarefas (turno-tarefas (nth index1 novo-estado)))
    		   (nova-duracao-conducao (+ (turno-duracao-conducao turno1)
    		                             (turno-duracao-conducao turno2))))

			(setf novas-tarefas (append novas-tarefas (turno-tarefas (nth index2 novo-estado))))

			(setf (nth index1 novo-estado) (cria-turno novas-tarefas :duracao-conducao nova-duracao-conducao))
;;;;

			(setf novo-estado (remove-nth index2 novo-estado)))

			novo-estado))))


(defun remove-nth (n list)
  (declare
    (type (integer 0) n)
    (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))



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
      		(if (null resto) (return))
      		(progn
        		(setf juntou? (junta-turnos estado primeiro i (first resto-aux) j)) 
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


(defun conta-pausas (turno)
  (let ((primeiro (first (turno-tarefas turno)))
        (resto (rest (turno-tarefas turno)))
        (resultado 0))
    (loop 
      (if (null resto) (return))
      (setf resultado (+ resultado (- (third (first resto)) (fourth primeiro))))
      (setf primeiro (first resto))
      (setf resto (rest resto)))resultado))

(defun heuristica-alternativa (estado)
  (let ((resultado 0))
    (dolist (turno estado) (setf resultado (+ resultado (conta-pausas turno))))
    (setf resultado (+ (list-length estado) (/ resultado 1000)))))


(defun le-estado-inicial (input)

	"Le um problema na sua representacao externa e transforma-o para a sua
	 representacao interna, transformando cada tarefa num turno.
	 Argumentos:
	 * input-- lista de tarefas correspondente a 
	               representacao externa de um problema a resolver.
	 Retorno:
	 * Lista de turnos correspondente ao estado inicial de um problema."

  (let ((problema '()))
    (dolist (i input)
		(let ((novo-turno (cria-turno (list i))))

      		(setf problema (append problema (list novo-turno)))))
  	problema))

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
		  (sucessores '()))
		(loop
			(loop
				(when (funcall objectivo? estado-actual)
					(if (< (custo-estado estado-actual) (custo-estado melhor-solucao))
						(setf melhor-solucao estado-actual))
					(return))
				(setf sucessores (funcall operador estado-actual))
				(setf estado-actual (nth (random (length sucessores)) sucessores))
			)
			(when (>= (calcula-tempo-execucao) +max-tempo-execucao+)
				(return-from sondagem-iterativa melhor-solucao))

			(setf estado-actual estado-inicial))))
#|
(defun ilds (problema)

	"Utiliza a estrategia ILDS para resolver um problema de procura.
	 Argumentos:
	 * problema -- struct de representacao de um problema.
	 Retorno:
	 * O estado final obtido."
	(flet ((ilds-iter (estado, k, profundidade)))
	) |#


(defun calcula-tempo-execucao ()

	"Calcula o tempo de execucao do programa.

	 Retorno:
	 * O tempo de execucao desde o inicio do programa em segundos."

	(float (/ (- (get-internal-run-time) *tempo-execucao-inicial*) internal-time-units-per-second)))


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
		  (usa-procura? NIL))
		(cond 
			((equal estrategia "melhor.abordagem")
				NIl)
			((equal estrategia "a*.melhor.heuristica")
				(progn
					(setf funcao-heuristica #'n-turnos)
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
				(setf solucao (sondagem-iterativa problema))))
			((equal estrategia "ILDS")
				NIL)
			((equal estrategia "abordagem.alternativa")
				(setf estrategia "profundidade")
				(setf usa-procura? T)))
		(when usa-procura?
			(setf problema (cria-problema (le-estado-inicial problema)
									  (list #'operador)
									  :objectivo? #'objectivo-p
									  :custo #'custo-estado
									  :heuristica funcao-heuristica))
			(setf solucao (procura problema estrategia))
			(setf solucao (car (last (car solucao)))))

		;;Se tiver sido resolvido usando procura.lisp
		;;e necessario obter o ultimo elemento do caminho devolvido

		(setf tempo (calcula-tempo-execucao))
		(cons (cons (cons solucao (custo-estado solucao)) (n-turnos solucao)) tempo)))
