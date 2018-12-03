(in-package :user)

(defconstant +duracao-max-turno+ 480)
(defconstant +duracao-min-turno+ 360)
(defconstant +duracao-pausa+ 40)
(defconstant +duracao-antes-refeicao+ 240)
(defconstant +local-inicial+ 'L1)


(defun last-turno (turno)

	"A funcao last, quando aplicada a um turno, devolve uma lista com a tarefa. 
	 Esta funcao last modificada devolve apenas a tarefa, em vez de uma lista 
	 com uma tarefa.
	 Argumentos:
	 * turno -- o turno do qual sera obtida a ultima tarefa.
	 Retorno:
	 * Uma lista de 4 elementos, correspondente a uma tarefa."

	 (car (last turno)))


(defun duracao-tarefa (tarefa )

	"Calcula a duracao de uma tarefa.
	 Argumentos:
	 * tarefa -- Lista de 4 elementos que representa uma tarefa.
	 Retorno:
	 * Um inteiro que representa a duracao da tarefa em minutos."
	 ;(format t "~A ~%" tarefa)
	(- (nth 3 tarefa) (nth 2 tarefa)))


(defun duracao-total-turno (turno)

	"Calcula a duracao total de um turno, desde o inicio (contabilizando deslocacoes inicial e final).
	 Argumentos:
	 * turno -- Lista de tarefas.
	 Retorno:
	 * A duracao do turno."
;(format t "~A ~%" turno)
	(let ((num-pausas 0)
		  (primeira-tarefa (car turno))
		  (ultima-tarefa (last-turno turno)))

		;;verifica se existem deslocacoes inicial e final a contabilizar para a duracao
		(if (not (eq (nth 0 primeira-tarefa) +local-inicial+)) ;inicial
			(setf num-pausas (1+ num-pausas)))
		(if (not (eq (nth 1 ultima-tarefa) +local-inicial+)) ;final
			(setf num-pausas (1+ num-pausas)))
		(+ (- (nth 3 ultima-tarefa) (nth 2 primeira-tarefa) (* num-pausas +duracao-pausa+)))))


(defun duracao-conducao-turno (turno)

	"Calcula a duracao total de conducao um turno.
	 Argumentos:
	 * turno -- Lista de tarefas.
	 Retorno:
	 * A duracao de conducao feita num turno."	

	(let ((duracao 0))
		(dolist (tarefa turno)
			(setf duracao (+ duracao (duracao-tarefa tarefa))))
		duracao))


(defun tarefas-sobrepostas-p (tarefa1 tarefa2)

	"Predicado que verifica se duas tarefas se sobrepoem.
	 Argumentos:
	 * tarefa1 -- Lista de 4 ou 1 elemento(s).
	 * tarefa2 -- Lista de 4 ou 1 elemento(s).
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


(defun une-turnos (turno1 turno2)

	"Une dois turnos, tendo em conta as restricoes impostas.
	 Argumentos:
	 * turno1 -- Lista de tarefas.
	 * turno2 -- Lista de tarefas.
	 Retorno:
	 * Novo turno gerado de unir turno1 e turno2 se puderem ser unidos,
	   NIL caso nao seja possível."

	;(format t "uniao ~A ~A ~%" turno1 turno2)
	(let* ((primeira-tarefa-t1 (car turno1))
		   (ultima-tarefa-t1 (last-turno turno1))
		   (primeira-tarefa-t2 (car turno2))
		   (ultima-tarefa-t2 (last-turno turno2))

		   (tempo-inicio-t1 (nth 2 primeira-tarefa-t1))
		   (tempo-inicio-t2 (nth 2 primeira-tarefa-t2))
		   (tempo-fim-t1 (nth 3 ultima-tarefa-t1))
		   (tempo-fim-t2 (nth 3 ultima-tarefa-t2))

     	   (local-inicio-t1 (nth 0 primeira-tarefa-t1))
		   (local-inicio-t2 (nth 0 primeira-tarefa-t2))
		   (local-fim-t1 (nth 1 ultima-tarefa-t1))
		   (local-fim-t2 (nth 1 ultima-tarefa-t2))

		   (tempo-conducao-t1 (duracao-conducao-turno turno1))
		   (tempo-conducao-t2 (duracao-conducao-turno turno2)))


		(if (not (eq local-inicio-t1 +local-inicial+)) 
			(setf tempo-inicio-t1 (- tempo-inicio-t1 +duracao-pausa+)))
	  
	  	(if (not (eq local-fim-t2 +local-inicial+)) 
			(setf tempo-fim-t2 (+ tempo-fim-t2 +duracao-pausa+)))

	  	(let ((une? NIL))
	  		;(format t "sobrepostas? ~A ~%" (tarefas-sobrepostas-p ultima-tarefa-t1 primeira-tarefa-t2))
	  		;(format t "duracao= ~A ~%" (- tempo-fim-t2 tempo-inicio-t1))
	  		;(format t "aux = ~A ~%" (<= tempo-conducao-t1 +duracao-antes-refeicao+))
			(if (and (<= (- tempo-fim-t2 tempo-inicio-t1) +duracao-max-turno+) ;tempototal < duracao max turno
					 (not (tarefas-sobrepostas-p ultima-tarefa-t1 primeira-tarefa-t2))
			         (or (and (> tempo-conducao-t1 +duracao-antes-refeicao+) ;tempo(t1) > 240
						      (<= tempo-conducao-t2 +duracao-antes-refeicao+)) ; tempo(t1) > 240 AND tempo(t2) <= 240 

						 (and (> tempo-conducao-t2 +duracao-antes-refeicao+) ; tempo(t2) > 240
			            	  (<= tempo-conducao-t1 +duracao-antes-refeicao+)) ; tempo(t1) <= 240 AND tempo(t2) > 240

	 					 (and (<= tempo-conducao-t1 +duracao-antes-refeicao+)
						      (<= tempo-conducao-t2 +duracao-antes-refeicao+)
						      (or (<= (+ tempo-conducao-t1 tempo-conducao-t2) +duracao-antes-refeicao+);tempo(t1) <= 240 AND tempo(t2) <= 240 AND tempo(t1) + tempo(t2) <= 240
								  (and (> (+ tempo-conducao-t1 tempo-conducao-t2) +duracao-antes-refeicao+) ;tempo(t1) <= 240 AND tempo(t2) <= 240 AND tempo(t1) + tempo(t2) > 240
								       (or (and (>= (- tempo-inicio-t2 tempo-fim-t1) +duracao-pausa+)
										  	    (eq local-inicio-t2 local-fim-t1)) ;existe espaco entre os turnos para refeicao?

										   (and (>= (- tempo-inicio-t2 tempo-fim-t1) (* 2 +duracao-pausa+))
										        (not (eq local-inicio-t2 local-fim-t1))) ;existe espaco entre os turnos para refeicao e transporte?

										   (tem-vaga-p turno1 T tempo-fim-t2) 
										   (tem-vaga-p turno2 NIL tempo-inicio-t1)))))))
						(setf une? T))
			(if une?
				(append turno1 turno2)
				NIL))))


(defun junta-turnos (estado turno1 index1 turno2 index2)
  (let ((turnos (copy-list estado)))
    (cond ((une-turnos turno1 turno2)
           (setf (nth index1 turnos) (append (nth index1 turnos) turno2))
           (setf turnos (remove-nth index2 turnos))turnos))))


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
            (setf resultado  (append resultado (list juntou?))))
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

	 (null (operador estado)))


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
	 		(setf duracao (+ duracao (duracao-total-turno turno))))
	 	duracao))


(defun conta-pausas (turno)
  (let ((primeiro (first turno))
        (resto (rest turno))
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

(defun heuristica (estado)
  (list-length estado))


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
      (setf problema (append problema (list (list i)))))
  	problema))


(defun faz-afectacao (problema estrategia)

	"Resolve um problema de afectacao de recursos.
	 Argumentos:
	 * problema -- representacao externa de um problema, uma lista de tarefas.
	 * estrategia -- uma string que representa o nome da estrategia a usar.
	 Retorno:
	 * Melhor distribuicao de tarefas obtida."

	;;comecar por transformar a lista de tarefas numa lista de turnos
	(setf problema (cria-problema (le-estado-inicial problema)
								  (list #'operador)
								  :objectivo? #'objectivo-p
								  :heuristica #'heuristica))
	(let ((solucao NIL))
		(cond 
			((equal estrategia "melhor.abordagem")
				NIl)
			((equal estrategia "a*.melhor.heuristica")
				(setf solucao (car (last (car (procura problema "a*"))))))
			((equal estrategia "a*.melhor.heuristica.alternativa")
				NIL)
			((equal estrategia "sondagem.iterativa")
				NIL)
			((equal estrategia "ILDS")
				NIL)
			((equal estrategia "abordagem.alternativa")
				(setf solucao (car (last (car (procura problema "profundidade")))))))
		(cons (cons solucao (custo-estado solucao)) (n-turnos solucao))))