
(in-package :user)

(defconstant +duracao-max-turno+ 480)
(defconstant +duracao-pausa+ 40)
(defconstant +duracao-antes-refeicao+ 240)
(defconstant +local-inicial+ 'L1)


(defun last-turno (turno)

	"A função last, quando aplicada a um turno, devolve uma lista com a tarefa. 
	 Esta função last modificada devolve apenas a tarefa, em vez de uma lista 
	 com uma tarefa.

	 Argumentos:
	 * turno -- o turno do qual será obtida a última tarefa.
	 Retorno:
	 * Uma lista de 4 elementos, correspondente a uma tarefa."

	 (car (last turno)))


(defun duracao-tarefa (tarefa)

	"Calcula a duração de uma tarefa.

	 Argumentos:
	 * tarefa -- Lista de 4 elementos que representa uma tarefa.
	 Retorno:
	 * Um inteiro que representa a duração da tarefa em minutos."

	(- (nth 3 tarefa) (nth 2 tarefa)))


(defun duracao-total-turno (turno)

	"Calcula a duração total de um turno, desde o inicio (contabilizando deslocações inicial e final).

	 Argumentos:
	 * turno -- Lista de tarefas.
	 Retorno:
	 * A duração do turno."

	(let ((num-pausas 0)
		  (primeira-tarefa (car turno))
		  (ultima-tarefa (last-turno turno))
		  (duracao 0))

		;;verifica se existem deslocações inicial e final a contabilizar para a duração
		(if (not (eq (nth 0 primeira-tarefa) +local-inicial+)) ;inicial
			(setf num-pausas (1+ num-pausas)))
		(if (not (eq (nth 1 ultima-tarefa) +local-inicial+)) ;final
			(setf num-pausas (1+ num-pausas)))

		(setf duracao (- (nth 3 ultima-tarefa) (nth 2 primeira-tarefa)))
		(setf duracao (+ duracao (* num-pausas +duracao-pausa+)))
		duracao))

(defun duracao-conducao-turno (turno)

	"Calcula a duração total de condução um turno.

	 Argumentos:
	 * turno -- Lista de tarefas.
	 Retorno:
	 * A duração de condução feita num turno."	

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
	 * T se se sobrepuserem, NIL caso contrário."

	;;se for necessaria uma pausa para deslocação entre as duas tarefas 
	;;deve ser contabilizado esse tempo extra
	(let ((pausa? 0)) ;0 ou 1 em vez de booleano para poder ser usado na multiplicação
		(if (not (eq (nth 1 tarefa1) (nth 0 tarefa2)))
			(setf pausa? 1))

		(< (nth 2 tarefa2) (+ (nth 3 tarefa1) (* pausa? +duracao-pausa+)))))

(defun cria-problema (input)

	"Le um problema na sua representação externa e transforma-o para a sua
	 representação interna, transformando cada tarefa num turno.

	 Argumentos:
	 * input-- lista de tarefas correspondente à 
	               representação externa de um problema a resolver.
	 Retorno:
	 * Lista de turnos correspondente ao estado inicial de um problema."

  (let ((problema '()))
    (dolist (i input)
      (setf problema (append problema (list (list i)))))
  	problema))


(defun tem-vaga-p (turno contrario?)

	"Verifica se um turno tem espaço para refeição.

	 Argumentos:
	 * turno -- lista de tarefas.
	 * contrario? -- T o início ao fim, NIL caso contrário
	 Retorno:
	 * T se existir espaço para refeicão, NIl caso contrário."

	(let ((tamanho (1- (length turno))))
		(dotimes (i tamanho)
			(if contrario? 
				(setf i (1- (- tamanho i))))
			(let ((t1 (nth i turno))
				  (t2 (nth (1+ i) turno)))
				;(format t "t1= ~A  t2= ~A ~%" t1 t2)
			    ;(format t "aux-vaga= ~A  ~%" (and (eq (nth 0 t2) (nth 1 t1))
							; (>= (- (nth 2 t2) (nth 3 t1)) +duracao-pausa+)))
				(if (or (and (eq (nth 0 t2) (nth 1 t1))
							 (>= (- (nth 2 t2) (nth 3 t1)) +duracao-pausa+));não é necessário um transporte.
						(and (not (eq (nth 0 t2) (nth 1 t1)))
							 (>= (- (nth 2 t2) (nth 3 t1)) (* 2 +duracao-pausa+)))) ;é necessario fazer um transporte? 
					(return-from tem-vaga-p T)))))
	NIL)


(defun une-turnos (turno1 turno2)

	"Une dois turnos, tendo em conta as restrições impostas.
	 Argumentos:
	 * turno1 -- Lista de tarefas.
	 * turno2 -- Lista de tarefas.
	 Retorno:
	 * Novo turno gerado de unir turno1 e turno2 se puderem ser unidos,
	   NIL caso não seja possível."

	(let ((primeira-tarefa-t1 (car turno1))
			(ultima-tarefa-t1 (last-turno turno1))
			(primeira-tarefa-t2 (car turno2))
			(ultima-tarefa-t2 (last-turno turno2)))
		(let

			((tempo-inicio-t1 (nth 2 primeira-tarefa-t1))
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
  		;(format t "duracao= ~A ~%" (- tempo-inicio-t2 tempo-fim-t1))
  		;(format t "aux = ~A ~%" (tem-vaga-p turno1 T))
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
									  	    (eq local-inicio-t2 local-fim-t1)) ;existe espaço entre os turnos para refeição?

									   (and (>= (- tempo-inicio-t2 tempo-fim-t1) (* 2 +duracao-pausa+))
									        (not (eq local-inicio-t2 local-fim-t1))) ;existe espaço entre os turnos para refeição e transporte?

									   (tem-vaga-p turno1 T) 
									   (tem-vaga-p turno2 NIL)))))))
					(setf une? T))
		(if une?
			(append turno1 turno2)
			NIL)))))



(defun gera-sucessores (estado)

	"Gera a lista de sucessores dado um estado, em que cada sucessor
	 consiste em 1 união de 2 turnos, mantendo-se os outros turnos iguais.
	 Argumentos:
	 * estado -- lista de turnos.
	 Retorno:
	 * Lista de estados que representa os estados gerados."

	 (let ((novos-estados '())
	 	   (estado-restante estado)
	 	   ;; prefixo e sufixo para adicionar antes e depois de cada novo turno gerado.
	 	   (prefixo '())
	 	   (sufixo '())
	 	   (novo-estado '()))
	 	(dolist (turno estado)
	 		;(format t "### ~A #### ~%" turno)
	 		;(format t "novos-estados: ~A ~%" novos-estados)

	 		(setf estado-restante (cdr estado-restante))
	 		(setf sufixo (cdr estado-restante))
	 		;(format t "o que falta ~A ~%" estado-restante)

	 		(if (not (NULL estado-restante))
	 			(let ((novo-turno (une-turnos turno (car estado-restante))))
	 				;(format t "turno possível: ~A  ~%" novo-turno)
	 				(if (not (null novo-turno))
	 					(progn
	 						(setf novo-estado (append prefixo (append (list novo-turno) sufixo)))
	 						;(format t "adicionando o estado ~A  ~%" novo-estado)
	 						(setf novos-estados (append novos-estados (list novo-estado)))))))
	 		(setf prefixo (append prefixo (list turno))))
	 	novos-estados))