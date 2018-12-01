
(in-package :user)

(defconstant DURACAO_MAX_TURNO 480)
(defconstant DURACAO_PAUSA 40)
(defconstant DURACAO_ANTES_REFEICAO 240)
(defconstant CARACTER_PAUSA 'P)

(defun car-turno-sem-pausa (turno)
	
	"Função car modificada para extrair o primeiro elemento
	 que não seja o caracter da pausa CARACTER_PAUSA.
	 Argumentos:
	 * turno -- Lista de tarefas.
	 Retorno:
	 * O primeiro turno da lista"

	(let ((resultado (car turno))
		  (resto (cdr turno)))
		 (loop
		 	(if (eq (car resultado) CARACTER_PAUSA)
		 		(progn
		 			(setf resultado (car resto))
		 			(setf resto (cdr resto)))
				(return resultado)))))

(defun last-turno-sem-pausa (turno)
	
	"Função last modificada para extrair o ultimo elemento
	 que não seja o caracter da pausa CARACTER_PAUSA.
	 Argumentos:
	 * turno -- Lista de tarefas.
	 Retorno:
	 * O ultimo turno da lista"

	(car-turno-sem-pausa (reverse turno)))

(defun last-tarefa (turno)

	"Função last modificada para devolver apenas a tarefa, 
	 em vez de uma lista com uma tarefa.
	 Argumentos:
	 * turno -- o turno do qual será obtida a última tarefa.
	 Retorno:
	 * Uma lista de 4 elementos, correspondente a uma tarefa."

	 (car (last turno)))

(defun tarefa-pausa-p (tarefa)

	"Verifica se uma tarefa é a tarefa de pausa.
	 Argumentos:
	 * tarefa -- Lista de 4 ou 1 elemento(s).
	 Retorno:
	 * T se for corresponder a uma tarefa de pausa, NIL caso contrário."
	 
	(eq (car tarefa) CARACTER_PAUSA))

(defun duracao-tarefa (tarefa &key (contabiliza-pausas? T))

	"Calcula a duração de uma tarefa.
	 Argumentos:
	 * tarefa -- Lista de 4 ou 1 elemento(s).
	 * contabiliza-pausas? -- booleano que indica se pausas devem ser contabilizadas
	 Retorno:
	 * A duração da tarefa."
	;(format t "duracao-tarefa ~A ~%" tarefa)
	(if (tarefa-pausa-p tarefa)
		(if contabiliza-pausas? 
			DURACAO_PAUSA
			0)
		(- (nth 3 tarefa) (nth 2 tarefa))))

(defun duracao-turno (turno &key (contabiliza-pausas? T))

	"Calcula a duração de um turno.
	 Argumentos:
	 * turno -- Lista de tarefas.
	 * contabiliza-pausas? -- booleano que indica se pausas devem ser contabilizadas
	 Retorno:
	 * A duração do turno."
	;(format t "duracao-turno ~A ~%" turno )
	(let ((duracao-total (- (nth 3 (last-turno-sem-pausa turno)) (nth 2 (car-turno-sem-pausa turno)))))
		(if contabiliza-pausas?
			(let ((num-pausas 0))
				(if (eq (car (car turno)) CARACTER_PAUSA)
					(setf num-pausas (1+ num-pausas)))
				(if (eq (car (last-tarefa turno)) CARACTER_PAUSA)
					(setf num-pausas (1+ num-pausas)))
				(setf duracao-total (+ duracao-total (* num-pausas DURACAO_PAUSA)))))
		duracao-total))

(defun tarefas-sobrepostas-p (tarefa1 tarefa2)

	"Predicado que verifica se duas tarefas se sobrepoem.
	 Argumentos:
	 * tarefa1 -- Lista de 4 ou 1 elemento(s).
	 * tarefa2 -- Lista de 4 ou 1 elemento(s).
	 Retorno:
	 * T se se sobrepuserem, NIL caso contrário."
	(and (not (eq (car tarefa2) CARACTER_PAUSA))
		 (not (eq (car tarefa1) CARACTER_PAUSA))
		 (< (nth 2 tarefa2) (car (last tarefa1)))))

(defun turno-pausa ()

	"Constrói uma lista com uma lista com o elemento de pausa, 
	 correspondente a um turno de pausa.
	 Retorno:
	 * Uma lista com apenas 1 elemento, o caracter de pausa."

	 (list (list CARACTER_PAUSA)))



(defun une-turnos (turno1 turno2)

	"Une dois turnos, tendo em conta as restrições impostas.
	 Argumentos:
	 * turno1 -- Lista de tarefas.
	 * turno2 -- Lista de tarefas.
	 Retorno:
	 * Novo turno gerado de unir turno1 e turno2 se puderem ser unidos,
	   NIL caso não seja possível."

	(if (eq (car (last-tarefa turno1)) CARACTER_PAUSA)
		(setf turno1 (butlast turno1)))
	(if (eq (car (car turno2)) CARACTER_PAUSA)
		(setf turno2 (cdr turno2)))
	(let ((turno-auxiliar turno1)
		  (turno-resultado '()))
		(if (and (not (tarefas-sobrepostas-p (last-tarefa turno1) (car turno2)))
				 (<=  (duracao-turno turno1) DURACAO_MAX_TURNO)
				 (<=  (duracao-turno turno2) DURACAO_MAX_TURNO))
			(progn
				(let ((num_pausas 0)
					  (chegada-turno1 (nth 1 (last-tarefa turno1)))
					  (partida-turno2 (car (car turno2))))
					;(format t "t1= ~A t2= ~A ~%" turno1 turno2)
					;(format t "ct1= ~A pt2= ~A ~%" chegada-turno1 partida-turno2)
					;;((L8 L9 89 99)) ((L2 L4 45 55)) destinos e partidas diferentes e ambos diferentes de L1
					;; necessitam de fazer 2 pausas, voltar a L1 e sair de L1
					(if (and (not (eq chegada-turno1 'L1))
						 	 (not (eq partida-turno2 'L1))
						 	 (not (eq chegada-turno1 partida-turno2)))
						(setf num_pausas 2))
					;;(((L8 L1 55 88)) ((L2 L4 45 55)) ou (((L8 L3 55 88)) ((L1 L4 45 55))
					;; a partida da segunda tarefa é L1, logo apenas é necessaria 1 pausa
					;; de levar o trabalhador de L3 a L1, equivalente para o primeiro exemplo
					(if (or (and (eq chegada-turno1 'L1)
								 (not (eq partida-turno2 'L1)))
							(and (not (eq chegada-turno1 'L1))
								 (eq partida-turno2 'L1)))
						(setf num_pausas 1))
					;;(((L8 L1 55 88)) ((L1 L4 45 55)) ou (((L8 L9 55 88)) ((L9 L4 45 55))
					;;neste caso não são necessárias pausas
					(if (eq chegada-turno1 partida-turno2)
						(setf num_pausas 0))
					;;acrescentar as pausas entre os turnos
					(dotimes (i num_pausas)
						(setf turno-auxiliar (append turno-auxiliar (turno-pausa)))))
				(setf turno-auxiliar (append turno-auxiliar turno2))
				;;refeições!
				;(format t "pre refeicoes ~A ~%" turno-auxiliar)
				(let ((duracao-corrente 0))
					 (dolist (tarefa turno-auxiliar)
					 	(setf duracao-corrente (+ duracao-corrente (duracao-tarefa tarefa :contabiliza-pausas? NIL)))
					 	(if (> duracao-corrente DURACAO_ANTES_REFEICAO)
					 		(setf turno-resultado (append turno-resultado (list (turno-pausa)))))
					 	(setf turno-resultado (append turno-resultado (list tarefa)))))
				;(format t "pos refeicoes ~A  ~A minutos ~%" turno-resultado (duracao-turno turno-resultado))
				;;após cumprir estes requisitos todos o turno continua válido?
				(if (> (duracao-turno turno-resultado) DURACAO_MAX_TURNO)
					NIL
					turno-resultado))
			NIL)))

(defun le-problema (problema)

	"Le um problema na sua representação externa e transforma-o para a sua
	 representação interna, transformando cada tarefa num turno e introduzindo
	 as pausas necessárias no inicio e fim do turno.
	 Argumentos:
	 * problema -- lista de tarefas correspondente à 
	               representação externa de um problema a resolver.
	 Retorno:
	 * Lista de turnos correspondente ao estado inicial de um problema."
	
	(let ((estado-resultado '())
		  (turno-aux '()))
		(dolist (tarefa problema)
			(if (not (eq (car tarefa) 'L1))
				(setf turno-aux (append turno-aux (turno-pausa))))
			(setf turno-aux (append turno-aux (list tarefa)))
			(if (not (eq (nth 1 tarefa) 'L1))
				(setf turno-aux (append turno-aux (turno-pausa))))
			(setf estado-resultado (append estado-resultado (list turno-aux)))
			(setf turno-aux '()))
		estado-resultado))

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