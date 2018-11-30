
(in-package :user)

(defconstant DURACAO_MAX_TURNO 480)
(defconstant DURACAO_PAUSA 40)
(defconstant DURACAO_ANTES_REFEICAO 240)
(defconstant CARACTER_PAUSA 'P)

(defun car-turno (turno)
	
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
	(let ((duracao-total (- (nth 3 (last-turno turno)) (nth 2 (car turno)))))
		(if contabiliza-pausas?
			(let ((num-pausas 0))
				(if (eq (car (car turno)) CARACTER_PAUSA)
					(setf num-pausas (1+ num-pausas)))
				(if (eq (car (last-turno turno)) CARACTER_PAUSA)
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

(defun last-turno (turno)

	"Função last modificada para devolver apenas a tarefa, 
	 em vez de uma lista com uma tarefa.
	 Argumentos:
	 * turno -- o turno do qual será obtida a última tarefa.
	 Retorno:
	 * Uma lista de 4 elementos, correspondente a uma tarefa."

	 (car (last turno)))

(defun une-turnos (turno1 turno2)

	"Une dois turnos, tendo em conta as restrições impostas.
	 Argumentos:
	 * turno1 -- Lista de tarefas.
	 * turno2 -- Lista de tarefas.
	 Retorno:
	 * Novo turno gerado de unir turno1 e turno2 se puderem ser unidos,
	   NIL caso não seja possível."

	(if (eq (car (last-turno turno1)) CARACTER_PAUSA)
		(setf turno1 (butlast turno1)))
	(if (eq (car (car turno2)) CARACTER_PAUSA)
		(setf turno2 (cdr turno2)))
	;(format t "ffff ~%")
	(let ((turno-auxiliar turno1)
		  (turno-resultado '()))
		(if (and (not (tarefas-sobrepostas-p (last-turno turno1) (car turno2)))
				 (<=  (duracao-turno turno1) DURACAO_MAX_TURNO)
				 (<=  (duracao-turno turno2) DURACAO_MAX_TURNO))
			(progn
				(let ((num_pausas 0)
					  (chegada-turno1 (nth 1 (last-turno turno1)))
					  (partida-turno2 (car (car turno2))))
					(format t "t1= ~A t2= ~A ~%" turno1 turno2)
					(format t "ct1= ~A pt2= ~A ~%" chegada-turno1 partida-turno2)
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
				(format t "pre refeicoes ~A ~%" turno-auxiliar)
				(let ((duracao-corrente 0))
					 (dolist (tarefa turno-auxiliar)
					 	(setf duracao-corrente (+ duracao-corrente (duracao-tarefa tarefa :contabiliza-pausas? NIL)))
					 	(if (> duracao-corrente DURACAO_ANTES_REFEICAO)
					 		(setf turno-resultado (append turno-resultado (list (turno-pausa)))))
					 	(setf turno-resultado (append turno-resultado (list tarefa)))))
				(format t "pos refeicoes ~A  ~A minutos ~%" turno-resultado (duracao-turno turno-resultado))
				;;após cumprir estes requisitos todos o turno continua válido?
				(if (> (duracao-turno turno-resultado) DURACAO_MAX_TURNO)
					NIL
					turno-resultado))
			NIL)))

(defun gera-sucessores (estado)
	estado
	"Gera a lista de sucessores dado um estado.
	 Argumentos:
	 * estado -- lista de turnos.
	 Retorno:
	 * Lista de estados que representa os estados gerados."

	)