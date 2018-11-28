
(in-package :user)

(defconstant DURACAO_MAX_TURNO 480)
(defconstant DURACAO_PAUSA_REFEICAO 40)
(defconstant DURACAO_ANTES_REFEICAO 240)
(defconstant CARACTER_PAUSA 'PAUSA)


;(defstruct tarefa
;	local_partida
;	local_chegada
;	instante_partida
;	instante_chegada)

;(defstruct problema)

;(defun rep-externa (tarefa)
;	(list (tarefa-local_partida tarefa-local_chegada tarefa-instante_partida tarefa-instante_chegada)))


(defun car-turno (turno)
	"Função car modificada para extrair o primeiro elemento
	 que não seja o caracter da pausa CARACTER_PAUSA.
	 Argumentos:
	 * turno -- o turno ao qual o primeiro elemento deve ser extraido.
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

;(defun viola-restricoes-p (turno)
;	(let ((tarefa_inicial (car turno)))
;		(flet ((tarefas-sobrepostas-p (turno)
;					(let ((curr_partida (tarefa-instante_partida (car-turno turno)))
;						  (curr_chegada (tarefa-instante_partida (car-turno turno)))
;						 (dolist (curr_turno (cdr turno))
;						 	(if (< (tarefa-instante_partida curr_turno) curr_chegada)
;						 		(return-from T)))
;						  NIL))))
;	(if (or (> (duracao-turno (turno)) DURACAO_MAX_TURNO)
;			())))))


;(defun custo-turno (turno))

;(defun faz-afectacao (problema estrategia))


;((L1 L3 1 5) (BREAK) (L3 L1 7 9) (L1 L6 8 11))