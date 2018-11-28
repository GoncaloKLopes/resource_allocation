
(in-package :user)

(defconstant DURACAO_MAX_TURNO 480)
(defconstant DURACAO_PAUSA_REFEICAO 40)
(defconstant DURACAO_ANTES_REFEICAO 240)


(defstruct tarefa
	local_partida
	local_chegada
	instante_partida
	instante_chegada)

;(defstruct problema)

(defun rep-externa (tarefa)
	(list (tarefa-local_partida tarefa-local_chegada tarefa-instance_partida tarefa-instante_chegada)))

(defun viola-restricoes (turno)
	(let ((tarefa_inicial (car turno)))
	(if (or (and (not (eq (tarefa-local_partida tarefa_inicial) 'L1)) 
				 (not (eq (tarefa-local_chegada tarefa_inicial) 'L1))))
			(> (duracao-turno (turno)) DURACAO_MAX_TURNO)
			()))

;(defun custo-turno (turno))

;(defun faz-afectacao (problema estrategia))
