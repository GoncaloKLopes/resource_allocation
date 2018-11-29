
(in-package :user)

(defconstant DURACAO_MAX_TURNO 480)
(defconstant DURACAO_PAUSA_REFEICAO 40)
(defconstant DURACAO_ANTES_REFEICAO 240)
(defconstant CARACTER_PAUSA 'PAUSA)

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

(defun duracao-tarefa (tarefa)

	"Calcula a duração de uma tarefa.
	 Argumentos:
	 * tarefa -- a tarefa cuja duração deve ser calculada.
	 Retorno:
	 * A duração da tarefa."

	(- (nth 3 tarefa) (1- (nth 2 tarefa))))

(defun duracao-turno (turno)

	"Calcula a duração de um turno.
	 Argumentos:
	 * turno -- o turno cuja duração deve ser calculada.
	 Retorno:
	 * A duração do turno."

	(if (eq 1 (length turno))
		(duracao-tarefa (car turno))
		(+ (duracao-tarefa (car turno)) (duracao-turno (cdr turno)))))

(defun tarefas-sobrepostas-p (tarefa1 tarefa2)

	"Predicado que verifica se duas tarefas se sobrepoem.
	 Argumentos:
	 * tarefa1 -- a primeira tarefa a ser verificada.
	 * tarefa2 -- a segunda tarefa a ser verificada.
	 Retorno:
	 * T se se sobrepuserem, NIL caso contrário."

	(and (not (eq (car tarefa2) CARACTER_PAUSA))
		 (not (eq (car tarefa1) CARACTER_PAUSA))
		 (< (nth 2 tarefa2) (nth 3 tarefa1))))
