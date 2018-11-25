;;;
;;;						FUNCTIONS
;;;
(deffunction general_question (?question)
	(format t "%s" ?pregunta)
	(bind ?respuesta (read))
	?respuesta
)

(deffunction question_with_default_values (?pregunta ?defaultValues)
	(format t "%s" ?pregunta)
	(format t "%s" ?defaultValues)
	(printout t "Valores posibles: ")
	(printout t ?defaultValues)
	(bind ?respuesta (read))
	?respuesta
)

;;;
;;;						MAIN MODULE
;;;

(defmodule MAIN (export ?ALL))

(defrule initial "intial rule"
	(initial-fact)
	=>
	(printout t "--------------------------------------------------------------" crlf)
	(printout t "------------ Sistema de Recomendacion de Ejercicios ----------" crlf)
	(printout t "--------------------------------------------------------------" crlf)
	(printout t crlf)
	(assert (new_avi))
)

(defrule avi_exists "rule to know if an avi is instantiated in the system"
	(new_avi)
	=>
	(bind ?nombre (general_question "Nombre: "))
	(bind ?edad (general_question "Edad: "))
	(bind ?sexo (question_with_default_values "Sexo: " "HOMBRE/MUJER"))
	(bind ?dependencia (question_with_default_values "Dependencia: " "INDEPENDIENTE/DEPENDIENTE"))
	;; CAMBIAR DEPENDENCIA
	(assert (Avi (nombre ?nombre)
							 (edad ?edad)
							 (sexo ?sexo)
							 (dependencia ?dependencia)))
	;; Enfermedades
	(focus ask_questions)
)


;;;
;;;						QUESTIONS MODULE
;;;

(defmodule ask_questions
	(import MAIN ?ALL)
	(export ?ALL)
)



;;;
;;;						RECOMENDATIONS MODULE
;;;
