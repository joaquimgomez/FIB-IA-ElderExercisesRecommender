; Wed Nov 28 22:28:27 GMT+01:00 2018
;
;+ (version "3.5")
;+ (build "Build 663")


(defclass %3ACLIPS_TOP_LEVEL_SLOT_CLASS "Fake class to save top-level slot information"
	(is-a USER)
	(role abstract)
	(single-slot diasALaSemana
		(type SYMBOL)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot fase
		(type SYMBOL)
		(allowed-values Inicial Mejora Mantenimiento)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot does
		(type SYMBOL)
;+		(allowed-parents Planilla)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot series
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot faseEjercicio
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot duracion
		(type FLOAT)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot trabaja
		(type SYMBOL)
;+		(allowed-parents Parte+del+Cuerpo)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot sexo
		(type SYMBOL)
		(allowed-values Hombre Mujer)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot hasA
		(type SYMBOL)
;+		(allowed-parents Enfermedad)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot nivelDeForma
		(type SYMBOL)
		(allowed-values Bajo Medio Alto)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot esFragil
		(type SYMBOL)
		(allowed-values FALSE TRUE)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot partOf
		(type SYMBOL)
;+		(allowed-parents Planilla)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot repeticiones
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot nombre
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot edad
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write)))

(defclass Avi
	(is-a USER)
	(role concrete)
	(single-slot sexo
		(type SYMBOL)
		(allowed-values Hombre Mujer)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot does
		(type SYMBOL)
;+		(allowed-parents Planilla)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot nombre
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot edad
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot hasA
		(type SYMBOL)
;+		(allowed-parents Enfermedad)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot nivelDeForma
		(type SYMBOL)
		(allowed-values Bajo Medio Alto)
;+		(cardinality 0 1)
		(create-accessor read-write)))

(defclass Independiente
	(is-a Avi)
	(role concrete)
	(single-slot esFragil
		(type SYMBOL)
		(allowed-values FALSE TRUE)
;+		(cardinality 0 1)
		(create-accessor read-write)))

(defclass Dependiente
	(is-a Avi)
	(role concrete))

(defclass Enfermedad
	(is-a USER)
	(role concrete))

(defclass Fragilidad
	(is-a Enfermedad)
	(role concrete))

(defclass Cardiovascular
	(is-a Enfermedad)
	(role concrete))

(defclass Diabetes
	(is-a Enfermedad)
	(role concrete))

(defclass Planilla
	(is-a USER)
	(role concrete)
	(single-slot fase
		(type SYMBOL)
		(allowed-values Inicial Mejora Mantenimiento)
;+		(cardinality 0 1)
		(create-accessor read-write)))

(defclass Ejercicio
	(is-a USER)
	(role concrete)
	(single-slot partOf
		(type SYMBOL)
;+		(allowed-parents Planilla)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot faseEjercicio
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot duracion
		(type FLOAT)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot diasALaSemana
		(type SYMBOL)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot trabaja
		(type SYMBOL)
;+		(allowed-parents Parte+del+Cuerpo)
;+		(cardinality 0 1)
		(create-accessor read-write)))

(defclass Aerobico
	(is-a Ejercicio)
	(role concrete))

(defclass Equilibrio
	(is-a Ejercicio)
	(role concrete))

(defclass Fortalecimiento
	(is-a Ejercicio)
	(role concrete)
	(single-slot repeticiones
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot series
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write)))

(defclass Flexibilidad
	(is-a Ejercicio)
	(role concrete))

(defclass Parte+del+Cuerpo
	(is-a USER)
	(role concrete))

	; Wed Nov 28 22:28:27 GMT+01:00 2018
	;
	;+ (version "3.5")
	;+ (build "Build 663")

	([KB_282909_Class29] of  Enfermedad
	)

	([KB_282909_Class30] of  %3APAL-CONSTRAINT
	)

	([KB_282909_Class31] of  %3AINSTANCE-ANNOTATION

		(%3ACREATION-TIMESTAMP "2018.11.25 17:58:09.204 GMT+01:00")
		(%3ACREATOR "joaquimgomez"))

	([KB_282909_Class5] of  %3AINSTANCE-ANNOTATION

		(%3AANNOTATED-INSTANCE [Independiente])
		(%3ACREATION-TIMESTAMP "2018.11.25 17:36:12.263 GMT+01:00")
		(%3ACREATOR "joaquimgomez"))

	([KB_282909_Class6] of  %3AINSTANCE-ANNOTATION

		(%3AANNOTATED-INSTANCE [Independiente])
		(%3ACREATION-TIMESTAMP "2018.11.25 17:36:13.291 GMT+01:00")
		(%3ACREATOR "joaquimgomez"))


;;;
;;;						FUNCTIONS
;;;
(deffunction general-question "to ask general questions" (?pregunta)
	(format t "%s" ?pregunta)
	(bind ?respuesta (read))
	?respuesta
)

(deffunction question-with-default-values "to ask questions with default answers values" (?pregunta ?defaultValues)
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

(defrule avi_new "rule to add new avi to the system"
	(new_avi)
	=>
	(bind ?nombre (general-question "Nombre: "))
	(bind ?edad (general-question "Edad: "))
	(bind ?sexo (question-with-default-values "Sexo: " "Hombre/Mujer"))
	(bind ?dependencia (question-with-default-values "Dependencia: " "Independiente/Dependiente"))
	(bind ?nivelDeForma (question-with-default-values "Nivel de Forma: " "Bajo/Medio/Alto"))
	(if (eq ?dependencia "Dependiente") then (assert (Dependiente (nombre ?nombre)
																													(edad ?edad)
																													(sexo ?sexo)
																													(nivelDeForma ?nivelDeForma)))
	else
	(bind ?esFragil ((question-with-defaultvalues "Es fragil: " "FALSE/TRUE")))
	(assert (Independiente (nombre ?nombre)
												 (edad ?edad)
												 (sexo ?sexo)
												 (nivelDeForma ?nivelDeForma)
												 (esFragil ?esFragil)))
	)
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

(defrule enfermedadCardiovascular "rule to know if avi have cardiovascular disease"
	(new_avi)
	?
	=>
	(bind ?enfCard (question-with-default-values "Enfermedad Cardiovascular" "Si/No"))
	(if (eq ?enfCard "Si") then
	else
	)
)

(defrule diabetes "rule to know if avi have diabetes"
	(new_avi)
	?
	=>
	(bind ?enfCard (question-with-default-values "Diabetes" "Si/No"))
	(if (eq ?enfCard "Si") then
	else
	)
)

(defrule fragilidad "rule to know if avi is fragile"
	(new_avi)
	?
	=>
	(bind ?enfCard (question-with-default-values "Fragil" "Si/No"))
	(if (eq ?enfCard "Si") then
	else
	)
)

;;;
;;;						RECOMENDATIONS MODULE
;;;
