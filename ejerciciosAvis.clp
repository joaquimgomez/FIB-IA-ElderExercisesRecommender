; Thu Nov 29 23:05:14 GMT+01:00 2018
;
;+ (version "3.5")
;+ (build "Build 663")


(defclass :CLIPS_TOP_LEVEL_SLOT_CLASS "Fake class to save top-level slot information"
	(is-a USER)
	(role abstract)
	(single-slot diasALaSemana
		(type INTEGER)
		(range 1 7)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot nombreEjercicio
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot fase
		(type SYMBOL)
		(allowed-values Inicial Mejora Mantenimiento)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot does
		(type INSTANCE)
;+		(allowed-classes Planilla)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot parte
		(type SYMBOL)
		(allowed-values Superior Inferior)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot series
		(type INTEGER)
		(range 1 ?VARIABLE)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot faseEjercicio
		(type SYMBOL)
		(allowed-values Inicial Mejora Mantenimiento)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot duracion
		(type INTEGER)
		(range 1 90)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot trabaja
		(type INSTANCE)
;+		(allowed-classes Parte+del+Cuerpo)
		(create-accessor read-write))
	(single-slot sexo
		(type SYMBOL)
		(allowed-values Hombre Mujer)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot hasA
		(type INSTANCE)
;+		(allowed-classes Enfermedad)
		(create-accessor read-write))
	(single-slot nivelDeForma
		(type SYMBOL)
		(allowed-values Bajo Medio Alto)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot grupoMuscular
		(type SYMBOL)
		(allowed-values PiernaDerecha PiernaIzquierda BrazoDerecho BrazoIzquierdo Torso Espalda Cuello)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot esFragil
		(type SYMBOL)
		(allowed-values FALSE TRUE)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot nombreParteCuerpo
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot porcentajeFCmax
		(type INTEGER)
		(range 0 100)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot partOf
		(type INSTANCE)
;+		(allowed-classes Planilla)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot repeticiones
		(type INTEGER)
		(range 1 ?VARIABLE)
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
		(type INSTANCE)
;+		(allowed-classes Planilla)
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
	(multislot hasA
		(type INSTANCE)
;+		(allowed-classes Enfermedad)
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
;+		(cardinality 1 1)
		(create-accessor read-write)))

(defclass Ejercicio
	(is-a USER)
	(role concrete)
	(single-slot partOf
		(type INSTANCE)
;+		(allowed-classes Planilla)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot faseEjercicio
		(type SYMBOL)
		(allowed-values Inicial Mejora Mantenimiento)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot duracion
		(type INTEGER)
		(range 1 90)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot diasALaSemana
		(type INTEGER)
		(range 1 7)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot nombreEjercicio
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot trabaja
		(type INSTANCE)
;+		(allowed-classes Parte+del+Cuerpo)
		(create-accessor read-write)))

(defclass Aerobico
	(is-a Ejercicio)
	(role concrete)
	(single-slot porcentajeFCmax
		(type INTEGER)
		(range 0 100)
;+		(cardinality 0 1)
		(create-accessor read-write)))

(defclass Equilibrio
	(is-a Ejercicio)
	(role concrete))

(defclass Fortalecimiento
	(is-a Ejercicio)
	(role concrete)
	(single-slot repeticiones
		(type INTEGER)
		(range 1 ?VARIABLE)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot series
		(type INTEGER)
		(range 1 ?VARIABLE)
;+		(cardinality 0 1)
		(create-accessor read-write)))

(defclass Flexibilidad
	(is-a Ejercicio)
	(role concrete))

(defclass Parte+del+Cuerpo
	(is-a USER)
	(role concrete)
	(single-slot parte
		(type SYMBOL)
		(allowed-values Superior Inferior)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot grupoMuscular
		(type SYMBOL)
		(allowed-values PiernaDerecha PiernaIzquierda BrazoDerecho BrazoIzquierdo Torso Espalda Cuello)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot nombreParteCuerpo
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write)))

; Thu Nov 29 23:05:14 GMT+01:00 2018
;
;+ (version "3.5")
;+ (build "Build 663")

([KB_282909_Class30] of  :PAL-CONSTRAINT
)

([KB_282909_Class31] of  :INSTANCE-ANNOTATION

	(:CREATION-TIMESTAMP "2018.11.25 17:58:09.204 GMT+01:00")
	(:CREATOR "joaquimgomez"))

([KB_282909_Class5] of  :INSTANCE-ANNOTATION

	(:ANNOTATED-INSTANCE [Independiente])
	(:CREATION-TIMESTAMP "2018.11.25 17:36:12.263 GMT+01:00")
	(:CREATOR "joaquimgomez"))

([KB_282909_Class6] of  :INSTANCE-ANNOTATION

	(:ANNOTATED-INSTANCE [Independiente])
	(:CREATION-TIMESTAMP "2018.11.25 17:36:13.291 GMT+01:00")
	(:CREATOR "joaquimgomez"))

([practicaProtege_Class10] of  Parte+del+Cuerpo

	(grupoMuscular PiernaIzquierda)
	(nombreParteCuerpo "CuadricepIzquierdo")
	(parte Inferior))

([practicaProtege_Class11] of  Parte+del+Cuerpo

	(grupoMuscular PiernaIzquierda)
	(nombreParteCuerpo "GemeloIzquierdo")
	(parte Inferior))

([practicaProtege_Class12] of  Parte+del+Cuerpo

	(grupoMuscular BrazoDerecho)
	(nombreParteCuerpo "BicepDerecho")
	(parte Superior))

([practicaProtege_Class13] of  Parte+del+Cuerpo

	(grupoMuscular BrazoDerecho)
	(nombreParteCuerpo "TricepDerecho")
	(parte Superior))

([practicaProtege_Class14] of  Parte+del+Cuerpo

	(grupoMuscular BrazoIzquierdo)
	(nombreParteCuerpo "BicepIzquierdo")
	(parte Superior))

([practicaProtege_Class15] of  Parte+del+Cuerpo

	(grupoMuscular BrazoIzquierdo)
	(nombreParteCuerpo "TricepIzquierdo")
	(parte Superior))

([practicaProtege_Class16] of  Parte+del+Cuerpo

	(grupoMuscular Espalda)
	(nombreParteCuerpo "Espalda")
	(parte Superior))

([practicaProtege_Class17] of  Parte+del+Cuerpo

	(grupoMuscular Torso)
	(nombreParteCuerpo "Cadera")
	(parte Superior))

([practicaProtege_Class18] of  Parte+del+Cuerpo

	(grupoMuscular PiernaDerecha)
	(nombreParteCuerpo "TobilloDerecho")
	(parte Inferior))

([practicaProtege_Class19] of  Parte+del+Cuerpo

	(grupoMuscular PiernaIzquierda)
	(nombreParteCuerpo "TobilloIzquierdo")
	(parte Inferior))

([practicaProtege_Class21] of  Aerobico

	(diasALaSemana 3)
	(duracion 10)
	(faseEjercicio Inicial)
	(nombreEjercicio "Caminar")
	(porcentajeFCmax 60)
	(trabaja
		[practicaProtege_Class8]
		[practicaProtege_Class10]
		[practicaProtege_Class9]
		[practicaProtege_Class11]
		[practicaProtege_Class18]
		[practicaProtege_Class19]
		[practicaProtege_Class17]))

([practicaProtege_Class24] of  Flexibilidad

	(diasALaSemana 2)
	(duracion 1)
	(faseEjercicio Inicial)
	(nombreEjercicio "EstiramientoBicepDerecho")
	(trabaja [practicaProtege_Class12]))

([practicaProtege_Class26] of  Flexibilidad

	(diasALaSemana 2)
	(duracion 1)
	(faseEjercicio Inicial)
	(nombreEjercicio "EstiramientoBicepIzquierdo")
	(trabaja [practicaProtege_Class14]))

([practicaProtege_Class27] of  Flexibilidad

	(diasALaSemana 2)
	(duracion 1)
	(faseEjercicio Inicial)
	(nombreEjercicio "EstiramientoCadera")
	(trabaja [practicaProtege_Class17]))

([practicaProtege_Class28] of  Flexibilidad

	(diasALaSemana 2)
	(duracion 1)
	(faseEjercicio Inicial)
	(nombreEjercicio "EstiramientoCuadricepDerecho")
	(trabaja [practicaProtege_Class8]))

([practicaProtege_Class29] of  Flexibilidad

	(diasALaSemana 2)
	(duracion 1)
	(faseEjercicio Inicial)
	(nombreEjercicio "EstiramientoCuadricepIzquierdo")
	(trabaja [practicaProtege_Class10]))

([practicaProtege_Class30] of  Flexibilidad

	(diasALaSemana 2)
	(duracion 1)
	(faseEjercicio Inicial)
	(nombreEjercicio "EstiramientoCuello")
	(trabaja [practicaProtege_Class5]))

([practicaProtege_Class31] of  Flexibilidad

	(diasALaSemana 2)
	(duracion 1)
	(faseEjercicio Inicial)
	(nombreEjercicio "EstiramientoEspalda")
	(trabaja [practicaProtege_Class16]))

([practicaProtege_Class32] of  Flexibilidad

	(diasALaSemana 2)
	(duracion 1)
	(faseEjercicio Inicial)
	(nombreEjercicio "EstiramientoGemeloDerecho")
	(trabaja [practicaProtege_Class9]))

([practicaProtege_Class33] of  Flexibilidad

	(diasALaSemana 2)
	(duracion 1)
	(faseEjercicio Inicial)
	(nombreEjercicio "EstiramientoGemeloIzquierdo")
	(trabaja [practicaProtege_Class11]))

([practicaProtege_Class34] of  Flexibilidad

	(diasALaSemana 2)
	(duracion 1)
	(faseEjercicio Inicial)
	(nombreEjercicio "EstiramientoTobilloDerecho")
	(trabaja [practicaProtege_Class18]))

([practicaProtege_Class35] of  Flexibilidad

	(diasALaSemana 2)
	(duracion 1)
	(faseEjercicio Inicial)
	(nombreEjercicio "EstiramientoTobilloIzquierdo")
	(trabaja [practicaProtege_Class19]))

([practicaProtege_Class36] of  Flexibilidad

	(diasALaSemana 2)
	(duracion 1)
	(faseEjercicio Inicial)
	(nombreEjercicio "EstiramientoTorso")
	(trabaja [practicaProtege_Class6]))

([practicaProtege_Class37] of  Flexibilidad

	(diasALaSemana 2)
	(duracion 1)
	(faseEjercicio Inicial)
	(nombreEjercicio "EstiramientoTricepDerecho")
	(trabaja [practicaProtege_Class13]))

([practicaProtege_Class38] of  Flexibilidad

	(diasALaSemana 2)
	(duracion 1)
	(faseEjercicio Inicial)
	(nombreEjercicio "EstiramientoTricepIzquierdo")
	(trabaja [practicaProtege_Class15]))

([practicaProtege_Class40] of  Aerobico Equilibrio

	(diasALaSemana 2)
	(duracion 45)
	(faseEjercicio Inicial)
	(nombreEjercicio "Tai Chi")
	(diasALaSemana 2)
	(duracion 45)
	(faseEjercicio Inicial)
	(nombreEjercicio "Tai Chi"))

([practicaProtege_Class41] of  Aerobico

	(diasALaSemana 2)
	(duracion 60)
	(faseEjercicio Inicial)
	(nombreEjercicio "Paseo")
	(porcentajeFCmax 60)
	(trabaja
		[practicaProtege_Class8]
		[practicaProtege_Class17]
		[practicaProtege_Class10]
		[practicaProtege_Class9]
		[practicaProtege_Class11]
		[practicaProtege_Class18]
		[practicaProtege_Class19]))

([practicaProtege_Class43] of  Aerobico

	(diasALaSemana 2)
	(duracion 20)
	(faseEjercicio Inicial)
	(nombreEjercicio "Andar")
	(porcentajeFCmax 65)
	(trabaja
		[practicaProtege_Class8]
		[practicaProtege_Class17]
		[practicaProtege_Class10]
		[practicaProtege_Class9]
		[practicaProtege_Class11]
		[practicaProtege_Class18]
		[practicaProtege_Class19]))

([practicaProtege_Class44] of  Aerobico

	(diasALaSemana 2)
	(duracion 20)
	(faseEjercicio Inicial)
	(nombreEjercicio "Bicicleta")
	(porcentajeFCmax 65)
	(trabaja
		[practicaProtege_Class17]
		[practicaProtege_Class8]
		[practicaProtege_Class10]
		[practicaProtege_Class9]
		[practicaProtege_Class11]
		[practicaProtege_Class18]
		[practicaProtege_Class19]))

([practicaProtege_Class45] of  Fortalecimiento

	(diasALaSemana 2)
	(duracion 2)
	(faseEjercicio Inicial)
	(nombreEjercicio "PesaBicepDerecho")
	(repeticiones 8)
	(series 2)
	(trabaja [practicaProtege_Class12]))

([practicaProtege_Class46] of  Fortalecimiento

	(diasALaSemana 2)
	(duracion 2)
	(faseEjercicio Inicial)
	(nombreEjercicio "PesaBicepIzquierdo")
	(repeticiones 8)
	(series 2)
	(trabaja [practicaProtege_Class14]))

([practicaProtege_Class47] of  Fortalecimiento

	(diasALaSemana 2)
	(duracion 2)
	(faseEjercicio Inicial)
	(nombreEjercicio "PesaTricepDerecho")
	(repeticiones 8)
	(series 2)
	(trabaja [practicaProtege_Class13]))

([practicaProtege_Class48] of  Fortalecimiento

	(diasALaSemana 2)
	(duracion 2)
	(faseEjercicio Inicial)
	(nombreEjercicio "PesaTricepIzquierdo")
	(repeticiones 8)
	(series 2)
	(trabaja [practicaProtege_Class15]))

([practicaProtege_Class5] of  Parte+del+Cuerpo

	(grupoMuscular Cuello)
	(nombreParteCuerpo "Cuello")
	(parte Superior))

([practicaProtege_Class6] of  Parte+del+Cuerpo

	(grupoMuscular Torso)
	(nombreParteCuerpo "Torso")
	(parte Superior))

([practicaProtege_Class8] of  Parte+del+Cuerpo

	(grupoMuscular PiernaDerecha)
	(nombreParteCuerpo "CuadricepDerecho")
	(parte Inferior))

([practicaProtege_Class9] of  Parte+del+Cuerpo

	(grupoMuscular PiernaDerecha)
	(nombreParteCuerpo "GemeloDerecho")
	(parte Inferior))

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
	(printout t "(" ?defaultValues "): ")
	(bind ?respuesta (read))
	?respuesta
)

(deffunction binary-question "" (?pregunta)
	(format t "%s" ?pregunta)
	(printout t " (si/no/s/n): ")
	(bind ?respuesta (read))
	(if (or (eq (str-compare (lowcase ?respuesta) si) 0) (eq (str-compare (lowcase ?respuesta) s) 0))
		then TRUE
		else FALSE
	)
)

;;;
;;;						MAIN MODULE
;;;

(defmodule MAIN (export ?ALL))

(defrule initial "initial rule"
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
	(bind ?sexo (question-with-default-values "Sexo " "Hombre/Mujer"))
	(bind ?dependencia (question-with-default-values "Dependencia " "Independiente/Dependiente"))
	(bind ?nivelDeForma (question-with-default-values "Nivel de Forma " "Bajo/Medio/Alto"))
	(if (eq (str-compare ?dependencia "Dependiente") 0)
		then (make-instance ?nombre of Dependiente (nombre ?nombre)
																							 (edad ?edad)
																							 (sexo ?sexo)
																							 (nivelDeForma ?nivelDeForma))
					(assert (Dependiente))
		else (bind ?esFragil (binary-question "Es fragil"))
			(make-instance ?nombre of Independiente (nombre ?nombre)
														 (edad ?edad)
														 (sexo ?sexo)
														 (nivelDeForma ?nivelDeForma)
														 (esFragil ?esFragil))
			(assert (Independiente))
	)
	(assert (Avi ?nombre))
)

;(defrule p
;	(new_avi)
;	?h <- (Avi ?nombre)
;	?aviFact <-(object (is-a Avi)(nombre ?nombreA))
;	(test (eq (str-compare  ?nombre ?nombreA) 0))
;	=>
;	(printout t (send ?aviFact get-edad))
;)



(defrule noEjercicioSi ""
	(new_avi)
	=>
	(printout t "Esta usted alguna de las siguiente condiciones?" crlf)
	(printout t "1. No ha tomado su medicación." crlf)
	(printout t "2. Infección aguda." crlf)
	(printout t "3. Presión arterial fuera de los valores normales." crlf)
	(printout t "4. Náuseas, vómitos, diarrea." crlf)
	(printout t "5. Hipoglucemia." crlf)
	(printout t "6. Mareo y/o síncope." crlf)
	(printout t "7. Síntomas de angina o taquicardia." crlf)
	(bind ?respuesta (binary-question "Respuesta" ))
	(if ?respuesta
		then
			(printout t "Recomendamos que no haga ejercicio y acuda a su médico de cabecera.")
			(assert (FIN))
		else
			(focus ask_questions))
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
	=>
	(bind ?enfCard (question-with-default-values "Enfermedad Cardiovascular" "Si/No"))
	(if (eq ?enfCard "Si") then (assert (enfermedadCardiovascular)))
)

(defrule diabetes "rule to know if avi have diabetes"
	(new_avi)
	=>
	(bind ?diabetes (question-with-default-values "Diabetes" "Si/No"))
	(if (eq ?diabetes "Si") then (assert (diabetes)))
)

(defrule fragilidad "rule to know if avi is fragile"
	(new_avi)
	=>
	(bind ?fragilidad (question-with-default-values "Fragil" "Si/No"))
	(if (eq ?fragilidad "Si") then (assert (fragilidad)))
)

(defrule check_partes_del_cuerpo ""
	(new_avi)
	=>
	(bind ?bicepDerecho (question-with-default-values "Presenta dolencia en el Bicep Derecho?" "Si/No"))
	(if (eq ?bicepDerecho "No") then (assert (bicepDerechoCorrecto)))
	(bind ?bicepIzquierdo (question-with-default-values "Presenta dolencia en el Bicep Izquierdo?" "Si/No"))
	(if (eq ?bicepIzquierdo "No") then (assert (bicepIzquierdoCorrecto)))
	(bind ?cadera (question-with-default-values "Presenta dolencia en la Cadera?" "Si/No"))
	(if (eq ?cadera "No") then (assert (caderaCorrecta)))
	(bind ?cuadricepDerecho (question-with-default-values "Presenta dolencia en el Cuadricep Derecho?" "Si/No"))
	(if (eq ?cuadricepDerecho "No") then (assert (cuadricepDerechoCorrecto)))
	(bind ?cuadricepIzquierdo (question-with-default-values "Presenta dolencia en el Cuadricep Izquierdo?" "Si/No"))
	(if (eq ?cuadricepIzquierdo "No") then (assert (cuadricepIzquierdoCorrecto)))
	(bind ?cuello (question-with-default-values "Presenta dolencia en el Cuello?" "Si/No"))
	(if (eq ?cuello "No") then (assert (cuelloCorrecto)))
	(bind ?espalda (question-with-default-values "Presenta dolencia en la Espalda?" "Si/No"))
	(if (eq ?espalda "No") then (assert (espaldaCorrecta)))
	(bind ?gemeloDerecho (question-with-default-values "Presenta dolencia en el Gemelo Derecho?" "Si/No"))
	(if (eq ?gemeloDerecho "No") then (assert (gemeloDerechoCorrecto)))
	(bind ?gemeloIzquierdo (question-with-default-values "Presenta dolencia en el Gemelo IZquierdo?" "Si/No"))
	(if (eq ?gemeloIzquierdo "No") then (assert (gemeloIzquierdoCorrecto)))
	(bind ?tobilloDerecho (question-with-default-values "Presenta dolencia en el Tobillo Derecho?" "Si/No"))
	(if (eq ?tobilloDerecho "No") then (assert (tobilloDerechoCorrecto)))
	(bind ?tobilloIzquierdo (question-with-default-values "Presenta dolencia en el Tobillo Izquierdo?" "Si/No"))
	(if (eq ?tobilloIzquierdo "No") then (assert (tobilloIzquierdoCorrecto)))
	(bind ?torso (question-with-default-values "Presenta dolencia en el Torso?" "Si/No"))
	(if (eq ?torso "No") then (assert (torsoCorrecto)))
	(bind ?tricepDerecho (question-with-default-values "Presenta dolencia en el Tricep Derecho?" "Si/No"))
	(if (eq ?tricepDerecho "No") then (assert (tricepDerechoCorrecto)))
	(bind ?tricepIzquierdo (question-with-default-values "Presenta dolencia en el Tricep Izquierdo?" "Si/No"))
	(if (eq ?tricepIzquierdo "No") then (assert (tricepIzquierdoCorrecto)))
)

(defrule material ""
	(new_avi)
	=>
	(printout t "Indique de los siguientes materiales si dispone de ellos o no:" crlf)
	(bind ?colchoneta (question-with-default-values "Colchoneta" "Si/No"))
	(if (eq ?colchoneta "Si") then (assert (tieneColchoneta)))
	(bind ?mancuernas (question-with-default-values "Mancuernas" "Si/No"))
	(if (eq ?mancuernas "Si") then (assert (tieneMancuernas)))
)

(defrule lastOfHere ""
	(new_avi)
	=>
	(focus inference_of_data)
)

;;;
;;;						INFERENCE MODULE
;;;

(defmodule inference_of_data
	(import MAIN ?ALL)
	(import ask_questions ?ALL)
	(export ?ALL)
)

(defrule ejercicioEstiramientoBicepDerecho ""
	(new_avi)
	(bicepDerechoCorrecto)
	=>
)

(defrule ejercicioEstiramientoBicepIzquierdo ""
	(new_avi)
	(bicepIzquierdoCorrecto)
	=>
)

(defrule ejercicioEstiramientoCadera ""
	(new_avi)
	(caderaCorrecta)
	=>
)

(defrule ejercicioEstiramientoCuadricepDerecho ""
	(new_avi)
	(cuadricepDerechoCorrecto)
	=>
)

(defrule ejercicioEstiramientoCuadricepIzquierdo ""
	(new_avi)
	(cuadricepIzquierdoCorrecto)
	=>
)

(defrule ejercicioEstiramientoCuello ""
	(new_avi)
	(cuelloCorrecto)
	=>
)

(defrule ejercicioEstiramientoEspalda ""
	(new_avi)
	(espaldaCorrecta)
	=>
)

(defrule ejercicioEstiramientoGemeloDerecho ""
	(new_avi)
	(gemeloDerechoCorrecto)
	=>
)

(defrule ejercicioEstiramientoGemeloIzquierdo ""
	(new_avi)
	(gemeloIzquierdoCorrecto)
	=>
)

(defrule ejercicioEstiramientoTobilloDerecho ""
	(new_avi)
	(tobilloDerechoCorrecto)
	=>
)

(defrule ejercicioEstiramientoTobilloIzquierdo ""
	(new_avi)
	(tobilloIzquierdoCorrecto)
	=>
)

(defrule ejercicioEstiramientoTorso ""
	(new_avi)
	(torsoCorrecto)
	=>
)

(defrule ejercicioEstiramientoTricepDerecho ""
	(new_avi)
	(tricepDerechoCorrecto)
	=>
)

(defrule ejercicioEstiramientoTricepIzquierdo ""
	(new_avi)
	(tricepIzquierdoCorrecto)
	=>
)

(defrule ejercicioFortalecimientoPesaBicepDerecho ""
	(new_avi)
	(bicepDerechoCorrecto)
	=>
)

(defrule ejercicioFortalecimientoBicepIzquierdo ""
	(new_avi)
	(bicepIzquierdoCorrecto)
	=>
)

(defrule ejercicioFortalecimientoTricepDerecho ""
	(new_avi)
	(tricepDerechoCorrecto)
	=>
)

(defrule ejercicioFortalecimientoTricepIzquierdo ""
	(new_avi)
	(tricepIzquierdoCorrecto)
	=>
)

(defrule ejercicioPaseo ""
	(new_avi)
	(caderaCorrecta)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(tobilloIzquierdoCorrecto)
	(tobilloDerechoCorrecto)
	=>
)

(defrule ejercicioAndar ""
	(new_avi)
	(caderaCorrecta)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(tobilloIzquierdoCorrecto)
	(tobilloDerechoCorrecto)
	=>

)

(defrule ejercicioBicicleta ""
	(new_avi)
	(caderaCorrecta)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(tobilloIzquierdoCorrecto)
	(tobilloDerechoCorrecto)
	(espaldaCorrecta)
	=>

)

(defrule ejercicioCaminar ""
	(new_avi)
	(caderaCorrecta)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(tobilloIzquierdoCorrecto)
	(tobilloDerechoCorrecto)
	=>

)

(defrule ejercicioTaichi ""
	(new_avi)
	(bicepDerechoCorrecto)
	(bicepIzquierdoCorrecto)
	(caderaCorrecta)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(cuelloCorrecto)
	(espaldaCorrecta)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(tobilloDerechoCorrecto)
	(tobilloIzquierdoCorrecto)
	(torsoCorrecto)
	(tricepDerechoCorrecto)
	(tricepIzquierdoCorrecto)
	=>

)

(defrule tieneEnfermedadCardiovascular ""
	(new_avi)
	(enfermedadCardiovascular)
	=>

)




; Regla por ejercicio, añadir ejercicio si Avi tiene alguna de esas enfermedades.

;;;
;;;						FILTER MODULE
;;;

;;;
;;;						RECOMENDATIONS MODULE
;;;
