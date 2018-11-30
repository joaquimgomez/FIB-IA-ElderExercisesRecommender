; Thu Nov 29 23:05:14 GMT+01:00 2018
;
;+ (version "3.5")
;+ (build "Build 663")

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
	(bind ?enfCard (binary-question "Enfermedad Cardiovascular" ))
	(if ?enfCard then (assert (enfermedadCardiovascular)))
)

(defrule diabetes "rule to know if avi have diabetes"
	(new_avi)
	=>
	(bind ?diabetes (binary-question "Diabetes"))
	(if ?diabetes then (assert (diabetes)))
)

(defrule fragilidad "rule to know if avi is fragile"
	(new_avi)
	?h <- (Avi ?nombre)
	=>
	;(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Tai Chi") 0)))
	;(bind ?exe (nth$ 1 ?ex))
	(bind ?abuelos (find-instance ((?a Independiente)) (eq (str-compare ?a:nombre ?nombre) 0)))
	(if (eq (length$ ?abuelos) 1) then
		(bind ?abu (nth$ 1 ?abuelos))
		(bind ?fragilidad (send ?abu get-esFragil))
	)
)

(defrule check_partes_del_cuerpo ""
	(new_avi)
	=>
	(bind ?bicepDerecho (binary-question "Presenta dolencia en el Bicep Derecho?"))
	(if (not ?bicepDerecho) then (assert (bicepDerechoCorrecto)))
	(bind ?bicepIzquierdo (binary-question "Presenta dolencia en el Bicep Izquierdo?"))
	(if (not ?bicepIzquierdo) then (assert (bicepIzquierdoCorrecto)))
	(bind ?cadera (binary-question "Presenta dolencia en la Cadera?"))
	(if (not ?cadera) then (assert (caderaCorrecta)))
	(bind ?cuadricepDerecho (binary-question "Presenta dolencia en el Cuadricep Derecho?"))
	(if (not ?cuadricepDerecho) then (assert (cuadricepDerechoCorrecto)))
	(bind ?cuadricepIzquierdo (binary-question "Presenta dolencia en el Cuadricep Izquierdo?"))
	(if (not ?cuadricepIzquierdo) then (assert (cuadricepIzquierdoCorrecto)))
	(bind ?cuello (binary-question "Presenta dolencia en el Cuello?"))
	(if (not ?cuello) then (assert (cuelloCorrecto)))
	(bind ?espalda (binary-question "Presenta dolencia en la Espalda?"))
	(if (not ?espalda) then (assert (espaldaCorrecta)))
	(bind ?gemeloDerecho (binary-question "Presenta dolencia en el Gemelo Derecho?"))
	(if (not ?gemeloDerecho) then (assert (gemeloDerechoCorrecto)))
	(bind ?gemeloIzquierdo (binary-question "Presenta dolencia en el Gemelo Izquierdo?"))
	(if (not ?gemeloIzquierdo) then (assert (gemeloIzquierdoCorrecto)))
	(bind ?tobilloDerecho (binary-question "Presenta dolencia en el Tobillo Derecho?"))
	(if (not ?tobilloDerecho) then (assert (tobilloDerechoCorrecto)))
	(bind ?tobilloIzquierdo (binary-question "Presenta dolencia en el Tobillo Izquierdo?"))
	(if (not ?tobilloIzquierdo) then (assert (tobilloIzquierdoCorrecto)))
	(bind ?torso (binary-question "Presenta dolencia en el Torso?"))
	(if (not ?torso) then (assert (torsoCorrecto)))
	(bind ?tricepDerecho (binary-question "Presenta dolencia en el Tricep Derecho?"))
	(if (not ?tricepDerecho) then (assert (tricepDerechoCorrecto)))
	(bind ?tricepIzquierdo (binary-question "Presenta dolencia en el Tricep Izquierdo?"))
	(if (not ?tricepIzquierdo) then (assert (tricepIzquierdoCorrecto)))
)

(defrule material ""
	(new_avi)
	=>
	(printout t "Indique de los siguientes materiales si dispone de ellos o no:" crlf)
	(bind ?colchoneta (binary-question "Colchoneta"))
	(if ?colchoneta then (assert (tieneColchoneta)))
	(bind ?mancuernas (binary-question "Mancuernas"))
	(if ?mancuernas then (assert (tieneMancuernas)))
)

(defrule lastOfHere ""
	(new_avi)
	=>
	(bind ?planilla (make-instance planilla of Planilla (fase Inicial)))
	(assert (planilla_avi ?planilla))
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
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Tai Chi") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
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
