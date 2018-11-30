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
	(make-instance planilla of Planilla (fase Inicial))
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
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare e:nombreEjercicio "ejercicioTaichi") 0)))
	(send ?ex set-partOf planilla)
)




; Regla por ejercicio, añadir ejercicio si Avi tiene alguna de esas enfermedades.

;;;
;;;						FILTER MODULE
;;;

;;;
;;;						RECOMENDATIONS MODULE
;;;
