; Thu Nov 29 23:05:14 GMT+01:00 2018
;
;+ (version "3.5")
;+ (build "Build 663")

;;;
;;;						FUNCTIONS
;;;
(deffunction general-question "to ask general questions" (?pregunta)
	(format t "%s" ?pregunta)
	(bind ?respuesta (read input))
	(printout t ?respuesta)
	?respuesta
)

(deffunction question-with-default-values "to ask questions with default answers values" (?pregunta ?defaultValues)
	(format t "%s" ?pregunta)
	(printout t "(" ?defaultValues "): ")
	(bind ?respuesta (read input))
	(printout t ?respuesta)
	?respuesta
)

(deffunction binary-question "" (?pregunta)
	(format t "%s" ?pregunta)
	(printout t " (si/no/s/n): ")
	(bind ?respuesta (read input))
	(printout t ?respuesta)
	(if (or (eq (str-compare (lowcase ?respuesta) si) 0) (eq (str-compare (lowcase ?respuesta) s) 0))
		then (return TRUE)
		else (return FALSE)
	)
)

(deffunction esNecesarioFiltrar "" ()
	;(printout t "esNecesarioFiltrar" crlf)
	(bind ?exercicis (find-all-instances ((?e Ejercicio))
		(neq (send ?e get-partOf) [nil])
	))

	(bind ?sumaduracion 0)

	(loop-for-count (?i 1 (length$ ?exercicis)) do
		(bind ?exe (nth$ ?i ?exercicis))
		(bind ?sumaduracion (+ ?sumaduracion (* (send ?exe get-diasALaSemana) (send ?exe get-duracion))))
	)
	;(printout t ?sumaduracion)
	(if (<= ?sumaduracion 100)
		then
			(return TRUE)
		else
			(assert (finFiltroAss))
			(return FALSE)
	)

)

(deffunction eliminarUno "" ()

	(bind ?exercicis (find-all-instances ((?e Ejercicio))
		(neq (send ?e get-partOf) [nil])
	))

	(bind ?sumadias 0)

	(loop-for-count (?i 1 (length$ ?exercicis)) do
		(bind ?exe (nth$ ?i ?exercicis))
		(bind ?sumadias (+ ?sumadias (send ?exe get-diasALaSemana)))
	)

	(loop-for-count (?j 1 (length$ ?exercicis)) do
		(bind ?rand (random 1 (length$ ?exercicis)))
		(bind ?exe (nth$ ?j ?exercicis))
		(if (>= (- ?sumadias (send ?exe get-diasALaSemana)) 3)
			then
				(send ?exe put-partOf [nil])
				(return TRUE)
		)
	)
	;(assert (finFiltro))
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
	(seed (round (time)))
)

(defrule avi_new "rule to add new avi to the system"
	(new_avi)
	=>
	(bind ?nombre (general-question "Nombre: "))
	(bind ?edad (general-question "Edad: "))
	(if (>= (integer ?edad) 65)
		then
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
		else
			(printout t "No cumple los requisitos de edad para utilizar este programa." crlf)
			(retract 1) ; El hecho 1 es new_avi.
			(assert (FIN))
	)
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
		(if ?fragilidad then (assert (fragil)))
	)

)

(defrule hipertension "rule to know if avi have hipertension"
	(new_avi)
	=>
	(bind ?hipertension (binary-question "Hipertensión"))
	(if ?hipertension then (assert (hipertension)))
)

(defrule sobrepeso "rule to know if avi have sobrepeso or obesidad"
	(new_avi)
	=>
	(bind ?sobrepeso (binary-question "Sobrepeso/obesidad"))
	(if ?sobrepeso then (assert (sobrepeso)))
)

(defrule pulmonar "rule to know if avi have a pulmonar disease"
	(new_avi)
	=>
	(bind ?pulmonar (binary-question "Enfermedad Pulmonar Obstructiva"))
	(if ?pulmonar then (assert (pulmonar)))
)

(defrule osteoporosis "rule to know if avi have osteoporosis"
	(new_avi)
	=>
	(bind ?osteoporosis (binary-question "Osteoporosis"))
	(if ?osteoporosis then (assert (osteoporosis)))
)

(defrule cancer "rule to know if avi have cancer"
	(new_avi)
	=>
	(bind ?cancer (binary-question "Cáncer"))
	(if ?cancer then (assert (cancer)))
)

(defrule artritis "rule to know if avi have artritis"
	(new_avi)
	=>
	(bind ?artritis (binary-question "Artritis"))
	(if ?artritis then (assert (artritis)))
)

(defrule fibrosis "rule to know if avi have fibrosis"
	(new_avi)
	=>
	(bind ?fibrosis (binary-question "Fibrosis quística"))
	(if ?fibrosis then (assert (fibrosis)))
)

(defrule depresion "rule to know if avi have depresion"
	(new_avi)
	=>
	(bind ?depresion (binary-question "Depresión"))
	(if ?depresion then (assert (depresion)))
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
	(printout t "End of questions" crlf)
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

;;;						FLEXIBILIDAD

(defrule ejercicioEstiramientoBicepDerecho ""
	(new_avi)
	(fragil)
	(bicepDerechoCorrecto)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoBicepDerecho") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
)

(defrule ejercicioEstiramientoBicepIzquierdo ""
	(new_avi)
	(fragil)
	(bicepIzquierdoCorrecto)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoBicepIzquierdo") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
)

(defrule ejercicioEstiramientoCadera ""
	(new_avi)
	(fragil)
	(caderaCorrecta)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoCadera") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
)

(defrule ejercicioEstiramientoCuadricepDerecho ""
	(new_avi)
	(fragil)
	(cuadricepDerechoCorrecto)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoCuadricepDerecho") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
)

(defrule ejercicioEstiramientoCuadricepIzquierdo ""
	(new_avi)
	(fragil)
	(cuadricepIzquierdoCorrecto)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoCuadricepIzquierdo") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
)

(defrule ejercicioEstiramientoCuello ""
	(new_avi)
	(fragil)
	(cuelloCorrecto)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoCuello") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
)

(defrule ejercicioEstiramientoEspalda ""
	(new_avi)
	(fragil)
	(espaldaCorrecta)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoEspalda") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
)

(defrule ejercicioEstiramientoGemeloDerecho ""
	(new_avi)
	(fragil)
	(gemeloDerechoCorrecto)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoGemeloDerecho") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
)

(defrule ejercicioEstiramientoGemeloIzquierdo ""
	(new_avi)
	(fragil)
	(gemeloIzquierdoCorrecto)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoGemeloIzquierdo") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
)

(defrule ejercicioEstiramientoTobilloDerecho ""
	(new_avi)
	(fragil)
	(tobilloDerechoCorrecto)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoTobilloDerecho") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
)

(defrule ejercicioEstiramientoTobilloIzquierdo ""
	(new_avi)
	(tobilloIzquierdoCorrecto)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoTobilloIzquierdo") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
)

(defrule ejercicioEstiramientoTorso ""
	(new_avi)
	(fragil)
	(torsoCorrecto)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoTorso") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
)

(defrule ejercicioEstiramientoTricepDerecho ""
	(new_avi)
	(fragil)
	(tricepDerechoCorrecto)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoTricepDerecho") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
)

(defrule ejercicioEstiramientoTricepIzquierdo ""
	(new_avi)
	(fragil)
	(tricepIzquierdoCorrecto)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoTricepIzquierdo") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
)

;;;						FUERZA

(defrule ejercicioFortalecimientoPesaBicepDerecho ""
	(new_avi)
	(bicepDerechoCorrecto)
	(tieneMancuernas)
	(or (sobrepeso) (pulmonar))
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "PesaBicepDerecho") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
)

(defrule ejercicioFortalecimientoPesaBicepIzquierdo ""
	(new_avi)
	(bicepIzquierdoCorrecto)
	(tieneMancuernas)
	(or (sobrepeso) (pulmonar))
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "PesaBicepIzquierdo") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
)

(defrule ejercicioFortalecimientoPesaTricepDerecho ""
	(new_avi)
	(tricepDerechoCorrecto)
	(tieneMancuernas)
	(or (sobrepeso) (pulmonar))
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "PesaTricepDerecho") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
)

(defrule ejercicioFortalecimientoPesaTricepIzquierdo ""
	(new_avi)
	(tricepIzquierdoCorrecto)
	(tieneMancuernas)
	(or (sobrepeso) (pulmonar))
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "PesaTricepIzquierdo") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
)

;;;						AERÓBICOS

(defrule ejercicioPaseo ""
	(new_avi)
	(caderaCorrecta)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(tobilloIzquierdoCorrecto)
	(tobilloDerechoCorrecto)
	(or (enfermedadCardiovascular) (hipertension) (sobrepeso))
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Paseo") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
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
	(or (enfermedadCardiovascular) (hipertension) (sobrepeso) (pulmonar))
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Andar") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
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
	(or (enfermedadCardiovascular) (hipertension) (sobrepeso) (pulmonar))
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Bicicleta") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
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
	(or (fragil) (hipertension) (sobrepeso) (osteoporosis))
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Caminar") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
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
	(or (fragil) (hipertension) (sobrepeso) (osteoporosis))
	(tieneColchoneta)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "TaiChi") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
)

(defrule finEjercicios
	(new_avi)
	=>
	(printout t "Fin" crlf crlf)
	;(watch all)

	(bind ?exercicis (find-all-instances ((?e Ejercicio))
		(neq (send ?e get-partOf) [nil])
	))

	(bind ?sumaduracion 0)

	(loop-for-count (?i 1 (length$ ?exercicis)) do
		(bind ?exe (nth$ ?i ?exercicis))
		(bind ?sumaduracion (+ ?sumaduracion (* (send ?exe get-diasALaSemana) (send ?exe get-duracion))))
	)
	;(printout t ?sumaduracion)
	(if (> ?sumaduracion 100) then (assert (necesitaFiltro ?sumaduracion)))

	(focus filter)
)

;;;
;;;						FILTER MODULE
;;;

(defmodule filter
	(import MAIN ?ALL)
	(import ask_questions ?ALL)
	(import inference_of_data ?ALL)
	(export ?ALL)
)

(defrule filtrar
	?f <- (necesitaFiltro ?sumaduracion)

	=>

	(bind ?exercicis (find-all-instances ((?e Ejercicio))
		(neq (send ?e get-partOf) [nil])
	))

	(bind ?sumadias 0)

	(loop-for-count (?i 1 (length$ ?exercicis)) do
		(bind ?exe (nth$ ?i ?exercicis))
		(bind ?sumadias (+ ?sumadias (send ?exe get-diasALaSemana)))
	)

	(bind ?j 1)
	(while (and (<= ?j (length$ ?exercicis)) (> ?sumaduracion 260))
	 do
		(bind ?rand (random 1 (length$ ?exercicis)))
		(bind ?exe (nth$ ?rand ?exercicis))
		(if (>= (- ?sumadias (send ?exe get-diasALaSemana)) 3)
			then
				(bind ?sumadias (- ?sumadias (send ?exe get-diasALaSemana)))
				(bind ?sumaduracion (- ?sumaduracion (send ?exe get-duracion)))
				(send ?exe put-partOf [nil])
		)
		(bind ?j (+ ?j 1))
	)

	(assert (finFiltroAss))
)

(defrule finFiltro
	(finFiltroAss)
	=>
	(focus recomendations)
)


;;;
;;;						RECOMENDATIONS MODULE
;;;

(defmodule recomendations
	(import MAIN ?ALL)
	(import ask_questions ?ALL)
	(import inference_of_data ?ALL)
	(import filter ?ALL)
	(export ?ALL)
)

(defrule printPlanilla
	(new_avi)
	=>
	(bind ?exercicis (find-all-instances ((?e Ejercicio))
		(neq (send ?e get-partOf) [nil])
	))

	(loop-for-count (?i 1 (length$ ?exercicis)) do
		(bind ?exe (nth$ ?i ?exercicis))
		(send ?exe print)
		(printout t crlf)
	)
)
