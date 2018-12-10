
;;;
;;;						FUNCTIONS
;;;

(deffunction general-question "function to ask general questions" (?pregunta)
	(format t "%s" ?pregunta)
	(bind ?respuesta (read))
	?respuesta
)

(deffunction question-with-default-values "function to ask questions with default answers values" (?pregunta ?defaultValues)
	(format t "%s" ?pregunta)
	(printout t "(" ?defaultValues "): ")
	(bind ?respuesta (read))
	?respuesta
)

(deffunction binary-question "function to ask questions with binary answers values" (?pregunta)
	(format t "%s" ?pregunta)
	(printout t " (si/no/s/n): ")
	(bind ?respuesta (read))
	(if (or (eq (str-compare (lowcase ?respuesta) si) 0) (eq (str-compare (lowcase ?respuesta) s) 0))
		then (return TRUE)
		else (return FALSE)
	)
)

(deffunction minimum "function to find the day with less exercices" (?minDias)

	; Init
	(bind ?dias (find-all-instances ((?d Dia))
		(neq ?d [nil])
	))
	(bind ?nDias 0)

	(foreach ?dia ?dias
		(bind ?nEjercicios 0)
		(bind ?calentamientos (send ?dia get-Calentamiento))
		(bind ?principales (send ?dia get-Principal))
		(bind ?recuperaciones (send ?dia get-Recuperacion))
		(bind ?nEjercicios (+ ?nEjercicios (length$ ?calentamientos)))
		(bind ?nEjercicios (+ ?nEjercicios (length$ ?principales)))
		(bind ?nEjercicios (+ ?nEjercicios (length$ ?recuperaciones)))
		(if (> ?nEjercicios 0)
			then
			(bind ?nDias (+ ?nDias 1))
		)
	)

	(return (not (< ?nDias ?minDias)))
)

(deffunction tiempoDia "function to calculate the duration of a day's session" (?dia)
	; Init
	(bind ?timeofday 0)
	(bind ?calentamientos (send ?dia get-Calentamiento))
	(bind ?principales (send ?dia get-Principal))
	(bind ?recuperaciones (send ?dia get-Recuperacion))

	; Sum time calentamiento
	(foreach ?calentamiento ?calentamientos do
		(bind ?timeofday (+ ?timeofday (send ?calentamiento get-duracion)))
	)

	; Sum time principales
	(foreach ?principal ?principales do
		(bind ?timeofday (+ ?timeofday (send ?principal get-duracion)))
	)

	; Sum time recuperaciones
	(foreach ?recuperacion ?recuperaciones do
		(bind ?timeofday (+ ?timeofday (send ?recuperacion get-duracion)))
	)

	; Return
	(return ?timeofday)
)

(deffunction countCalentamientos "function to count the number of Calentamientos in a day" (?dia)
	; Init
	(bind ?count 0)
	(bind ?calentamientos (send ?dia get-Calentamiento))

	; Sum count calentamiento
	(foreach ?calentamiento ?calentamientos do
			(bind ?count (+ ?count 1))
	)

	(return ?count)
)

(deffunction countSubclass "count number of exercices of a class" (?class ?dia)
	; Init
	(bind ?count 0)
	(bind ?calentamientos (send ?dia get-Calentamiento))
	(bind ?principales (send ?dia get-Principal))
	(bind ?recuperaciones (send ?dia get-Recuperacion))

	; Sum count subclass calentamiento
	(foreach ?calentamiento ?calentamientos do
		(if (eq (class ?calentamiento) ?class)
			then
			(bind ?count (+ ?count 1))
		)
	)

	; Sum count subclass principales
	(foreach ?principal ?principales do
		(if (eq (class ?principal) ?class)
			then
			(bind ?count (+ ?count 1))
		)
	)

	; Sum count subclass recuperaciones
	(foreach ?recuperacion ?recuperaciones do
		(if (eq (class ?recuperacion) ?class)
			then
			(bind ?count (+ ?count 1))
		)
	)

	(return ?count)
)

(deffunction hasSubclass "to know if the planilla has exercices of a class" (?class ?dia)

	; Init
	(bind ?calentamientos (send ?dia get-Calentamiento))
	(bind ?principales (send ?dia get-Principal))
	(bind ?recuperaciones (send ?dia get-Recuperacion))

	; Sum count subclass calentamiento
	(foreach ?calentamiento ?calentamientos do
		(if (eq (class ?calentamiento) ?class)
			then (return TRUE)
		)
	)

	; Sum count subclass principales
	(foreach ?principal ?principales do
		(if (eq (class ?principal) ?class)
			then (return TRUE)
		)
	)

	; Sum count subclass recuperaciones
	(foreach ?recuperacion ?recuperaciones do
		(if (eq (class ?recuperacion) ?class)
			then (return TRUE)
		)
	)

	(return FALSE)
)

(deffunction findDia "to get a Dia by name" (?name)
	(bind ?dia (find-instance ((?d Dia))
		(eq (send ?d get-nombreDia) ?name)
	))
	(bind ?dia (nth$ 1 ?dia))
	(return ?dia)
)

(deffunction prevDay "returns the previous day of dia" (?dia)
	(if (eq (str-compare (send ?dia get-nombreDia) "Lunes") 0)
		then (return (findDia "Domingo"))
	)
	(if (eq (str-compare (send ?dia get-nombreDia) "Martes") 0)
		then (return (findDia "Lunes"))
	)
	(if (eq (str-compare (send ?dia get-nombreDia) "Miercoles") 0)
		then (return (findDia "Martes"))
	)
	(if (eq (str-compare (send ?dia get-nombreDia) "Jueves") 0)
		then (return (findDia "Miercoles"))
	)
	(if (eq (str-compare (send ?dia get-nombreDia) "Viernes") 0)
		then (return (findDia "Jueves"))
	)
	(if (eq (str-compare (send ?dia get-nombreDia) "Sabado") 0)
		then (return (findDia "Viernes"))
	)
	(if (eq (str-compare (send ?dia get-nombreDia) "Domingo") 0)
		then (return (findDia "Sabado"))
	)
)

(deffunction nextDay "returns the next day of dia" (?dia)
	(if (eq (str-compare (send ?dia get-nombreDia) "Lunes") 0)
		then (return (findDia "Martes"))
	)
	(if (eq (str-compare (send ?dia get-nombreDia) "Martes") 0)
		then (return (findDia "Miercoles"))
	)
	(if (eq (str-compare (send ?dia get-nombreDia) "Miercoles") 0)
		then (return (findDia "Jueves"))
	)
	(if (eq (str-compare (send ?dia get-nombreDia) "Jueves") 0)
		then (return (findDia "Viernes"))
	)
	(if (eq (str-compare (send ?dia get-nombreDia) "Viernes") 0)
		then (return (findDia "Sabado"))
	)
	(if (eq (str-compare (send ?dia get-nombreDia) "Sabado") 0)
		then (return (findDia "Domingo"))
	)
	(if (eq (str-compare (send ?dia get-nombreDia) "Domingo") 0)
		then (return (findDia "Lunes"))
	)
)

(deffunction setDuracion "function to change the duration of an ejercicio" (?ejercicio ?duracion)
	(send ?ejercicio put-duracion ?duracion)
)

(deffunction setDuracionFuerza "determina la duración del ejercicio de fuerza según series y repeticiones" (?ejercicio)
	(bind ?duracion 0)
	(bind ?repeticiones (* (send ?ejercicio get-repeticiones) (send ?ejercicio get-series)))
	;;entre 20 y 30 repeticiones -> 3 min
	;;entre 10 y 20 -> 2 min
	;;entre 0 y 10 -> 1 min
	(if (< ?repeticiones 31) then
		(bind ?duracion 2)
		else
			(if (< ?repeticiones 46) then
				(bind ?duracion 3)
				else
					(bind ?duracion 5)))

	(send ?ejercicio put-duracion ?duracion)
)

(deffunction trabajaCommon "to know if two exercises work a muscle in common" (?trabaja1 ?trabaja2)
	(foreach ?musculo1 ?trabaja1
		(foreach ?musculo2 ?trabaja2
			(if (eq ?musculo1 ?musculo2)
				then (return TRUE)
			)
		)
	)
	(return FALSE)
)

(deffunction setSeries "defines the number of series of a ejercicio of fortalecimiento" (?ejercicio ?series)
	(send ?ejercicio put-series ?series)
)

(deffunction alreadyInCalentamientos "to know if an exercicise is already in calentamiento" (?ejercicio ?dia)
	; Init
	(bind ?calentamientos (send ?dia get-Calentamiento))

	; Check calentamiento
	(foreach ?calentamiento ?calentamientos do
		(if (eq ?calentamiento ?ejercicio) then (return TRUE))
	)

	; Return
	(return FALSE)
)

(deffunction setRepeticiones "defines the number of repeticiones of a ejercicio of fortalecimiento" (?ejercicio ?repeticiones)
	(send ?ejercicio put-repeticiones ?repeticiones)
)

(deffunction setFase "defines the fase of an ejercicio depending on the nivelDeForma" (?ejercicio ?nivelForma)
	(if (eq (str-compare ?nivelForma "Bajo") 0)
		then (send ?ejercicio put-faseEjercicio Inicial))
	(if (eq (str-compare ?nivelForma "Medio") 0)
		then (send ?ejercicio put-faseEjercicio Mejora))
	(if (eq (str-compare ?nivelForma "Alto") 0)
		then (send ?ejercicio put-faseEjercicio Mantenimiento))
)

(deffunction alreadyInRecuperacion "to know if an exercicise is already in calentamiento" (?ejercicio ?dia)
	; Init
	(bind ?recuperaciones (send ?dia get-Recuperacion))

	; Check recuperaciones
	(foreach ?recuperacion ?recuperaciones do
		(if (eq ?recuperacion ?ejercicio) then (return TRUE))
	)

	; Return
	(return FALSE)
)

(deffunction hasCalentamiento "to know if there's any Calentamiento available that works a muscle in common with ejercicio" (?ejercicio ?dia)
	(bind ?possibleCalentamientos (find-all-instances ((?e Ejercicio))
		(and
			(neq (send ?e get-partOf) [nil])
			(eq (send ?e get-tipo) Calentamiento)
			(> (send ?e get-diasALaSemana) 0)
			(trabajaCommon (send ?e get-trabaja) (send ?ejercicio get-trabaja))
			(not (alreadyInCalentamientos ?e ?dia))
		)
	))
	;(printout t ?possibleCalentamientos crlf)
	(return (> (length$ ?possibleCalentamientos) 0))
)

(deffunction getCalentamiento "returns a Calentamiento that works a muscle in common with ejercicio" (?ejercicio ?dia)
	(bind ?possibleCalentamientos (find-instance ((?e Ejercicio))
		(and
			(neq (send ?e get-partOf) [nil])
			(eq (send ?e get-tipo) Calentamiento)
			(> (send ?e get-diasALaSemana) 0)
			(trabajaCommon (send ?e get-trabaja) (send ?ejercicio get-trabaja))
			(not (alreadyInCalentamientos ?e ?dia))
		)
	))
	(return (nth$ 1 ?possibleCalentamientos))
)

(deffunction canAsssign "to know if can assign an exercise" (?ejercicio ?dia ?class ?maxFort ?fortNoConsecutivo)

	; Check time
	(bind ?timeofday (tiempoDia ?dia))
	(if (> (+ ?timeofday (send ?ejercicio get-duracion)) ?*max-duracion*)
		then
		(return FALSE)
	)

	; If isFortalecimiento
	(if (eq ?class Fortalecimiento)
		then

		; Count fortalecimiento
		(if (> ?maxFort -1)
			then
			(bind ?nFort (countSubclass Fortalecimiento ?dia))
			(if (not (< ?nFort ?maxFort))
				then
				(return FALSE)
			)
		)

		; Non-consecutive days
		(if ?fortNoConsecutivo
			then
			(bind ?prevDay (prevDay ?dia))
			(bind ?nextDay (nextDay ?dia))
			(if (or (hasSubclass Fortalecimiento ?prevDay) (hasSubclass Fortalecimiento ?nextDay))
				then
				(return FALSE)
			)
		)
	)

	(return TRUE)
)

(deffunction assignPrincipal "to assign a exercise of class Principal and its Calentamientos" (?ejercicio ?dia ?timelimit ?maxCalentamientos ?addFortalecimiento ?nFortalecimiento)

	; Init
	(bind ?calentamientos (send ?dia get-Calentamiento))
	(bind ?principales (send ?dia get-Principal))
	(bind ?recuperaciones (send ?dia get-Recuperacion))

	; Insert ejercicio
	(bind ?principales (insert$ ?principales (+ (length$ ?principales) 1) ?ejercicio))
	(send ?dia put-Principal ?principales)

	; Update diasALaSemana
	(bind ?new-diasALaSemana (- (send ?ejercicio get-diasALaSemana) 1))
	(send ?ejercicio put-diasALaSemana ?new-diasALaSemana)

	(bind ?nCalentamientos (countCalentamientos ?dia))

	; Insert calentamientos
	(while (and
			(hasCalentamiento ?ejercicio ?dia)
			(bind ?calentamiento (getCalentamiento ?ejercicio ?dia))
			(not (> (+ (tiempoDia ?dia) (send ?calentamiento get-duracion)) ?timelimit))
			(or (< ?nCalentamientos ?maxCalentamientos) (= ?maxCalentamientos -1))
		) do

		; Get listas dia
		(bind ?calentamientos (insert$ ?calentamientos (+ (length$ ?calentamientos) 1) ?calentamiento))
		(bind ?recuperaciones (insert$ ?recuperaciones (+ (length$ ?recuperaciones) 1) ?calentamiento))

		; Add calentamiento
		(send ?dia put-Calentamiento ?calentamientos)
		(send ?dia put-Recuperacion ?recuperaciones)

		; Update dias a la semana calentamiento
		(if (neq (class ?calentamiento) Flexibilidad)
			then
			(bind ?calentamiento-diasALaSemana (- (send ?calentamiento get-diasALaSemana) 1))
			(send ?calentamiento put-diasALaSemana ?calentamiento-diasALaSemana)
		)

		(bind ?nCalentamientos (+ ?nCalentamientos 1))
		;(printout t (tiempoDia ?dia) crlf)
	)

	; Insert fortalecimiento
	(if ?addFortalecimiento
		then
		(bind ?fortalecimientos (find-all-instances ((?e Fortalecimiento))
			(neq (send ?e get-partOf) [nil])
		))
		(foreach ?ex ?fortalecimientos
			(if (canAsssign ?ex ?dia Fortalecimiento ?nFortalecimiento FALSE)
				then
				(assignPrincipal ?ex ?dia ?timelimit ?maxCalentamientos FALSE 0)
			)
		)
	)
)

(deffunction calculaFCmax "calcula la frecuencia cardíaca máxima de un avi" (?avi)
	return (integer (- 208 (* 0.7 (send ?avi get-edad))))
)

(deffunction setFCejercicio "calcula la FC de un ejercicio indicado para una enfermedad y avi"(?ejercicio ?enfermedad ?avi)
	(switch ?enfermedad
		;;FRAGILIDAD
		(case Fragilidad then
			(switch (send ?ejercicio get-faseEjercicio)
				(case Inicial then
					(send ?ejercicio put-porcentajeFCmax 60))
				(case Mejora then
					(send ?ejercicio put-porcentajeFCmax 70))
				(case Mantenimiento then
					(send ?ejercicio put-porcentajeFCmax 80))))
		;;CARDIOVASCULAR
		(case Cardiovascular then
			(switch (send ?ejercicio get-faseEjercicio)
				(case Inicial then
					(send ?ejercicio put-porcentajeFCmax 65))
				(case Mejora then
					(send ?ejercicio put-porcentajeFCmax 70))
				(case Mantenimiento then
					(send ?ejercicio put-porcentajeFCmax 80))))
		;;HIPERTENSIÓN
		(case Hipertension then
			(switch (send ?ejercicio get-faseEjercicio)
				(case Inicial then
					(send ?ejercicio put-porcentajeFCmax 70))
				(case Mejora then
					(send ?ejercicio put-porcentajeFCmax 75))
				(case Mantenimiento then
					(send ?ejercicio put-porcentajeFCmax 80))))
		;;SOBREPESO
		(case Sobrepeso then
			(switch (send ?ejercicio get-faseEjercicio)
				(case Inicial then
					(send ?ejercicio put-porcentajeFCmax 70))
				(case Mejora then
					(send ?ejercicio put-porcentajeFCmax 80))
				(case Mantenimiento then
					(send ?ejercicio put-porcentajeFCmax 90))))
		;;DIABTETES
		(case Diabetes then
			(switch (send ?ejercicio get-faseEjercicio)
				(case Inicial then
					(send ?ejercicio put-porcentajeFCmax 60))
				(case Mejora then
					(send ?ejercicio put-porcentajeFCmax 65))
				(case Mantenimiento then
					(send ?ejercicio put-porcentajeFCmax 70))))
		;;PULMONAR
		(case Pulmonar then
			(switch (send ?ejercicio get-faseEjercicio)
				(case Inicial then
					(send ?ejercicio put-porcentajeFCmax 60))
				(case Mejora then
					(send ?ejercicio put-porcentajeFCmax 70))
				(case Mantenimiento then
					(send ?ejercicio put-porcentajeFCmax 80))))
		;;OSTEOPOROSIS
		(case Osteoporosis then
			(switch (send ?ejercicio get-faseEjercicio)
				(case Inicial then
					(send ?ejercicio put-porcentajeFCmax 70))
				(case Mejora then
					(send ?ejercicio put-porcentajeFCmax 80))
				(case Mantenimiento then
					(send ?ejercicio put-porcentajeFCmax 85))))
		;;CÁNCER
		(case Cancer then
			(switch (send ?ejercicio get-faseEjercicio)
				(case Inicial then
					(send ?ejercicio put-porcentajeFCmax 60))
				(case Mejora then
					(send ?ejercicio put-porcentajeFCmax 70))
				(case Mantenimiento then
					(send ?ejercicio put-porcentajeFCmax 75))))
		;;ARTRITIS
		(case Artritis then
			(switch (send ?ejercicio get-faseEjercicio)
				(case Inicial then
					(send ?ejercicio put-porcentajeFCmax 60))
				(case Mejora then
					(send ?ejercicio put-porcentajeFCmax 70))
				(case Mantenimiento then
					(send ?ejercicio put-porcentajeFCmax 80))))
		;;FIBROSIS
		(case Fibrosis then
			(send ?ejercicio put-porcentajeFCmax 75))
		;;DEPRESIÓN
		(case Despresion then
			(switch (send ?ejercicio get-faseEjercicio)
				(case Inicial then
					(send ?ejercicio put-porcentajeFCmax 60))
				(case Mejora then
					(send ?ejercicio put-porcentajeFCmax 75))
				(case Mantenimiento then
					(send ?ejercicio put-porcentajeFCmax 90))))
		)
	return (/ (* (calculaFCmax ?avi) (send ?ejercicio get-porcentajeFCmax)) 100)
)

(deffunction is-assigned "" (?ejercicio ?dias)
	(foreach ?dia ?dias
		; Init
		(bind ?calentamientos (send ?dia get-Calentamiento))
		(bind ?principales (send ?dia get-Principal))
		(bind ?recuperaciones (send ?dia get-Recuperacion))

		; Sum count subclass calentamiento
		(foreach ?calentamiento ?calentamientos do
			(if (eq (str-compare
				(send ?calentamiento get-nombreEjercicio)
				(send ?ejercicio get-nombreEjercicio)) 0)
				then
				(return TRUE)
			)
		)

		; Sum count subclass principales
		(foreach ?principal ?principales do
			(if (eq (str-compare
				(send ?principal get-nombreEjercicio)
				(send ?ejercicio get-nombreEjercicio)) 0)
				then
				(return TRUE)
			)
		)

		; Sum count subclass recuperaciones
		(foreach ?recuperacion ?recuperaciones do
			(if (eq (str-compare
				(send ?recuperacion get-nombreEjercicio)
				(send ?ejercicio get-nombreEjercicio)) 0)
				then
				(return TRUE)
			)
		)
	)
	(return FALSE)
)


;;;
;;;						MAIN MODULE
;;;

(defmodule MAIN
	(export ?ALL)
)

(defrule initial "initial rule"
	(initial-fact)
	=>
	(printout t "--------------------------------------------------------------" crlf)
	(printout t "--------------------------------------------------------------" crlf)
	(printout t "------------ SISTEMA DE RECOMENDACION DE EJERCICIOS ----------" crlf)
	(printout t "--------------------------------------------------------------" crlf)
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
			(assert (n_forma_def ?nivelDeForma))
			(if (eq (str-compare ?dependencia "Dependiente") 0)
				then (make-instance ?nombre of Dependiente (nombre ?nombre)
																									 (edad ?edad)
																									 (sexo ?sexo)
																									 (nivelDeForma ?nivelDeForma)
																									 (esFragil TRUE))
							(assert (Dependiente))
				else (bind ?esFragil (binary-question "Es fragil"))
							(make-instance ?nombre of Independiente (nombre ?nombre)
																		 (edad ?edad)
																		 (sexo ?sexo)
																		 (nivelDeForma ?nivelDeForma)
																		 (esFragil ?esFragil))
							(assert (Independiente))
			)
			(bind ?esfuerzo (question-with-default-values "Esfuerzo dispuesto a asumir" "Bajo(0-3)/Moderado(4-6)/Alto(7-10)"))
			(assert (Avi ?nombre))
		else
			(printout t "No cumple los requisitos de edad para utilizar este programa." crlf)
			(retract 1) ; El hecho 1 es new_avi.
			(assert (FIN))
	)
)


(defrule noEjercicioSi "rule to check critical states"
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
			(printout t crlf)
			(printout t "--------------------------------------------------------------" crlf)
			(printout t "-------------------- Dolencias musculares --------------------" crlf)
			(printout t "--------------------------------------------------------------" crlf)
			(printout t crlf)
			(focus ask_questions)
	)
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
	(bind ?enfCard (binary-question "Padece o quiere prevenir una enfermedad Cardiovascular" ))
	(if ?enfCard then
		(assert (enfermedadCardiovascular))
		(assert (aCardiovascular 2))
	)
)

(defrule diabetes "rule to know if avi have diabetes"
	(new_avi)
	=>
	(bind ?diabetes (binary-question "Padece o quiere prevenir la Diabetes"))
	(if ?diabetes then
		(assert (diabetes))
		(assert (aDiabetes 3))
	)
)

(defrule fragilidad "rule to know if avi is fragile"
	(new_avi)
	?h <- (Avi ?nombre)
	=>
	(bind ?abuelos (find-instance ((?a Independiente)) (eq (str-compare ?a:nombre ?nombre) 0)))
	(if (eq (length$ ?abuelos) 1) then
		(bind ?abu (nth$ 1 ?abuelos))
		(bind ?fragilidad (send ?abu get-esFragil))
		(if ?fragilidad then
			(assert (fragil))
			(assert (aFragilidad 3))
		)
	)
)

(defrule hipertension "rule to know if avi have hipertension"
	(new_avi)
	=>
	(bind ?hipertension (binary-question "Padece o quiere prevenir la Hipertensión"))
	(if ?hipertension then
		(assert (hipertension))
		(assert (aHipertension 7))
	)
)

(defrule sobrepeso "rule to know if avi have sobrepeso or obesidad"
	(new_avi)
	=>
	(bind ?sobrepeso (binary-question "Padece o quiere prevenir el Sobrepeso/obesidad"))
	(if ?sobrepeso then
		(assert (sobrepeso))
		(assert (aSobrepeso 7))
	)
)

(defrule pulmonar "rule to know if avi have a pulmonar disease"
	(new_avi)
	=>
	(bind ?pulmonar (binary-question "Padece o quiere prevenir una enfermedad Pulmonar"))
	(if ?pulmonar then
		(assert (pulmonar))
		(assert (aPulmonar 4))
	)
)

(defrule osteoporosis "rule to know if avi have osteoporosis"
	(new_avi)
	=>
	(bind ?osteoporosis (binary-question "Padece o quiere prevenir la Osteoporosis"))
	(if ?osteoporosis then
		(assert (osteoporosis))
		(assert (aOsteoporosis 6))
	)
)

(defrule cancer "rule to know if avi have cancer"
	(new_avi)
	=>
	(bind ?cancer (binary-question "Padece o quiere prevenir un Cáncer"))
	(if ?cancer then
		(assert (cancer))
		(assert (aCancer 5))
	)
)

(defrule artritis "rule to know if avi have artritis"
	(new_avi)
	=>
	(bind ?artritis (binary-question "Padece o quiere prevenir la Artritis"))
	(if ?artritis then
		(assert (artritis))
		(assert (aArtritis 11))
	)
)

(defrule fibrosis "rule to know if avi have fibrosis"
	(new_avi)
	=>
	(bind ?fibrosis (binary-question "Padece o quiere prevenir la Fibrosis quística"))
	(if ?fibrosis then
		(assert (fibrosis))
		(assert (aFibrosis 7))
	)
)

(defrule depresion "rule to know if avi have depresion"
	(new_avi)
	=>
	(bind ?depresion (binary-question "Padece o quiere prevenir la Depresión"))
	(if ?depresion then
		(assert (depresion))
		(assert (aDepresion 4))
	)
)

(defrule check_partes_del_cuerpo "rule to know the status of different parts of the body"
	(new_avi)
	=>
	(printout t crlf)
	(printout t "--------------------------------------------------------------" crlf)
	(printout t "-------------------- Dolencias musculares --------------------" crlf)
	(printout t "--------------------------------------------------------------" crlf)
	(printout t crlf)
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
	(bind ?abdominales (binary-question "Presenta dolencia en el Abdomen?"))
	(if (not ?abdominales) then (assert (abdominalesCorrectos)))
	(bind ?hombros (binary-question "Presenta dolencia en los Hombros?"))
	(if (not ?hombros) then (assert (hombrosCorrectos)))
	(bind ?cintura (binary-question "Presenta dolencia en la Cintura?"))
	(if (not ?cintura) then (assert (cinturaCorrecta)))
	(bind ?rodillaDerecha (binary-question "Presenta dolencia en la Rodilla Derecha?"))
	(if (not ?rodillaDerecha) then (assert (rodillaDerechaCorrecta)))
	(bind ?rodillaIzquierda (binary-question "Presenta dolencia en la Rodilla Izquierda"))
	(if (not ?rodillaIzquierda) then (assert (rodillaIzquierdaCorrecta)))
)

(defrule material "rule to know the material that is available"
	(new_avi)
	=>
	(printout t crlf)
	(printout t "--------------------------------------------------------------" crlf)
	(printout t "------------------- Disposición de material ------------------" crlf)
	(printout t "--------------------------------------------------------------" crlf)
	(printout t crlf)
	(printout t "Indique de los siguientes materiales si dispone de ellos o no:" crlf)
	(bind ?colchoneta (binary-question "Colchoneta"))
	(if ?colchoneta then (assert (tieneColchoneta)))
	(bind ?mancuernas (binary-question "Mancuernas"))
	(if ?mancuernas then (assert (tieneMancuernas)))
	(bind ?balon (binary-question "Balón medicinal"))
	(if ?balon then (assert (tieneBalon)))

)

(defrule noMoreQuestions "rule to activate the next module"
	(new_avi)
	=>
	(printout t "End of questions" crlf crlf)
	(bind ?planilla (make-instance planilla of Planilla
		(fase Inicial)
		(dias (find-all-instances ((?d Dia)) (neq ?d [nil])))
	))
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

(defrule ejercicioEstiramientoBicepDerecho "rule to add exercise to the plan"
	(new_avi)
	(bicepDerechoCorrecto)
	;(or (cancer) (fragil) (artritis))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoBicepDerecho") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (not (eq (str-compare ?nivelDeForma Bajo) 0)) then
		(setDuracion ?exe (* 2 (send ?exe get-duracion))))
)

(defrule ejercicioEstiramientoBicepIzquierdo "rule to add exercise to the plan"
	(new_avi)
	(bicepIzquierdoCorrecto)
	;(or (cancer) (fragil) (artritis))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoBicepIzquierdo") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (not (eq (str-compare ?nivelDeForma Bajo) 0)) then
		(setDuracion ?exe (* 2 (send ?exe get-duracion))))
)

(defrule ejercicioEstiramientoCadera "rule to add exercise to the plan"
	(new_avi)
	(caderaCorrecta)
	;(or (cancer) (fragil) (artritis))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoCadera") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (not (eq (str-compare ?nivelDeForma Bajo) 0)) then
		(setDuracion ?exe (* 2 (send ?exe get-duracion))))
)

(defrule ejercicioEstiramientoCintura "rule to add exercise to the plan"
	(new_avi)
	(cinturaCorrecta)
	;(or (cancer) (fragil) (artritis))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoCintura") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (not (eq (str-compare ?nivelDeForma Bajo) 0)) then
		(setDuracion ?exe (* 2 (send ?exe get-duracion))))
)

(defrule ejercicioEstiramientoCuadricepDerecho "rule to add exercise to the plan"
	(new_avi)
	(cuadricepDerechoCorrecto)
	;(or (cancer) (fragil) (artritis))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoCuadricepDerecho") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (not (eq (str-compare ?nivelDeForma Bajo) 0)) then
		(setDuracion ?exe (* 2 (send ?exe get-duracion))))
)

(defrule ejercicioEstiramientoCuadricepIzquierdo "rule to add exercise to the plan"
	(new_avi)
	(cuadricepIzquierdoCorrecto)
	;(or (cancer) (fragil) (artritis))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoCuadricepIzquierdo") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (not (eq (str-compare ?nivelDeForma Bajo) 0)) then
		(setDuracion ?exe (* 2 (send ?exe get-duracion))))
)

(defrule ejercicioEstiramientoCuello "rule to add exercise to the plan"
	(new_avi)
	(cuelloCorrecto)
	;(or (cancer) (fragil) (artritis))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoCuello") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (not (eq (str-compare ?nivelDeForma Bajo) 0)) then
		(setDuracion ?exe (* 2 (send ?exe get-duracion))))
)

(defrule ejercicioEstiramientoEspalda "rule to add exercise to the plan"
	(new_avi)
	(espaldaCorrecta)
	;(or (cancer) (fragil) (artritis))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoEspalda") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (not (eq (str-compare ?nivelDeForma Bajo) 0)) then
		(setDuracion ?exe (* 2 (send ?exe get-duracion))))
)

(defrule ejercicioEstiramientoGemeloDerecho "rule to add exercise to the plan"
	(new_avi)
	(gemeloDerechoCorrecto)
	;(or (cancer) (fragil) (artritis))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoGemeloDerecho") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (not (eq (str-compare ?nivelDeForma Bajo) 0)) then
		(setDuracion ?exe (* 2 (send ?exe get-duracion))))
)

(defrule ejercicioEstiramientoGemeloIzquierdo "rule to add exercise to the plan"
	(new_avi)
	(gemeloIzquierdoCorrecto)
	;(or (cancer) (fragil) (artritis))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoGemeloIzquierdo") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (not (eq (str-compare ?nivelDeForma Bajo) 0)) then
		(setDuracion ?exe (* 2 (send ?exe get-duracion))))
)

(defrule ejercicioEstiramientoHombros "rule to add exercise to the plan"
	(new_avi)
	(hombrosCorrectos)
	;(or (cancer) (fragil) (artritis))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoHombros") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (not (eq (str-compare ?nivelDeForma Bajo) 0)) then
		(setDuracion ?exe (* 2 (send ?exe get-duracion))))
)

(defrule ejercicioEstiramientoTobilloDerecho "rule to add exercise to the plan"
	(new_avi)
	(tobilloDerechoCorrecto)
	;(or (cancer) (fragil) (artritis))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoTobilloDerecho") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (not (eq (str-compare ?nivelDeForma Bajo) 0)) then
		(setDuracion ?exe (* 2 (send ?exe get-duracion))))
)

(defrule ejercicioEstiramientoTobilloIzquierdo "rule to add exercise to the plan"
	(new_avi)
	(tobilloIzquierdoCorrecto)
	;(or (cancer) (fragil) (artritis))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoTobilloIzquierdo") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (not (eq (str-compare ?nivelDeForma Bajo) 0)) then
		(setDuracion ?exe (* 2 (send ?exe get-duracion))))
)

(defrule ejercicioEstiramientoTorso "rule to add exercise to the plan"
	(new_avi)
	(torsoCorrecto)
	;(or (cancer) (fragil) (artritis))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoTorso") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (not (eq (str-compare ?nivelDeForma Bajo) 0)) then
		(setDuracion ?exe (* 2 (send ?exe get-duracion))))
)

(defrule ejercicioEstiramientoTricepDerecho "rule to add exercise to the plan"
	(new_avi)
	(tricepDerechoCorrecto)
	;(or (cancer) (fragil) (artritis))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoTricepDerecho") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (not (eq (str-compare ?nivelDeForma Bajo) 0)) then
		(setDuracion ?exe (* 2 (send ?exe get-duracion))))
)

(defrule ejercicioEstiramientoTricepIzquierdo "rule to add exercise to the plan"
	(new_avi)
	(tricepIzquierdoCorrecto)
	;(or (cancer) (fragil) (artritis))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "EstiramientoTricepIzquierdo") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (not (eq (str-compare ?nivelDeForma Bajo) 0)) then
		(setDuracion ?exe (* 2 (send ?exe get-duracion))))
)

;;;						FUERZA

(defrule ejercicioFortalecimientoAbdominales "rule to add exercise to the plan"
	(new_avi)
	(abdominalesCorrectos)
	;(or (sobrepeso) (pulmonar) (diabetes))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Abdominales") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	;;;Ejercicios con 3 series de 10 repeticiones en Inicial
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setRepeticiones ?exe (+ 5 (send ?exe get-repeticiones))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series)))
		(setRepeticiones ?exe (+ 7 (send ?exe get-repeticiones))))

	(setDuracionFuerza ?exe)
)

(defrule ejercicioElevacionPiernas "rule to add exercise to the plan"
	(new_avi)
	(caderaCorrecta)
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "ElevacionPiernas") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	;;;Ejercicios con 3 series de 10 repeticiones en Inicial
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setRepeticiones ?exe (+ 5 (send ?exe get-repeticiones))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series)))
		(setRepeticiones ?exe (+ 7 (send ?exe get-repeticiones))))
	(setDuracionFuerza ?exe)
)

(defrule ejercicioElevacionRodillas "rule to add exercise to the plan"
	(new_avi)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "ElevacionRodillas") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	;;;Ejercicios con 3 series de 10 repeticiones en Inicial
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setRepeticiones ?exe (+ 5 (send ?exe get-repeticiones))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series)))
		(setRepeticiones ?exe (+ 7 (send ?exe get-repeticiones))))
	(setDuracionFuerza ?exe)
)

(defrule ejercicioExtensionCadera "rule to add exercise to the plan"
	(new_avi)
	(caderaCorrecta)
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "ExtensionCadera") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	;;;Ejercicios con 3 series de 10 repeticiones en Inicial
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setRepeticiones ?exe (+ 5 (send ?exe get-repeticiones))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series)))
		(setRepeticiones ?exe (+ 7 (send ?exe get-repeticiones))))
	(setDuracionFuerza ?exe)
)

(defrule ejercicioExtensionRodillas "rule to add exercise to the plan"
	(new_avi)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "ExtensionRodillas") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	;;;Ejercicios con 3 series de 10 repeticiones en Inicial
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setRepeticiones ?exe (+ 5 (send ?exe get-repeticiones))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series)))
		(setRepeticiones ?exe (+ 7 (send ?exe get-repeticiones))))
	(setDuracionFuerza ?exe)
)

(defrule ejercicioExtensionTriceps "rule to add exercise to the plan"
	(new_avi)
	(tricepDerechoCorrecto)
	(tricepIzquierdoCorrecto)
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "ExtensionTriceps") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	;;;Ejercicios con 3 series de 10 repeticiones en Inicial
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setRepeticiones ?exe (+ 5 (send ?exe get-repeticiones))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series)))
		(setRepeticiones ?exe (+ 7 (send ?exe get-repeticiones))))
	(setDuracionFuerza ?exe)
)

(defrule ejercicioFlexionCadera "rule to add exercise to the plan"
	(new_avi)
	(caderaCorrecta)
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "FlexionCadera") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	;;;Ejercicios con 3 series de 10 repeticiones en Inicial
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setRepeticiones ?exe (+ 5 (send ?exe get-repeticiones))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series)))
		(setRepeticiones ?exe (+ 7 (send ?exe get-repeticiones))))
	(setDuracionFuerza ?exe)
)

(defrule ejercicioFortalecimientoFlexiones "rule to add exercise to the plan"
	(new_avi)
	(bicepDerechoCorrecto)
	(bicepIzquierdoCorrecto)
	;(or (sobrepeso) (pulmonar))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Flexiones") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	;;;Ejercicios con 3 series de 5 repeticiones en Inicial
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series)))
		(setRepeticiones ?exe (+ 2 (send ?exe get-repeticiones))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setSeries ?exe (+ 2 (send ?exe get-series)))
		(setRepeticiones ?exe (+ 5 (send ?exe get-repeticiones))))
	(setDuracionFuerza ?exe)
)

(defrule ejercicioFlexionPlantar "rule to add exercise to the plan"
	(new_avi)
	(tobilloDerechoCorrecto)
	(tobilloIzquierdoCorrecto)
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "FlexionPlantar") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	;;;Ejercicios con 3 series de 10 repeticiones en Inicial
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setRepeticiones ?exe (+ 5 (send ?exe get-repeticiones))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series)))
		(setRepeticiones ?exe (+ 7 (send ?exe get-repeticiones))))
	(setDuracionFuerza ?exe)
)

(defrule ejercicioFlexionRodillas"rule to add exercise to the plan"
	(new_avi)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "FlexionRodillas") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	;;;Ejercicios con 3 series de 10 repeticiones en Inicial
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setRepeticiones ?exe (+ 5 (send ?exe get-repeticiones))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series)))
		(setRepeticiones ?exe (+ 7 (send ?exe get-repeticiones))))
	(setDuracionFuerza ?exe)
)

(defrule ejercicioFortalecimientoLevantarseSentarse "rule to add exercise to the plan"
	(new_avi)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(caderaCorrecta)
	(tobilloDerechoCorrecto)
	(tobilloIzquierdoCorrecto)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	;(or (sobrepeso) (pulmonar))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "LevantarseSentarse") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	;;;Ejercicios con 2/3 series de 10 repeticiones en Inicial
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setRepeticiones ?exe (+ 5 (send ?exe get-repeticiones))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series)))
		(setRepeticiones ?exe (+ 7 (send ?exe get-repeticiones))))
	(setDuracionFuerza ?exe)
)

(defrule ejercicioMaquinaEliptica "rule to add exercise to the plan"
	(new_avi)
	(abdominalesCorrectos)
	(bicepDerechoCorrecto)
	(bicepIzquierdoCorrecto)
	(caderaCorrecta)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(espaldaCorrecta)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(hombrosCorrectos)
	(tobilloDerechoCorrecto)
	(tobilloIzquierdoCorrecto)
	(torsoCorrecto)
	(tricepDerechoCorrecto)
	(tricepIzquierdoCorrecto)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "MaquinaEliptica") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	;;;Elíptica -> No tiene repeticiones ni series
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setDuracion ?exe (+ 20 (send ?exe get-duracion))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setDuracion ?exe (+ 30 (send ?exe get-duracion))))
)

(defrule ejercicioFortalecimientoPesaBicepDerecho "rule to add exercise to the plan"
	(new_avi)
	(bicepDerechoCorrecto)
	(tieneMancuernas)
	;(or (sobrepeso) (pulmonar) (osteoporosis) (cancer) (artritis) (fibrosis))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "PesaBicepDerecho") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	;;;Ejercicios con 2 series de 8 repeticiones en Inicial
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series)))
		(setRepeticiones ?exe (+ 5 (send ?exe get-repeticiones))))
	(setDuracionFuerza ?exe)
)

(defrule ejercicioFortalecimientoPesaBicepIzquierdo "rule to add exercise to the plan"
	(new_avi)
	(bicepIzquierdoCorrecto)
	(tieneMancuernas)
	;(or (sobrepeso) (pulmonar) (osteoporosis) (cancer) (artritis) (fibrosis))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "PesaBicepIzquierdo") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	;;;Ejercicios con 2 series de 8 repeticiones en Inicial
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series)))
		(setRepeticiones ?exe (+ 5 (send ?exe get-repeticiones))))
	(setDuracionFuerza ?exe)
)

(defrule ejercicioFortalecimientoPesaTricepDerecho "rule to add exercise to the plan"
	(new_avi)
	(tricepDerechoCorrecto)
	(tieneMancuernas)
	;(or (sobrepeso) (pulmonar) (osteoporosis) (cancer) (artritis) (fibrosis))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "PesaTricepDerecho") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	;;;Ejercicios con 2 series de 8 repeticiones en Inicial
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series)))
		(setRepeticiones ?exe (+ 5 (send ?exe get-repeticiones))))
	(setDuracionFuerza ?exe)
)

(defrule ejercicioFortalecimientoPesaTricepIzquierdo "rule to add exercise to the plan"
	(new_avi)
	(tricepIzquierdoCorrecto)
	(tieneMancuernas)
	;(or (sobrepeso) (pulmonar) (osteoporosis) (cancer) (artritis) (fibrosis))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "PesaTricepIzquierdo") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	;;;Ejercicios con 2 series de 8 repeticiones en Inicial
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series)))
		(setRepeticiones ?exe (+ 5 (send ?exe get-repeticiones))))
	(setDuracionFuerza ?exe)
)

(defrule ejercicioFortalecimientoSentadillas "rule to add exercise to the plan"
	(new_avi)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(caderaCorrecta)
	(tobilloDerechoCorrecto)
	(tobilloIzquierdoCorrecto)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	;(or (sobrepeso) (pulmonar))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Sentadillas") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	;;;Ejercicios con 2/3 series de 10 repeticiones en Inicial
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setRepeticiones ?exe (+ 5 (send ?exe get-repeticiones))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series)))
		(setRepeticiones ?exe (+ 7 (send ?exe get-repeticiones))))
	(setDuracionFuerza ?exe)
)

(defrule ejercicioFortalecimientoSentadillasBalon "rule to add exercise to the plan"
	(new_avi)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(caderaCorrecta)
	(tobilloDerechoCorrecto)
	(tobilloIzquierdoCorrecto)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	(tieneBalon)
	;(or (sobrepeso) (pulmonar))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "SentadillasBalon") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	;;;Ejercicios con 2/3 series de 10 repeticiones en Inicial
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setRepeticiones ?exe (+ 5 (send ?exe get-repeticiones))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series)))
		(setRepeticiones ?exe (+ 7 (send ?exe get-repeticiones))))
	(setDuracionFuerza ?exe)
)

(defrule ejercicioFortalecimientoSentadillasMancuernas "rule to add exercise to the plan"
	(new_avi)
	(tieneMancuernas)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(caderaCorrecta)
	(tobilloDerechoCorrecto)
	(tobilloIzquierdoCorrecto)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	;(or (sobrepeso) (pulmonar))
	?f <- (n_forma_def ?nivelDeForma)
	?p <- (planilla_avi ?planilla)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "SentadillasMancuernas") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	;;;Ejercicios con 2/3 series de 10 repeticiones en Inicial
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setRepeticiones ?exe (+ 5 (send ?exe get-repeticiones))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setSeries ?exe (+ 1 (send ?exe get-series)))
		(setRepeticiones ?exe (+ 7 (send ?exe get-repeticiones))))
	(setDuracionFuerza ?exe)
)

;;;						AERÓBICOS

(defrule ejercicioAndar "rule to add exercise to the plan"
	(new_avi)
	(caderaCorrecta)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(tobilloIzquierdoCorrecto)
	(tobilloDerechoCorrecto)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	(or (enfermedadCardiovascular) (fragil) (hipertension) (sobrepeso) (pulmonar) (osteoporosis) (cancer) (artritis) (fibrosis))
	?p <- (planilla_avi ?planilla)
	?f <- (n_forma_def ?nivelDeForma)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Andar") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(if (eq (str-compare (send ?exe get-faseEjercicio) Inicial) 0) then
		(setFase ?exe ?nivelDeForma)
		(if (eq (str-compare ?nivelDeForma Medio) 0) then
			(setDuracion ?exe (+ 10 (send ?exe get-duracion))))
		(if (eq (str-compare ?nivelDeForma Alto) 0) then
			(setDuracion ?exe (+ 20 (send ?exe get-duracion)))))
)

(defrule ejercicioBaile "rule to add exercise to the plan"
	(new_avi)
	(caderaCorrecta)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(tobilloIzquierdoCorrecto)
	(tobilloDerechoCorrecto)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	(depresion)
	?p <- (planilla_avi ?planilla)
	?f <- (n_forma_def ?nivelDeForma)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Baile") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(if (eq (str-compare (send ?exe get-faseEjercicio) Inicial) 0) then
		(setFase ?exe ?nivelDeForma)
		(if (eq (str-compare ?nivelDeForma Medio) 0) then
			(setDuracion ?exe (+ 10 (send ?exe get-duracion))))
		(if (eq (str-compare ?nivelDeForma Alto) 0) then
			(setDuracion ?exe (+ 20 (send ?exe get-duracion)))))
)

(defrule ejercicioBicicleta "rule to add exercise to the plan"
	(new_avi)
	(caderaCorrecta)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(tobilloIzquierdoCorrecto)
	(tobilloDerechoCorrecto)
	(espaldaCorrecta)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	(or (enfermedadCardiovascular) (hipertension) (sobrepeso) (pulmonar) (cancer) (artritis) (fibrosis))
	?p <- (planilla_avi ?planilla)
	?f <- (n_forma_def ?nivelDeForma)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Bicicleta") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (eq (str-compare (send ?exe get-faseEjercicio) Inicial) 0) then
		(if (eq (str-compare ?nivelDeForma Medio) 0) then
			(setDuracion ?exe (+ 10 (send ?exe get-duracion))))
		(if (eq (str-compare ?nivelDeForma Alto) 0) then
			(setDuracion ?exe (+ 20 (send ?exe get-duracion)))))
)

(defrule ejercicioCaminar "rule to add exercise to the plan"
	(new_avi)
	(caderaCorrecta)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(tobilloIzquierdoCorrecto)
	(tobilloDerechoCorrecto)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	(or (enfermedadCardiovascular) (fragil) (hipertension) (sobrepeso) (pulmonar) (osteoporosis) (cancer) (artritis) (fibrosis))
	?p <- (planilla_avi ?planilla)
	?f <- (n_forma_def ?nivelDeForma)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Caminar") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
		(if (eq (str-compare (send ?exe get-faseEjercicio) Inicial) 0) then
		(setFase ?exe ?nivelDeForma)
		(if (eq (str-compare ?nivelDeForma Medio) 0) then
			(setDuracion ?exe (+ 5 (send ?exe get-duracion))))
		(if (eq (str-compare ?nivelDeForma Alto) 0) then
			(setDuracion ?exe (+ 10 (send ?exe get-duracion)))))
)

(defrule ejercicioCarrera "rule to add exercise to the plan"
	(new_avi)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(tobilloDerechoCorrecto)
	(tobilloIzquierdoCorrecto)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	(caderaCorrecta)
	(or (fibrosis) (depresion) (hipertension) (cancer))
	?p <- (planilla_avi ?planilla)
	?f <- (n_forma_def ?nivelDeForma)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Carrera") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(if (eq (str-compare (send ?exe get-faseEjercicio) Inicial) 0) then
		(setFase ?exe ?nivelDeForma)
		(if (eq (str-compare ?nivelDeForma Medio) 0) then
			(setDuracion ?exe (+ 10 (send ?exe get-duracion))))
		(if (eq (str-compare ?nivelDeForma Alto) 0) then
			(setDuracion ?exe (+ 20 (send ?exe get-duracion)))))
)

(defrule ejercicioGolf "rule to add exercise to the plan"
	(new_avi)
	(abdominalesCorrectos)
	(bicepDerechoCorrecto)
	(bicepIzquierdoCorrecto)
	(caderaCorrecta)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(cuelloCorrecto)
	(espaldaCorrecta)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(hombrosCorrectos)
	(tobilloDerechoCorrecto)
	(tobilloIzquierdoCorrecto)
	(torsoCorrecto)
	(tricepDerechoCorrecto)
	(tricepIzquierdoCorrecto)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	(depresion)
	?p <- (planilla_avi ?planilla)
	?f <- (n_forma_def ?nivelDeForma)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Golf") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(if (eq (str-compare (send ?exe get-faseEjercicio) Inicial) 0) then
		(setFase ?exe ?nivelDeForma)
		(if (eq (str-compare ?nivelDeForma Medio) 0) then
			(setDuracion ?exe (+ 10 (send ?exe get-duracion))))
		(if (eq (str-compare ?nivelDeForma Alto) 0) then
			(setDuracion ?exe (+ 20 (send ?exe get-duracion)))))
)

(defrule ejercicioMarcha "rule to add exercise to the plan"
	(new_avi)
	(caderaCorrecta)
	(cuadricepDerecho)
	(cuadricepIzquierdo)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(tobilloDerechoCorrecto)
	(tobilloIzquierdoCorrecto)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	(or (enfermedadCardiovascular) (hipertension) (sobrepeso) (osteoporosis) (cancer) (artritis) (fibrosis) (depresion))
	?p <- (planilla_avi ?planilla)
	?f <- (n_forma_def ?nivelDeForma)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Marcha") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (eq (str-compare (send ?exe get-faseEjercicio) Inicial) 0) then
		(if (eq (str-compare ?nivelDeForma Medio) 0) then
			(setDuracion ?exe (+ 5 (send ?exe get-duracion))))
		(if (eq (str-compare ?nivelDeForma Alto) 0) then
			(setDuracion ?exe (+ 15 (send ?exe get-duracion)))))
)

(defrule ejercicioNatacion "rule to add exercise to the plan"
	(new_avi)
	(abdominalesCorrectos)
	(bicepDerechoCorrecto)
	(bicepIzquierdoCorrecto)
	(caderaCorrecta)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(cuelloCorrecto)
	(espaldaCorrecta)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(hombrosCorrectos)
	(tobilloDerechoCorrecto)
	(tobilloIzquierdoCorrecto)
	(torsoCorrecto)
	(tricepDerechoCorrecto)
	(tricepIzquierdoCorrecto)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	(or (diabetes) (fibrosis))
	?p <- (planilla_avi ?planilla)
	?f <- (n_forma_def ?nivelDeForma)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Natacion") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (eq (str-compare (send ?exe get-faseEjercicio) Inicial) 0) then
		(if (eq (str-compare ?nivelDeForma Medio) 0) then
			(setDuracion ?exe (+ 5 (send ?exe get-duracion))))
		(if (eq (str-compare ?nivelDeForma Alto) 0) then
			(setDuracion ?exe (+ 15 (send ?exe get-duracion)))))
)

(defrule ejercicioPaseo "rule to add exercise to the plan"
	(new_avi)
	(caderaCorrecta)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(tobilloIzquierdoCorrecto)
	(tobilloDerechoCorrecto)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	(or (enfermedadCardiovascular) (fragil) (hipertension) (sobrepeso) (pulmonar) (osteoporosis) (cancer) (artritis) (fibrosis))
	?p <- (planilla_avi ?planilla)
	?f <- (n_forma_def ?nivelDeForma)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Paseo") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(if (eq (str-compare (send ?exe get-faseEjercicio) Inicial) 0) then
		(setFase ?exe ?nivelDeForma)
		(if (eq (str-compare ?nivelDeForma Medio) 0) then
			(setDuracion ?exe (+ 15 (send ?exe get-duracion))))
		(if (eq (str-compare ?nivelDeForma Alto) 0) then
			(setDuracion ?exe (+ 30 (send ?exe get-duracion)))))
)

(defrule ejercicioPatinaje "rule to add exercise to the plan"
	(new_avi)
	(abdominalesCorrectos)
	(caderaCorrecta)
	(cuadricepDerecho)
	(cuadricepIzquierdo)
	(espaldaCorrecta)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(tobilloDerechoCorrecto)
	(tobilloIzquierdoCorrecto)
	(torsoCorrecto)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	(depresion)
	?p <- (planilla_avi ?planilla)
	?f <- (n_forma_def ?nivelDeForma)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Patinaje") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(if (eq (str-compare (send ?exe get-faseEjercicio) Inicial) 0) then
		(setFase ?exe ?nivelDeForma)
		(if (eq (str-compare ?nivelDeForma Medio) 0) then
			(setDuracion ?exe (+ 10 (send ?exe get-duracion))))
		(if (eq (str-compare ?nivelDeForma Alto) 0) then
			(setDuracion ?exe (+ 20 (send ?exe get-duracion)))))
)

(defrule ejercicioPilates "rule to add exercise to the plan"
	(new_avi)
	(abdominalesCorrectos)
	(bicepDerechoCorrecto)
	(bicepIzquierdoCorrecto)
	(caderaCorrecta)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(cuelloCorrecto)
	(espaldaCorrecta)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(hombrosCorrectos)
	(tobilloDerechoCorrecto)
	(tobilloIzquierdoCorrecto)
	(torsoCorrecto)
	(tricepDerechoCorrecto)
	(tricepIzquierdoCorrecto)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	(artritis)
	?p <- (planilla_avi ?planilla)
	?f <- (n_forma_def ?nivelDeForma)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Pilates") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(if (eq (str-compare (send ?exe get-faseEjercicio) Inicial) 0) then
		(setFase ?exe ?nivelDeForma)
		(if (eq (str-compare ?nivelDeForma Medio) 0) then
			(setDuracion ?exe (+ 10 (send ?exe get-duracion))))
		(if (eq (str-compare ?nivelDeForma Alto) 0) then
			(setDuracion ?exe (+ 20 (send ?exe get-duracion)))))
)

(defrule ejercicioSenderismo "rule to add exercise to the plan"
	(new_avi)
	(abdominalesCorrectos)
	(bicepDerechoCorrecto)
	(bicepIzquierdoCorrecto)
	(caderaCorrecta)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(cuelloCorrecto)
	(espaldaCorrecta)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(hombrosCorrectos)
	(tobilloDerechoCorrecto)
	(tobilloIzquierdoCorrecto)
	(torsoCorrecto)
	(tricepDerechoCorrecto)
	(tricepIzquierdoCorrecto)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	(depresion)
	?p <- (planilla_avi ?planilla)
	?f <- (n_forma_def ?nivelDeForma)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Senderismo") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(if (eq (str-compare (send ?exe get-faseEjercicio) Inicial) 0) then
		(setFase ?exe ?nivelDeForma))
	;;;El senderismo ya tiene suficiente duración en la fase Inicial
)

(defrule ejercicioSubirEscaleras "rule to add exercise to the plan"
	(new_avi)
	(caderaCorrecta)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(tobilloDerechoCorrecto)
	(tobilloIzquierdoCorrecto)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	(or (osteoporosis) (fragil))
	?p <- (planilla_avi ?planilla)
	?f <- (n_forma_def ?nivelDeForma)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "SubirEscaleras") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(if (eq (str-compare (send ?exe get-faseEjercicio) Inicial) 0) then
		(setFase ?exe ?nivelDeForma)
		(if (eq (str-compare ?nivelDeForma Medio) 0) then
			(setDuracion ?exe (+ 5 (send ?exe get-duracion))))
		(if (eq (str-compare ?nivelDeForma Alto) 0) then
			(setDuracion ?exe (+ 15 (send ?exe get-duracion)))))
)

(defrule ejercicioYoga "rule to add exercise to the plan"
	(new_avi)
	(abdominalesCorrectos)
	(bicepDerechoCorrecto)
	(bicepIzquierdoCorrecto)
	(caderaCorrecta)
	(cuadricepDerechoCorrecto)
	(cuadricepIzquierdoCorrecto)
	(cuelloCorrecto)
	(espaldaCorrecta)
	(gemeloDerechoCorrecto)
	(gemeloIzquierdoCorrecto)
	(hombrosCorrectos)
	(tobilloDerechoCorrecto)
	(tobilloIzquierdoCorrecto)
	(torsoCorrecto)
	(tricepDerechoCorrecto)
	(tricepIzquierdoCorrecto)
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	(artritis)
	?p <- (planilla_avi ?planilla)
	?f <- (n_forma_def ?nivelDeForma)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "Yoga") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(if (eq (str-compare (send ?exe get-faseEjercicio) Inicial) 0) then
		(setFase ?exe ?nivelDeForma)
		(if (eq (str-compare ?nivelDeForma Medio) 0) then
			(setDuracion ?exe (+ 10 (send ?exe get-duracion))))
		(if (eq (str-compare ?nivelDeForma Alto) 0) then
			(setDuracion ?exe (+ 20 (send ?exe get-duracion)))))
)

;;;EQUILIBRIO

(defrule ejercicioSobreUnPie "rule to add exercise to the plan"
	(new_avi)
	(caderaCorrecta)
	(or (cuadricepDerechoCorrecto) (cuadricepIzquierdoCorrecto))
	(or (gemeloDerechoCorrecto) (gemeloIzquierdoCorrecto))
	(or (tobilloDerechoCorrecto) (tobilloIzquierdoCorrecto))
	(or (rodillaDerechaCorrecta) (rodillaIzquierdaCorrecta))
	?p <- (planilla_avi ?planilla)
	?f <- (n_forma_def ?nivelDeForma)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "SobreUnPie") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setDuracion ?exe (+ 1 (send ?exe get-duracion))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setDuracion ?exe (+ 2 (send ?exe get-duracion))))
)

(defrule ejercicioTaichi "rule to add exercise to the plan"
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
	(rodillaDerechaCorrecta)
	(rodillaIzquierdaCorrecta)
	;(or (fragil) (hipertension) (sobrepeso) (osteoporosis))
	(tieneColchoneta)
	?p <- (planilla_avi ?planilla)
	?f <- (n_forma_def ?nivelDeForma)
	=>
	(bind ?ex (find-instance ((?e Ejercicio)) (eq (str-compare ?e:nombreEjercicio "TaiChi") 0)))
	(bind ?exe (nth$ 1 ?ex))
	(send ?exe put-partOf ?planilla)
	(setFase ?exe ?nivelDeForma)
	(if (eq (str-compare ?nivelDeForma Medio) 0) then
		(setDuracion ?exe (+ 15 (send ?exe get-duracion))))
	(if (eq (str-compare ?nivelDeForma Alto) 0) then
		(setDuracion ?exe (+ 30 (send ?exe get-duracion))))
)

(defrule finEjercicios
	(new_avi)
	=>
	(focus recomendation)
)



;;;
;;;						RECOMMENDATION MODULE
;;;

(defmodule recommendation
	(import MAIN ?ALL)
	(import ask_questions ?ALL)
	(import inference_of_data ?ALL)
	(export ?ALL)
)

(defrule assignFragilidad
	(declare (salience 0))
	(new_avi)
	?assignFragilidad <- (aFragilidad ?i&:(> ?i 0))
	?ejercicio <- (object
		(is-a ?class&:(subclassp ?class Ejercicio))
		(diasALaSemana ?j&:(> ?j 0))
		(tipo Actividad | Otro)
		(nombreEjercicio "Caminar" | "Bicicleta")
		(partOf ?planilla&:(neq ?planilla [nil]))
	)
	?dia <- (object (is-a Dia))
	(not (done ?ejercicio ?dia))
	=>
	(assert (done ?ejercicio ?dia))
	(if (canAsssign ?ejercicio ?dia ?class 8 TRUE)
		then
		(assignPrincipal ?ejercicio ?dia ?*max-duracion* ?*max-calentamientos* TRUE 6)
		(retract ?assignFragilidad)
		(assert (aFragilidad (- ?i 1)))
	)
)

(defrule assignCardiovascular
	(declare (salience 0))
	(new_avi)
	?assignCardiovascular <- (aCardiovascular ?i&:(> ?i 0))
	?ejercicio <- (object
		(is-a ?class&:(subclassp ?class Ejercicio))
		(diasALaSemana ?j&:(> ?j 0))
		(tipo Actividad | Otro)
		(nombreEjercicio "Paseo" | "Bicicleta" | "Andar")
		(partOf ?planilla&:(neq ?planilla [nil]))
	)
	?dia <- (object (is-a Dia))
	(not (done ?ejercicio ?dia))
	=>
	(assert (done ?ejercicio ?dia))
	(if (canAsssign ?ejercicio ?dia ?class 5 TRUE)
		then
		(assignPrincipal ?ejercicio ?dia ?*max-duracion* ?*max-calentamientos* TRUE 5)
		(retract ?assignCardiovascular)
		(assert (aCardiovascular (- ?i 1)))
	)
)

(defrule assignHipertension
	(declare (salience 0))
	(new_avi)
	?assignHipertension <- (aHipertension ?i&:(> ?i 0))
	?ejercicio <- (object
		(is-a ?class&:(subclassp ?class Ejercicio))
		(diasALaSemana ?j&:(> ?j 0))
		(tipo Actividad | Otro)
		(nombreEjercicio "Caminar" | "Trotar" | "Correr" | "Remo" | "Nadar" | "Bicicleta"
		 | "Abdominales" | "ElevacionPiernas" | "ElevacionRodillas" | "ExtensionCadera"
		 | "ExtensionRodillas" | "ExtensionTriceps" | "FlexionCadera" | "Flexiones"
		 | "FlexionPlantar" | "FlexionRodillas" | "LevantarseSentarse" | "MaquinaEliptica"
		 | "PesaBicepDerecho" | "PesaBicepIzquierdo" | "PesaTricepDerecho" | "PesaTricepIzquierdo"
		 | "Sentadillas" | "SentadillasBalon" | "SentadillasMancuernas" | "TaiChi" | "SobreUnPie"
		)
		(partOf ?planilla&:(neq ?planilla [nil]))
	)
	?dia <- (object (is-a Dia))
	(not (done ?ejercicio ?dia))
	=>
	(assert (done ?ejercicio ?dia))
	(if (canAsssign ?ejercicio ?dia ?class 10 TRUE)
		then
		(assignPrincipal ?ejercicio ?dia 60 ?*max-calentamientos* FALSE 0)
		(retract ?assignHipertension)
		(assert (aHipertension (- ?i 1)))
	)
)

(defrule assignSobrepeso
	(declare (salience 0))
	(new_avi)
	?assignSobrepeso <- (aSobrepeso ?i&:(> ?i 0))
	?ejercicio <- (object
		(is-a ?class&:(subclassp ?class Ejercicio))
		(diasALaSemana ?j&:(> ?j 0))
		(tipo Actividad | Otro)
		(nombreEjercicio "Caminar" | "Trotar" | "Bicicleta" | "Abdominales"
		| "ElevacionPiernas" | "ElevacionRodillas" | "ExtensionCadera"
		| "ExtensionRodillas" | "ExtensionTriceps" | "FlexionCadera" | "Flexiones"
		| "FlexionPlantar" | "FlexionRodillas" | "LevantarseSentarse" | "MaquinaEliptica"
		| "PesaBicepDerecho" | "PesaBicepIzquierdo" | "PesaTricepDerecho" | "PesaTricepIzquierdo"
		| "Sentadillas" | "SentadillasBalon" | "SentadillasMancuernas")
		(partOf ?planilla&:(neq ?planilla [nil]))
	)
	?dia <- (object (is-a Dia))
	(not (done ?ejercicio ?dia))
	=>
	(assert (done ?ejercicio ?dia))
	(if (canAsssign ?ejercicio ?dia ?class 10 TRUE)
		then
		(assignPrincipal ?ejercicio ?dia 60 ?*max-calentamientos* FALSE 0)
		(retract ?assignSobrepeso)
		(assert (aSobrepeso (- ?i 1)))
	)
)

(defrule assignDiabetes
	(declare (salience 0))
	(new_avi)
	?assignDiabetes <- (aDiabetes ?i&:(> ?i 0))
	?ejercicio <- (object
		(is-a ?class&:(subclassp ?class Ejercicio))
		(diasALaSemana ?j&:(> ?j 0))
		(tipo Actividad | Otro)
		(nombreEjercicio "Natacion" | "Remo" | "Bicicleta" | "MaquinaEliptica")
		(partOf ?planilla&:(neq ?planilla [nil]))
	)
	?dia <- (object (is-a Dia))
	(not (done ?ejercicio ?dia))
	=>
	(assert (done ?ejercicio ?dia))
	(if (canAsssign ?ejercicio ?dia ?class 8 TRUE)
		then
		(assignPrincipal ?ejercicio ?dia ?*max-duracion* ?*max-calentamientos* TRUE 6)
		(retract ?assignDiabetes)
		(assert (aDiabetes (- ?i 1)))
	)
)

(defrule assignPulmonar
	(declare (salience 0))
	(new_avi)
	?assignPulmonar <- (aPulmonar ?i&:(> ?i 0))
	?ejercicio <- (object
		(is-a ?class&:(subclassp ?class Ejercicio))
		(diasALaSemana ?j&:(> ?j 0))
		(tipo Actividad | Otro)
		(nombreEjercicio "Andar" | "Bicicleta" | "Abdominales" | "ElevacionPiernas"
		| "ElevacionRodillas" | "ExtensionCadera" | "ExtensionRodillas" | "ExtensionTriceps"
		| "FlexionCadera" | "Flexiones" | "FlexionPlantar" | "FlexionRodillas" | "LevantarseSentarse"
		| "MaquinaEliptica" | "PesaBicepDerecho" | "PesaBicepIzquierdo" | "PesaTricepDerecho"
		| "PesaTricepIzquierdo" | "Sentadillas" | "SentadillasBalon" | "SentadillasMancuernas")
		(partOf ?planilla&:(neq ?planilla [nil]))
	)
	?dia <- (object (is-a Dia))
	(not (done ?ejercicio ?dia))
	=>
	(assert (done ?ejercicio ?dia))
	(if (canAsssign ?ejercicio ?dia ?class 5 TRUE)
		then
		(assignPrincipal ?ejercicio ?dia ?*max-duracion* ?*max-calentamientos* TRUE 5)
		(retract ?assignPulmonar)
		(assert (aPulmonar (- ?i 1)))
	)
)

(defrule assignOsteoporosis
	(declare (salience 0))
	(new_avi)
	?assignOsteoporosis <- (aOsteoporosis ?i&:(> ?i 0))
	?ejercicio <- (object
		(is-a ?class&:(subclassp ?class Ejercicio))
		(diasALaSemana ?j&:(> ?j 0))
		(tipo Actividad | Otro)
		(nombreEjercicio "Andar" | "Caminar" | "SubirEscaleras" | "Abdominales" | "ElevacionPiernas"
		| "ElevacionRodillas" | "ExtensionCadera" | "ExtensionRodillas" | "ExtensionTriceps"
		| "FlexionCadera" | "Flexiones" | "FlexionPlantar" | "FlexionRodillas" | "LevantarseSentarse"
		| "MaquinaEliptica" | "PesaBicepDerecho" | "PesaBicepIzquierdo" | "PesaTricepDerecho"
		| "PesaTricepIzquierdo" | "Sentadillas" | "SentadillasBalon" | "SentadillasMancuernas")
		(partOf ?planilla&:(neq ?planilla [nil]))
	)
	?dia <- (object (is-a Dia))
	(not (done ?ejercicio ?dia))
	=>
	(assert (done ?ejercicio ?dia))
	(if (canAsssign ?ejercicio ?dia ?class 13 TRUE)
		then
		(assignPrincipal ?ejercicio ?dia ?*max-duracion* ?*max-calentamientos* TRUE 13)
		(retract ?assignOsteoporosis)
		(assert (aOsteoporosis (- ?i 1)))
	)
)

(defrule assignCancer
	(declare (salience 0))
	(new_avi)
	?assignCancer <- (aCancer ?i&:(> ?i 0))
	?ejercicio <- (object
		(is-a ?class&:(subclassp ?class Ejercicio))
		(diasALaSemana ?j&:(> ?j 0))
		(tipo Actividad | Otro)
		(nombreEjercicio "Andar" | "Correr" | "Bicicleta" | "Nadar" | "Remo"
		| "SubirEscaleras" | "Abdominales" | "ElevacionPiernas"
		| "ElevacionRodillas" | "ExtensionCadera" | "ExtensionRodillas" | "ExtensionTriceps"
		| "FlexionCadera" | "Flexiones" | "FlexionPlantar" | "FlexionRodillas" | "LevantarseSentarse"
		| "MaquinaEliptica" | "PesaBicepDerecho" | "PesaBicepIzquierdo" | "PesaTricepDerecho"
		| "PesaTricepIzquierdo" | "Sentadillas" | "SentadillasBalon" | "SentadillasMancuernas")
		(partOf ?planilla&:(neq ?planilla [nil]))
	)
	?dia <- (object (is-a Dia))
	(not (done ?ejercicio ?dia))
	=>
	(assert (done ?ejercicio ?dia))
	(if (canAsssign ?ejercicio ?dia ?class 13 TRUE)
		then
		(assignPrincipal ?ejercicio ?dia ?*max-duracion* ?*max-calentamientos* TRUE 13)
		(retract ?assignCancer)
		(assert (aCancer (- ?i 1)))
	)
)

(defrule assignArtritis
	(declare (salience 0))
	(new_avi)
	?assignArtritis <- (aArtritis ?i&:(> ?i 0))
	?ejercicio <- (object
		(is-a ?class&:(subclassp ?class Ejercicio))
		(diasALaSemana ?j&:(> ?j 0))
		(tipo Actividad | Otro)
		(partOf ?planilla&:(neq ?planilla [nil]))
	)
	?dia <- (object (is-a Dia))
	(not (done ?ejercicio ?dia))
	=>
	(assert (done ?ejercicio ?dia))
	(if (canAsssign ?ejercicio ?dia ?class 10 TRUE)
		then
		(assignPrincipal ?ejercicio ?dia ?*max-duracion* ?*max-calentamientos* TRUE 8)
		(retract ?assignArtritis)
		(assert (aArtritis (- ?i 1)))
	)
)

(defrule assignFibrosis
	(declare (salience 0))
	(new_avi)
	?assignFibrosis <- (aFibrosis ?i&:(> ?i 0))
	?ejercicio <- (object
		(is-a ?class&:(subclassp ?class Ejercicio))
		(diasALaSemana ?j&:(> ?j 0))
		(tipo Actividad | Otro)
		(nombreEjercicio "Carrera" | "Correr" | "Bicicleta" | "Nadar" | "Remo"
		| "SubirEscaleras" | "Abdominales" | "ElevacionPiernas"
		| "ElevacionRodillas" | "ExtensionCadera" | "ExtensionRodillas" | "ExtensionTriceps"
		| "FlexionCadera" | "Flexiones" | "FlexionPlantar" | "FlexionRodillas" | "LevantarseSentarse"
		| "MaquinaEliptica" | "PesaBicepDerecho" | "PesaBicepIzquierdo" | "PesaTricepDerecho"
		| "PesaTricepIzquierdo" | "Sentadillas" | "SentadillasBalon" | "SentadillasMancuernas")
		(partOf ?planilla&:(neq ?planilla [nil]))
	)
	?dia <- (object (is-a Dia))
	(not (done ?ejercicio ?dia))
	=>
	(assert (done ?ejercicio ?dia))
	(if (canAsssign ?ejercicio ?dia ?class 11 TRUE)
		then
		(assignPrincipal ?ejercicio ?dia 60 ?*max-calentamientos* TRUE 11)
		(retract ?assignFibrosis)
		(assert (aFibrosis (- ?i 1)))
	)
)

(defrule assignDepresion
	(declare (salience 0))
	(new_avi)
	?assignDepresion <- (aDepresion ?i&:(> ?i 0))
	?ejercicio <- (object
		(is-a ?class&:(eq ?class Aerobico))
		(diasALaSemana ?j&:(> ?j 0))
		(tipo Actividad)
		(partOf ?planilla&:(neq ?planilla [nil]))
	)
	?dia <- (object (is-a Dia))
	(not (done ?ejercicio ?dia))
	=>
	(assert (done ?ejercicio ?dia))
	(if (canAsssign ?ejercicio ?dia ?class -1 TRUE)
		then
		(assignPrincipal ?ejercicio ?dia ?*max-duracion* ?*max-calentamientos* FALSE 0)
		(retract ?assignDepresion)
		(assert (aDepresion (- ?i 1)))
	)
)

(defrule assignOtros
	(declare (salience -1))
	(new_avi)
	?ejercicio <- (object
		(is-a ?class&:(subclassp ?class Ejercicio))
		(diasALaSemana ?j&:(> ?j 0))
		(tipo Actividad | Otro)
		(partOf ?planilla&:(neq ?planilla [nil]))
	)
	?dia <- (object (is-a Dia))
	(not (done ?ejercicio ?dia))
	=>
	(assert (done ?ejercicio ?dia))
	(if (and (canAsssign ?ejercicio ?dia ?class -1 TRUE) (not (minimum ?*min-dias*)))
		then
		(assignPrincipal ?ejercicio ?dia ?*max-duracion* ?*max-calentamientos* FALSE 0)
	)
)

(defrule finAssigning
	(declare (salience -2))
	=>
	(focus printing)
)


;;;
;;;						PRINTING MODULE
;;;

(defmodule printing
	(import MAIN ?ALL)
	(import ask_questions ?ALL)
	(import inference_of_data ?ALL)
	(import recomendation ?ALL)
	(export ?ALL)
)

(defrule printPlanilla
	(new_avi)
	=>
	(bind ?exercicis (find-all-instances ((?e Ejercicio))
		(neq (send ?e get-partOf) [nil])
	))

	(bind ?dias (find-all-instances ((?d Dia))
		(neq ?d [nil])
	))

	(printout t crlf)

	(printout t "--------------------------------------------------------------" crlf)
	(printout t "------------------- Planilla de ejercicios -------------------" crlf)
	(printout t "--------------------------------------------------------------" crlf)
	(printout t crlf)

	(printout t "EXERCICIS NO ASIGNATS" crlf)
	(foreach ?exe ?exercicis do
		(if (not (is-assigned ?exe ?dias))
			then
			(printout t "   " (send ?exe get-nombreEjercicio) crlf)
		)
	)

	(printout t crlf)

	(foreach ?dia ?dias do

		(printout t (upcase (send ?dia get-nombreDia)))
		(printout t crlf)

		(bind ?calentamientos (send ?dia get-Calentamiento))
		(bind ?principales (send ?dia get-Principal))
		(bind ?recuperaciones (send ?dia get-Recuperacion))

		(printout t "  " "Calentamiento:" crlf)

		(foreach ?calentamiento ?calentamientos do
			(printout t "    " (send ?calentamiento get-nombreEjercicio))
			(printout t " - " (send ?calentamiento get-duracion) " min.")
			(printout t crlf)
		)

		(printout t crlf "  " "Ejercicio:" crlf)

		(foreach ?principal ?principales do
			(printout t "    " (send ?principal get-nombreEjercicio))
			(printout t " - " (send ?principal get-duracion) " min.")
			(printout t crlf)
		)

		(printout t crlf "  " "Recuperacion:" crlf)

		(foreach ?recuperacion ?recuperaciones do
			(printout t "    " (send ?recuperacion get-nombreEjercicio))
			(printout t " - " (send ?recuperacion get-duracion) " min.")
			(printout t crlf)
		)

		(printout t crlf)
	)

	(printout t crlf "FIN" crlf crlf)
	(exit)
)
