(load "classes.pont")
(load-instances "instances.pins")
(defglobal ?*min-dias* = 3)
(defglobal ?*max-calentamientos* = 6)
(defglobal ?*max-duracion* = 60)
(load "rules.clp")
(run)
