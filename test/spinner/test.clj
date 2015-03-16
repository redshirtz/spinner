(ns spinner.test
  (:require [clojure.java.io   :as io])
  (:use spinner.core))

(comment

;Binding test
(binding [spinner-ns 'spinner]
  ; TODO
  ;  Namespace is mappenstructuur?
  (spindir 'spinner (io/as-file "./resources") rand-nth))

(refer 'spinner)


  ; File test
  (binding [spinner/*testen* "1996"]
    (repeatedly 10 #(spinner/testen)))

;  (spinner/a)
  (spinner/blabla)
  (spinner.en/english)

; Map test
(defspin macho (array-map
                "macho"          ["man" "b" "c"]
                "onder-de-plak"  ["mom" "dad" "scheit" "%macho%s"])
  rand-nth)

(spinner.test/macho->onder-de-plak)
(binding [spinner.test/*macho* "man"]
  (spinner.test/macho->onder-de-plak))
(spinner.test/macho)

; Seq test
(defspin f ["last" "omega" "mega stupid"] rand-nth)
(defspin mom ["%f%" "moeder"])
(defspin child ["%mom%"])
(parse-str "hi %mom%")
(spinner.test/f)
(spinner.test/mom)
(spinner.test/child)

;Function test
(defspin fnc (fn [] "functie"))
(spinner.test/fnc)

(parse-str "%mom%")

(@spinners "mom")

);; End comment'
