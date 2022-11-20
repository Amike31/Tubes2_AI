(deftemplate person
    (slot ID        (type INTEGER)      (default ?NONE))
    (slot HBsAg     (type SYMBOL)   (default NONE) (allowed-symbols NONE pos neg))
    (slot anti_HDV  (type SYMBOL)   (default NONE) (allowed-symbols NONE pos neg))
    (slot anti_HBs  (type SYMBOL)   (default NONE) (allowed-symbols NONE pos neg))
    (slot anti_HBc  (type SYMBOL)   (default NONE) (allowed-symbols NONE pos neg))
    (slot IgM_anti_HBc (type SYMBOL) (default NONE) (allowed-symbols NONE pos neg))
    (slot result    (type SYMBOL)   (default NONE))
)

(deffacts insert-facts
    (person (ID 0))
)

; (defrule initial-rule
;     ?r <- (result ~NONE)
;     =>
;     (retract ?r)
;     (modify ?p (result NONE))
; )

; ROOT
(defrule HBsAg_check
    ?p <- (person (ID ?ID) (HBsAg NONE))
    =>
    (printout t "HBsAg? ")
    (bind ?HBsAg (read))
    (modify ?p (HBsAg ?HBsAg))
)

; BAGIAN KIRI
(defrule anti_HDV_check
    ?p <- (person (ID ?ID) (result NONE) (HBsAg pos) (anti_HDV NONE))
    =>
    (printout t "anti-HDV? ")
    (bind ?anti_HDV (read))
    (modify ?p (anti_HDV ?anti_HDV))
    
    (if (eq ?anti_HDV pos) then
        (modify ?p (result hepatitis_BD))
    )
)

(defrule anti_HBc_check_1
    ?p <- (person (ID ?ID) (result NONE) (anti_HDV neg) (anti_HBc NONE))
    =>
    (printout t "anti-HBc? ")
    (bind ?anti_HBc (read))
    (modify ?p (anti_HBc ?anti_HBc))

    (if (eq ?anti_HBc neg) then
        (modify ?p (result Uncertain_configuration))
    )
)

(defrule anti_HBs_check_1
    ?p <- (person (ID ?ID) (result NONE) (anti_HDV neg) (anti_HBc pos) (anti_HBs NONE))
    ?check <- (anti_HBs NONE)
    =>
    (printout t "anti-HBs? ")
    (bind ?anti_HBs (read))
    (modify ?p (anti_HBs ?anti_HBs))

    (if (eq ?anti_HBs pos) then
        (modify ?p (result Uncertain_configuration))
    )
)

(defrule IgM_anti_HBc_check
    ?p <- (person (ID ?ID) (result NONE) (anti_HDV neg) (anti_HBs neg) (IgM_anti_HBc NONE))
    =>
    (printout t "IgM anti-HBc? ")
    (bind ?IgM_anti_HBc (read))
    (modify ?p (IgM_anti_HBc ?IgM_anti_HBc))

    (if (eq ?IgM_anti_HBc pos) then
        (modify ?p (result Acute_infection))
    )
    (if (eq ?IgM_anti_HBc neg) then
        (modify ?p (result Chronic_infection))
    )
)

; BAGIAN KANAN

(defrule anti_HBs_check_2
    ?p <- (person (ID ?ID) (result NONE) (HBsAg neg) (anti_HBs NONE))
    =>
    (printout t "anti-HBs? ")
    (bind ?anti_HBs (read))
    (modify ?p (anti_HBs ?anti_HBs))
)

(defrule anti_HBc_check_2
    ?p <- (person (ID ?ID) (result NONE) (HBsAg neg) (anti_HBs pos) (anti_HBc NONE))
    =>
    (printout t "anti-HBc? ")
    (bind ?anti_HBc (read))
    (modify ?p (anti_HBc ?anti_HBc))

    (if (eq ?anti_HBc pos) then
        (modify ?p (result Cured))
    )
    (if (eq ?anti_HBc neg) then
        (modify ?p (result Vaccinated))
    )
)

(defrule anti_HBc_check_3
    ?p <- (person (ID ?ID) (result NONE) (HBsAg neg) (anti_HBs neg) (anti_HBc NONE))
    =>
    (printout t "anti-HBc? ")
    (bind ?anti_HBc (read))
    (modify ?p (anti_HBc ?anti_HBc))

    (if (eq ?anti_HBc pos) then
        (modify ?p (result Unclear))
    )
    (if (eq ?anti_HBc neg) then
        (modify ?p (result Healthy))
    )
)


(defrule printResult
    (person (result ?r))
    (not (eq ?r NONE))
    =>
    (if (eq ?r hepatitis_BD)            then (bind ?p "Hepatitis B+D"))
    (if (eq ?r Uncertain_configuration) then (bind ?p "Uncertain configuration"))
    (if (eq ?r Acute_infection)         then (bind ?p "Acute infection"))
    (if (eq ?r Chronic_infection)       then (bind ?p "Chronic infection"))
    (if (eq ?r Cured) then (bind ?p "Cured"))
    (if (eq ?r Vaccinated) then (bind ?p "Vaccinated"))
    (if (eq ?r Unclear) then (bind ?p "Unclear (possible resolved)"))
    (if (eq ?r Healthy) then (bind ?p "Healthy not vaccinated or suspicious"))
    (printout t "Hasil Prediksi = " ?p crlf)
)