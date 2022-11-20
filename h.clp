(deffacts initial-facts
    (HBsAg          NONE)
    (anti_HDV       NONE)
    (anti_HBs       NONE)
    (anti_HBc       NONE)
    (IgM_anti_HBc   NONE)
    (result         NONE)
)

; (defrule initial-rule
;     ?r <- (result ~NONE)
;     =>
;     (retract ?r)
;     (assert (result NONE))
; )

; ROOT
(defrule HBsAg_check
    ?check <- (HBsAg NONE)
    =>
    (printout t "HBsAg? ")
    (bind ?HBsAg (read))
    (retract ?check)
    (assert (HBsAg ?HBsAg))
)

; BAGIAN KIRI
(defrule anti_HDV_check
    ?r <- (result NONE)
    (HBsAg pos)
    ?check <- (anti_HDV NONE)
    =>
    (printout t "anti-HDV? ")
    (bind ?anti_HDV (read))
    (retract ?check)
    (assert (anti_HDV ?anti_HDV))
    
    (if (eq ?anti_HDV pos) then
        (retract ?r)
        (assert (result hepatitis_BD))
    )
)

(defrule anti_HBc_check_1
    ?r <- (result NONE)
    ?check <- (anti_HBc NONE)
    (anti_HDV neg)
    =>
    (printout t "anti-HBc? ")
    (bind ?anti_HBc (read))
    (retract ?check)
    (assert (anti_HBc ?anti_HBc))

    (if (eq ?anti_HBc neg) then
        (retract ?r)
        (assert (result Uncertain_configuration))
    )
)

(defrule anti_HBs_check_1
    ?r <- (result NONE)
    ?check <- (anti_HBs NONE)
    (anti_HDV neg)
    (anti_HBc pos)
    =>
    (printout t "anti-HBs? ")
    (bind ?anti_HBs (read))
    (retract ?check)
    (assert (anti_HBs ?anti_HBs))

    (if (eq ?anti_HBs pos) then
        (retract ?r)
        (assert (result Uncertain_configuration))
    )
)

(defrule IgM_anti_HBc_check
    ?r <- (result NONE)
    ?check <- (IgM_anti_HBc NONE)
    (anti_HDV neg)
    (anti_HBs neg)
    =>
    (printout t "IgM anti-HBc? ")
    (bind ?IgM_anti_HBc (read))
    (retract ?check)
    (assert (IgM_anti_HBc ?IgM_anti_HBc))

    (if (eq ?IgM_anti_HBc pos) then
        (retract ?r)
        (assert (result Acute_infection))
    )
    (if (eq ?IgM_anti_HBc neg) then
        (retract ?r)
        (assert (result Chronic_infection))
    )
)

; BAGIAN KANAN

(defrule anti_HBs_check_2
    ?check <- (anti_HBs NONE)
    (HBsAg neg)
    =>
    (printout t "anti-HBs? ")
    (bind ?anti_HBs (read))
    (retract ?check)
    (assert (anti_HBs ?anti_HBs))
)

(defrule anti_HBc_check_2
    ?r <- (result NONE)
    ?check <- (anti_HBc NONE)
    (HBsAg neg)
    (anti_HBs pos)
    =>
    (printout t "anti-HBc? ")
    (bind ?anti_HBc (read))
    (retract ?check)
    (assert (anti_HBc ?anti_HBc))

    (if (eq ?anti_HBc pos) then
        (retract ?r)
        (assert (result Cured))
    )
    (if (eq ?anti_HBc neg) then
        (retract ?r)
        (assert (result Vaccinated))
    )
)

(defrule anti_HBc_check_3
    ?r <- (result NONE)
    ?check <- (anti_HBc NONE)
    (HBsAg neg)
    (anti_HBs neg)
    =>
    (printout t "anti-HBc? ")
    (bind ?anti_HBc (read))
    (retract ?check)
    (assert (anti_HBc ?anti_HBc))

    (if (eq ?anti_HBc pos) then
        (retract ?r)
        (assert (result Unclear))
    )
    (if (eq ?anti_HBc neg) then
        (retract ?r)
        (assert (result Healthy))
    )
)


(defrule printResult
    (result ?r)
    (not (result NONE))
    =>
    ; (if (eq ?r NONE) then (bind ?p ""))
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