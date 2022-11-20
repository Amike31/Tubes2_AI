;;;======================================================
;;;   Tugas Besar 2 IF3170
;;;   Prediksi Penyakit Hepatitis Menggunakan CLIPS
;;;======================================================

;;;*****************************
;;;* VALIDATION INPUT FUNCTION *
;;;*****************************

(deffunction ask_question (?question)
   (bind $?allowed (create$ pos neg))
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
         then (bind ?answer (lowcase ?answer)))
   (while (not (member$ ?answer ?allowed)) do
      (printout t ?question)
      (bind ?answer (read)))
      (if (lexemep ?answer) 
         then (bind ?answer (lowcase ?answer)))
   ?answer)

(deffunction get_value (?question)
   (bind ?response (ask_question ?question))
   ?response)

;;;************************
;;;* TEMPLATE FOR PATIENT *
;;;************************

(deftemplate patient
    (slot ID            (type INTEGER)  (default ?NONE))
    (slot HBsAg         (type SYMBOL)   (default NONE) (allowed-symbols NONE pos neg))
    (slot anti_HDV      (type SYMBOL)   (default NONE) (allowed-symbols NONE pos neg))
    (slot anti_HBs      (type SYMBOL)   (default NONE) (allowed-symbols NONE pos neg))
    (slot anti_HBc      (type SYMBOL)   (default NONE) (allowed-symbols NONE pos neg))
    (slot IgM_anti_HBc  (type SYMBOL)   (default NONE) (allowed-symbols NONE pos neg))
    (slot result        (type SYMBOL)   (default NONE))
)

(deffacts insert-facts
    (patient (ID 0))
)

;;;************************************
;;;* PATIENT CONDITION CHECKING RULES *
;;;************************************

; ROOT
(defrule HBsAg_check
    ?p <- (patient (ID ?ID) (HBsAg NONE))
    =>
    (modify ?p (HBsAg (get_value "HBsAg? ")))
)

; LEFT SIDE
(defrule anti_HDV_check
    ?p <- (patient (ID ?ID) (result NONE) (HBsAg pos) (anti_HDV NONE))
    =>
    (bind ?anti_HDV (get_value "anti-HDV? "))
    (modify ?p (anti_HDV ?anti_HDV))
    
    (if (eq ?anti_HDV pos) then
        (modify ?p (result hepatitis_BD))
    )
)

(defrule anti_HBc_check_1
    ?p <- (patient (ID ?ID) (result NONE) (anti_HDV neg) (anti_HBc NONE))
    =>
    (bind ?anti_HBc (get_value "anti-HBc? "))
    (modify ?p (anti_HBc ?anti_HBc))

    (if (eq ?anti_HBc neg) then
        (modify ?p (result Uncertain_configuration))
    )
)

(defrule anti_HBs_check_1
    ?p <- (patient (ID ?ID) (result NONE) (anti_HDV neg) (anti_HBc pos) (anti_HBs NONE))
    =>
    (bind ?anti_HBs (get_value "anti-HBs? "))
    (modify ?p (anti_HBs ?anti_HBs))

    (if (eq ?anti_HBs pos) then
        (modify ?p (result Uncertain_configuration))
    )
)

(defrule IgM_anti_HBc_check
    ?p <- (patient (ID ?ID) (result NONE) (anti_HDV neg) (anti_HBs neg) (IgM_anti_HBc NONE))
    =>
    (bind ?IgM_anti_HBc (get_value "IgM anti-HBc? "))
    (modify ?p (IgM_anti_HBc ?IgM_anti_HBc))

    (if (eq ?IgM_anti_HBc pos) then
        (modify ?p (result Acute_infection))
    )
    (if (eq ?IgM_anti_HBc neg) then
        (modify ?p (result Chronic_infection))
    )
)

; RIGHT SIDE

(defrule anti_HBs_check_2
    ?p <- (patient (ID ?ID) (result NONE) (HBsAg neg) (anti_HBs NONE))
    =>
    (bind ?anti_HBs (get_value "anti-HBs? "))
    (modify ?p (anti_HBs ?anti_HBs))
)

(defrule anti_HBc_check_2
    ?p <- (patient (ID ?ID) (result NONE) (HBsAg neg) (anti_HBs pos) (anti_HBc NONE))
    =>
    (bind ?anti_HBc (get_value "anti-HBc? "))
    (modify ?p (anti_HBc ?anti_HBc))

    (if (eq ?anti_HBc pos) then
        (modify ?p (result Cured))
    )
    (if (eq ?anti_HBc neg) then
        (modify ?p (result Vaccinated))
    )
)

(defrule anti_HBc_check_3
    ?p <- (patient (ID ?ID) (result NONE) (HBsAg neg) (anti_HBs neg) (anti_HBc NONE))
    =>
    (bind ?anti_HBc (get_value "anti-HBc? "))
    (modify ?p (anti_HBc ?anti_HBc))

    (if (eq ?anti_HBc pos) then
        (modify ?p (result Unclear))
    )
    (if (eq ?anti_HBc neg) then
        (modify ?p (result Healthy))
    )
)

;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************

(defrule system-banner 
    (declare (salience 10))
    =>
    (printout t "=====================================================" crlf)
    (printout t "+   Prediksi Penyakit Hepatitis Menggunakan CLIPS   +" crlf)
    (printout t "=====================================================" crlf))

(defrule printResult
    (patient (ID ?id) (result ?r))
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
    (printout t "Hasil Prediksi Pasien ke-" ?id " = " ?p crlf)
)