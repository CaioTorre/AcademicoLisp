(SETQ BD1 'NIL)
(SETQ BD1 (MATRICULAR '("Jo�o Paulo" "Ana Maria") '("APC" "C�lculo I") BD1 ))
(SETQ BD1 (VINCULAR '("Daniele") '("APC" "Linguagens Formais") BD1 ))
(SETQ BD1 (VINCULAR '("Ot�vio") '("C�lculo I") BD1 ))
(SETQ BD1 (VINCULAR-DISC-CURSO "APC" 'FECOMP BD1 ))
(SETQ BD1 (MATRICULAR '("Henrique" "Ana Maria") '("Paradigmas" "CLOC") BD1 ))
(SETQ BD1 (VINCULAR '("Pannain") '("CLOC") BD1 ))
(SETQ BD1 (VINCULAR '("Daniele") '("Paradigmas") BD1 ))
(SETQ BD1 (VINCULAR-DISC-CURSO "C�lculo I" 'FEA BD1 ))
(SETQ BD1 (VINCULAR-DISC-CURSO "CLOC" 'FEL BD1 ))
(SETQ BD1 (VINCULAR-DISC-CURSO "Linguagens Formais" 'FECOMP BD1 ))
(SETQ BD1 (VINCULAR-DISC-CURSO "Paradigmas" 'FAS BD1 ))