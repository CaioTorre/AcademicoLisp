;(setq db 
;(cons 
;	(cons 
;		(cons 
;			(cons 
;				(cons 
;					nil 
;					(cons 
;						(cons
;							(cons
;								(cons
;									nil
;									'(0 0 0 0)
;								)
;								'(0 0 0 0)
;							)
;							'(0 0 0 0)
;						)
;						'(0 0 0 0)
;					)
;				)
;				(cons
;					(cons
;						(cons
;							(cons
;								nil
;								'(0 0 0 0)
;							)
;							'(0 0 0 0)
;						)
;						'(0 0 0 0)
;					)
;					'(0 0 0 0)
;				)
;			)
;			'(p1 p2 p3 p4)
;		)
;		'(d1 d2 d3 d4)
;	)
;	'(a1 a2 a3 a4)
;)
;)

(setq db nil)

(defun TentaAdicionar (a b)
	(if (or (null a) (null b))
		nil
		(+ a b)
	)
) 	

(defun EncontraAluno (lista nome)
	(if (null lista)
		nil
		(if (eql (car lista) nome)
			0
			(TentaAdicionar 1 (EncontraAluno (cdr lista) nome))
		)
	)
)

(defun EncontraDisc (lista nome)
	(if (null lista)
		nil
		(if (eql (car lista) nome)
			0
			(TentaAdicionar 1 (EncontraDisc (cdr lista) nome))
		)
	)
)

(defun EncontraProf (lista nome)
	(if (null lista)
		nil
		(if (eql (car lista) nome)
			0
			(TentaAdicionar 1 (EncontraProf (cdr lista) nome))
		)
	)
)

(defun FazNVezes (func lista n)
	(if (= n 0)
		lista
		(FazNVezes func (funcall func lista) (- n 1))
	)
)

(defun CopiaExcluindo (lista nome)
	(if (null lista)
		nil
		(if (not (eql (car lista) nome))
			(cons (car lista) (copiaexcluindo (cdr lista) nome))
			(if (null (cadr lista))
				nil
				(cons (cadr lista) (copiaexcluindo (cddr lista) nome))
			)
		)
	)
)

(defun Existe? (lista nome)
	(if (eql (car lista) nome)
		t
		(if (null lista)
			nil
			(existe? (cdr lista) nome)
		)
	)
)

(defun AdicionaNoFinal (lista nome)
	(if (null lista)
		(cons nome (cons nil nil))
		(cons (car lista) (AdicionaNoFinal (cdr lista) nome))
	)
)

(defun AcessaNoGrafoAD (db linha coluna)
	(car
		(FazNVezes 
			(function cdr)
			(cdr
				(FazNVezes 
					(function car) 
					;(cdaaar db)
					(PegaGrafoAD db)
					(EncontraAluno (PegaListaAlunos db) linha)
				)
			)
			(EncontraDisc (PegaListaDiscs db) coluna)
		)
	)
)

(defun AcessaNoGrafoPD (db linha coluna)
	(car
		(FazNVezes 
			(function cdr)
			(cdr
				(FazNVezes 
					(function car) 
					;(cdaaar db)
					(PegaGrafoPD db)
					(EncontraProf (PegaListaProfs db) linha)
				)
			)
			(EncontraDisc (PegaListaDiscs db) coluna)
		)
	)
)

(defun Matricula (db alunos disciplinas)
	(cons
		(cons 
			(cons
				(cons
					(cons
						nil
						(EffVincula
							db
							'(nil)
							disciplinas
							(PegaListaProfs db)
							(PegaListaProfs db)
							;(cdaar db)
							;(cdaar db)
							(VerificaEAdiciona (PegaListaDiscs db) disciplinas (PegaListaDiscs db))
						)
						;(cdr (caaaar db))
					)
					(EffMatricula
						db 
						alunos
						disciplinas 
						(VerificaEAdiciona (PegaListaAlunos db) alunos (PegaListaAlunos db)) 
						(VerificaEAdiciona (PegaListaAlunos db) alunos (PegaListaAlunos db)) 
						(VerificaEAdiciona (PegaListaDiscs db) disciplinas (PegaListaDiscs db))
					)
				)
				(PegaListaProfs db)
				;(cdaar db)
			)
			(VerificaEAdiciona (PegaListaDiscs db) disciplinas (PegaListaDiscs db))
		)
		(VerificaEAdiciona (PegaListaAlunos db) alunos (PegaListaAlunos db))
	)
)

(defun MatriculaAD (db alunoatual alunos disciplinas iddisciplinas)
	(if (null iddisciplinas)
		nil
		(if (and (existe? disciplinas (car iddisciplinas)) (existe? alunos alunoatual))
			(cons 
				1 
				(MatriculaAD db alunoatual alunos disciplinas (cdr iddisciplinas))
			)
			(if (and (existe? (PegaListaAlunos db) alunoatual) (existe? (PegaListaDiscs db) (car iddisciplinas)))
				(cons
					(AcessaNoGrafoAD db alunoatual (car iddisciplinas))
					;0
					(MatriculaAD db alunoatual alunos disciplinas (cdr iddisciplinas))
				)
				(cons
					;(AcessaNoGrafoAD db alunoatual (car iddisciplinas))
					0
					(MatriculaAD db alunoatual alunos disciplinas (cdr iddisciplinas))
				)
			)
		)
	)
)

(defun EffMatricula (db alunos disciplinas idalunos idalunoscopia iddisciplinas)
	(if (null idalunos)
		nil
		;(cons
		;	nil 
		;	(MatriculaAD db alunos idalunoscopia disciplinas 
		;	iddisciplinas)
		;)
		(cons
			(EffMatricula db alunos disciplinas (cdr idalunos) idalunoscopia iddisciplinas)
			(MatriculaAD db (car idalunos) alunos disciplinas iddisciplinas)
		)
	)
)

(defun Vincula (db professores disciplinas)
	(cons
		(cons 
			(cons
				(cons
					(cons
						nil
						;(cdr (caaaar db))
						(EffVincula
							db 
							professores 
							disciplinas 
							(VerificaEAdiciona (PegaListaProfs db) professores (PegaListaProfs db)) 
							(VerificaEAdiciona (PegaListaProfs db) professores (PegaListaProfs db)) 
							(VerificaEAdiciona (PegaListaDiscs db) disciplinas (PegaListaDiscs db))
						)
					)
					(EffMatricula
						db 
						'(nil) 
						disciplinas 
						;(cdr db) 
						(PegaListaAlunos db)
						;(cdr db) 
						(PegaListaAlunos db)
						(VerificaEAdiciona (PegaListaDiscs db) disciplinas (PegaListaDiscs db))
					)
				)
				(VerificaEAdiciona (PegaListaProfs db) professores (PegaListaProfs db))
			)
			(VerificaEAdiciona (PegaListaDiscs db) disciplinas (PegaListaDiscs db))
		)
	(PegaListaAlunos db)
	)
)

(defun VinculaAD (db profatual professores disciplinas iddisciplinas)
	(if (null iddisciplinas)
		nil
		(if (and (existe? disciplinas (car iddisciplinas)) (existe? professores profatual))
			(cons 
				1 
				(VinculaAD db profatual professores disciplinas (cdr iddisciplinas))
			)
			(if (and (existe? (PegaListaProfs db) profatual) (existe? (PegaListaDiscs db) (car iddisciplinas)))
				(cons
					(AcessaNoGrafoPD db profatual (car iddisciplinas))
					;0
					(VinculaAD db profatual professores disciplinas (cdr iddisciplinas))
				)
				(cons
					;(AcessaNoGrafoAD db alunoatual (car iddisciplinas))
					0
					(VinculaAD db profatual professores disciplinas (cdr iddisciplinas))
				)
			)
		)
	)
)

(defun EffVincula (db professores disciplinas idprofs idprofscopia iddisciplinas)
	(if (null idprofs)
		nil
		;(cons
		;	nil 
		;	(MatriculaAD db alunos idalunoscopia disciplinas 
		;	iddisciplinas)
		;)
		(cons
			(EffVincula db professores disciplinas (cdr idprofs) idprofscopia iddisciplinas)
			(VinculaAD db (car idprofs) professores disciplinas iddisciplinas)
		)
	)
)

(defun VerificaEAdiciona (lista nomes antiga)
	(if (not (null lista))
		(cons (car lista) (VerificaEAdiciona (cdr lista) nomes antiga))
		(if (null (car nomes))
			;(cons nil nil)
			nil
			(if (not (existe? antiga (car nomes)))
				(cons (car nomes) (VerificaEAdiciona lista (cdr nomes) antiga))
				(VerificaEAdiciona lista (cdr nomes) antiga)
			)
		)
	)
)

(defun PegaListaAlunos (DB) (cdr DB))
(defun PegaListaDiscs (DB) (cdr (car DB)))
(defun PegaListaProfs (DB) (cdr (caar DB)))
(defun PegaGrafoAD (DB) (cdr (caaar DB)))
(defun PegaGrafoPD (DB) (cdr (caaaar DB)))
