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
			;(if (null (cadr lista))
			;	nil
			;	(cons (cadr lista) (copiaexcluindo (cddr lista) nome))
			;)
			(CopiaExcluindo (cdr lista) nome)
		)
	)
)

(defun CopiaExcluindoMulti (ListaOriginal ListaASerRemovida)
	(if (null ListaOriginal)
		nil
		(if (not (Existe? ListaASerRemovida (car ListaOriginal)))
			(cons (car ListaOriginal) (CopiaExcluindoMulti (cdr ListaOriginal) ListaASerRemovida))
			(CopiaExcluindoMulti (cdr ListaOriginal) ListaASerRemovida)
		)
	)
)

;(defun Existe? (lista nome)
;	(if (eql (car lista) nome)
;		t
;		(if (null lista)
;			nil
;			(existe? (cdr lista) nome)
;		)
;	)
;)

(defun Existe? (lista valor)
	(if (null lista)
		nil
		(if (eql (car lista) valor)
			t
			(Existe? (cdr lista) valor)
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

(defun Matricular (alunos disciplinas bd)
	(cons
		(cons 
			(cons
				(cons
					(cons
						nil
						(EffVincula
							bd
							'(nil)
							disciplinas
							(PegaListaProfs bd)
							(PegaListaProfs bd)
							;(cdaar db)
							;(cdaar db)
							(VerificaEAdiciona (PegaListaDiscs bd) disciplinas (PegaListaDiscs bd))
						)
						;(cdr (caaaar db))
					)
					(EffMatricula
						bd
						alunos
						disciplinas 
						(VerificaEAdiciona (PegaListaAlunos bd) alunos (PegaListaAlunos bd)) 
						(VerificaEAdiciona (PegaListaAlunos bd) alunos (PegaListaAlunos bd)) 
						(VerificaEAdiciona (PegaListaDiscs bd) disciplinas (PegaListaDiscs bd))
					)
				)
				(PegaListaProfs bd)
				;(cdaar db)
			)
			(VerificaEAdiciona (PegaListaDiscs bd) disciplinas (PegaListaDiscs bd))
		)
		(VerificaEAdiciona (PegaListaAlunos bd) alunos (PegaListaAlunos bd))
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

(defun Vincular (professores disciplinas bd)
	(cons
		(cons 
			(cons
				(cons
					(cons
						nil
						;(cdr (caaaar db))
						(EffVincula
							bd
							professores 
							disciplinas 
							(VerificaEAdiciona (PegaListaProfs bd) professores (PegaListaProfs bd)) 
							(VerificaEAdiciona (PegaListaProfs bd) professores (PegaListaProfs bd)) 
							(VerificaEAdiciona (PegaListaDiscs bd) disciplinas (PegaListaDiscs bd))
						)
					)
					(EffMatricula
						bd 
						'(nil) 
						disciplinas 
						;(cdr db) 
						(PegaListaAlunos bd)
						;(cdr db) 
						(PegaListaAlunos bd)
						(VerificaEAdiciona (PegaListaDiscs bd) disciplinas (PegaListaDiscs bd))
					)
				)
				(VerificaEAdiciona (PegaListaProfs bd) professores (PegaListaProfs bd))
			)
			(VerificaEAdiciona (PegaListaDiscs bd) disciplinas (PegaListaDiscs bd))
		)
	(PegaListaAlunos bd)
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

(defun Alunos? (bd)
	(cdr bd)
)

(defun Professores? (bd)
	(cdaar bd)
)

(defun Disciplinas? (bd)
	(cdar bd)
)

(defun Matriculados? (disciplina bd)
	(StepMatriculados bd (PegaListaAlunos bd) disciplina)
)

(defun StepMatriculados (db alunos disciplina)
	(if (null alunos)
		nil
		(if (eq (AcessaNoGrafoAD db (car alunos) disciplina) 1)
			(cons 
				(car alunos)
				(StepMatriculados db (cdr alunos) disciplina)
			)
			(StepMatriculados db (cdr alunos) disciplina) 
		)
	)
)

(defun Vinculados? (disciplina bd)
	(StepVinculados bd (PegaListaProfs bd) disciplina)
)

(defun StepVinculados (db professores disciplina)
	(if (null professores)
		nil
		(if (eq (AcessaNoGrafoPD db (car professores) disciplina) 1)
			(cons
				(car professores)
				(StepVinculados db (cdr professores) disciplina)
			)
			(StepVinculados db (cdr professores) disciplina)
		)
	)
)

(defun Cursa? (aluno bd)
	(StepCursa bd aluno (PegaListaDiscs bd))
)

(defun StepCursa (db aluno disciplinas)
	(if (null disciplinas)
		nil
		(if (eq (AcessaNoGrafoAD db aluno (car disciplinas)) 1)
			(cons 
				(car disciplinas)
				(StepCursa db aluno (cdr disciplinas))
			)
			(StepCursa db aluno (cdr disciplinas))
		)
	)
)

(defun Ministra? (professor bd)
	(StepMinistra bd professor (PegaListaDiscs bd))
)

(defun StepMinistra (db professor disciplinas)
	(if (null disciplinas)
		nil
		(if (eq (AcessaNoGrafoPD db professor (car disciplinas)) 1)
			(cons
				(car disciplinas)
				(StepMinistra db professor (cdr disciplinas))
			)
			(StepMinistra db professor (cdr disciplinas))
		)
	)
)

(defun PegaListaAlunos (DB) (cdr DB))
(defun PegaListaDiscs (DB) (cdr (car DB)))
(defun PegaListaProfs (DB) (cdr (caar DB)))
(defun PegaGrafoAD (DB) (cdr (caaar DB)))
(defun PegaGrafoPD (DB) (cdr (caaaar DB)))
