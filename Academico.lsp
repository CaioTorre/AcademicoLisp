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
		(if (equal (car lista) nome)
			0
			(TentaAdicionar 1 (EncontraAluno (cdr lista) nome))
		)
	)
)

(defun EncontraDisc (lista nome)
	(if (null lista)
		nil
		(if (equal (car lista) nome)
			0
			(TentaAdicionar 1 (EncontraDisc (cdr lista) nome))
		)
	)
)

(defun EncontraProf (lista nome)
	(if (null lista)
		nil
		(if (equal (car lista) nome)
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
		(if (not (equal (car lista) nome))
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
		(if (equal (car lista) valor)
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

(defun AdicionaListaCursos (listacursos disciplinasantigas disciplinasnovas n)
	(if (null disciplinasnovas)
		nil
		(if (Existe? disciplinasantigas (car disciplinasnovas))
			(cons (car (FazNVezes (function cdr) listacursos n)) (AdicionaListaCursos listacursos disciplinasantigas (cdr disciplinasnovas) (1+ n)))
			(cons nil (AdicionaListaCursos listacursos disciplinasantigas (cdr disciplinasnovas) (1+ n)))
		)
	)
)

(defun Vincular-Disc-Curso (disciplina curso bd)
	(cons
		(cons 
			(cons
				(cons
					(cons
						(cons 
							nil
							(StepVinculaDiscCurso (PegaListaCursos bd) (PegaListaDiscs bd) disciplina curso 0)
						)
						(PegaGrafoPD bd)
					)
					(PegaGrafoAD bd)
				)
				(PegaListaProfs bd)
			)
			(PegaListaDiscs bd)
		)
		(PegaListaAlunos bd)
	)
)

(defun StepVinculaDiscCurso (listacursos listadisciplinas disciplina curso n)
	(if (null listadisciplinas)
		nil
		(if (equal (car listadisciplinas) disciplina)
			(cons curso (StepVinculaDiscCurso listacursos (cdr listadisciplinas) disciplina curso (1+ n)))
			(cons (car (FazNVezes (function cdr) listacursos n)) (StepVinculaDiscCurso listacursos (cdr listadisciplinas) disciplina curso (1+ n)))
		)
	)
)

(defun Disc-Curso? (disciplina bd)
	(StepDiscCurso (PegaListaDiscs bd) disciplina (PegaListaCursos bd))
)

(defun StepDiscCurso (listadisciplinas disciplina listacursos)
	(if (null listadisciplinas)
		nil
		(if (equal (car listadisciplinas) disciplina)
			(car listacursos)
			(StepDiscCurso (cdr listadisciplinas) disciplina (cdr listacursos))
		)
	)
)

(defun Matricular (alunos disciplinas bd)
	(cons
		(cons 
			(cons
				(cons
					(cons
						(cons 
							nil
							(AdicionaListaCursos (PegaListaCursos bd) (PegaListaDiscs bd) (VerificaEAdiciona (PegaListaDiscs bd) disciplinas (PegaListaDiscs bd)) 0) 
						)
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
						(cons 
							nil
							(AdicionaListaCursos (PegaListaCursos bd) (PegaListaDiscs bd) (VerificaEAdiciona (PegaListaDiscs bd) disciplinas (PegaListaDiscs bd)) 0) 
						)
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

(defun Cancelar-Matricula (alunos disciplinas bd)
	(Desmatricula bd alunos disciplinas (PegaPrimerAlunos bd alunos disciplinas))
)

(defun RemoveListaCursos (listacursos listadiscsantiga listadiscsvazias)
	(if (null listadiscsantiga)
		nil
		(if (Existe? listadiscsvazias (car listadiscsantiga))
			(RemoveListaCursos (cdr listacursos) (cdr listadiscsantiga) listadiscsvazias)
			(cons (car listacursos) (RemoveListaCursos (cdr listacursos) (cdr listadiscsantiga) listadiscsvazias))
		)
	)
)

(defun Desmatricula (bd alunos disciplinas primedgrafo)
	(cons
		(cons 
			(cons
				(cons
					(cons
						;nil
						(cons 
							nil
							(RemoveListaCursos (PegaListaCursos bd) (PegaListaDiscs bd) (EncontraDiscsVazias primedgrafo (PegaGrafoPD bd) (PegaListaDiscs bd)))
							;(AdicionaListaCursos (PegaListaCursos bd) (PegaListaDiscs bd) (VerificaEAdiciona (PegaListaDiscs bd) disciplinas (PegaListaDiscs bd)) 0) 
						)
						;(PegaGrafoPD bd)
						(EffDesvincula
							bd
							'(nil)
							disciplinas
							(PegaListaProfs bd)
							(PegaListaDiscs bd)
							(EncontraProfsVazios (PegaGrafoPD bd) (PegaListaProfs bd))
							(EncontraDiscsVazias primedgrafo (PegaGrafoPD bd) (PegaListaDiscs bd))
						)
						;(cdr (caaaar db))
					)
					(EffDesmatricula
						bd
						alunos
						disciplinas
						(PegaListaAlunos bd)
						(PegaListaDiscs bd)
						(EncontraAlunosVazios primedgrafo (PegaListaAlunos bd))
						(EncontraDiscsVazias  primedgrafo (PegaGrafoPD bd) (PegaListaDiscs bd))
					)
				)
				(PegaListaProfs bd)
				;(cdaar db)
			)
			(CopiaExcluindoMulti (PegaListaDiscs bd) (EncontraDiscsVazias primedgrafo (PegaGrafoPD bd) (PegaListaDiscs bd)))
		)
		(CopiaExcluindoMulti (PegaListaAlunos bd) (EncontraAlunosVazios primedgrafo (PegaListaAlunos bd)))
	)
)

(defun PegaPrimerAlunos (db alunos disciplinas)
	(DesmatriculaPrimer db alunos disciplinas (PegaListaAlunos db) (PegaListaAlunos db) (PegaListaDiscs db))
)

(defun DesmatriculaPrimer (db alunos disciplinas idalunos idalunoscopia iddisciplinas)
	(if (null idalunos)
		nil
		(cons
			(DesmatriculaPrimer db alunos disciplinas (cdr idalunos) idalunoscopia iddisciplinas)
			(DesmatriculaPrimerLinha db (car idalunos) alunos disciplinas iddisciplinas)
		)
	)
)

(defun DesmatriculaPrimerLinha (db alunoatual alunos disciplinas iddisciplinas)
	(if (null iddisciplinas)
		nil
		(if (and (existe? disciplinas (car iddisciplinas)) (existe? alunos alunoatual))
			(cons 
				0
				(DesmatriculaPrimerLinha db alunoatual alunos disciplinas (cdr iddisciplinas))
			)
			(cons
				(AcessaNoGrafoAD db alunoatual (car iddisciplinas))
					;0
				(DesmatriculaPrimerLinha db alunoatual alunos disciplinas (cdr iddisciplinas))
			)
		)
	)
)

(defun EffDesmatricula (db alunos disciplinas alunosantigos disciplinasantigas alunosexcluidos disciplinasexcluidas)
	(if (null alunosantigos)
		nil
		(if (not (Existe? alunosexcluidos (car alunosantigos)))
			(cons
				(EffDesmatricula db alunos disciplinas (cdr alunosantigos) disciplinasantigas alunosexcluidos disciplinasexcluidas)
				(DesmatriculaAD db (car alunosantigos) alunos disciplinas disciplinasantigas disciplinasexcluidas)
			)
			(EffDesmatricula db alunos disciplinas (cdr alunosantigos) disciplinasantigas alunosexcluidos disciplinasexcluidas)
		)
	)
)

(defun DesmatriculaAD (db alunoatual alunos disciplinas disciplinasantigas disciplinasexcluidas)
	(if (null disciplinasantigas)
		nil
		(if (not (Existe? disciplinasexcluidas (car disciplinasantigas)))
			(if (and (Existe? disciplinas (car disciplinasantigas)) (Existe? alunos alunoatual))
				(cons
					0
					(DesmatriculaAD db alunoatual alunos disciplinas (cdr disciplinasantigas) disciplinasexcluidas)
				)
				(cons
					(AcessaNoGrafoAD db alunoatual (car disciplinasantigas))
					(DesmatriculaAD db alunoatual alunos disciplinas (cdr disciplinasantigas) disciplinasexcluidas)
				)
				
			)
			(DesmatriculaAD db alunoatual alunos disciplinas (cdr disciplinasantigas) disciplinasexcluidas)
		)
	)
)

(defun EncontraAlunosVazios (db alunos)
	(StepEncontraAlunosVazios db alunos 0)
)

(defun StepEncontraAlunosVazios (grafo alunos n)
	(if (null alunos)
		nil
		(if (ChecaLinhaVazia (cdr (FazNVezes (function car) grafo n)))
			(cons (car alunos) (StepEncontraAlunosVazios grafo (cdr alunos) (1+ n)))
			(StepEncontraAlunosVazios grafo (cdr alunos) (1+ n))
		)
	)
)

(defun EncontraDiscsVazias (grafoalunos grafoprofs disciplinas)
	(StepEncontraDiscsVazias grafoalunos grafoprofs disciplinas 0)
)

(defun StepEncontraDiscsVazias (grafoalunos grafoprofs disciplinas n)
	(if (null disciplinas)
		nil
		(if (and (ChecaColunaVazia n grafoalunos) (ChecaColunaVazia n grafoprofs))
			(cons (car disciplinas) (StepEncontraDiscsVazias grafoalunos grafoprofs (cdr disciplinas) (1+ n)))
			(StepEncontraDiscsVazias grafoalunos grafoprofs (cdr disciplinas) (1+ n))
		)
	)
)
;---------------------------------------------------
(defun Remover-Vinculo (professores disciplinas bd)
	(Desvincula bd professores disciplinas (PegaPrimerProfs bd professores disciplinas))
)

(defun Desvincula (bd profs disciplinas primedgrafo)
	(cons
		(cons 
			(cons
				(cons
					(cons
						;nil
						(cons 
							nil
							(RemoveListaCursos (PegaListaCursos bd) (PegaListaDiscs bd) (EncontraDiscsVazias (PegaGrafoAD bd) primedgrafo (PegaListaDiscs bd)))
							;(AdicionaListaCursos (PegaListaCursos bd) (PegaListaDiscs bd) (VerificaEAdiciona (PegaListaDiscs bd) disciplinas (PegaListaDiscs bd)) 0) 
						)
						(EffDesvincula
							bd
							profs
							disciplinas
							(PegaListaProfs bd)
							(PegaListaDiscs bd)
							(EncontraProfsVazios primedgrafo (PegaListaProfs bd))
							(EncontraDiscsVazias (PegaGrafoAD bd) primedgrafo (PegaListaDiscs bd));;;;;;;;;;gabi passou por aqui PegaGrafoAD to PegaGrafoPD
						)
						;(cdr (caaaar db))
					)
					;(PegaGrafoAD bd)
					(EffDesmatricula
							bd
							'(nil)
							disciplinas
							(PegaListaAlunos bd)
							(PegaListaDiscs bd)
							(EncontraAlunosVazios (PegaGrafoAD bd) (PegaListaAlunos bd))
							(EncontraDiscsVazias (PegaGrafoAD bd) primedgrafo (PegaListaDiscs bd))
					)
				)
				;(PegaListaProfs bd)
				(CopiaExcluindoMulti (PegaListaProfs bd) (EncontraProfsVazios primedgrafo (PegaListaProfs bd)))
				;(cdaar db)
			)
			(CopiaExcluindoMulti (PegaListaDiscs bd) (EncontraDiscsVazias (PegaGrafoAD bd) primedgrafo (PegaListaDiscs bd)))
		)
		(PegaListaAlunos bd)
		;(CopiaExcluindoMulti (PegaListaAlunos bd) (EncontraAlunosVazios primedgrafo (PegaListaAlunos bd)))
	)
)

(defun PegaPrimerProfs (db profs disciplinas)
	(DesvinculaPrimer db profs disciplinas (PegaListaProfs db) (PegaListaProfs db) (PegaListaDiscs db))
)

(defun DesvinculaPrimer (db profs disciplinas idprofs idprofscopia iddisciplinas)
	(if (null idprofs)
		nil
		(cons
			(DesvinculaPrimer db profs disciplinas (cdr idprofs) idprofscopia iddisciplinas)
			(DesvinculaPrimerLinha db (car idprofs) profs disciplinas iddisciplinas)
		)
	)
)

(defun DesvinculaPrimerLinha (db profatual profs disciplinas iddisciplinas)
	(if (null iddisciplinas)
		nil
		(if (and (existe? disciplinas (car iddisciplinas)) (existe? profs profatual))
			(cons 
				0
				(DesvinculaPrimerLinha db profatual profs disciplinas (cdr iddisciplinas))
			)
			(cons
				(AcessaNoGrafoPD db profatual (car iddisciplinas))
					;0
				(DesvinculaPrimerLinha db profatual profs disciplinas (cdr iddisciplinas))
			)
		)
	)
)

(defun EffDesvincula (db profs disciplinas profsantigos disciplinasantigas profsexcluidos disciplinasexcluidas)
	(if (null profsantigos)
		nil
		(if (not (Existe? profsexcluidos (car profsantigos)))
			(cons
				(EffDesvincula db profs disciplinas (cdr profsantigos) disciplinasantigas profsexcluidos disciplinasexcluidas)
				(DesvinculaAD db (car profsantigos) profs disciplinas disciplinasantigas disciplinasexcluidas)
			)
			(EffDesmatricula db profs disciplinas (cdr profsantigos) disciplinasantigas profsexcluidos disciplinasexcluidas)
		)
	)
)

(defun DesvinculaAD (db profatual profs disciplinas disciplinasantigas disciplinasexcluidas)
	(if (null disciplinasantigas)
		nil
		(if (not (Existe? disciplinasexcluidas (car disciplinasantigas)))
			(if (and (Existe? disciplinas (car disciplinasantigas)) (Existe? profs profatual))
				(cons
					0
					(DesvinculaAD db profatual profs disciplinas (cdr disciplinasantigas) disciplinasexcluidas)
				)
				(cons
					(AcessaNoGrafoPD db profatual (car disciplinasantigas))
					(DesvinculaAD db profatual profs disciplinas (cdr disciplinasantigas) disciplinasexcluidas)
				)
				
			)
			(DesvinculaAD db profatual profs disciplinas (cdr disciplinasantigas) disciplinasexcluidas)
		)
	)
)

(defun EncontraProfsVazios (db profs)
	(StepEncontraProfsVazios db profs 0)
)

(defun StepEncontraProfsVazios (grafo profs n)
	(if (null profs)
		nil
		(if (ChecaLinhaVazia (cdr (FazNVezes (function car) grafo n)))
			(cons (car profs) (StepEncontraAlunosVazios grafo (cdr profs) (1+ n)))
			(StepEncontraAlunosVazios grafo (cdr profs) (1+ n))
		)
	)
)
;---------------------------------------------------
(defun ChecaLinhaVazia (linha)
	(if (null linha)
		t
		(if (not (zerop (car linha)))
			nil
			(ChecaLinhaVazia (cdr linha))
		)
	)
)

(defun ChecaColunaVazia (coluna grafo)
	(if (null grafo)
		t
		(if (not (zerop (car (FazNVezes (function cdr) (cdr grafo) coluna))))
			nil
			(ChecaColunaVazia coluna (car grafo))
		)
	)
)

(defun PegaListaAlunos (DB) (cdr DB))
(defun PegaListaDiscs (DB) (cdr (car DB)))
(defun PegaListaProfs (DB) (cdr (caar DB)))
(defun PegaGrafoAD (DB) (cdr (caaar DB)))
(defun PegaGrafoPD (DB) (cdr (caaaar DB)))
(defun PegaListaCursos (DB) (cdar (caaaar DB)))
