;=============================================== INICIO MATRICULAR ==============================================
(defun Matricular (alunos disciplinas bd)
	(cons
		(cons 
			(cons
				(cons
					(cons
						(cons 
							nil
							(AdicionaListaCursos 
								(PegaListaCursos bd) 
								(PegaListaDiscs bd) 
								(VerificaEAdiciona 
									(PegaListaDiscs bd) 
									disciplinas 
									(PegaListaDiscs bd)
								) 
								0
							) 
						)
						(EffVincula
							bd
							'(nil)
							disciplinas
							(PegaListaProfs bd)
							(PegaListaProfs bd)
							(VerificaEAdiciona (PegaListaDiscs bd) disciplinas (PegaListaDiscs bd))
						)
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
			)
			(VerificaEAdiciona (PegaListaDiscs bd) disciplinas (PegaListaDiscs bd))
		)
		(VerificaEAdiciona (PegaListaAlunos bd) alunos (PegaListaAlunos bd))
	)
)

(defun EffMatricula (bd alunos disciplinas idalunos idalunoscopia iddisciplinas)
	(if (null idalunos)
		nil
		(cons
			(EffMatricula 
				bd 
				alunos 
				disciplinas 
				(cdr idalunos) 
				idalunoscopia 
				iddisciplinas
			)
			(MatriculaAD 
				bd 
				(car idalunos) 
				alunos 
				disciplinas 
				iddisciplinas
			)
		)
	)
)

(defun MatriculaAD (bd alunoatual alunos disciplinas iddisciplinas)
	(if (null iddisciplinas)
		nil
		(if (and (existe? disciplinas (car iddisciplinas)) (existe? alunos alunoatual))
			(cons 
				1 
				(MatriculaAD
					bd
					alunoatual
					alunos
					disciplinas
					(cdr iddisciplinas)
				)
			)
			(if (and (existe? (PegaListaAlunos bd) alunoatual) (existe? (PegaListaDiscs bd) (car iddisciplinas)))
				(cons
					(AcessaNoGrafoAD
						bd 
						alunoatual 
						(car iddisciplinas)
					)
					(MatriculaAD
						bd 
						alunoatual 
						alunos 
						disciplinas 
						(cdr iddisciplinas)
					)
				)
				(cons
					0
					(MatriculaAD 
						bd 
						alunoatual 
						alunos 
						disciplinas 
						(cdr iddisciplinas)
					)
				)
			)
		)
	)
)
;================================================= FIM MATRICULAR ===============================================
;=========================================== INICIO CANCELAR-MATRICULA ==========================================
(defun Cancelar-Matricula (alunos disciplinas bd)
	(Desmatricula bd alunos disciplinas (PegaPrimerAlunos bd alunos disciplinas))
)

(defun Desmatricula (bd alunos disciplinas primedgrafo)
	(cons
		(cons 
			(cons
				(cons
					(cons
						(cons 
							nil
							(RemoveListaCursos 
								(PegaListaCursos bd) 
								(PegaListaDiscs bd) 
								(EncontraDiscsVazias 
									primedgrafo 
									(PegaGrafoPD bd) 
									(PegaListaDiscs bd) 
									0
								)
							)
						)
						(EffDesvincula
							bd
							'(nil)
							disciplinas
							(PegaListaProfs bd)
							(PegaListaDiscs bd)
							(EncontraLinhasVazias (PegaGrafoPD bd) (PegaListaProfs bd) 0)
							(EncontraDiscsVazias primedgrafo (PegaGrafoPD bd) (PegaListaDiscs bd) 0)
						)
					)
					(EffDesmatricula
						bd
						alunos
						disciplinas
						(PegaListaAlunos bd)
						(PegaListaDiscs bd)
						(EncontraLinhasVazias primedgrafo (PegaListaAlunos bd) 0)
						(EncontraDiscsVazias  primedgrafo (PegaGrafoPD bd) (PegaListaDiscs bd) 0)
					)
				)
				(PegaListaProfs bd)
			)
			(CopiaExcluindoMulti 
				(PegaListaDiscs bd) 
				(EncontraDiscsVazias 
					primedgrafo 
					(PegaGrafoPD bd) 
					(PegaListaDiscs bd) 
					0
				)
			)
		)
		(CopiaExcluindoMulti 
			(PegaListaAlunos bd) 
			(EncontraLinhasVazias 
				primedgrafo 
				(PegaListaAlunos bd) 
				0
			)
		)
	)
)

(defun PegaPrimerAlunos (bd alunos disciplinas)
	(DesmatriculaPrimer bd alunos disciplinas (PegaListaAlunos bd) (PegaListaAlunos bd) (PegaListaDiscs bd))
)

(defun DesmatriculaPrimer (bd alunos disciplinas idalunos idalunoscopia iddisciplinas)
	(if (null idalunos)
		nil
		(cons
			(DesmatriculaPrimer
				bd 
				alunos 
				disciplinas 
				(cdr idalunos) 
				idalunoscopia 
				iddisciplinas
			)
			(DesmatriculaPrimerLinha 
				bd 
				(car idalunos) 
				alunos 
				disciplinas 
				iddisciplinas
			)
		)
	)
)

(defun DesmatriculaPrimerLinha (bd alunoatual alunos disciplinas iddisciplinas)
	(if (null iddisciplinas)
		nil
		(if (and (existe? disciplinas (car iddisciplinas)) (existe? alunos alunoatual))
			(cons 
				0
				(DesmatriculaPrimerLinha 
					bd 
					alunoatual 
					alunos 
					disciplinas 
					(cdr iddisciplinas)
				)
			)
			(cons
				(AcessaNoGrafoAD
					bd 
					alunoatual 
					(car iddisciplinas)
				)
				(DesmatriculaPrimerLinha 
					bd 
					alunoatual 
					alunos 
					disciplinas 
					(cdr iddisciplinas)
				)
			)
		)
	)
)

(defun EffDesmatricula (bd alunos disciplinas alunosantigos disciplinasantigas alunosexcluidos disciplinasexcluidas)
	(if (null alunosantigos)
		nil
		(if (not (Existe? alunosexcluidos (car alunosantigos)))
			(cons
				(EffDesmatricula 
					bd 
					alunos 
					disciplinas 
					(cdr alunosantigos) 
					disciplinasantigas 
					alunosexcluidos 
					disciplinasexcluidas
				)
				(DesmatriculaAD 
					bd 
					(car alunosantigos) 
					alunos 
					disciplinas 
					disciplinasantigas 
					disciplinasexcluidas
				)
			)
			(EffDesmatricula 
				bd 
				alunos 
				disciplinas 
				(cdr alunosantigos) 
				disciplinasantigas 
				alunosexcluidos 
				disciplinasexcluidas
			)
		)
	)
)

(defun DesmatriculaAD (bd alunoatual alunos disciplinas disciplinasantigas disciplinasexcluidas)
	(if (null disciplinasantigas)
		nil
		(if (not (Existe? disciplinasexcluidas (car disciplinasantigas)))
			(if (and (Existe? disciplinas (car disciplinasantigas)) (Existe? alunos alunoatual))
				(cons
					0
					(DesmatriculaAD
						bd 
						alunoatual 
						alunos 
						disciplinas 
						(cdr disciplinasantigas) 
						disciplinasexcluidas
					)
				)
				(cons
					(AcessaNoGrafoAD 
						bd 
						alunoatual 
						(car disciplinasantigas)
					)
					(DesmatriculaAD 
						bd 
						alunoatual 
						alunos 
						disciplinas 
						(cdr disciplinasantigas) 
						disciplinasexcluidas
					)
				)
			)
			(DesmatriculaAD 
				bd 
				alunoatual 
				alunos 
				disciplinas 
				(cdr disciplinasantigas)
				disciplinasexcluidas
			)
		)
	)
)
;============================================= FIM CANCELAR-MATRICULA ===========================================
;================================================ INICIO VINCULAR ===============================================
(defun Vincular (professores disciplinas bd)
	(cons
		(cons 
			(cons
				(cons
					(cons
						(cons 
							nil
							(AdicionaListaCursos 
								(PegaListaCursos bd) 
								(PegaListaDiscs bd) 
								(VerificaEAdiciona 
									(PegaListaDiscs bd) 
									disciplinas 
									(PegaListaDiscs bd)
								) 
								0
							) 
						)
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
						(PegaListaAlunos bd)
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

(defun EffVincula (bd professores disciplinas idprofs idprofscopia iddisciplinas)
	(if (null idprofs)
		nil
		(cons
			(EffVincula 
				bd 
				professores 
				disciplinas 
				(cdr idprofs) 
				idprofscopia 
				iddisciplinas
			)
			(VinculaAD 
				bd 
				(car idprofs) 
				professores 
				disciplinas 
				iddisciplinas
			)
		)
	)
)

(defun VinculaAD (bd profatual professores disciplinas iddisciplinas)
	(if (null iddisciplinas)
		nil
		(if (and (existe? disciplinas (car iddisciplinas)) (existe? professores profatual))
			(cons 
				1 
				(VinculaAD
					bd 
					profatual 
					professores 
					disciplinas 
					(cdr iddisciplinas)
				)
			)
			(if (and (existe? (PegaListaProfs bd) profatual) (existe? (PegaListaDiscs bd) (car iddisciplinas)))
				(cons
					(AcessaNoGrafoPD 
						bd 
						profatual 
						(car iddisciplinas)
					)
					(VinculaAD 
						bd 
						profatual 
						professores 
						disciplinas 
						(cdr iddisciplinas)
					)
				)
				(cons
					0
					(VinculaAD 
						bd 
						profatual 
						professores 
						disciplinas 
						(cdr iddisciplinas)
					)
				)
			)
		)
	)
)
;================================================= FIM VINCULAR ===============================================
;============================================ INICIO REMOVER-VINCULO ==========================================
(defun Remover-Vinculo (professores disciplinas bd)
	(Desvincula bd professores disciplinas (PegaPrimerProfs bd professores disciplinas))
)

(defun Desvincula (bd profs disciplinas primedgrafo)
	(cons
		(cons 
			(cons
				(cons
					(cons
						(cons 
							nil
							(RemoveListaCursos 
								(PegaListaCursos bd) 
								(PegaListaDiscs bd) 
								(EncontraDiscsVazias 
									(PegaGrafoAD bd) 
									primedgrafo 
									(PegaListaDiscs bd) 
									0
								)
							)
						)
						(EffDesvincula
							bd
							profs
							disciplinas
							(PegaListaProfs bd)
							(PegaListaDiscs bd)
							(EncontraLinhasVazias primedgrafo (PegaListaProfs bd) 0)
							(EncontraDiscsVazias (PegaGrafoAD bd) primedgrafo (PegaListaDiscs bd) 0)
						)
					)
					(EffDesmatricula
							bd
							'(nil)
							disciplinas
							(PegaListaAlunos bd)
							(PegaListaDiscs bd)
							(EncontraLinhasVazias (PegaGrafoAD bd) (PegaListaAlunos bd) 0)
							(EncontraDiscsVazias (PegaGrafoAD bd) primedgrafo (PegaListaDiscs bd) 0)
					)
				)
				(CopiaExcluindoMulti 
					(PegaListaProfs bd) 
					(EncontraLinhasVazias 
						primedgrafo 
						(PegaListaProfs bd) 
						0
					)
				)
			)
			(CopiaExcluindoMulti 
				(PegaListaDiscs bd) 
				(EncontraDiscsVazias 
					(PegaGrafoAD bd) 
					primedgrafo 
					(PegaListaDiscs bd) 
					0
				)
			)
		)
		(PegaListaAlunos bd)
	)
)

(defun PegaPrimerProfs (bd profs disciplinas)
	(DesvinculaPrimer bd profs disciplinas (PegaListaProfs bd) (PegaListaProfs bd) (PegaListaDiscs bd))
)

(defun DesvinculaPrimer (bd profs disciplinas idprofs idprofscopia iddisciplinas)
	(if (null idprofs)
		nil
		(cons
			(DesvinculaPrimer 
				bd 
				profs 
				disciplinas 
				(cdr idprofs) 
				idprofscopia 
				iddisciplinas
			)
			(DesvinculaPrimerLinha 
				bd 
				(car idprofs) 
				profs 
				disciplinas 
				iddisciplinas
			)
		)
	)
)

(defun DesvinculaPrimerLinha (bd profatual profs disciplinas iddisciplinas)
	(if (null iddisciplinas)
		nil
		(if (and (existe? disciplinas (car iddisciplinas)) (existe? profs profatual))
			(cons 
				0
				(DesvinculaPrimerLinha 
					bd 
					profatual 
					profs 
					disciplinas 
					(cdr iddisciplinas)
				)
			)
			(cons
				(AcessaNoGrafoPD 
					bd 
					profatual 
					(car iddisciplinas)
				)
				(DesvinculaPrimerLinha 
					bd 
					profatual 
					profs 
					disciplinas 
					(cdr iddisciplinas)
				)
			)
		)
	)
)

(defun EffDesvincula (bd profs disciplinas profsantigos disciplinasantigas profsexcluidos disciplinasexcluidas)
	(if (null profsantigos)
		nil
		(if (not (Existe? profsexcluidos (car profsantigos)))
			(cons
				(EffDesvincula 
					bd 
					profs 
					disciplinas 
					(cdr profsantigos) 
					disciplinasantigas 
					profsexcluidos 
					disciplinasexcluidas
				)
				(DesvinculaAD 
					bd 
					(car profsantigos) 
					profs 
					disciplinas 
					disciplinasantigas 
					disciplinasexcluidas
				)
			)
			(EffDesmatricula 
				bd 
				profs 
				disciplinas 
				(cdr profsantigos) 
				disciplinasantigas 
				profsexcluidos 
				disciplinasexcluidas
			)
		)
	)
)

(defun DesvinculaAD (bd profatual profs disciplinas disciplinasantigas disciplinasexcluidas)
	(if (null disciplinasantigas)
		nil
		(if (not (Existe? disciplinasexcluidas (car disciplinasantigas)))
			(if (and (Existe? disciplinas (car disciplinasantigas)) (Existe? profs profatual))
				(cons
					0
					(DesvinculaAD 
						bd 
						profatual 
						profs 
						disciplinas 
						(cdr disciplinasantigas) 
						disciplinasexcluidas
					)
				)
				(cons
					(AcessaNoGrafoPD 
						bd 
						profatual 
						(car disciplinasantigas)
					)
					(DesvinculaAD 
						bd 
						profatual 
						profs 
						disciplinas 
						(cdr disciplinasantigas) 
						disciplinasexcluidas
					)
				)
			)
			(DesvinculaAD 
				bd 
				profatual 
				profs 
				disciplinas 
				(cdr disciplinasantigas) 
				disciplinasexcluidas
			)
		)
	)
)
;=============================================== FIM REMOVER-VINCULO ==========================================
;=========================================== INICIO VINCULAR-DISC-CURSO =======================================
(defun Vincular-Disc-Curso (disciplina curso bd)
	(if (null (Existe? (cdr (car bd)) disciplina))
	bd
		(cons
			(cons 
				(cons
					(cons
						(cons
							(cons 
								nil
								(StepVinculaDiscCurso 
									(PegaListaCursos bd) 
									(PegaListaDiscs bd) 
									disciplina 
									curso 
									0
								)
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
)

(defun StepVinculaDiscCurso (listacursos listadisciplinas disciplina curso n)
	(if (null listadisciplinas)
		nil
		(if (equal (car listadisciplinas) disciplina)
			(cons 
				curso 
				(StepVinculaDiscCurso 
					listacursos 
					(cdr listadisciplinas) 
					disciplina 
					curso 
					(1+ n)
				)
			)
			(cons 
				(car 
					(FazNVezes 
						(function cdr) 
						listacursos 
						n
					)
				) 
				(StepVinculaDiscCurso 
					listacursos 
					(cdr listadisciplinas) 
					disciplina 
					curso 
					(1+ n)
				)
			)
		)
	)
)
;============================================= FIM VINCULAR-DISC-CURSO ========================================
;================================================== INICIO ALUNOS =============================================
(defun Alunos? (bd)
	(cdr bd)
)
;==================================================== FIM ALUNOS ==============================================
;================================================ INICIO PROFESSORES ==========================================
(defun Professores? (bd)
	(cdaar bd)
)
;================================================== FIM PROFESSORES ===========================================
;================================================ INICIO DISCIPLINAS ==========================================
(defun Disciplinas? (bd)
	(cdar bd)
)
;================================================== FIM DISCIPLINAS ===========================================
;================================================ INICIO MATRICULADOS =========================================
(defun Matriculados? (disciplina bd)
	(if (existe? (PegaListaDiscs bd) disciplina)
		(StepMatriculados bd (PegaListaAlunos bd) disciplina)
		nil
	)
)

(defun StepMatriculados (bd alunos disciplina)
	(if (null alunos)
		nil
		(if (eq (AcessaNoGrafoAD bd (car alunos) disciplina) 1)
			(cons 
				(car alunos)
				(StepMatriculados bd (cdr alunos) disciplina)
			)
			(StepMatriculados bd (cdr alunos) disciplina) 
		)
	)
)
;================================================ FIM MATRICULADOS =========================================
;=============================================== INICIO VINCULADOS =========================================
(defun Vinculados? (disciplina bd)
	(if (existe? (PegaListaDiscs bd) disciplina)
		(StepVinculados bd (PegaListaProfs bd) disciplina)
		nil
	)
)

(defun StepVinculados (bd professores disciplina)
	(if (null professores)
		nil
		(if (eq (AcessaNoGrafoPD bd (car professores) disciplina) 1)
			(cons
				(car professores)
				(StepVinculados bd (cdr professores) disciplina)
			)
			(StepVinculados bd (cdr professores) disciplina)
		)
	)
)
;================================================= FIM VINCULADOS ==========================================
;================================================== INICIO CURSA ===========================================
(defun Cursa? (aluno bd)
	(if (existe? (PegaListaAlunos bd) aluno)
		(StepCursa bd aluno (PegaListaDiscs bd))
		nil
	)
)

(defun StepCursa (bd aluno disciplinas)
	(if (null disciplinas)
		nil
		(if (eq (AcessaNoGrafoAD bd aluno (car disciplinas)) 1)
			(cons 
				(car disciplinas)
				(StepCursa bd aluno (cdr disciplinas))
			)
			(StepCursa bd aluno (cdr disciplinas))
		)
	)
)
;==================================================== FIM CURSA ============================================
;================================================= INICIO MINISTRA =========================================
(defun Ministra? (professor bd)
	(if (existe? (PegaListaProfs bd) professor)
		(StepMinistra bd professor (PegaListaDiscs bd))
		nil
	)
)

(defun StepMinistra (bd professor disciplinas)
	(if (null disciplinas)
		nil
		(if (eq (AcessaNoGrafoPD bd professor (car disciplinas)) 1)
			(cons
				(car disciplinas)
				(StepMinistra bd professor (cdr disciplinas))
			)
			(StepMinistra bd professor (cdr disciplinas))
		)
	)
)
;=================================================== FIM MINISTRA ==========================================
;================================================= INICIO DISC-CURSO =======================================
(defun Disc-Curso? (disciplina bd)
	(if (existe? (PegaListaDiscs bd) disciplina)
		(StepDiscCurso (PegaListaDiscs bd) disciplina (PegaListaCursos bd))
		nil
	)
)

(defun StepDiscCurso (listadisciplinas disciplina listacursos)
	(if (null listadisciplinas)
		nil
		(if (equal (car listadisciplinas) disciplina)
			(car listacursos)
			(StepDiscCurso 
				(cdr listadisciplinas) 
				disciplina 
				(cdr listacursos)
			)
		)
	)
)
;=================================================== FIM DISC-CURSO ========================================

;=============================================== INICIO FUNCOES SUPORTE ====================================
(defun TentaAdicionar (a b)
	(if (or (null a) (null b))
		nil
		(+ a b)
	)
) 	

(defun EncontraEmLista (lista nome) ;Procura algum nome na lista e retorna a posicao
	(if (null lista)
		nil
		(if (equal (car lista) nome)
			0
			(TentaAdicionar 1 (EncontraEmLista (cdr lista) nome))
		)
	)
)

(defun FazNVezes (func lista n) ;Repete uma funcao (cdr ou car) n vezes (usar com encontraemlista)
	(if (= n 0)
		lista
		(FazNVezes func (funcall func lista) (- n 1))
	)
)

(defun CopiaExcluindoMulti (ListaOriginal ListaASerRemovida) ;Copia elementos de uma lista, desde que nao estejam na outra lista
	(if (null ListaOriginal)
		nil
		(if (not (Existe? ListaASerRemovida (car ListaOriginal)))
			(cons 
				(car ListaOriginal) 
				(CopiaExcluindoMulti 
					(cdr ListaOriginal) 
					ListaASerRemovida
				)
			)
			(CopiaExcluindoMulti 
				(cdr ListaOriginal) 
				ListaASerRemovida
			)
		)
	)
)

(defun Existe? (lista valor) ;Verifica se existe o elemento na lista
	(if (null lista)
		nil
		(if (equal (car lista) valor)
			t
			(Existe? (cdr lista) valor)
		)
	)
)

(defun AcessaNoGrafoAD (bd linha coluna)
	(car
		(FazNVezes 
			(function cdr)
			(cdr
				(FazNVezes 
					(function car)
					(PegaGrafoAD bd)
					(EncontraEmLista (PegaListaAlunos bd) linha) ;Encontra a posicao referente ao aluno (linha)
				)
			)
			(EncontraEmLista (PegaListaDiscs bd) coluna) ;Encontra a posicao referente a disciplina (coluna)
		)
	)
)

(defun AcessaNoGrafoPD (bd linha coluna)
	(car
		(FazNVezes 
			(function cdr)
			(cdr
				(FazNVezes 
					(function car)
					(PegaGrafoPD bd)
					(EncontraEmLista (PegaListaProfs bd) linha) ;Encontra a posicao referente ao professor (linha)
				)
			)
			(EncontraEmLista (PegaListaDiscs bd) coluna) ;Encontra a posicao referente a disciplina (coluna)
		)
	)
)

(defun AdicionaListaCursos (listacursos disciplinasantigas disciplinasnovas n)
	(if (null disciplinasnovas)
		nil
		(if (Existe? disciplinasantigas (car disciplinasnovas))
			(cons 
				(car 
					(FazNVezes (function cdr) listacursos n)
				) 
				(AdicionaListaCursos 
					listacursos 
					disciplinasantigas 
					(cdr disciplinasnovas) 
					(1+ n)
				)
			)
			(cons 
				nil 
				(AdicionaListaCursos 
					listacursos 
					disciplinasantigas 
					(cdr disciplinasnovas) 
					(1+ n)
				)
			)
		)
	)
)

(defun RemoveListaCursos (listacursos listadiscsantiga listadiscsvazias)
	(if (null listadiscsantiga)
		nil
		(if (Existe? listadiscsvazias (car listadiscsantiga))
			(RemoveListaCursos 
				(cdr listacursos) 
				(cdr listadiscsantiga) 
				listadiscsvazias
			)
			(cons
				(car listacursos) 
				(RemoveListaCursos 
					(cdr listacursos) 
					(cdr listadiscsantiga) 
					listadiscsvazias
				)
			)
		)
	)
)

(defun VerificaEAdiciona (lista nomes antiga)
	(if (not (null lista))
		(cons 
			(car lista) 
			(VerificaEAdiciona (cdr lista) nomes antiga)
		)
		(if (null (car nomes))
			nil
			(if (not (existe? antiga (car nomes)))
				(cons 
					(car nomes) 
					(VerificaEAdiciona lista (cdr nomes) antiga)
				)
				(VerificaEAdiciona lista (cdr nomes) antiga)
			)
		)
	)
)

(defun EncontraLinhasVazias (grafo referencia n)
	(if (null referencia)
		nil
		(if (ChecaLinhaVazia (cdr (FazNVezes (function car) grafo n)))
			(cons 
				(car referencia) 
				(EncontraLinhasVazias 
					grafo 
					(cdr referencia) 
					(1+ n)
				)
			)
			(EncontraLinhasVazias 
				grafo 
				(cdr referencia) 
				(1+ n)
			)
		)
	)
)

(defun EncontraDiscsVazias (grafoalunos grafoprofs disciplinas n)
	(if (null disciplinas)
		nil
		(if (and (ChecaColunaVazia n grafoalunos) (ChecaColunaVazia n grafoprofs))
			(cons
				(car disciplinas) 
				(EncontraDiscsVazias 
					grafoalunos 
					grafoprofs 
					(cdr disciplinas) 
					(1+ n)
				)
			)
			(EncontraDiscsVazias 
				grafoalunos 
				grafoprofs 
				(cdr disciplinas) 
				(1+ n)
			)
		)
	)
)

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

(defun PegaListaAlunos (bd) (cdr bd))
(defun PegaListaDiscs (bd) (cdr (car bd)))
(defun PegaListaProfs (bd) (cdr (caar bd)))
(defun PegaGrafoAD (bd) (cdr (caaar bd)))
(defun PegaGrafoPD (bd) (cdr (caaaar bd)))
(defun PegaListaCursos (bd) (cdar (caaaar bd)))
