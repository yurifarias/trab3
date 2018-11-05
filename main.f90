 PROGRAM estruturas
    IMPLICIT NONE
    
    INTEGER :: f1 = 101, f2 = 102
    CHARACTER*50 titulo
    CHARACTER(LEN=15) rigt
    INTEGER i, j, k, l, nele, nnos, noj, nok, conf_ele[ALLOCATABLE](:,:)
    REAL*8 larg[ALLOCATABLE](:), altu[ALLOCATABLE](:), area[ALLOCATABLE](:), iner[ALLOCATABLE](:), mate[ALLOCATABLE](:)
    REAL*8 conf_nos[ALLOCATABLE](:,:), compx[ALLOCATABLE](:), compy[ALLOCATABLE](:), comp[ALLOCATABLE](:)
    REAL*8 soll[ALLOCATABLE](:,:), esfn[ALLOCATABLE](:), esfg[ALLOCATABLE](:), esft[ALLOCATABLE](:)
    REAL*8 val1, val2, val3, val4, val5, cosE, senE, rest[ALLOCATABLE](:,:), dglo[ALLOCATABLE](:), maux_slo[ALLOCATABLE](:,:)
    REAL*8 rest_restrito[ALLOCATABLE](:,:), esft_restrito[ALLOCATABLE](:), vrig[ALLOCATABLE](:,:,:), vrot[ALLOCAtABLE](:,:,:)
    REAL*8,DIMENSION(6,6) :: rloc = 0.0, mrot = 0.0, rott, maux, rglo, maux_rot, maux_rlo
    REAL*8,DIMENSION(6) :: sloc, sglo, vaux, vaux_dgl, vaux_dlo, vaux_elo, vaux_slo
    LOGICAL restricoes[ALLOCATABLE](:)
    
    ! f1: Arquivo de entrada de dados 'entrada.txt'
    ! f2: Arquivo de saída de dados 'saida.txt'
    ! titulo: String para armazenar titulo do trabalho
    ! i, j, k, l: Variáveis para iteração
    ! nele: Número de elementos
    ! nnos: Número de nós
    ! noj: Variável de auxílio para montagem da matriz de rigidez da estrutura
    ! nok: Variável de auxílio para montagem da matriz de rigidez da estrutura
    ! conf_ele: Matriz que armazena nó inicial e final para cada elemento [noi, nof]
    ! conf_nos: Matriz que armazena posição x e y de cada nó [posx, posy]
    ! larg: Vetor que armazena largura da seção de cada elemento
    ! altu: Vetor que armazena altura da seção de cada elemento
    ! area: Vetor que armazena area da seção de cada elemento
    ! iner: Vetor que armazena momento de inércia da seção de cada elemento
    ! compx: Vetor que armazena projeção do elemento em relação ao eixo x
    ! compy: Vetor que armazena projeção do elemento em relação ao eixo y
    ! comp: Vetor que armazena comprimento de cada elemento
    ! rloc: Matriz de rigidez local do elemento, já inicializada com zeros
    ! rglo: Matriz de rigidez global do elemento
    ! rest: Matriz de rigidez da estrutura
    ! mrot: Matriz de rotação para cada elemento
    ! rott: Transposta da matriz acima
    ! maux: Matriz auxiliar para cálculo matricial
    ! vaux: Vetor auxiliar para cálculo matricial
    ! val1: Coeficiente EA/L
    ! val2: Coeficiente 12EI/L^3
    ! val3: Coeficiente 6EI/L^2
    ! val4: Coeficiente 4EI/L
    ! val5: Coeficiente 2EI/L
    ! maux_slo: Matriz auxiliar que armazena solicitações nodais
    ! soll: Matriz que armazena carregamentos distribuídos normais e transversais locais [Nli, Nlf, Qli, Qlf]
    ! sloc: Vetor que armazena carregamentos locais do elemento [Fxli, Fyli, Mli, Fxlf, Fylf, Mlf]
    ! sglo: Vetor que armazena carregamentos globais do elemento [Fxgi, Fygi, Mgi, Fxgf, Fygf, Mgf]
    ! esfn: Vetor que armazena esforços nodais [Fx, Fy, M]
    ! esfg: Vetor que armazena esforços globais das solicitações [sglo]
    ! esft: Vetor que armazena esforços globais totais [esft] = [esfg] + [esfn]
    ! restricoes: Vetor que armazena restrições nodais [TRUE/FALSE]
    ! rest_restrito: Matriz de rigidez global da estrutura com restrições (técnica 1/0)
    ! esft_restrito: Vetor de esforços nodais globais totais com restrições (técnica 1/0)
    ! dglo: Vetor de deslocamentos nodais globais
    ! vrig: Vetor que armazena matrizes de rigidez locais dos elementos
    ! vrot: Vetor que armazena matrizes de rotação dos elementos
    ! vaux_dgl: Vetor auxiliar que armazena vetor de deslocamentos globais do elemento i
    ! vaux_dlo: Vetor auxiliar que armazena vetor de deslocamentos locais do elemento i
    ! vaux_slo: Vetor auxiliar que armazena vetor de solicitações locais no elemento i
    ! vaux_elo: Vetor auxiliar que armazena vetor de esforços locais do elemento i [rloc]{dloc} - {sloc} = {esfl}
    ! maux_rlo: Matriz auxiliar que armazena matriz de rigidez local do elemento i
    ! maux_rot: Matriz auxiliar que armazena matriz de rotação do elemento i
    
    ! Abrir arquivos de entrada e de saída
    OPEN(f1, FILE='entrada.txt', STATUS='unknown')
    OPEN(f2, FILE='saida.txt', STATUS='unknown')
    
    ! Ler título do projeto
    READ(f1, '(A)') titulo
    WRITE(f2, '(A)') titulo
    
    ! Pular uma linha no arquivo de saída
    WRITE(f2, *)
    READ(f1, *)
    READ(f1, *)
    
    ! Ler número de elementos
    READ(f1, *) nele
   
    ! Alocar vetores: larg, altu, area, iner, mate, compx, compy, comp
    ALLOCATE(larg(nele), altu(nele), area(nele), iner(nele), mate(nele), compx(nele), compy(nele), comp(nele))
    
    ! Alocar matriz com nó inicial e final para cada elemento
    ALLOCATE(conf_ele(nele,2))
    
    ! Alocar matriz que armazena carregamento distribuido normal e transversal
    ALLOCATE(soll(nele,4))
    
    ! Alocar vetor que armazena matriz de rigidez e matriz de rotação de cada elemento
    ALLOCATE(vrig(6,6,nele), vrot(6,6,nele),maux_slo(6,nele))
    
    ! Escrever cabeçalho referente aos elementos no arquivo de saída
    READ(f1, *)
    READ(f1, *)
    WRITE(f2, '(A)') '  Elemento   No inicial   No final   Mod. El (N/m2)    Largura (m)     Altura (m)  '
    
    ! Laço para pegar dados dos elementos e escrever no arquivo de saída
    DO i=1, nele
        READ(f1, *) j, conf_ele(j,1), conf_ele(j,2), mate(j), larg(j), altu(j), soll(j,1), soll(j,2), soll(j,3), soll(j,4)
        WRITE(f2, 51) j, conf_ele(j,1), conf_ele(j,2), mate(j), larg(j), altu(j)
    ENDDO
    
    ! Ler número de nós
    READ(f1, *)
    READ(f1, *)
    READ(f1, *) nnos
    
    ! Alocar matriz com posicoes x e y de cada nó
    ALLOCATE(conf_nos(nnos,2))
    
    ! Alocar vetor com carregamentos nodais
    ALLOCATE(esfn(3*nnos), esfg(3*nnos))
    esfg = 0.0
    
    ! Alocar matriz de rigidez da estrutura
    ALLOCATE(rest(3*nnos, 3*nnos))
    rest = 0.0
    
    ! Alocar vetor de restricoes nos nós
    ALLOCATE(restricoes(3*nnos))
    
    ! Alocar vetor de deslocamentos globais
    ALLOCATE(dglo(3*nnos))
    
    ! Escrever cabeçalho referente aos nós e suas posições
    READ(f1, *)
    READ(f1, *)
    READ(f1, *)
    WRITE(f2, *)
    WRITE(f2,'(A)') '     No        X (m)       Y (m)'
    
    ! Laço para pegar dados dos nós e escrever no arquivo de saída
    DO i=1, nnos
        READ(f1, *) j, conf_nos(j,1), conf_nos(j,2), esfn(3*i - 2), esfn(3*i - 1), esfn(3*i), restricoes(3*i-2), restricoes(3*i-1), restricoes(3*i)
        WRITE(f2, 52) j, conf_nos(j,1), conf_nos(j,2)
    ENDDO
    
    DO i=1, nele
        area(i) = larg(i) * altu(i)                                         ! Área
        iner(i) = larg(i) * altu(i) ** 3 / 12                               ! Momento de inércia
        compx(i) = conf_nos(conf_ele(i,2), 1) - conf_nos(conf_ele(i,1), 1)  ! Projeção do comprimento sobre o eixo x
        compy(i) = conf_nos(conf_ele(i,2), 2) - conf_nos(conf_ele(i,1), 2)  ! Projeção do comprimento sobre o eixo y
        comp(i) = ((compx(i) ** 2) + (compy(i) ** 2)) ** 0.5
        
        val1 = mate(i) * area(i) / comp(i)              ! EA/L
        val2 = 12 * mate(i) * iner(i) / (comp(i) ** 3)  ! 12EI/L^3
        val3 = 6 * mate(i) * iner(i) / (comp(i) ** 2)   ! 6EI/L^2
        val4 = 4 * mate(i) * iner(i) / comp(i)          ! 4EI/L
        val5 = 2 * mate(i) * iner(i) / comp(i)          ! 2EI/L
        
        cosE = compx(i) / comp(i)   ! Cosseno (projeção x sobre comprimento)
        senE = compy(i) / comp(i)   ! Seno (projeção y sobre comprimento)
        
        ! Início da matriz de rigidez local do elemento i
        
        ! Linha 1
        rloc(1,1) = val1
        rloc(1,4) = - val1
        
        ! Linha 2
        rloc(2,2) = val2
        rloc(2,3) = val3
        rloc(2,5) = - val2
        rloc(2,6) = val3
        
        ! Linha 3
        rloc(3,2) = val3
        rloc(3,3) = val4
        rloc(3,5) = - val3
        rloc(3,6) = val5
        
        ! Linha 4
        rloc(4,1) = - val1
        rloc(4,4) = val1
        
        ! Linha 5
        rloc(5,2) = - val2
        rloc(5,3) = - val3
        rloc(5,5) = val2
        rloc(5,6) = - val3
        
        ! Linha 6
        rloc(6,2) = val3
        rloc(6,3) = val5
        rloc(6,5) = - val3
        rloc(6,6) = val4
        
        ! Fim da matriz de rigidez local do elemento i
        
        ! Armazenar matriz de rigidez do elemento i
        DO j=1, 6
            DO k=1, 6
                vrig(j,k,i) = rloc(j,k)
            ENDDO
        ENDDO
        
        ! Início da matriz de rotação do elemento i
        
        ! Linha 1
        mrot(1,1) = cosE
        mrot(1,2) = senE
        
        ! Linha 2
        mrot(2,1) = - senE
        mrot(2,2) = cosE
        
        ! Linha 3
        mrot(3,3) = 1
        
        ! Linha 4
        mrot(4,4) = cosE
        mrot(4,5) = senE
        
        ! Linha 5
        mrot(5,4) = - senE
        mrot(5,5) = cosE
        
        ! Linha 6
        mrot(6,6) = 1
        
        ! Fim da matriz de rotação do elemento i
        
        ! Armazenar matriz de rotação do elemento i
        DO j=1, 6
            DO k=1, 6
                vrot(j,k,i) = mrot(j,k)
            ENDDO
        ENDDO
        
        ! Transpor matriz de rotação [mrot] e armazenar em [rott]
        rott = TRANSPOSE(mrot)
        
        ! Montar vetor de solicitações locais para o elemento i
        sloc(1) = (soll(i,1) / 3 + soll(i,2) / 6) * comp(i)               ! (Ni/3 + Nf/6)*L
        sloc(2) = (7 * soll(i,3) / 20 + 3 * soll(i,4) / 20) * comp(i)     ! (7*Qi/20 + 3Qf/20)*L
        sloc(3) = (soll(i,3) / 20 + soll(i,4) / 30) * comp(i) ** 2        ! (Qi/20 + Qf/30)*L^2
        sloc(4) = (soll(i,1) / 6 + soll(i,2) / 3) * comp(i)               ! (Ni/6 + Nf/3)*L
        sloc(5) = (3 * soll(i,3) / 20 + 7 * soll(i,4) / 20) * comp(i)     ! (3*Qi/20 + 7Qf/20)*L
        sloc(6) = (- soll(i,3) / 30 - soll(i,4) / 20) * comp(i) ** 2      ! (-Qi/30 - Qf/20)*L^2
        
        ! Endereçar vetores de solicitações locais
        DO j=1, 6
            maux_slo(j,i) = sloc(j)
        ENDDO
        
        ! Cálculo do vetor de solicitações globais para o elemento i
        sglo = MATMUL(rott, sloc)
        
        ! Endereçamento do vetor de solicitações globais para vetor esforços globais
        DO j=-2, 0
            noj = 3 * conf_ele(i,1) + j
            nok = 3 * conf_ele(i,2) + j
            esfg(noj) = esfg(noj) + sglo(3 + j)
            esfg(nok) = esfg(nok) + sglo(6 + j)
        ENDDO
        
        ! Geração de saída de dados
        WRITE(f2,*)
        WRITE(f2,53) 'ELEMENTO', i
        WRITE(f2,*)
        
        WRITE(f2,'(A)') 'COMPRIMENTO (m)      COSSENO           SENO        AREA (m2)     MINERCIA (m4)  MELASTICIDADE (N/m2)'
        WRITE(f2,54) comp(i), cosE, senE, area(i), iner(i), mate(i)
        WRITE(f2,*)
        
        WRITE(f2,'(a36,i3)') 'MATRIZ DE RIGIDEZ LOCAL DO ELEMENTO ', i
        DO k=1, 6
            WRITE(f2, 55) rloc(k,1), rloc(k,2), rloc(k,3), rloc(k,4), rloc(k,5), rloc(k,6)
        ENDDO
        WRITE(f2,*)
        
        WRITE(f2,56) 'MATRIZ DE ROTACAO DO ELEMENTO ', i
        DO k=1, 6
            WRITE(f2, 55) mrot(k,1), mrot(k,2), mrot(k,3), mrot(k,4), mrot(k,5), mrot(k,6)
        ENDDO
        WRITE(f2,*)
        
        WRITE(f2,57) 'MATRIZ DE ROTACAO TRANSPOSTA DO ELEMENTO ', i
        DO k=1, 6
            WRITE(f2, 55) rott(k,1), rott(k,2), rott(k,3), rott(k,4), rott(k,5), rott(k,6)
        ENDDO
        WRITE(f2,*)
        
        ! Multiplicação da matriz de rotação transposta pela matriz de rigidez local
        maux = MATMUL(rott, rloc)
        
        ! Multiplicação da matriz acima pela matriz de rotação, resultando na matriz de rigidez global
        rglo = MATMUL(maux, mrot)
                
        WRITE(f2,58) 'MATRIZ DE RIGIDEZ GLOBAL DO ELEMENTO ', i
        DO k=1, 6
            WRITE(f2, 55) rglo(k,1), rglo(k,2), rglo(k,3), rglo(k,4), rglo(k,5), rglo(k,6)
        ENDDO
        
        WRITE(f2, *)
        
        ! Montagem da matriz de rigidez da estrutura
        
        ! Para (nó inicial, nó inicial)
        WRITE(f2, '(A)') 'QUANDRANTE (NO INICIAL, NO INICIAL)'
        DO j=-2, 0
            noj = 3 * conf_ele(i,1) + j
            DO k=-2, 0
                nok = 3 * conf_ele(i,1) + k
                rest(noj, nok) = rest(noj, nok) + rglo(3 + j, 3 + k)
            ENDDO
            WRITE(f2, 59) rest(noj, nok - 2), rest(noj, nok - 1), rest(noj, nok)
        ENDDO
        WRITE(f2, *)
        
        ! Para (nó inicial, nó final)
        WRITE(f2, '(A)') 'QUANDRANTE (NO INICIAL, NO FINAL)'
        DO j=-2, 0
            noj = 3 * conf_ele(i,1) + j
            DO k=-2, 0
                nok = 3 * conf_ele(i,2) + k
                rest(noj, nok) = rest(noj, nok) + rglo(3 + j, 6 + k)
            ENDDO
            WRITE(f2, 59) rest(noj, nok - 2), rest(noj, nok - 1), rest(noj, nok)
        ENDDO
        WRITE(f2, *)
        
        ! Para (nó final, nó inicial)
        WRITE(f2, '(A)') 'QUANDRANTE (NO FINAL, NO INICIAL)'
        DO j=-2, 0
            noj = 3 * conf_ele(i,2) + j
            DO k=-2, 0
                nok = 3 * conf_ele(i,1) + k
                rest(noj, nok) = rest(noj, nok) + rglo(6 + j, 3 + k)
            ENDDO
            WRITE(f2, 59) rest(noj, nok - 2), rest(noj, nok - 1), rest(noj, nok)
        ENDDO
        WRITE(f2, *)
        
        ! Para (nó final, nó final)
        WRITE(f2, '(A)') 'QUANDRANTE (NO FINAL, NO FINAL)'
        DO j=-2, 0
            noj = 3 * conf_ele(i,2) + j
            DO k=-2, 0
                nok = 3 * conf_ele(i,2) + k
                rest(noj, nok) = rest(noj, nok) + rglo(6 + j, 6 + k)
            ENDDO
            WRITE(f2, 59) rest(noj, nok - 2), rest(noj, nok - 1), rest(noj, nok)
        ENDDO
        WRITE(f2, *)
        
        ! Fim da montagem da matriz de rigidez da estrutura
        
        WRITE(f2, '(a36,i3)') 'VETOR DE CARGAS GLOBAIS DO ELEMENTO ', i
        DO j=1, 6
            WRITE(f2, 60) sglo(j)
        ENDDO
    ENDDO
    
    ! Adição das solicitações globais dos elementos com as cargas nodais [esfg] + [esfn]
    esft = esfg + esfn
    
    WRITE(f2, *)
    
    ! Inclusão das restrições
    rest_restrito = rest
    esft_restrito = esft
    DO i=1, 3*nnos
        IF (restricoes(i)) THEN
            DO j=1, 3*nnos
                IF (i == j) THEN
                    rest_restrito(i,i) = 1
                ELSE
                    rest_restrito(i,j) = 0
                    rest_restrito(j,i) = 0
                ENDIF
            ENDDO
            esft_restrito(i) = 0
        ENDIF
    ENDDO
        
    ! Imprime matriz de rigidez da estrutura
    WRITE(rigt, '(a1,i4,a6)') '(', 3*nnos, 'f16.3)'
    WRITE(f2, '(A)') 'MATRIZ DE RIGIDEZ GLOBAL DA ESTRUTURA'
    WRITE(f2, rigt) rest
     
    WRITE(f2, *)
    
    ! Imprime matriz de rigidez da estrutura com restrições
    WRITE(rigt, '(a1,i4,a6)') '(', 3*nnos, 'f16.3)'
    WRITE(f2, '(A)') 'MATRIZ DE RIGIDEZ GLOBAL DA ESTRUTURA COM RESTRICOES'
    WRITE(f2, rigt) rest_restrito
     
    WRITE(f2, *)
    
    ! Imprime vetor de cargas globais dos elementos
    WRITE(f2, '(A)') 'VETOR DE SOLICITACOES GLOBAIS DOS ELEMENTOS'
    WRITE(f2, 60) esfg
    
    WRITE(f2, *)
    ! Imprime vetor de cargas globais totais [esft] = [esfg] + [esfn]
    WRITE(f2, '(A)') 'VETOR DE CARGAS GLOBAIS'
    WRITE(f2, 60) esft
    
    WRITE(f2, *)
    ! Imprime vetor de cargas globais totais com restrições [esft] = [esfg] + [esfn]
    WRITE(f2, '(A)') 'VETOR DE CARGAS GLOBAIS COM RESTRICOES'
    WRITE(f2, 60) esft_restrito
    
    ! Solucionar o sistema linear [K]{u} = {f}
    CALL DLSLRGa (3*nnos, 3*nnos, rest_restrito, esft_restrito, 1, dglo)
    
    ! Imprimir deslocamentos globais
    WRITE(f2, *)
    WRITE(f2, '(A)') 'VETOR DE DESLOCAMENTOS GLOBAIS'
    WRITE(f2, 60) dglo
    WRITE(f2, *)
    
    maux = 0.0
    ! Calcular esforços locais
    
    WRITE(f2, '(a)') 'SOLUCAO'
    WRITE(f2, *)
    
    DO i=1, nele
        WRITE(f2, '(a9,i3)') 'ELEMENTO ', i
        ! Endereçar deslocamentos globais de cada elemento
        DO j=-2, 0
            noj = 3 * conf_ele(i,1) + j
            nok = 3 * conf_ele(i,2) + j
            vaux_dgl(3+j) = dglo(noj)
            vaux_dgl(6+j) = dglo(nok)
        ENDDO
        
        ! Endereçar solicitações locais
        DO j=1, 6
            vaux_slo(j) = maux_slo(j,i)
        ENDDO
        
        ! Imprimir vetor de deslocamentos globais do elemento i
        WRITE(f2,*)
        WRITE(f2, '(a43,i3)') 'VETOR DE DESLOCAMENTOS GLOBAIS DO ELEMENTO ', i
        WRITE(f2, 60) vaux_dgl
        
        ! Montar matriz de rotação do elemento i
        DO j=1, 6
            DO k=1, 6
                maux_rot(j,k) = vrot(j,k,i)
            ENDDO
        ENDDO
        
        ! Calcular deslocamentos locais [rot]{dglo} = {dloc}
        vaux_dlo = MATMUL(maux_rot,vaux_dgl)
        
        ! Imprimir vetor de deslocamentos locais do elemento i
        WRITE(f2,*)
        WRITE(f2, '(a42,i3)') 'VETOR DE DESLOCAMENTOS LOCAIS DO ELEMENTO ', i
        WRITE(f2, 60) vaux_dlo
        
        ! Montar matriz de rigidez do elemento i
        DO j=1, 6
            DO k=1, 6
                maux_rlo(j,k) = vrig(j,k,i)
            ENDDO
        ENDDO
        
        ! Imprimir matriz de rigidez local do elemento
        WRITE(f2,*)
        WRITE(f2, '(a30,i3)') 'MATRIZ DE RIGIDEZ DO ELEMENTO ', i
        WRITE(f2, 55) maux_rlo
        
        ! Calcular esforços locais [rloc]{dloc} + {sloc} = {esfl}
        vaux_elo = MATMUL(maux_rlo,vaux_dlo) - vaux_slo
        
        ! Imprimir esforços locais
        WRITE(f2, *)
        WRITE(f2, '(a38, i3)') 'VETOR DE ESFORCOS LOCAIS NO ELEMENTO ', i
        WRITE(f2, '(es16.3)') vaux_elo
        
        WRITE(f2, '(A)') '                      No inicial        No final'
        WRITE(f2, '(a16,2es16.3)') ' Esf.Normal (N) ', vaux_elo(1), vaux_elo(4)
        WRITE(f2, '(a16,2es16.3)') 'Esf.Cortante (N)', vaux_elo(2), vaux_elo(5)
        WRITE(f2, '(a16,2es16.3)') 'Mom.Fletor (N*m)', vaux_elo(3), vaux_elo(6)
        WRITE(f2, *)
        
    ENDDO
    
    CLOSE(f1)
    CLOSE(f2)
         
51  FORMAT(i6,2i12,3x,2es16.3,5es15.3) ! Elem NoIn NoFi MEle Larg Altu
52  FORMAT(1i6,2x,2f12.3) ! No X Y
53  FORMAT(a8,i2)
54  FORMAT(es12.2,3x,f12.3,4x,f12.3,4x,es13.3,3x,es13.3,2x,es16.3) ! Comp Cose Seno Area MIne MEla
55  FORMAT(6f16.1)
56  FORMAT(a30,i2)
57  FORMAT(a41,i2) 
58  FORMAT(a37,i2)
59  FORMAT(3f16.1)
60  FORMAT(es16.3)    
    
END PROGRAM estruturas