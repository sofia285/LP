:-[codigo_comum].

% Sofia Paiva   102835   sofia.paiva@tecnico.ulisboa.pt


% 2.1 **********************************************************************
% extrai_ilhas_linha(N_L,Linha,Ilha)
% Funcao que recebe um inteiro positivo (N_L), uma lista que 
% corresponde a uma lista (Linha) e uma lista (Ilha) que devolve 
% as ilhas ordenadas na esquerda para a direita.

extrai_ilhas_linha(N_L,Linha,Ilha) :-
    extrai_ilhas_linha(N_L,Linha,[],Ilha,0).

extrai_ilhas_linha(_,[],_,[],_).

%Verifica se o numero de pontes e' superior a 0
extrai_ilhas_linha(N_L,[P|R],Aux_Ilha,[H|T],Counter) :-
    P > 0,
    Counter_2 is Counter + 1,
    append(Aux_Ilha,ilha(P,(N_L,Counter_2)), H),
    extrai_ilhas_linha(N_L,R,Aux_Ilha,T,Counter_2).
    
%Verifica se o numero de linhas e' igual a 0
extrai_ilhas_linha(N_L,[P|R],Aux_Ilha,Ilha,Counter) :-
    P = 0,
    Counter_2 is Counter + 1,
    extrai_ilhas_linha(N_L,R,Aux_Ilha,Ilha,Counter_2).


% 2.2 **********************************************************************
% ilhas(Puz, Ilhas)
% Funcao que recebe um puzzle (Puz) e Ilhas devolve a lista
% ordenada das ilhas do puzzle.
% Utiliza a funcao extrai_ilhas_linha (2.1).

ilhas(Puz,Ilhas):-
    ilhas(Puz,Aux_Ilha,0),
    append(Aux_Ilha,Ilhas).
    
ilhas([],[],_).

ilhas([P|R],[H|T],Counter):-
    Counter_2 is Counter + 1,
    extrai_ilhas_linha(Counter_2,P,H),
    ilhas(R,T,Counter_2).


% 2.3 **********************************************************************
% viziznhas(Ilhas,Ilha,Viziznhas)
% Funcao que recebe uma lista (Ilhas) de ilhas do puzzle e uma dessas ilhas
% (Ilha). Devolve a lista das ilhas ordenadas (Viziznhas).

vizinhas(Ilhas,Ilha,Vizinhas):-
    vizinhas(Ilhas,Ilha,[],[],[],[],Vizinhas).

vizinhas([],_,Esq,Dir,Cim,Bai,Vizinhas) :-
    append(Esq,Cim,Aux),
    append(Bai,Dir,Aux2),
    append(Aux,Aux2,Vizinhas).

%Verifica se e' a ilha original
vizinhas([ilha(_,(L,C))|R],ilha(_,(L2,C2)),Esq,Dir,Cim,Bai,Vizinhas):-
    L=L2,
    C=C2,
    vizinhas(R,ilha(_,(L2,C2)),Esq,Dir,Cim,Bai,Vizinhas).

%Verifica se esta' 'a esquerda da ilha original
vizinhas([ilha(N_L,(L,C))|R],ilha(_,(L2,C2)),_,Dir,Cim,Bai,Vizinhas):-
    (L<L2,C=C2),
    vizinhas(R,ilha(_,(L2,C2)),[ilha(N_L,(L,C))],Dir,Cim,Bai,Vizinhas).

%Verifica se esta' 'a direita da ilha original
vizinhas([ilha(N_L,(L,C))|R],ilha(_,(L2,C2)),Esq,Dir,Cim,Bai,Vizinhas):-
    (L>L2,C=C2),
    Dir = [],
    vizinhas(R,ilha(_,(L2,C2)),Esq,[ilha(N_L,(L,C))],Cim,Bai,Vizinhas).

%Verifica se esta' a cima da ilha original
vizinhas([ilha(N_L,(L,C))|R],ilha(_,(L2,C2)),Esq,Dir,_,Bai,Vizinhas):-
    (L=L2,C<C2),
    vizinhas(R,ilha(_,(L2,C2)),Esq,Dir,[ilha(N_L,(L,C))],Bai,Vizinhas).

%Verifica se esta' a baixo da ilha original
vizinhas([ilha(N_L,(L,C))|R],ilha(_,(L2,C2)),Esq,Dir,Cim,Bai,Vizinhas):-
    (L=L2,C>C2),
    Bai = [],
    vizinhas(R,ilha(_,(L2,C2)),Esq,Dir,Cim,[ilha(N_L,(L,C))],Vizinhas).

%No caso de nao estar na mesma coluna ou linha da ilha original entra aqui
vizinhas([_|R],Ilha,Esq,Dir,Cim,Bai,Vizinhas):-
    vizinhas(R,Ilha,Esq,Dir,Cim,Bai,Vizinhas).

 
% 2.4 **********************************************************************
% estado(Ilhas,Estado)
% Funcao que recebe uma lista de ilhas no puzzle (Ilhas) e devolve 
% uma lista ordenada de cada entrada das ilhas (Estado).
% Utiliza a funcao vizinhas (2.3).

estado(Ilhas,Estado):-
    estado(Ilhas,Ilhas,Estado).

estado([],_,[]).

estado([P|R],Aux_Ilha,[H|T]):-
    vizinhas(Aux_Ilha,P,Vizinhas),
    append([P],[Vizinhas],Aux_Estado),
    append(Aux_Estado,[[]],H),
    estado(R,Aux_Ilha,T).


% 2.5 **********************************************************************
% posicoes_entre(Pos1,Pos2,Posicoes)
% Funcao que recebe duas posicoes (Pos1 e Pos2) e devolve uma lista 
% de posicoes entre elas.

% No caso da posicao 2 ser superior a' posicao 1 entao entra aqui
posicoes_entre((L1,C1),(L2,C2),Posicoes):-
    L2 >= L1,
    C2 >= C1,
    posicoes_entre((L1,C1),(L2,C2),L1,C1,[],Posicoes).

% No caso da posicao 1 ser superior a' posicao 2 entao entra aqui
posicoes_entre((L1,C1),(L2,C2),Posicoes):-
    (C1 > C2;(C2 >= C1,L1 > L2)),
    posicoes_entre((L2,C2),(L1,C1),L2,C2,[],Posicoes).

posicoes_entre((_,_),(L2,C2),L2,C2,[],_).

% Verifica se as posicoes estao na mesma linha e se deve parar de adicionar posicoes
posicoes_entre((L1,C1),(L2,C2),Pos_L,Pos_C,Aux_Pos,[]):-
    L1 = L2,
    New_Pos_C is Pos_C + 1,
    New_Pos_C = C2,
    posicoes_entre((L1,C1),(L2,C2),Pos_L,New_Pos_C,Aux_Pos,[]).
    
% Verifica se as posicoes estao na mesma linha
posicoes_entre((L1,C1),(L2,C2),Pos_L,Pos_C,Aux_Pos,[H|T]):-
    L1 = L2,
    New_Pos_C is Pos_C + 1,
    append(Aux_Pos,(L1,New_Pos_C),H),
    posicoes_entre((L1,C1),(L2,C2),Pos_L,New_Pos_C,Aux_Pos,T).

%Verifica se as posicoes estao na mesma coluna e se deve parar de adicionar posicoes
posicoes_entre((L1,C1),(L2,C2),Pos_L,Pos_C,Aux_Pos,[]):-
    C1 = C2,
    New_Pos_L is Pos_L + 1,
    New_Pos_L = L2,
    posicoes_entre((L1,C1),(L2,C2),New_Pos_L,Pos_C,Aux_Pos,[]).
    
% Verifica se as posicoes estao na mesma coluna
posicoes_entre((L1,C1),(L2,C2),Pos_L,Pos_C,Aux_Pos,[H|T]):-
    C1 = C2,
    New_Pos_L is Pos_L + 1,
    append(Aux_Pos,(New_Pos_L,C1),H),
    posicoes_entre((L1,C1),(L2,C2),New_Pos_L,Pos_C,Aux_Pos,T).


% 2.6 **********************************************************************
% cria_ponte(Pos1,Pos2,Ponte)
% Funcao que recebe duas posicoes (Pos1 e Pos2) e devolve a ponte
% criada entre essas duas posicoes (Ponte).

% Verifica se a segunda posicao vem depois da primeira, no caso de ser verdade entra aqui
cria_ponte((L1,C1),(L2,C2),Ponte):-
    L2 >= L1,
    C2 >= C1,
    Ponte = ponte((L1,C1),(L2,C2)).

% No caso de nao entrar na anterior, entra nesta
cria_ponte((L1,C1),(L2,C2),Ponte):-
    (C1 > C2;(C2 >= C1,L1 > L2)),
    Ponte = ponte((L2,C2),(L1,C1)).


% 2.7 **********************************************************************
% caminho_livre(Pos1,Pos2,Posicoes,Ilha,Vizinha)
% Funcao que recebe duas posicoes (Pos1 e Pos2), uma lista de
% posicoes que se encontram entre Pos1 e Pos2, uma ilha (Ilha) e 
% uma vizinha dela (Vizinha).
% Utiliza a funcao posicoes_entre (2.5) e a cria_pontes (2.6).

caminho_livre(Pos1,Pos2,Posicoes,ilha(_,(L1,C1)),ilha(_,(L2,C2))):-
    posicoes_entre((L1,C1),(L2,C2),Pos_entre),
    !,
    caminho_livre(Pos1,Pos2,Posicoes,Pos_entre,ilha(_,(L1,C1)),ilha(_,(L2,C2))).

caminho_livre(_,_,[],_,_,_).

caminho_livre((L3,C3),(L4,C4),_,_,ilha(_,(L1,C1)),ilha(_,(L2,C2))):-
    cria_ponte((L3,C3),(L4,C4),Ponte),
    cria_ponte((L1,C1),(L2,C2),Ponte_2),
    Ponte = Ponte_2,
    caminho_livre((L3,C3),(L4,C4),_,[],ilha(_,(L1,C1)),ilha(_,(L2,C2))).
    
caminho_livre(Pos1,Pos2,[P|R],Pos_entre,ilha(_,(L1,C1)),ilha(_,(L2,C2))):-
    findall(X,(member(X,Pos_entre),X = P),Pos_igual),
    length(Pos_igual,Comp),
    Comp = 0,
    caminho_livre(Pos1,Pos2,R,Pos_entre,ilha(_,(L1,C1)),ilha(_,(L2,C2))).


% 2.8 **********************************************************************
% actualiza_vizinhas_entrada(Pos1,Pos2,Posicoes,Entrada,Nova_Entrada)
% Funcao que recebe duas posicoes (Pos1 e Pos2) que irao passar a ter uma 
% ponte entre elas e uma lista de posicoes entre as mesmas (Posicoes).
% Recebe tambem uma entrada (Entrada) e devolve uma nova_entrada 
% (Nova_Entrada) que igual 'a entrada exceto no que diz respeito 'a lista 
% das ilhas vizinhas, esta deve ser actualizada.

actualiza_vizinhas_entrada(Pos1,Pos2,Posicoes,[Ilha,Vizinhas,Lista],Nova_Entrada):-
    actualiza_vizinhas_entrada(Pos1,Pos2,Posicoes,[Ilha,Vizinhas,Lista],[],Ilha,Vizinhas,Aux),
    append([Ilha],[Aux],Aux2),
    append(Aux2,[[]],Nova_Entrada).

actualiza_vizinhas_entrada(_,_,_,_,[],_,[],[]).

% Verifica se o caminho entre duas ilhas esta' livre, no caso de estar entra aqui
actualiza_vizinhas_entrada((L1,C1),(L2,C2),Posicoes,Entrada,Aux_I,Ilha,[P|R],[H|T]):-
    caminho_livre((L1,C1),(L2,C2),Posicoes,Ilha,P),
    append(Aux_I,P,H),
    actualiza_vizinhas_entrada((L1,C1),(L2,C2),Posicoes,Entrada,Aux_I,Ilha,R,T).

% No caso do caminho entre as ilhas nao estar livre, entra aqui
actualiza_vizinhas_entrada((L1,C1),(L2,C2),Posicoes,Entrada,Aux_I,Ilha,[_|R],Aux):-
    actualiza_vizinhas_entrada((L1,C1),(L2,C2),Posicoes,Entrada,Aux_I,Ilha,R,Aux).
    

% 2.9 **********************************************************************    
% actualiza_vizinhas_apos_ponte(Estado,Pos1,Pos2,Novo_Estado)
% Funcao que recebe um estado (Estado), duas posicoes (Pos1 e Pos 2) entre
% as quais foi adicionada uma ponte e devolve uma lista (Novo_Estado)
%  que e' equivalente ao estado so' que com as ilhas vizinhas atualizadas.
% Utiliza a funcao posicoes_entre (2.5) actualiza_vizinhas_entrada (2.8).

actualiza_vizinhas_apos_pontes(Estado,Pos1,Pos2,Novo_Estado):-
    posicoes_entre(Pos1,Pos2,Pos_entre),
    actualiza_vizinhas_apos_pontes(Estado,Pos1,Pos2,Pos_entre,[],Novo_Estado).

actualiza_vizinhas_apos_pontes([],_,_,_,[],[]).

actualiza_vizinhas_apos_pontes([P|R],Pos1,Pos2,Pos_entre,Aux_E,[H|T]):-
    actualiza_vizinhas_entrada(Pos1,Pos2,Pos_entre,P,Nova_Entrada),
    append(Aux_E,Nova_Entrada,H),
    actualiza_vizinhas_apos_pontes(R,Pos1,Pos2,Pos_entre,Aux_E,T).
   

% 2.10 *********************************************************************
% ilhas_terminadas(Estado,Ilhas_Term)
% Funcao que recebe um estado (Estado) e ilhas terminadas (Ilhas_Term)
% e' a lista das ilhas cujo numero de pontes e' diferente de 'X'.

ilhas_terminadas(Estado,Ilhas_Term):-
    ilhas_terminadas(Estado,[],Ilhas_Term).

ilhas_terminadas([],_,[]).

% Verifica se o numero de pontes da ilha e' igual a 'X'
ilhas_terminadas([[ilha(N_pontes,Pos), _, Pontes]|R],Aux_E,[H|T]):-
    N_pontes \= 'X',
    length(Pontes,Comp_Pontes),
    N_pontes = Comp_Pontes,
    append(Aux_E,ilha(N_pontes,Pos),H),
    ilhas_terminadas(R,Aux_E,T).

ilhas_terminadas([_|R],Aux_E,Ilhas_Term):-
    ilhas_terminadas(R,Aux_E,Ilhas_Term).


% 2.11 *********************************************************************
%tira_ilhas_terminadas_entrada(Ilhas_Term,Entrada,Nova_Entrada)
%Funcao que recebe uma lista de ilhas terminadas (Ilhas_Term) e uma
% entrada (Entrada) e devolve uma nova entrada (Nova_Entrada) resultante
% de remover as ilhas terminadas da lista das ilhas vizinhas da entrada.

tira_ilhas_terminadas_entrada(Ilhas_Term,[Ilha,Vizinhas,Lista],Nova_Entrada):-
    tira_ilhas_terminadas_entrada(Ilhas_Term,[Ilha,Vizinhas,Lista],Vizinhas,[],Nova_Entrada).

tira_ilhas_terminadas_entrada(_,_,[],_,[_,[],_]).

tira_ilhas_terminadas_entrada(Ilhas_Term,[Ilha,_,_],[P|R],Aux_E,[Ilha,[H|T],[]]):-
    findall(X,(member(X,Ilhas_Term),P = X),Aux_I),
    length(Aux_I,Comp),
    Comp = 0,
    append(Aux_E,P,H),
    tira_ilhas_terminadas_entrada(Ilhas_Term,[Ilha,_,_],R,Aux_E,[Ilha,T,[]]).

tira_ilhas_terminadas_entrada(Ilhas_Term,[Ilha,_,_],[_|R],Aux_E,[Ilha,Vz,[]]):-
    tira_ilhas_terminadas_entrada(Ilhas_Term,[Ilha,_,_],R,Aux_E,[Ilha,Vz,[]]).


% 2.12 *********************************************************************
% tira_ilhas_terminadas(Estado,Ilhas_Term,Novo_Estado)
% Funcao que recebe um estado (Estado) e uma lista de ilhas terminadas
% (Ilhas_Term) e devolve uma lista (Novo_Estado) que resulta de aplicar a
% funcao tira_ilhas_terminadas_entrada (2.11) a cada entrada do estado.

tira_ilhas_terminadas(Estado,Ilhas_Term,Novo_Estado):-
    tira_ilhas_terminadas(Estado,Ilhas_Term,[],Novo_Estado).

tira_ilhas_terminadas([],_,_,[]).

tira_ilhas_terminadas([P|R],Ilhas_Term,Aux_E,[H|T]):-
    tira_ilhas_terminadas_entrada(Ilhas_Term,P,H),
    tira_ilhas_terminadas(R,Ilhas_Term,Aux_E,T).


% 2.13 *********************************************************************
% marca_ilhas_terminadas_entrada(Ilhas_Term,Entrada,Nova_Entrada)
% Funcao que recebe uma lista de ilhas terminadas (Ilhas_Term) e uma 
% entrada (Entrada). No caso de a ilha da entrada pertencer 'as ilhas
% terminadas entao a nova entrada (Nova_Entrada) corresponde 'a entrada so'
% que no lugar das pontes da ilha aparece um 'X'. No caso de a ilha nao 
% pertencer, entao a nova entrada (Nova_Entrada) e' igual 'a entrada anterior.

marca_ilhas_terminadas_entrada(Ilhas_Term,Entrada,Nova_Entrada):-
    marca_ilhas_terminadas_entrada(Ilhas_Term,Entrada,[],Nova_Entrada).

marca_ilhas_terminadas_entrada(_,[],_,_).

marca_ilhas_terminadas_entrada(Ilhas_Term,[ilha(P,(L,C)),Vz,_],Aux_E,Nova_Entrada):-
    findall(X,(member(X,Ilhas_Term),X == ilha(P,(L,C))),Aux_I),
    length(Aux_I,Comp),
    Comp \= 0,
    append(Aux_E,[ilha('X',(L,C)),Vz,[]],Nova_Entrada),
    marca_ilhas_terminadas_entrada(Ilhas_Term,[],Aux_E,Nova_Entrada).

marca_ilhas_terminadas_entrada(Ilhas_Term,[Ilha,Vz,_],Aux_E,Nova_Entrada):-
    findall(X,(member(X,Ilhas_Term),X == Ilha),Aux_I),
    length(Aux_I,Comp),
    Comp = 0,
    append(Aux_E,[Ilha,Vz,[]],Nova_Entrada),
    marca_ilhas_terminadas_entrada(Ilhas_Term,[],Aux_E,Nova_Entrada).


% 2.14 *********************************************************************
% marca_ilhas_terminadas(Estado,Ilhas_Term,Novo_Estado)
% Funcao que recebe um estado (Estado) e uma lista de ilhas terminadas
% (Ilhas_Term). Devolve uma lista (Novo_Estado) que resulta de aplicar a
% funcao marca_ilhas_terminadas_entrada (2.13) a cada entrada do estado.

marca_ilhas_terminadas(Estado,Ilhas_Term,Novo_Estado):-
    marca_ilhas_terminadas(Estado,Ilhas_Term,[],Novo_Estado).

marca_ilhas_terminadas([],_,_,[]).

marca_ilhas_terminadas([P|R],Ilhas_Term,Aux_E,[H|T]):-
    marca_ilhas_terminadas_entrada(Ilhas_Term,P,Aux_I),
    append(Aux_E,Aux_I,H),
    marca_ilhas_terminadas(R,Ilhas_Term,Aux_E,T).


% 2.15 *********************************************************************
% trata_ilhas_terminadas(Estado,Novo_Estado)
% Funcao que recebe um estado (Estado) e devolve o estado (Novo_Estado) que
% resulta de aplicar a funcao tira_ilhas_terminadas (2.12) e marca_ilhas_terminadas (2.14)
% ao estado.

trata_ilhas_terminadas(Estado,Novo_Estado):-
    ilhas_terminadas(Estado,Ilhas_Term),
    trata_ilhas_terminadas(Estado,Ilhas_Term,[],Novo_Estado).

trata_ilhas_terminadas([],_,_,_).

trata_ilhas_terminadas(Estado,Ilhas_Term,Aux_E,Novo_Estado):-
    tira_ilhas_terminadas(Estado,Ilhas_Term,New_Estado),
    marca_ilhas_terminadas(New_Estado,Ilhas_Term,N_Estado),
    append(Aux_E,N_Estado,Novo_Estado),
    trata_ilhas_terminadas([],Ilhas_Term,Aux_E,Novo_Estado).


% 2.16 *********************************************************************
% junta_pontes(Estado,Num_Pontes,Ilha1,Ilha2,Novo_Estado)
% Funcao que recebe um estado (Estado) e duas ilhas (Ilha1 e Ilha2).
% Devolve um estado (Novo_Estado) que resulta da adicao do numero de pontes
% (Num_Pontes) entre a primeira e a segunda ilha. Utiliza a funcao auxiliar
% junta_pontes_aux.

junta_pontes(Estado,Num_Pontes,ilha(N_P,(L1,C1)),ilha(N_P2,(L2,C2)),Novo_Estado):-
    cria_ponte((L1,C1),(L2,C2),Ponte),
    actualiza_vizinhas_apos_pontes(Estado,(L1,C1),(L2,C2),New_Estado),
    junta_pontes_aux(Num_Pontes,Ponte,[],Pontes),
    junta_pontes(New_Estado,Num_Pontes,ilha(N_P,(L1,C1)),ilha(N_P2,(L2,C2)),Pontes,[],Aux_Estado,Novo_Estado),
    trata_ilhas_terminadas(Aux_Estado,Novo_Estado).

junta_pontes([],_,_,_,_,_,[],_).

junta_pontes([[ilha(N_P,(L,C)),Vz,_]|R],Num_Pontes,Ilha1,Ilha2,Pontes,Aux_E,[H|T],_):-
    (ilha(N_P,(L,C)) = Ilha1; ilha(N_P,(L,C)) = Ilha2),
    N_P = Num_Pontes,
    append(Aux_E,[ilha('X',(L,C)),Vz,Pontes],H),
    junta_pontes(R,Num_Pontes,Ilha1,Ilha2,Pontes,Aux_E,T,_).

junta_pontes([[ilha(N_P,(L,C)),Vz,_]|R],Num_Pontes,Ilha1,Ilha2,Pontes,Aux_E,[H|T],_):-
    (ilha(N_P,(L,C)) = Ilha1; ilha(N_P,(L,C)) = Ilha2),
    N_P \= Num_Pontes,
    append(Aux_E,[ilha(N_P,(L,C)),Vz,Pontes],H),
    junta_pontes(R,Num_Pontes,Ilha1,Ilha2,Pontes,Aux_E,T,_).

junta_pontes([Ilha|R],Num_Pontes,Ilha1,Ilha2,Pontes,Aux_E,[H|T],_):-
    append(Aux_E,Ilha,H),
    junta_pontes(R,Num_Pontes,Ilha1,Ilha2,Pontes,Aux_E,T,_).
    
junta_pontes_aux(0,_,_,[]).

junta_pontes_aux(Num_Pontes,Ponte,Aux_E,[H|T]):-
    append(Aux_E,Ponte,H),
    Num_Pontes_2 is Num_Pontes - 1,
    junta_pontes_aux(Num_Pontes_2,Ponte,Aux_E,T).