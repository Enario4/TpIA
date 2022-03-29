%*******************************************************************************
%                                    AETOILE
%*******************************************************************************

/*
Rappels sur l'algorithme
 
- structures de donnees principales = 2 ensembles : P (etat pendants) et Q (etats clos)
- P est dedouble en 2 arbres binaires de recherche equilibres (AVL) : Pf et Pu
 
   Pf est l'ensemble des etats pendants (pending states), ordonnes selon
   f croissante (h croissante en cas d'egalite de f). Il permet de trouver
   rapidement le prochain etat a developper (celui qui a f(U) minimum).
   
   Pu est le meme ensemble mais ordonne lexicographiquement (selon la donnee de
   l'etat). Il permet de retrouver facilement n'importe quel etat pendant

   On gere les 2 ensembles de fa�on synchronisee : chaque fois qu'on modifie
   (ajout ou retrait d'un etat dans Pf) on fait la meme chose dans Pu.

   Q est l'ensemble des etats deja developpes. Comme Pu, il permet de retrouver
   facilement un etat par la donnee de sa situation.
   Q est modelise par un seul arbre binaire de recherche equilibre.

Predicat principal de l'algorithme :

   aetoile(Pf,Pu,Q)

   - reussit si Pf est vide ou bien contient un etat minimum terminal
   - sinon on prend un etat minimum U, on genere chaque successeur S et les valeurs g(S) et h(S)
	 et pour chacun
		si S appartient a Q, on l'oublie
		si S appartient a Ps (etat deja rencontre), on compare
			g(S)+h(S) avec la valeur deja calculee pour f(S)
			si g(S)+h(S) < f(S) on reclasse S dans Pf avec les nouvelles valeurs
				g et f 
			sinon on ne touche pas a Pf
		si S est entierement nouveau on l'insere dans Pf et dans Ps
	- appelle recursivement etoile avec les nouvelles valeurs NewPF, NewPs, NewQs

*/

%*******************************************************************************

:- ['avl.pl'].       % predicats pour gerer des arbres bin. de recherche   
:- ['taquin.pl'].    % predicats definissant le systeme a etudier

%*******************************************************************************

main :-
	% initialisations Pf, Pu et Q 
	%final_state(Ini),
	initial_state(Ini),
	heuristique2(Ini,H0),
	G0 is 0,
	F0 is H0+G0,
	Data=[F0,H0,G0],

	empty(Pfi),
	empty(Pui),
	empty(Q),
	insert([Data,Ini],Pfi,Pf),
	insert([Ini,Data,nil,nil],Pui,Pu),
	write('\n'),
	write(Data),
	% lancement de Aetoile
	aetoile(Pf,Pu,Q).

%*******************************************************************************
affiche_solution(Q):-
	write('	affiche sol'),
	final_state(Fin),
	affiche_solution(Q,Fin).

affiche_solution(_,nil):-
	write2('	Fin de solution').

affiche_solution(Q,Arr):-
	suppress([Arr,_,Pere,A],Q,Q2),
	affiche_solution(Q2,Pere),
	write2('\n'),
	write2(A),
	write2('\n').

write2(nil).
write2(E):-
	write(E).



expand(U,Gu,Succs):-

	findall([S, [Fs,Hs,Gs], U, A], 
				(rule(A,1,U,S),
				Gs is Gu+1,
				heuristique2(S,Hs), 
				Fs is Hs+Gs), 
				Succs).

loop_successors(Pfs,Pus,_,[],Pfs,Pus).

loop_successors(Pf,Pu,Q,[[S,[F,H,G],_,_]|RS],Pff,Puf):-
	belongs([S,_,_,_],Q),
	%write('lp1'),
	loop_successors(Pf,Pu,Q,RS,Pff,Puf).

loop_successors(Pf,Pu,Q,[[S,[Fs,Hs,_],_,_]|RS],Pff,Puf):-
	not(belongs([S,_,_,_],Q)),
	belongs([S,[F,H,_],_,_],Pu),
	(F<Fs ; F==Fs),
	%write('lp2'),
	loop_successors(Pf,Pu,Q,RS,Pff,Puf).

loop_successors(Pf,Pu,Q,[[S,[Fs,Hs,Gs],Pere,A]|RS],Pff,Puf):-
	not(belongs([S,_,_,_],Q)),
	belongs([S,[F,H,_],_,_],Pu),
	(F>Fs ; F==Fs),
	suppress([S,[F,_,_],_,_],Pu,Pus),
	suppress([[F,_,_],S],Pf,Pfs),
	insert([S,[Fs,Hs,Gs],Pere,A],Pus,Puf),
	insert([[Fs,Hs,Gs],S],Pfs,Pff),
	%write('lp3'),
	loop_successors(Pff,Puf,Q,RS,Pff,Puf).

loop_successors(Pf,Pu,Q,[[S,[Fs,Hs,Gs],Pere,A]|RS],Pff,Puf):-
	not(belongs([S,_,_,_],Q)),
	not(belongs([S,_,_,_],Pu)),
	insert([S,[Fs,Hs,Gs],Pere,A],Pu,Pus),
	insert([[Fs,Hs,Gs],S],Pf,Pfs),
	%write('lp4'),
	loop_successors(Pfs,Pus,Q,RS,Pff,Puf).




aetoile(nil,nil,_):- 
	write('PAS de SOLUTION : L ETAT FINAL N EST PAS ATTEIGNABLE ! ').

aetoile(Pf,Pu,Q):-
	final_state(Fin),
	suppress_min([[F,H,G],Fin],Pf,_),
	suppress([Fin,[F,H,G],Pere,A],Pu,_),
	insert([Fin,[F,H,G],Pere,A],Q,Qfinal),
	write('aetoile Fin'),
	affiche_solution(Qfinal).

aetoile(Pf,Pu,Q):-
%on enlève le nœud de Pf correspondant à l’état U à développer (celui de valeur F minimale) et on enlève aussi le nœud
%frère associé dans Pu 
	suppress_min([[F,H,G],U],Pf,Pf2),
	suppress([U,[F,H,G],Pere,A],Pu,Pu2),
	expand(U,G,Succ),
	loop_successors(Pf2,Pu2,Q,Succ,Pff,Puf),
	%write('\n'),
	%write(Pff),
	%write('\n'),

	insert([U,[F,H,G],Pere,A],Q,Q2),
	aetoile(Pff,Puf,Q2).

	


%Term1 @< Term2 est vrai si Term1 précède Term2 dans l'ordre lexicographique,
	

	