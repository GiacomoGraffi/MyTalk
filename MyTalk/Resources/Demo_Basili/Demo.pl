:- op(300,xfy,&).

:- ensure_loaded('./ontology.pl').  % lIBRERIE CONCETTUALI
:- ensure_loaded('./ExampleNPPP.pl').  % lIBRERIE Grammaticali

?- use_module(library(system)).
?- use_module(library(lists)).


/* ******************** */
example(0, ['Giuseppe', corre]).
example(1, ['Giuseppe', usa, il, prolog]).
example(2, ['Giuseppe', usa, il, linguaggio, 'C']).
example(3, ['Giuseppe', diede, il, libro, a, 'Mario']).
example(4, ['Giuseppe', corre,in, 'Gennaio']).
example(5, ['Giuseppe', diede, il, libro, di, 'Mario',allo,zio]).
example(6, ['Giuseppe', diede, il, libro, a, 'Mario',in, campagna]).
example(7, ['Mario', diede, il, libro, a, 'Giuseppe', in, campagna]).
example(8, ['Giuseppe', diede, il, libro, a, 'Mario',in, 'Gennaio']).
example(9, [lo, zio, di, 'Mario',corre]).
example(10, [lo, zio, di, 'Mario',diede, a,'Giuseppe',il, libro, in, 'Gennaio']).
example(11, [lo, zio, di, 'Mario',corre, da,'Roma',a,'Ostia Antica',in, 'Gennaio']).
example(12, [il, libro, di, 'Mario',tratta, il, prolog]).
example(13, [il, libro,tratta,di,storia,a,'Gennaio' ]).

example(14, [lo,zio,di,'Mario',diede,il,libro,di,geografia,a,'Giuseppe',a,'Marzo']).
example(15, [lo,zio,di,'Mario',usa,il,libro,di,geografia,con,'Giuseppe',a,'Marzo']).
example(16,  [il,saggio,sul,tavolo,tratta,del,libro,di,geografia,per,'Mario']).


quali_esempi :-
	nl,
	example(N, Sent),
	write('['),write(N),write(']  '), st_frase(Sent), nl,
	fail.
quali_esempi.

first_msg :- nl,
	write('To run the system on the above examples please type: \n'),
	write('  qsem_an(N)\n'), 
	write('where N is a valid example identifier and FL will express the logical form corresponding to sentence N\n'),
	write('Unfortunately, new sentences are not supported.\n').
	
qsem_an(N) :- sem_an(N, _).
sem_an(N, FL) :-
	quali_esempi,
	example(N, Sent),
/*	s(FL, Sent, []), */
    phrase(s(FL), Sent, []),
	write('The logic form of the example '),write(N),nl,
	tab(2),st_frase(Sent),nl,
	write(' is:  \n\t'),
	writeq(FL),
  write('\n').
sem_an(N, _) :-
	\+ example(N, _),
	!,
	write('Wrong identifier for the example!!\n\n').

sem_an(N,_) :-
	write('No result:  the analysis of the example : '), write(N),	
	write(' has failed!\n'),
	write('\nPlease verify the Grammatical Lexicon or the Ontology. \n').


st_frase([]).
st_frase([A|Rest]) :-
	write(A),write(' '),
	!,
	st_frase(Rest).
	
	
header :-
  	write('\nMini Semantic Interpreter: version 1.1. No with pretty printing is supported.\n'),
  quali_esempi,
  first_msg.

?-header.	
