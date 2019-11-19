/* *********** Esercitazione di IA      ************************ *

Semantic Interpreter
Author: R. Basili
a.a. 2001-2008

Version 1.1: 
Targeted Phenomena
 - Proper Nouns 
 - Non Argumental Verbal post-modifiers 
 - Interpretation of the semantic relations realized throug  prepositional phrases
 * ************************************************************* */

:- op(300,xfy,&).
:- ensure_loaded('./ontology.pl').  % lIBRERIE CONCETTUALI



/* ************************************************************* 
   Grammar
 ************************************************************* */

%(1 - not working)
% s(SSem & Mod) --> np(SubjSem&Mod), vp(VerbPhraseSem),
%            {betareduce(VerbPhraseSem,SubjSem,SSem)}.

%(2 - extended form)
% s(SSem) --> np(SubjSem), vp(VerbPhraseSem),
%            {betareduce(VerbPhraseSem,SubjSem,SSem)}.

%(3 - partially executed, not inverted form)
%

s(SSem) --> np(SubjSem), vp(SubjSem^SSem).


%% pp   sono gli argomenti non verbali, opzionali (?)
%% vpk  è il kernel dei verbi
			
vp(VP & Mod) -->
           vpk(_, VP),
           pp(Mod).
vp(VP) -->
      vpk(_, VP).


%kernels of fully realized verbal phrases 

% 1 - come nell'esempio su (2): vp --> iv, v --> tv np. Distinzione tra mod/non mod.
vpk(_, VP) --> iv(VP).
vpk(_, VP_Sem & Mod) --> 
          tv(TV_Sem), 
          np(Arg & Mod),
          {betareduce(TV_Sem,Arg,VP_Sem)}.
vpk(_, VP_Sem) --> tv(TV_Sem), 
                   np(Arg),
                   {betareduce(TV_Sem,Arg,VP_Sem)}.

% 2 - aggiungo anche vp --> v, np, pp ; vp --> np, v, pp ; vp --> v, pp.

vpk(3/Pform, VP_Sem & Mod) --> 
        v(3/Pform,Z^Y1^Sem), 
        np(Y & Mod),     %NP complesso - possibilmente del tipo NPK PP
        pp(3/Pform,Z), {Z\=A&B}, 
        {betareduce(Z^Y1^Sem, Z, Y1^Sem),
         betareduce(  Y1^Sem, Y, VP_Sem)}.

vpk(3/Pform, VP_Sem & Mod) --> 
        v(3/Pform,Z^Y1^Sem), 
        np(Y), {Y\=A&B},      
	    pp(3/Pform,Z & Mod),%PP complesso - del tipo P NPK PP
        {betareduce(Z^Y1^Sem, Z, Y1^Sem),
         betareduce(  Y1^Sem, Y, VP_Sem)}.

vpk(3/Pform, VP_Sem & Mod1 & Mod2) --> 
        v(3/Pform, Z^Y1^Sem), 
        np(Y & Mod1),   %NP complesso - possibilmente del tipo NPK PP
        pp(3/Pform, Z & Mod2), %PP complesso - del tipo P NPK PP
        {betareduce(Z^Y1^Sem, Z, Y1^Sem),
         betareduce(  Y1^Sem, Y, VP_Sem)}.
		 
vpk(3/Pform, VP_Sem) --> 
        v(3/Pform,Z^Y1^Sem), 
        np(Y), {Y\=A&B},         
        pp(3/Pform,Z),
        {betareduce(Z^Y1^Sem, Z, Y1^Sem),
         betareduce(  Y1^Sem, Y, VP_Sem)}.
		 
vpk(3/Pform, VP_Sem & Mod) --> 
        v(3/Pform,Z^Y1^Sem), 
        pp(3/Pform,Z),{Z\=A&B}, 
        np(Y & Mod),     %NP complesso - possibilmente del tipo NPK PP
        {betareduce(Z^Y1^Sem, Z, Y1^Sem),
         betareduce(  Y1^Sem, Y, VP_Sem)}.

vpk(3/Pform, VP_Sem) --> 
        v(3/Pform,Z^Y1^Sem), 
        pp(3/Pform,Z), {Z\=A&B}, 
        np(Y), {Y\=A&B},        
        {betareduce(Z^Y1^Sem, Z, Y1^Sem),
         betareduce(  Y1^Sem, Y, VP_Sem)}.

		 
vpk(2/Pform, Sem & Mod) --> 
        v(2/Pform,Z^Sem), 
        pp(2/Pform, Z & Mod),  %PP complesso "del libro di storia"
        {betareduce(Z^Sem, Z, Sem)}.
vpk(2/Pform, Sem) --> 
        v(2/Pform,Z^Sem), 
        pp(2/Pform,Z),
		{Z \= (A & B),
         betareduce(Z^Sem, Z,Sem)}.

		
%----------------------------------------------------------------
%kernels of fully realized noun phrases 
np(Sem & Mod) -->
    npk(Sem),
    pp(np / Sem, Mod).

np(Sem) -->
    npk(Sem).

%----------------------------------------------------------------
%Argomenti Verbali preposizionali
%Form indicizza la relazione tra il lessico e le regole di attacco preposizionali
%Sem e' il semantic carrier del sintagma preposizionale (Head)
pp(Form, Sem)  --> 
            p(Form), 
            npk(Sem).

			%caso nominale - sequenze NP --> NPK PP

pp(np / PPHead_Sem, PPSem) -->
      p(np,Arg^PPHead_Sem^Expr),
      np(Arg),
      {pp_interpretation(Arg^PPHead_Sem^Expr, PPSem)}.

pp(2/_, Arg & Mod) -->
      p(2/_),
      np(Arg & Mod).
pp(2/_, Arg) -->
      p(2/_),
      np(Arg),
	  {Arg \= A&B}.
	  
pp(3/_, Arg & Mod) -->
      p(3/_),
      np(Arg & Mod).
pp(3/_, Arg) -->
      p(3/_),
      np(Arg),
	  {Arg \= A&B}.
	  
%----------------------------------------------------------------
%Modificatori preposizionali destri
%caso verbale - sequenze VP --> VPK PP
pp(Sem & Sem2) -->
      p(Arg^Expr),
      npk(Arg),
      {pp_interpretation(Arg^Expr, Sem)},
      pp(Sem2).


pp(Sem) -->
      p(Arg^Expr),
      np(Arg),
      {pp_interpretation(Arg^Expr, Sem)}.



/* Lexical Rules  *****************************************************

   Deal with:
- intransitive, transitive and ditransitive verbs 
- Proper Noun interpretations
- compound nominal (e.g. "il linguaggio Prolog" as a singleton noun)
 *********************************************************************** */

%----------------------------------------------------------------
% VERBS
tv(X^Y^usa(Y,X)) --> [usa].
tv(X^Y^compra(Y,X)) --> [compra].

tv(X^Y^trattare(Y,X)) --> [tratta].

iv(X^corre(X)) --> [corre]. 


v(2/di, Y^X^trattare(X,Y) ) --> 
        [tratta].
v(3/a, Z^Y^X^dare(X,Y,Z) ) --> 
        [diede].
v(3/da, Z^Y^X^comprare(X,Y,Z) )  --> 
        [compra].


%----------------------------------------------------------------
%NOUNS 

% Extra grammatical treatment of proper nouns as kernels of NPs
npk(PN) --> [PN],{np_is(PN,_)}.

%npk(giuseppe) --> ['Giuseppe'].
%npk(mario) --> ['Mario'].

npk(prolog) --> [il,prolog].
npk(X) --> [il, linguaggio, X].
npk(campagna) --> [campagna].
npk(zio) --> [lo,zio].
npk(zio) --> [zio].
npk(libro) --> [il,libro].
npk(saggio) --> [il,saggio].
npk(agosto) --> [agosto].
npk(storia) --> [storia].
npk(geografia) --> [geografia].
npk(marmo) --> [marmo].
npk(tavolo) --> [tavolo].


%----------------------------------------------------------------
/* Treatment of the prepositions */
%CASE 1: non argumental verbal post-modifiers
% - prepare the logical form of a prepositional phrase PP to be 
%   beta-reduced through its nominal filler
p(X^a(X,PPSem)) -->
        [a].
p(X^da(X,PPSem)) -->
        [da].
p(X^in(X,PPSem)) -->
        [in].
p(X^in(X,PPSem)) -->
        [nel].
p(X^con(X,PPSem)) -->
        [con].
p(X^di(X,PPSem)) -->
        [di].
p(X^di(X,PPSem)) -->
        [del].
p(X^per(X,PPSem)) -->
        [per].
p(X^su(X,PPSem)) -->
        [su].
p(X^su(X,PPSem)) -->
        [sul].
		

%----------------------------------------------------------------
%CASE 3: Argumental prepositional modifiers 
%No interpretation is needed as semantic relations here 
%are fully determined by the main verb 
p(3/a) -->
        [a].
p(3/a) -->
        [al].
p(3/a) -->
        [allo]. 
p(3/da) -->
        [da].
p(3/in) -->
        [in].
p(2/di) -->
        [di].
p(2/di) -->
        [del].
	
%----------------------------------------------------------------
%CASE 2: nominal post-modifiers - An example is "di" ("of")
p(np,X^Y^di(Y,X,PPSem)) -->
        [di].
p(np,X^Y^di(Y,X,PPSem)) -->
        [del].
p(np,X^Y^su(Y,X,PPSem)) -->
        [su].
p(np,X^Y^su(Y,X,PPSem)) -->
        [sul].
p(np,X^Y^per(Y,X,PPSem)) -->
        [per].
p(np,X^Y^in(Y,X,PPSem)) -->
        [in].
p(np,X^Y^in(Y,X,PPSem)) -->
        [nel].

	
/* *********** Grammatical Libraries ************************ */

%BETAREDUCE
%CASE 1: Noun phrases post-modified by PP :
%   - The argument Arg is complex, such as 
%               Arg & PostMod
%     so that the main predicate is given by Arg and a logical 
%     conjunction is generated with the semantic interpretation of the PP
%     as PostMod

betareduce(Arg^Expr, Arg2 & PostMod, Expr & PostMod) :-
          Arg = Arg2,
          !.


%CASE 2. Recursive treatment of verb post-modifiers introduced by a PP:
%   - the correct interpretatipon is the logical conjunction between 
%     the main predicate and the sequence of candidate (post)modifiers

betareduce(Pred1 & Modlist,Arg,Sem & Modlist) :-
          betareduce(Pred1,Arg,Sem).


%CASE 3: Basic \beta-reduction case

betareduce(Arg^Expr,Arg,Expr).

