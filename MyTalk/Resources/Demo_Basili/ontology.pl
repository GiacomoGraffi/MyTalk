% Toy Ontology for the Mini Semantic Interpreter 

% Mostly General Concepts
top(luogo).
top(tempo).
top(animate).
top(abstraction).
top(matter).

% IS_A Hierarchy
is_a(luogo_naturale, luogo).
is_a(luogo_artificiale,luogo).
is_a(oggetto,luogo_artificiale).
is_a(manufatto,oggetto).
is_a(campagna,luogo_naturale).
is_a(lattina,manufatto).
is_a(marmo,matter).

is_a(giorno,tempo).
is_a(mese,tempo).
is_a(anno,tempo).

is_a(persona,animate).
is_a(animale,animate).
is_a(parente,persona).
is_a(zio,parente).

is_a(disciplina, abstraction).


% Proper Noun Classes
is_a(person_np, persona).
is_a(artifact_np, manufatto).
is_a(location_np, luogo_naturale).
is_a(city_np, luogo_naturale).


is_a(year_np,anno).
is_a(day_np,giorno).
is_a(month_np,mese).

is_a(disciplina_np, disciplina).

% Instances/Individuals in the Ontology

%Person Names
np_is('Mario',person_np).
np_is(giuseppe,person_np).
np_is('Giuseppe',person_np).
np_is('Maria',person_np).
np_is('Fabio',person_np).

%Products/Artifacts
np_is(prolog,artifact_np).
np_is('C',artifact_np).
np_is(libro,artifact_np).
np_is(saggio,artifact_np).
np_is(tavolo,artifact_np).

%Locations
np_is('Roma', city_np).
np_is('Ostia Antica', city_np).

np_is('Lago di Bracciano', location_np).

%Time Expressions
np_is('Lunedi', day_np).
np_is('Martedi', day_np).
np_is('Mercoledi', day_np).
np_is('Giovedi', day_np).
np_is('Venerdi', day_np).
np_is('Sabato', day_np).
np_is('Domenica', day_np).

np_is('Gennaio', month_np).
np_is('Febbraio', month_np).
np_is('Marzo', month_np).
np_is('Aprile', month_np).
np_is('Maggio', month_np).
np_is('Giugno', month_np).
np_is('Luglio', month_np).
np_is('Agosto', month_np).
np_is('Settembre', month_np).
np_is('Ottobre', month_np).
np_is('Novembre', month_np).
np_is('Dicembre', month_np).


np_is('1999', year_np).

%Abstractions 
np_is(storia, disciplina_np).
np_is(geografia, disciplina_np).



% Transitive closure of the basic IS_A relation

%Treatment of Proper Nouns
tc_isa(X,Y) :-
   np_is(X, Classe_NP),
   !,
   tc_isa(Classe_NP,Y).

%General Case
tc_isa(X,Y) :- 
   is_a(X,Y),
   !.
tc_isa(X,Y) :- 
   is_a(X,Z),
   !,
   tc_isa(Z,Y).




% Interpretation of semantic relations
%        Arg^Expr 
% is such that
% Expr =.. [Preposition, PP_Arg, PPSemantics]
%
%vedi regole di p e pp (non argomentali)
% Caso 1: Complex PPs such as:
% PP PP
% (con (lo zio) (di Maria)) ==> 
%         (Zio & PArente(Zio,MAria))^con(Zio&PArente(Zio,MAria))
pp_interpretation( (Arg&Mod)^Expr, SemForm & Mod) :-
% project_head(Arg&Mod, Arg),
   Expr \= (A^B),
   Expr =.. [Prep | _],     % project_predicate(Expr, Prep),
   Proj_Expr =.. [Prep, Arg, SemForm],
   call(Proj_Expr).
pp_interpretation( Arg^Head^Expr, SemForm) :-
   Expr \= (A^B),
   call(Expr),
   Expr =.. [Prep, Head, Arg, SemForm].
pp_interpretation( Arg^Expr, SemForm) :-
   Expr \= (A^B),
   call(Expr),
   Expr =.. [Prep,Arg,SemForm].


%Interpretation Rules  for simple prepositions
%Verbal predicates (Monadic predicate case)
% Preposition: "a" (to)
a(X,a_luogo(X)) :-
     tc_isa(X,luogo).
%Simplistic rule
a(X,tempo(X)) :-
     ( 
     tc_isa(X,giorno)  %DAYS
     ; 
     tc_isa(X,mese) ), %MONTHS
     !.
% Preposition: "da" (from)
da(X,origine(X)) :-
     tc_isa(X,luogo).
	 
% Preposition: "in"
in(X,luogo(X)) :-
     tc_isa(X,luogo).
in(X,tempo(X)) :-
     tc_isa(X,mese), %MONTHS
     !.
	 
%Regola Grossolana
%in(X,tempo(X)) :-
%     tc_isa(X,tempo).
in(X,tempo(X)) :-
     ( 
       tc_isa(X,anno)    %YEARS
       ; 
       tc_isa(X,mese) ). %MONTHS

% Preposition: "con"
con(X,in_compagnia_di(X)) :-
     tc_isa(X,persona).
con(X,strumento(X)) :-
     tc_isa(X,matter).

% Preposition: "per"
per(X,beneficiario(X)) :-
     tc_isa(X,persona).
per(X,intervallo(X)) :-
     tc_isa(X,tempo).

% Preposition: "per"
su(X,topic(X)) :-
     tc_isa(X,persona).
su(X,topic(X)) :-
     tc_isa(X,tempo).
su(X,location(X)) :-
     tc_isa(X,manufatto).
	 
% Preposition: "per"
su(X,topic(X)) :-
     tc_isa(X,persona).
su(X,topic(X)) :-
     tc_isa(X,tempo).
su(X,location(X)) :-
     tc_isa(X,manufatto).
	 
%Simplistic rule
di(X,tempo(X)) :-
     ( 
     tc_isa(X,giorno)  %DAYS
     ; 
     tc_isa(X,mese) ),  %giorno,notte
     !.
	 
%di(_,_) :- fail.
 
%Rules for Nominal Post-Modifiers (Diadic predicates)
di(Head,ModNP,possessor(Head,ModNP)) :-
     tc_isa(Head,oggetto),
     tc_isa(ModNP,persona).
di(Head,ModNP,topic(Head,ModNP)) :-
     tc_isa(Head,oggetto),
     tc_isa(ModNP,disciplina).
di(Head,ModNP,parente(Head,ModNP)) :-
     tc_isa(Head,parente),
     tc_isa(ModNP,persona).
di(Head,ModNP,material(Head,ModNP)) :-
     tc_isa(Head,oggetto),
     tc_isa(ModNP,matter).
   
per(Head,ModNP,beneficiario(Head,ModNP)) :-
     tc_isa(Head,oggetto),
     tc_isa(ModNP,persona).

su(Head,ModNP,position(Head,ModNP)) :-
     tc_isa(Head,oggetto),
     tc_isa(ModNP,oggetto).
su(Head,ModNP,topic(Head,ModNP)) :-
     tc_isa(Head,artifact_np),
     tc_isa(ModNP,persona).
	 
a(Head,ModNP,manner(Head,ModNP)) :-
     tc_isa(Head,oggetto),
     tc_isa(ModNP,manner).

in(Head,ModNP,location(Head,ModNP)) :-
     tc_isa(Head,oggetto),
     tc_isa(ModNP,oggetto).

con(Head,ModNP,company(Head,ModNP)) :-
     tc_isa(Head,oggetto),
     tc_isa(ModNP,oggetto).

da(Head,ModNP,origine(Head,ModNP)) :-
     tc_isa(Head,persona),
     tc_isa(ModNP,luogo).