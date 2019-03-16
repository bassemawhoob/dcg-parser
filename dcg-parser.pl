%% Explanation adapted from Wikipedia.org (https://en.wikipedia.org/wiki/Definite_clause_grammar):
%% The main practical use of a DCG is to parse sentences of the given grammar, i.e. to construct a parse tree. 
%% This can be done by providing "extra arguments" to the functors in the DCG, like in the following rules:
%%    s(s(NP,VP)) --> noun_phrase(NP), verb_phrase(VP).
%%    noun_phrase(np(D,N)) --> det(D), noun(N).
%%    verb_phrase(vp(V,NP)) --> verb(V), noun_phrase(NP).
%%    det(d(the)) --> [the].
%%    det(d(a)) --> [a].
%%    noun(n(bat)) --> [bat].
%%    noun(n(cat)) --> [cat].
%%    verb(v(eats)) --> [eats].
%% One can now query the interpreter to yield a parse tree of any given sentence:
%% | ?- s(Parse_tree, [the,bat,eats,a,cat], []).
%% Parse_tree = s(np(d(the),n(bat)),vp(v(eats),np(d(a),n(cat)))) ? ;

%% The sample of words and their types have been retrieved from the Macmillan Dictionary:
%% https://www.macmillandictionary.com

%% The main structures of an English sentetence are explained and have been retrieved from:
%% https://www.slideshare.net/rubenzapatad/syntax-tree-diagrams (Slide 14)
%% https://faculty.washington.edu/wassink/LING200/lect14_syntax2.pdf

%% Sample tests
%% s(T,[the,young,boy,who,worked,for,the,old,man,pushed,and,stored,a,big,box,in,the,large,empty,room,after,school],[]).
%% s(T,[the,old,woman,and,the,old,man,gave,the,poor,young,man,a,white,envelope,in,the,shed,behind,the,building],[]).
%% s(T,[every,boy,quickly,climbed,some,big,tree,while,every,girl,secretly,watched,some,boy],[]).
%% s(T,[some,brilliant,students,and,many,professors,watched,and,admired,talented,lecturers,and,appreciated,bright,scientists,and,researchers],[]).
%% 

s(s(S, SS)) --> basic_statement(S), statement_rest(SS).
basic_statement(s(NP,VP)) --> noun_phrase(NP), verb_phrase(VP).
statement_rest(s(C, S, SS)) --> conjunction(C), basic_statement(S), statement_rest(SS).
statement_rest(s(_)) --> [].

noun_phrase(np(N)) --> noun(N).
noun_phrase(np(D,N)) --> det(D), noun(N).
noun_phrase(np(D,A,N)) --> det(D), adj_phrase(A), noun(N).
noun_phrase(np(D,A,N,CON,NP)) --> det(D), adj_phrase(A), noun(N), conjunction(CON), noun_phrase(NP).
noun_phrase(np(D,A,N,P)) --> det(D), adj_phrase(A), noun(N), pronoun_phrase(P).
noun_phrase(np(D,A,N,PP)) --> det(D), adj_phrase(A), noun(N), preposition_phrase(PP) .
noun_phrase(np(D,N,PP)) --> det(D), noun(N), preposition_phrase(PP).
noun_phrase(np(D,N,P)) --> det(D), noun(N), pronoun_phrase(P).
noun_phrase(np(D,A,N,NP)) --> det(D), adj_phrase(A), noun(N), noun_phrase(NP).
noun_phrase(np(D,A,N,P)) --> det(D), adj_phrase(A), noun(N), preposition_phrase(P).
noun_phrase(np(D,A,N,P)) --> det(D), adj_phrase(A), noun(N), adverb_phrase(P).
noun_phrase(np(D,N,P)) --> det(D), noun(N), adverb_phrase(P).
noun_phrase(np(D,A,N,NP)) --> det(D), adj_phrase(A), noun(N), noun_phrase(NP).
noun_phrase(np(A,N)) --> adj_phrase(A), noun(N).
noun_phrase(np(A,N,CON,VP)) --> adj_phrase(A), noun(N), conjunction(CON), verb_phrase(VP).
noun_phrase(np(A,N,CON,VP)) --> adj_phrase(A), noun(N), conjunction(CON), noun_phrase(VP).


verb_phrase(vp(V)) --> verb(V).
verb_phrase(vp(V,CON,VP)) --> verb(V), conjunction(CON), verb_phrase(VP).
verb_phrase(vp(V,A)) --> verb(V), pronoun_phrase(A).
verb_phrase(vp(V,NP)) --> verb(V), noun_phrase(NP).
verb_phrase(vp(V,NP,A)) --> verb(V), noun_phrase(NP), adverb_phrase(A).
verb_phrase(vp(V,NP,PP)) --> verb(V), noun_phrase(NP), preposition_phrase(PP).
verb_phrase(vp(V,PP)) --> verb(V), preposition_phrase(PP).
verb_phrase(vp(V)) --> adverb_phrase(V).

preposition_phrase(prep(D)) --> preposition(D).
preposition_phrase(prep(D,NP)) --> preposition(D), noun_phrase(NP).

adj_phrase(adj(A)) --> adjective(A).
adj_phrase(adj(A,AP)) --> adjective(A), adj_phrase(AP).

adverb_phrase(adv(X,VP)) --> adverb(adv(X)), verb_phrase(VP).

pronoun_phrase(pp(P,VP)) --> pronoun(P), verb_phrase(VP).
pronoun_phrase(pp(P)) --> pronoun(P).
pronoun_phrase(pp(P,NP)) --> pronoun(P), noun_phrase(NP).

%% Nouns
 noun(n(paper)) --> [paper].
 noun(n(students)) --> [students].
 noun(n(love)) --> [love].
 noun(n(girl)) --> [girl].
 noun(n(man)) --> [man].
 noun(n(room)) --> [room].
 noun(n(warmth)) --> [warmth].
 noun(n(woman)) --> [woman].
 noun(n(letters)) --> [letters].
 noun(n(zone)) --> [zone].
 noun(n(writings)) --> [writings].
 noun(n(scientists)) --> [scientists].
 noun(n(idea)) --> [idea].
 noun(n(lecturers)) --> [lecturers].
 noun(n(building)) --> [building].
 noun(n(box)) --> [box].
 noun(n(professors)) --> [professors].
 noun(n(area)) --> [area].
 noun(n(plant)) --> [plant].
 noun(n(school)) --> [school].
 noun(n(researchers)) --> [researchers].
 noun(n(tree)) --> [tree].
 noun(n(university)) --> [university].
 noun(n(boy)) --> [boy].
 noun(n(number)) --> [number].
 noun(n(shed)) --> [shed].
 noun(n(theatre)) --> [theatre].
 noun(n(envelope)) --> [envelope].


%% Verbs
 verb(v(ask)) --> [ask].
 verb(v(eat)) --> [eat].
 verb(v(jumped)) --> [jumped].
 verb(v(travel)) --> [travel].
 verb(v(replaced)) --> [replaced].
 verb(v(listen)) --> [listen].
 verb(v(appreciated)) --> [appreciated].
 verb(v(pushed)) --> [pushed].
 verb(v(sleep)) --> [sleep].
 verb(v(played)) --> [played].
 verb(v(connect)) --> [connect].
 verb(v(push)) --> [push].
 verb(v(watched)) --> [watched].
 verb(v(gave)) --> [gave].
 verb(v(admired)) --> [admired].
 verb(v(ran)) --> [ran].
 verb(v(stored)) --> [stored].
 verb(v(kill)) --> [kill].
 verb(v(see)) --> [see].
 verb(v(climbed)) --> [climbed].
 verb(v(run)) --> [run].
 verb(v(saw)) --> [saw].
 verb(v(throw)) --> [throw].
 verb(v(yell)) --> [yell].
 verb(v(put)) --> [put].
 verb(v(yell)) --> [yell].
 verb(v(worked)) --> [worked].

%% Adjectives
 adjective(ad(bright)) --> [bright]. 
 adjective(ad(gentle)) --> [gentle].
 adjective(ad(fierce)) --> [fierce].
 adjective(ad(ambitious)) --> [ambitious].
 adjective(ad(soft)) --> [soft].
 adjective(ad(happy)) --> [happy].
 adjective(ad(entertaining)) --> [entertaining].
 adjective(ad(amusing)) --> [amusing].
 adjective(ad(old)) --> [old].
 adjective(ad(empty)) --> [empty].
 adjective(ad(beautiful)) --> [beautiful].
 adjective(ad(exciting)) --> [exciting].
 adjective(ad(nervous)) --> [nervous].
 adjective(ad(large)) --> [large].
 adjective(ad(hard)) --> [hard].
 adjective(ad(brilliant)) --> [brilliant].
 adjective(ad(loving)) --> [loving].
 adjective(ad(rich)) --> [rich].
 adjective(ad(young)) --> [young].
 adjective(ad(poor)) --> [poor].
 adjective(ad(sad)) --> [sad].
 adjective(ad(talented)) --> [talented]. 
 adjective(ad(calm)) --> [calm].
 adjective(ad(big)) --> [big].
 adjective(adj(white)) --> [white].

%% Adverbs
 adverb(adv(together)) --> [together].
 adverb(adv(quickly)) --> [quickly].
 adverb(adv(innocently)) --> [innocently].
 adverb(adv(always)) --> [always].
 adverb(adv(strictly)) --> [strictly].
 adverb(adv(likely)) --> [likely].
 adverb(adv(faithfully)) --> [faithfully].
 adverb(adv(carelessly)) --> [carelessly].
 adverb(adv(secretly)) --> [secretly].
 adverb(adv(often)) --> [often].

%% Prepositions
 preposition(prep(under)) --> [under].
 preposition(prep(for)) --> [for].
 preposition(prep(between)) --> [between].
 preposition(prep(before)) --> [before].
 preposition(prep(behind)) --> [behind].
 preposition(prep(at)) --> [at].
 preposition(prep(after)) --> [after].
 preposition(prep(above)) --> [above].
 preposition(prep(in)) --> [in].
 preposition(prep(within)) --> [within].
 preposition(prep(with)) --> [with].
 
%% Determiner
 det(d(these)) --> [these].
 det(d(those)) --> [those].
 det(d(an)) --> [an].
 det(d(other)) --> [other].
 det(d(any)) --> [any].
 det(d(that)) --> [that].
 det(d(the)) --> [the].
 det(d(a)) --> [a].
 det(d(such)) --> [such].
 det(d(every)) --> [every].
 det(d(this)) --> [this].
 det(d(many)) --> [many].
 det(d(some)) --> [some].

%% Conjunctions
 conjunction(con(and)) --> [and].
 conjunction(con(while)) --> [while].
 conjunction(con(or)) --> [or].
 conjunction(con(but)) --> [but].

%% Pronouns
 pronoun(pro(me)) --> [me].
 pronoun(pro(who)) --> [who].
 pronoun(pro(i)) --> [i].
 pronoun(pro(us)) --> [us].
 pronoun(pro(we)) --> [we].


