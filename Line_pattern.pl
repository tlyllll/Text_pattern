:- op( 950, xfx, ==>  ). 
:- dynamic fact/1, new_fact/1.
:- retractall( fact(_) ).
:- use_module(library(statistics)).
%% cut list from [p,q]
slice(AsBsCs,P,Q,Bs) :-
   append(AsBs,_Cs,AsBsCs),
   append(As  , Bs,AsBs  ),
   length([_|As],P),
   length( AsBs ,Q).
%%return index and length of the fisrt constant'1'
one(Input,Top,OneLen):-
    nth1(Top,Input,1),
	length(Input,Len),
    slice(Input,Top,Len,SplitList),
    oneLen(SplitList,OneLen,Len,Top).
oneLen(SplitList,OneLen, Len, Top):-  
    nth0(OneLen,SplitList,0);
    OneLen is Len-Top+1.

%% Check whether an array contains only one line
single_line(L):-
    one(L,Top,OneLen),
    End is Top+OneLen,
    length(L,Len),
    slice(L,End,Len,Temp),!,
    \+nth1(_,Temp,1).

%Allen's Relationship
precedes( Line1,Line2 ):-
    one(Line1,Top1,OneLen1),
    one(Line2,Top2,_),!,
    Tail1 is Top1+OneLen1,
    Tail1 < Top2.

meets( Line1,Line2 ):-
    one(Line1,Top1,OneLen1),
    one(Line2,Top2,_),!,
    Tail1 is Top1+OneLen1,
    Tail1 = Top2.

overlaps( Line1,Line2 ):-
    one(Line1,Top1,OneLen1),
    one(Line2,Top2,OneLen2),!,
    Top1 < Top2,
    Tail1 is Top1+OneLen1,
    Tail1 > Top2,
    Tail2 is Top2+OneLen2,
    Tail2 > Tail1.

starts( Line1,Line2 ):-
    one(Line1,Top1,OneLen1),
    one(Line2,Top2,OneLen2),!,
    OneLen2 > OneLen1,
    Top1 is Top2.

during( Line1,Line2 ):-
    one(Line1,Top1,OneLen1),
    one(Line2,Top2,OneLen2),!,
    Top1 > Top2,
    Tail1 is Top1+OneLen1,
    Tail2 is Top2+OneLen2,
    Tail2 > Tail1.

finishes( Line1,Line2 ):-
    reverse(Line1, ReversedList1),
    reverse(Line2, ReversedList2),
    starts(ReversedList1,ReversedList2),!.

equal( Line1,Line2 ):-
    one(Line1,Top1,OneLen1),
    one(Line2,Top2,OneLen2),!,
    Top1 is Top2,
    Tail1 is Top1+OneLen1,
    Tail2 is Top2+OneLen2,
    Tail2 is Tail1.

fact( [X, m, Y]):- meets(X,Y),!.
fact( [Y, mi, X]):- fact( [X, m, Y]),!.
fact( [X, o, Y]):- overlaps(X,Y),!.
fact( [Y, oi, X]):- fact( [X, o, Y]),!.
fact( [X, s, Y]):- starts(X,Y),!.
fact( [Y, si, X]):- fact( [X, s, Y]),!.
fact( [X, d, Y]):- during(X,Y),!.
fact( [Y, di, X]):- fact( [X, d, Y]),!.
fact( [X, f, Y]):- finishes(X,Y),!.
fact( [Y, fi, X]):- fact( [X, f, Y]),!.
fact( [X, p, Y]):- precedes(X,Y),!.
fact( [Y, pi, X]):- fact( [X, p, Y]),!.
fact( [X, e, Y]):- equal(X,Y),!.
fact( [Y, e, X]):- fact( [X, e, Y]),!.








%% Show all facts involving terms in list L

describe(Line1,Line2) :- write('=== Facts involving: '),
    	write( Line1 ),write( ', ' ),write(Line2 ), write(' ==='), nl, 
        ( ((member(Line1,F), member(Line2,F),fact(F)), write(F), nl) ; true ), !.

%% classfying pattern of the lines groups
startsame(X,Y):- starts(X,Y); starts(Y,X); equal(X,Y).
alignedleft([_|[]]).
alignedleft([H1,H2|T]) :-
    single_line(H1),
    startsame(H1,H2),!, 
    alignedleft([H2|T]).

finishsame(X,Y):- 
    finishes(X,Y); finishes(Y,X); equal(X,Y).
alignedright([_|[]]).
alignedright([H1,H2|T]) :-
    single_line(H1),
    finishsame(H1,H2),!, 
    alignedright([H2|T]).

centre(Line1,Line2):-     
    one(Line1,Top1,OneLen1),
    one(Line2,Top2,OneLen2),!,
    Tail1 is Top1+(OneLen1/2),
    Tail2 is Top2+(OneLen2/2),
    Tail2 == Tail1.
centred([_|[]]).
centred([H1,H2|T]) :-
    single_line(H1),
    centre(H1,H2),!, 
    centred([H2|T]).

meshed_alternately([_|[]]).
meshed_alternately([H1,H2|[]]):-
    single_line(H1),
    single_line(H2),
    overlaps(H1,H2).
meshed_alternately([H1,H2,H3|T]) :-
    single_line(H1),
    single_line(H2),
    overlaps(H1,H2),
    overlaps(H3,H2), 
    meshed_alternately([H3|T]).

end_start([_|[]]).
end_start([H1,H2,H3|T]) :-
    single_line(H1),
    single_line(H2),
    meets(H1,H2),!, 
    end_start([H3|T]).
end_start([H1,H2|[]]) :-
    single_line(H1),
    single_line(H2),
    meets(H1,H2),!.

cascaded_check(H1,H2) :- overlaps(H1,H2); meets(H1,H2).
cascaded([_|[]]).
cascaded([H1,H2|T]) :-
    single_line(H1),!, 
    cascaded_check(H1,H2),!, 
    cascaded([H2|T]).

near(A,B):- A is B+1;A is B;A is B-1.
middle(L,LW,RW):- 
    one(L,LT,LW),
    length(L,ListLen),
    End is LT+LW,
    slice(L,End,ListLen,Temp),
    one(Temp,_,RW).

framing([H1,H2|T]):- 
    single_line(H1),!,
    framing([H2|T],H1,0).
framing([H1,H2|T],UpList,Lines):-
    	single_line(H1),
    	equal(H1,UpList),!,
        NewLines is Lines+1,!,
        framing([H2|T],UpList,NewLines).
framing([H1,H2|T],UpList,Lines):- 
    	starts(H1,UpList),
    	finishes(H1,UpList),
        middle(H1,LW,RW),!,
    	framing([H2|T],UpList,Lines,LW,RW).
framing([H1,H2|T],UpList,Lines,LW,RW):-
		\+single_line(H1),
    	starts(H1,UpList),
    	finishes(H1,UpList),
    	middle(H1,LW1,RW1),
    	near(LW1,LW),
    	near(RW1,RW),!,
    	framing([H2|T],UpList,Lines,LW,RW).
framing([H1,H2|T],UpList,Lines,_,_):-
        equal(H1,UpList),
        NewLines is Lines-1,!,
        framing_bottom([H2|T],UpList,NewLines).
framing([H1|[]],UpList,Lines,_,_):- 
    equal(H1,UpList),
    Lines == 0.
framing_bottom([H1,H2|T],UpList,Lines):- 
    equal(H1,UpList),
    NewLines is Lines-1,!,
    framing_bottom([H2|T],UpList,NewLines).

framing_bottom([H1|[]],UpList,Lines):- 
    Lines == 0,!,
    equal(H1,UpList).


pattern(L,framing):- framing(L),!.
pattern(L,centered):- centred(L),!.
pattern(L,aligned_left):- alignedleft(L),!.
pattern(L,aligned_right):- alignedright(L),!.
pattern(L,end_start):- end_start(L),!.
pattern(L,meshed_alternately):- meshed_alternately(L),!.
pattern(L,cascaded):- cascaded(L),!.
patterns(L):- write(L),nl,
    write("Finding pattern......"),nl,
    pattern(L,X),write(X),nl.


