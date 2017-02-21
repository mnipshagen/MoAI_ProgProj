query(1, [
	[
		[t1,[tue1,tue2,tue3, wed1,wed2,wed3, thu1,thu2,thu3]],
		[t2,[mon1,mon2,mon3, tue1,tue2,tue3, wed1,wed2,wed3]],
		[t3,[thu1,thu2,thu3, fri1,fri2,fri3]],
		[t4,[mon2,mon3,tue2,tue3,wed2,wed3,thu2,thu3,fri2,fri3]],
		[t5,[mon1,mon2,mon3,tue1,tue2,tue3,wed1,wed2,wed3,thu1,thu2,thu3,fri1,fri2,fri3]]
	],
	[
		[l1,[t1,t2],[r1,r2,r4]], [l2,[t1,t2],[r1,r2,r4]], 
		[l3,[t1,t2],[r1,r2,r4]], [l4,[t1,t2],[r1,r2,r4]], 
		[l5,[t1,t2],[r1,r2,r4]], [l6,[t3,t4],[r1,r2,r4]], 
		[l7,[t3,t4],[r1,r2,r4]], [l8,[t3,t4],[r1,r2,r4]], 
		[l9,[t3,t4],[r1,r2,r4]], [l10,[t3,t4],[r1,r2,r4]],
		[l11,[t2,t5],[r1,r3,r5]], [l12,[t2,t5],[r1,r3,r5]], 
		[l13,[t2,t5],[r1,r3,r5]], [l14,[t2,t5],[r1,r3,r5]], 
		[l15,[t2,t5],[r1,r3,r5]], [l16,[t3,t5],[r1,r3,r5]], 
		[l17,[t3,t5],[r1,r3,r5]], [l18,[t3,t5],[r1,r3,r5]], 
		[l19,[t3,t5],[r1,r3,r5]], [l20,[t3,t5],[r1,r3,r5]]
	],
	[
		[r1,[mon1,mon2,mon3]],
		[r2,[mon1,mon2,mon3,tue1,tue2,tue3,wed1,wed2,wed3,thu1,thu2,thu3]],
		[r3,[tue1,tue2,tue3,wed1,wed2,wed3,thu1,thu2,thu3,fri1,fri2,fri3]],
		[r4,[mon1,tue1,wed1,thu1,fri1]],
		[r5,[mon3,tue3,wed3,thu3,fri3]]
	],
	[[mon1,mon2,mon3],[tue1,tue2,tue3],[wed1,wed2,wed3],[thu1,thu2,thu3],[fri1,fri2,fri3]]
	]).

query(2, [
	[
		[t1,[l1,l2,l3],[mon1,tue1,wed1]],
		[t2,[l2,l3,l4],[mon2,tue1,tue2]],
		[t3,[l4,l5,l6],[mon1,mon2,tue1,tue2,wed1,wed2]]
	],
	[
		[l1,[r1,r2]],[l2,[r1,r2]],[l3,[r2,r3]],[l4,[r1,r3]],[l5,[r3],[l6,[r1,r2,r3]]]
	],
	[
		[r1,[mon1,mon2,tue1,tue2,wed1,wed2]],
		[r2,[mon1,mon2,tue1,tue2]],
		[r3,[mon1,tue1,wed1]]
	],
	[
		[mon1,mon2],[tue1,tue2],[wed1,wed2],[],[]
	]
	]).

start(X,S) :-
	query(X,D),
	solve(D,S).

% Solution S : [[Monday Schedule], [Tuesday Schedule], [Wednesday Schedule], [Thursday Schedule], [Friday Schedule]]

solve([Teachers,Lectures,Rooms,TimeSlots], S) :-
	initTeacherCounter(Teachers),
	unify(Lectures, Teachers,Rooms,UniFied),
	merge_sort(UniFied,Sorted),
	schedule(Sorted,[],S).

initTeacherCounter([]).
initTeacherCounter([[Teacher,_]|R]) :-
	asserta(teacherCounter(Teacher,0)),
	initTeacherCounter(R).

unify([],_,_,[]).
unify([[Lecture,TaughtBy,InRooms]|Lectures],Teachers,Rooms,[[Lecture,TaughtBy,InRooms,PossibleTimes]|R]) :-
	addTimes(TaughtBy,Teachers,TeacherTimes),
	unique(TeacherTimes,TTimes),
	addTimes(InRooms,Rooms,RoomTimes),
	unique(RoomTimes,RTimes),
	inter(TTimes,RTimes,PossibleTimes),
	unify(Lectures,Teachers,Rooms,R).

addTimes([],_,[]).
addTimes([O|R],List,All) :-
	member([O,Times],List),
	addTimes(R,List,More),
	append(More,Times,All).

unique([],[]) :- !.
unique([X|R1],[X|R2]) :-
	not(member(X,R1)),
	unique(R1,R2).
unique([_|R1],R2) :-
	unique(R1,R2).

inter([], L2, L2).
inter([H1|T1], L2, [H1|Res]) :-
    member(H1, L2),
    inter(T1, L2, Res).
inter([_|T1], L2, Res) :-
    inter(T1, L2, Res).


merge_sort([],[]).     % empty list is already sorted
merge_sort([X],[X]).   % single element list is already sorted
merge_sort(List,Sorted):-
    List=[_,_|_],divide(List,L1,L2),     			% list with at least two elements is divided into two parts
	merge_sort(L1,Sorted1),merge_sort(L2,Sorted2),  % then each part is sorted
	merge(Sorted1,Sorted2,Sorted).                  % and sorted parts are merged
merge([],L,L).
merge(L,[],L):-L\=[].
merge([[X1,X2,X3,X]|T1],[[Y1,Y2,Y3,Y]|T2],[[X1,X2,X3,X]|T]):-
	length(X,N1),
	length(Y,N2),
	N1=<N2,
	merge(T1,[[Y1,Y2,Y3,Y]|T2],T).
merge([[X1,X2,X3,X]|T1],[[Y1,Y2,Y3,Y]|T2],[[Y1,Y2,Y3,Y]|T]):-
	length(X,N1),
	length(Y,N2),
	N1>N2,
	merge([[X1,X2,X3,X]|T1],T2,T).
divide(L,L1,L2):-even_odd2(L,L1,L2).
even_odd2([],[],[]).
even_odd2([H|T],E,[H|O]):-even_odd2(T,O,E).

schedule([],Schedule,Schedule).
schedule([[Lecture,Teachers,Rooms,TimeSlots]|R],Path,[[Slot,Lecture,Teacher,Room]|Res]) :-
	length(Path,N),
	member(Teacher,Teachers),
	member(Room,Rooms),
	member(Slot,TimeSlots),
	teacherCounter(Teacher,C), C<5,
	not(member([Slot,_,Teacher,_],Path)),
	not(member([Slot,_,_,Room],Path)),
	retract(teacherCounter(Teacher,C)),
	C1 is C+1,
	asserta(teacherCounter(Teacher,C1)),
	schedule(R,[[Slot,Lecture,Teacher,Room]|Path],Res).
