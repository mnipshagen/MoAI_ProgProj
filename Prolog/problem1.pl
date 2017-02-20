:- use_module(library(clpfd)).

query(1, [
	[
		[t1,[l1,l2,l3,l4,l5],[tue1,tue2,tue3, wed1,wed2,wed3, thu1,thu2,thu3]],
		[t2,[l1,l2,l3,l4,l5,l11,l12,l13,l14,l15],[mon1,mon2,mon3, tue1,tue2,tue3, wed1,wed2,wed3]],
		[t3,[l6,l7,l8,l9,l10,l16,l17,l18,l19,l20],[thu1,thu2,thu3, fri1,fri2,fri3]],
		[t4,[l6,l7,l8,l9,l10],[mon2,mon3,tue2,tue3,wed2,wed3,thu2,thu3,fri2,fri3]],
		[t5,[l11,l12,l13,l14,l15,l16,l17,l18,l19,l20],[mon1,mon2,mon3,tue1,tue2,tue3,wed1,wed2,wed3,thu1,thu2,thu3,fri1,fri2,fri3]]
	],
	[
		[l1,[r1,r2,r4]], [l2,[r1,r2,r4]], 
		[l3,[r1,r2,r4]], [l4,[r1,r2,r4]], 
		[l5,[r1,r2,r4]], [l6,[r1,r2,r4]], 
		[l7,[r1,r2,r4]], [l8,[r1,r2,r4]], 
		[l9,[r1,r2,r4]], [l10,[r1,r2,r4]],
		[l11,[r1,r3,r5]], [l12,[r1,r3,r5]], 
		[l13,[r1,r3,r5]], [l14,[r1,r3,r5]], 
		[l15,[r1,r3,r5]], [l16,[r1,r3,r5]], 
		[l17,[r1,r3,r5]], [l18,[r1,r3,r5]], 
		[l19,[r1,r3,r5]], [l20,[r1,r3,r5]]
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

start(X,S) :-
	query(X,D),
	solve(D,S).

% Solution S : [[Monday Schedule], [Tuesday Schedule], [Wednesday Schedule], [Thursday Schedule], [Friday Schedule]]

solve([Teachers,Lectures,Rooms,[Mon,Tue,Wed,Thu,Fri]], S) :-
		timeroom(Mon,Rooms,MonPairs),
		timeroom(Tue,Rooms,TuePairs),
		append(MonPairs, TuePairs, Pairs1),
		timeroom(Wed,Rooms,WedPairs),
		append(Pairs1, WedPairs, Pairs2),
		timeroom(Thu,Rooms,ThuPairs),
		append(Pairs2, ThuPairs, Pairs3),
		timeroom(Fri,Rooms,FriPairs),
		append(Pairs3,FriPairs,Pairs).



timeroom([],_,_).
timeroom([Slot|Timeslots], Rooms, [[Slot,Room]|R]) :-
	member([Room, Slots], Rooms),
	member(Slot, Slots),
	timeroom(Timeslots,Rooms,R).