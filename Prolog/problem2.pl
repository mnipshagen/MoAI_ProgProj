%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Created by:	Moritz Nipshagen, Anna Sandor, Arno Stiefvater
%%				{mnipshagen,asandor,astiefvater}@uos.de
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  A program to create a time schedule given:
%%		TimeSlots, for five different days
%%		Rooms, and at which times they are available
%%		Teachers, and at which times they are available,
%%					no teacher may teach more than 5 lectures,
%%		Lectures, and by which teachers they might be taught,
%%					and in which rooms they can be hold
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%	Queries to this program need to be formulated in the following form:
%%	[+Teachers,+Lectures,+Rooms,+TimeSlots]
%%	Where: Teachers is a list of lists, each representing one teacher:
%%				[teacher name,[available times]]
%%			Lectures is a list of lists, each representing one lecture:
%%				[lecture name, [teachers], [rooms]]
%%				where teachers contains the names of possible teachers,
%%				and rooms contains the name of possible rooms
%%			Rooms is a list of lists, each representing one room:
%%				[roomname, [available times]]
%%			
%%	The resulting schedule will be printed to the console.
%%
%%	To use the test queries in the program just use 'start(1)' or 'start(2)'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% The example given in the slides, written down as a query.
query(1, [
	[
		[t1,[tue1,tue2,tue3, wed1,wed2,wed3, thu1,thu2,thu3]],
		[t2,[mon1,mon2,mon3, tue1,tue2,tue3, wed1,wed2,wed3]],
		[t3,[thu1,thu2,thu3, fri1,fri2,fri3]],
		[t4,[mon2,mon3,tue2,tue3,wed2,wed3,thu2,thu3,fri2,fri3]],
		[t5,[mon1,mon2,mon3,tue1,tue2,tue3,
				wed1,wed2,wed3,thu1,thu2,thu3,fri1,fri2,fri3]]
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
	[
		[mon1,mon2,mon3],[tue1,tue2,tue3],
		[wed1,wed2,wed3],[thu1,thu2,thu3],[fri1,fri2,fri3]
	]
	]).

%% a simpler query for test purposes
query(2, [
	[
		[t1,[mon1,tue1,wed1]],
		[t2,[mon2,tue1,tue2]],
		[t3,[mon1,mon2,tue1,tue2,wed1,wed2]]
	],
	[
		[l1,[t1],[r1,r2]],
		[l2,[t1,t2],[r1,r2]],
		[l3,[t1,t2],[r2,r3]],
		[l4,[t2,t3],[r1,r3]],
		[l5,[t3],[r3]],
		[l6,[t3],[r1,r2,r3]]
	],
	[
		[r1,[mon1,mon2,tue1,tue2,wed1,wed2]],
		[r2,[mon1,mon2,tue1,tue2]],
		[r3,[mon1,tue1,wed1]]
	],
	[
		[mon1,mon2],[tue1,tue2],[wed1,wed2],[thu1,thu2],[fri1,fri2]
	]
	]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% used to kick-off the schedule with a built in quert
%% X is the number of query to be consulted, S holds the schedule
%% start(+X,-S)
start(X) :-
	query(X,D),
	solve(D).

%% attempting to build a schedule out of the given data
%% solve(+Data,-S)
solve([Teachers,Lectures,Rooms,Days]) :-
	initTeacherCounter(Teachers),
	unify(Lectures, Teachers,Rooms,UniFied),
	merge_sort(UniFied,Sorted),
	schedule(Sorted,[],Schedule),
	sortAndPrint(Days,Schedule).

%% initialises for each teacher a global counter with 0, so we can keep track of
%% how many lectures each teacher is teaching
%% initTeacherCounter(+Teachers)
initTeacherCounter([]).
initTeacherCounter([[Teacher,_]|R]) :-
	asserta(teacherCounter(Teacher,0)),
	initTeacherCounter(R).

%% unifies the given Data to one list of lists in the form
%% [LectureName, ListOfTeachers, ListOfRooms]
%% unify(+Lectures,+Teachers,+Rooms,-UnifiedList)
unify([],_,_,[]).
unify([[Lecture,TaughtBy,InRooms]|Lectures],
		Teachers,
		Rooms,
		[[Lecture,TaughtBy,InRooms,PossibleTimes]|R]) :-

			addTimes(TaughtBy,Teachers,TeacherTimes),
			unique(TeacherTimes,TTimes),
			addTimes(InRooms,Rooms,RoomTimes),
			unique(RoomTimes,RTimes),
			inter(TTimes,RTimes,PossibleTimes),
			unify(Lectures,Teachers,Rooms,R).


%% This takes the room or teacher data from a lecture
%% and finds the corresponding times in the teacher or room data
%% addTimes(+ListOfName, +NameData, -TimeList)
addTimes([],_,[]).
addTimes([O|R],List,All) :-
	member([O,Times],List),
	addTimes(R,List,More),
	append(More,Times,All).

%% Eliminates all multiples from a list
%% Cut, because the is no alternative to being unique
%% unique(+List,-ListOfUniques)
unique([],[]) :- !.
unique([X|R1],[X|R2]) :-
	not(member(X,R1)),
	unique(R1,R2).
unique([_|R1],R2) :-
	unique(R1,R2).

%% calculates the intersection of two lists
%% inter(+List1,+List2,-Intersection)
inter([], L2, L2).
inter([H1|T1], L2, [H1|Res]) :-
    member(H1, L2),
    inter(T1, L2, Res).
inter([_|T1], L2, Res) :-
    inter(T1, L2, Res).

%% A merge sort algorithm, sorting the unified list from above
%% by the amount of possible TimeSlots
%% merge_sort(+List,-SortedList)
merge_sort([],[]).
merge_sort([X],[X]).
merge_sort(List,Sorted):-
    List=[_,_|_],even_odd2(List,L1,L2), 
	merge_sort(L1,Sorted1),merge_sort(L2,Sorted2),
	merge(Sorted1,Sorted2,Sorted).

%% merges two lists, sorted by the length of the 4th sublist
%%merge(+L1,+L2,-MergedList)
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

%% splits a list into two parts which may be uneven by 1
%% even_odd2(+List,-Part1,-Part2)
even_odd2([],[],[]).
even_odd2([H|T],E,[H|O]):-even_odd2(T,O,E).

%% creates a schedule from the unified list.
%% the unified list is sorted by amount of timeslots available.
%% (Most constrained variable)
%% We then pick a teacher, room and timeslot for the current lecture,
%% then test whether the teacher has less than 5 lectures
%% and check whether the room or teacher are already busy at this time.
%% If not we increase the teacher's counter by 1 and add the tuple to
%% our schedule.
%% Must be called with an empty list as second argument, or an existing
%% schedule for it to work.
%% schedule(+UnifiedData,[],-ResultingSchedule)
schedule([],Schedule,Schedule).
schedule([[Lecture,Teachers,Rooms,TimeSlots]|R],Schedule,Res) :-
	member(Teacher,Teachers),
	member(Room,Rooms),
	member(Slot,TimeSlots),
	teacherCounter(Teacher,C), C<5,
	not(member([Slot,_,Teacher,_],Schedule)),
	not(member([Slot,_,_,Room],Schedule)),
	retract(teacherCounter(Teacher,C)),
	C1 is C+1,
	asserta(teacherCounter(Teacher,C1)),
	schedule(R,[[Slot,Lecture,Teacher,Room]|Schedule],Res).

%% sorts the schdule according to the input timeslots
%% also prints each day in one line for readability
%% sortAndPrint(+Days,+Schedule)
sortAndPrint([],_).
sortAndPrint([Day|Days],Schedule) :-
	daySort(Day,Schedule,DayList),
	write(DayList), nl,
	sortAndPrint(Days,Schedule).

%% finds all scheduled lectures in the list for the given day
%% daySort(+Day,+Schedule,-SortedList)
daySort([],_,[]).
daySort([TimeSlot|Slots],Schedule,DayList) :-
	findall([TimeSlot,Lecture,Teacher,Room],
			(member([TimeSlot,Lecture,Teacher,Room],Schedule)),
			SlotList),
	daySort(Slots,Schedule,R),
	append(SlotList,R,DayList).