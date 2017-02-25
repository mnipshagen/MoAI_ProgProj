%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Created by:	Moritz Nipshagen, Anna Sandor, Arno Stiefvater
%%				{mnipshagen,asandor,astiefvater}@uos.de
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%															
%%  A program to create a time schedule given:
%%		TimeSlots, for five different days 	
%%		Rooms, and at which times they are available
%%		Teachers, and at which times they are available,
%%					no teacher may teach more than 5 lectures,
%%		Lectures, and by which teachers they might be taught,
%%					and in which rooms they can be hold 
%%														
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%														
%%	Queries to this program need to be formulated in the following form:
%%	[+Teachers,+Lectures,+Rooms,+TimeSlots]								
%%	Where: Teachers is a list of lists, each representing one teacher:
%%				[teacher name,[available times]]				
%%			Lectures is a list of lists, each representing one lecture:
%%				[lecture name, [teachers], [rooms]]						
%%				where teachers contains the names of possible teachers,	
%%				and rooms contains the name of possible Rooms 			
%%			Rooms is a list of lists, each representing one room:		
%%				[roomname, [available times]]							
%%																		
%% Note: the timeslots need to have a consistent naming!				
%% See the implemented queries as examples and further reference.		
%%																		
%%	The resulting schedule will be printed to the console.				
%%																		
%%	To use the test queries in the program just use 'start(1)'
%%																			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% used to kick-off the schedule with a built in quert
%% X is the number of query to be consulted
%% start(+X)
start(X) :-
	query(X,D),
	solve(D).

%% attempting to build a schedule out of the given data
%% solve(+Data)
solve([Teachers,Lectures,Rooms,Days]) :-
	initTeacherCounter(Teachers,TCounter),
	% all set. Let's build a schedule.
	buildPairs(Lectures,Teachers,Rooms,Unified),
	merge_sort(Unified,Sorted),
	schedule(Sorted,[],Schedule,TCounter),
	% schedule done. print it! Show it to the world!
	sortAndPrint(Days,Schedule).

%% Builds all possible combinations of Room and Lecturer for each Lecture
%% buildPairs(+Lectures,+Teachers,+Rooms,-AllPossiblePairs)
buildPairs([],_,_,[]).
buildPairs([[Lecture,TaughtBy,InRooms]|Lectures],
			Teachers,Rooms,
			[[Lecture,Pairs]|Res]) :-

	findPairs(TaughtBy,Teachers,InRooms,Rooms,Pairs),
	buildPairs(Lectures,Teachers,Rooms,Res).

%% Given a set of teachers and rooms it finds all possible combinations of these
%% two sets.
%% findPairs(+TeacherSet,+TeacherDB,+RoomSet,+RoomDB,-Pairs)
findPairs([],_,_,_,[]).
findPairs([Teacher|RT],Teachers,InRooms,Rooms,Result) :-
	member([Teacher,TimeSlots],Teachers),
	fitRooms(Teacher,TimeSlots,InRooms,Rooms,Res),
	findPairs(RT,Teachers,InRooms,Rooms,Res1),
	append(Res,Res1,Result).

%% for a given Teacher, it finds all rooms out of a set, which are available
%% at the same timeslots the teacher is available at.
%% fitRooms(+Teacher, +TeacherTimeSlots, +RoomSet, +RoomDB, -Pairs)
fitRooms(_,[],_,_,[]).
fitRooms(Teacher,[Slot|TimeSlots],InRooms,Rooms,Pairs) :-
	findall([Slot,Teacher,Room],
			(
				member(Room,InRooms),
				member([Room,RoomSlots],Rooms),
				member(Slot,RoomSlots)
			),
			Res),
	fitRooms(Teacher,TimeSlots,InRooms,Rooms,Res1),
	append(Res,Res1,Pairs).

%% initialises for each teacher a counter with 0, so we can keep track of
%% how many lectures each teacher is teaching and which times we already used
%% Teacher and Counter are stored as as pair.
%% initTeacherCounter(+Teachers, -TeacherCounterList)
initTeacherCounter([],[]).
initTeacherCounter([[Teacher,_]|R],[[Teacher,0]|Res]) :-
	initTeacherCounter(R,Res).

%% A merge sort algorithm, sorting the unified list from above
%% by the amount of possible combinations of teacher & room
%% merge_sort(+List,-SortedList)
merge_sort([],[]).
merge_sort([X],[X]).
merge_sort(List,Sorted):-
    List=[_,_|_],
    even_odd2(List,L1,L2),
	merge_sort(L1,Sorted1),
	merge_sort(L2,Sorted2),
	merge(Sorted1,Sorted2,Sorted).

%% merges two lists, sorted by the length of the 2nd sublist
%% (the amoung of possible teacher-room-combinations)
%%merge(+L1,+L2,-MergedList)
merge([],L,L).
merge(L,[],L):-L\=[].
merge([[LecX,X]|T1],[[LecY,Y]|T2],[[LecX,X]|T]):-
	length(X,N1),
	length(Y,N2),
	N1=<N2,
	merge(T1,[[LecY,Y]|T2],T).
merge([[LecX,X]|T1],[[LecY,Y]|T2],[[LecY,Y]|T]):-
	length(X,N1),
	length(Y,N2),
	N1>N2,
	merge([[LecX,X]|T1],T2,T).

%% splits a list into two parts which may be uneven by 1
%% even_odd2(+List,-Part1,-Part2)
even_odd2([],[],[]).
even_odd2([H|T],E,[H|O]):-even_odd2(T,O,E).

%% creates a schedule from the unified list.
%% the unified list is sorted by amount of combinations available.
%% (Most constrained variable)
%% We then pick a teacher, room and timeslot for the current lecture,
%% then test whether the teacher has less than 5 lectures.
%% If not we increase the teacher's counter by 1 and add the tuple to
%% our schedule.
%% We also delete all timeslot-room and timeslot-teacher combinations from
%% all the other lecture combinations, so that each teacher and room is only
%% used once per timeslot.
%% we then sort the whole list again to get the new most constrained lecture
%% Additionally, if a teacher hit his maximum of lectures to teach, (s)he will
%% be reomoved from all other pairs from the still "todo" list.
%% 	MaxLectures gives the amount of lectures each teacher can teach.
%% Must be called with an empty list as second argument for it to work.
%% schedule(+LecturesWithPairs,[],-ResultingSchedule,+TeacherCounter)
schedule([],Schedule,Schedule,_).
schedule([[Lecture,Pairs]|R],Schedule,Res,TCounter) :-
	MaxLectures = 5,
	member([TimeSlot,Teacher,Room],Pairs),
	member([Teacher,C],TCounter),
	C<MaxLectures,
	C1 is C+1,
	delete(TCounter,[Teacher,C],TCounterTemp),
	append(TCounterTemp,[[Teacher,C1]],NTCounter),
	(
	(C1=MaxLectures) ->
				(
				removeTeacher(Teacher,R,NewR),
				reserve(TimeSlot,Teacher,Room,NewR,Reserved),
				merge_sort(Reserved,Sorted),
				schedule(Sorted,
						[[TimeSlot,Lecture,Teacher,Room]|Schedule],
						Res,
						NTCounter)
				);
				(
				reserve(TimeSlot,Teacher,Room,R,Reserved),
				merge_sort(Reserved,Sorted),
				schedule(Sorted,
						[[TimeSlot,Lecture,Teacher,Room]|Schedule],
						Res,
						NTCounter)
				)).

%% "reserves" the teacher and room for a given timeslot, by deleting any other
%% timeslot-teacher or timeslot-room combination from the "todo" list
%% reserve(+TimeSlot,+Teacher,+Room,+LecturesWithPairs,-LecturesWithLessPairs)
reserve(_,_,_,[],[]).
reserve(Slot,T,R,[[Lecture,Pairs]|Rest],[[Lecture,NPairs]|Res]) :-
	delReserved(Slot,T,R,Pairs,NPairs),
	reserve(Slot,T,R,Rest,Res).

%% from a set of pairs this removes all pairs, which use T(eacher) or (R)oom
%% at that timeslot.
%% delReserved(+TimeSlot,+Teacher,+Room,+Pairs,-NewPairs)
delReserved(_,_,_,[],[]).
delReserved(Slot,T,R,[[Time,Teacher,Room]|Rest],NPairs) :-
	Slot=Time,
	(T=Teacher ; R=Room),
	delReserved(Slot,T,R,Rest,NPairs),!.
delReserved(Slot,T,R,[[Time,Teacher,Room]|Rest],[[Time,Teacher,Room]|Npairs]) :-
	delReserved(Slot,T,R,Rest,Npairs).

%% goes through the "todo" list and removes all pairs with the given teacher
%% removeTeacher(+Teacher,+ToDoList,-NewTodoList)
removeTeacher(_,[],[]).
removeTeacher(Teacher,[[Lecture,Pairs]|R],[[Lecture,NPairs]|Res]) :-
	remPairs(Teacher,Pairs,NPairs),
	removeTeacher(Teacher,R,Res).

%% removes all pairs from a set of pairs which use the given teacher
%% remPairs(+Teacher,+Pairs,-LessPairs)
remPairs(_,[],[]).
remPairs(T,[[_Slot,Teacher,_Room]|R],NPairs) :-
	T=Teacher,
	remPairs(T,R,NPairs),!.
remPairs(T,[[Slot,Teacher,Room]|R],[[Slot,Teacher,Room]|NPairs]) :-
	remPairs(T,R,NPairs).

%% sorts the schdule according to the input timeslots
%% also prints each timeslot in one line for readability
%% sortAndPrint(+Days,+Schedule)
sortAndPrint([],_).
sortAndPrint([Day|Days],Schedule) :-
	daySort(Day,Schedule),
	nl,
	sortAndPrint(Days,Schedule).

%% finds all scheduled lectures in the list for the given day
%% daySort(+Day,+Schedule,-SortedList)
daySort([],_).
daySort([TimeSlot|Slots],Schedule) :-
	findall([TimeSlot,Lecture,Teacher,Room],
			(member([TimeSlot,Lecture,Teacher,Room],Schedule)),
			SlotList),
	write(SlotList),nl,
	daySort(Slots,Schedule).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test Queries

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

%% a query without solution
query(3, [
	[
		[t1,[mon1,mon2,mon3]],
		[t2,[tue1,tue2,tue3]]
	],
 	[
 		[l1,[t1,t2],[r1]]
 	],
  	[
  		[r1,[wed1,wed2,wed3]]
  	],
  	[
  		[mon1,mon2,mon3],[tue1,tue2,tue3],[wed1,wed2,wed3]
  	]
  	]).

%% another test query
query(4, [
    [
        [t1,[mon1,tue1,tue2,fri1,fri2]],
        [t2,[mon2,tue1,tue2,wed1,wed2,thu1,thu2]],
        [t3,[wed1,wed2,thu2,fri2]],
        [t4,[tue1,tue2,fri1,fri2]],
        [t5,[mon1,mon2,thu1,thu2]]
    ],
    [
        [art1,[t1,t2],[r2]],
        [art2,[t1,t2],[r2]],
        [music1,[t2,t3],[r1]],
        [music2,[t2,t3],[r1]],
        [ger1,[t1,t3],[r1,r2,r3]],
        [ger2,[t1,t3],[r1,r2,r3]],
        [eng,[t1,t3,t4],[r1,r2,r3]],
        [french,[t4],[r1,r2,r3]],
        [math1,[t5,t1],[r1,r2,r3]],
        [math2,[t5,t1],[r1,r2,r3]]
    ],
    [
        [r1,[mon1,mon2,tue1,tue2,wed1,wed2,thu1,thu2,fri1,fri2]],
        [r2,[mon1,mon2,tue1,tue2,wed1,wed2,thu1,thu2,fri1,fri2]],
        [r3,[mon1,mon2,tue1,tue2,wed1,wed2,thu1,thu2,fri1,fri2]]
    ],
    [
        [mon1,mon2],[tue1,tue2],[wed1,wed2],[thu1,thu2],[fri1,fri2]
    ]   
    ]).
