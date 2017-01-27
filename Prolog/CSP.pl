%-----------------------------------------------------------------------------%
% some code. not relevant. maybe.
% some problem structure
%-----------------------------------------------------------------------------%
problem(time_slots, rooms, instructors, lectures).
problem(['Mon 8-10', 'Mon 10-12', 'Mon 12-14',
		 'Tue 8-10', 'Tue 10-12', 'Tue 12-14',
		 'Wed 8-10', 'Wed 10-12', 'Wed 12-14',
		 'Thu 8-10', 'Thu 10-12', 'Thu 12-14',
		 'Fri 8-10', 'Fri 10-12', 'Fri 12-14'],
		[1,2,3,4,5],
		[]).

%-----------------------------------------------------------------------------%
% General Problem
% 
% Build a schedule for a Lecture timetable
%	Lectures:
%		Need a room
%		Need a time
%		Need an instructor
% 	Rooms:
%		Only one lecture at a time
%		some lectures are room restrained
%		rooms can only be used at certain times
%	Instructors:
%		Instructor can only hold one lecture at a time
%		Instructor can only hold specific lectures
%		Instructurs are time restrained 
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% Ideas & Notes
%
% given problem consists of: time slots, rooms, instructors, # of lectures
% 	and which lecture can be hold by which instructor and which rooms
%	and which instructor is free on which time slot
%	and which rooms are free at which time slots
%
% Find instructor with most options others don't have
%	(least constraining variable)
% Use IDA* to find solution or something like this
%-----------------------------------------------------------------------------%


%-----------------------------------------------------------------------------%
% CSP solver for Time Schedule
%-----------------------------------------------------------------------------%

schedule(Problem, Solution) :-
	problem(Problem, Data),
	h(Data, MaxDepth),
	solve(Data, MaxDepth, Solution).

%-----------------------------------------------------------------------------%
% Heuristics
%
% Idea: lecturers or rooms will probably be most constraining.
% Look for lecturer-room-lecture-time combination that will block the least
% (least constraining variable heuristics)
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% Lecture
%
% Basic structure:
% Lecture(_Name, _Instructor, _Room, _Time).
%-----------------------------------------------------------------------------%