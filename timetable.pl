% ------------------------   
% TimeTableGen - Prolog University Timetable Generator
% ------------------------

:- use_module(library(csv)).
:- dynamic course/3, lecturer/3, room/2, group/2, time_slot/1.
:- dynamic preferred_day/1, exclude_slot/1.

% ------------------------ Scheduling Core ------------------------

schedule(Timetable) :-
    findall(CourseCode-Hours-Groups, course(CourseCode, Hours, Groups), CourseList),
    writeln("Courses to schedule:"), writeln(CourseList),
    assign_slots(CourseList, [], Timetable),
    valid_timetable(Timetable).

assign_slots([], Timetable, Timetable).
assign_slots([CourseCode-Hours-Groups | Rest], Acc, Timetable) :-
    findall(Lecturer-Unavailable,
            (lecturer(Lecturer, Courses, Unavailable), member(CourseCode, Courses)),
            LecturerOptions),
    try_lecturers(LecturerOptions, CourseCode, Hours, Groups, Acc, NewAcc),
    assign_slots(Rest, NewAcc, Timetable).

try_lecturers([], Course, H, G, _, _) :-
    format("❌ Failed to assign course ~w (Hours: ~w, Groups: ~w)~n", [Course, H, G]),
    fail.

try_lecturers([Lecturer-Unavailable | Others], Course, H, G, Acc, NewAcc) :-
    format("Attempting ~w for course ~w (~w hours, groups: ~w)~n", [Lecturer, Course, H, G]),
    ( assign_hours(Course, Lecturer, H, G, Unavailable, Slots) ->
        append(Acc, Slots, NewAcc)
    ; try_lecturers(Others, Course, H, G, Acc, NewAcc)
    ).

assign_hours(_, _, 0, _, _, []) :- !.
assign_hours(Course, Lecturer, Hours, Groups, Unavailable, [course(Course, Slot, Room, Lecturer) | Rest]) :-
    total_group_size(Groups, Size),
    room(Room, Capacity),
    Capacity >= Size,
    time_slot(Slot),
    \+ member(Slot, Unavailable),
    \+ exclude_slot(Slot),
    ( \+ preferred_day(_) ; (preferred_day(Day), atom_concat(Day, _, Slot)) ),
    NewH is Hours - 1,
    assign_hours(Course, Lecturer, NewH, Groups, Unavailable, Rest).

total_group_size(Groups, Total) :-
    findall(Size, (member(G, Groups), group(G, Size)), Sizes),
    sum_list(Sizes, Total).

valid_timetable(Timetable) :-
    no_conflicts(Timetable),
    no_double_bookings(Timetable).

no_conflicts([]).
no_conflicts([course(_, Slot, Room, Lecturer) | Rest]) :-
    \+ member(course(_, Slot, Room, _), Rest),
    \+ member(course(_, Slot, _, Lecturer), Rest),
    no_conflicts(Rest).

no_double_bookings([]).
no_double_bookings([course(Course, Slot, _, _) | Rest]) :-
    \+ member(course(Course, Slot, _, _), Rest),
    no_double_bookings(Rest).

% ------------------------ CSV Export ------------------------

export_csv(Timetable, FileName) :-
    open(FileName, write, Stream),
    write(Stream, "Course,Slot,Room,Lecturer\n"),
    forall(member(course(C, S, R, L), Timetable),
           format(Stream, "~w,~w,~w,~w\n", [C, S, R, L])),
    close(Stream).

% ------------------------ ASCII Printer ------------------------

print_timetable(Timetable) :-
    sort(Timetable, Sorted),
    write('------------------------------------------\n'),
    write('| Course |     Slot    | Room | Lecturer |\n'),
    write('------------------------------------------\n'),
    forall(member(course(C, S, R, L), Sorted),
           format('| ~w | ~w | ~w | ~w |\n', [C, S, R, L])),
    write('------------------------------------------\n').

% ------------------------ HTML Timetable Export ------------------------

export_html(Timetable, FileName) :-
    open(FileName, write, Stream),
    write(Stream, '<!DOCTYPE html><html><head><meta charset="UTF-8"><title>Timetable</title>'),
    write(Stream, '<style>table { border-collapse: collapse; width: 80%; margin: 2em auto; }'),
    write(Stream, 'th, td { border: 1px solid #ccc; padding: 8px; text-align: center; }'),
    write(Stream, 'th { background: #f2f2f2; }</style></head><body>'),
    write(Stream, '<h2 style="text-align:center;">University Timetable</h2><table>'),
    write(Stream, '<tr><th>Course</th><th>Slot</th><th>Room</th><th>Lecturer</th></tr>'),
    forall(member(course(C, S, R, L), Timetable),
           format(Stream, '<tr><td>~w</td><td>~w</td><td>~w</td><td>~w</td></tr>', [C, S, R, L])),
    write(Stream, '</table></body></html>'),
    close(Stream).

% ------------------------ Type-Safe Helpers ------------------------

safe_string(S, Str) :- 
    (atom(S) -> atom_string(S, Str)
    ; number(S) -> number_string(S, Str)
    ; string(S) -> Str = S
    ).

safe_atom(S, Atom) :-
    safe_string(S, Str),
    atom_string(Atom, Str).

safe_number(S, N) :-
    safe_string(S, Str),
    atom_number(Str, N).

% ------------------------ CSV Loader ------------------------

load_all_csv(_) :-
    write('Loading courses...\n'), load_courses('courses.csv'),
    write('Loading lecturers...\n'), load_lecturers('lecturers.csv'),
    write('Loading groups...\n'), load_groups('groups.csv'),
    write('Loading rooms...\n'), load_rooms('rooms.csv'),
    write('Loading slots...\n'), load_slots('slots.csv'),
    write('Finished loading CSVs.\n').

load_courses(File) :-
    csv_read_file(File, [row(course_code, hours_per_week, student_groups)|Rows], []),
    forall(member(row(Code, Hours, Groups), Rows),
        ( safe_atom(Code, ACode),
          safe_number(Hours, HNum),
          safe_string(Groups, GStr),
          split_string(GStr, ";", "", GList),
          maplist(safe_atom, GList, GroupAtoms),
          assertz(course(ACode, HNum, GroupAtoms))
        )).

load_lecturers(File) :-
    csv_read_file(File, [row(lecturer_name, courses, unavailable_slots)|Rows], []),
    forall(member(row(Name, CourseStr, UnavStr), Rows),
        ( safe_atom(Name, AName),
          safe_string(CourseStr, CStr), split_string(CStr, ";", "", CList),
          safe_string(UnavStr, UStr), split_string(UStr, ";", "", UList),
          maplist(safe_atom, CList, CourseAtoms),
          maplist(safe_atom, UList, UnavAtoms),
          assertz(lecturer(AName, CourseAtoms, UnavAtoms))
        )).

load_groups(File) :-
    csv_read_file(File, [row(group_name, number_of_students)|Rows], []),
    forall(member(row(Name, Num), Rows),
        ( safe_atom(Name, AName),
          safe_number(Num, N),
          assertz(group(AName, N))
        )).

load_rooms(File) :-
    csv_read_file(File, [row(room_id, capacity)|Rows], []),
    forall(member(row(Room, Cap), Rows),
        ( safe_atom(Room, ARoom),
          safe_number(Cap, N),
          assertz(room(ARoom, N))
        )).

load_slots(File) :-
    csv_read_file(File, [row(time_slots)|Rows], []),
    forall(member(row(Slot), Rows),
        ( safe_atom(Slot, ASlot),
          assertz(time_slot(ASlot))
        )).

% ------------------------ CLI Constraint Helpers ------------------------

set_preferred_day(Day) :-
    retractall(preferred_day(_)),
    assertz(preferred_day(Day)).

set_exclude_slot(Slot) :-
    assertz(exclude_slot(Slot)).

% ------------------------ Main Entry Point ------------------------

main :- print_usage.

main(CSVOut, HTMLOut, PreferredDay, ExcludedSlots) :-
    write("Loading CSV files...\n"),
    load_all_csv(_),
    (nonvar(PreferredDay), PreferredDay \= '' -> set_preferred_day(PreferredDay) ; true),
    forall(member(Slot, ExcludedSlots), set_exclude_slot(Slot)),
    schedule(T),
    print_timetable(T),
    format("Exporting CSV to: ~w~n", [CSVOut]),
    export_csv(T, CSVOut),
    format("Exporting HTML to: ~w~n", [HTMLOut]),
    export_html(T, HTMLOut),
    halt.

print_usage :-
    nl,
    write("TimeTableGen - Prolog University Timetable Generator"), nl,
    write("---------------------------------------------------"), nl,
    write("Usage Example:"), nl,
    write("  swipl -s timetable.pl -g \"main('output.csv', 'timetable.html', wed, [fri_10])\""), nl,
    write("Arguments:"), nl,
    write("  - Input CSVs must exist in same folder: courses.csv, lecturers.csv, groups.csv, rooms.csv, slots.csv"), nl,
    write("  - Output CSV file path"), nl,
    write("  - Output HTML file path"), nl,
    write("  - Preferred teaching day (e.g. wed) or '' for none"), nl,
    write("  - List of excluded time slots (e.g. [mon_8, fri_10])"), nl,
    nl.
