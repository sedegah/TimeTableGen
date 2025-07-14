% ------------------------     
% TimeTableGen - Prolog University Timetable Generator
% ------------------------

:- use_module(library(csv)).
:- use_module(library(random)).
:- dynamic course/3, lecturer/3, room/2, group/2, time_slot/1.
:- dynamic preferred_day/1, exclude_slot/1.

% ------------------------ Scheduling Core ------------------------

schedule(Timetable) :-
    findall(CourseCode-Hours-Groups, course(CourseCode, Hours, Groups), CourseList),
    writeln("Courses to schedule:"), writeln(CourseList),
    assign_slots(CourseList, [], Timetable),
    print_conflict_summary(Timetable).

assign_slots([], Timetable, Timetable).
assign_slots([CourseCode-Hours-Groups | Rest], Acc, Timetable) :-
    findall(Lecturer-Unavailable,
            (lecturer(Lecturer, Courses, Unavailable), member(CourseCode, Courses)),
            LecturerOptions),
    try_lecturers(LecturerOptions, CourseCode, Hours, Groups, Acc, NewAcc),
    assign_slots(Rest, NewAcc, Timetable).

try_lecturers([], Course, H, G, _, _) :-
    format("⚠️ Failed to assign course ~w (Hours: ~w, Groups: ~w)~n", [Course, H, G]),
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
    findall(S, time_slot(S), Slots),
    random_permutation(Slots, Shuffled),
    member(Slot, Shuffled),
    room(Room, Capacity),
    Capacity >= Size,
    \+ member(Slot, Unavailable),
    \+ exclude_slot(Slot),
    ( preferred_day(Day) -> atom_concat(Day, _, Slot) ; true ),
    NewH is Hours - 1,
    assign_hours(Course, Lecturer, NewH, Groups, Unavailable, Rest).

total_group_size(Groups, Total) :-
    findall(Size, (member(G, Groups), group(G, Size)), Sizes),
    sum_list(Sizes, Total).

print_conflict_summary(Timetable) :-
    (no_conflicts(Timetable) -> true ; writeln("⚠️ Room or Lecturer conflict detected")),
    (no_double_bookings(Timetable) -> true ; writeln("⚠️ Course double-booked")),
    true.

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
        ( atom_string(ACode, Code),
          HNum = Hours,
          split_string(Groups, ";", "", GList),
          maplist(atom_string, GroupAtoms, GList),
          assertz(course(ACode, HNum, GroupAtoms))
        )).

load_lecturers(File) :-
    csv_read_file(File, [row(lecturer_name, courses, unavailable_slots)|Rows], []),
    forall(member(row(Name, CourseStr, UnavStr), Rows),
        ( atom_string(AName, Name),
          split_string(CourseStr, ";", "", CList),
          split_string(UnavStr, ";", "", UList),
          maplist(atom_string, CourseAtoms, CList),
          maplist(atom_string, UnavAtoms, UList),
          assertz(lecturer(AName, CourseAtoms, UnavAtoms))
        )).

load_groups(File) :-
    csv_read_file(File, [row(group_name, number_of_students)|Rows], []),
    forall(member(row(Name, Num), Rows),
        ( atom_string(AName, Name),
          N = Num,
          assertz(group(AName, N))
        )).

load_rooms(File) :-
    csv_read_file(File, [row(room_id, capacity)|Rows], []),
    forall(member(row(Room, Cap), Rows),
        ( atom_string(ARoom, Room),
          N = Cap,
          assertz(room(ARoom, N))
        )).

load_slots(File) :-
    csv_read_file(File, [row(time_slots)|Rows], []),
    forall(member(row(Slot), Rows),
        ( atom_string(ASlot, Slot),
          assertz(time_slot(ASlot))
        )).

% ------------------------ CLI Entry Point ------------------------

main :-
    current_prolog_flag(argv, Argv),
    ( Argv = [CSVOut, HTMLOut, PreferredDayRaw, ExcludedRaw] ->
        atom_string(PreferredDay, PreferredDayRaw),
        parse_exclude_slots(ExcludedRaw, ExcludedSlots),
        run_scheduler(CSVOut, HTMLOut, PreferredDay, ExcludedSlots)
    ; Argv = [CSVOut, HTMLOut] ->
        run_scheduler(CSVOut, HTMLOut, '', [])
    ; print_usage, halt(1)
    ).

parse_exclude_slots(ExStr, Slots) :-
    ( sub_string(ExStr, _, _, _, "[") ->
        term_string(Slots, ExStr)
    ; split_string(ExStr, ";,", " ", SlotStrings), maplist(atom_string, SlotStrings, Slots)
    ).

run_scheduler(CSVOut, HTMLOut, PreferredDay, ExcludedSlots) :-
    write("Loading CSV files...\n"),
    load_all_csv(_),
    (PreferredDay \= '' -> retractall(preferred_day(_)), assertz(preferred_day(PreferredDay)) ; true),
    retractall(exclude_slot(_)),
    forall(member(Slot, ExcludedSlots), assertz(exclude_slot(Slot))),
    ( schedule(Timetable) ->
        print_timetable(Timetable),
        format("Exporting CSV to: ~w~n", [CSVOut]),
        export_csv(Timetable, CSVOut),
        format("Exporting HTML to: ~w~n", [HTMLOut]),
        export_html(Timetable, HTMLOut),
        halt
    ; writeln("⚠️ Could not perfectly generate timetable, partial assignments shown above."), halt(1)
    ).

print_usage :-
    nl,
    write("TimeTableGen - Prolog University Timetable Generator"), nl,
    write("---------------------------------------------------"), nl,
    write("Usage Examples:"), nl,
    write("  swipl -s timetable.pl -- output.csv output.html wed \"[fri_10]\""), nl,
    write("  swipl -s timetable.pl -- output.csv output.html"), nl,
    nl,
    write("Arguments:"), nl,
    write("  - Input CSVs must exist in same folder: courses.csv, lecturers.csv, groups.csv, rooms.csv, slots.csv"), nl,
    write("  - Output CSV file path"), nl,
    write("  - Output HTML file path"), nl,
    write("  - (Optional) Preferred teaching day (e.g. wed)"), nl,
    write("  - (Optional) Excluded time slots (e.g. [mon_8, fri_10])"), nl,
    nl.

:- initialization(main, main).
