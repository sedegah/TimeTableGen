Here is a complete, professional-style `README.md` for your **TimeTableGen** repository:

---

```markdown
# TimeTableGen – Prolog University Timetable Generator

**TimeTableGen** is a constraint-based scheduling engine written in SWI-Prolog for generating valid university lecture timetables. It automatically assigns courses to time slots and rooms based on constraints such as lecturer availability, room capacities, and group size.

This project demonstrates logic programming and constraint resolution using real-world academic scheduling data from CSV input.

---

## Features

- CSV-driven input format for courses, lecturers, student groups, rooms, and time slots.
- Automatic assignment of courses to valid time slots and rooms.
- Room capacity constraints and group size checks.
- Lecturer availability constraints and course-hour tracking.
- No time slot, room, or lecturer conflicts.
- Command-line constraints:
  - Specify preferred teaching days.
  - Exclude specific time slots.
- Outputs:
  - Console (ASCII-formatted) table.
  - CSV export of timetable.
  - HTML export for browser-friendly viewing.

---

## File Structure

```

timetable.pl         % Main Prolog codebase
sample\_input.csv     % Example CSV input
output.csv           % Generated CSV timetable (output)
timetable.html       % Generated HTML timetable (output)

````

---

## Input Format

All scheduling data is provided via a single CSV file, structured in **sections separated by empty lines**. Example:

```csv
course_code,hours_per_week,student_groups
cs101,3,group1;group2
cs102,2,group1

lecturer_name,courses,unavailable_slots
john,cs101,mon_8;fri_10
alice,cs102,
bob,ma101,wed_10

group_name,number_of_students
group1,45
group2,25

room_id,capacity
r101,50
r102,30

time_slots
mon_8
mon_10
tue_8
tue_10
wed_8
wed_10
thu_8
thu_10
fri_8
fri_10
````

---

## How to Run

### Option 1: Run interactively inside SWI-Prolog

```prolog
?- load_all_csv('sample_input.csv').
?- set_preferred_day(wed).           % optional
?- set_exclude_slot(fri_10).         % optional
?- schedule(Timetable).
?- print_timetable(Timetable).
?- export_csv(Timetable, 'output.csv').
?- export_html(Timetable, 'timetable.html').
```

### Option 2: Run everything from CLI (batch mode)

```bash
swipl -s timetable.pl -g "main('sample_input.csv', 'output.csv', 'timetable.html', wed, [fri_10])"
```

To run without constraints:

```bash
swipl -s timetable.pl -g "main('sample_input.csv', 'output.csv', 'timetable.html', '', [])"
```

To display help:

```bash
swipl -s timetable.pl -g main
```

---

## Available Functions

### Scheduling & Validation

* `schedule(-Timetable)`
  Generate a valid timetable based on current facts and constraints.

* `valid_timetable(+Timetable)`
  Ensures no room, lecturer, or course conflicts.

* `assign_hours/6`
  Assigns a lecturer to one or more valid time slots.

### CSV Input & Output

* `load_all_csv(+FilePath)`
  Load and assert all facts from a structured CSV file.

* `export_csv(+Timetable, +FilePath)`
  Save the timetable to a CSV file.

* `print_timetable(+Timetable)`
  Pretty-print timetable to console.

* `export_html(+Timetable, +FilePath)`
  Export timetable as a styled HTML file.

### CLI Constraint Helpers

* `set_preferred_day(+DayAtom)`
  Bias scheduling toward a preferred day prefix (e.g. `wed`).

* `set_exclude_slot(+SlotAtom)`
  Prevent use of a specific time slot (e.g. `fri_10`).

### Entry Point

* `main/0`
  Shows usage instructions.

* `main(+CSVIn, +CSVOut, +HTMLOut, +PreferredDay, +ExcludedSlots)`
  End-to-end run with custom settings.

---

## Example Output (Console)

```
------------------------------------------
| Course |     Slot    | Room | Lecturer |
------------------------------------------
| cs101  | wed_8       | r101 | john     |
| cs101  | tue_8       | r101 | john     |
| cs101  | thu_10      | r101 | john     |
| cs102  | mon_10      | r102 | alice    |
| cs102  | tue_10      | r102 | alice    |
------------------------------------------
```

---

## Requirements

* [SWI-Prolog](https://www.swi-prolog.org/) ≥ 8.0
  (Ensure `library(csv)` is available.)

---

## License

MIT License. For educational use, research, and extension.

---

## Author

Created by Kimathi Elikplim Sedegah
University of Ghana | Computer Science
GitHub: [@sedegah](https://github.com/sedegah)

