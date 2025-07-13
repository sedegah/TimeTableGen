Got it — here's the **corrected and professional version** of that section using proper formatting and explanation for each file. I've also cleaned up the entire README structure for clarity.

---

### ✅ Full, Cleaned-up `README.md` (Final Version)

````markdown
# TimeTableGen – Prolog University Timetable Generator

**TimeTableGen** is a logic-based timetable generator built in SWI-Prolog. It assigns university lectures to time slots and rooms while satisfying constraints like lecturer availability, room capacity, and student group size.

---

## Features

- Loads data from a structured CSV file.
- Automatically generates a valid, conflict-free timetable.
- Honors:
  - Course hour requirements
  - Lecturer availability
  - Room capacity
  - Student group overlaps
- Prevents conflicts (no double-booked lecturers, rooms, or groups).
- Optional command-line constraints:
  - Preferred teaching day (e.g., `wed`)
  - Excluded time slots (e.g., `fri_10`)
- Exports timetable in:
  - Console-friendly ASCII format
  - CSV file (`output.csv`)
  - Styled HTML (`timetable.html`)

---

## Project Files

| File              | Description                                 |
|-------------------|---------------------------------------------|
| `timetable.pl`     | Main Prolog source code                     |
| `sample_input.csv` | Sample input dataset in structured CSV form |
| `output.csv`       | Timetable output in CSV format              |
| `timetable.html`   | Timetable output in browser-viewable HTML   |

---

## CSV Input Format

The CSV file is separated into five sections by **empty lines**:

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

### Option 1: Interactively in SWI-Prolog

```prolog
?- load_all_csv('sample_input.csv').
?- set_preferred_day(wed).         % Optional
?- set_exclude_slot(fri_10).       % Optional
?- schedule(Timetable).
?- print_timetable(Timetable).
?- export_csv(Timetable, 'output.csv').
?- export_html(Timetable, 'timetable.html').
```

### Option 2: Single-Command CLI Mode

```bash
swipl -s timetable.pl -g "main('sample_input.csv', 'output.csv', 'timetable.html', wed, [fri_10])"
```

To run without constraints:

```bash
swipl -s timetable.pl -g "main('sample_input.csv', 'output.csv', 'timetable.html', '', [])"
```

---

## Key Functions

### Scheduling

* `schedule(-Timetable)`
  Generates a conflict-free timetable.

* `valid_timetable(+Timetable)`
  Ensures the timetable is valid (no overlaps or overbookings).

### Input/Output

* `load_all_csv(+FilePath)`
  Parses a CSV file and asserts facts.

* `export_csv(+Timetable, +FilePath)`
  Saves the timetable as a CSV file.

* `export_html(+Timetable, +FilePath)`
  Saves the timetable as a styled HTML file.

* `print_timetable(+Timetable)`
  Displays the timetable as a formatted table in the terminal.

### Constraints

* `set_preferred_day(+DayAtom)`
  Prefer slots that begin with a specific weekday (e.g., `wed`).

* `set_exclude_slot(+SlotAtom)`
  Exclude specific time slots from consideration.

### Entry Point

* `main/0`
  Displays help/usage instructions.

* `main(+CSVIn, +CSVOut, +HTMLOut, +PreferredDay, +ExcludedSlots)`
  Full run with parameters for automation and CLI use.

---

## Requirements

* [SWI-Prolog](https://www.swi-prolog.org/) (v8.0 or later)
* CSV input formatted as specified

---

## License

MIT License

---

## Author

Kimathi Elikplim Sedegah
University of Ghana – Computer Science
GitHub: [@sedegah](https://github.com/sedegah)

