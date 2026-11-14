# Barking Gecko Acoustics

Code and data for a passive acoustic monitoring (PAM) study of barking geckos (*Ptenopus*) across three habitats (Gravel Plains, River Bed, Sand Sea), plus a playback experiment, temperature loggers and burrow counts.

The repo has two independent parts:

| Folder | What it does |
|---|---|
| `pipeline/` | Cuts fixed-length clips out of long field recordings, then splits the clips evenly and reproducibly among annotators. Written to be reused on other recording projects. |
| `analysis/` | Rebuilds every figure and table in the paper from one Excel workbook. |

Everything runs from the repo root. Requires R (4.x).

```r
install.packages(c("tuneR", "yaml", "readxl", "dplyr", "ggplot2"))  # nlme ships with R
```

## 1. Audio pipeline (`pipeline/`)

### Trim

For every recorder, extracts the final N minutes before each clock hour (default: the last 10 minutes of each hour, for hours you choose). A clip written for 19:00 covers 18:50-19:00.

- Handles recorders that write one long file per night, and duty-cycled recorders (many short files with gaps, e.g. 55 s on / 5 s off). For duty-cycled recorders the clip is stitched from every file starting inside the window, and the log records the true audio duration.
- Writes one log row per clip (source files, number of segments, actual duration, status).

Input expectations: WAV files whose names contain a start timestamp, with one top-level folder per recorder. Files may be nested deeper inside each recorder folder. Defaults match `YYYYMMDD_HHMMSS.WAV` and recorder folders such as `GP2` or `GP2_Night2-4`.

### Assign

Divides clips among annotators so that each annotator's total is as even as integer counts allow, and so that no annotator gets a disproportionate share of one recorder.

- Greedy rule: each clip goes to the annotator with the fewest clips so far (ties: fewest from that clip's recorder, then random).
- Incremental: a ledger CSV records every assignment. Re-running only assigns clips that are not yet in the ledger, so a clip is never moved after an annotator may have started on it. Never edit or delete the ledger; it only grows.
- Clips are copied (not moved) into `Annotator_N/NightM/`, leaving the master clip folder intact.
- The ledger stores paths relative to the trimmed-clips folder, so the folder can be moved or remounted without clips looking new.

Trimming and assigning are deliberately separate scripts: an even split needs the whole pool of clips, so randomizing during extraction would not help.

### Usage

```bash
cp pipeline/config.example.yml pipeline/config.yml   # edit paths and settings
Rscript pipeline/run_trim.R
Rscript pipeline/run_assign.R
```

Both scripts take an optional config path as an argument. Settings in `config.yml`:

| Setting | Meaning |
|---|---|
| `clip_length_min` | Clip length in minutes (default 10) |
| `target_hours` | Keep clips ending on these hours of day, e.g. `[19, 20, 21, 22]`; `null` keeps every hour |
| `timestamp_regex`, `timestamp_format` | Where the start time sits in each filename, and how to parse it |
| `recorder_regex` | Matched against each top-level folder name to get the recorder ID |
| `n_annotators` | Number of annotators |
| `reference_night1_date` | Date of Night 1, used for `NightM` subfolders; `null` puts clips directly in `Annotator_N/` |
| `seed` | Optional random seed for reproducible assignments |

Recorder clocks are treated as local clock time with no timezone or daylight-saving adjustment.

### Tests

```bash
Rscript pipeline/tests/run_tests.R
```

Builds small synthetic recordings and checks clip timing, stitching, custom filename formats, ledger behavior across repeated runs, and balance across batches.

## 2. Analysis (`analysis/`)

```bash
Rscript analysis/run_all.R
```

Reads `data/BarkingGecko_Final_Data_Sheet.xlsx` and writes to `analysis/output/`. To use a different workbook location, set the `BGA_DATA` environment variable.

| Output | Script | Workbook tab |
|---|---|---|
| Fig 1, burrow counts by habitat | `burrow_figures.R` | Burrow_Density |
| Fig 2, species composition by habitat; supplementary figure, by location | `species_figures.R` | PAM_Processed |
| Fig 3, calls on hot vs cold nights; Fig 4, split by habitat | `temp_figures.R` | HOBO_Processed |
| Fig 5, response time by playback treatment | `playback_figures.R` | Playback |
| Table 1, Tukey HSD of temperature between nights | `tukey_table.R`, `tukey_table_latex.R` | HOBO |
| Table 2, min/max temperature by habitat and night | `temp_range_table_latex.R`, `temp_range_table.R` | HOBO |
| Mixed-model statistics (printed to console) | `temp_stats.R` | HOBO_Processed |

Tables are produced as PNG, CSV and LaTeX (`.tex` plus a standalone compilable version). Scripts read workbook columns by position, so if you add or reorder columns in a tab, update the matching `read_tab()` call in that script.

## Data

`data/BarkingGecko_Final_Data_Sheet.xlsx` holds all raw and processed data used in the paper; see its `Read_Me` and `Metadata` tabs. Audio files are not included.

## License

No license has been chosen yet.
