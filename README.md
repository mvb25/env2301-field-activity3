# ENV2301 Field Activity 3 — field data app

A deliberately simple phone-first Shiny app for collecting two kinds of measurements in two vegetation patches:

- **GRS densitometer:** canopy cover (%)
- **Kestrel 5500:** air temperature (°C), relative humidity (%), wind speed (m/s)

The porometer/fluorometer is intentionally **not** included. It can be used separately for instructor-led discussion of additional sampling issues.

## Student workflow

Each student group should use one phone. Give each group its own link:

- `https://YOUR-APP-URL/?group=G1`
- `https://YOUR-APP-URL/?group=G2`
- ...
- `https://YOUR-APP-URL/?group=G6`

The group ID is therefore entered once. Patch, method, person ID, and Kestrel ID are retained until changed.

The app automatically assigns point IDs. Ordinary points are `D01`, `D02`, ... for densitometer measurements and `K01`, `K02`, ... for Kestrel measurements. After a successful save, the next ordinary point number advances automatically.

For each observation students choose one of:

- **New point** — use the next point number.
- **Repeat previous** — another measurement at exactly the previous point.
- **Shared reference** — densitometer only; records the marked common location as `R-A` or `R-B` depending on patch.

For the densiometer, students also select a person ID (`P1`–`P8`). These IDs are deliberately anonymous and let the class examine observer-related variation.

Students should mark the point IDs on their **paper sketch of their sampling pattern** if useful. The sketches provide the spatial context; the app does not use GPS.

## Instructor view

Open:

`https://YOUR-APP-URL/?view=instructor`

The instructor view contains:

- current observation count and CSV download;
- group-specific student links;
- patch comparison plots with raw observations;
- group-level means, SDs, and sample sizes;
- the shared-reference densiometer measurements;
- a sample-size/resampling demonstration;
- the raw data table.

If you set the environment variable `INSTRUCTOR_PIN`, the instructor view asks for that PIN. If it is unset, the instructor view opens directly.

## Data structure

All observations feed into one table with these columns:

`timestamp_server, timestamp_app, session_id, group_id, patch, method, point_id, point_number, is_repeat, is_reference, person_id, instrument_id, temperature_c, rh_pct, wind_ms, canopy_pct`

This structure supports comparison of:

- within-patch variation;
- differences between the two particular patches;
- group-to-group differences in patch estimates;
- observer variation at the shared densiometer reference point;
- Kestrel variables with contrasting variability;
- repeated measurements at one point;
- consequences of changing sample size.

## Backend: Google Sheet via Apps Script

The deployed Shiny app should **not** use a CSV file as its permanent database. Posit Connect Cloud runtime files are not persistent. The included Apps Script provides a small authenticated endpoint that writes every observation into one Google Sheet.

### 1. Create the Sheet

Create a blank Google Sheet, for example `ENV2301_FA3_data`.

Copy its spreadsheet ID from the URL. It is the long string between `/d/` and `/edit`.

### 2. Install the Apps Script

In the Sheet, open **Extensions > Apps Script**.

Replace the default code with `backend/Code.gs` from this project.

In **Project Settings > Script Properties**, add:

- `SHEET_ID` = your spreadsheet ID
- `API_TOKEN` = a long random token that you choose

### 3. Deploy the Apps Script

Choose **Deploy > New deployment > Web app**.

Use:

- Execute as: **Me**
- Who has access: **Anyone**, if your institutional Google Workspace policy permits this

Copy the deployed URL ending in `/exec`.

The API token protects reads and writes even though the endpoint itself is reachable publicly. The class data contain only anonymous group/person IDs.

If NUS Google Workspace policy does not allow an anonymous Apps Script web app, the Shiny front end can be retained and the backend swapped for another persistent service. Do not rely on Connect Cloud's local filesystem for class data.

## Test locally in RStudio

From the app directory:

```r
install.packages(c("shiny", "httr2", "jsonlite", "rsconnect"))

Sys.setenv(
  DATA_API_URL = "YOUR_APPS_SCRIPT_EXEC_URL",
  DATA_API_TOKEN = "YOUR_TOKEN"
)

shiny::runApp()
```

If `DATA_API_URL` is not set, the app starts in **LOCAL TEST MODE** and writes to `data/field_activity3.csv`. This is useful only for testing on one computer. It is not safe shared storage for the field class.

## Prepare for Posit Connect Cloud

Connect Cloud requires `manifest.json` for Shiny for R deployments. From the app directory, run:

```r
source("setup_manifest.R")
```

This creates `manifest.json` using `rsconnect::writeManifest()`.

Commit the app files, including `manifest.json`, to a GitHub repository. Publish the repository as a Shiny for R application in Posit Connect Cloud, with `app.R` as the primary file.

In **Advanced settings**, add these secret variables:

- `DATA_API_URL`
- `DATA_API_TOKEN`
- optionally `INSTRUCTOR_PIN`

Do not put the token in `app.R`, GitHub, or the shared spreadsheet.

## Field-use recommendations

1. Use **one phone per group** so point numbering remains simple.
2. Give groups their group-specific URL or QR code.
3. Mark one physical shared densiometer reference point in each patch.
4. Ask groups to plan and sketch how they will sample each patch before starting.
5. Ask them to spread observations through the patch while deciding for themselves how to obtain a good estimate of the patch mean.
6. Keep Kestrel units fixed to °C, %RH, and m/s for the activity.
7. Test the app on the actual mobile network/Wi-Fi before class.

## Files

- `app.R` — Shiny app
- `www/styles.css` — phone-first styling
- `backend/Code.gs` — Google Apps Script / Google Sheets backend
- `setup_manifest.R` — creates Connect Cloud `manifest.json`
- `data/` — local-test storage only

## Interface update — v1.1
The three Kestrel measurement fields are stacked vertically at all screen widths. This avoids horizontal overflow in narrow browser windows and gives the same reliable layout on phones. The data schema and backend are unchanged, so an existing Google Sheet / Apps Script backend does not need to be recreated.


## Version 1.2 note
Removed the explicit `req_method("POST")` call so Google Apps Script ContentService redirects are handled correctly by httr2/libcurl.


## Shared densiometer reference points

Each patch can contain five deliberately selected shared reference locations spanning contrasting canopy openness. In the app, choose **Shared reference** and then select **R1–R5**. The stored IDs are `R-A1` … `R-A5` in Patch A and `R-B1` … `R-B5` in Patch B. These points are intended primarily for comparing observer measurements under the same canopy conditions. Because they are deliberately selected and repeatedly measured, do not treat all reference-point records as independent observations when estimating the ordinary patch mean.
