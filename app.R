library(shiny)
library(httr2)
library(jsonlite)

# ENV2301 Field Activity 3
# Student data-entry app + instructor analysis view.
#
# Production backend: Google Apps Script endpoint writing to Google Sheets.
# Configure with environment variables:
#   DATA_API_URL   = deployed Apps Script /exec URL
#   DATA_API_TOKEN = token matching Apps Script property API_TOKEN
# Optional:
#   INSTRUCTOR_PIN = PIN required to open ?view=instructor
#
# Group-specific student links use ?group=G1 ... ?group=G9.

DATA_API_URL <- Sys.getenv("DATA_API_URL", "")
DATA_API_TOKEN <- Sys.getenv("DATA_API_TOKEN", "")
INSTRUCTOR_PIN <- Sys.getenv("INSTRUCTOR_PIN", "")
LOCAL_DATA_FILE <- file.path("data", "field_activity3.csv")

DATA_COLUMNS <- c(
  "timestamp_server", "timestamp_app", "session_id",
  "group_id", "patch", "method", "point_id", "point_number",
  "is_repeat", "is_reference", "person_id", "instrument_id",
  "temperature_c", "rh_pct", "wind_ms", "canopy_pct"
)

NUMERIC_COLUMNS <- c("point_number", "temperature_c", "rh_pct", "wind_ms", "canopy_pct")
LOGICAL_COLUMNS <- c("is_repeat", "is_reference")

empty_data <- function() {
  out <- as.data.frame(setNames(replicate(length(DATA_COLUMNS), logical(0), simplify = FALSE), DATA_COLUMNS))
  for (nm in setdiff(DATA_COLUMNS, c(NUMERIC_COLUMNS, LOGICAL_COLUMNS))) out[[nm]] <- character(0)
  for (nm in NUMERIC_COLUMNS) out[[nm]] <- numeric(0)
  for (nm in LOGICAL_COLUMNS) out[[nm]] <- logical(0)
  out
}


normalise_timestamp_column <- function(z) {
  n <- length(z)
  out <- rep(NA_character_, n)

  # Preserve real R date-time objects.
  if (inherits(z, "POSIXt")) {
    return(format(as.POSIXct(z), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"))
  }

  if (inherits(z, "Date")) {
    return(format(as.POSIXct(z, tz = "UTC"), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"))
  }

  raw <- trimws(as.character(z))
  raw[raw %in% c("", "NA", "NULL", "null")] <- NA_character_

  excel_origin <- as.POSIXct("1899-12-30 00:00:00", tz = "UTC")

  for (i in seq_len(n)) {
    s <- raw[i]
    if (is.na(s) || !nzchar(s)) next

    # Excel / Google Sheets serial date-time, e.g. 46290.04267.
    num <- suppressWarnings(as.numeric(s))
    if (length(num) == 1 && is.finite(num) && num > 20000 && num < 80000) {
      tt <- excel_origin + num * 86400
      out[i] <- format(tt, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
      next
    }

    # Epoch milliseconds or seconds, included defensively.
    if (length(num) == 1 && is.finite(num) && num > 1e12) {
      tt <- as.POSIXct(num / 1000, origin = "1970-01-01", tz = "UTC")
      out[i] <- format(tt, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
      next
    }
    if (length(num) == 1 && is.finite(num) && num > 1e9) {
      tt <- as.POSIXct(num, origin = "1970-01-01", tz = "UTC")
      out[i] <- format(tt, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
      next
    }

    # Otherwise retain the timestamp text as supplied by the live app/backend.
    out[i] <- s
  }

  out
}

normalise_data <- function(x) {
  if (is.null(x) || length(x) == 0) return(empty_data())
  x <- as.data.frame(x, stringsAsFactors = FALSE)

  for (nm in DATA_COLUMNS) {
    if (!nm %in% names(x)) x[[nm]] <- NA
  }

  x <- x[, DATA_COLUMNS, drop = FALSE]

  # Normalize timestamps before the rest of the dashboard sees them.
  x$timestamp_server <- normalise_timestamp_column(x$timestamp_server)
  x$timestamp_app <- normalise_timestamp_column(x$timestamp_app)

  for (nm in NUMERIC_COLUMNS) {
    x[[nm]] <- suppressWarnings(as.numeric(x[[nm]]))
  }

  for (nm in LOGICAL_COLUMNS) {
    z <- x[[nm]]
    if (is.logical(z)) {
      x[[nm]] <- z
    } else {
      x[[nm]] <- tolower(as.character(z)) %in% c("true", "t", "1", "yes")
    }
  }

  x
}


backend_is_remote <- function() nzchar(DATA_API_URL)

append_observation <- function(obs) {
  if (backend_is_remote()) {
    payload <- c(as.list(obs), list(token = DATA_API_TOKEN))
    # req_body_json() automatically makes this a POST request. Do not also use
    # req_method("POST"): that creates a custom POST method in libcurl, which
    # can be retained when Google Apps Script redirects ContentService output
    # to script.googleusercontent.com and can produce a false HTTP 405 after
    # the row has already been written.
    response <- request(DATA_API_URL) |>
      req_body_json(payload, auto_unbox = TRUE, null = "null") |>
      req_timeout(seconds = 15) |>
      req_perform()

    body <- resp_body_json(response, simplifyVector = TRUE)
    if (is.null(body$status) || !identical(as.character(body$status), "ok")) {
      stop(if (!is.null(body$message)) body$message else "Data service returned an error.")
    }
    return(invisible(TRUE))
  }

  # Local testing only. This is not safe persistent storage for cloud deployment.
  dir.create(dirname(LOCAL_DATA_FILE), recursive = TRUE, showWarnings = FALSE)
  row <- as.data.frame(obs, stringsAsFactors = FALSE)
  for (nm in DATA_COLUMNS) if (!nm %in% names(row)) row[[nm]] <- NA
  row <- row[, DATA_COLUMNS, drop = FALSE]
  write.table(
    row,
    file = LOCAL_DATA_FILE,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(LOCAL_DATA_FILE),
    append = file.exists(LOCAL_DATA_FILE),
    qmethod = "double",
    na = ""
  )
  invisible(TRUE)
}

read_observations <- function() {
  if (backend_is_remote()) {
    response <- request(DATA_API_URL) |>
      req_url_query(action = "read", token = DATA_API_TOKEN) |>
      req_timeout(seconds = 20) |>
      req_perform()

    body <- resp_body_json(response, simplifyVector = TRUE)
    if (is.null(body$status) || !identical(as.character(body$status), "ok")) {
      stop(if (!is.null(body$message)) body$message else "Could not read data.")
    }
    if (is.null(body$rows) || length(body$rows) == 0) return(empty_data())
    return(normalise_data(body$rows))
  }

  if (!file.exists(LOCAL_DATA_FILE)) return(empty_data())
  normalise_data(read.csv(LOCAL_DATA_FILE, stringsAsFactors = FALSE, check.names = FALSE))
}

iso_time <- function(x = Sys.time()) {
  format(x, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

random_session_id <- function() {
  paste0(
    format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC"), "-",
    paste(sample(c(letters, 0:9), 7, replace = TRUE), collapse = "")
  )
}

valid_number <- function(x, min_value, max_value) {
  length(x) == 1 && !is.null(x) && !is.na(x) && is.finite(x) && x >= min_value && x <= max_value
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

VARIABLE_LABELS <- c(
  canopy_pct = "Canopy cover (%)",
  temperature_c = "Temperature (°C)",
  rh_pct = "Relative humidity (%)",
  wind_ms = "Wind speed (m/s)"
)

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=1"),
    tags$title("ENV2301 Field Activity 3"),
    includeCSS(file.path("www", "styles.css"))
  ),
  uiOutput("root_ui")
)

server <- function(input, output, session) {
  session_id <- random_session_id()
  group_id <- reactiveVal(NULL)
  instructor_view <- reactiveVal(FALSE)
  instructor_unlocked <- reactiveVal(!nzchar(INSTRUCTOR_PIN))
  status_message <- reactiveVal("")
  status_type <- reactiveVal("ok")
  last_submit_at <- reactiveVal(as.POSIXct("1970-01-01", tz = "UTC"))

  keys <- c("Densitometer_A", "Densitometer_B", "Kestrel_A", "Kestrel_B")
  counters <- reactiveVal(setNames(as.list(rep(1L, length(keys))), keys))
  last_points <- reactiveVal(setNames(as.list(rep(NA_character_, length(keys))), keys))

  analysis_store <- reactiveVal(empty_data())
  analysis_message <- reactiveVal("Press Refresh data to load observations.")

  # Read URL parameters once the browser is connected.
  observeEvent(session$clientData$url_search, {
    q <- parseQueryString(session$clientData$url_search %||% "")
    if (!is.null(q$view) && identical(tolower(q$view), "instructor")) {
      instructor_view(TRUE)
    }
    if (!is.null(q$group)) {
      g <- toupper(q$group)
      if (g %in% paste0("G", 1:9)) group_id(g)
    }
  }, once = TRUE, ignoreInit = FALSE)

  counter_key <- function(method, patch) paste(method, patch, sep = "_")

  get_counter <- function(method, patch) {
    vals <- counters()
    key <- counter_key(method, patch)
    as.integer(vals[[key]] %||% 1L)
  }

  set_counter <- function(method, patch, value) {
    vals <- counters()
    vals[[counter_key(method, patch)]] <- as.integer(value)
    counters(vals)
  }

  get_last_point <- function(method, patch) {
    vals <- last_points()
    vals[[counter_key(method, patch)]] %||% NA_character_
  }

  set_last_point <- function(method, patch, value) {
    vals <- last_points()
    vals[[counter_key(method, patch)]] <- as.character(value)
    last_points(vals)
  }

  initialise_counters <- function(g) {
    dat <- tryCatch(read_observations(), error = function(e) NULL)
    if (is.null(dat) || nrow(dat) == 0) return(invisible(NULL))
    dat <- dat[dat$group_id == g, , drop = FALSE]
    if (nrow(dat) == 0) return(invisible(NULL))

    for (method in c("Densitometer", "Kestrel")) {
      prefix <- if (method == "Densitometer") "D" else "K"
      for (patch in c("A", "B")) {
        z <- dat[dat$method == method & dat$patch == patch & grepl(paste0("^", prefix, "[0-9]+$"), dat$point_id), , drop = FALSE]
        if (nrow(z) > 0) {
          nums <- suppressWarnings(as.integer(sub(paste0("^", prefix), "", z$point_id)))
          nums <- nums[is.finite(nums)]
          if (length(nums)) set_counter(method, patch, max(nums, na.rm = TRUE) + 1L)
        }
      }
    }
  }

  observeEvent(group_id(), {
    g <- group_id()
    if (!is.null(g)) initialise_counters(g)
  }, ignoreNULL = TRUE)

  observeEvent(input$set_group, {
    req(input$group_select)
    group_id(input$group_select)
  })

  observeEvent(input$change_group, {
    group_id(NULL)
    status_message("")
  })

  observeEvent(input$unlock_instructor, {
    if (!nzchar(INSTRUCTOR_PIN) || identical(as.character(input$instructor_pin), INSTRUCTOR_PIN)) {
      instructor_unlocked(TRUE)
    } else {
      showNotification("Incorrect PIN.", type = "error", duration = 3)
    }
  })

  current_point <- reactive({
    req(input$method, input$patch)
    mode <- input$point_mode %||% "new"
    method <- input$method
    patch <- input$patch

    if (method == "Densitometer" && identical(mode, "reference")) {
      ref_no <- input$reference_no %||% "R1"
      return(paste0("R-", patch, sub("^R", "", ref_no)))
    }
    if (identical(mode, "repeat")) {
      lp <- get_last_point(method, patch)
      if (!is.na(lp) && nzchar(lp)) return(lp)
    }
    prefix <- if (method == "Densitometer") "D" else "K"
    paste0(prefix, sprintf("%02d", get_counter(method, patch)))
  })

  output$current_point <- renderText(current_point())

  output$root_ui <- renderUI({
    if (instructor_view()) {
      if (!instructor_unlocked()) {
        return(div(
          class = "app-shell compact-shell",
          h2("ENV2301 Field Activity 3"),
          p(class = "subtitle", "Instructor view"),
          passwordInput("instructor_pin", "Instructor PIN"),
          actionButton("unlock_instructor", "Open instructor view", class = "btn-primary big-button")
        ))
      }
      return(instructor_ui())
    }

    if (is.null(group_id())) return(group_setup_ui())
    student_ui()
  })

  group_setup_ui <- reactive({
    div(
      class = "app-shell compact-shell",
      h2("ENV2301 Field Activity 3"),
      p(class = "subtitle", "Select your group once for this session."),
      selectInput("group_select", "Group", choices = paste0("G", 1:9), selected = "G1"),
      actionButton("set_group", "Start", class = "btn-primary big-button"),
      if (!backend_is_remote()) div(class = "warning-box", "LOCAL TEST MODE — data are not using the shared Google Sheet backend.")
    )
  })

  student_ui <- reactive({
    div(
      class = "app-shell",
      div(
        class = "topbar",
        div(strong("ENV2301 Field Activity 3"), br(), span(class = "small-muted", paste("Group", group_id()))),
        actionLink("change_group", "Change group", class = "small-link")
      ),
      if (!backend_is_remote()) div(class = "warning-box", "LOCAL TEST MODE — do not use this mode for the class field session."),
      div(
        class = "control-card",
        radioButtons(
          "method", "Method",
          choices = c("Densitometer", "Kestrel"),
          selected = "Densitometer", inline = TRUE
        ),
        radioButtons(
          "patch", "Patch",
          choices = c("A", "B"),
          selected = "A", inline = TRUE
        )
      ),
      uiOutput("method_fields"),
      div(class = "status-line", uiOutput("status_ui"))
    )
  })

  output$method_fields <- renderUI({
    req(input$method, input$patch)

    point_modes <- if (input$method == "Densitometer") {
      c("New point" = "new", "Repeat previous" = "repeat", "Shared reference" = "reference")
    } else {
      c("New point" = "new", "Repeat previous" = "repeat")
    }

    method_specific <- if (input$method == "Densitometer") {
      tagList(
        conditionalPanel(
          condition = "input.point_mode == 'reference'",
          radioButtons(
            "reference_no", "Shared reference point",
            choices = paste0("R", 1:5), selected = "R1", inline = TRUE
          )
        ),
        radioButtons(
          "person_id", "Person",
          choices = paste0("P", 1:5), selected = "P1", inline = TRUE
        ),
        numericInput("canopy_pct", "Canopy cover (%)", value = NA, min = 0, max = 100, step = 0.1)
      )
    } else {
      tagList(
        radioButtons(
          "instrument_id", "Kestrel",
          choices = paste0("K", 1:3), selected = "K1", inline = TRUE
        ),
        div(
          class = "three-grid",
          numericInput("temperature_c", "Temperature (°C)", value = NA, step = 0.1),
          numericInput("rh_pct", "Relative humidity (%)", value = NA, min = 0, max = 100, step = 0.1),
          numericInput("wind_ms", "Wind speed (m/s)", value = NA, min = 0, step = 0.1)
        )
      )
    }

    div(
      class = "control-card measurement-card",
      radioButtons("point_mode", "Point", choices = point_modes, selected = "new", inline = TRUE),
      div(class = "point-display", span("Recording as "), strong(textOutput("current_point", inline = TRUE))),
      method_specific,
      actionButton("submit_obs", "SAVE MEASUREMENT", class = "btn-primary submit-button")
    )
  })

  output$status_ui <- renderUI({
    msg <- status_message()
    if (!nzchar(msg)) return(NULL)
    cls <- if (status_type() == "error") "status-error" else "status-ok"
    span(class = cls, msg)
  })

  observeEvent(input$submit_obs, {
    # Ignore accidental rapid double taps.
    if (as.numeric(difftime(Sys.time(), last_submit_at(), units = "secs")) < 0.8) return()
    last_submit_at(Sys.time())

    req(group_id(), input$method, input$patch)
    method <- input$method
    patch <- input$patch
    mode <- input$point_mode %||% "new"

    if (identical(mode, "repeat")) {
      lp <- get_last_point(method, patch)
      if (is.na(lp) || !nzchar(lp)) {
        status_type("error")
        status_message("No previous point to repeat yet.")
        return()
      }
    }

    point_id <- current_point()
    point_num <- if (grepl("^[DK][0-9]+$", point_id)) suppressWarnings(as.integer(sub("^[DK]", "", point_id))) else NA_integer_

    obs <- list(
      timestamp_server = "",
      timestamp_app = iso_time(),
      session_id = session_id,
      group_id = group_id(),
      patch = patch,
      method = method,
      point_id = point_id,
      point_number = point_num,
      is_repeat = identical(mode, "repeat"),
      is_reference = grepl("^R-", point_id),
      person_id = "",
      instrument_id = "",
      temperature_c = NA_real_,
      rh_pct = NA_real_,
      wind_ms = NA_real_,
      canopy_pct = NA_real_
    )

    if (method == "Densitometer") {
      if (is.null(input$person_id) || !nzchar(input$person_id)) {
        status_type("error"); status_message("Select the person taking the reading."); return()
      }
      if (!valid_number(input$canopy_pct, 0, 100)) {
        status_type("error"); status_message("Enter canopy cover between 0 and 100%."); return()
      }
      obs$person_id <- input$person_id
      obs$canopy_pct <- as.numeric(input$canopy_pct)
    } else {
      if (is.null(input$instrument_id) || !nzchar(input$instrument_id)) {
        status_type("error"); status_message("Select the Kestrel ID."); return()
      }
      if (!valid_number(input$temperature_c, -30, 70)) {
        status_type("error"); status_message("Check the temperature value."); return()
      }
      if (!valid_number(input$rh_pct, 0, 100)) {
        status_type("error"); status_message("Enter RH between 0 and 100%."); return()
      }
      if (!valid_number(input$wind_ms, 0, 100)) {
        status_type("error"); status_message("Check the wind-speed value in m/s."); return()
      }
      obs$instrument_id <- input$instrument_id
      obs$temperature_c <- as.numeric(input$temperature_c)
      obs$rh_pct <- as.numeric(input$rh_pct)
      obs$wind_ms <- as.numeric(input$wind_ms)
    }

    ok <- tryCatch({
      append_observation(obs)
      TRUE
    }, error = function(e) {
      status_type("error")
      status_message(paste("NOT SAVED —", conditionMessage(e)))
      FALSE
    })
    if (!ok) return()

    set_last_point(method, patch, point_id)
    if (identical(mode, "new")) set_counter(method, patch, get_counter(method, patch) + 1L)

    status_type("ok")
    status_message(paste("Saved", paste0("Patch ", patch), point_id))

    # Return to the default workflow and clear only changing measurement values.
    updateRadioButtons(session, "point_mode", selected = "new")
    if (method == "Densitometer") {
      updateNumericInput(session, "canopy_pct", value = NA)
    } else {
      updateNumericInput(session, "temperature_c", value = NA)
      updateNumericInput(session, "rh_pct", value = NA)
      updateNumericInput(session, "wind_ms", value = NA)
    }
  })




  instructor_ui <- reactive({
    div(
      class = "instructor-shell",
      div(class = "topbar", h2("ENV2301 Field Activity 3 — Instructor")),
      if (!backend_is_remote()) div(class = "warning-box", "LOCAL TEST MODE — cloud deployment needs DATA_API_URL and DATA_API_TOKEN."),
      tabsetPanel(
        id = "instructor_tabs",
        tabPanel(
          "Overview",
          br(),
          actionButton("refresh_data", "Refresh data", class = "btn-primary"),
          downloadButton("download_data", "Download full CSV"),
          br(), br(),
          textOutput("analysis_message"),
          p(class = "small-muted", "The dashboard reads the Google Sheet tab named 'data'. If demo data were imported into a new tab, copy the rows into 'data' below the existing header."),
          h4("Field progress"),
          p("Counts are shown separately for ordinary points, shared reference readings and repeats."),
          tableOutput("progress_table"),
          h4("Loaded-data check"),
          tableOutput("data_check_table"),
          uiOutput("group_links")
        ),
        tabPanel(
          "Patch & group estimates",
          br(),
          selectInput(
            "analysis_var", "Variable",
            choices = c(
              "Canopy cover (%)" = "canopy_pct",
              "Temperature (°C)" = "temperature_c",
              "Relative humidity (%)" = "rh_pct",
              "Wind speed (m/s)" = "wind_ms"
            ),
            selected = "canopy_pct"
          ),
          p("Patch estimates use ordinary, non-repeat points only. Deliberately selected reference points are excluded."),
          h4("Ordinary observations by patch"),
          plotOutput("patch_plot", height = "420px"),
          tableOutput("patch_summary"),
          h4("Nine independent group attempts to estimate the two patches"),
          plotOutput("group_estimate_plot", height = "440px"),
          tableOutput("group_summary")
        ),
        tabPanel(
          "Same-location variation",
          br(),
          h4("Densitometer shared reference points"),
          p("R1–R5 were deliberately selected to span canopy conditions. They are used here to compare readings taken at the same locations, not to estimate patch means."),
          plotOutput("reference_plot", height = "420px"),
          tableOutput("reference_summary"),
          hr(),
          h4("Kestrel repeated points"),
          selectInput(
            "repeat_var", "Kestrel variable",
            choices = c(
              "Temperature (°C)" = "temperature_c",
              "Relative humidity (%)" = "rh_pct",
              "Wind speed (m/s)" = "wind_ms"
            ),
            selected = "temperature_c"
          ),
          p("Repeated readings at the same point show short-term/instrument variation. The comparison with spread among ordinary points is descriptive, not a formal variance decomposition."),
          tableOutput("kestrel_repeat_table"),
          tableOutput("variation_scale_table")
        ),
        tabPanel(
          "Kestrel through time",
          br(),
          selectInput(
            "time_var", "Kestrel variable",
            choices = c(
              "Temperature (°C)" = "temperature_c",
              "Relative humidity (%)" = "rh_pct",
              "Wind speed (m/s)" = "wind_ms"
            ),
            selected = "temperature_c"
          ),
          p("The field session takes place over time, so apparent patch differences can be partly confounded with when each patch was sampled."),
          plotOutput("time_plot", height = "430px"),
          h4("Sampling windows by group and patch"),
          tableOutput("time_order_table")
        ),
        tabPanel(
          "Sample size",
          br(),
          selectInput(
            "resample_var", "Variable",
            choices = c(
              "Canopy cover (%)" = "canopy_pct",
              "Temperature (°C)" = "temperature_c",
              "Relative humidity (%)" = "rh_pct",
              "Wind speed (m/s)" = "wind_ms"
            ),
            selected = "canopy_pct"
          ),
          selectInput("resample_patch", "Patch", choices = c("A", "B"), selected = "A"),
          selectInput("resample_group", "Data source", choices = c("All groups", paste0("G", 1:9)), selected = "All groups"),
          uiOutput("resample_n_control"),
          p("Bootstrap samples are drawn with replacement from ordinary observations. This shows sampling variability conditional on the observations that were actually collected; it cannot recover conditions the sampling design missed."),
          plotOutput("resample_plot", height = "360px"),
          plotOutput("resample_curve_plot", height = "360px"),
          tableOutput("resample_summary"),
          p(class = "small-muted", "SE = SD / sqrt(n) is shown as a teaching approximation. It assumes that the observations counted in n behave as independent sampling units. Spatial clustering or repeated sampling can make the effective amount of independent information smaller.")
        ),
        tabPanel(
          "Raw data",
          br(),
          fluidRow(
            column(3, selectInput("raw_group", "Group", choices = c("All", paste0("G", 1:9)), selected = "All")),
            column(3, selectInput("raw_patch", "Patch", choices = c("All", "A", "B"), selected = "All")),
            column(3, selectInput("raw_method", "Method", choices = c("All", "Densitometer", "Kestrel"), selected = "All")),
            column(3, selectInput("raw_type", "Record type", choices = c("All", "Ordinary", "Repeat", "Reference"), selected = "All"))
          ),
          tableOutput("raw_table")
        )
      )
    )
  })

  load_analysis_data <- function() {
    tryCatch({
      dat <- read_observations()
      analysis_store(dat)
      analysis_message(paste(nrow(dat), "observations loaded."))
    }, error = function(e) {
      analysis_message(paste("Could not load data:", conditionMessage(e)))
    })
  }

  observeEvent(input$refresh_data, load_analysis_data())
  observeEvent(instructor_view(), {
    if (isTRUE(instructor_view()) && isTRUE(instructor_unlocked())) load_analysis_data()
  }, ignoreInit = TRUE)
  observeEvent(instructor_unlocked(), {
    if (isTRUE(instructor_view()) && isTRUE(instructor_unlocked())) load_analysis_data()
  }, ignoreInit = TRUE)

  output$analysis_message <- renderText(analysis_message())

  output$download_data <- downloadHandler(
    filename = function() paste0("ENV2301_FA3_", format(Sys.Date(), "%Y-%m-%d"), ".csv"),
    content = function(file) write.csv(analysis_store(), file, row.names = FALSE, na = "")
  )

  output$group_links <- renderUI({
    proto <- session$clientData$url_protocol %||% "https:"
    host <- session$clientData$url_hostname %||% ""
    port <- session$clientData$url_port %||% ""
    path <- session$clientData$url_pathname %||% "/"
    if (!nzchar(host)) return(NULL)
    port_text <- if (nzchar(port) && !port %in% c("80", "443")) paste0(":", port) else ""
    base <- paste0(proto, "//", host, port_text, path)
    tags <- lapply(paste0("G", 1:9), function(g) {
      url <- paste0(base, "?group=", g)
      tags$div(class = "group-link-row", tags$strong(g), tags$a(href = url, target = "_blank", url))
    })
    tagList(h4("Student group links"), tags)
  })

  empty_variable_data <- function() {
    data.frame(
      timestamp_app = character(0),
      group_id = character(0),
      patch = character(0),
      method = character(0),
      point_id = character(0),
      is_repeat = logical(0),
      is_reference = logical(0),
      person_id = character(0),
      instrument_id = character(0),
      value = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  variable_data <- function(var, ordinary_only = TRUE, group = NULL, patch = NULL) {
    dat <- analysis_store()
    if (nrow(dat) == 0 || is.null(var) || !nzchar(var) || !var %in% names(dat)) {
      return(empty_variable_data())
    }

    target_method <- if (identical(var, "canopy_pct")) "Densitometer" else "Kestrel"

    # Work with an explicit numeric vector rather than relying on the source
    # column retaining its class after JSON/Google Sheets import.
    vals <- suppressWarnings(as.numeric(unlist(dat[[var]], use.names = FALSE)))
    if (length(vals) != nrow(dat)) return(empty_variable_data())

    keep <- dat$method == target_method & is.finite(vals)
    keep[is.na(keep)] <- FALSE

    if (ordinary_only) {
      ordinary <- !dat$is_reference & !dat$is_repeat
      ordinary[is.na(ordinary)] <- FALSE
      keep <- keep & ordinary
    }
    if (!is.null(group) && !identical(group, "All groups") && !identical(group, "All")) {
      gkeep <- dat$group_id == group
      gkeep[is.na(gkeep)] <- FALSE
      keep <- keep & gkeep
    }
    if (!is.null(patch) && !identical(patch, "All")) {
      pkeep <- dat$patch == patch
      pkeep[is.na(pkeep)] <- FALSE
      keep <- keep & pkeep
    }

    if (!any(keep)) return(empty_variable_data())

    out <- dat[keep, c(
      "timestamp_app", "group_id", "patch", "method", "point_id",
      "is_repeat", "is_reference", "person_id", "instrument_id"
    ), drop = FALSE]
    out$value <- vals[keep]
    out
  }

  safe_sd <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 2) return(NA_real_)
    sd(x)
  }

  summarise_values <- function(x) {
    x <- x[is.finite(x)]
    n <- length(x)
    s <- safe_sd(x)
    data.frame(
      n = n,
      mean = if (n) mean(x) else NA_real_,
      sd = s,
      se = if (n >= 2) s / sqrt(n) else NA_real_
    )
  }

  empty_plot <- function(message) {
    plot.new()
    text(0.5, 0.5, message, cex = 1.0)
    invisible(NULL)
  }

  output$progress_table <- renderTable({
    dat <- analysis_store()
    groups <- paste0("G", 1:9)
    rows <- lapply(groups, function(g) {
      z <- dat[dat$group_id == g, , drop = FALSE]
      ordinary <- !z$is_reference & !z$is_repeat
      data.frame(
        Group = g,
        Densi_A = sum(z$method == "Densitometer" & z$patch == "A" & ordinary, na.rm = TRUE),
        Densi_B = sum(z$method == "Densitometer" & z$patch == "B" & ordinary, na.rm = TRUE),
        Ref_A = sum(z$method == "Densitometer" & z$patch == "A" & z$is_reference, na.rm = TRUE),
        Ref_B = sum(z$method == "Densitometer" & z$patch == "B" & z$is_reference, na.rm = TRUE),
        Kestrel_A = sum(z$method == "Kestrel" & z$patch == "A" & ordinary, na.rm = TRUE),
        Kestrel_B = sum(z$method == "Kestrel" & z$patch == "B" & ordinary, na.rm = TRUE),
        Repeats = sum(z$is_repeat, na.rm = TRUE)
      )
    })
    do.call(rbind, rows)
  }, striped = TRUE, spacing = "s")

  output$data_check_table <- renderTable({
    dat <- analysis_store()
    if (nrow(dat) == 0) return(NULL)
    ordinary <- !dat$is_reference & !dat$is_repeat
    ordinary[is.na(ordinary)] <- FALSE
    data.frame(
      Category = c(
        "All rows",
        "Densitometer ordinary",
        "Densitometer repeats",
        "Densitometer references",
        "Kestrel ordinary",
        "Kestrel repeats"
      ),
      n = c(
        nrow(dat),
        sum(dat$method == "Densitometer" & ordinary, na.rm = TRUE),
        sum(dat$method == "Densitometer" & dat$is_repeat & !dat$is_reference, na.rm = TRUE),
        sum(dat$method == "Densitometer" & dat$is_reference, na.rm = TRUE),
        sum(dat$method == "Kestrel" & ordinary, na.rm = TRUE),
        sum(dat$method == "Kestrel" & dat$is_repeat, na.rm = TRUE)
      )
    )
  }, striped = TRUE, spacing = "s")

  selected_variable_data <- reactive({
    req(input$analysis_var)
    variable_data(input$analysis_var, ordinary_only = TRUE)
  })

  output$patch_plot <- renderPlot({
    d <- selected_variable_data()
    if (nrow(d) == 0 || !"value" %in% names(d) || !any(is.finite(d$value))) {
      empty_plot("No ordinary observations for this variable yet.")
      return(invisible(NULL))
    }

    keep <- is.finite(d$value) & d$patch %in% c("A", "B")
    keep[is.na(keep)] <- FALSE
    d <- d[keep, , drop = FALSE]
    if (nrow(d) == 0) {
      empty_plot("No ordinary observations for this variable yet.")
      return(invisible(NULL))
    }

    vals <- list(
      A = d$value[d$patch == "A"],
      B = d$value[d$patch == "B"]
    )
    present <- lengths(vals) > 0
    vals_present <- vals[present]
    if (length(vals_present) == 0) {
      empty_plot("No ordinary observations for this variable yet.")
      return(invisible(NULL))
    }

    cols <- grDevices::hcl.colors(9, "Dark 3")
    group_index <- match(d$group_id, paste0("G", 1:9))
    group_index[is.na(group_index)] <- 1L
    point_cols <- cols[group_index]

    boxplot(
      vals_present,
      xlab = "Patch",
      ylab = unname(VARIABLE_LABELS[input$analysis_var]),
      outline = FALSE
    )

    patch_positions <- setNames(seq_along(vals_present), names(vals_present))
    x <- unname(patch_positions[d$patch])
    set.seed(2301)
    points(jitter(x, amount = 0.08), d$value, pch = 16, cex = 0.85, col = point_cols)
    legend("topright", legend = paste0("G", 1:9), col = cols, pch = 16, cex = 0.72, ncol = 3, bty = "n")
  })

  output$patch_summary <- renderTable({
    d <- selected_variable_data()
    if (nrow(d) == 0) return(NULL)
    rows <- lapply(c("A", "B"), function(p) {
      z <- d$value[d$patch == p]
      s <- summarise_values(z)
      data.frame(
        Patch = p,
        n = s$n,
        Mean = round(s$mean, 3),
        SD = round(s$sd, 3),
        SE = round(s$se, 3)
      )
    })
    do.call(rbind, rows)
  }, striped = TRUE, spacing = "s", na = "")

  group_patch_summary <- reactive({
    d <- selected_variable_data()
    groups <- paste0("G", 1:9)
    rows <- lapply(groups, function(g) {
      row <- data.frame(Group = g)
      for (p in c("A", "B")) {
        x <- d$value[d$group_id == g & d$patch == p]
        s <- summarise_values(x)
        row[[paste0("n_", p)]] <- s$n
        row[[paste0("mean_", p)]] <- s$mean
        row[[paste0("sd_", p)]] <- s$sd
        row[[paste0("se_", p)]] <- s$se
      }
      row$A_minus_B <- row$mean_A - row$mean_B
      row
    })
    do.call(rbind, rows)
  })

  output$group_estimate_plot <- renderPlot({
    s <- group_patch_summary()
    if (is.null(s) || nrow(s) == 0) {
      empty_plot("No group means available yet.")
      return(invisible(NULL))
    }

    y <- suppressWarnings(as.numeric(c(s$mean_A, s$mean_B)))
    y <- y[is.finite(y)]
    if (length(y) == 0) {
      empty_plot("No group means available yet.")
      return(invisible(NULL))
    }

    yr <- range(y, finite = TRUE)
    pad <- if (!all(is.finite(yr)) || diff(yr) == 0) 1 else 0.08 * diff(yr)
    yr <- yr + c(-pad, pad)

    cols <- grDevices::hcl.colors(9, "Dark 3")
    plot(
      NA_real_, NA_real_,
      xlim = c(0.8, 2.2), ylim = yr,
      xaxt = "n",
      xlab = "Patch", ylab = unname(VARIABLE_LABELS[input$analysis_var])
    )
    axis(1, at = c(1, 2), labels = c("A", "B"))

    for (i in seq_len(nrow(s))) {
      vals <- suppressWarnings(as.numeric(c(s$mean_A[i], s$mean_B[i])))
      good <- is.finite(vals)
      if (any(good)) {
        points(c(1, 2)[good], vals[good], pch = 16, col = cols[i], cex = 1.1)
      }
      if (all(good)) lines(c(1, 2), vals, col = cols[i], lwd = 1.5)
    }
    legend("topright", legend = s$Group, col = cols, pch = 16, lty = 1, cex = 0.75, ncol = 3, bty = "n")
  })

  output$group_summary <- renderTable({
    s <- group_patch_summary()
    if (is.null(s) || nrow(s) == 0) return(NULL)
    out <- data.frame(
      Group = s$Group,
      n_A = s$n_A,
      Mean_A = round(s$mean_A, 3),
      SD_A = round(s$sd_A, 3),
      SE_A = round(s$se_A, 3),
      n_B = s$n_B,
      Mean_B = round(s$mean_B, 3),
      SD_B = round(s$sd_B, 3),
      SE_B = round(s$se_B, 3),
      A_minus_B = round(s$A_minus_B, 3)
    )
    out
  }, striped = TRUE, spacing = "s", na = "")

  output$reference_plot <- renderPlot({
    dat <- analysis_store()
    canopy <- suppressWarnings(as.numeric(unlist(dat$canopy_pct, use.names = FALSE)))
    keep <- dat$method == "Densitometer" & dat$is_reference & is.finite(canopy)
    keep[is.na(keep)] <- FALSE
    if (!any(keep)) {
      empty_plot("No shared-reference readings yet.")
      return(invisible(NULL))
    }
    d <- dat[keep, c("group_id", "patch", "point_id", "person_id"), drop = FALSE]
    d$canopy_pct <- canopy[keep]
    lev <- c(paste0("R-A", 1:5), paste0("R-B", 1:5))
    d$point <- factor(d$point_id, levels = lev)
    d <- d[!is.na(d$point) & is.finite(d$canopy_pct), , drop = FALSE]
    if (nrow(d) == 0) {
      empty_plot("No valid shared-reference readings yet.")
      return(invisible(NULL))
    }
    cols <- grDevices::hcl.colors(9, "Dark 3")
    group_index <- match(d$group_id, paste0("G", 1:9))
    group_index[is.na(group_index)] <- 1L
    x <- as.numeric(d$point)
    set.seed(2301)
    plot(jitter(x, amount = 0.10), d$canopy_pct,
         pch = 16, col = cols[group_index],
         xaxt = "n", xlab = "Shared reference point",
         ylab = "Canopy cover (%)", xlim = c(0.5, 10.5))
    axis(1, at = 1:10, labels = c(paste0("A", 1:5), paste0("B", 1:5)))
    means <- tapply(d$canopy_pct, d$point, mean, na.rm = TRUE)
    mean_pos <- which(is.finite(means))
    if (length(mean_pos)) points(mean_pos, means[mean_pos], pch = 18, cex = 1.6)
    legend("topright", legend = c(paste0("G", 1:9), "Point mean"),
           col = c(cols, "black"), pch = c(rep(16, 9), 18),
           cex = 0.7, ncol = 2, bty = "n")
  })

  output$reference_summary <- renderTable({
    dat <- analysis_store()
    canopy <- suppressWarnings(as.numeric(unlist(dat$canopy_pct, use.names = FALSE)))
    keep <- dat$method == "Densitometer" & dat$is_reference & is.finite(canopy)
    keep[is.na(keep)] <- FALSE
    if (!any(keep)) return(NULL)
    d <- dat[keep, c("group_id", "patch", "point_id", "person_id"), drop = FALSE]
    d$canopy_pct <- canopy[keep]
    lev <- c(paste0("R-A", 1:5), paste0("R-B", 1:5))
    rows <- lapply(lev, function(id) {
      z <- d[d$point_id == id, , drop = FALSE]
      if (nrow(z) == 0) return(NULL)
      data.frame(
        Point = id,
        n = nrow(z),
        Groups = length(unique(z$group_id[nzchar(z$group_id)])),
        People = length(unique(paste(z$group_id, z$person_id, sep = "-"))),
        Mean = mean(z$canopy_pct, na.rm = TRUE),
        SD = safe_sd(z$canopy_pct),
        Min = min(z$canopy_pct, na.rm = TRUE),
        Max = max(z$canopy_pct, na.rm = TRUE)
      )
    })
    out <- do.call(rbind, rows)
    if (is.null(out)) return(NULL)
    for (nm in c("Mean", "SD", "Min", "Max")) out[[nm]] <- round(out[[nm]], 2)
    out
  }, striped = TRUE, spacing = "s", na = "")

  kestrel_repeat_data <- reactive({
    req(input$repeat_var)
    dat <- analysis_store()
    var <- input$repeat_var
    d <- dat[
      dat$method == "Kestrel" & !dat$is_reference &
        !is.na(dat[[var]]) & is.finite(dat[[var]]),
      c("group_id", "patch", "point_id", "is_repeat", "instrument_id", var),
      drop = FALSE
    ]
    if (nrow(d) == 0) return(data.frame())
    names(d)[ncol(d)] <- "value"
    key <- paste(d$group_id, d$patch, d$point_id, sep = "|")
    repeated_keys <- unique(key[d$is_repeat | duplicated(key) | duplicated(key, fromLast = TRUE)])
    d$key <- key
    d[d$key %in% repeated_keys, , drop = FALSE]
  })

  output$kestrel_repeat_table <- renderTable({
    d <- kestrel_repeat_data()
    if (nrow(d) == 0) return(data.frame(Message = "No Kestrel repeated points for this variable yet."))
    rows <- lapply(split(d, d$key), function(z) {
      data.frame(
        Group = z$group_id[1],
        Patch = z$patch[1],
        Point = z$point_id[1],
        n = nrow(z),
        Instruments = paste(sort(unique(z$instrument_id[nzchar(z$instrument_id)])), collapse = ", "),
        Mean = mean(z$value),
        SD = safe_sd(z$value),
        Range = diff(range(z$value))
      )
    })
    out <- do.call(rbind, rows)
    for (nm in c("Mean", "SD", "Range")) out[[nm]] <- round(out[[nm]], 3)
    rownames(out) <- NULL
    out
  }, striped = TRUE, spacing = "s", na = "")

  pooled_within_sd <- function(d) {
    if (nrow(d) == 0) return(NA_real_)
    spl <- split(d$value, d$key)
    spl <- spl[vapply(spl, length, integer(1)) >= 2]
    if (!length(spl)) return(NA_real_)
    dfs <- vapply(spl, safe_sd, numeric(1))
    ns <- vapply(spl, length, integer(1))
    good <- is.finite(dfs) & ns >= 2
    if (!any(good)) return(NA_real_)
    sqrt(sum((ns[good] - 1) * dfs[good]^2) / sum(ns[good] - 1))
  }

  output$variation_scale_table <- renderTable({
    req(input$repeat_var)
    var <- input$repeat_var
    ordinary <- variable_data(var, ordinary_only = TRUE)
    repeats <- kestrel_repeat_data()
    rows <- lapply(c("A", "B"), function(p) {
      x <- ordinary$value[ordinary$patch == p]
      r <- repeats[repeats$patch == p, , drop = FALSE]
      data.frame(
        Patch = p,
        Ordinary_points_n = length(x),
        SD_among_ordinary_points = safe_sd(x),
        Repeated_points_n = length(unique(r$key)),
        Pooled_SD_within_repeated_points = pooled_within_sd(r)
      )
    })
    out <- do.call(rbind, rows)
    out$SD_among_ordinary_points <- round(out$SD_among_ordinary_points, 3)
    out$Pooled_SD_within_repeated_points <- round(out$Pooled_SD_within_repeated_points, 3)
    out
  }, striped = TRUE, spacing = "s", na = "")



  parse_local_time <- function(x) {
    x <- trimws(as.character(x))
    x[x %in% c("", "NA", "NULL", "null")] <- NA_character_

    # Store parsed times numerically so a single malformed timestamp cannot
    # cause as.POSIXct() to fail for the entire vector.
    out_num <- rep(NA_real_, length(x))

    for (i in seq_along(x)) {
      xi <- x[i]
      if (is.na(xi) || !nzchar(xi)) next

      # Google/JSON commonly supplies ISO-8601 timestamps such as:
      # 2026-09-25T01:01:27.000Z
      z <- suppressWarnings(strptime(
        xi,
        format = "%Y-%m-%dT%H:%M:%OSZ",
        tz = "UTC"
      ))

      # ISO timestamp without a trailing Z.
      if (is.na(z)) {
        z <- suppressWarnings(strptime(
          xi,
          format = "%Y-%m-%dT%H:%M:%OS",
          tz = "UTC"
        ))
      }

      # Conventional date-time strings.
      if (is.na(z)) {
        z <- suppressWarnings(strptime(
          xi,
          format = "%Y-%m-%d %H:%M:%OS",
          tz = "UTC"
        ))
      }

      # ISO timestamps carrying a numeric UTC offset.
      if (is.na(z)) {
        xi_offset <- sub(
          "([+-][0-9]{2}):([0-9]{2})$",
          "\\1\\2",
          xi
        )
        z <- suppressWarnings(strptime(
          xi_offset,
          format = "%Y-%m-%dT%H:%M:%OS%z",
          tz = "UTC"
        ))
      }

      if (!is.na(z)) {
        out_num[i] <- as.numeric(as.POSIXct(z, tz = "UTC"))
      }
    }

    tt <- as.POSIXct(out_num, origin = "1970-01-01", tz = "UTC")

    # Change display timezone only; the underlying instant remains UTC-based.
    attr(tt, "tzone") <- "Asia/Singapore"
    tt
  }


  kestrel_time_data <- reactive({
    req(input$time_var)
    d <- variable_data(input$time_var, ordinary_only = FALSE)
    if (nrow(d) == 0) return(empty_variable_data())
    d$time_local <- parse_local_time(d$timestamp_app)
    good <- !is.na(d$time_local) & is.finite(as.numeric(d$time_local)) & is.finite(d$value)
    good[is.na(good)] <- FALSE
    d[good, , drop = FALSE]
  })

  output$time_plot <- renderPlot({
    d <- kestrel_time_data()
    if (nrow(d) == 0) {
      empty_plot("No Kestrel observations with valid timestamps yet.")
      return(invisible(NULL))
    }
    xnum <- as.numeric(d$time_local)
    good <- is.finite(xnum) & is.finite(d$value) & d$patch %in% c("A", "B")
    good[is.na(good)] <- FALSE
    d <- d[good, , drop = FALSE]
    xnum <- xnum[good]
    if (nrow(d) == 0 || !length(xnum)) {
      empty_plot("No Kestrel observations with valid timestamps yet.")
      return(invisible(NULL))
    }
    patch_cols <- grDevices::hcl.colors(2, "Dark 2")
    idx <- match(d$patch, c("A", "B"))
    plot(d$time_local, d$value,
         pch = c(16, 17)[idx], col = patch_cols[idx],
         xlab = "Local time (Singapore)",
         ylab = unname(VARIABLE_LABELS[input$time_var]))
    legend("topright", legend = c("Patch A", "Patch B"),
           col = patch_cols, pch = c(16, 17), bty = "n")
  })

  output$time_order_table <- renderTable({
    d <- kestrel_time_data()
    if (nrow(d) == 0) return(NULL)
    keys <- interaction(d$group_id, d$patch, drop = TRUE)
    rows <- lapply(split(d, keys), function(z) {
      data.frame(
        Group = z$group_id[1],
        Patch = z$patch[1],
        n = nrow(z),
        First = format(min(z$time_local), "%H:%M:%S"),
        Last = format(max(z$time_local), "%H:%M:%S")
      )
    })
    out <- do.call(rbind, rows)
    out <- out[order(out$First, out$Group, out$Patch), ]
    rownames(out) <- NULL
    out
  }, striped = TRUE, spacing = "s")

  resample_source_data <- reactive({
    req(input$resample_var, input$resample_patch, input$resample_group)
    variable_data(
      input$resample_var,
      ordinary_only = TRUE,
      group = input$resample_group,
      patch = input$resample_patch
    )
  })

  output$resample_n_control <- renderUI({
    d <- resample_source_data()
    nmax <- nrow(d)
    if (nmax < 2) return(p("At least two ordinary observations are needed."))
    sliderInput(
      "resample_n", "Sample size (n)",
      min = 2, max = nmax,
      value = min(5, nmax), step = 1
    )
  })

  resample_results <- reactive({
    if (is.null(input$resample_n)) return(NULL)
    d <- resample_source_data()
    if (nrow(d) == 0 || !"value" %in% names(d)) return(NULL)
    x <- d$value[is.finite(d$value)]
    n <- suppressWarnings(as.integer(input$resample_n))
    if (length(x) < 2 || length(n) != 1 || is.na(n) || n < 2 || n > length(x)) return(NULL)
    set.seed(2301 + n)
    means <- replicate(1000, mean(sample(x, size = n, replace = TRUE)))
    list(x = x, n = n, means = means)
  })

  output$resample_plot <- renderPlot({
    z <- resample_results()
    if (is.null(z)) {
      empty_plot("Not enough ordinary observations for this selection yet.")
      return(invisible(NULL))
    }
    hist(z$means, breaks = "FD",
         main = paste("1,000 bootstrap sample means; n =", z$n),
         xlab = "Sample mean")
    abline(v = mean(z$x), lwd = 2)
  })

  output$resample_curve_plot <- renderPlot({
    d <- resample_source_data()
    if (nrow(d) == 0 || !"value" %in% names(d)) {
      empty_plot("Not enough ordinary observations for this selection yet.")
      return(invisible(NULL))
    }
    x <- d$value[is.finite(d$value)]
    if (length(x) < 2) {
      empty_plot("Not enough ordinary observations for this selection yet.")
      return(invisible(NULL))
    }
    ns <- 2:length(x)
    set.seed(2301)
    boot_sd <- vapply(ns, function(n) {
      vals <- replicate(300, mean(sample(x, size = n, replace = TRUE)))
      sd(vals)
    }, numeric(1))
    theoretical <- sd(x) / sqrt(ns)
    yr <- range(c(boot_sd, theoretical), finite = TRUE)
    if (length(yr) != 2 || any(!is.finite(yr))) {
      empty_plot("Sampling-variability curve could not be calculated.")
      return(invisible(NULL))
    }
    if (diff(yr) == 0) yr <- yr + c(-0.5, 0.5)
    plot(ns, boot_sd, type = "b", pch = 16,
         xlab = "Sample size (n)",
         ylab = "SD of sample means", ylim = yr)
    lines(ns, theoretical, lty = 2, lwd = 2)
    legend("topright", legend = c("Bootstrap", "SD / sqrt(n)"),
           lty = c(1, 2), pch = c(16, NA), bty = "n")
  })

  output$resample_summary <- renderTable({
    z <- resample_results()
    if (is.null(z)) return(data.frame(Message = "Not enough ordinary observations for this selection yet."))
    data.frame(
      Quantity = c(
        "Available ordinary observations",
        "Full-data mean",
        "Observed SD",
        "Selected sample size",
        "SD of 1,000 bootstrap means",
        "SD / sqrt(n)"
      ),
      Value = c(
        length(z$x), round(mean(z$x), 3), round(sd(z$x), 3), z$n,
        round(sd(z$means), 3), round(sd(z$x) / sqrt(z$n), 3)
      )
    )
  }, striped = TRUE, spacing = "s")

  raw_filtered <- reactive({
    dat <- analysis_store()
    if (nrow(dat) == 0) return(dat)
    keep <- rep(TRUE, nrow(dat))
    if (!is.null(input$raw_group) && input$raw_group != "All") keep <- keep & dat$group_id == input$raw_group
    if (!is.null(input$raw_patch) && input$raw_patch != "All") keep <- keep & dat$patch == input$raw_patch
    if (!is.null(input$raw_method) && input$raw_method != "All") keep <- keep & dat$method == input$raw_method
    if (!is.null(input$raw_type) && input$raw_type != "All") {
      if (input$raw_type == "Ordinary") keep <- keep & !dat$is_reference & !dat$is_repeat
      if (input$raw_type == "Repeat") keep <- keep & dat$is_repeat
      if (input$raw_type == "Reference") keep <- keep & dat$is_reference
    }
    dat[keep, , drop = FALSE]
  })

  output$raw_table <- renderTable({
    dat <- raw_filtered()
    if (nrow(dat) == 0) return(NULL)
    tail(dat, 200)
  }, striped = TRUE, spacing = "xs")


}

shinyApp(ui, server)
