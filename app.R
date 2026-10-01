library(shiny)
library(httr2)
library(jsonlite)

# ENV2301 Field Activity 3
# Student data-entry app + instructor analysis dashboard
#
# Production backend: Google Apps Script endpoint writing to Google Sheets.
# Environment variables:
#   DATA_API_URL
#   DATA_API_TOKEN
# Optional:
#   INSTRUCTOR_PIN
#
# Student links can use ?group=G1 ... ?group=G9
# Instructor view: ?view=instructor

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

NUMERIC_COLUMNS <- c(
  "point_number", "temperature_c", "rh_pct", "wind_ms", "canopy_pct"
)
LOGICAL_COLUMNS <- c("is_repeat", "is_reference")

VARIABLE_LABELS <- c(
  canopy_pct = "Canopy cover (%)",
  temperature_c = "Temperature (°C)",
  rh_pct = "Relative humidity (%)",
  wind_ms = "Wind speed (m/s)"
)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

empty_data <- function() {
  out <- as.data.frame(
    setNames(replicate(length(DATA_COLUMNS), logical(0), simplify = FALSE), DATA_COLUMNS)
  )
  for (nm in setdiff(DATA_COLUMNS, c(NUMERIC_COLUMNS, LOGICAL_COLUMNS))) {
    out[[nm]] <- character(0)
  }
  for (nm in NUMERIC_COLUMNS) out[[nm]] <- numeric(0)
  for (nm in LOGICAL_COLUMNS) out[[nm]] <- logical(0)
  out
}

normalise_timestamp_column <- function(z) {
  n <- length(z)
  out <- rep(NA_character_, n)

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

    num <- suppressWarnings(as.numeric(s))
    if (length(num) == 1 && is.finite(num) && num > 20000 && num < 80000) {
      tt <- excel_origin + num * 86400
      out[i] <- format(tt, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
      next
    }
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

  dir.create(dirname(LOCAL_DATA_FILE), recursive = TRUE, showWarnings = FALSE)
  row <- as.data.frame(obs, stringsAsFactors = FALSE)
  for (nm in DATA_COLUMNS) {
    if (!nm %in% names(row)) row[[nm]] <- NA
  }
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
  normalise_data(
    read.csv(LOCAL_DATA_FILE, stringsAsFactors = FALSE, check.names = FALSE)
  )
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
  length(x) == 1 &&
    !is.null(x) &&
    !is.na(x) &&
    is.finite(x) &&
    x >= min_value &&
    x <= max_value
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
  text(0.5, 0.5, message, cex = 1)
  invisible(NULL)
}

ui <- fluidPage(
  tags$head(
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1, maximum-scale=1"
    ),
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
    as.integer(vals[[counter_key(method, patch)]] %||% 1L)
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
        z <- dat[
          dat$method == method &
            dat$patch == patch &
            grepl(paste0("^", prefix, "[0-9]+$"), dat$point_id),
          ,
          drop = FALSE
        ]
        if (nrow(z) > 0) {
          nums <- suppressWarnings(
            as.integer(sub(paste0("^", prefix), "", z$point_id))
          )
          nums <- nums[is.finite(nums)]
          if (length(nums)) {
            set_counter(method, patch, max(nums, na.rm = TRUE) + 1L)
          }
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
    if (!nzchar(INSTRUCTOR_PIN) ||
        identical(as.character(input$instructor_pin), INSTRUCTOR_PIN)) {
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
        return(
          div(
            class = "app-shell compact-shell",
            h2("ENV2301 Field Activity 3"),
            p(class = "subtitle", "Instructor view"),
            passwordInput("instructor_pin", "Instructor PIN"),
            actionButton(
              "unlock_instructor",
              "Open instructor view",
              class = "btn-primary big-button"
            )
          )
        )
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
      selectInput(
        "group_select",
        "Group",
        choices = paste0("G", 1:9),
        selected = "G1"
      ),
      actionButton("set_group", "Start", class = "btn-primary big-button"),
      if (!backend_is_remote()) {
        div(
          class = "warning-box",
          "LOCAL TEST MODE — data are not using the shared Google Sheet backend."
        )
      }
    )
  })

  student_ui <- reactive({
    div(
      class = "app-shell",
      div(
        class = "topbar",
        div(
          strong("ENV2301 Field Activity 3"),
          br(),
          span(class = "small-muted", paste("Group", group_id()))
        ),
        actionLink("change_group", "Change group", class = "small-link")
      ),
      if (!backend_is_remote()) {
        div(
          class = "warning-box",
          "LOCAL TEST MODE — do not use this mode for the class field session."
        )
      },
      div(
        class = "control-card",
        radioButtons(
          "method",
          "Method",
          choices = c("Densitometer", "Kestrel"),
          selected = "Densitometer",
          inline = TRUE
        ),
        radioButtons(
          "patch",
          "Patch",
          choices = c("A", "B"),
          selected = "A",
          inline = TRUE
        )
      ),
      uiOutput("method_fields"),
      div(class = "status-line", uiOutput("status_ui"))
    )
  })

  output$method_fields <- renderUI({
    req(input$method, input$patch)

    point_modes <- if (input$method == "Densitometer") {
      c(
        "Move to new location" = "new",
        "Another reading at this location" = "repeat",
        "Shared reference" = "reference"
      )
    } else {
      c(
        "Move to new location" = "new",
        "Another reading at this location" = "repeat"
      )
    }

    method_specific <- if (input$method == "Densitometer") {
      tagList(
        conditionalPanel(
          condition = "input.point_mode == 'reference'",
          radioButtons(
            "reference_no",
            "Shared reference point",
            choices = paste0("R", 1:5),
            selected = "R1",
            inline = TRUE
          )
        ),
        radioButtons(
          "person_id",
          "Person",
          choices = paste0("P", 1:5),
          selected = "P1",
          inline = TRUE
        ),
        numericInput(
          "canopy_pct",
          "Canopy cover (%)",
          value = NA,
          min = 0,
          max = 100,
          step = 0.1
        )
      )
    } else {
      tagList(
        radioButtons(
          "instrument_id",
          "Kestrel",
          choices = paste0("K", 1:3),
          selected = "K1",
          inline = TRUE
        ),
        div(
          class = "three-grid",
          numericInput(
            "temperature_c",
            "Temperature (°C)",
            value = NA,
            step = 0.1
          ),
          numericInput(
            "rh_pct",
            "Relative humidity (%)",
            value = NA,
            min = 0,
            max = 100,
            step = 0.1
          ),
          numericInput(
            "wind_ms",
            "Wind speed (m/s)",
            value = NA,
            min = 0,
            step = 0.1
          )
        )
      )
    }

    div(
      class = "control-card measurement-card",
      radioButtons(
        "point_mode",
        "Point",
        choices = point_modes,
        selected = "new",
        inline = TRUE
      ),
      div(
        class = "point-display",
        span("Recording as "),
        strong(textOutput("current_point", inline = TRUE))
      ),
      p(
        class = "small-muted",
        "If you are finished at this location, select Move to new location. ",
        "You can move on even if you did not complete all planned repeat readings."
      ),
      method_specific,
      actionButton(
        "submit_obs",
        "SAVE MEASUREMENT",
        class = "btn-primary submit-button"
      )
    )
  })

  output$status_ui <- renderUI({
    msg <- status_message()
    if (!nzchar(msg)) return(NULL)
    cls <- if (status_type() == "error") "status-error" else "status-ok"
    span(class = cls, msg)
  })

  observeEvent(input$submit_obs, {

    if (as.numeric(
      difftime(Sys.time(), last_submit_at(), units = "secs")
    ) < 0.8) return()

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

    point_num <- if (grepl("^[DK][0-9]+$", point_id)) {
      suppressWarnings(as.integer(sub("^[DK]", "", point_id)))
    } else {
      NA_integer_
    }

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
        status_type("error")
        status_message("Select the person taking the reading.")
        return()
      }

      if (!valid_number(input$canopy_pct, 0, 100)) {
        status_type("error")
        status_message("Enter canopy cover between 0 and 100%.")
        return()
      }

      obs$person_id <- input$person_id
      obs$canopy_pct <- as.numeric(input$canopy_pct)

    } else {

      if (is.null(input$instrument_id) || !nzchar(input$instrument_id)) {
        status_type("error")
        status_message("Select the Kestrel ID.")
        return()
      }

      if (!valid_number(input$temperature_c, -30, 70)) {
        status_type("error")
        status_message("Check the temperature value.")
        return()
      }

      if (!valid_number(input$rh_pct, 0, 100)) {
        status_type("error")
        status_message("Enter RH between 0 and 100%.")
        return()
      }

      if (!valid_number(input$wind_ms, 0, 100)) {
        status_type("error")
        status_message("Check the wind-speed value in m/s.")
        return()
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

    if (identical(mode, "new")) {
      set_counter(method, patch, get_counter(method, patch) + 1L)
    }

    status_type("ok")
    status_message(paste("Saved", paste0("Patch ", patch), point_id))

    # After the first reading at a new ordinary point, the revised protocol
    # expects repeated readings at that same point. Default to another reading
    # at this location. When the group is ready to move, they explicitly select
    # Move to new location. Reduced replication is allowed.
    if (identical(mode, "new")) {
      updateRadioButtons(session, "point_mode", selected = "repeat")
    }

    if (method == "Densitometer") {
      updateNumericInput(session, "canopy_pct", value = NA)
    } else {
      updateNumericInput(session, "temperature_c", value = NA)
      updateNumericInput(session, "rh_pct", value = NA)
      updateNumericInput(session, "wind_ms", value = NA)
    }
  })

  # ---------------------------------------------------------------------------
  # Instructor dashboard
  # ---------------------------------------------------------------------------

  instructor_ui <- reactive({
    div(
      class = "instructor-shell",
      div(
        class = "topbar",
        h2("ENV2301 Field Activity 3 — Instructor")
      ),
      if (!backend_is_remote()) {
        div(
          class = "warning-box",
          "LOCAL TEST MODE — cloud deployment needs DATA_API_URL and DATA_API_TOKEN."
        )
      },
      tabsetPanel(
        id = "instructor_tabs",

        tabPanel(
          "Overview",
          br(),
          actionButton("refresh_data", "Refresh data", class = "btn-primary"),
          downloadButton("download_data", "Download full CSV"),
          br(), br(),
          textOutput("analysis_message"),
          p(
            class = "small-muted",
            "Spatial summaries count sampling locations, not repeated readings."
          ),
          h4("Field progress"),
          p(
            "Canopy and Kestrel counts refer to unique sampling locations. ",
            "The planned replication is three people with three canopy readings each, ",
            "and five Kestrel readings per location. Locations with less replication ",
            "are retained and flagged rather than discarded."
          ),
          tableOutput("progress_table"),
          h4("Loaded-data check"),
          tableOutput("data_check_table"),
          uiOutput("group_links")
        ),

        tabPanel(
          "Patch & group estimates",
          br(),
          selectInput(
            "analysis_var",
            "Variable",
            choices = c(
              "Canopy cover (%)" = "canopy_pct",
              "Temperature (°C)" = "temperature_c",
              "Relative humidity (%)" = "rh_pct",
              "Wind speed (m/s)" = "wind_ms"
            ),
            selected = "canopy_pct"
          ),
          p(
            "Each point in these analyses is one spatial sampling location. ",
            "For canopy cover, the three readings made by one person are first ",
            "averaged to a person mean, and person means are then averaged to ",
            "a location mean. Kestrel repeats are likewise averaged to a location mean."
          ),
          h4("Location means within each group's sample"),
          plotOutput("patch_plot", height = "500px"),
          tableOutput("patch_summary"),
          h4("Group estimates of the two patches"),
          plotOutput("group_estimate_plot", height = "430px"),
          tableOutput("group_summary")
        ),

        tabPanel(
          "Same-location variation",
          br(),
          h4("Ordinary canopy location"),
          p(
            "Select one location to compare the three directional readings ",
            "made by each person and differences among people measuring the same location."
          ),
          fluidRow(
            column(
              4,
              selectInput(
                "same_patch",
                "Patch",
                choices = c("A", "B"),
                selected = "A"
              )
            ),
            column(
              4,
              selectInput(
                "same_group",
                "Group",
                choices = paste0("G", 1:9),
                selected = "G1"
              )
            ),
            column(4, uiOutput("same_location_ui"))
          ),
          plotOutput("same_location_plot", height = "390px"),
          tableOutput("same_location_table"),
          hr(),

          h4("Shared densitometer reference points"),
          p(
            "R1–R5 are deliberately selected locations. Person means are shown ",
            "so three directional readings by one person do not count as three observers."
          ),
          plotOutput("reference_plot", height = "420px"),
          tableOutput("reference_summary"),
          hr(),

          h4("Kestrel readings within sampling locations"),
          selectInput(
            "repeat_var",
            "Kestrel variable",
            choices = c(
              "Temperature (°C)" = "temperature_c",
              "Relative humidity (%)" = "rh_pct",
              "Wind speed (m/s)" = "wind_ms"
            ),
            selected = "temperature_c"
          ),
          p(
            "The five successive Kestrel readings describe short-term variation ",
            "at one location. Their mean is used as the spatial point estimate ",
            "elsewhere in the dashboard."
          ),
          tableOutput("kestrel_repeat_table"),
          tableOutput("variation_scale_table")
        ),

        tabPanel(
          "Variation levels",
          br(),
          h4("Where is the observed variation in canopy cover?"),
          p(
            "This descriptive partition separates variation between patches, ",
            "among groups within patches, among sampling locations within groups, ",
            "among people at the same location, and among the three readings made ",
            "by one person."
          ),
          plotOutput("variation_partition_plot", height = "330px"),
          tableOutput("variation_partition_table"),
          h4("Replication actually achieved"),
          p(
            "Reduced replication is allowed when field time is limited. ",
            "This table shows how much information is available at the lower levels ",
            "of the hierarchy."
          ),
          tableOutput("replication_coverage_table"),
          p(
            class = "small-muted",
            "This is a descriptive sum-of-squares partition of the class dataset, ",
            "not a formal population-level variance-components model. Locations or ",
            "people with fewer repeat measurements still contribute to the analysis, ",
            "but they provide less information about variation at the lower levels."
          )
        ),

        tabPanel(
          "Kestrel through time",
          br(),
          selectInput(
            "time_var",
            "Kestrel variable",
            choices = c(
              "Temperature (°C)" = "temperature_c",
              "Relative humidity (%)" = "rh_pct",
              "Wind speed (m/s)" = "wind_ms"
            ),
            selected = "temperature_c"
          ),
          p(
            "All individual Kestrel readings are retained here. Repeated readings ",
            "from one location are connected. Apparent patch differences can partly ",
            "reflect when each patch was sampled."
          ),
          plotOutput("time_plot", height = "430px"),
          h4("Sampling windows by group and patch"),
          tableOutput("time_order_table")
        ),

        tabPanel(
          "Sample size",
          br(),
          selectInput(
            "resample_var",
            "Variable",
            choices = c(
              "Canopy cover (%)" = "canopy_pct",
              "Temperature (°C)" = "temperature_c",
              "Relative humidity (%)" = "rh_pct",
              "Wind speed (m/s)" = "wind_ms"
            ),
            selected = "canopy_pct"
          ),
          selectInput(
            "resample_patch",
            "Patch",
            choices = c("A", "B"),
            selected = "A"
          ),
          selectInput(
            "resample_group",
            "Data source",
            choices = c("All groups", paste0("G", 1:9)),
            selected = "All groups"
          ),
          uiOutput("resample_n_control"),
          p(
            "Bootstrap samples are drawn with replacement from location means. ",
            "Repeated measurements at one location therefore do not count as ",
            "independent spatial observations."
          ),
          plotOutput("resample_plot", height = "360px"),
          plotOutput("resample_curve_plot", height = "360px"),
          tableOutput("resample_summary"),
          p(
            class = "small-muted",
            "SE = SD / sqrt(n) is shown as a teaching approximation. Here n is ",
            "the number of spatial locations, not the number of raw readings."
          )
        ),

        tabPanel(
          "Raw data",
          br(),
          fluidRow(
            column(
              3,
              selectInput(
                "raw_group",
                "Group",
                choices = c("All", paste0("G", 1:9)),
                selected = "All"
              )
            ),
            column(
              3,
              selectInput(
                "raw_patch",
                "Patch",
                choices = c("All", "A", "B"),
                selected = "All"
              )
            ),
            column(
              3,
              selectInput(
                "raw_method",
                "Method",
                choices = c("All", "Densitometer", "Kestrel"),
                selected = "All"
              )
            ),
            column(
              3,
              selectInput(
                "raw_type",
                "Record type",
                choices = c("All", "Ordinary", "Repeat", "Reference"),
                selected = "All"
              )
            )
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
    if (isTRUE(instructor_view()) && isTRUE(instructor_unlocked())) {
      load_analysis_data()
    }
  }, ignoreInit = TRUE)

  observeEvent(instructor_unlocked(), {
    if (isTRUE(instructor_view()) && isTRUE(instructor_unlocked())) {
      load_analysis_data()
    }
  }, ignoreInit = TRUE)

  output$analysis_message <- renderText(analysis_message())

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("ENV2301_FA3_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
    },
    content = function(file) {
      write.csv(analysis_store(), file, row.names = FALSE, na = "")
    }
  )

  output$group_links <- renderUI({
    proto <- session$clientData$url_protocol %||% "https:"
    host <- session$clientData$url_hostname %||% ""
    port <- session$clientData$url_port %||% ""
    path <- session$clientData$url_pathname %||% "/"

    if (!nzchar(host)) return(NULL)

    port_text <- if (nzchar(port) && !port %in% c("80", "443")) {
      paste0(":", port)
    } else {
      ""
    }

    base <- paste0(proto, "//", host, port_text, path)

    rows <- lapply(paste0("G", 1:9), function(g) {
      url <- paste0(base, "?group=", g)
      tags$div(
        class = "group-link-row",
        tags$strong(g),
        tags$a(href = url, target = "_blank", url)
      )
    })

    tagList(h4("Student group links"), rows)
  })

  # ---------------------------------------------------------------------------
  # Hierarchical data preparation
  # ---------------------------------------------------------------------------

  empty_variable_data <- function() {
    data.frame(
      timestamp_app = character(0),
      group_id = character(0),
      patch = character(0),
      method = character(0),
      point_id = character(0),
      person_id = character(0),
      instrument_id = character(0),
      n_readings = integer(0),
      n_people = integer(0),
      complete = logical(0),
      value = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  densitometer_ordinary_raw <- function(
      group = NULL,
      patch = NULL,
      point = NULL) {

    dat <- analysis_store()
    if (nrow(dat) == 0) return(data.frame())

    canopy <- suppressWarnings(
      as.numeric(unlist(dat$canopy_pct, use.names = FALSE))
    )

    keep <- dat$method == "Densitometer" &
      !dat$is_reference &
      is.finite(canopy)

    keep[is.na(keep)] <- FALSE

    if (!is.null(group)) {
      z <- dat$group_id == group
      z[is.na(z)] <- FALSE
      keep <- keep & z
    }

    if (!is.null(patch)) {
      z <- dat$patch == patch
      z[is.na(z)] <- FALSE
      keep <- keep & z
    }

    if (!is.null(point)) {
      z <- dat$point_id == point
      z[is.na(z)] <- FALSE
      keep <- keep & z
    }

    if (!any(keep)) return(data.frame())

    out <- dat[
      keep,
      c(
        "timestamp_app", "group_id", "patch", "point_id",
        "person_id", "is_repeat"
      ),
      drop = FALSE
    ]

    out$value <- canopy[keep]
    out$person_id[
      is.na(out$person_id) | !nzchar(out$person_id)
    ] <- "Unknown"

    out
  }

  densitometer_person_means <- function(
      group = NULL,
      patch = NULL,
      point = NULL) {

    d <- densitometer_ordinary_raw(group, patch, point)
    if (nrow(d) == 0) return(data.frame())

    key <- paste(
      d$group_id,
      d$patch,
      d$point_id,
      d$person_id,
      sep = "|"
    )

    rows <- lapply(split(seq_len(nrow(d)), key), function(ii) {
      z <- d[ii, , drop = FALSE]
      data.frame(
        group_id = z$group_id[1],
        patch = z$patch[1],
        point_id = z$point_id[1],
        person_id = z$person_id[1],
        n_readings = nrow(z),
        value = mean(z$value, na.rm = TRUE),
        sd_within = safe_sd(z$value),
        stringsAsFactors = FALSE
      )
    })

    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }

  densitometer_location_means <- function(
      group = NULL,
      patch = NULL) {

    pm <- densitometer_person_means(group, patch)
    if (nrow(pm) == 0) return(empty_variable_data())

    key <- paste(
      pm$group_id,
      pm$patch,
      pm$point_id,
      sep = "|"
    )

    rows <- lapply(split(seq_len(nrow(pm)), key), function(ii) {
      z <- pm[ii, , drop = FALSE]

      data.frame(
        timestamp_app = "",
        group_id = z$group_id[1],
        patch = z$patch[1],
        method = "Densitometer",
        point_id = z$point_id[1],
        person_id = "",
        instrument_id = "",
        n_readings = sum(z$n_readings),
        n_people = length(unique(z$person_id)),
        complete =
          length(unique(z$person_id)) >= 3 &&
          all(z$n_readings >= 3),
        value = mean(z$value, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })

    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }

  kestrel_raw <- function(
      var,
      group = NULL,
      patch = NULL) {

    dat <- analysis_store()

    if (nrow(dat) == 0 ||
        is.null(var) ||
        !var %in% names(dat)) {
      return(data.frame())
    }

    vals <- suppressWarnings(
      as.numeric(unlist(dat[[var]], use.names = FALSE))
    )

    keep <- dat$method == "Kestrel" &
      !dat$is_reference &
      is.finite(vals)

    keep[is.na(keep)] <- FALSE

    if (!is.null(group) &&
        !identical(group, "All groups") &&
        !identical(group, "All")) {

      z <- dat$group_id == group
      z[is.na(z)] <- FALSE
      keep <- keep & z
    }

    if (!is.null(patch) &&
        !identical(patch, "All")) {

      z <- dat$patch == patch
      z[is.na(z)] <- FALSE
      keep <- keep & z
    }

    if (!any(keep)) return(data.frame())

    out <- dat[
      keep,
      c(
        "timestamp_app", "group_id", "patch", "point_id",
        "instrument_id", "is_repeat"
      ),
      drop = FALSE
    ]

    out$value <- vals[keep]
    out
  }

  kestrel_location_means <- function(
      var,
      group = NULL,
      patch = NULL) {

    d <- kestrel_raw(var, group, patch)
    if (nrow(d) == 0) return(empty_variable_data())

    key <- paste(
      d$group_id,
      d$patch,
      d$point_id,
      sep = "|"
    )

    rows <- lapply(split(seq_len(nrow(d)), key), function(ii) {
      z <- d[ii, , drop = FALSE]

      inst <- unique(
        z$instrument_id[
          !is.na(z$instrument_id) &
            nzchar(z$instrument_id)
        ]
      )

      data.frame(
        timestamp_app = z$timestamp_app[1],
        group_id = z$group_id[1],
        patch = z$patch[1],
        method = "Kestrel",
        point_id = z$point_id[1],
        person_id = "",
        instrument_id = paste(sort(inst), collapse = ", "),
        n_readings = nrow(z),
        n_people = NA_integer_,
        complete = nrow(z) >= 5,
        value = mean(z$value, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })

    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }

  variable_data <- function(
      var,
      ordinary_only = TRUE,
      group = NULL,
      patch = NULL) {

    if (identical(var, "canopy_pct")) {
      return(
        densitometer_location_means(
          group = if (!is.null(group) &&
                      !group %in% c("All", "All groups")) {
            group
          } else {
            NULL
          },
          patch = if (!is.null(patch) &&
                      patch != "All") {
            patch
          } else {
            NULL
          }
        )
      )
    }

    if (ordinary_only) {
      return(kestrel_location_means(var, group, patch))
    }

    d <- kestrel_raw(var, group, patch)
    if (nrow(d) == 0) return(empty_variable_data())

    data.frame(
      timestamp_app = d$timestamp_app,
      group_id = d$group_id,
      patch = d$patch,
      method = "Kestrel",
      point_id = d$point_id,
      person_id = "",
      instrument_id = d$instrument_id,
      n_readings = 1L,
      n_people = NA_integer_,
      complete = TRUE,
      value = d$value,
      stringsAsFactors = FALSE
    )
  }

  canopy_location_completeness <- function(dat) {

    if (nrow(dat) == 0) return(data.frame())

    vals <- suppressWarnings(as.numeric(dat$canopy_pct))

    keep <- dat$method == "Densitometer" &
      !dat$is_reference &
      is.finite(vals)

    keep[is.na(keep)] <- FALSE

    raw <- dat[keep, , drop = FALSE]
    if (nrow(raw) == 0) return(data.frame())

    raw$person_id[
      is.na(raw$person_id) | !nzchar(raw$person_id)
    ] <- "Unknown"

    pkey <- paste(
      raw$group_id,
      raw$patch,
      raw$point_id,
      raw$person_id,
      sep = "|"
    )

    person_counts <- lapply(
      split(seq_len(nrow(raw)), pkey),
      function(ii) {
        z <- raw[ii, , drop = FALSE]
        data.frame(
          group_id = z$group_id[1],
          patch = z$patch[1],
          point_id = z$point_id[1],
          person_id = z$person_id[1],
          n_readings = nrow(z),
          stringsAsFactors = FALSE
        )
      }
    )

    pm <- do.call(rbind, person_counts)

    lkey <- paste(
      pm$group_id,
      pm$patch,
      pm$point_id,
      sep = "|"
    )

    rows <- lapply(
      split(seq_len(nrow(pm)), lkey),
      function(ii) {
        z <- pm[ii, , drop = FALSE]
        data.frame(
          group_id = z$group_id[1],
          patch = z$patch[1],
          point_id = z$point_id[1],
          people = length(unique(z$person_id)),
          readings = sum(z$n_readings),
          complete =
            length(unique(z$person_id)) >= 3 &&
            all(z$n_readings >= 3),
          stringsAsFactors = FALSE
        )
      }
    )

    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }

  kestrel_location_completeness <- function(dat) {

    if (nrow(dat) == 0) return(data.frame())

    keep <- dat$method == "Kestrel" & !dat$is_reference
    keep[is.na(keep)] <- FALSE

    d <- dat[keep, , drop = FALSE]
    if (nrow(d) == 0) return(data.frame())

    key <- paste(
      d$group_id,
      d$patch,
      d$point_id,
      sep = "|"
    )

    rows <- lapply(
      split(seq_len(nrow(d)), key),
      function(ii) {
        z <- d[ii, , drop = FALSE]
        data.frame(
          group_id = z$group_id[1],
          patch = z$patch[1],
          point_id = z$point_id[1],
          readings = nrow(z),
          complete = nrow(z) >= 5,
          stringsAsFactors = FALSE
        )
      }
    )

    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }

  # ---------------------------------------------------------------------------
  # Overview
  # ---------------------------------------------------------------------------

  output$progress_table <- renderTable({

    dat <- analysis_store()
    groups <- paste0("G", 1:9)

    dc <- canopy_location_completeness(dat)
    kc <- kestrel_location_completeness(dat)

    rows <- lapply(groups, function(g) {

      dz <- if (nrow(dc)) {
        dc[dc$group_id == g, , drop = FALSE]
      } else {
        data.frame()
      }

      kz <- if (nrow(kc)) {
        kc[kc$group_id == g, , drop = FALSE]
      } else {
        data.frame()
      }

      data.frame(
        Group = g,
        Canopy_loc_A =
          if (nrow(dz)) sum(dz$patch == "A") else 0,
        Canopy_loc_B =
          if (nrow(dz)) sum(dz$patch == "B") else 0,
        Canopy_below_plan =
          if (nrow(dz)) sum(!dz$complete) else 0,
        Kestrel_loc_A =
          if (nrow(kz)) sum(kz$patch == "A") else 0,
        Kestrel_loc_B =
          if (nrow(kz)) sum(kz$patch == "B") else 0,
        Kestrel_below_plan =
          if (nrow(kz)) sum(!kz$complete) else 0,
        Reference_reads =
          sum(
            dat$group_id == g &
              dat$method == "Densitometer" &
              dat$is_reference,
            na.rm = TRUE
          )
      )
    })

    do.call(rbind, rows)

  }, striped = TRUE, spacing = "s")

  output$data_check_table <- renderTable({

    dat <- analysis_store()
    if (nrow(dat) == 0) return(NULL)

    dc <- canopy_location_completeness(dat)
    kc <- kestrel_location_completeness(dat)

    data.frame(
      Category = c(
        "All rows",
        "Canopy sampling locations",
        "Canopy locations complete (>=3 people x >=3 readings)",
        "Canopy locations below planned replication",
        "Canopy ordinary raw readings",
        "Densitometer reference readings",
        "Kestrel sampling locations",
        "Kestrel locations complete (>=5 readings)",
        "Kestrel locations below planned replication",
        "Kestrel raw readings"
      ),
      n = c(
        nrow(dat),
        nrow(dc),
        if (nrow(dc)) sum(dc$complete) else 0,
        if (nrow(dc)) sum(!dc$complete) else 0,
        sum(
          dat$method == "Densitometer" &
            !dat$is_reference,
          na.rm = TRUE
        ),
        sum(
          dat$method == "Densitometer" &
            dat$is_reference,
          na.rm = TRUE
        ),
        nrow(kc),
        if (nrow(kc)) sum(kc$complete) else 0,
        if (nrow(kc)) sum(!kc$complete) else 0,
        sum(
          dat$method == "Kestrel" &
            !dat$is_reference,
          na.rm = TRUE
        )
      )
    )

  }, striped = TRUE, spacing = "s")

  # ---------------------------------------------------------------------------
  # Patch and group estimates
  # ---------------------------------------------------------------------------

  selected_variable_data <- reactive({
    req(input$analysis_var)
    variable_data(input$analysis_var, ordinary_only = TRUE)
  })

  output$patch_plot <- renderPlot({

    d <- selected_variable_data()

    if (nrow(d) == 0 || !any(is.finite(d$value))) {
      empty_plot("No spatial location means for this variable yet.")
      return(invisible(NULL))
    }

    d <- d[
      is.finite(d$value) &
        d$patch %in% c("A", "B") &
        d$group_id %in% paste0("G", 1:9),
      ,
      drop = FALSE
    ]

    if (nrow(d) == 0) {
      empty_plot("No spatial location means for this variable yet.")
      return(invisible(NULL))
    }

    groups <- paste0("G", 1:9)
    cols <- grDevices::hcl.colors(9, "Dark 3")

    yr <- range(d$value, finite = TRUE)
    pad <- if (diff(yr) == 0) 1 else diff(yr) * 0.08
    yr <- yr + c(-pad, pad)

    oldpar <- par(
      mfrow = c(1, 2),
      mar = c(4.2, 4.2, 2.2, 1)
    )
    on.exit(par(oldpar), add = TRUE)

    for (p in c("A", "B")) {

      z <- d[d$patch == p, , drop = FALSE]

      vals <- lapply(
        groups,
        function(g) z$value[z$group_id == g]
      )

      present <- lengths(vals) > 0

      if (!any(present)) {
        plot.new()
        title(main = paste("Patch", p))
        text(0.5, 0.5, "No location means yet.")
        next
      }

      boxplot(
        vals[present],
        at = which(present),
        xlim = c(0.5, 9.5),
        ylim = yr,
        xaxt = "n",
        outline = FALSE,
        border = cols[which(present)],
        col = grDevices::adjustcolor(
          cols[which(present)],
          alpha.f = 0.10
        ),
        xlab = "Student group",
        ylab = unname(VARIABLE_LABELS[input$analysis_var]),
        main = paste("Patch", p)
      )

      axis(
        1,
        at = 1:9,
        labels = groups,
        cex.axis = 0.8
      )

      set.seed(2301 + match(p, c("A", "B")))

      for (i in seq_along(groups)) {

        zz <- z[z$group_id == groups[i], , drop = FALSE]
        if (!nrow(zz)) next

        points(
          jitter(rep(i, nrow(zz)), amount = 0.08),
          zz$value,
          pch = 16,
          cex = 0.72,
          col = cols[i]
        )

        points(
          i,
          mean(zz$value),
          pch = 18,
          cex = 1.25,
          col = cols[i]
        )
      }
    }
  })

  output$patch_summary <- renderTable({

    d <- selected_variable_data()
    if (nrow(d) == 0) return(NULL)

    rows <- lapply(c("A", "B"), function(p) {

      z <- d[
        d$patch == p & is.finite(d$value),
        ,
        drop = FALSE
      ]

      s <- summarise_values(z$value)

      data.frame(
        Patch = p,
        Groups = length(unique(z$group_id)),
        Locations = s$n,
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

        x <- d$value[
          d$group_id == g &
            d$patch == p
        ]

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

    y <- suppressWarnings(
      as.numeric(c(s$mean_A, s$mean_B))
    )
    y <- y[is.finite(y)]

    if (!length(y)) {
      empty_plot("No group estimates available yet.")
      return(invisible(NULL))
    }

    yr <- range(y, finite = TRUE)
    pad <- if (diff(yr) == 0) 1 else 0.08 * diff(yr)
    yr <- yr + c(-pad, pad)

    cols <- grDevices::hcl.colors(9, "Dark 3")

    plot(
      NA_real_,
      NA_real_,
      xlim = c(0.8, 2.2),
      ylim = yr,
      xaxt = "n",
      xlab = "Patch",
      ylab = unname(VARIABLE_LABELS[input$analysis_var])
    )

    axis(1, at = c(1, 2), labels = c("A", "B"))

    for (i in seq_len(nrow(s))) {

      vals <- suppressWarnings(
        as.numeric(c(s$mean_A[i], s$mean_B[i]))
      )

      good <- is.finite(vals)

      if (any(good)) {
        points(
          c(1, 2)[good],
          vals[good],
          pch = 16,
          col = cols[i],
          cex = 1.05
        )
      }

      if (all(good)) {
        lines(
          c(1, 2),
          vals,
          col = cols[i],
          lwd = 1.4
        )
      }
    }

    legend(
      "topright",
      legend = s$Group,
      col = cols,
      pch = 16,
      lty = 1,
      cex = 0.72,
      ncol = 3,
      bty = "n"
    )
  })

  output$group_summary <- renderTable({

    s <- group_patch_summary()

    if (is.null(s) || nrow(s) == 0) return(NULL)

    data.frame(
      Group = s$Group,
      Locations_A = s$n_A,
      Mean_A = round(s$mean_A, 3),
      SD_A = round(s$sd_A, 3),
      SE_A = round(s$se_A, 3),
      Locations_B = s$n_B,
      Mean_B = round(s$mean_B, 3),
      SD_B = round(s$sd_B, 3),
      SE_B = round(s$se_B, 3),
      A_minus_B = round(s$A_minus_B, 3)
    )

  }, striped = TRUE, spacing = "s", na = "")

  # ---------------------------------------------------------------------------
  # Same-location variation
  # ---------------------------------------------------------------------------

  output$same_location_ui <- renderUI({

    d <- densitometer_ordinary_raw(
      input$same_group,
      input$same_patch
    )

    pts <- if (nrow(d)) {
      sort(unique(d$point_id[nzchar(d$point_id)]))
    } else {
      character(0)
    }

    selectInput(
      "same_location",
      "Location",
      choices = if (length(pts)) pts else c("No locations" = ""),
      selected = if (length(pts)) pts[1] else ""
    )
  })

  output$same_location_plot <- renderPlot({

    req(
      input$same_patch,
      input$same_group,
      input$same_location
    )

    d <- densitometer_ordinary_raw(
      input$same_group,
      input$same_patch,
      input$same_location
    )

    if (nrow(d) == 0) {
      empty_plot("No canopy readings for this location yet.")
      return(invisible(NULL))
    }

    people <- sort(unique(d$person_id))
    x <- match(d$person_id, people)

    cols <- grDevices::hcl.colors(
      max(3, length(people)),
      "Dark 3"
    )[seq_along(people)]

    yr <- range(d$value, finite = TRUE)
    pad <- if (diff(yr) == 0) 2 else max(2, 0.12 * diff(yr))

    plot(
      jitter(x, amount = 0.07),
      d$value,
      pch = 16,
      col = cols[x],
      xaxt = "n",
      xlim = c(0.5, length(people) + 0.5),
      ylim = yr + c(-pad, pad),
      xlab = "Person",
      ylab = "Canopy cover (%)"
    )

    axis(
      1,
      at = seq_along(people),
      labels = people
    )

    pm <- tapply(
      d$value,
      d$person_id,
      mean,
      na.rm = TRUE
    )

    ppos <- match(names(pm), people)

    points(
      ppos,
      pm,
      pch = 18,
      cex = 1.5,
      col = cols[ppos]
    )

    locmean <- mean(pm, na.rm = TRUE)
    abline(h = locmean, lty = 2, lwd = 2)

    legend(
      "topright",
      legend = c(
        "Raw reading",
        "Person mean",
        "Location mean"
      ),
      pch = c(16, 18, NA),
      lty = c(NA, NA, 2),
      bty = "n",
      cex = 0.8
    )
  })

  output$same_location_table <- renderTable({

    req(
      input$same_patch,
      input$same_group,
      input$same_location
    )

    d <- densitometer_ordinary_raw(
      input$same_group,
      input$same_patch,
      input$same_location
    )

    if (nrow(d) == 0) return(NULL)

    rows <- lapply(
      split(d, d$person_id),
      function(z) {
        data.frame(
          Person = z$person_id[1],
          Readings = nrow(z),
          Mean = mean(z$value),
          SD = safe_sd(z$value),
          Min = min(z$value),
          Max = max(z$value)
        )
      }
    )

    out <- do.call(rbind, rows)

    for (nm in c("Mean", "SD", "Min", "Max")) {
      out[[nm]] <- round(out[[nm]], 2)
    }

    out

  }, striped = TRUE, spacing = "s", na = "")

  reference_person_means <- reactive({

    dat <- analysis_store()

    canopy <- suppressWarnings(
      as.numeric(unlist(dat$canopy_pct, use.names = FALSE))
    )

    keep <- dat$method == "Densitometer" &
      dat$is_reference &
      is.finite(canopy)

    keep[is.na(keep)] <- FALSE

    if (!any(keep)) return(data.frame())

    d <- dat[
      keep,
      c(
        "group_id", "patch",
        "point_id", "person_id"
      ),
      drop = FALSE
    ]

    d$value <- canopy[keep]

    d$person_id[
      is.na(d$person_id) | !nzchar(d$person_id)
    ] <- "Unknown"

    key <- paste(
      d$group_id,
      d$patch,
      d$point_id,
      d$person_id,
      sep = "|"
    )

    rows <- lapply(
      split(seq_len(nrow(d)), key),
      function(ii) {

        z <- d[ii, , drop = FALSE]

        data.frame(
          group_id = z$group_id[1],
          patch = z$patch[1],
          point_id = z$point_id[1],
          person_id = z$person_id[1],
          n_readings = nrow(z),
          value = mean(z$value),
          stringsAsFactors = FALSE
        )
      }
    )

    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  })

  output$reference_plot <- renderPlot({

    d <- reference_person_means()

    if (nrow(d) == 0) {
      empty_plot("No shared-reference readings yet.")
      return(invisible(NULL))
    }

    lev <- c(
      paste0("R-A", 1:5),
      paste0("R-B", 1:5)
    )

    d$point <- factor(d$point_id, levels = lev)
    d <- d[!is.na(d$point), , drop = FALSE]

    cols <- grDevices::hcl.colors(9, "Dark 3")

    gi <- match(
      d$group_id,
      paste0("G", 1:9)
    )
    gi[is.na(gi)] <- 1L

    set.seed(2301)

    plot(
      jitter(as.numeric(d$point), amount = 0.10),
      d$value,
      pch = 16,
      col = cols[gi],
      xaxt = "n",
      xlab = "Shared reference point",
      ylab = "Canopy cover (%)",
      xlim = c(0.5, 10.5)
    )

    axis(
      1,
      at = 1:10,
      labels = c(
        paste0("A", 1:5),
        paste0("B", 1:5)
      )
    )

    means <- tapply(
      d$value,
      d$point,
      mean,
      na.rm = TRUE
    )

    good <- which(is.finite(means))

    if (length(good)) {
      points(
        good,
        means[good],
        pch = 18,
        cex = 1.5
      )
    }

    legend(
      "topright",
      legend = c(
        paste0("G", 1:9),
        "Reference mean"
      ),
      col = c(cols, "black"),
      pch = c(rep(16, 9), 18),
      cex = 0.68,
      ncol = 2,
      bty = "n"
    )
  })

  output$reference_summary <- renderTable({

    d <- reference_person_means()
    if (nrow(d) == 0) return(NULL)

    lev <- c(
      paste0("R-A", 1:5),
      paste0("R-B", 1:5)
    )

    rows <- lapply(lev, function(id) {

      z <- d[d$point_id == id, , drop = FALSE]
      if (!nrow(z)) return(NULL)

      data.frame(
        Point = id,
        Person_means = nrow(z),
        Groups = length(unique(z$group_id)),
        Mean = mean(z$value),
        SD = safe_sd(z$value),
        Min = min(z$value),
        Max = max(z$value)
      )
    })

    out <- do.call(rbind, rows)
    if (is.null(out)) return(NULL)

    for (nm in c("Mean", "SD", "Min", "Max")) {
      out[[nm]] <- round(out[[nm]], 2)
    }

    out

  }, striped = TRUE, spacing = "s", na = "")

  kestrel_repeat_data <- reactive({

    req(input$repeat_var)

    d <- kestrel_raw(input$repeat_var)
    if (nrow(d) == 0) return(data.frame())

    d$key <- paste(
      d$group_id,
      d$patch,
      d$point_id,
      sep = "|"
    )

    d
  })

  output$kestrel_repeat_table <- renderTable({

    d <- kestrel_repeat_data()

    if (nrow(d) == 0) {
      return(
        data.frame(
          Message = "No Kestrel readings yet."
        )
      )
    }

    rows <- lapply(
      split(d, d$key),
      function(z) {

        data.frame(
          Group = z$group_id[1],
          Patch = z$patch[1],
          Point = z$point_id[1],
          Readings = nrow(z),
          Protocol =
            if (nrow(z) >= 5) {
              "5/5"
            } else {
              paste0(nrow(z), "/5")
            },
          Mean = mean(z$value),
          SD = safe_sd(z$value),
          Range = diff(range(z$value))
        )
      }
    )

    out <- do.call(rbind, rows)

    for (nm in c("Mean", "SD", "Range")) {
      out[[nm]] <- round(out[[nm]], 3)
    }

    rownames(out) <- NULL
    out

  }, striped = TRUE, spacing = "s", na = "")

  pooled_within_sd <- function(d) {

    if (nrow(d) == 0) return(NA_real_)

    spl <- split(d$value, d$key)
    spl <- spl[vapply(spl, length, integer(1)) >= 2]

    if (!length(spl)) return(NA_real_)

    sds <- vapply(spl, safe_sd, numeric(1))
    ns <- vapply(spl, length, integer(1))

    good <- is.finite(sds) & ns >= 2

    if (!any(good)) return(NA_real_)

    sqrt(
      sum(
        (ns[good] - 1) * sds[good]^2
      ) /
        sum(ns[good] - 1)
    )
  }

  output$variation_scale_table <- renderTable({

    req(input$repeat_var)

    loc <- kestrel_location_means(input$repeat_var)
    raw <- kestrel_repeat_data()

    rows <- lapply(c("A", "B"), function(p) {

      x <- loc$value[loc$patch == p]
      r <- raw[raw$patch == p, , drop = FALSE]

      data.frame(
        Patch = p,
        Locations = length(x),
        SD_among_location_means = safe_sd(x),
        Locations_with_repeats =
          length(unique(r$key)),
        Pooled_SD_within_locations =
          pooled_within_sd(r)
      )
    })

    out <- do.call(rbind, rows)

    out$SD_among_location_means <-
      round(out$SD_among_location_means, 3)

    out$Pooled_SD_within_locations <-
      round(out$Pooled_SD_within_locations, 3)

    out

  }, striped = TRUE, spacing = "s", na = "")

  # ---------------------------------------------------------------------------
  # Descriptive hierarchy / variation partition
  # ---------------------------------------------------------------------------

  canopy_variation_partition <- reactive({

    d <- densitometer_ordinary_raw()

    if (nrow(d) < 2) return(NULL)

    d <- d[is.finite(d$value), , drop = FALSE]
    if (nrow(d) < 2) return(NULL)

    grand <- mean(d$value)

    patch_mean <- ave(
      d$value,
      d$patch,
      FUN = function(x) mean(x, na.rm = TRUE)
    )

    group_key <- paste(
      d$patch,
      d$group_id,
      sep = "|"
    )

    group_mean <- ave(
      d$value,
      group_key,
      FUN = function(x) mean(x, na.rm = TRUE)
    )

    location_key <- paste(
      group_key,
      d$point_id,
      sep = "|"
    )

    location_mean <- ave(
      d$value,
      location_key,
      FUN = function(x) mean(x, na.rm = TRUE)
    )

    person_key <- paste(
      location_key,
      d$person_id,
      sep = "|"
    )

    person_mean <- ave(
      d$value,
      person_key,
      FUN = function(x) mean(x, na.rm = TRUE)
    )

    ss <- c(
      "Between patches" =
        sum((patch_mean - grand)^2),
      "Groups within patches" =
        sum((group_mean - patch_mean)^2),
      "Locations within groups" =
        sum((location_mean - group_mean)^2),
      "People within locations" =
        sum((person_mean - location_mean)^2),
      "Within person / direction" =
        sum((d$value - person_mean)^2)
    )

    total <- sum((d$value - grand)^2)

    if (!is.finite(total) || total <= 0) {
      return(NULL)
    }

    data.frame(
      Component = names(ss),
      SS = as.numeric(ss),
      Percent = 100 * as.numeric(ss) / total,
      stringsAsFactors = FALSE
    )
  })

  output$variation_partition_plot <- renderPlot({

    v <- canopy_variation_partition()

    if (is.null(v) || nrow(v) == 0) {
      empty_plot(
        "Not enough canopy data for a variation partition yet."
      )
      return(invisible(NULL))
    }

    cols <- grDevices::hcl.colors(
      nrow(v),
      "Set 2"
    )

    mat <- matrix(
      v$Percent,
      nrow = nrow(v),
      ncol = 1
    )

    oldpar <- par(
      mar = c(4.5, 2.5, 2, 2)
    )
    on.exit(par(oldpar), add = TRUE)

    barplot(
      mat,
      horiz = TRUE,
      col = cols,
      border = NA,
      xlim = c(0, 100),
      names.arg = "",
      xlab = "Percent of observed sum of squares"
    )

    left <- c(
      0,
      cumsum(v$Percent)[-nrow(v)]
    )

    centers <- left + v$Percent / 2

    short <- c(
      "Patch",
      "Group",
      "Location",
      "Person",
      "Within"
    )

    for (i in seq_len(nrow(v))) {
      if (v$Percent[i] >= 5) {
        text(
          centers[i],
          0.7,
          labels = paste0(
            short[i],
            "\n",
            round(v$Percent[i], 1),
            "%"
          ),
          cex = 0.8
        )
      }
    }

    legend(
      "top",
      inset = c(0, -0.26),
      xpd = TRUE,
      legend = v$Component,
      fill = cols,
      ncol = 2,
      bty = "n",
      cex = 0.78
    )
  })

  output$variation_partition_table <- renderTable({

    v <- canopy_variation_partition()
    if (is.null(v)) return(NULL)

    data.frame(
      Component = v$Component,
      Percent = round(v$Percent, 1),
      Sum_of_squares = round(v$SS, 2)
    )

  }, striped = TRUE, spacing = "s")

  output$replication_coverage_table <- renderTable({

    loc <- densitometer_location_means()
    per <- densitometer_person_means()

    if (nrow(loc) == 0 && nrow(per) == 0) return(NULL)

    count_by_patch <- function(d, condition) {
      if (nrow(d) == 0) return(c(A = 0L, B = 0L, Total = 0L))
      flag <- condition(d)
      flag[is.na(flag)] <- FALSE
      c(
        A = sum(flag & d$patch == "A"),
        B = sum(flag & d$patch == "B"),
        Total = sum(flag)
      )
    }

    rows <- list(
      "Locations with 3+ people" =
        count_by_patch(loc, function(d) d$n_people >= 3),
      "Locations with 2 people" =
        count_by_patch(loc, function(d) d$n_people == 2),
      "Locations with 1 person" =
        count_by_patch(loc, function(d) d$n_people == 1),
      "Person-location sets with 3+ readings" =
        count_by_patch(per, function(d) d$n_readings >= 3),
      "Person-location sets with 2 readings" =
        count_by_patch(per, function(d) d$n_readings == 2),
      "Person-location sets with 1 reading" =
        count_by_patch(per, function(d) d$n_readings == 1)
    )

    out <- data.frame(
      Replication = names(rows),
      Patch_A = vapply(rows, function(x) unname(x["A"]), numeric(1)),
      Patch_B = vapply(rows, function(x) unname(x["B"]), numeric(1)),
      Total = vapply(rows, function(x) unname(x["Total"]), numeric(1)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    names(out) <- c("Replication achieved", "Patch A", "Patch B", "Total")
    out

  }, striped = TRUE, spacing = "s")

  # ---------------------------------------------------------------------------
  # Kestrel through time
  # ---------------------------------------------------------------------------

  parse_local_time <- function(x) {

    n <- length(x)
    out_num <- rep(NA_real_, n)

    if (inherits(x, "POSIXt")) {
      out <- as.POSIXct(x)
      attr(out, "tzone") <- "Asia/Singapore"
      return(out)
    }

    raw <- trimws(as.character(x))
    raw[raw %in% c("", "NA", "NULL", "null")] <- NA_character_

    for (i in seq_len(n)) {

      s <- raw[i]
      if (is.na(s) || !nzchar(s)) next

      z <- suppressWarnings(
        as.POSIXct(
          strptime(
            s,
            format = "%Y-%m-%dT%H:%M:%OSZ",
            tz = "UTC"
          )
        )
      )

      if (is.na(z)) {
        z <- suppressWarnings(
          as.POSIXct(
            strptime(
              s,
              format = "%Y-%m-%dT%H:%M:%OS",
              tz = "UTC"
            )
          )
        )
      }

      if (is.na(z)) {
        z <- suppressWarnings(
          as.POSIXct(
            strptime(
              s,
              format = "%Y-%m-%d %H:%M:%OS",
              tz = "UTC"
            )
          )
        )
      }

      if (!is.na(z)) {
        out_num[i] <- as.numeric(z)
        next
      }

      num <- suppressWarnings(as.numeric(s))

      if (length(num) == 1 && is.finite(num)) {
        if (num > 1e12) {
          out_num[i] <- num / 1000
        } else if (num > 1e9) {
          out_num[i] <- num
        } else if (num > 20000 && num < 80000) {
          out_num[i] <-
            as.numeric(
              as.POSIXct(
                "1899-12-30 00:00:00",
                tz = "UTC"
              )
            ) +
            num * 86400
        }
      }
    }

    structure(
      out_num,
      class = c("POSIXct", "POSIXt"),
      tzone = "Asia/Singapore"
    )
  }

  kestrel_time_data <- reactive({

    req(input$time_var)

    d <- variable_data(
      input$time_var,
      ordinary_only = FALSE
    )

    if (nrow(d) == 0) {
      return(empty_variable_data())
    }

    d$time_local <- parse_local_time(
      d$timestamp_app
    )

    good <-
      !is.na(d$time_local) &
      is.finite(as.numeric(d$time_local)) &
      is.finite(d$value)

    good[is.na(good)] <- FALSE

    d[good, , drop = FALSE]
  })

  output$time_plot <- renderPlot({

    d <- kestrel_time_data()

    if (nrow(d) == 0) {
      empty_plot(
        "No Kestrel observations with valid timestamps yet."
      )
      return(invisible(NULL))
    }

    d <- d[
      is.finite(d$value) &
        d$patch %in% c("A", "B"),
      ,
      drop = FALSE
    ]

    if (!nrow(d)) {
      empty_plot(
        "No Kestrel observations with valid timestamps yet."
      )
      return(invisible(NULL))
    }

    cols <- grDevices::hcl.colors(
      2,
      "Dark 2"
    )

    patch_index <- match(
      d$patch,
      c("A", "B")
    )

    plot(
      d$time_local,
      d$value,
      type = "n",
      xlab = "Local time (Singapore)",
      ylab = unname(
        VARIABLE_LABELS[input$time_var]
      )
    )

    key <- paste(
      d$group_id,
      d$patch,
      d$point_id,
      sep = "|"
    )

    for (ii in split(seq_len(nrow(d)), key)) {

      if (length(ii) < 2) next

      ii <- ii[order(d$time_local[ii])]

      p <- match(
        d$patch[ii[1]],
        c("A", "B")
      )

      lines(
        d$time_local[ii],
        d$value[ii],
        col = grDevices::adjustcolor(
          cols[p],
          alpha.f = 0.35
        ),
        lwd = 1
      )
    }

    points(
      d$time_local,
      d$value,
      pch = c(16, 17)[patch_index],
      col = cols[patch_index]
    )

    legend(
      "topright",
      legend = c("Patch A", "Patch B"),
      col = cols,
      pch = c(16, 17),
      bty = "n"
    )
  })

  output$time_order_table <- renderTable({

    d <- kestrel_time_data()
    if (nrow(d) == 0) return(NULL)

    keys <- interaction(
      d$group_id,
      d$patch,
      drop = TRUE
    )

    rows <- lapply(
      split(d, keys),
      function(z) {

        nl <- length(unique(z$point_id))

        data.frame(
          Group = z$group_id[1],
          Patch = z$patch[1],
          Locations = nl,
          Readings = nrow(z),
          Readings_per_location =
            if (nl) {
              round(nrow(z) / nl, 1)
            } else {
              NA_real_
            },
          First =
            format(
              min(z$time_local),
              "%H:%M:%S"
            ),
          Last =
            format(
              max(z$time_local),
              "%H:%M:%S"
            )
        )
      }
    )

    out <- do.call(rbind, rows)

    out <- out[
      order(
        out$First,
        out$Group,
        out$Patch
      ),
    ]

    rownames(out) <- NULL
    out

  }, striped = TRUE, spacing = "s")

  # ---------------------------------------------------------------------------
  # Sample-size demonstration
  # ---------------------------------------------------------------------------

  resample_source_data <- reactive({

    req(
      input$resample_var,
      input$resample_patch,
      input$resample_group
    )

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

    if (nmax < 2) {
      return(
        p("At least two spatial location means are needed.")
      )
    }

    sliderInput(
      "resample_n",
      "Sample size (n)",
      min = 2,
      max = nmax,
      value = min(5, nmax),
      step = 1
    )
  })

  resample_results <- reactive({

    if (is.null(input$resample_n)) return(NULL)

    d <- resample_source_data()
    x <- d$value[is.finite(d$value)]

    n <- suppressWarnings(
      as.integer(input$resample_n)
    )

    if (length(x) < 2 ||
        length(n) != 1 ||
        is.na(n) ||
        n < 2 ||
        n > length(x)) {
      return(NULL)
    }

    set.seed(2301 + n)

    means <- replicate(
      1000,
      mean(
        sample(
          x,
          size = n,
          replace = TRUE
        )
      )
    )

    list(
      x = x,
      n = n,
      means = means
    )
  })

  output$resample_plot <- renderPlot({

    z <- resample_results()

    if (is.null(z)) {
      empty_plot(
        "Not enough location means for this selection yet."
      )
      return(invisible(NULL))
    }

    hist(
      z$means,
      breaks = "FD",
      main = paste(
        "1,000 bootstrap sample means; n =",
        z$n
      ),
      xlab = "Sample mean"
    )

    abline(
      v = mean(z$x),
      lwd = 2
    )
  })

  output$resample_curve_plot <- renderPlot({

    d <- resample_source_data()
    x <- d$value[is.finite(d$value)]

    if (length(x) < 2) {
      empty_plot(
        "Not enough location means for this selection yet."
      )
      return(invisible(NULL))
    }

    ns <- 2:length(x)

    set.seed(2301)

    boot_sd <- vapply(
      ns,
      function(n) {

        vals <- replicate(
          300,
          mean(
            sample(
              x,
              size = n,
              replace = TRUE
            )
          )
        )

        sd(vals)
      },
      numeric(1)
    )

    theoretical <- sd(x) / sqrt(ns)

    yr <- range(
      c(boot_sd, theoretical),
      finite = TRUE
    )

    if (diff(yr) == 0) {
      yr <- yr + c(-0.5, 0.5)
    }

    plot(
      ns,
      boot_sd,
      type = "b",
      pch = 16,
      xlab = "Sample size (number of locations)",
      ylab = "SD of sample means",
      ylim = yr
    )

    lines(
      ns,
      theoretical,
      lty = 2,
      lwd = 2
    )

    legend(
      "topright",
      legend = c(
        "Bootstrap",
        "SD / sqrt(n)"
      ),
      lty = c(1, 2),
      pch = c(16, NA),
      bty = "n"
    )
  })

  output$resample_summary <- renderTable({

    z <- resample_results()

    if (is.null(z)) {
      return(
        data.frame(
          Message =
            "Not enough location means for this selection yet."
        )
      )
    }

    data.frame(
      Quantity = c(
        "Available spatial locations",
        "Full-data mean",
        "Observed SD",
        "Selected sample size",
        "SD of 1,000 bootstrap means",
        "SD / sqrt(n)"
      ),
      Value = c(
        length(z$x),
        round(mean(z$x), 3),
        round(sd(z$x), 3),
        z$n,
        round(sd(z$means), 3),
        round(sd(z$x) / sqrt(z$n), 3)
      )
    )

  }, striped = TRUE, spacing = "s")

  # ---------------------------------------------------------------------------
  # Raw data
  # ---------------------------------------------------------------------------

  raw_filtered <- reactive({

    dat <- analysis_store()
    if (nrow(dat) == 0) return(dat)

    keep <- rep(TRUE, nrow(dat))

    if (!is.null(input$raw_group) &&
        input$raw_group != "All") {
      keep <- keep &
        dat$group_id == input$raw_group
    }

    if (!is.null(input$raw_patch) &&
        input$raw_patch != "All") {
      keep <- keep &
        dat$patch == input$raw_patch
    }

    if (!is.null(input$raw_method) &&
        input$raw_method != "All") {
      keep <- keep &
        dat$method == input$raw_method
    }

    if (!is.null(input$raw_type) &&
        input$raw_type != "All") {

      if (input$raw_type == "Ordinary") {
        keep <- keep &
          !dat$is_reference &
          !dat$is_repeat
      }

      if (input$raw_type == "Repeat") {
        keep <- keep &
          dat$is_repeat
      }

      if (input$raw_type == "Reference") {
        keep <- keep &
          dat$is_reference
      }
    }

    keep[is.na(keep)] <- FALSE
    dat[keep, , drop = FALSE]
  })

  output$raw_table <- renderTable({

    dat <- raw_filtered()
    if (nrow(dat) == 0) return(NULL)

    tail(dat, 200)

  }, striped = TRUE, spacing = "xs")
}

shinyApp(ui, server)
