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

normalise_data <- function(x) {
  if (is.null(x) || length(x) == 0) return(empty_data())
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  for (nm in DATA_COLUMNS) {
    if (!nm %in% names(x)) x[[nm]] <- NA
  }
  x <- x[, DATA_COLUMNS, drop = FALSE]
  for (nm in NUMERIC_COLUMNS) x[[nm]] <- suppressWarnings(as.numeric(x[[nm]]))
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
          downloadButton("download_data", "Download CSV"),
          br(), br(),
          textOutput("analysis_message"),
          uiOutput("group_links")
        ),
        tabPanel(
          "Patch comparison",
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
          plotOutput("patch_plot", height = "420px"),
          h4("Group summaries"),
          tableOutput("group_summary")
        ),
        tabPanel(
          "Reference point",
          br(),
          p("Densiometer readings recorded at the five shared reference points in each patch."),
          tableOutput("reference_table")
        ),
        tabPanel(
          "Sample size",
          br(),
          p("Repeatedly draw samples from the pooled class observations and examine how sample means vary."),
          uiOutput("resample_controls"),
          plotOutput("resample_plot", height = "360px"),
          tableOutput("resample_summary")
        ),
        tabPanel(
          "Raw data",
          br(),
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

  selected_variable_data <- reactive({
    req(input$analysis_var)
    dat <- analysis_store()
    if (nrow(dat) == 0) return(data.frame())
    var <- input$analysis_var
    keep <- !is.na(dat[[var]])
    if (var == "canopy_pct") keep <- keep & dat$method == "Densitometer" else keep <- keep & dat$method == "Kestrel"
    out <- dat[keep, c("timestamp_app", "group_id", "patch", "method", "point_id", "person_id", "instrument_id", var), drop = FALSE]
    names(out)[ncol(out)] <- "value"
    out$value <- as.numeric(out$value)
    out
  })

  output$patch_plot <- renderPlot({
    d <- selected_variable_data()
    validate(need(nrow(d) > 0, "No observations for this variable yet."))
    boxplot(
      value ~ patch, data = d,
      xlab = "Patch",
      ylab = unname(VARIABLE_LABELS[input$analysis_var])
    )
    stripchart(value ~ patch, data = d, vertical = TRUE, method = "jitter", add = TRUE, pch = 16, cex = 0.8)
  })

  output$group_summary <- renderTable({
    d <- selected_variable_data()
    if (nrow(d) == 0) return(NULL)
    mean_df <- aggregate(value ~ group_id + patch, d, mean, na.rm = TRUE)
    sd_df <- aggregate(value ~ group_id + patch, d, sd, na.rm = TRUE)
    n_df <- aggregate(value ~ group_id + patch, d, length)
    names(mean_df)[3] <- "mean"
    names(sd_df)[3] <- "sd"
    names(n_df)[3] <- "n"
    out <- Reduce(function(x, y) merge(x, y, by = c("group_id", "patch"), all = TRUE), list(mean_df, sd_df, n_df))
    out$mean <- round(out$mean, 2)
    out$sd <- round(out$sd, 2)
    out[order(out$patch, out$group_id), ]
  }, striped = TRUE, spacing = "s", na = "")

  output$reference_table <- renderTable({
    dat <- analysis_store()
    if (nrow(dat) == 0) return(NULL)
    d <- dat[dat$method == "Densitometer" & dat$is_reference & !is.na(dat$canopy_pct),
             c("timestamp_app", "group_id", "patch", "point_id", "person_id", "canopy_pct"), drop = FALSE]
    if (nrow(d) == 0) return(NULL)
    d$canopy_pct <- round(d$canopy_pct, 1)
    d[order(d$patch, d$group_id, d$timestamp_app), ]
  }, striped = TRUE, spacing = "s")

  output$resample_controls <- renderUI({
    d <- selected_variable_data()
    if (nrow(d) == 0) return(p("No data available for the selected variable."))
    patches <- sort(unique(d$patch))
    patch <- patches[1]
    nmax <- max(table(d$patch))
    tagList(
      selectInput("resample_patch", "Patch", choices = patches, selected = patch),
      sliderInput("resample_n", "Sample size (n)", min = 2, max = max(2, nmax), value = min(5, max(2, nmax)), step = 1)
    )
  })

  resample_results <- reactive({
    req(input$resample_patch, input$resample_n)
    d <- selected_variable_data()
    x <- d$value[d$patch == input$resample_patch]
    x <- x[is.finite(x)]
    n <- as.integer(input$resample_n)
    validate(need(length(x) >= 2, "Not enough observations yet."))
    validate(need(n <= length(x), paste("Maximum n for this patch is", length(x))))
    set.seed(2301 + n)
    means <- replicate(500, mean(sample(x, size = n, replace = FALSE)))
    list(x = x, n = n, means = means)
  })

  output$resample_plot <- renderPlot({
    z <- resample_results()
    hist(z$means, breaks = "FD", main = paste("500 sample means; n =", z$n), xlab = "Sample mean")
    abline(v = mean(z$x), lwd = 2)
  })

  output$resample_summary <- renderTable({
    z <- resample_results()
    data.frame(
      quantity = c("Available observations", "Full-data mean", "Observed SD", "Sample size", "SD of 500 sample means", "SD / sqrt(n)"),
      value = c(
        length(z$x),
        round(mean(z$x), 3),
        round(sd(z$x), 3),
        z$n,
        round(sd(z$means), 3),
        round(sd(z$x) / sqrt(z$n), 3)
      )
    )
  }, striped = TRUE, spacing = "s")

  output$raw_table <- renderTable({
    dat <- analysis_store()
    if (nrow(dat) == 0) return(NULL)
    tail(dat, 100)
  }, striped = TRUE, spacing = "xs")
}

shinyApp(ui, server)
