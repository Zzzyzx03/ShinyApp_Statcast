# =============================================================================
#  PITCH QUALITY LAB - Shiny App
#  A pitcher's-eye, fully filterable view of Baseball Savant data
#
#  SUCCESS METRIC: CSW% = (Called strikes + Whiffs) / Pitches.
#
#  EXTRA INFERENCE GRAPH:
#  Random-sample chi-square test of pitch type vs strike-like outcome.
#
#  HOW TO RUN
#  ----------
#    install.packages(c("shiny", "ggplot2"))
#    shiny::runApp("app.R")
# =============================================================================

library(shiny)
library(ggplot2)

DATA_PATH <- "savant_data_2.csv"

# ----------------------------------------------------------------------------
#  Locate the CSV
# ----------------------------------------------------------------------------
find_data_path <- function() {
  if (file.exists(DATA_PATH)) {
    return(DATA_PATH)
  }
  NA_character_
}

ord <- function(x, lv) factor(as.character(x), levels = lv)

to_num <- function(x) suppressWarnings(as.numeric(x))

format_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("< 0.001")
  sprintf("%.4f", p)
}

# ----------------------------------------------------------------------------
#  Read + derive every analysis variable
# ----------------------------------------------------------------------------
prepare_data <- function(path) {
  df <- read.csv(
    path,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM",
    check.names = FALSE
  )

  names(df)[1] <- sub("^[^[:alnum:]_]+", "", names(df)[1])

  required_cols <- c(
    "description", "pitch_name", "release_speed",
    "release_spin_rate", "balls", "strikes",
    "pfx_x", "pfx_z", "release_extension",
    "p_throws", "stand", "home_team", "away_team",
    "inning_topbot", "plate_x", "plate_z",
    "sz_top", "sz_bot"
  )

  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Required column(s) missing from CSV:",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  numeric_cols <- c(
    "release_speed", "release_spin_rate", "balls", "strikes",
    "pfx_x", "pfx_z", "release_extension",
    "plate_x", "plate_z", "sz_top", "sz_bot"
  )

  for (nm in numeric_cols) {
    df[[nm]] <- to_num(df[[nm]])
  }

  d <- tolower(ifelse(is.na(df$description), "", df$description))

  df$is_whiff <- d %in% c(
    "swinging_strike",
    "swinging_strike_blocked",
    "foul_tip",
    "bunt_foul_tip",
    "missed_bunt"
  )

  df$is_called <- d == "called_strike"
  df$is_csw    <- df$is_whiff | df$is_called

  # Binary outcome for the random-sample chi-square graph.
  # Statcast type:
  #   B = ball
  #   S = strike
  #   X = ball put in play
  #
  # Strike-like = S or X.
  # Ball = B.
  if ("type" %in% names(df)) {
    df$strike_binary <- ifelse(
      df$type %in% c("S", "X"),
      "Strike-like",
      ifelse(df$type == "B", "Ball", NA)
    )
  } else {
    # Fallback if the CSV does not include Statcast's type column.
    df$strike_binary <- ifelse(df$is_csw, "CSW", "Non-CSW")
  }

  df$opponent <- ifelse(
    tolower(df$inning_topbot) == "top",
    df$away_team,
    df$home_team
  )

  vlv <- c("<80", "80-85", "85-90", "90-95", "95-100", "100+")

  df$velo_bin <- ord(
    cut(
      df$release_speed,
      c(-Inf, 80, 85, 90, 95, 100, Inf),
      labels = vlv
    ),
    vlv
  )

  slv <- c(
    "<1800", "1800-2100", "2100-2300",
    "2300-2500", "2500-2800", "2800+"
  )

  df$spin_bin <- ord(
    cut(
      df$release_spin_rate,
      c(-Inf, 1800, 2100, 2300, 2500, 2800, Inf),
      labels = slv
    ),
    slv
  )

  df$count_state <- paste0(df$balls, "-", df$strikes)

  mlv <- c("<6 in", "6-12", "12-18", "18-24", "24+ in")

  mov_in <- sqrt(df$pfx_x^2 + df$pfx_z^2) * 12

  df$move_bin <- ord(
    cut(
      mov_in,
      c(-Inf, 6, 12, 18, 24, Inf),
      labels = mlv
    ),
    mlv
  )

  elv <- c("<5.5 ft", "5.5-6", "6-6.5", "6.5-7", "7+ ft")

  df$ext_bin <- ord(
    cut(
      df$release_extension,
      c(-Inf, 5.5, 6, 6.5, 7, Inf),
      labels = elv
    ),
    elv
  )

  df$matchup <- paste0(df$p_throws, "HP vs ", df$stand, "HB")

  df
}

# ----------------------------------------------------------------------------
#  Filter setup
# ----------------------------------------------------------------------------
FILTERS <- list(
  list(id = "f_pitch", col = "pitch_name",  lab = "Pitch type"),
  list(id = "f_velo",  col = "velo_bin",    lab = "Velocity bin (mph)"),
  list(id = "f_spin",  col = "spin_bin",    lab = "Spin-rate bin (rpm)"),
  list(id = "f_count", col = "count_state", lab = "Count (balls-strikes)"),
  list(id = "f_move",  col = "move_bin",    lab = "Total movement"),
  list(id = "f_ext",   col = "ext_bin",     lab = "Release extension"),
  list(id = "f_match", col = "matchup",     lab = "Handedness matchup"),
  list(id = "f_opp",   col = "opponent",    lab = "Opponent (batting team)")
)

levels_of <- function(df, col) {
  v <- df[[col]]

  if (is.factor(v)) {
    return(levels(droplevels(v[!is.na(v)])))
  }

  u <- unique(as.character(v))
  u <- u[!is.na(u) & nzchar(u)]

  sort(u)
}

# ----------------------------------------------------------------------------
#  Summary table
# ----------------------------------------------------------------------------
csw_row <- function(label, s) {
  data.frame(
    Group    = label,
    Pitches  = nrow(s),
    `CSW %`  = sprintf("%.1f%%", 100 * mean(s$is_csw)),
    `Whiff %`  = sprintf("%.1f%%", 100 * mean(s$is_whiff)),
    `Called %` = sprintf("%.1f%%", 100 * mean(s$is_called)),
    check.names    = FALSE,
    stringsAsFactors = FALSE
  )
}

build_summary <- function(df) {
  if (nrow(df) == 0) return(NULL)

  head_row <- csw_row("\u25b6 ALL (current slice)", df)

  types <- df$pitch_name
  types <- sort(
    table(types[!is.na(types) & nzchar(types)]),
    decreasing = TRUE
  )

  if (length(types) == 0) return(head_row)

  rows <- lapply(names(types), function(t) {
    csw_row(t, df[!is.na(df$pitch_name) & df$pitch_name == t, ])
  })

  rbind(head_row, do.call(rbind, head(rows, 13)))
}

# ----------------------------------------------------------------------------
#  UI
# ----------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Oswald:wght@500;700&family=Roboto+Mono:wght@400;500&display=swap');

    :root {
      --night:#0e1a24; --panel:#13242f; --line:#21404f;
      --chalk:#f4f1e8; --grass:#3fa66a; --dirt:#c8794a;
      --hot:#e23b3b; --muted:#8aa3ad; --gold:#f2b134;
    }

    body {
      background:var(--night);
      color:var(--chalk);
      font-family:'Roboto Mono',monospace;
    }

    .app-title {
      font-family:'Oswald',sans-serif;
      font-weight:700;
      font-size:2.3rem;
      letter-spacing:.08em;
      text-transform:uppercase;
      color:var(--chalk);
      border-bottom:3px solid var(--dirt);
      padding-bottom:.35rem;
      margin-bottom:.1rem;
    }

    .app-sub {
      color:var(--muted);
      font-size:.82rem;
      letter-spacing:.1em;
      text-transform:uppercase;
      margin-bottom:1.3rem;
    }

    .well, .panel-card {
      background:var(--panel);
      border:1px solid var(--line);
      border-radius:6px;
    }

    .panel-card {
      padding:1rem 1.2rem;
      margin-bottom:1rem;
    }

    .panel-card h4 {
      font-family:'Oswald',sans-serif;
      font-weight:700;
      letter-spacing:.06em;
      text-transform:uppercase;
      color:var(--gold);
      margin-top:0;
      font-size:1.05rem;
    }

    table.tbl {
      border-collapse:collapse;
      width:100%;
      font-size:.8rem;
      font-family:'Roboto Mono',monospace;
    }

    table.tbl thead tr {
      background:var(--dirt);
      color:var(--night);
    }

    table.tbl th {
      padding:.45rem .55rem;
      text-align:left;
      font-family:'Oswald',sans-serif;
      letter-spacing:.03em;
      text-transform:uppercase;
    }

    table.tbl td {
      padding:.32rem .55rem;
      border-bottom:1px solid var(--line);
    }

    table.tbl tbody tr:nth-child(even) {
      background:rgba(255,255,255,.03);
    }

    table.tbl tbody tr:first-child {
      background:rgba(242,177,52,.16);
      font-weight:600;
    }

    table.tbl tbody tr:hover {
      background:rgba(242,177,52,.10);
    }

    .src-note {
      color:var(--muted);
      font-size:.72rem;
      font-style:italic;
      margin-top:.6rem;
    }

    .graph-interpretation {
      color:var(--chalk);
      background:rgba(255,255,255,.04);
      border-left:3px solid var(--gold);
      padding:.75rem .9rem;
      border-radius:4px;
      font-size:.82rem;
      line-height:1.45;
      margin-top:.75rem;
    }

    .takeaway {
      border-left:3px solid var(--grass);
      background:rgba(63,166,106,.10);
      padding:.6rem 1rem;
      border-radius:4px;
      font-size:.85rem;
    }

    .warn {
      border-left:3px solid var(--hot);
      background:rgba(226,59,59,.12);
      padding:.6rem 1rem;
      border-radius:4px;
      color:#ffb3b3;
      font-size:.85rem;
    }

    .control-label, label {
      color:var(--chalk) !important;
      font-family:'Oswald',sans-serif;
      letter-spacing:.04em;
    }

    .filt-hint {
      color:var(--muted);
      font-size:.7rem;
      font-style:italic;
      margin:-.4rem 0 .8rem 0;
    }
  "))),

  div(class = "app-title", "Pitch Quality Lab"),

  div(
    class = "app-sub",
    "Stack filters, map CSW% by location, and test pitch type vs strike outcome with random samples"
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Filters - stack any combination"),

      div(
        class = "filt-hint",
        "Leave a box empty = no restriction. Tick several to widen it."
      ),

      uiOutput("filter_ui"),

      actionButton("reset", "Reset all filters"),

      tags$hr(),

      h4("Random sample test"),

      div(
        class = "filt-hint",
        "Choose a sample size, then redraw the random sample for the chi-square test."
      ),

      sliderInput(
        "chisq_n",
        "Sample size",
        min   = 50,
        max   = 1000,
        value = 250,
        step  = 50
      ),

      actionButton(
        "draw_chisq_sample",
        "Draw new chi-square sample"
      ),

      conditionalPanel(
        condition = "output.needs_upload == true",
        tags$hr(),
        fileInput(
          "csv",
          "Upload savant_data_2.csv",
          accept = ".csv"
        )
      ),

      uiOutput("src_note")
    ),

    mainPanel(
      width = 9,

      uiOutput("missing_msg"),

      fluidRow(
        column(
          6,
          div(
            class = "panel-card",
            h4("Summary - Slice + Pitch Breakdown"),
            uiOutput("summary_table"),
            div(
              class = "src-note",
              paste(
                "First row = whole filtered slice;",
                "rows below = per pitch type.",
                "CSW% = called strikes plus whiffs divided by pitches."
              )
            )
          )
        ),

        column(
          6,
          div(
            class = "panel-card",
            h4("CSW% by Location - Catcher's View"),
            plotOutput("heat_plot", height = "370px"),
            div(
              class = "graph-interpretation",
              strong("Interpretation: "),
              "This heatmap shows where pitches are most effective at producing CSW outcomes,
              meaning called strikes or whiffs. The graph is shown from the catcher's view,
              so the horizontal axis represents pitch location across the plate and the vertical
              axis represents pitch height. The dashed rectangle marks the approximate strike zone.
              Warmer areas indicate locations where a larger share of pitches earned called strikes
              or swings and misses, while cooler areas indicate lower CSW rates. This graph is useful
              because it connects pitch quality to location instead of only looking at pitch type or
              velocity. It also updates with the filters, so the user can see how the effective
              location zones change for different pitch types, speeds, counts, movement profiles,
              matchups, or opponents."
            )
          )
        )
      ),

      fluidRow(
        column(
          12,
          div(
            class = "panel-card",
            h4("Random Sample Chi-Square Test"),
            plotOutput("chisq_sample_plot", height = "410px"),
            div(class = "src-note", textOutput("chisq_sample_note")),
            div(
              class = "graph-interpretation",
              strong("Interpretation: "),
              "This graph takes a random sample from the currently filtered data and compares
              strike-like outcomes across pitch types. Each bar represents one pitch type in the
              random sample, and the colored sections show the proportion of pitches that were balls
              versus strike-like outcomes. The chi-square test asks whether pitch type and strike
              outcome appear independent in that sample. A small p-value suggests that the
              distribution of balls and strike-like outcomes differs by pitch type, while a larger
              p-value suggests that the sample does not provide strong evidence of an association.
              Because the user can change the sample size and redraw the sample, this graph also
              shows how statistical evidence can shift when the sample changes. It should be
              interpreted as an interactive inference tool, not as proof that every individual
              pitch is independent."
            )
          )
        )
      ),

      fluidRow(
        column(
          12,
          div(
            class = "panel-card",
            div(class = "takeaway", textOutput("takeaway"))
          )
        )
      )
    )
  )
)

# ----------------------------------------------------------------------------
#  Server
# ----------------------------------------------------------------------------
server <- function(input, output, session) {

  current_path <- reactive({
    if (!is.null(input$csv)) return(input$csv$datapath)
    find_data_path()
  })

  dataset <- reactive({
    p <- current_path()
    if (is.na(p)) return(NULL)
    prepare_data(p)
  })

  output$needs_upload <- reactive({
    is.na(find_data_path()) && is.null(input$csv)
  })

  outputOptions(output, "needs_upload", suspendWhenHidden = FALSE)

  output$missing_msg <- renderUI({
    if (is.na(find_data_path()) && is.null(input$csv)) {
      div(
        class = "panel-card",
        div(
          class = "warn",
          paste0(
            "savant_data_2.csv was not found on disk. ",
            "Put it in the same folder as this app or use the uploader. ",
            "No synthetic data is used."
          )
        )
      )
    }
  })

  output$src_note <- renderUI({
    txt <- if (!is.null(input$csv)) {
      paste0("Live data uploaded: ", input$csv$name)
    } else if (!is.na(find_data_path())) {
      paste0("Live data on disk: ", find_data_path())
    } else {
      "Waiting for the CSV..."
    }
    div(class = "src-note", txt)
  })

  output$filter_ui <- renderUI({
    d <- dataset()
    if (is.null(d)) return(NULL)

    lapply(FILTERS, function(f) {
      selectizeInput(
        f$id,
        f$lab,
        choices  = levels_of(d, f$col),
        selected = character(0),
        multiple = TRUE,
        options  = list(
          placeholder = "All (no filter)",
          plugins     = list("remove_button")
        )
      )
    })
  })

  observeEvent(input$reset, {
    d <- dataset()
    if (is.null(d)) return()

    for (f in FILTERS) {
      updateSelectizeInput(session, f$id, selected = character(0))
    }
  })

  filtered <- reactive({
    d <- dataset()
    if (is.null(d)) return(NULL)

    for (f in FILTERS) {
      sel <- input[[f$id]]
      if (!is.null(sel) && length(sel) > 0) {
        d <- d[
          !is.na(d[[f$col]]) & as.character(d[[f$col]]) %in% sel,
        ]
      }
    }
    d
  })

  # ---- Summary table ----
  output$summary_table <- renderUI({
    d <- filtered()

    if (is.null(d)) {
      return(HTML("<em>Load the CSV to see the summary.</em>"))
    }

    if (nrow(d) == 0) {
      return(HTML(paste0(
        "<em>No pitches match the current filters. ",
        "Loosen or reset them.</em>"
      )))
    }

    df  <- build_summary(d)
    hdr <- paste0("<th>", names(df), "</th>", collapse = "")

    body <- apply(df, 1, function(r) {
      paste0(
        "<tr>",
        paste0("<td>", r, "</td>", collapse = ""),
        "</tr>"
      )
    })

    HTML(paste0(
      "<table class='tbl'><thead><tr>", hdr,
      "</tr></thead><tbody>",
      paste0(body, collapse = ""),
      "</tbody></table>"
    ))
  })

  # ---- Graph 1: CSW% location heatmap ----
  output$heat_plot <- renderPlot({
    d <- filtered()

    validate(need(!is.null(d),  "Load the CSV to see the heatmap."))
    validate(need(nrow(d) > 0,  "No pitches match the current filters."))

    d <- d[
      !is.na(d$plate_x) & !is.na(d$plate_z) &
        abs(d$plate_x) <= 2 &
        d$plate_z >= 0.5 & d$plate_z <= 4.5,
    ]

    validate(need(
      nrow(d) >= 25,
      "Too few pitches in this slice to map (need >= 25)."
    ))

    sz_top <- mean(d$sz_top, na.rm = TRUE)
    sz_bot <- mean(d$sz_bot, na.rm = TRUE)

    ggplot(d, aes(plate_x, plate_z, z = as.numeric(is_csw))) +
      stat_summary_2d(bins = 14, fun = mean) +
      scale_fill_gradientn(
        colours = c("#13242f", "#2f5b6b", "#3fa66a", "#f2b134", "#e23b3b"),
        name    = "CSW%",
        labels  = function(x) paste0(round(x * 100), "%")
      ) +
      annotate(
        "rect",
        xmin = -0.83, xmax = 0.83,
        ymin = sz_bot, ymax = sz_top,
        fill      = NA,
        colour    = "#f4f1e8",
        linewidth = 0.9,
        linetype  = "dashed"
      ) +
      coord_fixed(xlim = c(-2, 2), ylim = c(0.5, 4.5)) +
      labs(
        x     = "Horizontal location (ft, catcher's view)",
        y     = "Height (ft)",
        title = "Where the slice earns strikes"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.background  = element_rect(fill = "#13242f", colour = NA),
        panel.background = element_rect(fill = "#13242f", colour = NA),
        panel.grid       = element_line(colour = "#21404f"),
        text             = element_text(colour = "#f4f1e8"),
        axis.text        = element_text(colour = "#8aa3ad"),
        plot.title       = element_text(face = "bold", colour = "#f2b134"),
        legend.position  = "right",
        legend.background = element_rect(fill = "#13242f", colour = NA),
        legend.key        = element_rect(fill = "#13242f", colour = NA)
      )
  })

  # ---- Random sample for chi-square graph ----
  chisq_sample <- eventReactive(
    {
      list(input$draw_chisq_sample, input$chisq_n)
    },
    {
      d <- filtered()

      if (is.null(d) || nrow(d) == 0) return(NULL)

      d <- d[
        !is.na(d$pitch_name) & nzchar(d$pitch_name) &
          !is.na(d$strike_binary),
      ]

      if (nrow(d) < 50) return(NULL)

      sample_size <- min(input$chisq_n, nrow(d))
      sampled     <- d[sample(seq_len(nrow(d)), sample_size), ]

      sampled$pitch_group <- sampled$pitch_name

      pitch_counts <- table(sampled$pitch_group)
      rare_pitches <- names(pitch_counts[pitch_counts < 10])
      sampled$pitch_group[sampled$pitch_group %in% rare_pitches] <- "Rare pitch type"

      sampled$pitch_group  <- as.character(sampled$pitch_group)
      sampled$strike_binary <- as.character(sampled$strike_binary)

      tab <- table(sampled$pitch_group, sampled$strike_binary)

      if (nrow(tab) < 2 || ncol(tab) < 2) return(NULL)

      first_test    <- suppressWarnings(chisq.test(tab))
      min_expected  <- min(first_test$expected)
      use_simulated <- min_expected < 5

      final_test <- suppressWarnings(
        chisq.test(tab, simulate.p.value = use_simulated, B = 2000)
      )

      plot_df        <- as.data.frame(tab)
      names(plot_df) <- c("pitch_group", "strike_binary", "pitches")

      pitch_totals        <- aggregate(pitches ~ pitch_group, data = plot_df, FUN = sum)
      names(pitch_totals)[2] <- "total_pitches"

      plot_df        <- merge(plot_df, pitch_totals, by = "pitch_group")
      plot_df$prop   <- plot_df$pitches / plot_df$total_pitches

      order_df           <- pitch_totals[order(-pitch_totals$total_pitches), ]
      plot_df$pitch_group <- factor(plot_df$pitch_group, levels = order_df$pitch_group)

      list(
        data        = plot_df,
        table       = tab,
        test        = final_test,
        sample_size = sample_size,
        min_expected = min_expected,
        simulated   = use_simulated
      )
    },
    ignoreNULL = FALSE
  )

  # ---- Graph 2: Chi-square random sample stacked bar chart ----
  output$chisq_sample_plot <- renderPlot({
    cs <- chisq_sample()

    validate(need(
      !is.null(cs),
      "Not enough data in this filtered slice to run the chi-square sample."
    ))

    plot_df <- cs$data

    ggplot(plot_df, aes(x = pitch_group, y = prop, fill = strike_binary)) +
      geom_col(width = 0.75) +
      scale_y_continuous(
        labels = function(x) paste0(round(x * 100), "%"),
        limits = c(0, 1)
      ) +
      scale_fill_manual(
        values = c(
          "Ball"        = "#e23b3b",
          "Strike-like" = "#3fa66a",
          "Non-CSW"     = "#e23b3b",
          "CSW"         = "#3fa66a"
        ),
        name = "Outcome"
      ) +
      labs(
        x        = "Pitch type in random sample",
        y        = "Proportion of pitches",
        title    = "Random Sample: Strike Outcome by Pitch Type",
        subtitle = paste0("Chi-square p-value = ", format_p(cs$test$p.value))
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.background  = element_rect(fill = "#13242f", colour = NA),
        panel.background = element_rect(fill = "#13242f", colour = NA),
        panel.grid       = element_line(colour = "#21404f"),
        text             = element_text(colour = "#f4f1e8"),
        axis.text        = element_text(colour = "#8aa3ad"),
        axis.text.x      = element_text(angle = 35, hjust = 1),
        plot.title       = element_text(face = "bold", colour = "#f2b134"),
        plot.subtitle    = element_text(colour = "#8aa3ad"),
        legend.background = element_rect(fill = "#13242f", colour = NA),
        legend.key        = element_rect(fill = "#13242f", colour = NA)
      )
  })

  output$chisq_sample_note <- renderText({
    cs <- chisq_sample()

    if (is.null(cs)) {
      return("Not enough data in this filtered slice to run the chi-square test.")
    }

    method_text <- if (cs$simulated) {
      "Because at least one expected count was below 5, the app used a simulated chi-square p-value."
    } else {
      "All expected counts were large enough for the standard chi-square approximation."
    }

    sprintf(
      "Random sample size: %s pitches. Chi-square statistic = %.2f, df = %s, p-value = %s. The null hypothesis is that pitch type and strike outcome are independent in this random sample. %s",
      format(cs$sample_size, big.mark = ","),
      unname(cs$test$statistic),
      ifelse(is.null(cs$test$parameter), "simulated", unname(cs$test$parameter)),
      format_p(cs$test$p.value),
      method_text
    )
  })

  # ---- Dynamic takeaway ----
  output$takeaway <- renderText({
    d <- filtered()

    if (is.null(d))    return("Load the CSV to see the summary finding.")
    if (nrow(d) == 0)  return("No pitches match the current filters - loosen or reset.")

    active <- sum(vapply(FILTERS, function(f) length(input[[f$id]]) > 0, logical(1)))

    fl <- if (active == 0) {
      "No filters - full dataset"
    } else {
      sprintf("%d filter%s active", active, if (active == 1) "" else "s")
    }

    warn <- if (nrow(d) < 50) {
      " Small slice - rates here are noisy, read as suggestive."
    } else {
      ""
    }

    sprintf(
      "%s. %s pitches in this slice - CSW%% %.1f (whiff %.1f, called %.1f).%s",
      fl,
      format(nrow(d), big.mark = ","),
      100 * mean(d$is_csw),
      100 * mean(d$is_whiff),
      100 * mean(d$is_called),
      warn
    )
  })
}

shinyApp(ui, server)
