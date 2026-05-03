# ---- Packages ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  shiny, ggplot2, bslib, readr, readxl, haven,
  shinyjs, thematic, e1071
)

thematic_shiny(font = "auto")

# ---- Source helpers ----
source("ui_helpers.R")   # section_header, stat_row, kpi_box,
# render_qual_report, render_quant_report, render_full_summary

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5, bootswatch = "darkly",
                   primary = "#00C853", secondary = "#FFA726"),
  
  # CSS loaded from www/styles.css automatically by Shiny
  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$link(
      rel  = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    )
  ),
  
  # ---- Title Bar ----
  div(class = "title-bar",
      tags$h2(
        tags$i(class = "fa-solid fa-chart-column", style = "margin-right:10px;"),
        "SMART STATISTICAL ANALYSIS SYSTEM"
      ),
      tags$p(
        "Automatic data type detection \u00b7 ",
        "Full statistical analysis \u00b7 ",
        "Intelligent recommendations"
      )
  ),
  
  fluidRow(
    
    # ---- Sidebar ----
    column(3,
           div(class = "sidebar-panel",
               
               div(class = "section-title",
                   tags$i(class = "fa-solid fa-database", style = "margin-right:6px;"),
                   "1  Data Input"),
               
               div(class = "input-card",
                   radioButtons("source", NULL,
                                choices  = c("Enter Numbers" = "Numbers",
                                             "Upload File"   = "File"),
                                selected = "Numbers")
               ),
               
               conditionalPanel("input.source == 'Numbers'",
                                div(class = "input-card",
                                    textAreaInput(
                                      "data",
                                      "Enter values (numbers OR categories)",
                                      value  = "40 10 50 90 20 45 10 30 60 75 25 85",
                                      rows   = 3,
                                      resize = "none"
                                    ),
                                    helpText("Examples: 10,20,30  OR  Male,Female,Male",
                                             style = "color:#37474F;font-size:11px;")
                                )
               ),
               
               conditionalPanel("input.source == 'File'",
                                div(class = "input-card",
                                    fileInput("file", "Upload (.csv / .xlsx / .sav):",
                                              accept = c(".csv", ".xlsx", ".xls", ".sav")),
                                    actionButton("clear_file", "Clear",
                                                 class = "btn-danger btn-sm"),
                                    br(), br(),
                                    uiOutput("col_selector")
                                )
               ),
               
               hr(),
               
               div(class = "section-title",
                   tags$i(class = "fa-solid fa-tag", style = "margin-right:6px;"),
                   "2  Detected Type"),
               uiOutput("type_badge_ui"),
               
               hr(),
               
               div(class = "section-title",
                   tags$i(class = "fa-solid fa-sliders", style = "margin-right:6px;"),
                   "3  Plot Controls"),
               
               conditionalPanel("input.main_tabs == 'Histogram'",
                                sliderInput("bins",     "Bins:",           5,  80, 20, 1)),
               conditionalPanel("input.main_tabs == 'Density Plot'",
                                sliderInput("adjust",   "Smoothness:",     0.2, 3,  1, 0.1)),
               conditionalPanel("input.main_tabs == 'SD Plot'",
                                sliderInput("sd_k",     "SD Multiplier:",  0.5, 3,  1, 0.1)),
               conditionalPanel("input.main_tabs == 'Forecast'",
                                numericInput("future_n","Points Ahead:",   5,   1, 50)),
               
               hr(),
               downloadButton(
                 "dl_plot",
                 tags$span(tags$i(class = "fa-solid fa-download",
                                  style = "margin-right:6px;"),
                           "Save Plot"),
                 class = "btn-success w-100"
               )
           )
    ),
    
    # ---- Main Panel ----
    column(9,
           tabsetPanel(id = "main_tabs",
                       
                       tabPanel(tags$span(tags$i(class = "fa-solid fa-robot",
                                                 style = "margin-right:5px;"), "Smart Report"),
                                br(), uiOutput("smart_report")),
                       
                       tabPanel(tags$span(tags$i(class = "fa-solid fa-list-ul",
                                                 style = "margin-right:5px;"), "Full Summary"),
                                br(), uiOutput("full_summary_ui")),
                       
                       tabPanel(tags$span(tags$i(class = "fa-solid fa-chart-bar",
                                                 style = "margin-right:5px;"), "Histogram"),
                                br(), plotOutput("plt_hist", height = "480px")),
                       
                       tabPanel(tags$span(tags$i(class = "fa-solid fa-box",
                                                 style = "margin-right:5px;"), "Boxplot"),
                                br(), plotOutput("plt_box", height = "480px")),
                       
                       tabPanel(tags$span(tags$i(class = "fa-solid fa-chart-area",
                                                 style = "margin-right:5px;"), "Density Plot"),
                                br(), plotOutput("plt_density", height = "480px")),
                       
                       tabPanel(tags$span(tags$i(class = "fa-solid fa-ruler",
                                                 style = "margin-right:5px;"), "SD Plot"),
                                br(), plotOutput("plt_sd", height = "480px")),
                       
                       tabPanel(tags$span(tags$i(class = "fa-solid fa-chart-line",
                                                 style = "margin-right:5px;"), "Normal Curve"),
                                br(), plotOutput("plt_normal", height = "480px")),
                       
                       tabPanel(tags$span(tags$i(class = "fa-solid fa-chart-scatter",
                                                 style = "margin-right:5px;"), "Q-Q Plot"),
                                br(), plotOutput("plt_qq", height = "480px")),
                       
                       tabPanel(tags$span(tags$i(class = "fa-solid fa-stairs",
                                                 style = "margin-right:5px;"), "ECDF"),
                                br(), plotOutput("plt_ecdf", height = "480px")),
                       
                       tabPanel(tags$span(tags$i(class = "fa-solid fa-link",
                                                 style = "margin-right:5px;"), "Correlation"),
                                br(), plotOutput("plt_rel", height = "420px"),
                                br(), verbatimTextOutput("txt_corr")),
                       
                       tabPanel(tags$span(tags$i(class = "fa-solid fa-wand-magic-sparkles",
                                                 style = "margin-right:5px;"), "Forecast"),
                                br(), plotOutput("plt_reg", height = "480px"))
           )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  observeEvent(input$clear_file, shinyjs::reset("file"))
  current_plot <- reactiveVal(NULL)
  
  # ---- Raw File ----------------------------------------
  raw_df <- reactive({
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    # FIX: every case on its own line — no fall-through
    df <- switch(ext,
                 csv  = readr::read_csv(input$file$datapath, show_col_types = FALSE),
                 xlsx = readxl::read_excel(input$file$datapath),
                 xls  = readxl::read_excel(input$file$datapath),
                 sav  = haven::read_spss(input$file$datapath),
                 stop("Unsupported format. Use .csv, .xlsx, .xls, or .sav"))
    as.data.frame(df)
  })
  
  output$col_selector <- renderUI({
    req(raw_df())
    tagList(
      selectInput("var1", "Primary Variable (X):", choices = names(raw_df())),
      selectInput("var2", "Second Variable Y (optional):",
                  choices = c("None" = "None", names(raw_df())))
    )
  })
  
  # ---- Auto-Detect Data --------------------------------
  auto_data <- reactive({
    if (input$source == "Numbers") {
      tokens <- unlist(strsplit(trimws(input$data), "[[:space:],]+"))
      tokens <- tokens[nzchar(tokens)]
      validate(need(length(tokens) >= 2, "Please enter at least 2 values."))
      
      num_vals <- suppressWarnings(as.numeric(tokens))
      is_num   <- is.finite(num_vals)
      
      # If all entered values are numeric -> Quantitative
      if (all(is_num)) {
        return(list(type = "Quantitative", x = num_vals, y = NULL, labels = NULL))
      }
      
      # Otherwise treat as Qualitative
      vals <- as.character(tokens)
      vals <- vals[nzchar(vals)]
      validate(need(length(vals) >= 1, "No valid categorical data found."))
      
      tbl  <- as.data.frame(table(vals), stringsAsFactors = FALSE)
      names(tbl) <- c("Category", "Frequency")
      tbl  <- tbl[order(-tbl$Frequency), ]
      tbl$Percentage <- round(tbl$Frequency / sum(tbl$Frequency) * 100, 1)
      
      return(list(type = "Qualitative", x = tbl$Frequency, y = NULL, labels = tbl))
    }
    
    req(input$var1)
    df  <- raw_df()
    col <- df[[input$var1]]
    
    if (is.numeric(col)) {
      x <- as.numeric(col)[is.finite(as.numeric(col))]
      y <- NULL
      if (!is.null(input$var2) && input$var2 != "None" && input$var2 %in% names(df))
        y <- as.numeric(df[[input$var2]])
      validate(need(length(x) >= 2, "No valid numeric data found."))
      list(type = "Quantitative", x = x, y = y, labels = NULL)
      
    } else {
      vals <- as.character(col)
      vals <- vals[!is.na(vals) & nzchar(vals)]
      validate(need(length(vals) >= 1, "No valid categorical data found."))
      tbl  <- as.data.frame(table(vals), stringsAsFactors = FALSE)
      names(tbl) <- c("Category", "Frequency")
      tbl  <- tbl[order(-tbl$Frequency), ]
      tbl$Percentage <- round(tbl$Frequency / sum(tbl$Frequency) * 100, 1)
      list(type = "Qualitative", x = tbl$Frequency, y = NULL, labels = tbl)
    }
  })
  
  # ---- Type Badge ----------------------------------------
  output$type_badge_ui <- renderUI({
    d <- auto_data()
    if (d$type == "Quantitative")
      div(class = "type-badge type-quant",
          tags$i(class = "fa-solid fa-hashtag", style = "margin-right:6px;"),
          "QUANTITATIVE")
    else
      div(class = "type-badge type-qual",
          tags$i(class = "fa-solid fa-tag", style = "margin-right:6px;"),
          "QUALITATIVE")
  })
  
  # ---- Statistics (computed once, shared everywhere) ----
  quant_stats <- reactive({
    d <- auto_data()
    req(d$type == "Quantitative")
    x <- d$x
    n <- length(x)
    
    mean_v   <- mean(x)
    median_v <- median(x)
    ft       <- table(x)
    mf       <- max(ft)
    mode_v   <- if (mf == 1) "No unique mode" else
      paste(as.numeric(names(ft[ft == mf])), collapse = ", ")
    
    var_v   <- var(x)
    sd_v    <- sd(x)
    iqr_v   <- IQR(x)
    # Guard: avoid Inf / NaN when mean == 0
    cv_v    <- if (!is.na(mean_v) && mean_v != 0) (sd_v / mean_v) * 100 else NA
    se_v    <- sd_v / sqrt(n)
    range_v <- diff(range(x))
    
    q1      <- quantile(x, .25)
    q3      <- quantile(x, .75)
    deciles <- quantile(x, seq(.1, .9, .1))
    pctiles <- quantile(x, c(.05, .10, .25, .50, .75, .90, .95))
    
    lower_f  <- q1 - 1.5 * iqr_v
    upper_f  <- q3 + 1.5 * iqr_v
    outliers <- x[x < lower_f | x > upper_f]
    
    skew_v <- e1071::skewness(x)
    kurt_v <- e1071::kurtosis(x)
    
    t_crit <- qt(.975, df = n - 1)
    ci     <- c(mean_v - t_crit * se_v, mean_v + t_crit * se_v)
    
    sw <- if (n >= 3 && n <= 5000) shapiro.test(x) else NULL
    
    list(
      n = n, min = min(x), max = max(x), range = range_v, sum = sum(x),
      mean = mean_v, median = median_v, mode = mode_v,
      var = var_v, sd = sd_v, iqr = iqr_v, cv = cv_v, se = se_v,
      q1 = q1, q3 = q3, deciles = deciles, pctiles = pctiles,
      lower_f = lower_f, upper_f = upper_f, outliers = outliers,
      skew = skew_v, kurt = kurt_v, ci = ci, sw = sw
    )
  })
  
  # ---- Dark ggplot2 theme --------------------------------
  dt <- function() {
    ggplot2::theme(
      plot.background   = ggplot2::element_rect(fill = "#080C10", color = NA),
      panel.background  = ggplot2::element_rect(fill = "#080C10", color = NA),
      panel.grid.major  = ggplot2::element_line(color = "#1C2A35", linewidth = .35),
      panel.grid.minor  = ggplot2::element_line(color = "#111820", linewidth = .20),
      plot.title        = ggplot2::element_text(color = "#E0E0E0", size = 13,
                                                face = "bold", family = "mono"),
      plot.subtitle     = ggplot2::element_text(color = "#546E7A", size = 10),
      plot.caption      = ggplot2::element_text(color = "#37474F", size = 9),
      axis.text         = ggplot2::element_text(color = "#78909C"),
      axis.title        = ggplot2::element_text(color = "#90A4AE"),
      axis.ticks        = ggplot2::element_line(color = "#1C2A35"),
      legend.background = ggplot2::element_rect(fill = "#0D1117", color = NA),
      legend.text       = ggplot2::element_text(color = "#90A4AE"),
      legend.title      = ggplot2::element_text(color = "#B0BEC5")
    )
  }
  
  # ============================================================
  # SMART REPORT — delegates to ui_helpers.R
  # ============================================================
  output$smart_report <- renderUI({
    d <- auto_data()
    if (d$type == "Qualitative")
      render_qual_report(d$labels)
    else
      render_quant_report(quant_stats())
  })
  
  # ============================================================
  # FULL SUMMARY — delegates to ui_helpers.R
  # ============================================================
  output$full_summary_ui <- renderUI({
    d <- auto_data()
    s <- if (d$type == "Quantitative") quant_stats() else NULL
    render_full_summary(d, s)
  })
  
  # ============================================================
  # PLOTS
  # ============================================================
  get_x <- reactive({ auto_data()$x })
  
  # -- Histogram --
  output$plt_hist <- renderPlot({
    req(auto_data()$type == "Quantitative")
    x <- get_x(); m <- mean(x); md <- median(x); s <- sd(x)
    p <- ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x)) +
      ggplot2::geom_histogram(bins = input$bins, fill = "#1565C0",
                              color = "#42A5F5", alpha = 0.85) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = m,  color = "Mean"),
                          linewidth = 1.3) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = md, color = "Median"),
                          linewidth = 1.3, linetype = "dashed") +
      ggplot2::scale_color_manual(
        values = c(Mean = "#FF5252", Median = "#69F0AE"), name = NULL) +
      ggplot2::annotate("text", x = m,  y = Inf,
                        label = paste0("Mean = ",   round(m,  2)),
                        color = "#FF5252", vjust = 2,   hjust = -.1, size = 3.5) +
      ggplot2::annotate("text", x = md, y = Inf,
                        label = paste0("Median = ", round(md, 2)),
                        color = "#69F0AE", vjust = 3.8, hjust = -.1, size = 3.5) +
      ggplot2::labs(
        title    = "Histogram",
        subtitle = paste0("n = ", length(x),
                          "   Mean = ", round(m, 2),
                          "   SD = ",   round(s, 2),
                          "   Bins = ", input$bins),
        x = "Value", y = "Frequency",
        caption = paste0("Range: [", round(min(x), 2), " , ", round(max(x), 2), "]")
      ) + dt()
    current_plot(p); p
  })
  
  # -- Boxplot --
  output$plt_box <- renderPlot({
    req(auto_data()$type == "Quantitative")
    x <- get_x()
    q1 <- quantile(x, .25); q3 <- quantile(x, .75); iqr <- IQR(x)
    p <- ggplot2::ggplot(data.frame(x = x),
                         ggplot2::aes(x = factor(1), y = x)) +
      ggplot2::geom_boxplot(fill = "#E65100", color = "#FFB74D",
                            outlier.color = "#FF5252", outlier.size = 3, width = .35) +
      ggplot2::geom_jitter(width = .1, alpha = .45, color = "#90A4AE", size = 2) +
      ggplot2::stat_summary(fun = mean, geom = "point",
                            color = "#00E5ff", size = 5, shape = 18) +
      ggplot2::annotate("text", x = 1.32, y = q1,
                        label = paste0("Q1 = ", round(q1, 2)),
                        color = "#FFA726", size = 3.3) +
      ggplot2::annotate("text", x = 1.32, y = median(x),
                        label = paste0("Md = ", round(median(x), 2)),
                        color = "#69F0AE", size = 3.3) +
      ggplot2::annotate("text", x = 1.32, y = q3,
                        label = paste0("Q3 = ", round(q3, 2)),
                        color = "#FFA726", size = 3.3) +
      ggplot2::labs(
        title    = "Boxplot",
        subtitle = paste0("IQR = ", round(iqr, 2),
                          "   Outliers: ",
                          sum(x < q1 - 1.5 * iqr | x > q3 + 1.5 * iqr),
                          "   Diamond = Mean"),
        x = "", y = "Value"
      ) +
      dt() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank())
    current_plot(p); p
  })
  
  # -- Density Plot --
  output$plt_density <- renderPlot({
    req(auto_data()$type == "Quantitative")
    x <- get_x(); m <- mean(x); md <- median(x)
    p <- ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x)) +
      ggplot2::geom_density(adjust = input$adjust, fill = "#4A148C",
                            color = "#CE93D8", alpha = .6, linewidth = 1) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = m,  color = "Mean"),
                          linewidth = 1.1) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = md, color = "Median"),
                          linewidth = 1.1, linetype = "dashed") +
      ggplot2::scale_color_manual(
        values = c(Mean = "#FF5252", Median = "#69F0AE"), name = NULL) +
      ggplot2::labs(
        title    = "Kernel Density Plot",
        subtitle = paste0("Bandwidth Adjust = ", input$adjust,
                          "   Mean = ", round(m, 2),
                          "   Median = ", round(md, 2)),
        x = "Value", y = "Density",
        caption = paste0("Skewness = ", round(e1071::skewness(x), 3))
      ) + dt()
    current_plot(p); p
  })
  
  # -- SD Plot --
  output$plt_sd <- renderPlot({
    req(auto_data()$type == "Quantitative")
    x <- get_x(); m <- mean(x); s <- sd(x); k <- input$sd_k
    pct <- round(sum(x >= m - k * s & x <= m + k * s) / length(x) * 100, 1)
    p <- ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x)) +
      ggplot2::geom_histogram(bins = 25, fill = "#263238",
                              color = "#455A64", alpha = .9) +
      ggplot2::annotate("rect", xmin = m - k * s, xmax = m + k * s,
                        ymin = 0, ymax = Inf, fill = "#00C853", alpha = .07) +
      ggplot2::geom_vline(xintercept = m,         color = "#FFD740", linewidth = 1.4) +
      ggplot2::geom_vline(xintercept = m + k * s, color = "#FF5252",
                          linewidth = 1, linetype = "dashed") +
      ggplot2::geom_vline(xintercept = m - k * s, color = "#FF5252",
                          linewidth = 1, linetype = "dashed") +
      ggplot2::annotate("text", x = m, y = Inf,
                        label = paste0("Mean = ", round(m, 2)),
                        color = "#FFD740", vjust = 2, size = 3.5) +
      ggplot2::annotate("text", x = m + k * s, y = Inf,
                        label = paste0("+", k, "\u03c3 = ", round(m + k * s, 2)),
                        color = "#FF5252", vjust = 2, hjust = -.1, size = 3) +
      ggplot2::annotate("text", x = m - k * s, y = Inf,
                        label = paste0("-", k, "\u03c3 = ", round(m - k * s, 2)),
                        color = "#FF5252", vjust = 2, hjust = 1.1, size = 3) +
      ggplot2::labs(
        title    = "Standard Deviation Plot",
        subtitle = paste0("Mean \u00b1 ", k, " SD   |   ",
                          pct, "% of data within the shaded region"),
        x = "Value", y = "Count"
      ) + dt()
    current_plot(p); p
  })
  
  # -- Normal Curve --
  output$plt_normal <- renderPlot({
    req(auto_data()$type == "Quantitative")
    x <- get_x()
    validate(need(sd(x) > 0, "Zero variance - cannot draw the normal curve."))
    mu <- mean(x); sg <- sd(x)
    xs  <- seq(mu - 4 * sg, mu + 4 * sg, length.out = 300)
    ys  <- dnorm(xs, mu, sg)
    dfn <- data.frame(x = xs, y = ys)
    p <- ggplot2::ggplot(dfn, ggplot2::aes(x, y)) +
      ggplot2::geom_ribbon(
        data = subset(dfn, x >= mu - sg   & x <= mu + sg),
        ggplot2::aes(ymin = 0, ymax = y), fill = "#00C853", alpha = .25) +
      ggplot2::geom_ribbon(
        data = subset(dfn, x >= mu - 2*sg & x <= mu + 2*sg),
        ggplot2::aes(ymin = 0, ymax = y), fill = "#00C853", alpha = .12) +
      ggplot2::geom_ribbon(
        data = subset(dfn, x >= mu - 3*sg & x <= mu + 3*sg),
        ggplot2::aes(ymin = 0, ymax = y), fill = "#00C853", alpha = .06) +
      ggplot2::geom_line(color = "#00E5FF", linewidth = 1.6) +
      ggplot2::geom_vline(xintercept = mu, color = "#FFD740", linewidth = 1) +
      ggplot2::annotate("text", x = mu,        y = max(ys) * 1.04,
                        label = paste0("\u03bc = ", round(mu, 2)),
                        color = "#FFD740", size = 4.5) +
      ggplot2::annotate("text", x = mu + sg,   y = max(ys) * .62,
                        label = "+1\u03c3", color = "#69F0AE", size = 3.5, hjust = 0) +
      ggplot2::annotate("text", x = mu - sg,   y = max(ys) * .62,
                        label = "-1\u03c3", color = "#69F0AE", size = 3.5, hjust = 1) +
      ggplot2::annotate("text", x = mu + 2*sg, y = max(ys) * .22,
                        label = "+2\u03c3", color = "#FFA726", size = 3.5, hjust = 0) +
      ggplot2::annotate("text", x = mu - 2*sg, y = max(ys) * .22,
                        label = "-2\u03c3", color = "#FFA726", size = 3.5, hjust = 1) +
      ggplot2::labs(
        title    = "Normal Distribution Curve",
        subtitle = paste0("\u03bc = ", round(mu, 2),
                          "   \u03c3 = ", round(sg, 2),
                          "   68% within \u00b11\u03c3",
                          "   95% within \u00b12\u03c3",
                          "   99.7% within \u00b13\u03c3"),
        x = "Value", y = "Density"
      ) + dt()
    current_plot(p); p
  })
  
  # -- Q-Q Plot --
  output$plt_qq <- renderPlot({
    req(auto_data()$type == "Quantitative")
    x    <- get_x()
    sw_p <- if (length(x) >= 3 && length(x) <= 5000) shapiro.test(x)$p.value else NA
    note <- if (!is.na(sw_p))
      paste0("Shapiro-Wilk  p = ", round(sw_p, 4), "   \u2192   ",
             if (sw_p > .05) "Normal distribution" else "Non-normal distribution")
    else "Shapiro-Wilk: N/A (n out of range)"
    p <- ggplot2::ggplot(data.frame(s = x), ggplot2::aes(sample = s)) +
      ggplot2::stat_qq(color = "#00E5FF", size = 2.5, alpha = .8) +
      ggplot2::stat_qq_line(color = "#FF5252", linewidth = 1.2) +
      ggplot2::labs(
        title    = "Q-Q Plot (Quantile-Quantile)",
        subtitle = paste0("Points close to the red line indicate normality   |   ", note),
        x = "Theoretical Quantiles", y = "Sample Quantiles"
      ) + dt()
    current_plot(p); p
  })
  
  # -- ECDF --
  output$plt_ecdf <- renderPlot({
    req(auto_data()$type == "Quantitative")
    x <- get_x()
    p <- ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x)) +
      ggplot2::stat_ecdf(color = "#FF7043", linewidth = 1.4) +
      ggplot2::geom_hline(yintercept = c(.25, .50, .75),
                          linetype = "dashed", color = "#37474F") +
      ggplot2::annotate("text", x = min(x), y = .27,
                        label = "25th Pct.", color = "#FFA726", size = 3.3) +
      ggplot2::annotate("text", x = min(x), y = .52,
                        label = "50th Pct.", color = "#69F0AE", size = 3.3) +
      ggplot2::annotate("text", x = min(x), y = .77,
                        label = "75th Pct.", color = "#FFA726", size = 3.3) +
      ggplot2::labs(
        title    = "Empirical Cumulative Distribution Function (ECDF)",
        subtitle = paste0("Cumulative proportion   n = ", length(x)),
        x = "Value", y = "Cumulative Proportion F(x)"
      ) + dt()
    current_plot(p); p
  })
  
  # -- Correlation --
  output$plt_rel <- renderPlot({
    d <- auto_data()
    req(d$type == "Quantitative", !is.null(d$y))
    validate(need(!all(is.na(d$y)), "No valid Y data found."))
    df2 <- na.omit(data.frame(X = d$x, Y = d$y))
    validate(need(nrow(df2) >= 3, "At least 3 paired observations are required."))
    r  <- cor(df2$X, df2$Y); r2 <- r^2
    cl <- if (abs(r) > .7) "Strong" else if (abs(r) > .4) "Moderate" else "Weak"
    cd <- if (r > 0) "Positive" else "Negative"
    p <- ggplot2::ggplot(df2, ggplot2::aes(X, Y)) +
      ggplot2::geom_point(color = "#00E5FF", size = 3, alpha = .75) +
      ggplot2::geom_smooth(method = "lm", color = "#FF5252", fill = "#FF5252",
                           alpha = .12, linewidth = 1.2) +
      ggplot2::annotate("text",
                        x = min(df2$X) + .05 * diff(range(df2$X)),
                        y = max(df2$Y) - .05 * diff(range(df2$Y)),
                        label = paste0("r  = ", round(r, 3),
                                       "\nR\u00b2 = ", round(r2, 3)),
                        color = "#FFD740", size = 4, hjust = 0) +
      ggplot2::labs(
        title    = "Scatter Plot with Regression Line",
        subtitle = paste0(cl, " ", cd, " linear relationship   r = ",
                          round(r, 3), "   R\u00b2 = ", round(r2, 3)),
        x = "X Variable", y = "Y Variable"
      ) + dt()
    current_plot(p); p
  })
  
  output$txt_corr <- renderPrint({
    d <- auto_data()
    validate(need(d$type == "Quantitative",
                  "Correlation analysis requires quantitative data."))
    validate(need(!is.null(d$y),
                  "Please select a Y variable to compute correlation."))
    df2 <- na.omit(data.frame(X = d$x, Y = d$y))
    validate(need(nrow(df2) >= 3, "At least 3 paired observations are required."))
    r  <- cor(df2$X, df2$Y); r2 <- r^2
    n  <- nrow(df2)
    ts <- r * sqrt(n - 2) / sqrt(1 - r^2)
    pv <- 2 * pt(-abs(ts), df = n - 2)
    cat("==========================================\n")
    cat("     PEARSON CORRELATION ANALYSIS         \n")
    cat("==========================================\n\n")
    cat(sprintf("  Pearson r            : %.4f\n",   r))
    cat(sprintf("  R-squared  (R2)      : %.4f\n",   r2))
    cat(sprintf("  t-statistic          : %.4f\n",   ts))
    cat(sprintf("  Degrees of Freedom   : %d\n",     n - 2))
    cat(sprintf("  p-value              : %.4g\n",   pv))
    cat(sprintf("  Significance         : %s\n",
                if      (pv < .001) "*** p < 0.001  (Highly significant)"
                else if (pv < .01)  "**  p < 0.01   (Very significant)"
                else if (pv < .05)  "*   p < 0.05   (Significant)"
                else                "ns  p >= 0.05  (Not significant)"))
    cat(sprintf("  Strength             : %s %s correlation\n",
                if (abs(r) > .7) "Strong" else if (abs(r) > .4) "Moderate" else "Weak",
                if (r > 0) "positive" else "negative"))
    cat("\n------------------------------------------\n")
    cat(sprintf("  R2 = %.1f%% of variance in Y is explained by X.\n", r2 * 100))
    cat("==========================================\n")
  })
  
  # -- Regression / Forecast --
  output$plt_reg <- renderPlot({
    req(auto_data()$type == "Quantitative")
    d  <- auto_data()
    df <- if (!is.null(d$y)) {
      tmp <- na.omit(data.frame(X = d$x, Y = d$y))
      tmp[order(tmp$X), ]
    } else {
      data.frame(X = seq_along(d$x), Y = d$x)
    }
    validate(need(nrow(df) >= 2, "Not enough data points for regression."))
    
    mod   <- lm(Y ~ X, data = df)
    b0    <- coef(mod)[1]; b1 <- coef(mod)[2]
    r2    <- summary(mod)$r.squared
    
    fut   <- data.frame(X = seq(max(df$X) + 1, by = 1,
                                length.out = input$future_n))
    fut$Y <- predict(mod, fut)
    brg   <- data.frame(X = c(tail(df$X, 1), fut$X[1]),
                        Y = c(tail(df$Y, 1), fut$Y[1]))
    
    ym <- min(c(df$Y, fut$Y)) - .1 * diff(range(c(df$Y, fut$Y)))
    yr <- diff(range(c(df$Y, fut$Y)))
    if (yr == 0) yr <- 1   # guard zero range
    
    eq <- paste0("Y = ", round(b0, 2),
                 if (b1 >= 0) " + " else " - ",
                 abs(round(b1, 2)), "X     R\u00b2 = ", round(r2, 3))
    
    p <- ggplot2::ggplot() +
      ggplot2::geom_ribbon(data = df,
                           ggplot2::aes(x = X, ymin = ym, ymax = Y),
                           fill = "#00C853", alpha = .10) +
      ggplot2::geom_line(data  = df,
                         ggplot2::aes(x = X, y = Y),
                         color = "#00C853", linewidth = 1.6) +
      ggplot2::geom_point(data = df,
                          ggplot2::aes(x = X, y = Y),
                          color = "#00E676", size = 3, alpha = .85) +
      ggplot2::geom_line(data  = brg,
                         ggplot2::aes(x = X, y = Y),
                         color = "#FFA726", linewidth = 1, linetype = "dashed") +
      ggplot2::geom_ribbon(data = fut,
                           ggplot2::aes(x = X, ymin = ym, ymax = Y),
                           fill = "#FFA726", alpha = .08) +
      ggplot2::geom_line(data  = fut,
                         ggplot2::aes(x = X, y = Y),
                         color = "#FFA726", linewidth = 1.6, linetype = "dashed") +
      ggplot2::geom_point(data = fut,
                          ggplot2::aes(x = X, y = Y),
                          color = "#FFA726", size = 4, shape = 25, fill = "#FFA726") +
      ggplot2::geom_segment(
        data = fut,
        ggplot2::aes(x = X, xend = X, y = Y + yr * .03, yend = Y + yr * .10),
        arrow = grid::arrow(length = grid::unit(.2, "cm"), type = "closed"),
        color = "#FFA726", linewidth = .8) +
      ggplot2::geom_label(
        data = tail(df,  1),
        ggplot2::aes(x = X, y = Y,
                     label = paste0(" Last: ", round(Y, 2))),
        color = "#00E676", fill = "#0D1117", size = 3.5, fontface = "bold",
        label.padding = grid::unit(.2, "lines")) +
      ggplot2::geom_label(
        data = tail(fut, 1),
        ggplot2::aes(x = X, y = Y,
                     label = paste0(" Forecast: ", round(Y, 2))),
        color = "#FFA726", fill = "#0D1117", size = 3.5, fontface = "bold",
        label.padding = grid::unit(.2, "lines")) +
      ggplot2::geom_hline(yintercept = mean(df$Y),
                          color = "#37474F", linewidth = .5, linetype = "dotted") +
      ggplot2::annotate("text", x = min(df$X), y = mean(df$Y),
                        label = paste0("Mean = ", round(mean(df$Y), 2)),
                        color = "#37474F", size = 2.8, vjust = -.5, hjust = 0) +
      ggplot2::labs(
        title    = "Linear Regression & Forecast",
        subtitle = paste0("Actual (green)   Forecast (orange, next ",
                          input$future_n, " pts)   |   ", eq),
        x = "X", y = "Value",
        caption  = "Shaded = trend region   |   Triangles = forecast points"
      ) + dt()
    current_plot(p); p
  })
  
  # ---- Download ------------------------------------------
  output$dl_plot <- downloadHandler(
    filename = function() paste0("stat_plot_", Sys.Date(), ".png"),
    content  = function(file) {
      p <- current_plot()
      validate(need(!is.null(p), "Please open a plot tab first."))
      ggplot2::ggsave(file, plot = p, width = 11, height = 6.5,
                      dpi = 300, bg = "#080C10")
    }
  )
}

# ============================================================
# RUN
# ============================================================
shinyApp(ui, server)