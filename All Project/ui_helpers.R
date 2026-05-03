# ============================================================
# ui_helpers.R
# Contains:
#   - section_header()   reusable section title widget
#   - stat_row()         reusable table row widget
#   - kpi_box()          reusable KPI card widget
#   - render_qual_report()
#   - render_quant_report()
#   - render_full_summary()
# ============================================================

# ---- Reusable UI primitives --------------------------------

section_header <- function(icon_class, title, color = "#00C853") {
  div(
    style = paste0(
      "font-family:'Space Mono',monospace;font-size:11px;text-transform:uppercase;",
      "letter-spacing:2px;color:", color, ";margin-bottom:10px;padding-bottom:6px;",
      "border-bottom:1px solid #1C2A35;"
    ),
    tags$i(class = icon_class, style = "margin-right:7px;"),
    title
  )
}

stat_row <- function(label, value, highlight = FALSE) {
  cls <- if (highlight) " class='hi'" else ""
  tags$tr(
    HTML(cls),            # inject class on <tr> if highlighted
    tags$td(
      style = "color:#78909C;padding:6px 10px;font-size:13px;",
      label
    ),
    tags$td(
      style = paste0(
        if (highlight) "color:#00C853;font-weight:bold;" else "color:#E0E0E0;",
        "padding:6px 10px;font-size:13px;",
        "font-family:'Space Mono',monospace;text-align:right;"
      ),
      as.character(value)
    )
  )
}

kpi_box <- function(color_class, label, value, sub = NULL) {
  div(
    class = paste("kpi-box", color_class),
    div(class = "kpi-label", label),
    div(class = "kpi-value", value),
    if (!is.null(sub)) div(class = "kpi-sub", sub)
  )
}

# ---- Qualitative Smart Report --------------------------------

render_qual_report <- function(tbl) {
  mode_cat <- tbl$Category[1]
  total    <- sum(tbl$Frequency)

  rows_html <- paste(sapply(seq_len(nrow(tbl)), function(i) {
    pct <- tbl$Percentage[i]
    bar <- paste0('<div class="bar-fill" style="width:', min(pct * 2, 100), '%;"></div>')
    sprintf(
      '<tr>
         <td>%s</td>
         <td style="text-align:right;font-family:monospace;">%d</td>
         <td style="text-align:right;font-family:monospace;">%.1f%%</td>
         <td style="width:130px;">%s</td>
       </tr>',
      tbl$Category[i], tbl$Frequency[i], pct, bar
    )
  }), collapse = "")

  div(
    div(class = "kpi-grid",
        kpi_box("purple", "Data Type",           "Qualitative", "Categorical"),
        kpi_box("green",  "Total Observations",  total),
        kpi_box("orange", "Number of Categories",nrow(tbl)),
        kpi_box("cyan",   "Mode (Most Frequent)", mode_cat,
                paste0(tbl$Percentage[1], "% of data"))
    ),

    div(class = "analysis-section",
        h5(style = "color:#7C4DFF;",
           tags$i(class = "fa-solid fa-table", style = "margin-right:7px;"),
           "Frequency Table"),
        div(class = "freq-table-wrap",
            HTML(paste0(
              '<table class="freq-tbl">
                 <thead><tr>
                   <th>Category</th>
                   <th>Frequency</th>
                   <th>Percentage</th>
                   <th>Distribution</th>
                 </tr></thead>
                 <tbody>', rows_html, '</tbody>
               </table>'
            ))
        )
    ),

    div(class = "recommend-box",
        h4(tags$i(class = "fa-solid fa-lightbulb", style = "margin-right:8px;"),
           "Smart Recommendation"),
        div(class = "rec-row",
            div(class = "rec-icon",
                tags$i(class = "fa-solid fa-bullseye", style = "color:#7C4DFF;")),
            div(
              div(class = "rec-label", "Best Measure of Central Tendency"),
              div(class = "rec-val",   "MODE"),
              div(class = "rec-reason",
                  paste0("For qualitative data, the Mode is the only valid measure of ",
                         "central tendency. '", mode_cat, "' is the most frequent category (",
                         tbl$Percentage[1], "%)."))
            )
        ),
        div(class = "rec-row",
            div(class = "rec-icon",
                tags$i(class = "fa-solid fa-chart-pie", style = "color:#7C4DFF;")),
            div(
              div(class = "rec-label", "Best Measure of Dispersion"),
              div(class = "rec-val",   "Frequency / Proportion"),
              div(class = "rec-reason",
                  "Variance and SD do not apply to qualitative data. ",
                  "Use frequencies and percentages to describe the spread of categories.")
            )
        )
    )
  )
}

# ---- Quantitative Smart Report --------------------------------

render_quant_report <- function(s) {

  is_skewed    <- abs(s$skew) > 0.5
  has_outliers <- length(s$outliers) > 0
  use_median   <- is_skewed || has_outliers

  best_center  <- if (use_median) "Median"           else "Mean"
  best_disp    <- if (use_median) "IQR"              else "Standard Deviation"
  cc           <- if (use_median) "#FFA726"          else "#00C853"   # centre colour
  dc           <- if (use_median) "#FFA726"          else "#00C853"   # disp colour

  skew_label <- if      (abs(s$skew) < 0.5) "Symmetric (Normal-like)"
  else if (s$skew > 0)  "Right-skewed (Positive Skew)"
  else                  "Left-skewed (Negative Skew)"

  kurt_label <- if      (abs(s$kurt) < 0.5) "Mesokurtic (Normal)"
  else if (s$kurt > 0)  "Leptokurtic (Heavy Tails / Peaked)"
  else                  "Platykurtic (Light Tails / Flat)"

  normality_label <- if (!is.null(s$sw)) {
    if (s$sw$p.value > 0.05) "Normal" else "Non-Normal"
  } else "N/A"

  var_level <- if (is.na(s$cv))   "N/A"
  else if (s$cv < 15) "Low"
  else if (s$cv < 30) "Moderate"
  else                "High"

  center_reason <- if (use_median) {
    parts <- c()
    if (is_skewed)    parts <- c(parts, paste0("skewed distribution (Skewness = ", round(s$skew, 2), ")"))
    if (has_outliers) parts <- c(parts, paste0(length(s$outliers), " outlier(s) detected"))
    paste0("The data has ", paste(parts, collapse = " and "),
           ". The Median is robust and is not influenced by extreme values.")
  } else {
    "The data is symmetric with no outliers. The Mean utilises all values and is the most efficient estimator."
  }

  disp_reason <- if (use_median)
    "The IQR (Q3 - Q1) is the natural companion to the Median; equally resistant to outliers."
  else
    "The Standard Deviation is the natural companion to the Mean, measuring average spread from the centre."

  div(
    # KPI row
    div(class = "kpi-grid",
        kpi_box("green",  "Best Central Tendency",  tags$span(style = paste0("color:",cc), best_center)),
        kpi_box("orange", "Best Dispersion",         tags$span(style = paste0("color:",dc), best_disp)),
        kpi_box("cyan",   "Variability Level",       var_level,
                if (!is.na(s$cv)) paste0("CV = ", round(s$cv, 1), "%") else "CV = N/A"),
        kpi_box("purple", "Distribution Shape",
                if (abs(s$skew) < .5) "Symmetric" else if (s$skew > 0) "Right Skewed" else "Left Skewed"),
        kpi_box("red",    "Normality",               normality_label)
    ),

    # Central Tendency
    div(class = "analysis-section",
        h5(style = "color:#00C853;",
           tags$i(class = "fa-solid fa-crosshairs", style = "margin-right:7px;"),
           "Measures of Central Tendency"),
        tags$table(class = "stat-table",
                   tags$tr(tags$td("Mean   (Arithmetic Average)"), tags$td(round(s$mean,   4))),
                   tags$tr(tags$td("Median (Middle Value)"),       tags$td(round(s$median, 4))),
                   tags$tr(tags$td("Mode   (Most Frequent Value)"),tags$td(s$mode)),
                   tags$tr(class = "best-row",
                           tags$td(tags$strong(style = paste0("color:", cc),
                                               paste0("Recommended: ", best_center))),
                           tags$td(tags$strong(
                             style = paste0("color:", cc, ";font-size:16px;"),
                             if (best_center == "Mean") round(s$mean, 4) else round(s$median, 4)
                           ))
                   )
        )
    ),

    # Dispersion
    div(class = "analysis-section",
        h5(style = "color:#FFA726;",
           tags$i(class = "fa-solid fa-arrows-left-right", style = "margin-right:7px;"),
           "Measures of Dispersion (Variability)"),
        tags$table(class = "stat-table",
                   tags$tr(tags$td("Variance (s\u00b2)"),             tags$td(round(s$var,   4))),
                   tags$tr(tags$td("Standard Deviation (s)"),         tags$td(round(s$sd,    4))),
                   tags$tr(tags$td("IQR  (Q3 - Q1)"),                 tags$td(round(s$iqr,   4))),
                   tags$tr(tags$td("Range  (Max - Min)"),             tags$td(round(s$range, 4))),
                   tags$tr(tags$td("Coefficient of Variation (CV)"),
                           tags$td(if (!is.na(s$cv)) paste0(round(s$cv, 2), "%") else "N/A")),
                   tags$tr(tags$td("Standard Error (SE)"),            tags$td(round(s$se,    4))),
                   tags$tr(class = "best-row",
                           tags$td(tags$strong(style = paste0("color:", dc),
                                               paste0("Recommended: ", best_disp))),
                           tags$td(tags$strong(
                             style = paste0("color:", dc, ";font-size:16px;"),
                             if (best_disp == "IQR") round(s$iqr, 4) else round(s$sd, 4)
                           ))
                   )
        )
    ),

    # Confidence Interval
    div(class = "analysis-section",
        h5(style = "color:#00E5FF;",
           tags$i(class = "fa-solid fa-gauge-high", style = "margin-right:7px;"),
           "95% Confidence Interval for the Mean"),
        tags$table(class = "stat-table",
                   tags$tr(tags$td("Lower Bound"),   tags$td(round(s$ci[1], 4))),
                   tags$tr(tags$td("Upper Bound"),   tags$td(round(s$ci[2], 4))),
                   tags$tr(tags$td("Interpretation"),
                           tags$td(tags$small(
                             style = "color:#78909C;",
                             paste0("We are 95% confident the true population mean lies between ",
                                    round(s$ci[1], 2), " and ", round(s$ci[2], 2), ".")
                           )))
        )
    ),

    # Shape + Outliers (side by side)
    fluidRow(
      column(6,
             div(class = "analysis-section",
                 h5(style = "color:#00E5FF;",
                    tags$i(class = "fa-solid fa-wave-square", style = "margin-right:7px;"),
                    "Distribution Shape"),
                 tags$table(class = "stat-table",
                            tags$tr(tags$td("Skewness"), tags$td(round(s$skew, 4))),
                            tags$tr(tags$td(""),         tags$td(tags$small(style = "color:#546E7A;", skew_label))),
                            tags$tr(tags$td("Kurtosis"), tags$td(round(s$kurt, 4))),
                            tags$tr(tags$td(""),         tags$td(tags$small(style = "color:#546E7A;", kurt_label)))
                 )
             )
      ),
      column(6,
             div(class = "analysis-section",
                 h5(style = "color:#FF5252;",
                    tags$i(class = "fa-solid fa-triangle-exclamation", style = "margin-right:7px;"),
                    "Outlier Detection (IQR Method)"),
                 tags$table(class = "stat-table",
                            tags$tr(tags$td("Lower Fence"), tags$td(round(s$lower_f, 3))),
                            tags$tr(tags$td("Upper Fence"), tags$td(round(s$upper_f, 3))),
                            tags$tr(tags$td("Outlier Count"),
                                    tags$td(
                                      if (length(s$outliers) == 0)
                                        tags$span(style = "color:#00C853;", "0 - None detected")
                                      else
                                        tags$span(style = "color:#FF5252;",
                                                  paste0(length(s$outliers), " outlier(s) found"))
                                    )
                            ),
                            if (length(s$outliers) > 0)
                              tags$tr(tags$td("Outlier Values"),
                                      tags$td(paste(sort(s$outliers), collapse = ", ")))
                 )
             )
      )
    ),

    # Normality test (only if available)
    if (!is.null(s$sw))
      div(class = "analysis-section",
          h5(style = "color:#CE93D8;",
             tags$i(class = "fa-solid fa-microscope", style = "margin-right:7px;"),
             "Shapiro-Wilk Normality Test"),
          tags$table(class = "stat-table",
                     tags$tr(tags$td("Test Statistic  (W)"), tags$td(round(s$sw$statistic, 4))),
                     tags$tr(tags$td("p-value"),             tags$td(round(s$sw$p.value,   4))),
                     tags$tr(tags$td("Conclusion"),
                             tags$td(
                               if (s$sw$p.value > 0.05)
                                 tags$span(style = "color:#00C853;",
                                           "Normal distribution  (p > 0.05, fail to reject H\u2080)")
                               else
                                 tags$span(style = "color:#FF5252;",
                                           "Non-normal distribution  (p \u2264 0.05, reject H\u2080)")
                             ))
          )
      ),

    # Final recommendation
    div(class = "recommend-box",
        h4(tags$i(class = "fa-solid fa-check-double", style = "margin-right:8px;"),
           "Final Smart Recommendation"),
        div(class = "rec-row",
            div(class = "rec-icon",
                tags$i(class = "fa-solid fa-chart-simple", style = paste0("color:", cc, ";"))),
            div(
              div(class = "rec-label", "Recommended Measure of Central Tendency"),
              div(class = "rec-val",   style = paste0("color:", cc), best_center),
              div(class = "rec-reason", center_reason)
            )
        ),
        div(class = "rec-row",
            div(class = "rec-icon",
                tags$i(class = "fa-solid fa-ruler-combined", style = paste0("color:", dc, ";"))),
            div(
              div(class = "rec-label", "Recommended Measure of Dispersion"),
              div(class = "rec-val",   style = paste0("color:", dc), best_disp),
              div(class = "rec-reason", disp_reason)
            )
        ),
        hr(),
        div(style = "font-size:13px;color:#B0BEC5;line-height:1.8;",
            tags$i(class = "fa-solid fa-circle-info", style = "margin-right:6px;color:#546E7A;"),
            "For this dataset (n\u00a0=\u00a0", tags$strong(s$n), "):  use  ",
            tags$strong(style = paste0("color:", cc), best_center), " = ",
            tags$strong(style = paste0("color:", cc),
                        if (best_center == "Mean") round(s$mean, 4) else round(s$median, 4)),
            "  as the measure of centre, and  ",
            tags$strong(style = paste0("color:", dc), best_disp), " = ",
            tags$strong(style = paste0("color:", dc),
                        if (best_disp == "IQR") round(s$iqr, 4) else round(s$sd, 4)),
            "  as the measure of dispersion."
        )
    )
  )
}

# ---- Full Summary (HTML) --------------------------------

render_full_summary <- function(d, s = NULL) {

  # ---- Qualitative ----
  if (d$type == "Qualitative") {
    tbl   <- d$labels
    total <- sum(tbl$Frequency)

    return(div(
      div(class = "kpi-grid",
          kpi_box("purple", "Data Type",          "Qualitative"),
          kpi_box("green",  "Total Observations", total),
          kpi_box("orange", "Categories",         nrow(tbl)),
          kpi_box("cyan",   "Mode",               tbl$Category[1],
                  paste0(tbl$Percentage[1], "%"))
      ),

      div(class = "summary-block",
          section_header("fa-solid fa-table",
                         "Frequency Distribution Table", "#7C4DFF"),
          div(class = "freq-table-wrap",
              tags$table(class = "freq-tbl",
                         tags$thead(tags$tr(
                           tags$th("Category"),
                           tags$th(style = "text-align:right;", "Frequency"),
                           tags$th(style = "text-align:right;", "Relative Freq."),
                           tags$th(style = "text-align:right;", "Percentage"),
                           tags$th("Bar")
                         )),
                         tags$tbody(lapply(seq_len(nrow(tbl)), function(i) {
                           pct <- tbl$Percentage[i]
                           rel <- round(tbl$Frequency[i] / total, 4)
                           tags$tr(
                             tags$td(tbl$Category[i]),
                             tags$td(style = "text-align:right;font-family:monospace;", tbl$Frequency[i]),
                             tags$td(style = "text-align:right;font-family:monospace;", rel),
                             tags$td(style = "text-align:right;font-family:monospace;", paste0(pct, "%")),
                             tags$td(HTML(paste0(
                               '<div class="bar-fill" style="width:',
                               min(pct * 2, 100), '%;"></div>'
                             )))
                           )
                         }))
              )
          )
      ),

      div(class = "recommend-box",
          h4(tags$i(class = "fa-solid fa-lightbulb", style = "margin-right:8px;"),
             "Recommendation"),
          tags$p(style = "font-size:13px;color:#B0BEC5;margin:0;",
                 tags$strong(style = "color:#FFA726;", "Qualitative data: "),
                 "Use MODE for central tendency and FREQUENCIES / PROPORTIONS for variability. ",
                 "Variance and Standard Deviation are not applicable.")
      )
    ))
  }

  # ---- Quantitative ----
  is_skewed    <- abs(s$skew) > 0.5
  has_outliers <- length(s$outliers) > 0
  use_median   <- is_skewed || has_outliers
  best_center  <- if (use_median) "Median"   else "Mean"
  best_disp    <- if (use_median) "IQR"      else "Standard Deviation"
  cc           <- if (use_median) "#FFA726"  else "#00C853"
  dc           <- if (use_median) "#FFA726"  else "#00C853"

  div(
    # Top KPIs
    div(class = "kpi-grid",
        kpi_box("green",  "Sample Size (n)", s$n),
        kpi_box("orange", "Sum",             round(s$sum,   2)),
        kpi_box("cyan",   "Minimum",         round(s$min,   4)),
        kpi_box("purple", "Maximum",         round(s$max,   4)),
        kpi_box("red",    "Range",           round(s$range, 4))
    ),

    fluidRow(
      column(6,
             div(class = "summary-block",
                 section_header("fa-solid fa-crosshairs",
                                "Measures of Central Tendency", "#00C853"),
                 tags$table(class = "summ-table",
                            stat_row("Mean (Arithmetic Average)",       round(s$mean,   4)),
                            stat_row("Median (Middle Value)",           round(s$median, 4)),
                            stat_row("Mode (Most Frequent)",            s$mode),
                            stat_row("95% CI — Lower Bound",           round(s$ci[1],  4)),
                            stat_row("95% CI — Upper Bound",           round(s$ci[2],  4)),
                            stat_row(paste0("Recommended: ", best_center),
                                     if (best_center == "Mean") round(s$mean, 4)
                                     else round(s$median, 4),
                                     highlight = TRUE)
                 )
             )
      ),
      column(6,
             div(class = "summary-block",
                 section_header("fa-solid fa-arrows-left-right",
                                "Measures of Dispersion", "#FFA726"),
                 tags$table(class = "summ-table",
                            stat_row("Variance (s\u00b2)",              round(s$var,   4)),
                            stat_row("Standard Deviation (s)",          round(s$sd,    4)),
                            stat_row("IQR  (Q3 \u2212 Q1)",            round(s$iqr,   4)),
                            stat_row("Range  (Max \u2212 Min)",         round(s$range, 4)),
                            stat_row("Coefficient of Variation (CV)",
                                     if (!is.na(s$cv)) paste0(round(s$cv, 2), "%") else "N/A"),
                            stat_row("Standard Error (SE)",             round(s$se,    4)),
                            stat_row(paste0("Recommended: ", best_disp),
                                     if (best_disp == "IQR") round(s$iqr, 4)
                                     else round(s$sd, 4),
                                     highlight = TRUE)
                 )
             )
      )
    ),

    fluidRow(
      column(6,
             div(class = "summary-block",
                 section_header("fa-solid fa-percent",
                                "Quartiles & Key Percentiles", "#00E5FF"),
                 tags$table(class = "summ-table",
                            stat_row("Q1  (25th Percentile)",               round(s$q1,         4)),
                            stat_row("Q2  (50th Percentile = Median)",      round(s$median,     4)),
                            stat_row("Q3  (75th Percentile)",               round(s$q3,         4)),
                            stat_row("P5",                                   round(s$pctiles[1], 3)),
                            stat_row("P10",                                  round(s$pctiles[2], 3)),
                            stat_row("P90",                                  round(s$pctiles[6], 3)),
                            stat_row("P95",                                  round(s$pctiles[7], 3))
                 )
             )
      ),
      column(6,
             div(class = "summary-block",
                 section_header("fa-solid fa-list-ol",
                                "Deciles (D1 \u2013 D9)", "#CE93D8"),
                 tags$table(class = "summ-table",
                            lapply(1:9, function(i)
                              stat_row(
                                paste0("D", i, "  (", i * 10, "th Percentile)"),
                                round(s$deciles[i], 4)
                              )
                            )
                 )
             )
      )
    ),

    fluidRow(
      column(6,
             div(class = "summary-block",
                 section_header("fa-solid fa-wave-square",
                                "Distribution Shape", "#00E5FF"),
                 tags$table(class = "summ-table",
                            stat_row("Skewness",
                                     paste0(round(s$skew, 4), "  (",
                                            if (abs(s$skew) < .5) "Symmetric"
                                            else if (s$skew > 0) "Right-skewed"
                                            else "Left-skewed", ")")),
                            stat_row("Kurtosis",
                                     paste0(round(s$kurt, 4), "  (",
                                            if (abs(s$kurt) < .5) "Mesokurtic"
                                            else if (s$kurt > 0) "Leptokurtic"
                                            else "Platykurtic", ")"))
                 )
             )
      ),
      column(6,
             div(class = "summary-block",
                 section_header("fa-solid fa-triangle-exclamation",
                                "Outlier Detection (IQR Method)", "#FF5252"),
                 tags$table(class = "summ-table",
                            stat_row("Lower Fence  (Q1 \u2212 1.5\u00d7IQR)", round(s$lower_f, 3)),
                            stat_row("Upper Fence  (Q3 + 1.5\u00d7IQR)",      round(s$upper_f, 3)),
                            stat_row("Number of Outliers",
                                     if (length(s$outliers) == 0)
                                       "0  (None detected)"
                                     else
                                       paste0(length(s$outliers), " outlier(s)")),
                            if (length(s$outliers) > 0)
                              stat_row("Outlier Values",
                                       paste(sort(s$outliers), collapse = ", "))
                 )
             )
      )
    ),

    # Shapiro-Wilk (only when available)
    if (!is.null(s$sw))
      div(class = "summary-block",
          section_header("fa-solid fa-microscope",
                         "Shapiro-Wilk Normality Test", "#CE93D8"),
          tags$table(class = "summ-table",
                     stat_row("Test Statistic  W",         round(s$sw$statistic, 4)),
                     stat_row("p-value",                   round(s$sw$p.value,   4)),
                     stat_row("Decision (\u03b1 = 0.05)",
                              if (s$sw$p.value > 0.05)
                                "Fail to reject H\u2080 \u2192 Data is normally distributed"
                              else
                                "Reject H\u2080 \u2192 Data is NOT normally distributed")
          )
      ),

    # Recommendation banner
    div(class = "recommend-box",
        h4(tags$i(class = "fa-solid fa-check-double", style = "margin-right:8px;"),
           "Summary Recommendation"),
        div(style = "font-size:13px;color:#B0BEC5;line-height:1.9;",
            div(
              tags$i(class = "fa-solid fa-circle-dot",
                     style = paste0("color:", cc, ";margin-right:8px;")),
              tags$strong(style = paste0("color:", cc), "Central Tendency: "),
              best_center, " = ",
              tags$strong(style = paste0("color:", cc),
                          if (best_center == "Mean") round(s$mean, 4)
                          else round(s$median, 4))
            ),
            div(
              tags$i(class = "fa-solid fa-circle-dot",
                     style = paste0("color:", dc, ";margin-right:8px;")),
              tags$strong(style = paste0("color:", dc), "Dispersion: "),
              best_disp, " = ",
              tags$strong(style = paste0("color:", dc),
                          if (best_disp == "IQR") round(s$iqr, 4)
                          else round(s$sd, 4))
            )
        )
    )
  )
}
