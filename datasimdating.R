library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(DT)
library(reshape2)
library(emmeans)
library(zip)

# CSS für Dark Mode
dark_css <- "
<style>
.content-wrapper, .right-side { background-color: #1a1a1a; color: #ffffff; }
.box { background-color: #2d2d2d; border-top-color: #605ca8; }
.box-header { background-color: #3c3c3c; color: #ffffff; }
.box-body { background-color: #2d2d2d; color: #ffffff; }
.form-control { background-color: #3c3c3c; color: #ffffff; border-color: #555; }
.selectize-input { background-color: #3c3c3c !important; color: #fff !important; }
.selectize-dropdown { background-color: #2d2d2d !important; color: #fff !important; }
.sidebar { background-color: #1a1a1a; }

.dataTables_wrapper { color: #fff !important; }
table.dataTable { color: #fff !important; background-color: #2d2d2d !important; }
table.dataTable thead th { color: #fff !important; background-color: #3c3c3c !important; border-color: #555 !important; }
table.dataTable tbody td { color: #fff !important; border-color: #444 !important; }
.dataTables_length, .dataTables_filter, .dataTables_info, .dataTables_paginate { color: #fff !important; }
.paginate_button { color: #fff !important; }
.paginate_button.current { color: #000 !important; }

.slider-label { color: #ce9ffc; font-weight: bold; font-size: 12px; margin-top: 10px; }
.help-text { color: #aaa; font-size: 11px; margin-bottom: 10px; font-style: italic; }
.section-header { color: #888; font-size: 11px; font-weight: bold; margin: 8px 0 4px 0; text-transform: uppercase; letter-spacing: 0.5px; }
.interpretation-box { background-color: #252525; border-left: 3px solid #605ca8; padding: 10px 15px; margin-top: 10px; font-size: 13px; }
.hyp-relevant { color: #69ff69; }
.hyp-note { color: #aaa; font-style: italic; }

.export-btn { 
background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important; 
border: none !important;
margin-top: 15px;
}

</style>

"



# ============================================================================
# DATENSIMULATION
# ============================================================================

simulate_study_data <- function(n_per_group = 100, scenario = "expected", custom_params = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  if (scenario == "custom" && !is.null(custom_params)) {
    base_trust_realistic <- custom_params$base_trust_realistic
    base_cheater_realistic <- custom_params$base_cheater_realistic
    base_trust_inverted <- custom_params$base_trust_inverted
    base_cheater_inverted <- custom_params$base_cheater_inverted
    lzo_slope_realistic <- custom_params$lzo_slope_realistic
    lzo_slope_inverted <- custom_params$lzo_slope_inverted
  } else if (scenario == "expected") {
    base_trust_realistic <- 0.65; base_cheater_realistic <- 0.78
    base_trust_inverted <- 0.72; base_cheater_inverted <- 0.58
    lzo_slope_realistic <- 0.08; lzo_slope_inverted <- -0.06
  } else if (scenario == "null") {
    base_trust_realistic <- 0.70; base_cheater_realistic <- 0.70
    base_trust_inverted <- 0.70; base_cheater_inverted <- 0.70
    lzo_slope_realistic <- 0; lzo_slope_inverted <- 0
  } else if (scenario == "partial_h1_only") {
    base_trust_realistic <- 0.65; base_cheater_realistic <- 0.78
    base_trust_inverted <- 0.60; base_cheater_inverted <- 0.72
    lzo_slope_realistic <- 0.05; lzo_slope_inverted <- 0.02
  } else if (scenario == "strong_moderation") {
    base_trust_realistic <- 0.65; base_cheater_realistic <- 0.75
    base_trust_inverted <- 0.70; base_cheater_inverted <- 0.60
    lzo_slope_realistic <- 0.15; lzo_slope_inverted <- -0.12
  } else if (scenario == "ceiling_effect") {
    base_trust_realistic <- 0.85; base_cheater_realistic <- 0.92
    base_trust_inverted <- 0.88; base_cheater_inverted <- 0.75
    lzo_slope_realistic <- 0.04; lzo_slope_inverted <- -0.05
  }
  
  n_total <- n_per_group * 2
  ecology <- rep(c("realistisch", "invertiert"), each = n_per_group)
  subject_id <- 1:n_total
  lzo <- rnorm(n_total, mean = 0, sd = 1)
  kzo <- scale(rnorm(n_total, mean = 0, sd = 1) + 0.2 * lzo)[,1]
  
  data <- data.frame(subject_id = subject_id, ecology = ecology, lzo = lzo, kzo = kzo)
  
  data$csim_trustworthy <- ifelse(data$ecology == "realistisch",
                                  base_trust_realistic + 0.03 * data$lzo + rnorm(n_total, 0, 0.08),
                                  base_trust_inverted + lzo_slope_inverted * data$lzo + rnorm(n_total, 0, 0.08))
  
  data$csim_cheater <- ifelse(data$ecology == "realistisch",
                              base_cheater_realistic + lzo_slope_realistic * data$lzo + rnorm(n_total, 0, 0.08),
                              base_cheater_inverted + 0.02 * data$lzo + rnorm(n_total, 0, 0.08))
  
  data$csim_trustworthy <- pmin(pmax(data$csim_trustworthy, 0.3), 1.0)
  data$csim_cheater <- pmin(pmax(data$csim_cheater, 0.3), 1.0)
  
  data_long <- data %>%
    pivot_longer(cols = c(csim_trustworthy, csim_cheater), names_to = "valence", values_to = "csim") %>%
    mutate(valence = ifelse(valence == "csim_trustworthy", "vertrauenswürdig", "betrügerisch"),
           valence = factor(valence, levels = c("vertrauenswürdig", "betrügerisch")),
           ecology = factor(ecology, levels = c("realistisch", "invertiert")))
  
  params <- list(
    base_trust_realistic = base_trust_realistic, base_cheater_realistic = base_cheater_realistic,
    base_trust_inverted = base_trust_inverted, base_cheater_inverted = base_cheater_inverted,
    lzo_slope_realistic = lzo_slope_realistic, lzo_slope_inverted = lzo_slope_inverted
  )
  
  return(list(wide = data, long = data_long, params = params))
}

# ============================================================================
# PLOT FUNKTIONEN
# ============================================================================

dark_theme <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#2d2d2d", color = NA),
      panel.background = element_rect(fill = "#2d2d2d"),
      text = element_text(color = "white"),
      axis.text = element_text(color = "white"),
      legend.background = element_rect(fill = "#2d2d2d"),
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "white"),
      strip.text = element_text(color = "white", face = "bold", size = 12),
      panel.grid.major = element_line(color = "#444444"),
      panel.grid.minor = element_line(color = "#333333")
    )
}

# Balkendiagramm
plot_interaction_bar <- function(data_long) {
  sum_data <- data_long %>% 
    group_by(ecology, valence) %>% 
    summarise(m = mean(csim), se = sd(csim)/sqrt(n()), .groups = "drop")
  
  ggplot(sum_data, aes(x = ecology, y = m, fill = valence)) +
    geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
    geom_errorbar(aes(ymin = m - se, ymax = m + se), position = position_dodge(0.8), width = 0.25) +
    scale_fill_manual(values = c("vertrauenswürdig" = "#40E0D0", "betrügerisch" = "#DC143C")) +
    dark_theme() +
    theme(legend.position = "bottom") +
    labs(y = "CSIM", x = "Informationsökologie", fill = "Valenz",
         title = "2x2 Interaktion: Ökologie x Valenz")
}

# Liniendiagramm
plot_interaction_line <- function(data_long) {
  sum_data <- data_long %>% 
    group_by(ecology, valence) %>% 
    summarise(m = mean(csim), se = sd(csim)/sqrt(n()), .groups = "drop")
  
  ggplot(sum_data, aes(x = ecology, y = m, color = valence, group = valence)) +
    geom_line(size = 1.5) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = m - se, ymax = m + se), width = 0.1, size = 1) +
    scale_color_manual(values = c("vertrauenswürdig" = "#40E0D0", "betrügerisch" = "#DC143C")) +
    dark_theme() +
    theme(legend.position = "bottom") +
    labs(y = "CSIM", x = "Informationsökologie", color = "Valenz",
         title = "Interaktionseffekt (Liniendiagramm)")
}

# Simple Slopes gruppiert
plot_simple_slopes_grouped <- function(data_wide) {
  plot_data <- data_wide %>%
    mutate(lzo_group = ifelse(lzo > median(lzo), "LZO hoch", "LZO niedrig")) %>%
    pivot_longer(c(csim_trustworthy, csim_cheater), names_to = "valence", values_to = "csim") %>%
    mutate(valence = ifelse(valence == "csim_trustworthy", "Trustworthy", "Cheater"),
           valence = factor(valence, levels = c("Trustworthy", "Cheater")))
  
  agg_data <- plot_data %>% 
    group_by(ecology, lzo_group, valence) %>% 
    summarise(mean_csim = mean(csim), .groups = "drop")
  
  ggplot(agg_data, aes(x = valence, y = mean_csim)) +
    geom_line(aes(group = paste(ecology, lzo_group), linetype = lzo_group), 
              color = "white", size = 1.2, alpha = 0.8) +
    geom_point(aes(color = valence, shape = lzo_group), size = 4) +
    facet_wrap(~ecology) +
    scale_color_manual(values = c("Trustworthy" = "#40E0D0", "Cheater" = "#DC143C")) +
    scale_linetype_manual(values = c("LZO hoch" = "solid", "LZO niedrig" = "dashed")) +
    scale_shape_manual(values = c("LZO hoch" = 16, "LZO niedrig" = 17)) +
    dark_theme() +
    theme(legend.position = "bottom") +
    labs(title = "Simple Slopes: LZO-Moderation", 
         y = "CSIM", color = "Valenz", linetype = "LZO", shape = "LZO")
}

# Scatter mit Regression
plot_simple_slopes_scatter <- function(data_long) {
  ggplot(data_long, aes(x = lzo, y = csim, color = valence)) +
    geom_point(alpha = 0.4, size = 1.5) +
    geom_smooth(method = "lm", se = TRUE, size = 1.2) +
    facet_wrap(~ecology) +
    scale_color_manual(values = c("vertrauenswürdig" = "#40E0D0", "betrügerisch" = "#DC143C")) +
    dark_theme() +
    theme(legend.position = "bottom") +
    labs(title = "LZO x Valenz: Regressionslinien",
         x = "Langzeitorientierung (z)", y = "CSIM", color = "Valenz")
}

# Differenzen-Plot
plot_difference_by_lzo <- function(data_wide) {
  data_wide <- data_wide %>%
    mutate(diff_score = csim_cheater - csim_trustworthy)
  
  ggplot(data_wide, aes(x = lzo, y = diff_score, color = ecology)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "yellow", size = 1) +
    geom_point(alpha = 0.5, size = 2) +
    geom_smooth(method = "lm", se = TRUE, size = 1.3) +
    scale_color_manual(values = c("realistisch" = "#4169E1", "invertiert" = "#FF8C00")) +
    dark_theme() +
    theme(legend.position = "bottom") +
    labs(title = "Cheater-Vorteil als Funktion von LZO",
         x = "Langzeitorientierung (z)", 
         y = "Differenz: CSIM(Cheater) - CSIM(Trust)",
         color = "Ökologie")
}

# Raincloud
plot_raincloud <- function(data_long) {
  set.seed(123)
  ggplot(data_long, aes(x = valence, y = csim, fill = valence)) +
    geom_violin(alpha = 0.4, trim = FALSE, width = 0.8) +
    geom_boxplot(width = 0.2, fill = "white", alpha = 0.7, outlier.shape = NA) +
    geom_jitter(aes(color = valence), width = 0.2, alpha = 0.4, size = 1.5) +
    facet_wrap(~ecology) +
    scale_fill_manual(values = c("vertrauenswürdig" = "#40E0D0", "betrügerisch" = "#DC143C")) +
    scale_color_manual(values = c("vertrauenswürdig" = "#40E0D0", "betrügerisch" = "#DC143C")) +
    dark_theme() +
    theme(legend.position = "none") +
    labs(title = "Verteilung der CSIM-Werte", y = "CSIM", x = "Valenz")
}

# Korrelationsmatrix
plot_correlation_matrix <- function(data_wide) {
  vars <- data_wide %>% select(csim_trustworthy, csim_cheater, lzo, kzo)
  colnames(vars) <- c("Trust", "Cheater", "LZO", "KZO")
  M <- cor(vars)
  meltM <- melt(M)
  
  ggplot(meltM, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white", alpha = 0.8) +
    geom_text(aes(label = round(value, 2)), color = "black", size = 6, fontface = "bold") +
    scale_fill_gradient2(low = "#DC143C", high = "#40E0D0", mid = "white", midpoint = 0, limit = c(-1, 1)) +
    dark_theme() +
    theme(axis.title = element_blank()) +
    labs(title = "Korrelationsmatrix") +
    coord_fixed()
}

# ============================================================================
# UI
# ============================================================================

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Cheater-Memory Simulation"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Übersicht", tabName = "overview", icon = icon("info-circle")),
      menuItem("Daten", tabName = "simulation", icon = icon("database")),
      menuItem("2x2 Interaktion", tabName = "interaction", icon = icon("chart-bar")),
      menuItem("Simple Slopes", tabName = "simple_slopes", icon = icon("chart-line")),
      menuItem("Deskriptiv", tabName = "descriptive", icon = icon("chart-area")),
      menuItem("Statistik", tabName = "statistics", icon = icon("table")),
      menuItem("LMM & Kontraste", tabName = "lmm_results", icon = icon("calculator")),
      menuItem("Export", tabName = "export", icon = icon("download"))
       ),
    
    tags$head(HTML(dark_css)),
    
    div(style = "padding: 15px;",
        selectInput("scenario", "Szenario:", 
                    choices = c("Erwartet" = "expected", "Null" = "null", "Nur H1" = "partial_h1_only", 
                                "Starke Moderation" = "strong_moderation", "Deckeneffekt" = "ceiling_effect", "Custom" = "custom")),
        
        conditionalPanel("input.scenario == 'custom'",
                         div(style = "background:#3c3c3c;padding:10px;border-radius:5px;margin-bottom:10px;",
                             
                             # Realistische Ökologie
                             div(class = "section-header", "Realistische Ökologie (70% Trust, 30% Cheater)"),
                             
                             div(class = "slider-label", "Trustworthy (häufig)"),
                             div(class = "help-text", "Baseline CSIM für häufige Kategorie (~0.65)"),
                             sliderInput("base_trust_realistic", NULL, min = 0.3, max = 1, value = 0.65, step = 0.01),
                             
                             div(class = "slider-label", "Cheater (selten)"),
                             div(class = "help-text", "Baseline CSIM für seltene Kategorie. Höher = stärkerer H1-Effekt"),
                             sliderInput("base_cheater_realistic", NULL, min = 0.3, max = 1, value = 0.78, step = 0.01),
                             
                             hr(style = "border-color:#555;margin:10px 0;"),
                             
                             # Invertierte Ökologie
                             div(class = "section-header", "Invertierte Ökologie (30% Trust, 70% Cheater)"),
                             
                             div(class = "slider-label", "Trustworthy (selten)"),
                             div(class = "help-text", "Hier selten. Höher als Cheater(Inv) = H2 Reversal"),
                             sliderInput("base_trust_inverted", NULL, min = 0.3, max = 1, value = 0.72, step = 0.01),
                             
                             div(class = "slider-label", "Cheater (häufig)"),
                             div(class = "help-text", "Hier häufig. Niedriger als Trust(Inv) = H2 bestätigt"),
                             sliderInput("base_cheater_inverted", NULL, min = 0.3, max = 1, value = 0.58, step = 0.01),
                             
                             hr(style = "border-color:#555;margin:10px 0;"),
                             
                             # LZO Moderation
                             div(class = "section-header", "LZO-Moderation (H3)"),
                             
                             div(class = "slider-label", "LZO-Slope Realistisch"),
                             div(class = "help-text", "Effekt von LZO auf Cheater-CSIM. Positiv = H3 bestätigt"),
                             sliderInput("lzo_slope_realistic", NULL, min = -0.2, max = 0.2, value = 0.08, step = 0.01),
                             
                             div(class = "slider-label", "LZO-Slope Invertiert"),
                             div(class = "help-text", "Effekt von LZO auf Trust-CSIM. Negativ = H3 bestätigt"),
                             sliderInput("lzo_slope_inverted", NULL, min = -0.2, max = 0.2, value = -0.06, step = 0.01)
                         )
        ),
        
        sliderInput("n_per_group", "N pro Gruppe:", min = 30, max = 150, value = 50, step = 10),
        numericInput("seed", "Seed:", value = 42),
        actionButton("simulate", "Simulieren", icon = icon("play"), class = "btn-primary btn-block")
    )
  ),
  
  dashboardBody(
    tabItems(
      # ===== ÜBERSICHT =====
      tabItem(tabName = "overview",
              box(width = 12, title = "Studienübersicht", status = "primary",
                  h3("Ungewöhnlich = Unvergesslich?"),
                  p("2x2 Design: Ökologie (between) x Valenz (within) + LZO (kontinuierlich)"),
                  tags$ul(
                    tags$li(strong("H1:"), "Realistisch: Cheater > Trustworthy (Seltenheitseffekt)"),
                    tags$li(strong("H2:"), "Invertiert: Trustworthy > Cheater (Reversal)"),
                    tags$li(strong("H3:"), "LZO moderiert den Seltenheitseffekt")
                  )
              ),
              box(width = 12, title = "Szenarien-Übersicht", status = "info",
                  tags$table(style = "width:100%; color:white;",
                             tags$tr(tags$th("Szenario"), tags$th("H1"), tags$th("H2"), tags$th("H3")),
                             tags$tr(tags$td("Erwartet"), tags$td("Ja"), tags$td("Ja"), tags$td("Ja")),
                             tags$tr(tags$td("Nur H1"), tags$td("Ja"), tags$td("Nein"), tags$td("Schwach")),
                             tags$tr(tags$td("Null"), tags$td("Nein"), tags$td("Nein"), tags$td("Nein")),
                             tags$tr(tags$td("Starke Moderation"), tags$td("Ja"), tags$td("Ja"), tags$td("Stark"))
                  )
              )
      ),
      
      # ===== DATEN =====
      tabItem(tabName = "simulation",
              fluidRow(
                box(width = 12, title = "Rohdaten (Wide Format)", DTOutput("raw_data"))
              ),
              fluidRow(
                box(width = 6, title = "Datenstruktur", verbatimTextOutput("structure")),
                box(width = 6, title = "Simulationsparameter", tableOutput("params"))
              )
      ),
      
      # ===== INTERAKTION =====
      tabItem(tabName = "interaction",
              fluidRow(
                box(width = 6, title = "Balkendiagramm", plotOutput("inter_plot_bar", height = "400px")),
                box(width = 6, title = "Liniendiagramm", plotOutput("inter_plot_line", height = "400px"),
                    div(class = "interpretation-box",
                        p(strong("Interpretation:"), "Kreuzende Linien zeigen eine disordinale Interaktion (Reversal)."),
                        p(strong("Erwartet:"), "Linien kreuzen sich - Cheater-Vorteil kippt zu Trustworthy-Vorteil."),
                        p(strong("Nur H1:"), "Linien bleiben parallel mit Cheater durchgehend höher.")
                    ))
              ),
              fluidRow(
                box(width = 6, title = "Mittelwerte", tableOutput("inter_means")),
                box(width = 6, title = "Effektgrößen (Cohen's d)", verbatimTextOutput("eff_sizes"))
              )
      ),
      
      # ===== SIMPLE SLOPES =====
      tabItem(tabName = "simple_slopes",
              fluidRow(
                box(width = 12, title = "Simple Slopes: Median-Split", 
                    plotOutput("ss_plot_grouped", height = "400px"),
                    div(class = "interpretation-box",
                        p(strong("Was zeigt der Graph:"), "Vergleich des Valenz-Effekts für Personen mit hoher vs. niedriger LZO (Median-Split). Divergierende Linien bedeuten Moderation."),
                        p(strong("Erwartet:"), "Realistisch: Bei hoher LZO ist der Cheater-Vorteil größer (steilere Linie). Invertiert: Bei hoher LZO ist der Trustworthy-Vorteil größer."),
                        p(strong("Nur H1:"), "Linien verlaufen weitgehend parallel - LZO hat kaum Einfluss.")
                    ))
              ),
              fluidRow(
                box(width = 6, title = "Scatter: Kontinuierliche LZO", 
                    plotOutput("ss_plot_scatter", height = "380px"),
                    div(class = "interpretation-box",
                        p(strong("Was zeigt der Graph:"), "Rohdaten mit Regressionslinien. Unterschiedliche Steigungen der Linien = Moderation durch LZO."),
                        p(strong("Erwartet:"), "Realistisch: Cheater-Linie steigt stärker mit LZO. Invertiert: Trust-Linie steigt stärker."),
                        p(strong("Nur H1:"), "Beide Linien haben ähnliche Steigungen in beiden Ökologien.")
                    )),
                box(width = 6, title = "Differenzen-Plot: Cheater-Vorteil x LZO", 
                    plotOutput("ss_plot_diff", height = "380px"),
                    div(class = "interpretation-box",
                        p(strong("Was zeigt der Graph:"), "Y-Achse = Cheater minus Trustworthy. Oberhalb der gelben Linie = Cheater-Vorteil, unterhalb = Trustworthy-Vorteil."),
                        p(strong("Erwartet:"), "Blaue Linie (realistisch) steigt mit LZO an, orange Linie (invertiert) fällt oder bleibt im negativen Bereich."),
                        p(strong("Nur H1:"), "Beide Linien im positiven Bereich (Cheater immer besser), Steigung gering.")
                    ))
              ),
              fluidRow(
                box(width = 12, title = "Korrelationsmatrix", plotOutput("cor_plot", height = "350px"))
              )
      ),
      
      # ===== DESKRIPTIV =====
      tabItem(tabName = "descriptive",
              fluidRow(
                box(width = 12, title = "Raincloud Plots", plotOutput("rain_plot", height = "450px"))
              ),
              fluidRow(
                box(width = 6, title = "Individuelle Profile", plotOutput("ind_plot", height = "350px")),
                box(width = 6, title = "LZO Verteilung", plotOutput("lzo_plot", height = "350px"))
              ),
              fluidRow(
                box(width = 12, title = "Differenzenscores", plotOutput("diff_plot", height = "300px"))
              )
      ),
      
      # ===== STATISTIK =====
      tabItem(tabName = "statistics",
              fluidRow(
                box(width = 12, title = "Deskriptivstatistik", DTOutput("desc_stats"))
              ),
              fluidRow(
                box(width = 12, title = "Korrelationen", DTOutput("cor_table"))
              )
      ),
      
      # ===== LMM & KONTRASTE =====
      tabItem(tabName = "lmm_results",
              fluidRow(
                box(width = 12, title = "LMM Fixed Effects", 
                    DTOutput("lmm_coef"),
                    div(class = "interpretation-box",
                        p(strong("Relevante Zeilen:")),
                        tags$ul(
                          tags$li(span(class = "hyp-relevant", "valence:"), " Haupteffekt Valenz (über beide Ökologien). Positiv = Cheater > Trust im Mittel."),
                          tags$li(span(class = "hyp-relevant", "ecology:valence:"), " H1 & H2 - Die zentrale Interaktion. Signifikant = Valenz-Effekt unterscheidet sich zwischen Ökologien."),
                          tags$li(span(class = "hyp-relevant", "ecology:valence:lzo:"), " H3 - Dreifachinteraktion. Signifikant = LZO moderiert die Ökologie x Valenz Interaktion.")
                        ),
                        p(class = "hyp-note", "Hinweis: Koeffizienten zeigen Abweichungen von der Referenzkategorie (realistisch/vertrauenswürdig).")
                    ))
              ),
              fluidRow(
                box(width = 12, title = "ANOVA (Typ III)", 
                    DTOutput("anova_tab"),
                    div(class = "interpretation-box",
                        p(strong("Relevante Zeilen:")),
                        tags$ul(
                          tags$li(span(class = "hyp-relevant", "ecology:valence:"), " Test der Interaktion H1/H2. Signifikant = Valenzeffekt hängt von Ökologie ab."),
                          tags$li(span(class = "hyp-relevant", "ecology:valence:scale(lzo):"), " Test von H3. Signifikant = LZO moderiert den Interaktionseffekt.")
                        ),
                        p(class = "hyp-note", "KZO-Effekte sind meist nicht hypothesenrelevant und können vernachlässigt werden.")
                    ))
              ),
              fluidRow(
                box(width = 12, title = "Geplante Kontraste (Hypothesentests)", 
                    DTOutput("contrasts_tab"),
                    div(class = "interpretation-box",
                        p(strong("Direkte Hypothesentests:")),
                        tags$ul(
                          tags$li(span(class = "hyp-relevant", "H1 (Cheater vs Trust | Realistisch):"), " Positiver Schätzer & p < .05 bestätigt H1."),
                          tags$li(span(class = "hyp-relevant", "H2 (Trust vs Cheater | Invertiert):"), " Positiver Schätzer & p < .05 bestätigt H2 (Reversal)."),
                          tags$li(span(class = "hyp-relevant", "Interaktion (H1 vs H2):"), " Testet ob sich die Effekte signifikant unterscheiden.")
                        ),
                        p(class = "hyp-note", "Diese Kontraste testen die Hypothesen direkt und sind präziser als die ANOVA-Haupteffekte.")
                    ))
              ),
              fluidRow(
                box(width = 12, title = "Simple Slopes für LZO (H3)", 
                    DTOutput("simple_slopes_tab"),
                    div(class = "interpretation-box",
                        p(strong("H3-Test: LZO-Effekt auf den Valenzunterschied")),
                        tags$ul(
                          tags$li("Realistisch: Positiver Slope = Bei hoher LZO stärkerer Cheater-Vorteil (H3 bestätigt)."),
                          tags$li("Invertiert: Negativer Slope = Bei hoher LZO stärkerer Trust-Vorteil (H3 bestätigt).")
                        ),
                        p(class = "hyp-note", "Berechnet als Effekt von LZO auf die Differenz CSIM(Cheater) - CSIM(Trust) pro Ökologie.")
                    ))
              )
      ),
      
      # ===== EXPORT =====
      tabItem(tabName = "export",
              fluidRow(
                box(width = 12, title = "Export", status = "primary",
                    p("Exportiere alle Visualisierungen und Daten für das aktuelle Szenario."),
                    p("Dateinamensschema: ", code("[SZENARIO]_[TYP].png/csv")),
                    hr(),
                    fluidRow(
                      column(4,
                             h4("Graphen"),
                             checkboxGroupInput("export_plots", NULL,
                                                choices = c(
                                                  "Interaktion (Balken)" = "interaction_bar",
                                                  "Interaktion (Linien)" = "interaction_line",
                                                  "Simple Slopes (Gruppiert)" = "ss_grouped",
                                                  "Simple Slopes (Scatter)" = "ss_scatter",
                                                  "Differenzen x LZO" = "ss_diff",
                                                  "Korrelationsmatrix" = "correlation",
                                                  "Raincloud" = "raincloud",
                                                  "LZO Verteilung" = "lzo_dist",
                                                  "Differenzen-Dichte" = "diff_density"
                                                ),
                                                selected = c("interaction_bar", "interaction_line", 
                                                             "ss_grouped", "ss_diff", "raincloud"))
                      ),
                      column(4,
                             h4("Daten"),
                             checkboxGroupInput("export_data", NULL,
                                                choices = c(
                                                  "Rohdaten (Wide)" = "data_wide",
                                                  "Rohdaten (Long)" = "data_long",
                                                  "Deskriptivstatistik" = "desc_stats",
                                                  "Korrelationen" = "correlations",
                                                  "LMM Koeffizienten" = "lmm_coef",
                                                  "ANOVA" = "anova",
                                                  "Geplante Kontraste" = "contrasts",
                                                  "Simple Slopes H3" = "simple_slopes"
                                                ),
                                                selected = c("data_wide", "desc_stats", "lmm_coef", "contrasts"))
                      ),
                      column(4,
                             h4("Einstellungen"),
                             sliderInput("export_width", "Breite (inches):", min = 6, max = 14, value = 10),
                             sliderInput("export_height", "Höhe (inches):", min = 4, max = 10, value = 6),
                             sliderInput("export_dpi", "DPI:", min = 100, max = 600, value = 300, step = 50)
                      )
                    ),
                    hr(),
                    downloadButton("download_all", "Alles als ZIP exportieren", class = "btn-lg export-btn"),
                    br(), br(),
                    verbatimTextOutput("export_preview")
                )
              )
      )
      
          )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  study_data <- reactiveVal(NULL)
  
  observeEvent(input$simulate, {
    custom <- if(input$scenario == "custom") {
      list(base_trust_realistic = input$base_trust_realistic, 
           base_cheater_realistic = input$base_cheater_realistic,
           base_trust_inverted = input$base_trust_inverted, 
           base_cheater_inverted = input$base_cheater_inverted,
           lzo_slope_realistic = input$lzo_slope_realistic, 
           lzo_slope_inverted = input$lzo_slope_inverted)
    } else NULL
    
    data <- simulate_study_data(input$n_per_group, input$scenario, custom, input$seed)
    study_data(data)
  })
  
  observe({
    if(is.null(study_data())) {
      data <- simulate_study_data(100, "expected", NULL, 42)
      study_data(data)
    }
  })
  
  # LMM Model
  lmm_model <- reactive({
    req(study_data())
    data <- study_data()$long
    data$subject_id <- factor(data$subject_id)
    
    tryCatch({
      lmer(csim ~ ecology * valence * scale(lzo) + ecology * valence * scale(kzo) + (valence|subject_id), 
           data = data, control = lmerControl(optimizer = "bobyqa"))
    }, error = function(e){
      lmer(csim ~ ecology * valence * scale(lzo) + ecology * valence * scale(kzo) + (1|subject_id), data = data)
    })
  })
  
  # ===== TAB 2: DATEN =====
  output$raw_data <- renderDT({
    req(study_data())
    datatable(study_data()$wide, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$structure <- renderPrint({
    req(study_data())
    cat("WIDE FORMAT\n")
    cat("Probanden:", nrow(study_data()$wide), "\n")
    cat("Variablen:", ncol(study_data()$wide), "\n\n")
    cat("LONG FORMAT\n")
    cat("Beobachtungen:", nrow(study_data()$long), "\n")
  })
  
  output$params <- renderTable({
    req(study_data())
    p <- study_data()$params
    data.frame(
      Parameter = c("Trust Real", "Cheater Real", "Trust Inv", "Cheater Inv", "LZO Slope Real", "LZO Slope Inv"),
      Wert = c(p$base_trust_realistic, p$base_cheater_realistic, p$base_trust_inverted, 
               p$base_cheater_inverted, p$lzo_slope_realistic, p$lzo_slope_inverted)
    )
  }, digits = 2)
  
  # ===== TAB 3: INTERAKTION =====
  output$inter_plot_bar <- renderPlot({
    req(study_data())
    plot_interaction_bar(study_data()$long)
  })
  
  output$inter_plot_line <- renderPlot({
    req(study_data())
    plot_interaction_line(study_data()$long)
  })
  
  output$inter_means <- renderTable({
    req(study_data())
    study_data()$long %>% 
      group_by(ecology, valence) %>% 
      summarise(M = round(mean(csim), 3), SD = round(sd(csim), 3), .groups = "drop")
  })
  
  output$eff_sizes <- renderPrint({
    req(study_data())
    d <- study_data()$wide
    d1 <- (mean(d$csim_cheater[d$ecology == "realistisch"]) - mean(d$csim_trustworthy[d$ecology == "realistisch"])) / 
      sqrt((var(d$csim_cheater[d$ecology == "realistisch"]) + var(d$csim_trustworthy[d$ecology == "realistisch"])) / 2)
    d2 <- (mean(d$csim_trustworthy[d$ecology == "invertiert"]) - mean(d$csim_cheater[d$ecology == "invertiert"])) / 
      sqrt((var(d$csim_trustworthy[d$ecology == "invertiert"]) + var(d$csim_cheater[d$ecology == "invertiert"])) / 2)
    cat("H1 (Cheater - Trust | Realistisch): d =", round(d1, 2), "\n")
    cat("H2 (Trust - Cheater | Invertiert): d =", round(d2, 2), "\n")
    cat("\nd = 0.2 klein | 0.5 mittel | 0.8 groß")
  })
  
  # ===== TAB 4: SIMPLE SLOPES =====
  output$ss_plot_grouped <- renderPlot({
    req(study_data())
    plot_simple_slopes_grouped(study_data()$wide)
  })
  
  output$ss_plot_scatter <- renderPlot({
    req(study_data())
    plot_simple_slopes_scatter(study_data()$long)
  })
  
  output$ss_plot_diff <- renderPlot({
    req(study_data())
    plot_difference_by_lzo(study_data()$wide)
  })
  
  output$cor_plot <- renderPlot({
    req(study_data())
    plot_correlation_matrix(study_data()$wide)
  })
  
  # ===== TAB 5: DESKRIPTIV =====
  output$rain_plot <- renderPlot({
    req(study_data())
    plot_raincloud(study_data()$long)
  })
  
  output$ind_plot <- renderPlot({
    req(study_data())
    set.seed(123)
    samp <- sample(unique(study_data()$long$subject_id), min(30, length(unique(study_data()$long$subject_id))))
    sub <- study_data()$long %>% filter(subject_id %in% samp)
    
    ggplot(sub, aes(x = valence, y = csim, group = subject_id, color = valence)) +
      geom_line(alpha = 0.3, color = "gray70") +
      geom_point(alpha = 0.6, size = 2) +
      facet_wrap(~ecology) +
      scale_color_manual(values = c("vertrauenswürdig" = "#40E0D0", "betrügerisch" = "#DC143C")) +
      dark_theme() +
      theme(legend.position = "none") +
      labs(y = "CSIM", x = "Valenz")
  })
  
  output$lzo_plot <- renderPlot({
    req(study_data())
    ggplot(study_data()$wide, aes(x = lzo, fill = ecology)) +
      geom_density(alpha = 0.6) +
      scale_fill_manual(values = c("realistisch" = "#4169E1", "invertiert" = "#FF8C00")) +
      dark_theme() +
      labs(title = "LZO-Verteilung (Randomisierungscheck)", x = "LZO (z)", fill = "Ökologie")
  })
  
  output$diff_plot <- renderPlot({
    req(study_data())
    data <- study_data()$wide %>% mutate(diff = csim_cheater - csim_trustworthy)
    
    ggplot(data, aes(x = diff, fill = ecology)) +
      geom_density(alpha = 0.6) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "yellow", size = 1) +
      scale_fill_manual(values = c("realistisch" = "#4169E1", "invertiert" = "#FF8C00")) +
      dark_theme() +
      labs(title = "Differenzenscores pro Person", x = "CSIM(Cheater) - CSIM(Trust)", fill = "Ökologie")
  })
  
  # ===== TAB 6: STATISTIK =====
  output$desc_stats <- renderDT({
    req(study_data())
    tab <- study_data()$long %>% 
      group_by(ecology, valence) %>%
      summarise(N = n(), M = round(mean(csim), 3), SD = round(sd(csim), 3), 
                Min = round(min(csim), 3), Max = round(max(csim), 3), .groups = "drop")
    datatable(tab, options = list(dom = 't'), rownames = FALSE)
  })
  
  output$cor_table <- renderDT({
    req(study_data())
    vars <- study_data()$wide %>% select(csim_trustworthy, csim_cheater, lzo, kzo)
    colnames(vars) <- c("Trustworthy", "Cheater", "LZO", "KZO")
    M <- round(cor(vars), 3)
    meltM <- melt(M)
    colnames(meltM) <- c("Var1", "Var2", "r")
    datatable(meltM, rownames = FALSE, options = list(pageLength = 16, searching = FALSE))
  })
  
  # ===== TAB 7: LMM & KONTRASTE =====
  
  # LMM Koeffizienten - mit bereinigten Namen
  output$lmm_coef <- renderDT({
    req(study_data())
    model <- lmm_model()
    sum <- summary(model)
    coef_df <- as.data.frame(sum$coefficients)
    coef_df$Term <- rownames(coef_df)
    colnames(coef_df) <- c("Estimate", "SE", "df", "t", "p", "Term")
    
    # Namen bereinigen
    coef_df$Term <- gsub("ecologyinvertiert", "ecology", coef_df$Term)
    coef_df$Term <- gsub("valencebetrügerisch", "valence", coef_df$Term)
    coef_df$Term <- gsub("scale\\(lzo\\)", "lzo", coef_df$Term)
    coef_df$Term <- gsub("scale\\(kzo\\)", "kzo", coef_df$Term)
    
    coef_df <- coef_df[, c("Term", "Estimate", "SE", "df", "t", "p")]
    coef_df$Sig <- ifelse(coef_df$p < 0.001, "***", ifelse(coef_df$p < 0.01, "**", ifelse(coef_df$p < 0.05, "*", "")))
    
    datatable(coef_df, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE)) %>%
      formatRound(c("Estimate", "SE", "t"), 3) %>%
      formatRound("p", 4) %>%
      formatStyle(columns = names(coef_df), color = "white", backgroundColor = "#2d2d2d") %>%
      formatStyle("Sig", color = "#69ff69", fontWeight = "bold")
  })
  
  # ANOVA
  output$anova_tab <- renderDT({
    req(study_data())
    model <- lmm_model()
    a <- anova(model)
    a_df <- as.data.frame(a)
    a_df$Effect <- rownames(a_df)
    a_df <- a_df[, c("Effect", "NumDF", "DenDF", "F value", "Pr(>F)")]
    colnames(a_df) <- c("Effekt", "df1", "df2", "F", "p")
    
    # Namen bereinigen
    a_df$Effekt <- gsub("scale\\(lzo\\)", "lzo", a_df$Effekt)
    a_df$Effekt <- gsub("scale\\(kzo\\)", "kzo", a_df$Effekt)
    
    a_df$Sig <- ifelse(a_df$p < 0.001, "***", ifelse(a_df$p < 0.01, "**", ifelse(a_df$p < 0.05, "*", "")))
    
    datatable(a_df, rownames = FALSE, options = list(pageLength = 15)) %>%
      formatRound(c("F"), 2) %>%
      formatRound("p", 4) %>%
      formatStyle(columns = names(a_df), color = "white", backgroundColor = "#2d2d2d") %>%
      formatStyle("Sig", color = "#69ff69", fontWeight = "bold")
  })
  
  # Geplante Kontraste
  output$contrasts_tab <- renderDT({
    req(study_data())
    model <- lmm_model()
    
    # EMMs berechnen
    emm <- emmeans(model, ~ valence | ecology)
    
    # Kontraste: Cheater vs Trust innerhalb jeder Ökologie
    contrasts_result <- contrast(emm, method = "revpairwise", adjust = "bonferroni")
    contrasts_df <- as.data.frame(contrasts_result)
    
    # Interaktionskontrast berechnen
    emm_int <- emmeans(model, ~ valence * ecology)
    int_contrast <- contrast(emm_int, interaction = "pairwise")
    int_df <- as.data.frame(int_contrast)
    
    # Zusammenfügen
    result <- data.frame(
      Hypothese = c("H1: Cheater vs Trust (Realistisch)", 
                    "H2: Cheater vs Trust (Invertiert)",
                    "H1 vs H2: Interaktionskontrast"),
      Schätzer = c(contrasts_df$estimate[1], 
                   contrasts_df$estimate[2],
                   int_df$estimate[1]),
      SE = c(contrasts_df$SE[1], 
             contrasts_df$SE[2],
             int_df$SE[1]),
      t = c(contrasts_df$t.ratio[1], 
            contrasts_df$t.ratio[2],
            int_df$t.ratio[1]),
      p = c(contrasts_df$p.value[1], 
            contrasts_df$p.value[2],
            int_df$p.value[1])
    )
    
    result$Sig <- ifelse(result$p < 0.001, "***", ifelse(result$p < 0.01, "**", ifelse(result$p < 0.05, "*", "")))
    result$Interpretation <- c(
      ifelse(result$Schätzer[1] > 0 & result$p[1] < 0.05, "H1 bestätigt", "H1 nicht bestätigt"),
      ifelse(result$Schätzer[2] < 0 & result$p[2] < 0.05, "H2 bestätigt (Reversal)", "H2 nicht bestätigt"),
      ifelse(result$p[3] < 0.05, "Signifikante Interaktion", "Keine sign. Interaktion")
    )
    
    datatable(result, rownames = FALSE, options = list(dom = 't', pageLength = 10)) %>%
      formatRound(c("Schätzer", "SE", "t"), 3) %>%
      formatRound("p", 4) %>%
      formatStyle(columns = names(result), color = "white", backgroundColor = "#2d2d2d") %>%
      formatStyle("Sig", color = "#69ff69", fontWeight = "bold") %>%
      formatStyle("Interpretation", color = "#ce9ffc")
  })
  
  # Simple Slopes für H3
  output$simple_slopes_tab <- renderDT({
    req(study_data())
    
    # Manuelle Berechnung der Simple Slopes für Differenzen
    data_wide <- study_data()$wide
    data_wide$diff <- data_wide$csim_cheater - data_wide$csim_trustworthy
    
    # Getrennte Regressionen
    mod_real <- lm(diff ~ lzo, data = data_wide[data_wide$ecology == "realistisch", ])
    mod_inv <- lm(diff ~ lzo, data = data_wide[data_wide$ecology == "invertiert", ])
    
    result <- data.frame(
      Ökologie = c("Realistisch", "Invertiert"),
      `LZO-Slope` = c(coef(mod_real)["lzo"], coef(mod_inv)["lzo"]),
      SE = c(summary(mod_real)$coefficients["lzo", "Std. Error"],
             summary(mod_inv)$coefficients["lzo", "Std. Error"]),
      t = c(summary(mod_real)$coefficients["lzo", "t value"],
            summary(mod_inv)$coefficients["lzo", "t value"]),
      p = c(summary(mod_real)$coefficients["lzo", "Pr(>|t|)"],
            summary(mod_inv)$coefficients["lzo", "Pr(>|t|)"])
    )
    
    result$Sig <- ifelse(result$p < 0.001, "***", ifelse(result$p < 0.01, "**", ifelse(result$p < 0.05, "*", "")))
    result$Interpretation <- c(
      ifelse(result$LZO.Slope[1] > 0 & result$p[1] < 0.05, "H3 bestäigt: LZO verstärkt Cheater-Vorteil", 
             ifelse(result$p[1] >= 0.05, "Kein sign. LZO-Effekt", "Unerwartete Richtung")),
      ifelse(result$LZO.Slope[2] < 0 & result$p[2] < 0.05, "H3 bestätigt: LZO verstärkt Trust-Vorteil",
             ifelse(result$p[2] >= 0.05, "Kein sign. LZO-Effekt", "Unerwartete Richtung"))
    )
    
    colnames(result) <- c("Ökologie", "LZO-Slope", "SE", "t", "p", "Sig", "Interpretation")
    
    datatable(result, rownames = FALSE, options = list(dom = 't')) %>%
      formatRound(c("LZO-Slope", "SE", "t"), 3) %>%
      formatRound("p", 4) %>%
      formatStyle(columns = names(result), color = "white", backgroundColor = "#2d2d2d") %>%
      formatStyle("Sig", color = "#69ff69", fontWeight = "bold") %>%
      formatStyle("Interpretation", color = "#ce9ffc")
  })
  
  # ===== EXPORT =====
  
  output$export_preview <- renderPrint({
    scenario_name <- toupper(input$scenario)
    
    cat("Vorschau der Dateinamen:\n")
    cat("========================\n\n")
    
    if (length(input$export_plots) > 0) {
      cat("GRAPHEN:\n")
      for (p in input$export_plots) {
        cat(paste0("  ", scenario_name, "_", p, ".png\n"))
      }
      cat("\n")
    }
    
    if (length(input$export_data) > 0) {
      cat("DATEN:\n")
      for (d in input$export_data) {
        cat(paste0("  ", scenario_name, "_", d, ".csv\n"))
      }
    }
    
    cat("\n========================\n")
    cat(paste0("ZIP: ", scenario_name, "_export.zip"))
  })
  
  output$download_all <- downloadHandler(
    filename = function() {
      paste0(toupper(input$scenario), "_export.zip")
    },
    content = function(file) {
      temp_dir <- tempdir()
      export_dir <- file.path(temp_dir, "export")
      dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)
      
      scenario_name <- toupper(input$scenario)
      files_to_zip <- c()
      
      # Graphen exportieren
      if (length(input$export_plots) > 0) {
        
        plot_functions <- list(
          interaction_bar = function() plot_interaction_bar(study_data()$long),
          interaction_line = function() plot_interaction_line(study_data()$long),
          ss_grouped = function() plot_simple_slopes_grouped(study_data()$wide),
          ss_scatter = function() plot_simple_slopes_scatter(study_data()$long),
          ss_diff = function() plot_difference_by_lzo(study_data()$wide),
          correlation = function() plot_correlation_matrix(study_data()$wide),
          raincloud = function() plot_raincloud(study_data()$long),
          lzo_dist = function() {
            ggplot(study_data()$wide, aes(x = lzo, fill = ecology)) +
              geom_density(alpha = 0.6) +
              scale_fill_manual(values = c("realistisch" = "#4169E1", "invertiert" = "#FF8C00")) +
              dark_theme() +
              labs(title = "LZO-Verteilung", x = "LZO (z)", fill = "Ökologie")
          },
          diff_density = function() {
            data <- study_data()$wide %>% mutate(diff = csim_cheater - csim_trustworthy)
            ggplot(data, aes(x = diff, fill = ecology)) +
              geom_density(alpha = 0.6) +
              geom_vline(xintercept = 0, linetype = "dashed", color = "yellow", size = 1) +
              scale_fill_manual(values = c("realistisch" = "#4169E1", "invertiert" = "#FF8C00")) +
              dark_theme() +
              labs(title = "Differenzenscores", x = "CSIM(Cheater) - CSIM(Trust)", fill = "Ökologie")
          }
        )
        
        for (plot_name in input$export_plots) {
          if (plot_name %in% names(plot_functions)) {
            filename <- file.path(export_dir, paste0(scenario_name, "_", plot_name, ".png"))
            ggsave(filename, plot = plot_functions[[plot_name]](), 
                   width = input$export_width, height = input$export_height, 
                   dpi = input$export_dpi, bg = "#2d2d2d")
            files_to_zip <- c(files_to_zip, filename)
          }
        }
      }
      
      # Daten exportieren
      if (length(input$export_data) > 0) {
        
        if ("data_wide" %in% input$export_data) {
          filename <- file.path(export_dir, paste0(scenario_name, "_data_wide.csv"))
          write.csv(study_data()$wide, filename, row.names = FALSE)
          files_to_zip <- c(files_to_zip, filename)
        }
        
        if ("data_long" %in% input$export_data) {
          filename <- file.path(export_dir, paste0(scenario_name, "_data_long.csv"))
          write.csv(study_data()$long, filename, row.names = FALSE)
          files_to_zip <- c(files_to_zip, filename)
        }
        
        if ("desc_stats" %in% input$export_data) {
          filename <- file.path(export_dir, paste0(scenario_name, "_desc_stats.csv"))
          desc <- study_data()$long %>% 
            group_by(ecology, valence) %>%
            summarise(N = n(), M = mean(csim), SD = sd(csim), 
                      Min = min(csim), Max = max(csim), .groups = "drop")
          write.csv(desc, filename, row.names = FALSE)
          files_to_zip <- c(files_to_zip, filename)
        }
        
        if ("correlations" %in% input$export_data) {
          filename <- file.path(export_dir, paste0(scenario_name, "_correlations.csv"))
          vars <- study_data()$wide %>% select(csim_trustworthy, csim_cheater, lzo, kzo)
          write.csv(cor(vars), filename)
          files_to_zip <- c(files_to_zip, filename)
        }
        
        if ("lmm_coef" %in% input$export_data) {
          filename <- file.path(export_dir, paste0(scenario_name, "_lmm_coef.csv"))
          model <- lmm_model()
          coef_df <- as.data.frame(summary(model)$coefficients)
          coef_df$Term <- rownames(coef_df)
          write.csv(coef_df, filename, row.names = FALSE)
          files_to_zip <- c(files_to_zip, filename)
        }
        
        if ("anova" %in% input$export_data) {
          filename <- file.path(export_dir, paste0(scenario_name, "_anova.csv"))
          a <- as.data.frame(anova(lmm_model()))
          a$Effect <- rownames(a)
          write.csv(a, filename, row.names = FALSE)
          files_to_zip <- c(files_to_zip, filename)
        }
        
        if ("contrasts" %in% input$export_data) {
          filename <- file.path(export_dir, paste0(scenario_name, "_contrasts.csv"))
          model <- lmm_model()
          emm <- emmeans(model, ~ valence | ecology)
          contrasts_result <- contrast(emm, method = "revpairwise", adjust = "bonferroni")
          contrasts_df <- as.data.frame(contrasts_result)
          write.csv(contrasts_df, filename, row.names = FALSE)
          files_to_zip <- c(files_to_zip, filename)
        }
        
        if ("simple_slopes" %in% input$export_data) {
          filename <- file.path(export_dir, paste0(scenario_name, "_simple_slopes_h3.csv"))
          data_wide <- study_data()$wide
          data_wide$diff <- data_wide$csim_cheater - data_wide$csim_trustworthy
          
          mod_real <- lm(diff ~ lzo, data = data_wide[data_wide$ecology == "realistisch", ])
          mod_inv <- lm(diff ~ lzo, data = data_wide[data_wide$ecology == "invertiert", ])
          
          result <- data.frame(
            Ecology = c("Realistisch", "Invertiert"),
            LZO_Slope = c(coef(mod_real)["lzo"], coef(mod_inv)["lzo"]),
            SE = c(summary(mod_real)$coefficients["lzo", "Std. Error"],
                   summary(mod_inv)$coefficients["lzo", "Std. Error"]),
            t = c(summary(mod_real)$coefficients["lzo", "t value"],
                  summary(mod_inv)$coefficients["lzo", "t value"]),
            p = c(summary(mod_real)$coefficients["lzo", "Pr(>|t|)"],
                  summary(mod_inv)$coefficients["lzo", "Pr(>|t|)"])
          )
          write.csv(result, filename, row.names = FALSE)
          files_to_zip <- c(files_to_zip, filename)
        }
      }
      
      # ZIP erstellen
      zip::zip(file, files = basename(files_to_zip), root = export_dir)
    }
  )
}

shinyApp(ui = ui, server = server)
