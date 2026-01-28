library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(DT)
library(reshape2)
library(zip)

# CSS für Dark Mode mit hellen Tabellen
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

/* Kritisch: DataTables hell machen */
.dataTables_wrapper { color: #fff !important; }
table.dataTable { color: #fff !important; background-color: #2d2d2d !important; }
table.dataTable thead th { color: #fff !important; background-color: #3c3c3c !important; border-color: #555 !important; }
table.dataTable tbody td { color: #fff !important; border-color: #444 !important; }
.dataTables_length, .dataTables_filter, .dataTables_info, .dataTables_paginate { color: #fff !important; }
.paginate_button { color: #fff !important; }
.paginate_button.current { color: #000 !important; }

/* Slider Labels */
.slider-label { color: #ce9ffc; font-weight: bold; font-size: 12px; margin-top: 10px; }
.help-text { color: #aaa; font-size: 11px; margin-bottom: 10px; font-style: italic; }

/* Export Button Styling */
.export-btn { 
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important; 
  border: none !important;
  margin-top: 15px;
}
.export-btn:hover { 
  background: linear-gradient(135deg, #764ba2 0%, #667eea 100%) !important; 
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
# PLOT FUNKTIONEN (für Export wiederverwendbar)
# ============================================================================

# Theme für alle Plots
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

# 2x2 Balkendiagramm
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
    labs(y = "CSIM (Source Memory)", x = "Informationsökologie", fill = "Valenz",
         title = "2x2 Interaktion: Ökologie × Valenz")
}

# NEU: 2x2 Liniendiagramm für Reversal
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
    labs(y = "CSIM (Source Memory)", x = "Informationsökologie", color = "Valenz",
         title = "Interaktionseffekt: Kreuzung = Reversal (H2)",
         subtitle = "Parallele Linien = kein Interaktionseffekt, Kreuzung = disordinale Interaktion")
}

# Simple Slopes: Gruppiertes Liniendiagramm
plot_simple_slopes_grouped <- function(data_wide) {
  plot_data <- data_wide %>%
    mutate(lzo_group = ifelse(lzo > median(lzo), "LZO hoch (+1SD)", "LZO niedrig (-1SD)")) %>%
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
    scale_linetype_manual(values = c("LZO hoch (+1SD)" = "solid", "LZO niedrig (-1SD)" = "dashed")) +
    scale_shape_manual(values = c("LZO hoch (+1SD)" = 16, "LZO niedrig (-1SD)" = 17)) +
    dark_theme() +
    theme(legend.position = "bottom") +
    labs(title = "Simple Slopes: Moderation durch LZO (H3)", 
         subtitle = "Divergierende Linien = LZO moderiert den Valenz-Effekt",
         y = "CSIM", color = "Valenz", linetype = "LZO-Gruppe", shape = "LZO-Gruppe")
}

# NEU: Scatter mit Regressionslinien (kontinuierliche LZO)
plot_simple_slopes_scatter <- function(data_long) {
  ggplot(data_long, aes(x = lzo, y = csim, color = valence)) +
    geom_point(alpha = 0.4, size = 1.5) +
    geom_smooth(method = "lm", se = TRUE, size = 1.2) +
    facet_wrap(~ecology) +
    scale_color_manual(values = c("vertrauenswürdig" = "#40E0D0", "betrügerisch" = "#DC143C")) +
    dark_theme() +
    theme(legend.position = "bottom") +
    labs(title = "LZO × Valenz: Kontinuierliche Moderation (H3)",
         subtitle = "Unterschiedliche Steigungen = Moderation durch Langzeitorientierung",
         x = "Langzeitorientierung (z-standardisiert)", y = "CSIM", color = "Valenz")
}

# NEU: Differenzen-Plot (sehr intuitiv für H3)
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
    labs(title = "Cheater-Vorteil als Funktion von LZO (H3)",
         subtitle = "Gelbe Linie = kein Vorteil | Oberhalb = Cheater besser | Unterhalb = Trustworthy besser",
         x = "Langzeitorientierung (z-standardisiert)", 
         y = "Differenz: CSIM(Cheater) − CSIM(Trustworthy)",
         color = "Ökologie") +
    annotate("text", x = -2, y = 0.15, label = "Cheater-Vorteil", color = "#DC143C", hjust = 0, size = 3.5) +
    annotate("text", x = -2, y = -0.15, label = "Trustworthy-Vorteil", color = "#40E0D0", hjust = 0, size = 3.5)
}

# Raincloud Plot
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
    labs(title = "Verteilung der CSIM-Werte", y = "CSIM Score", x = "Valenz")
}

# Individuelle Profile
plot_individual_profiles <- function(data_long) {
  set.seed(123)
  samp <- sample(unique(data_long$subject_id), min(30, length(unique(data_long$subject_id))))
  sub <- data_long %>% filter(subject_id %in% samp)
  
  ggplot(sub, aes(x = valence, y = csim, group = subject_id, color = valence)) +
    geom_line(alpha = 0.3, color = "gray70") +
    geom_point(alpha = 0.6, size = 2) +
    facet_wrap(~ecology) +
    scale_color_manual(values = c("vertrauenswürdig" = "#40E0D0", "betrügerisch" = "#DC143C")) +
    dark_theme() +
    theme(legend.position = "none") +
    labs(title = "Individuelle Profile (Stichprobe n=30)", y = "CSIM", x = "Valenz")
}

# LZO Verteilung
plot_lzo_distribution <- function(data_wide) {
  ggplot(data_wide, aes(x = lzo, fill = ecology)) +
    geom_density(alpha = 0.6) +
    scale_fill_manual(values = c("realistisch" = "#4169E1", "invertiert" = "#FF8C00")) +
    dark_theme() +
    labs(title = "LZO-Verteilung nach Ökologie (Randomisierungscheck)", 
         subtitle = "Gute Überlappung = erfolgreiche Randomisierung",
         x = "Langzeitorientierung (z-standardisiert)", fill = "Ökologie")
}

# Differenzenscores Dichte
plot_difference_density <- function(data_wide) {
  data <- data_wide %>% mutate(diff = csim_cheater - csim_trustworthy)
  
  ggplot(data, aes(x = diff, fill = ecology)) +
    geom_density(alpha = 0.6) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "yellow", size = 1) +
    scale_fill_manual(values = c("realistisch" = "#4169E1", "invertiert" = "#FF8C00")) +
    dark_theme() +
    labs(title = "Verteilung der Differenzenscores", 
         subtitle = "CSIM(Cheater) − CSIM(Trustworthy) | Gelb = kein Unterschied",
         x = "Differenz", y = "Dichte", fill = "Ökologie")
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
      menuItem("LMM", tabName = "lmm_results", icon = icon("calculator")),
      menuItem("Export", tabName = "export", icon = icon("download"))
    ),
    
    tags$head(HTML(dark_css)),
    
    div(style = "padding: 15px;",
        selectInput("scenario", "Szenario:", 
                    choices = c("Erwartet" = "expected", "Null" = "null", "Nur H1" = "partial_h1_only", 
                                "Starke Moderation" = "strong_moderation", "Deckeneffekt" = "ceiling_effect", "Custom" = "custom")),
        
        conditionalPanel("input.scenario == 'custom'",
                         div(style = "background:#3c3c3c;padding:10px;border-radius:5px;margin-bottom:10px;",
                             div(class = "slider-label", "Trustworthy (Realistisch)"),
                             div(class = "help-text", "Basis-SM wenn häufig (70%). Normal ~0.65"),
                             sliderInput("base_trust_realistic", NULL, min = 0.3, max = 1, value = 0.65, step = 0.01),
                             
                             div(class = "slider-label", "Cheater (Realistisch)"),
                             div(class = "help-text", "Basis-SM wenn selten (30%). Höher = stärkerer H1"),
                             sliderInput("base_cheater_realistic", NULL, min = 0.3, max = 1, value = 0.78, step = 0.01),
                             
                             hr(style = "border-color:#666;"),
                             div(class = "slider-label", "Trustworthy (Invertiert)"),
                             div(class = "help-text", "Hier selten (30%). Höher = stärkerer H2 Reversal"),
                             sliderInput("base_trust_inverted", NULL, min = 0.3, max = 1, value = 0.72, step = 0.01),
                             
                             div(class = "slider-label", "Cheater (Invertiert)"),
                             div(class = "help-text", "Hier häufig (70%). Normal niedrig ~0.58"),
                             sliderInput("base_cheater_inverted", NULL, min = 0.3, max = 1, value = 0.58, step = 0.01),
                             
                             hr(style = "border-color:#666;"),
                             div(class = "slider-label", "LZO Moderation Realistisch"),
                             div(class = "help-text", "Positiv = LZO verstärkt Cheater-Vorteil (H3)"),
                             sliderInput("lzo_slope_realistic", NULL, min = -0.2, max = 0.2, value = 0.08, step = 0.01),
                             
                             div(class = "slider-label", "LZO Moderation Invertiert"),
                             div(class = "help-text", "Negativ = LZO verstärkt Trustworthy-Vorteil"),
                             sliderInput("lzo_slope_inverted", NULL, min = -0.2, max = 0.2, value = -0.06, step = 0.01)
                         )
        ),
        
        sliderInput("n_per_group", "N pro Gruppe:", min = 44, max = 200, value = 100, step = 11),
        numericInput("seed", "Seed:", value = 42),
        actionButton("simulate", "Simulieren", icon = icon("play"), class = "btn-primary btn-block")
    )
  ),
  
  dashboardBody(
    tabItems(
      # Tab 1: Übersicht
      tabItem(tabName = "overview",
              box(width = 12, title = "Studienübersicht", status = "primary",
                  h3("Ungewöhnlich = Unvergesslich?"),
                  p("2x2 Design: Ökologie (between) × Valenz (within) × LZO (kontinuierlich)"),
                  tags$ul(
                    tags$li(strong("H1:"), "Realistisch: Cheater > Trustworthy (Seltenheitseffekt)"),
                    tags$li(strong("H2:"), "Invertiert: Trustworthy > Cheater (Reversal)"),
                    tags$li(strong("H3:"), "LZO moderiert den Rarity-Effekt")
                  )
              ),

      ),
      
      # Tab 2: Daten
      tabItem(tabName = "simulation",
              fluidRow(
                box(width = 12, title = "Rohdaten (Wide Format)", DTOutput("raw_data"))
              ),
              fluidRow(
                box(width = 6, title = "Datenstruktur", verbatimTextOutput("structure")),
                box(width = 6, title = "Simulationsparameter", tableOutput("params"))
              )
      ),
      
      # Tab 3: Interaktion
      tabItem(tabName = "interaction",
              fluidRow(
                box(width = 6, title = "Balkendiagramm (klassisch)", plotOutput("inter_plot_bar", height = "450px")),
                box(width = 6, title = "Liniendiagramm (Reversal)", plotOutput("inter_plot_line", height = "450px")),
                  
              ),
              fluidRow(
                box(width = 6, title = "Mittelwerte", tableOutput("inter_means")),
                box(width = 6, title = "Effektgrößen (Cohen's d)", verbatimTextOutput("eff_sizes"))
              )
      ),
      
      # Tab 4: Simple Slopes
      tabItem(tabName = "simple_slopes",
              fluidRow(
                box(width = 12, title = "Simple Slopes: Gruppiert (Median-Split)", 
                    plotOutput("ss_plot_grouped", height = "400px"))
              ),
              fluidRow(
                box(width = 6, title = "Scatter mit Regressionslinien (kontinuierlich)", 
                    plotOutput("ss_plot_scatter", height = "400px"),
                    p(em("Zeigt den kontinuierlichen Einfluss von LZO auf CSIM für beide Valenz-Stufen."))),
                box(width = 6, title = "Differenzen-Plot: Cheater-Vorteil × LZO", 
                    plotOutput("ss_plot_diff", height = "400px"),
                    p(em("Sehr intuitiv: Zeigt direkt, wie LZO den Cheater-Vorteil beeinflusst (H3).")))
              ),
              fluidRow(
                box(width = 12, title = "Korrelationsmatrix", plotOutput("cor_plot", height = "350px"))
              )
      ),
      
      # Tab 5: Deskriptiv
      tabItem(tabName = "descriptive",
              fluidRow(
                box(width = 12, title = "Raincloud Plots", plotOutput("rain_plot", height = "450px"))
              ),
              fluidRow(
                box(width = 6, title = "Individuelle Profile (Stichprobe)", plotOutput("ind_plot", height = "350px")),
                box(width = 6, title = "LZO Verteilung nach Ökologie", plotOutput("lzo_plot", height = "350px"))
              ),
              fluidRow(
                box(width = 12, title = "Differenzenscores pro Person", plotOutput("diff_plot", height = "300px"))
              )
      ),
      
      # Tab 6: Statistik
      tabItem(tabName = "statistics",
              fluidRow(
                box(width = 12, title = "Deskriptivstatistik", DTOutput("desc_stats"))
              ),
              fluidRow(
                box(width = 12, title = "Korrelationen", DTOutput("cor_table"))
              )
      ),
      
      # Tab 7: LMM
      tabItem(tabName = "lmm_results",
              fluidRow(
                box(width = 12, title = "LMM Fixed Effects", DTOutput("lmm_coef"))
              ),
              fluidRow(
                box(width = 12, title = "ANOVA", DTOutput("anova_tab"))
              )
      ),
      
      # Tab 8: Export
      tabItem(tabName = "export",
              fluidRow(
                box(width = 12, title = "Export-Optionen", status = "primary",
                    p("Exportiere alle Visualisierungen und Daten für das aktuell eingestellte Szenario."),
                    p(strong("Dateinamensschema:"), code("[SZENARIO]_[GRAPHTYP].png"), " bzw. ", code("[SZENARIO]_[DATENTYP].csv")),
                    hr(),
                    fluidRow(
                      column(4,
                             h4("Graphen exportieren"),
                             checkboxGroupInput("export_plots", "Auswählen:",
                                                choices = c(
                                                  "Interaktion (Balken)" = "interaction_bar",
                                                  "Interaktion (Linien)" = "interaction_line",
                                                  "Simple Slopes (Gruppiert)" = "ss_grouped",
                                                  "Simple Slopes (Scatter)" = "ss_scatter",
                                                  "Differenzen × LZO" = "ss_diff",
                                                  "Korrelationsmatrix" = "correlation",
                                                  "Raincloud" = "raincloud",
                                                  "Individuelle Profile" = "individual",
                                                  "LZO Verteilung" = "lzo_dist",
                                                  "Differenzen-Dichte" = "diff_density"
                                                ),
                                                selected = c("interaction_bar", "interaction_line", "ss_grouped", 
                                                             "ss_scatter", "ss_diff", "raincloud"))
                      ),
                      column(4,
                             h4("Daten exportieren"),
                             checkboxGroupInput("export_data", "Auswählen:",
                                                choices = c(
                                                  "Rohdaten (Wide)" = "data_wide",
                                                  "Rohdaten (Long)" = "data_long",
                                                  "Deskriptivstatistik" = "desc_stats",
                                                  "Korrelationen" = "correlations",
                                                  "LMM Koeffizienten" = "lmm_coef",
                                                  "ANOVA Tabelle" = "anova"
                                                ),
                                                selected = c("data_wide", "desc_stats", "lmm_coef"))
                      ),
                      column(4,
                             h4("Einstellungen"),
                             sliderInput("export_width", "Breite (inches):", min = 6, max = 14, value = 10),
                             sliderInput("export_height", "Höhe (inches):", min = 4, max = 10, value = 6),
                             sliderInput("export_dpi", "DPI:", min = 100, max = 600, value = 300, step = 50)
                      )
                    ),
                    hr(),
                    downloadButton("download_all", "📦 Alles als ZIP exportieren", class = "btn-lg export-btn"),
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
  
  # ============================================================================
  # Tab 2: Daten
  # ============================================================================
  
  output$raw_data <- renderDT({
    req(study_data())
    datatable(study_data()$wide, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$structure <- renderPrint({
    req(study_data())
    cat("=== WIDE FORMAT ===\n")
    cat("Probanden:", nrow(study_data()$wide), "\n")
    cat("Variablen:", ncol(study_data()$wide), "\n\n")
    cat("=== LONG FORMAT ===\n")
    cat("Beobachtungen:", nrow(study_data()$long), "\n")
    cat("(2 Messungen pro Person)\n")
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
  
  # ============================================================================
  # Tab 3: Interaktion
  # ============================================================================
  
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
    cat("H1 (Cheater-Vorteil in Realistisch): d =", round(d1, 2), "\n")
    cat("H2 (Trustworthy-Vorteil in Invertiert): d =", round(d2, 2), "\n")
    cat("\nInterpretation:\n")
    cat("d = 0.2: klein | 0.5: mittel | 0.8: groß")
  })
  
  # ============================================================================
  # Tab 4: Simple Slopes
  # ============================================================================
  
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
  
  # ============================================================================
  # Tab 5: Deskriptiv
  # ============================================================================
  
  output$rain_plot <- renderPlot({
    req(study_data())
    plot_raincloud(study_data()$long)
  })
  
  output$ind_plot <- renderPlot({
    req(study_data())
    plot_individual_profiles(study_data()$long)
  })
  
  output$lzo_plot <- renderPlot({
    req(study_data())
    plot_lzo_distribution(study_data()$wide)
  })
  
  output$diff_plot <- renderPlot({
    req(study_data())
    plot_difference_density(study_data()$wide)
  })
  
  # ============================================================================
  # Tab 6: Statistik
  # ============================================================================
  
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
    datatable(meltM, rownames = FALSE, options = list(pageLength = 16, searching = FALSE)) %>%
      formatRound("r", 3)
  })
  
  # ============================================================================
  # Tab 7: LMM
  # ============================================================================
  
  output$lmm_coef <- renderDT({
    req(study_data())
    model <- lmm_model()
    sum <- summary(model)
    coef_df <- as.data.frame(sum$coefficients)
    coef_df$Term <- rownames(coef_df)
    colnames(coef_df) <- c("Estimate", "SE", "df", "t", "p", "Term")
    coef_df <- coef_df[, c("Term", "Estimate", "SE", "df", "t", "p")]
    coef_df$Sig <- ifelse(coef_df$p < 0.001, "***", ifelse(coef_df$p < 0.01, "**", ifelse(coef_df$p < 0.05, "*", "")))
    
    datatable(coef_df, rownames = FALSE, 
              options = list(pageLength = 20, scrollX = TRUE)) %>%
      formatRound(c("Estimate", "SE", "t"), 3) %>%
      formatRound("p", 4) %>%
      formatStyle(columns = c("Term", "Estimate", "SE", "df", "t", "p", "Sig"), 
                  color = "white", backgroundColor = "#2d2d2d") %>%
      formatStyle("Sig", color = "#69ff69", fontWeight = "bold")
  })
  
  output$anova_tab <- renderDT({
    req(study_data())
    model <- lmm_model()
    a <- anova(model)
    a_df <- as.data.frame(a)
    a_df$Effect <- rownames(a_df)
    a_df <- a_df[, c("Effect", "NumDF", "DenDF", "F value", "Pr(>F)")]
    colnames(a_df) <- c("Effekt", "df_num", "df_den", "F", "p")
    a_df$Sig <- ifelse(a_df$p < 0.001, "***", ifelse(a_df$p < 0.01, "**", ifelse(a_df$p < 0.05, "*", "n.s.")))
    
    datatable(a_df, rownames = FALSE, options = list(pageLength = 15)) %>%
      formatRound(c("F"), 2) %>%
      formatRound("p", 4) %>%
      formatStyle(columns = c("Effekt", "df_num", "df_den", "F", "p", "Sig"),
                  color = "white", backgroundColor = "#2d2d2d") %>%
      formatStyle("Sig", color = "#69ff69", fontWeight = "bold")
  })
  
  # ============================================================================
  # Tab 8: Export
  # ============================================================================
  
  # Preview der Dateinamen
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
    cat(paste0("ZIP-Datei: ", scenario_name, "_export.zip"))
  })
  
  # Download Handler
  output$download_all <- downloadHandler(
    filename = function() {
      paste0(toupper(input$scenario), "_export.zip")
    },
    content = function(file) {
      # Temporäres Verzeichnis erstellen
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
          individual = function() plot_individual_profiles(study_data()$long),
          lzo_dist = function() plot_lzo_distribution(study_data()$wide),
          diff_density = function() plot_difference_density(study_data()$wide)
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
      }
      
      # ZIP erstellen
      zip::zip(file, files = basename(files_to_zip), root = export_dir)
    }
  )
}

shinyApp(ui = ui, server = server)