# Plot helpers for static figures rendered at build time (knitr, ggplot2).
# License: MIT. Figures use the synthetic cohort or purely schematic data.
# All labels are German (learning content).

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

epir_cols <- c(
  primary = "#0b6e6e",   # teal accent (matches styles.scss $primary)
  accent  = "#d9822b",   # orange
  blue    = "#4a6fa5",
  grey    = "#9aa5b1",
  light   = "#e3e8ec",
  dark    = "#333333"
)

epir_theme <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = base_size + 1),
      plot.subtitle = element_text(colour = "#555555"),
      plot.caption = element_text(colour = "#777777", size = base_size - 3, hjust = 0),
      legend.position = "bottom",
      legend.title = element_blank(),
      strip.text = element_text(face = "bold")
    )
}

# --- Icon array: "x of 100 persons" -----------------------------------------
# cases: named numeric vector, one panel per name, each value = cases per 100
plot_icon_array <- function(cases, label_case = "erkrankt", label_no = "nicht erkrankt") {
  grid <- expand.grid(col = 1:10, row = 1:10)
  dat <- bind_rows(lapply(names(cases), function(nm) {
    k <- round(cases[[nm]])
    grid |>
      mutate(
        idx = (row - 1) * 10 + col,
        status = if_else(idx <= k, label_case, label_no),
        panel = nm
      )
  }))
  dat$panel <- factor(dat$panel, levels = names(cases))
  dat$status <- factor(dat$status, levels = c(label_case, label_no))
  ggplot(dat, aes(x = col, y = -row, fill = status)) +
    geom_tile(width = 0.85, height = 0.85, colour = NA) +
    facet_wrap(~panel) +
    scale_fill_manual(values = setNames(c(epir_cols[["primary"]], epir_cols[["light"]]), c(label_case, label_no))) +
    coord_equal() +
    epir_theme() +
    theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank())
}

# --- Schematic: point vs. period prevalence ----------------------------------
# Purely schematic episodes (no data). Time axis in months of one year.
plot_point_vs_period <- function(stichtag = 7, periode = c(4, 9)) {
  episodes <- tribble(
    ~person, ~start, ~ende,
    1,  1.0,  3.0,    # vor Stichtag und vor Periode: zählt nirgends
    2,  2.0,  8.0,    # kreuzt Stichtag: Punkt + Periode
    3,  5.0,  6.0,    # nur in Periode, nicht am Stichtag
    4,  6.5, 12.0,    # kreuzt Stichtag: Punkt + Periode
    5,  8.5, 11.0,    # nach Stichtag, in Periode
    6, 10.0, 12.0,    # nach Periode: zählt nirgends
    7,  0.5, 12.0,    # chronisch, ganzes Jahr: Punkt + Periode
    8,  3.0,  3.8     # kurz vor Periode: zählt nirgends
  ) |>
    mutate(
      am_stichtag = start <= stichtag & ende >= stichtag,
      in_periode  = start <= periode[2] & ende >= periode[1],
      kategorie = case_when(
        am_stichtag ~ "am Stichtag krank (Punkt- und Periodenprävalenz)",
        in_periode  ~ "nur im Zeitraum krank (nur Periodenprävalenz)",
        TRUE        ~ "außerhalb (zählt nicht)"
      )
    )
  ggplot(episodes) +
    annotate("rect", xmin = periode[1], xmax = periode[2], ymin = 0.4, ymax = 8.6,
             fill = epir_cols[["blue"]], alpha = 0.10) +
    geom_segment(aes(x = start, xend = ende, y = person, yend = person, colour = kategorie), linewidth = 4, lineend = "round") +
    geom_vline(xintercept = stichtag, colour = epir_cols[["accent"]], linewidth = 1.1, linetype = "dashed") +
    annotate("label", x = stichtag, y = 8.9, label = "Stichtag (Punktprävalenz)", colour = epir_cols[["accent"]], size = 3.6) +
    annotate("label", x = mean(periode), y = 0.1, label = "Zeitraum (Periodenprävalenz)", colour = epir_cols[["blue"]], size = 3.6) +
    scale_colour_manual(values = c(
      "am Stichtag krank (Punkt- und Periodenprävalenz)" = epir_cols[["primary"]],
      "nur im Zeitraum krank (nur Periodenprävalenz)" = epir_cols[["blue"]],
      "außerhalb (zählt nicht)" = epir_cols[["grey"]]
    )) +
    scale_x_continuous(breaks = c(1, 4, 7, 10, 12), labels = c("Jan", "Apr", "Jul", "Okt", "Dez"), limits = c(0, 12.5)) +
    scale_y_continuous(breaks = 1:8, labels = paste("Person", 1:8), limits = c(-0.2, 9.2)) +
    labs(x = NULL, y = NULL) +
    epir_theme() +
    guides(colour = guide_legend(nrow = 3)) +
    theme(panel.grid.major.y = element_blank())
}

# --- Swimmer plot: follow-up of a sample of persons -------------------------
plot_followup <- function(kohorte, n_prev = 4, n_case = 3, n_cens = 3, n_full = 14, seed = 1) {
  # Stratified selection so that all four follow-up patterns are visible.
  # With a purely random draw of ~20 persons the incident cases (~8 %) are
  # often missing, which makes the legend confusing.
  set.seed(seed)
  prev <- kohorte |> filter(ckd_baseline == 1) |> slice_sample(n = n_prev) |>
    mutate(status = "CKD schon zu Beginn (nicht unter Risiko)", ende = 0.08)   # short stub so the legend key is drawn
  at_risk <- kohorte |> filter(ckd_baseline == 0) |>
    mutate(
      ende = followup_jahre,
      status = case_when(
        ckd_inzident == 1 ~ "neue CKD (Fall)",
        followup_jahre >= 5 - 1e-9 ~ "5 Jahre beobachtet, keine CKD",
        TRUE ~ "vorzeitig ausgeschieden (zensiert)"
      )
    )
  risk <- bind_rows(
    at_risk |> filter(status == "neue CKD (Fall)") |> slice_sample(n = n_case),
    at_risk |> filter(status == "vorzeitig ausgeschieden (zensiert)") |> slice_sample(n = n_cens),
    at_risk |> filter(status == "5 Jahre beobachtet, keine CKD") |> slice_sample(n = n_full)
  )
  dat <- bind_rows(prev, risk) |>
    mutate(status = factor(status, levels = c(
      "neue CKD (Fall)", "5 Jahre beobachtet, keine CKD",
      "vorzeitig ausgeschieden (zensiert)", "CKD schon zu Beginn (nicht unter Risiko)"
    ))) |>
    arrange(status, desc(ende)) |>
    mutate(pos = rev(row_number()))
  cols <- c(
    "neue CKD (Fall)" = epir_cols[["accent"]],
    "5 Jahre beobachtet, keine CKD" = epir_cols[["primary"]],
    "vorzeitig ausgeschieden (zensiert)" = epir_cols[["blue"]],
    "CKD schon zu Beginn (nicht unter Risiko)" = epir_cols[["grey"]]
  )
  ggplot(dat, aes(y = pos, colour = status)) +
    geom_segment(aes(x = 0, xend = ende, yend = pos), linewidth = 2, lineend = "round") +
    geom_point(data = filter(dat, status == "neue CKD (Fall)"), aes(x = ende), shape = 4, size = 3.2, stroke = 1.6) +
    geom_point(data = filter(dat, status == "vorzeitig ausgeschieden (zensiert)"), aes(x = ende), shape = 21, fill = "white", size = 3, stroke = 1.2) +
    geom_vline(xintercept = 0, colour = epir_cols[["dark"]], linewidth = 0.5) +
    geom_vline(xintercept = 5, colour = epir_cols[["dark"]], linewidth = 0.5, linetype = "dotted") +
    scale_colour_manual(values = cols, drop = FALSE) +
    scale_x_continuous(breaks = 0:5, limits = c(-0.3, 5.3)) +
    scale_y_continuous(breaks = NULL) +
    labs(x = "Jahre seit Studienbeginn (Screening)", y = "je eine Linie = eine Person") +
    epir_theme() +
    guides(colour = guide_legend(nrow = 2, override.aes = list(linewidth = 2, shape = NA))) +
    theme(panel.grid.major.y = element_blank())
}

# --- Person-time: nominal vs. actual ----------------------------------------
plot_person_time <- function(n_risk, pj_actual, years = 5) {
  dat <- tibble(
    was = factor(c("Wenn alle 5 Jahre beobachtet worden wären\n(1638 Personen × 5 Jahre)", "Tatsächliche Personenzeit unter Risiko\n(Summe der Linienlängen)"),
                 levels = rev(c("Wenn alle 5 Jahre beobachtet worden wären\n(1638 Personen × 5 Jahre)", "Tatsächliche Personenzeit unter Risiko\n(Summe der Linienlängen)"))),
    pj = c(n_risk * years, pj_actual)
  )
  ggplot(dat, aes(x = pj, y = was, fill = was)) +
    geom_col(width = 0.55) +
    geom_text(aes(label = paste(format(round(pj), big.mark = " "), "Personenjahre")), hjust = 1.1, colour = "white", fontface = "bold", size = 4) +
    scale_fill_manual(values = c(epir_cols[["primary"]], epir_cols[["grey"]])) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Personenjahre", y = NULL) +
    epir_theme() +
    theme(legend.position = "none", panel.grid.major.y = element_blank())
}

# ============================================================================
# Module 3: relative and absolute risk
# ============================================================================

# --- Icon array with "baseline" vs. "extra" cases ---------------------------
# risks: named numeric vector of risks (0-1), first element = reference group.
# Cells up to the reference risk are coloured as cases in both panels; cells
# between reference and exposed risk are highlighted as "zusätzlich".
plot_icon_array_pair <- function(risks, label_case = "erkranken", label_extra = "zusätzlich erkranken (durch die Exposition)",
                                 label_no = "erkranken nicht") {
  k0 <- round(100 * risks[[1]])
  grid <- expand.grid(col = 1:10, row = 1:10)
  dat <- bind_rows(lapply(names(risks), function(nm) {
    k <- round(100 * risks[[nm]])
    grid |> mutate(
      idx = (row - 1) * 10 + col,
      status = case_when(idx <= min(k, k0) ~ label_case, idx <= k ~ label_extra, TRUE ~ label_no),
      panel = sprintf("%s: %d von 100", nm, k)
    )
  }))
  dat$panel <- factor(dat$panel, levels = unique(dat$panel))
  dat$status <- factor(dat$status, levels = c(label_case, label_extra, label_no))
  ggplot(dat, aes(x = col, y = -row, fill = status)) +
    geom_tile(width = 0.85, height = 0.85, colour = NA) +
    facet_wrap(~panel) +
    scale_fill_manual(values = setNames(c(epir_cols[["primary"]], epir_cols[["accent"]], epir_cols[["light"]]),
                                        c(label_case, label_extra, label_no)), drop = FALSE) +
    coord_equal() +
    epir_theme() +
    theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank())
}

# --- Relative vs. absolute: same RR, different baseline risks ---------------
plot_rel_vs_abs <- function(base_risks = c(0.01, 0.069), rr = 2.54, labels = c("seltenes Ereignis", "Kohorte: CKD bei Diabetes")) {
  dat <- tibble(
    szenario = factor(sprintf("%s\nBasisrisiko %s %%", labels, format(round(100 * base_risks, 1), decimal.mark = ",")), levels = rev(sprintf("%s\nBasisrisiko %s %%", labels, format(round(100 * base_risks, 1), decimal.mark = ",")))),
    ohne = base_risks,
    mit = base_risks * rr,
    diff = base_risks * rr - base_risks
  )
  long <- dat |> tidyr::pivot_longer(c(ohne, mit), names_to = "gruppe", values_to = "risiko") |>
    mutate(gruppe = factor(if_else(gruppe == "ohne", "ohne Exposition", "mit Exposition"), levels = c("ohne Exposition", "mit Exposition")))
  ggplot() +
    geom_segment(data = dat, aes(x = ohne, xend = mit, y = szenario, yend = szenario), colour = epir_cols[["accent"]], linewidth = 2.2) +
    geom_point(data = long, aes(x = risiko, y = szenario, colour = gruppe), size = 5) +
    geom_text(data = dat, aes(x = ohne, y = szenario, label = sprintf("+%s Prozentpunkte", trimws(format(round(100 * diff, 1), decimal.mark = ",")))),
              hjust = 0, vjust = -1.1, colour = epir_cols[["accent"]], fontface = "bold", size = 4) +
    scale_colour_manual(values = c("ohne Exposition" = epir_cols[["primary"]], "mit Exposition" = epir_cols[["dark"]])) +
    scale_x_continuous(labels = function(x) paste0(format(100 * x, decimal.mark = ","), " %"), limits = c(0, max(dat$mit) * 1.15), expand = expansion(mult = c(0, 0.02))) +
    labs(x = sprintf("Risiko in 5 Jahren (beide Szenarien: RR = %s)", format(rr, decimal.mark = ",")), y = NULL) +
    epir_theme() +
    theme(panel.grid.major.y = element_blank())
}

# --- OR versus RR for different baseline risks ------------------------------
plot_or_vs_rr <- function(base_risks = c(0.01, 0.069, 0.20), rr_max = 4, mark = NULL) {
  dat <- tidyr::expand_grid(r0 = base_risks, rr = seq(1, rr_max, by = 0.01)) |>
    filter(r0 * rr < 1) |>
    mutate(r1 = r0 * rr, or = (r1 / (1 - r1)) / (r0 / (1 - r0)),
           basis = factor(sprintf("Basisrisiko %s %%", format(100 * r0, decimal.mark = ",")),
                          levels = sprintf("Basisrisiko %s %%", format(100 * base_risks, decimal.mark = ","))))
  p <- ggplot(dat, aes(x = rr, y = or, colour = basis)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = epir_cols[["grey"]]) +
    geom_line(linewidth = 1.3) +
    annotate("text", x = 2.1, y = 2.1, label = "OR = RR (Diagonale)", hjust = 0, vjust = 2.2, colour = epir_cols[["grey"]], size = 3.6) +
    scale_colour_manual(values = unname(epir_cols[c("primary", "blue", "accent")])) +
    labs(x = "Relatives Risiko (RR)", y = "Odds Ratio (OR)") +
    coord_cartesian(xlim = c(1, rr_max), ylim = c(1, max(dat$or, na.rm = TRUE))) +
    epir_theme()
  if (!is.null(mark)) {
    p <- p + annotate("point", x = mark[["rr"]], y = mark[["or"]], size = 4, shape = 21, fill = "white", colour = epir_cols[["dark"]], stroke = 1.4) +
      annotate("text", x = mark[["rr"]], y = mark[["or"]], label = mark[["label"]], hjust = -0.1, vjust = 1.4, size = 3.6, colour = epir_cols[["dark"]])
  }
  p
}

# --- Schematic: cohort vs. case-control reading of the 2x2 table ------------
plot_design_schema <- function() {
  boxes <- tribble(
    ~design, ~x, ~y, ~label, ~fill,
    "Kohortenstudie: vorwärts", 1, 2, "Exponierte\n(bekannte Zahl)", "primary",
    "Kohortenstudie: vorwärts", 1, 1, "Nicht-Exponierte\n(bekannte Zahl)", "primary",
    "Kohortenstudie: vorwärts", 3, 2, "Erkrankt?\n→ Risiko je Gruppe", "accent",
    "Kohortenstudie: vorwärts", 3, 1, "Erkrankt?\n→ Risiko je Gruppe", "accent",
    "Fall-Kontroll-Studie: rückwärts", 3, 2, "Fälle\n(Zahl gewählt)", "accent",
    "Fall-Kontroll-Studie: rückwärts", 3, 1, "Kontrollen\n(Zahl gewählt)", "accent",
    "Fall-Kontroll-Studie: rückwärts", 1, 2, "Exponiert?\n→ Odds der Exposition", "primary",
    "Fall-Kontroll-Studie: rückwärts", 1, 1, "Exponiert?\n→ Odds der Exposition", "primary"
  )
  arrows <- tribble(
    ~design, ~x, ~xend, ~y,
    "Kohortenstudie: vorwärts", 1.78, 2.22, 2,
    "Kohortenstudie: vorwärts", 1.78, 2.22, 1,
    "Fall-Kontroll-Studie: rückwärts", 2.22, 1.78, 2,
    "Fall-Kontroll-Studie: rückwärts", 2.22, 1.78, 1
  )
  result <- tribble(
    ~design, ~label,
    "Kohortenstudie: vorwärts", "Ergebnis: Risiken, RR, Risikodifferenz, NNT – und OR",
    "Fall-Kontroll-Studie: rückwärts", "Ergebnis: nur OR (Risiken nicht schätzbar)"
  )
  lv <- c("Kohortenstudie: vorwärts", "Fall-Kontroll-Studie: rückwärts")
  boxes$design <- factor(boxes$design, lv); arrows$design <- factor(arrows$design, lv); result$design <- factor(result$design, lv)
  ggplot() +
    geom_tile(data = boxes, aes(x = x, y = y, fill = fill), width = 1.5, height = 0.7, colour = NA) +
    geom_text(data = boxes, aes(x = x, y = y, label = label), colour = "white", size = 3.2, lineheight = 0.9, fontface = "bold") +
    geom_segment(data = arrows, aes(x = x, xend = xend, y = y, yend = y), arrow = arrow(length = unit(0.25, "cm"), type = "closed"), linewidth = 1, colour = epir_cols[["dark"]]) +
    geom_text(data = result, aes(x = 2, y = 0.25, label = label), size = 3.6, colour = epir_cols[["dark"]]) +
    annotate("text", x = 1, y = 2.6, label = "Exposition", size = 3.6, colour = "#555555") +
    annotate("text", x = 3, y = 2.6, label = "Erkrankung", size = 3.6, colour = "#555555") +
    facet_wrap(~design, ncol = 2) +
    scale_fill_manual(values = c(primary = epir_cols[["primary"]], accent = epir_cols[["accent"]]), guide = "none") +
    coord_cartesian(xlim = c(0.2, 3.8), ylim = c(0, 2.8)) +
    epir_theme() +
    theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank())
}
