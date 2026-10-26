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

# ---------------------------------------------------------------------------
# Module 2: diagnostic tests
# Colour semantics: orange = disease present (dark = detected / TP, light =
# missed / FN); petrol = disease absent (dark = correctly negative / TN, light =
# false alarm / FP). Same hex values are used in assets/epir-widgets.js.
# ---------------------------------------------------------------------------
epir_dx_cols <- c(
  TP = "#d9822b",   # krank, Test positiv (richtig positiv)
  FN = "#f3cfa6",   # krank, Test negativ (falsch negativ)
  FP = "#9fd0d0",   # gesund, Test positiv (falsch positiv)
  TN = "#0b6e6e"    # gesund, Test negativ (richtig negativ)
)
epir_dx_labels <- c(
  TP = "krank, Test positiv (richtig positiv)",
  FN = "krank, Test negativ (falsch negativ)",
  FP = "gesund, Test positiv (falsch positiv)",
  TN = "gesund, Test negativ (richtig negativ)"
)

# German percent formatting for figure labels ("79,2 %")
fmt_pct_de <- function(x, digits = 1) paste0(formatC(100 * x, format = "f", digits = digits, decimal.mark = ","), "\u00a0%")

# Icon array of the whole cohort: one tile per person, grouped by disease status
# (columns of the 2x2 table) so that sensitivity/specificity are visible as
# proportions *within* the orange and the petrol block.
# counts: named vector c(TP, FN, FP, TN); ncol: tiles per row.
plot_dx_icon_array <- function(counts, nrow = 25, gap = 3) {
  stopifnot(all(c("TP", "FN", "FP", "TN") %in% names(counts)))
  # left block: persons with the disease (TP then FN), right block: without (FP then TN);
  # both blocks are filled column by column from the top, so the test errors (light tones)
  # sit at the bottom of each block and the proportion within a block is sensitivity/specificity.
  block <- function(keys, x0) {
    st <- factor(rep(keys, times = counts[keys]), levels = c("TP", "FN", "FP", "TN"))
    i <- seq_along(st) - 1
    tibble::tibble(status = st, x = x0 + i %/% nrow, y = -(i %% nrow))
  }
  n_sick <- sum(counts[c("TP", "FN")]); n_well <- sum(counts[c("FP", "TN")])
  cols_sick <- ceiling(n_sick / nrow)
  df <- dplyr::bind_rows(block(c("TP", "FN"), 0), block(c("FP", "TN"), cols_sick + gap))
  n <- nrow(df)
  ggplot2::ggplot(df, ggplot2::aes(x, y, fill = status)) +
    ggplot2::geom_tile(width = 0.85, height = 0.85) +
    ggplot2::scale_fill_manual(values = epir_dx_cols, labels = epir_dx_labels, name = NULL, drop = FALSE) +
    ggplot2::annotate("text", x = -0.5, y = 1.8, hjust = 0, label = sprintf("mit Albuminurie (%d)", n_sick),
                      colour = epir_dx_cols[["TP"]], fontface = "bold", size = 3.6) +
    ggplot2::annotate("text", x = cols_sick + gap + (ceiling(n_well / nrow) - 1) / 2, y = 1.8,
                      label = sprintf("ohne Albuminurie (%s)", format(n_well, big.mark = "\u202f")),
                      colour = epir_dx_cols[["TN"]], fontface = "bold", size = 3.6) +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::labs(
      title = sprintf("%s Personen der Kohorte, jede Kachel eine Person", format(n, big.mark = "\u202f")),
      subtitle = "links krank, rechts gesund – helle Töne: Testergebnis falsch"
    ) +
    epir_theme() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(), legend.position = "bottom"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2))
}

# Natural-frequency tree: N -> sick / not sick -> test result.
# counts as above; labels for test and disease are German learning text.
plot_dx_tree <- function(counts, disease = "Albuminurie", test = "Teststreifen") {
  stopifnot(all(c("TP", "FN", "FP", "TN") %in% names(counts)))
  tp <- counts[["TP"]]; fn <- counts[["FN"]]; fp <- counts[["FP"]]; tn <- counts[["TN"]]
  n <- tp + fn + fp + tn
  f <- function(x) format(x, big.mark = "\u202f")
  nodes <- tibble::tibble(
    id = c("root", "sick", "well", "TP", "FN", "FP", "TN"),
    x = c(0, -1.6, 1.6, -2.4, -0.8, 0.8, 2.4),
    y = c(3, 2, 2, 1, 1, 1, 1),
    label = c(
      sprintf("%s Personen", f(n)),
      sprintf("%s mit %s", f(tp + fn), disease),
      sprintf("%s ohne %s", f(fp + tn), disease),
      sprintf("%s Test positiv\n(richtig positiv)", f(tp)),
      sprintf("%s Test negativ\n(falsch negativ)", f(fn)),
      sprintf("%s Test positiv\n(falsch positiv)", f(fp)),
      sprintf("%s Test negativ\n(richtig negativ)", f(tn))
    ),
    fill = c("#ffffff", epir_dx_cols[["TP"]], epir_dx_cols[["TN"]], epir_dx_cols[["TP"]], epir_dx_cols[["FN"]], epir_dx_cols[["FP"]], epir_dx_cols[["TN"]]),
    txt = c("#333333", "#ffffff", "#ffffff", "#ffffff", "#333333", "#333333", "#ffffff")
  )
  edges <- tibble::tibble(
    from = c("root", "root", "sick", "sick", "well", "well"),
    to = c("sick", "well", "TP", "FN", "FP", "TN")
  )
  edges <- edges |>
    dplyr::left_join(nodes[, c("id", "x", "y")], by = c("from" = "id")) |>
    dplyr::left_join(nodes[, c("id", "x", "y")], by = c("to" = "id"), suffix = c("", "_to"))
  arrows <- tibble::tibble(
    x = c(-1.6, 1.6), y = c(0.45, 0.45),
    label = c(sprintf("Sensitivität = %s / %s = %s", f(tp), f(tp + fn), fmt_pct_de(tp / (tp + fn))),
              sprintf("Spezifität = %s / %s = %s", f(tn), f(fp + tn), fmt_pct_de(tn / (fp + tn)))),
    col = c(epir_dx_cols[["TP"]], epir_dx_cols[["TN"]])
  )
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = edges, ggplot2::aes(x = x, y = y - 0.22, xend = x_to, yend = y_to + 0.22), colour = "#666666", linewidth = 0.6) +
    ggplot2::geom_label(data = nodes, ggplot2::aes(x, y, label = label, fill = fill, colour = txt),
                        size = 3.4, label.padding = grid::unit(0.35, "lines"), linewidth = 0.3, lineheight = 0.95) +
    ggplot2::geom_text(data = arrows, ggplot2::aes(x, y, label = label, colour = col), size = 3.4, fontface = "bold") +
    ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(limits = c(-3.3, 3.3)) +
    ggplot2::scale_y_continuous(limits = c(0.2, 3.4)) +
    ggplot2::labs(title = "Häufigkeitsbaum: erst Krankheit, dann Testergebnis",
                  subtitle = "Lesen von oben nach unten ergibt Sensitivität und Spezifität (Spalten der Vierfeldertafel)") +
    epir_theme() +
    ggplot2::theme(axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(), legend.position = "none")
}

# Column-wise vs row-wise reading of the diagnostic 2x2 table (two panels).
plot_dx_reading <- function(counts) {
  stopifnot(all(c("TP", "FN", "FP", "TN") %in% names(counts)))
  tp <- counts[["TP"]]; fn <- counts[["FN"]]; fp <- counts[["FP"]]; tn <- counts[["TN"]]
  f <- function(x) format(x, big.mark = "\u202f")
  cells <- tibble::tibble(
    key = c("TP", "FP", "FN", "TN"),
    x = c(1, 2, 1, 2),      # column: 1 = krank, 2 = gesund
    y = c(2, 2, 1, 1),      # row: 2 = Test positiv, 1 = Test negativ
    n = c(tp, fp, fn, tn)
  )
  cells$label <- sprintf("%s\n%s", c("RP", "FP", "FN", "RN"), f(cells$n))
  panel <- function(mode) {
    hl <- if (mode == "col") {
      tibble::tibble(xmin = c(0.5, 1.5), xmax = c(1.5, 2.5), ymin = 0.5, ymax = 2.5,
                     txt = c(sprintf("Sensitivität\n%s / %s\n= %s", f(tp), f(tp + fn), fmt_pct_de(tp / (tp + fn))),
                             sprintf("Spezifität\n%s / %s\n= %s", f(tn), f(fp + tn), fmt_pct_de(tn / (fp + tn)))),
                     tx = c(1, 2), ty = 2.95)
    } else {
      tibble::tibble(xmin = 0.5, xmax = 2.5, ymin = c(1.5, 0.5), ymax = c(2.5, 1.5),
                     txt = c(sprintf("PPV\n%s / %s\n= %s", f(tp), f(tp + fp), fmt_pct_de(tp / (tp + fp))),
                             sprintf("NPV\n%s / %s\n= %s", f(tn), f(fn + tn), fmt_pct_de(tn / (fn + tn)))),
                     tx = 2.75, ty = c(2, 1))
    }
    ttl <- if (mode == "col") "Spaltenweise lesen (Testgüte):\nWie gut erkennt der Test?" else "Zeilenweise lesen (Vorhersagewerte):\nWas bedeutet mein Ergebnis?"
    ggplot2::ggplot() +
      ggplot2::geom_rect(data = hl, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                         fill = NA, colour = epir_cols[["dark"]], linewidth = 1.1, linetype = "dashed") +
      ggplot2::geom_tile(data = cells, ggplot2::aes(x, y, fill = key), width = 0.9, height = 0.9) +
      ggplot2::geom_text(data = cells, ggplot2::aes(x, y, label = label, colour = key %in% c("TP", "TN")), size = 3.8, lineheight = 0.9, fontface = "bold") +
      ggplot2::geom_text(data = hl, ggplot2::aes(tx, ty, label = txt), size = 3.3, lineheight = 0.9, hjust = if (mode == "col") 0.5 else 0) +
      ggplot2::scale_fill_manual(values = epir_dx_cols, guide = "none") +
      ggplot2::scale_colour_manual(values = c(`TRUE` = "#ffffff", `FALSE` = "#333333"), guide = "none") +
      ggplot2::scale_x_continuous(breaks = c(1, 2), labels = c("Albuminurie", "keine\nAlbuminurie"),
                                  limits = if (mode == "col") c(0.4, 2.6) else c(0.4, 3.9), position = "top") +
      ggplot2::scale_y_continuous(breaks = c(1, 2), labels = c("Test\nnegativ", "Test\npositiv"), limits = c(0.4, if (mode == "col") 3.4 else 2.6)) +
      ggplot2::labs(title = ttl, x = NULL, y = NULL) +
      epir_theme(base_size = 12) +
      ggplot2::theme(panel.grid = ggplot2::element_blank(), plot.title = ggplot2::element_text(size = 11.5))
  }
  patchwork::wrap_plots(panel("col"), panel("row"), ncol = 2, widths = c(1, 1.35))
}

# PPV and NPV as a function of prevalence for fixed sensitivity/specificity;
# `mark` = named vector of prevalences (proportions) to highlight.
plot_dx_ppv_curve <- function(sens, spez, mark = NULL, max_prev = 0.6, nudge = NULL) {
  pr <- seq(0.001, max_prev, by = 0.001)
  df <- tibble::tibble(
    prev = rep(pr, 2),
    value = c(sens * pr / (sens * pr + (1 - spez) * (1 - pr)),
              spez * (1 - pr) / (spez * (1 - pr) + (1 - sens) * pr)),
    measure = rep(c("Positiver Vorhersagewert (PPV)", "Negativer Vorhersagewert (NPV)"), each = length(pr))
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(prev, value, colour = measure)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::scale_colour_manual(values = c("Positiver Vorhersagewert (PPV)" = epir_cols[["accent"]],
                                            "Negativer Vorhersagewert (NPV)" = epir_cols[["primary"]]), name = NULL) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(format(100 * x, decimal.mark = ","), " %")) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(100 * x, " %"), limits = c(0, 1)) +
    ggplot2::labs(x = "Prävalenz (Vortestwahrscheinlichkeit)", y = NULL,
                  title = sprintf("Gleicher Test (Sensitivität %s, Spezifität %s), verschiedene Gruppen", fmt_pct_de(sens), fmt_pct_de(spez)),
                  subtitle = "PPV steigt mit der Prävalenz, NPV sinkt – Sensitivität und Spezifität bleiben gleich") +
    epir_theme()
  if (!is.null(mark)) {
    md <- tibble::tibble(
      prev = as.numeric(mark), name = names(mark),
      ppv = sens * prev / (sens * prev + (1 - spez) * (1 - prev)),
      ny = if (is.null(nudge)) rep(-0.14, length(mark)) else nudge
    )
    p <- p +
      ggplot2::geom_vline(data = md, ggplot2::aes(xintercept = prev), colour = epir_cols[["grey"]], linetype = "dotted") +
      ggplot2::geom_point(data = md, ggplot2::aes(prev, ppv), colour = epir_cols[["accent"]], size = 3, inherit.aes = FALSE) +
      ggplot2::geom_label(data = md, ggplot2::aes(prev, ppv + ny, label = sprintf("%s\nPPV %s", name, fmt_pct_de(ppv, 0))),
                          size = 3.1, lineheight = 0.9, inherit.aes = FALSE, colour = epir_cols[["dark"]])
  }
  p
}
