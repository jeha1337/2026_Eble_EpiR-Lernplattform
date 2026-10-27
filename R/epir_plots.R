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

# ============================================================================
# Module 4: distributions, random variation, sample size
# Colour semantics: petrol = data / persons, orange = study estimates or
# marked value, blue = reference line (true value, cut-off), grey = model curve.
# ============================================================================

fmt_de <- function(x, digits = 1) formatC(x, format = "f", digits = digits, decimal.mark = ",", big.mark = "\u202f")

# --- Histogram with mean, median and +-1 / +-2 SD bands ---------------------
plot_hist_sd <- function(x, xlab, binwidth = 5, show_median = TRUE) {
  m <- mean(x); s <- sd(x); md <- median(x)
  bands <- data.frame(xmin = c(m - 2 * s, m - s), xmax = c(m + 2 * s, m + s),
                      band = factor(c("Mittelwert \u00b1 2 SD", "Mittelwert \u00b1 1 SD"),
                                    levels = c("Mittelwert \u00b1 1 SD", "Mittelwert \u00b1 2 SD")))
  p <- ggplot() +
    geom_rect(data = bands, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = band), alpha = 0.35) +
    scale_fill_manual(values = c("Mittelwert \u00b1 1 SD" = "#b7d8d8", "Mittelwert \u00b1 2 SD" = "#e3eeee")) +
    geom_histogram(data = data.frame(x = x), aes(x = x), binwidth = binwidth, boundary = 0,
                   fill = epir_cols[["primary"]], colour = "white", linewidth = 0.3) +
    geom_vline(xintercept = m, colour = epir_cols[["accent"]], linewidth = 1.1) +
    annotate("label", x = m, y = Inf, vjust = 1.3, hjust = 1.03, label = sprintf("Mittelwert %s", fmt_de(m)),
             colour = epir_cols[["accent"]], fill = "white", size = 3.6, linewidth = 0) +
    labs(x = xlab, y = "Anzahl Personen") +
    epir_theme()
  if (show_median) {
    p <- p + geom_vline(xintercept = md, colour = epir_cols[["blue"]], linetype = "dashed", linewidth = 0.9) +
      annotate("label", x = md, y = Inf, vjust = 1.3, hjust = -0.03, label = sprintf("Median %s", fmt_de(md)),
               colour = epir_cols[["blue"]], fill = "white", size = 3.6, linewidth = 0)
  }
  p
}

# --- Histogram (density scale) with fitted normal curve and 68/95 shading ----
plot_hist_normal <- function(x, xlab, binwidth = 5) {
  m <- mean(x); s <- sd(x)
  grid <- data.frame(x = seq(min(x) - 5, max(x) + 5, length.out = 400))
  grid$d <- dnorm(grid$x, m, s)
  shade <- function(a, b, lab) { g <- grid[grid$x >= a & grid$x <= b, ]; g$zone <- lab; g }
  zones <- rbind(shade(m - 2 * s, m + 2 * s, "\u2248 95 % innerhalb \u00b1 2 SD"),
                 shade(m - s, m + s, "\u2248 68 % innerhalb \u00b1 1 SD"))
  zones$zone <- factor(zones$zone, levels = c("\u2248 68 % innerhalb \u00b1 1 SD", "\u2248 95 % innerhalb \u00b1 2 SD"))
  ggplot() +
    geom_histogram(data = data.frame(x = x), aes(x = x, y = after_stat(density)), binwidth = binwidth, boundary = 0,
                   fill = epir_cols[["light"]], colour = "white", linewidth = 0.3) +
    geom_area(data = zones, aes(x = x, y = d, fill = zone), alpha = 0.55, position = "identity") +
    scale_fill_manual(values = setNames(c(epir_cols[["primary"]], "#9fd0d0"), levels(zones$zone))) +
    geom_line(data = grid, aes(x = x, y = d), colour = epir_cols[["dark"]], linewidth = 1) +
    geom_vline(xintercept = m, colour = epir_cols[["accent"]], linewidth = 1) +
    annotate("text", x = m, y = max(grid$d) * 1.08, label = "\u03bc", colour = epir_cols[["accent"]], size = 5) +
    annotate("segment", x = m, xend = m + s, y = max(grid$d) * 0.62, yend = max(grid$d) * 0.62,
             arrow = arrow(ends = "both", length = unit(0.12, "cm")), colour = epir_cols[["dark"]]) +
    annotate("text", x = m + s / 2, y = max(grid$d) * 0.68, label = "\u03c3", size = 5, colour = epir_cols[["dark"]]) +
    scale_y_continuous(breaks = NULL) +
    labs(x = xlab, y = NULL, caption = "Graue Balken: Histogramm der Kohorte. Kurve: Normalverteilung mit demselben Mittelwert \u03bc und derselben Standardabweichung \u03c3.") +
    epir_theme()
}

# --- Right-skewed variable on a linear and on a log10 axis (two panels) -----
plot_hist_log <- function(x, xlab, cutoff = NULL, cutoff_label = NULL) {
  m <- mean(x); md <- median(x)
  dat <- data.frame(x = x)
  mk <- function(p, trans) {
    p <- p + geom_vline(xintercept = m, colour = epir_cols[["accent"]], linewidth = 1) +
      geom_vline(xintercept = md, colour = epir_cols[["blue"]], linetype = "dashed", linewidth = 0.9)
    if (!is.null(cutoff)) p <- p + geom_vline(xintercept = cutoff, colour = epir_cols[["dark"]], linetype = "dotted", linewidth = 0.8)
    p + labs(y = "Anzahl Personen") + epir_theme()
  }
  lin <- mk(ggplot(dat, aes(x = x)) +
              geom_histogram(binwidth = 5, boundary = 0, fill = epir_cols[["primary"]], colour = "white", linewidth = 0.3) +
              labs(x = sprintf("%s – lineare Achse", xlab)) +
              annotate("label", x = m, y = Inf, vjust = 1.3, hjust = -0.05, label = sprintf("Mittelwert %s", fmt_de(m)), colour = epir_cols[["accent"]], fill = "white", size = 3.4, linewidth = 0) +
              annotate("label", x = md, y = Inf, vjust = 3.0, hjust = -0.05, label = sprintf("Median %s", fmt_de(md)), colour = epir_cols[["blue"]], fill = "white", size = 3.4, linewidth = 0))
  lg <- mk(ggplot(dat, aes(x = x)) +
             geom_histogram(bins = 40, fill = epir_cols[["primary"]], colour = "white", linewidth = 0.3) +
             scale_x_log10(breaks = c(1, 3, 10, 30, 100, 300), labels = function(b) fmt_de(b, 0)) +
             labs(x = sprintf("%s – log\u2081\u2080-Achse (gleiche Abst\u00e4nde = gleiche Faktoren)", xlab)) +
             annotate("label", x = m, y = Inf, vjust = 1.3, hjust = -0.05, label = "Mittelwert", colour = epir_cols[["accent"]], fill = "white", size = 3.4, linewidth = 0) +
             annotate("label", x = md, y = Inf, vjust = 1.3, hjust = 1.05, label = "Median", colour = epir_cols[["blue"]], fill = "white", size = 3.4, linewidth = 0))
  if (!is.null(cutoff_label)) lg <- lg + annotate("label", x = cutoff, y = Inf, vjust = 3.0, hjust = -0.05, label = cutoff_label, colour = epir_cols[["dark"]], fill = "white", size = 3.2, linewidth = 0)
  patchwork_free_stack(lin, lg)
}

# Stack two ggplots vertically without extra dependencies (gridExtra/patchwork
# are not required): uses grid viewports.
patchwork_free_stack <- function(top, bottom) {
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 1)))
  print(top, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(bottom, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
  invisible(NULL)
}

# --- Boxplots by group (two variables side by side) -------------------------
plot_box_by_group <- function(kohorte) {
  dat <- kohorte |>
    transmute(gruppe = if_else(diabetes == 1, "mit Diabetes", "ohne Diabetes"),
              egfr = egfr, uacr = uacr) |>
    tidyr::pivot_longer(-gruppe, names_to = "variable", values_to = "wert")
  dat$gruppe <- factor(dat$gruppe, levels = c("ohne Diabetes", "mit Diabetes"))
  p1 <- ggplot(dplyr::filter(dat, variable == "egfr"), aes(x = gruppe, y = wert, fill = gruppe)) +
    geom_boxplot(width = 0.5, outlier.size = 0.9, outlier.alpha = 0.5) +
    scale_fill_manual(values = c("ohne Diabetes" = "#9fd0d0", "mit Diabetes" = "#f3cfa6"), guide = "none") +
    labs(x = NULL, y = "eGFR (mL/min/1,73 m\u00b2)") + epir_theme()
  p2 <- ggplot(dplyr::filter(dat, variable == "uacr"), aes(x = gruppe, y = wert, fill = gruppe)) +
    geom_boxplot(width = 0.5, outlier.size = 0.9, outlier.alpha = 0.5) +
    geom_hline(yintercept = 30, linetype = "dotted", colour = epir_cols[["dark"]]) +
    scale_y_log10(breaks = c(1, 3, 10, 30, 100, 300), labels = function(b) fmt_de(b, 0)) +
    scale_fill_manual(values = c("ohne Diabetes" = "#9fd0d0", "mit Diabetes" = "#f3cfa6"), guide = "none") +
    labs(x = NULL, y = "UACR (mg/g), log\u2081\u2080-Achse") + epir_theme()
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
  print(p1, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p2, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
  invisible(NULL)
}

# --- Sampling distribution of a prevalence estimate for several n -----------
# est: named list of numeric vectors (estimates as proportions), names = "n = 50" etc.
plot_sampling_dist <- function(est, true_p) {
  dat <- bind_rows(lapply(names(est), function(nm) data.frame(n = nm, p = 100 * est[[nm]])))
  dat$n <- factor(dat$n, levels = names(est))
  sds <- dat |> group_by(n) |> summarise(sd = sd(p), .groups = "drop") |>
    mutate(lab = sprintf("%s: Streuung (SD) %s Prozentpunkte", n, fmt_de(sd)))
  dat$n <- factor(dat$n, levels = names(est), labels = sds$lab)
  ggplot(dat, aes(x = p)) +
    geom_histogram(binwidth = 1, boundary = 0, fill = epir_cols[["accent"]], colour = "white", linewidth = 0.3) +
    geom_vline(xintercept = 100 * true_p, colour = epir_cols[["blue"]], linewidth = 1) +
    facet_wrap(~n, ncol = 1, scales = "free_y") +
    scale_y_continuous(breaks = NULL) +
    labs(x = "Gesch\u00e4tzte Pr\u00e4valenz in der einzelnen Studie (%)", y = "Anzahl Studien",
         caption = sprintf("Blaue Linie: wahre Pr\u00e4valenz %s %%. Je Reihe 1000 simulierte Studien.", fmt_de(100 * true_p))) +
    epir_theme() + theme(strip.text = element_text(hjust = 0))
}

# --- SD (persons) versus SE (study means) on the same axis ------------------
plot_sd_vs_se <- function(x, means, xlab) {
  dat <- bind_rows(
    data.frame(wert = x, was = "Einzelwerte der Personen (Streuung = SD)"),
    data.frame(wert = means, was = sprintf("Mittelwerte von %d Studien \u00e0 n = 100 (Streuung = Standardfehler)", length(means)))
  )
  dat$was <- factor(dat$was, levels = unique(dat$was))
  ggplot(dat, aes(x = wert, fill = was)) +
    geom_histogram(binwidth = 1, boundary = 0, colour = "white", linewidth = 0.2) +
    facet_wrap(~was, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = c(epir_cols[["primary"]], epir_cols[["accent"]]), guide = "none") +
    geom_vline(xintercept = mean(x), colour = epir_cols[["blue"]], linewidth = 0.9) +
    scale_y_continuous(breaks = NULL) +
    labs(x = xlab, y = NULL) +
    epir_theme() + theme(strip.text = element_text(hjust = 0))
}

# --- Schematic: random error versus systematic error (four targets) ---------
plot_target_schema <- function(seed = 4) {
  set.seed(seed)
  panels <- data.frame(
    panel = c("pr\u00e4zise und richtig", "pr\u00e4zise, aber verzerrt", "unpr\u00e4zise, aber richtig", "unpr\u00e4zise und verzerrt"),
    spread = c(0.12, 0.12, 0.45, 0.45), dx = c(0, 0.9, 0, 0.9), dy = c(0, 0.5, 0, 0.5)
  )
  panels$panel <- factor(panels$panel, levels = panels$panel)
  shots <- bind_rows(lapply(seq_len(nrow(panels)), function(i) {
    data.frame(panel = panels$panel[i],
               x = panels$dx[i] + rnorm(12, 0, panels$spread[i]),
               y = panels$dy[i] + rnorm(12, 0, panels$spread[i]))
  }))
  rings <- bind_rows(lapply(c(1.6, 1.2, 0.8, 0.4), function(r) {
    t <- seq(0, 2 * pi, length.out = 120)
    bind_rows(lapply(levels(panels$panel), function(p) data.frame(panel = factor(p, levels(panels$panel)), x = r * cos(t), y = r * sin(t), r = r)))
  }))
  ggplot() +
    geom_path(data = rings, aes(x = x, y = y, group = r), colour = epir_cols[["grey"]], linewidth = 0.5) +
    geom_point(data = data.frame(x = 0, y = 0), aes(x, y), colour = epir_cols[["blue"]], size = 3.5, shape = 3, stroke = 1.2) +
    geom_point(data = shots, aes(x = x, y = y), colour = epir_cols[["accent"]], size = 2.4, alpha = 0.9) +
    facet_wrap(~panel, nrow = 1) +
    coord_equal(xlim = c(-1.7, 1.7), ylim = c(-1.7, 1.7)) +
    labs(caption = "Blaues Kreuz: wahrer Wert. Orange Punkte: Ergebnisse einzelner Studien.") +
    epir_theme() +
    theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank())
}

# ---------------------------------------------------------------------------
# Module 5a: study designs and bias
# Colour semantics as above: orange = case/event, petrol = healthy/reference,
# blue = censored, grey = not applicable. Schematic figures carry no data.
# ---------------------------------------------------------------------------

# Design map: who assigns the exposure (rows) x direction of observation (cols)
plot_design_map <- function() {
  cells <- tribble(
    ~zuteilung, ~zeit, ~design, ~mass, ~fill,
    "das Leben\n(Beobachtung)", "Momentaufnahme", "Querschnittsstudie", "Pr\u00e4valenz,\nPr\u00e4valenzverh\u00e4ltnis", "primary",
    "das Leben\n(Beobachtung)", "vorw\u00e4rts:\nExposition \u2192 Erkrankung", "Kohortenstudie", "Risiko, Inzidenzrate,\nRR, Risikodifferenz", "primary",
    "das Leben\n(Beobachtung)", "r\u00fcckw\u00e4rts:\nErkrankung \u2192 Exposition", "Fall-Kontroll-Studie", "nur Odds Ratio", "accent",
    "die Forschenden\n(per Zufall)", "Momentaufnahme", "\u2013", "nicht m\u00f6glich", "grey",
    "die Forschenden\n(per Zufall)", "vorw\u00e4rts:\nExposition \u2192 Erkrankung", "Randomisierte\nkontrollierte Studie (RCT)", "Risiko, RR,\nRisikodifferenz, NNT", "blue",
    "die Forschenden\n(per Zufall)", "r\u00fcckw\u00e4rts:\nErkrankung \u2192 Exposition", "\u2013", "nicht m\u00f6glich", "grey"
  )
  cells$zuteilung <- factor(cells$zuteilung, levels = c("die Forschenden\n(per Zufall)", "das Leben\n(Beobachtung)"))
  cells$zeit <- factor(cells$zeit, levels = c("r\u00fcckw\u00e4rts:\nErkrankung \u2192 Exposition", "Momentaufnahme", "vorw\u00e4rts:\nExposition \u2192 Erkrankung"))
  cols <- c(primary = epir_cols[["primary"]], accent = epir_cols[["accent"]], blue = epir_cols[["blue"]], grey = epir_cols[["light"]])
  cells$txt <- ifelse(cells$fill == "grey", "#777777", "white")
  ggplot(cells, aes(x = zeit, y = zuteilung)) +
    geom_tile(aes(fill = fill), width = 0.94, height = 0.9, colour = NA) +
    geom_text(aes(label = design, colour = txt), fontface = "bold", size = 3.9, lineheight = 0.9, nudge_y = 0.14) +
    geom_text(aes(label = mass, colour = txt), size = 3.1, lineheight = 0.9, nudge_y = -0.2) +
    scale_fill_manual(values = cols, guide = "none") +
    scale_colour_identity() +
    scale_x_discrete(position = "top") +
    labs(x = "In welche Zeitrichtung wird beobachtet?", y = "Wer teilt die Exposition zu?") +
    epir_theme() +
    theme(panel.grid = element_blank(), axis.title = element_text(face = "bold", size = 12),
          axis.text = element_text(size = 10.5, colour = epir_cols[["dark"]]), axis.title.x.top = element_text(margin = margin(b = 6)))
}

# Timelines of the designs on a common axis; "heute" = recruitment.
plot_design_timelines <- function() {
  rows <- tribble(
    ~design, ~y, ~x, ~xend, ~dir, ~lab,
    "Randomisierte kontrollierte Studie", 5, 0, 4, "vor", "Zufallszuteilung heute \u2192 Beobachtung nach vorn",
    "Prospektive Kohortenstudie", 4, 0, 4, "vor", "Exposition heute erfasst \u2192 Beobachtung nach vorn",
    "Retrospektive Kohortenstudie", 3, -4, 0, "vor", "Exposition aus alten Akten \u2192 Erkrankung bis heute (Blick vorw\u00e4rts, aber in der Vergangenheit)",
    "Fall-Kontroll-Studie", 2, 0, -4, "rueck", "F\u00e4lle und Kontrollen heute \u2192 fr\u00fchere Exposition erfragt",
    "Querschnittsstudie", 1, 0, 0, "punkt", "Exposition und Erkrankung gleichzeitig gemessen"
  )
  cols <- c(vor = epir_cols[["primary"]], rueck = epir_cols[["accent"]], punkt = epir_cols[["blue"]])
  ggplot(rows) +
    annotate("rect", xmin = -4.6, xmax = 0, ymin = 0.55, ymax = 5.6, fill = epir_cols[["light"]], alpha = 0.45) +
    geom_hline(yintercept = c(1.5, 2.5, 3.5, 4.5), colour = "#d5dadf", linewidth = 0.4) +
    geom_vline(xintercept = 0, colour = epir_cols[["dark"]], linewidth = 0.6) +
    geom_segment(data = filter(rows, dir != "punkt"), aes(x = x, xend = xend, y = y - 0.1, yend = y - 0.1, colour = dir),
                 linewidth = 2.2, lineend = "round", arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
    geom_point(data = filter(rows, dir == "punkt"), aes(x = x, y = y - 0.1, colour = dir), size = 6, shape = 18) +
    geom_text(aes(x = -4.5, y = y + 0.3, label = design), hjust = 0, fontface = "bold", size = 3.8, colour = epir_cols[["dark"]]) +
    geom_text(aes(x = -4.5, y = y - 0.34, label = lab), hjust = 0, size = 3.0, colour = "#555555") +
    annotate("text", x = -2.3, y = 5.78, label = "Vergangenheit", size = 3.4, colour = "#555555") +
    annotate("text", x = 2.3, y = 5.78, label = "Zukunft", size = 3.4, colour = "#555555") +
    annotate("label", x = 0, y = 0.3, label = "heute: Rekrutierung", size = 3.3, colour = epir_cols[["dark"]]) +
    scale_colour_manual(values = cols, guide = "none") +
    scale_x_continuous(limits = c(-4.6, 4.6), breaks = NULL) +
    scale_y_continuous(limits = c(0.1, 5.95), breaks = NULL) +
    labs(x = NULL, y = NULL) +
    epir_theme() +
    theme(panel.grid = element_blank())
}

# Schematic: from association to causation, the three alternative explanations.
plot_three_explanations <- function() {
  boxes <- tribble(
    ~x, ~label, ~sub, ~fill,
    1, "Beobachtete\nAssoziation", "z. B. RR = 1,66", "dark",
    2, "Zufall?", "Modul 4:\nStichprobenfehler", "blue",
    3, "Bias?", "dieses Modul:\nsystematischer Fehler", "accent",
    4, "Confounding?", "Modul 5b:\ndritte Variable", "accent",
    5, "Ursache?", "Bradford Hill:\nGesichtspunkte abw\u00e4gen", "primary"
  )
  cols <- c(dark = "#555555", blue = epir_cols[["blue"]], accent = epir_cols[["accent"]], primary = epir_cols[["primary"]])
  ggplot(boxes) +
    geom_segment(data = data.frame(x = 1:4 + 0.42, xend = 2:5 - 0.42), aes(x = x, xend = xend, y = 1, yend = 1),
                 arrow = arrow(length = unit(0.25, "cm"), type = "closed"), linewidth = 0.9, colour = epir_cols[["dark"]]) +
    geom_tile(aes(x = x, y = 1, fill = fill), width = 0.84, height = 0.55, colour = NA) +
    geom_text(aes(x = x, y = 1.06, label = label), colour = "white", fontface = "bold", size = 3.8, lineheight = 0.9) +
    geom_text(aes(x = x, y = 0.56, label = sub), colour = "#444444", size = 3.1, lineheight = 0.9) +
    annotate("text", x = 3, y = 1.45, label = "Erst wenn alle drei Alternativen unwahrscheinlich sind, wird Kausalit\u00e4t plausibel", size = 3.5, colour = "#555555") +
    scale_fill_manual(values = cols, guide = "none") +
    coord_cartesian(xlim = c(0.5, 5.5), ylim = c(0.35, 1.55)) +
    epir_theme() +
    theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank())
}

# Loss to follow-up by exposure (non-cases only): share censored before year 5.
plot_censoring_by_exposure <- function(unter_risiko, exposure = "hypertonie", labels = c("ohne Hypertonie", "mit Hypertonie")) {
  dat <- unter_risiko |>
    filter(ckd_inzident == 0) |>
    group_by(exp = .data[[exposure]]) |>
    summarise(n = n(), zensiert = sum(followup_jahre < 5 - 1e-9), anteil = zensiert / n, .groups = "drop") |>
    mutate(grp = factor(labels[exp + 1], levels = labels),
           lab = sprintf("%s\n(%d von %d)", fmt_pct_de(anteil, 1), zensiert, n))
  ggplot(dat, aes(x = grp, y = anteil)) +
    geom_col(fill = epir_cols[["blue"]], width = 0.55) +
    geom_text(aes(label = lab), vjust = -0.3, size = 3.6, lineheight = 0.9, colour = epir_cols[["dark"]]) +
    scale_y_continuous(labels = function(x) paste0(100 * x, " %"), limits = c(0, 0.3), expand = expansion(mult = c(0, 0.02))) +
    labs(x = NULL, y = "vorzeitig ausgeschieden", caption = "Nur Personen ohne neue CKD; Ausscheiden vor Ende des f\u00fcnften Jahres.") +
    epir_theme() +
    theme(panel.grid.major.x = element_blank())
}

# Dose-response: incident CKD by baseline eGFR category (persons at risk).
plot_dose_response <- function(unter_risiko) {
  dat <- unter_risiko |>
    mutate(kat = cut(egfr, breaks = c(60, 75, 90, Inf), right = FALSE,
                     labels = c("60\u201374", "75\u201389", "\u2265 90"))) |>
    group_by(kat) |>
    summarise(n = n(), faelle = sum(ckd_inzident), risiko = faelle / n, .groups = "drop") |>
    mutate(lab = sprintf("%s\n(%d von %d)", fmt_pct_de(risiko, 1), faelle, n))
  ggplot(dat, aes(x = kat, y = risiko)) +
    geom_col(fill = epir_cols[["accent"]], width = 0.6) +
    geom_text(aes(label = lab), vjust = -0.3, size = 3.6, lineheight = 0.9, colour = epir_cols[["dark"]]) +
    scale_y_continuous(labels = function(x) paste0(100 * x, " %"), limits = c(0, 0.18), expand = expansion(mult = c(0, 0.02))) +
    labs(x = "eGFR zu Studienbeginn (mL/min/1,73 m\u00b2)", y = "neue CKD innerhalb von 5 Jahren") +
    epir_theme() +
    theme(panel.grid.major.x = element_blank())
}

# =============================================================================
# Module 5b: confounding and effect modification
# =============================================================================

fmt_rr_de <- function(x) formatC(x, format = "f", digits = 2, decimal.mark = ",")

# Four small causal diagrams: confounder, mediator, two non-confounders.
# Nodes: E (exposure, teal), O (outcome, orange), Z (third variable, grey);
# the questioned E -> O association is dashed.
plot_dag_panels <- function() {
  nodes <- function(panel, labels) {
    tibble(panel = panel, name = c("E", "O", "Z"), x = c(1, 2, 1.5), y = c(1, 1, 1.9), label = labels)
  }
  n <- bind_rows(
    nodes("a) Confounder", c("Hypertonie", "neue CKD", "Alter")),
    nodes("b) Mediator (Zwischenschritt)", c("Hypertonie", "neue CKD", "Gef\u00e4\u00dfschaden\nder Niere")),
    nodes("c) nur mit der Exposition assoziiert", c("Hypertonie", "neue CKD", "Kochsalz-\nzufuhr (fiktiv)")),
    nodes("d) nur mit dem Outcome assoziiert", c("Hypertonie", "neue CKD", "famili\u00e4re\nNierenerkrankung"))
  ) |> mutate(panel = factor(panel, levels = unique(panel)))
  e <- bind_rows(
    tibble(panel = "a) Confounder", from = c("Z", "Z", "E"), to = c("E", "O", "O")),
    tibble(panel = "b) Mediator (Zwischenschritt)", from = c("E", "Z"), to = c("Z", "O")),
    tibble(panel = "c) nur mit der Exposition assoziiert", from = c("Z", "E"), to = c("E", "O")),
    tibble(panel = "d) nur mit dem Outcome assoziiert", from = c("Z", "E"), to = c("O", "O"))
  ) |> mutate(panel = factor(panel, levels = levels(n$panel)))
  e <- e |>
    left_join(n |> select(panel, from = name, x, y), by = c("panel", "from")) |>
    left_join(n |> select(panel, to = name, xend = x, yend = y), by = c("panel", "to")) |>
    mutate(questioned = from == "E" & to == "O",
           dx = xend - x, dy = yend - y, len = sqrt(dx^2 + dy^2),
           x = x + dx / len * 0.22, y = y + dy / len * 0.22,
           xend = xend - dx / len * 0.22, yend = yend - dy / len * 0.22)
  n <- n |> mutate(fill = case_when(name == "E" ~ "primary", name == "O" ~ "accent", TRUE ~ "grey"),
                   ly = if_else(name == "Z", y + 0.3, y - 0.3))
  ggplot() +
    geom_segment(data = e, aes(x = x, y = y, xend = xend, yend = yend, linetype = questioned),
                 arrow = arrow(length = unit(0.25, "cm"), type = "closed"), linewidth = 0.9, colour = epir_cols[["dark"]]) +
    geom_point(data = n, aes(x = x, y = y, colour = fill), size = 13) +
    geom_text(data = n, aes(x = x, y = y, label = name), colour = "white", size = 4.2, fontface = "bold") +
    geom_text(data = n, aes(x = x, y = ly, label = label), colour = epir_cols[["dark"]], size = 3.1, lineheight = 0.85) +
    scale_colour_manual(values = c(primary = epir_cols[["primary"]], accent = epir_cols[["accent"]], grey = "#6b7680"), guide = "none") +
    scale_linetype_manual(values = c(`TRUE` = "22", `FALSE` = "solid"), guide = "none") +
    facet_wrap(~ panel, ncol = 2) +
    coord_cartesian(xlim = c(0.6, 2.4), ylim = c(0.55, 2.35)) +
    labs(caption = "E = Exposition, O = Outcome, Z = dritte Variable. Gestrichelt: die zu pr\u00fcfende Assoziation.\nNur in a) ist Z ein Confounder: Z beeinflusst Exposition und Outcome und liegt nicht auf dem Weg dazwischen.") +
    epir_theme() +
    theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), strip.text = element_text(hjust = 0))
}

# Decision flowchart: stratum RRs equal? -> adjusted vs crude? (after Webb, Bain & Page 2020)
plot_strat_decision <- function() {
  b <- tribble(
    ~x, ~y, ~label, ~fill, ~w,
    2, 3, "Rohes RR berechnen,\ndann nach der dritten Variable stratifizieren", "dark", 2.6,
    2, 2, "Sind die Stratum-RRs\nungef\u00e4hr gleich?", "blue", 2.0,
    3.6, 1, "Nein \u2192 Effektmodifikation\nStratum-RRs getrennt berichten", "accent", 2.0,
    1.1, 1, "Ja \u2192 Unterscheidet sich das\nadjustierte vom rohen RR?", "blue", 2.0,
    0.1, 0, "Ja \u2192 Confounding\nadjustiertes RR berichten", "accent", 1.7,
    2.1, 0, "Nein \u2192 kein relevantes Confounding\nrohes RR gen\u00fcgt", "primary", 1.8
  )
  arrows <- tribble(
    ~x, ~y, ~xend, ~yend,
    2, 2.72, 2, 2.3,
    2, 1.7, 3.6, 1.3,
    2, 1.7, 1.1, 1.3,
    1.1, 0.7, 0.1, 0.3,
    1.1, 0.7, 2.1, 0.3
  )
  cols <- c(dark = "#555555", blue = epir_cols[["blue"]], accent = epir_cols[["accent"]], primary = epir_cols[["primary"]])
  ggplot(b) +
    geom_segment(data = arrows, aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.22, "cm"), type = "closed"), linewidth = 0.8, colour = epir_cols[["dark"]]) +
    geom_tile(aes(x = x, y = y, fill = fill, width = w), height = 0.58, colour = NA) +
    geom_text(aes(x = x, y = y, label = label), colour = "white", size = 3.3, lineheight = 0.9, fontface = "bold") +
    scale_fill_manual(values = cols, guide = "none") +
    coord_cartesian(xlim = c(-0.7, 4.7), ylim = c(-0.4, 3.4)) +
    labs(caption = "Vereinfacht: \u201eungef\u00e4hr gleich\u201c nach Augenma\u00df; exakt pr\u00fcft ein Heterogenit\u00e4tstest (nicht Teil dieses Moduls).\nSchema adaptiert nach Webb, Bain & Page 2020.") +
    epir_theme() +
    theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank())
}

# Simpson-style line plot: risk by exposure, one line per stratum plus the crude line.
# tables: list of c(a, n1, c, n0) per stratum (a/n1 exposed, c/n0 unexposed); names = stratum labels.
plot_simpson <- function(tables, exposure_labels, ylab, caption = NULL) {
  d <- bind_rows(lapply(names(tables), function(s) {
    t <- tables[[s]]
    tibble(stratum = s, exp = c(0, 1), risk = c(t[3] / t[4], t[1] / t[2]), n = c(t[4], t[2]))
  }))
  tot <- Reduce(`+`, tables)
  d <- bind_rows(d, tibble(stratum = "alle zusammen (roh)", exp = c(0, 1), risk = c(tot[3] / tot[4], tot[1] / tot[2]), n = c(tot[4], tot[2]))) |>
    mutate(stratum = factor(stratum, levels = c(names(tables), "alle zusammen (roh)")))
  lab <- d |> group_by(stratum) |> summarise(y = risk[exp == 1], rr = risk[exp == 1] / risk[exp == 0], .groups = "drop") |>
    mutate(label = paste0(stratum, "\nRR = ", fmt_rr_de(rr)))
  # spread labels vertically if they would collide
  lab <- lab |> arrange(y) |> mutate(y = { v <- y; for (i in seq_along(v)[-1]) if (v[i] - v[i - 1] < 0.045) v[i] <- v[i - 1] + 0.045; v })
  ggplot(d, aes(x = exp, y = risk, group = stratum, colour = stratum)) +
    geom_line(aes(linetype = stratum), linewidth = 1.1) +
    geom_point(aes(size = n)) +
    geom_text(data = lab, aes(x = 1.08, y = y, label = label), hjust = 0, size = 3.4, lineheight = 0.9, fontface = "bold", show.legend = FALSE) +
    scale_colour_manual(values = c(epir_cols[["primary"]], epir_cols[["accent"]], "#555555"), guide = "none") +
    scale_linetype_manual(values = c("solid", "solid", "22"), guide = "none") +
    scale_size_area(max_size = 9, guide = "none") +
    scale_x_continuous(breaks = c(0, 1), labels = exposure_labels, limits = c(-0.15, 1.75)) +
    scale_y_continuous(labels = function(x) paste0(100 * x, " %"), limits = c(0, max(d$risk) * 1.12)) +
    labs(x = NULL, y = ylab, caption = caption) +
    epir_theme() +
    theme(panel.grid.major.x = element_blank())
}

# Cohort: incident CKD by hypertension, crude and within two age strata (facets).
plot_strat_bars <- function(unter_risiko, cut_age = 65) {
  lv <- c("alle (roh)", sprintf("unter %d Jahre", cut_age), sprintf("%d Jahre und \u00e4lter", cut_age))
  d <- unter_risiko |>
    mutate(altersgruppe = if_else(alter < cut_age, lv[2], lv[3])) |>
    group_by(altersgruppe, hypertonie) |>
    summarise(n = n(), f = sum(ckd_inzident), r = f / n, .groups = "drop")
  roh <- unter_risiko |> group_by(hypertonie) |> summarise(n = n(), f = sum(ckd_inzident), r = f / n, .groups = "drop") |> mutate(altersgruppe = lv[1])
  d <- bind_rows(roh, d) |>
    mutate(altersgruppe = factor(altersgruppe, levels = lv),
           grp = factor(if_else(hypertonie == 1, "mit Hypertonie", "ohne Hypertonie"), levels = c("ohne Hypertonie", "mit Hypertonie")),
           lab = sprintf("%s\n(%d/%d)", fmt_pct_de(r), f, n))
  rr <- d |> group_by(altersgruppe) |> summarise(rr = r[hypertonie == 1] / r[hypertonie == 0], .groups = "drop") |> mutate(label = paste0("RR = ", fmt_rr_de(rr)))
  ymax <- max(d$r) * 1.35
  ggplot(d, aes(x = grp, y = r, fill = grp)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = lab), vjust = -0.3, size = 3.1, lineheight = 0.9) +
    geom_text(data = rr, aes(x = 1.5, y = ymax * 0.95, label = label), inherit.aes = FALSE, fontface = "bold", size = 3.8) +
    facet_wrap(~ altersgruppe) +
    scale_fill_manual(values = c(epir_cols[["primary"]], epir_cols[["accent"]]), guide = "none") +
    scale_y_continuous(labels = function(x) paste0(round(100 * x), " %"), limits = c(0, ymax), expand = expansion(mult = c(0, 0.02))) +
    labs(x = NULL, y = "neue CKD in 5 Jahren") +
    epir_theme() +
    theme(panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 9))
}

# Stratum RRs and crude RR on a log scale (effect modification example).
# rrs: named numeric vector of stratum RRs (names = labels incl. counts); crude: numeric.
plot_strat_forest <- function(rrs, crude, crude_label, caption = NULL) {
  d <- tibble(lab = c(names(rrs), crude_label), rr = c(unname(rrs), crude), typ = c(rep("Stratum", length(rrs)), "roh")) |>
    mutate(y = rev(seq_len(n())), lab = factor(lab, levels = rev(lab)))
  ggplot(d, aes(x = rr, y = lab)) +
    geom_vline(xintercept = 1, colour = "#888888", linetype = "22") +
    geom_vline(xintercept = crude, colour = epir_cols[["grey"]]) +
    geom_point(aes(colour = typ, shape = typ), size = 5) +
    geom_text(aes(label = paste0("RR = ", fmt_rr_de(rr))), vjust = -1.1, size = 3.4, fontface = "bold") +
    scale_colour_manual(values = c(Stratum = epir_cols[["accent"]], roh = "#555555"), guide = "none") +
    scale_shape_manual(values = c(Stratum = 16, roh = 18), guide = "none") +
    scale_x_log10(breaks = c(0.5, 1, 1.5, 2, 3, 4), limits = c(0.7, 4.2)) +
    scale_y_discrete(expand = expansion(add = c(0.6, 0.9))) +
    labs(x = "relatives Risiko (log-Skala)", y = NULL, caption = caption) +
    epir_theme()
}

# Randomisation balances measured and unmeasured characteristics: split the cohort at random.
plot_randomisation <- function(unter_risiko, seed = 5) {
  set.seed(seed)
  r <- unter_risiko |> mutate(gruppe = sample(rep(c("Gruppe A (Los)", "Gruppe B (Los)"), length.out = n())))
  s <- r |> group_by(gruppe) |> summarise(m = mean(alter), ant65 = mean(alter >= 65), hyp = mean(hypertonie), diab = mean(diabetes), .groups = "drop")
  ggplot(r, aes(x = alter, fill = gruppe)) +
    geom_histogram(binwidth = 5, boundary = 40, colour = "white", linewidth = 0.3) +
    facet_wrap(~ gruppe) +
    geom_text(data = s, aes(x = 85, y = Inf, label = sprintf("Alter im Mittel %s J.\n65+: %s\nHypertonie: %s\nDiabetes: %s", fmt_de(m), fmt_pct_de(ant65, 0), fmt_pct_de(hyp, 0), fmt_pct_de(diab, 0))),
              inherit.aes = FALSE, hjust = 1, vjust = 1.2, size = 3.2, lineheight = 0.95) +
    scale_fill_manual(values = c(epir_cols[["blue"]], epir_cols[["primary"]]), guide = "none") +
    labs(x = "Alter (Jahre)", y = "Personen") +
    epir_theme()
}
