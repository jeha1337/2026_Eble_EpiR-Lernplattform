# EpiR – Interactive learning platform for epidemiology and R

Static Quarto website with in-browser R (webR via [quarto-live](https://github.com/r-wasm/quarto-live)),
R-based slider apps ([Shinylive](https://github.com/quarto-ext/shinylive)) and small dependency-free
JavaScript widgets for the code-free reading path (quiz, calculators, pool model; `assets/epir-widgets.js`).
No server, no login, no user data. Target audience: medical, dental and molecular-medicine
students (University of Freiburg, QB 1 and MolMed module 13). Learning content is in German.

**Status: prototype (October 2026).** Module 1 is a complete draft for review (two reading paths: code-free concept + R deep dive); module 2 is a technical prototype; modules 3–9 are planned.

## Repository layout

```
_quarto.yml                 site configuration (format live-html, shinylive filter, webR packages)
index.qmd                   start page
technik.qmd                 technical self-check page (webR load, packages, data, plotting)
module/01_*.qmd             module 1 – prevalence and incidence (calculators, pool model, quiz, 4 graded R exercises)
module/02_*.qmd             module 2 – diagnostic tests: Sens/Spez/PPV/NPV (diagnostic 2×2 + predictive-value widgets, quiz, 3 graded R exercises, Shinylive app)
module/03_*.qmd             module 3 – relative and absolute risk: RR, OR, NNT (2×2 calculators, slider widget, quiz, 4 graded R exercises)
module/04_*.qmd             module 4 – distributions and random variation: mean/SD vs median/IQR, normal and log-normal, SD vs SE, sqrt-n rule (study simulator widget, quiz, 4 graded R exercises)
module/05a_*.qmd            module 5a – study designs and bias: design map, timelines, measure per design, selection/information bias, loss to follow-up, Bradford Hill (design detective + ordering widgets, quiz, 3 graded R exercises incl. embedded case-control sampling)
module/05b_*.qmd            module 5b – confounding and effect modification: confounder conditions, causal diagrams, stratification, Mantel-Haenszel RR, Simpson reversal, effect modification, randomisation (DAG + stratifier widgets, quiz, 4 graded R exercises, Parsons problem)
module/_template.qmd        module template with QA checklist (ignored by Quarto)
data/simulate_ckd_cohort.R  simulation script for the synthetic cohort (seed 20261026)
data/ckd_kohorte.csv        synthetic dataset (n = 2000) – NOT real patient data
data/codebook.md            variable definitions and expected summary values
R/epir_plots.R              ggplot2 helpers for build-time figures (icon arrays, swimmer plot, person-time bars, RR/OR curves, design schema, diagnostic tree, PPV curves, histograms with SD bands / normal curve / log axis, boxplots, sampling distributions, target schema, design map/timelines, three-explanations schema, censoring by exposure, dose-response bars)
assets/epir-widgets.js      dependency-free widgets: quiz (MC + numeric), measure calculator, pool model, 2×2 table, relative-vs-absolute slider, diagnostic 2×2, predictive-value tree, study simulator (sampling variation), design detective, ordering task (keyboard-friendly, reusable for Parsons problems), DAG role assignment, stratifier (Mantel-Haenszel)
assets/includes/            HTML include that loads the widget script on every page
styles.scss                 theme tweaks (accent colour, callout classes, widget styles)
_extensions/                quarto-live (r-wasm/live) and shinylive (quarto-ext/shinylive)
glossar.qmd                 site-wide glossary (German / English, short definitions, module links)
.github/workflows/          CI: render + deploy to GitHub Pages
```

## Local setup

Requirements: [Quarto](https://quarto.org) >= 1.4 (developed with 1.10.19), R >= 4.3,
R packages `shinylive`, `knitr`, `rmarkdown`, `dplyr`, `readr`, `tidyr`, `ggplot2`, `patchwork`, `ragg`.
The render must run in a UTF-8 locale (`LANG=C.UTF-8`), otherwise German umlauts in the
build-time figures are garbled.

```bash
# R packages (shinylive needs libarchive-dev on Debian/Ubuntu)
Rscript -e 'install.packages(c("shinylive","knitr","rmarkdown","dplyr","readr","tidyr","ggplot2","patchwork","ragg"))'

# Extensions are vendored in _extensions/. To (re)install or update them:
#   quarto add r-wasm/quarto-live
#   quarto add quarto-ext/shinylive

# Regenerate the synthetic dataset (deterministic)
Rscript data/simulate_ckd_cohort.R

# Render and preview (UTF-8 locale required for figure labels)
export LANG=C.UTF-8 LC_ALL=C.UTF-8
quarto render
quarto preview
```

Important: Shinylive requires a service worker, so the rendered site **must be served over HTTP(S)**
(`quarto preview`, `python3 -m http.server`, GitHub Pages). Opening `_site/index.html` directly
from the file system or inside a sandboxed iframe will leave the Shinylive app on its loading spinner.
webR exercises work in both cases.

## Deployment (GitHub Pages)

Repository: <https://github.com/jeha1337/2026_Eble_EpiR-Lernplattform> (public since 2026-10-27).
Live site: <https://jeha1337.github.io/2026_Eble_EpiR-Lernplattform/> (configured as `site-url` in `_quarto.yml`).

1. GitHub Pages is enabled with source **GitHub Actions** (Settings → Pages). On the free plan this
   requires a public repository; private repositories need GitHub Pro/Team.
2. Every push to `main` runs `.github/workflows/publish.yml`: the `build` job renders the site
   (R + Quarto, data regenerated from `data/simulate_ckd_cohort.R`) and uploads `_site`; the `deploy`
   job publishes it to Pages. A failed render blocks the deployment.

## Authoring conventions

- Each module: clinical question → learning objectives → concept with worked example →
  interactive exercises → common mistakes → take-home messages → self-test → sources.
- Exercise blocks use quarto-live syntax: `#| exercise: <label>`, `#| setup: true`,
  `#| check: true` (grader returns `list(correct=, message=, type=)`).
  Hints/solutions: `::: {.hint exercise="<label>"}` / `::: {.solution exercise="<label>"}`.
- The dataset is attached per page via `webr: resources: [../data/ckd_kohorte.csv]` and is
  read in webR with `read_csv("data/ckd_kohorte.csv")`.
- Graders compute expected values from the data at run time (robust to simulation changes);
  tolerances are documented in `data/codebook.md`.
- Static figures are generated at render time with knitr/ggplot2 (`R/epir_plots.R`, device `ragg_png`);
  label them (`#| label: fig-...`) and give `fig-cap` and `fig-alt`. No hand-drawn images.
- Only packages available as WebAssembly builds can be used in webR: <https://repo.r-wasm.org>.
- Content language: German. Code, commit messages and technical docs: English.

## Licence

Code: MIT (see `LICENSE`). Learning content and data: CC BY 4.0 (see `LICENSE-CONTENT`).

## Widgets (code-free path)

`assets/epir-widgets.js` is loaded on every page via `include-in-header` (see `_quarto.yml`) and
initialises the widget types declared as raw HTML blocks in the `.qmd` files:

| Widget | Markup | Notes |
|---|---|---|
| Quiz | `<div class="epir-quiz"><script type="application/json">{ "items": [...] }</script></div>` | item types `mc` (single choice, per-option feedback) and `num` (numeric answer with tolerance; optional `near` list for targeted feedback on typical wrong answers) |
| Calculator | `<div class="epir-calc" data-mode="prev|cuminc|rate" data-cases="…" data-denom="…" data-years="…" data-lock="true"></div>` | prevalence / cumulative incidence / incidence rate with German answer sentence |
| Pool model | `<div class="epir-pool" data-inc="18.5" data-dur="10"></div>` | steady-state relation prevalence odds = incidence rate × mean duration; shows approximation and exact value |
| 2×2 table | `<div class="epir-2x2" data-a="29" data-b="136" data-c="102" data-d="1371" data-years="5" data-design="cohort|casecontrol" data-exposed="…" data-unexposed="…" data-outcome="…"></div>` | editable four-fold table; cohort mode gives risks, RR, OR, risk difference, NNT/NNH and answer sentence; case-control mode gives odds of exposure and OR only |
| Relative vs. absolute | `<div class="epir-relabs" data-base="6.9" data-rr="2.54"></div>` | sliders for baseline risk and RR with a 2 × 100 icon array; shows relative change, absolute difference and NNT/NNH |
| Diagnostic 2×2 | `<div class="epir-dx" data-tp="175" data-fn="46" data-fp="179" data-tn="1600" data-disease="…" data-test="…"></div>` | editable table (TP/FN/FP/TN, colour-coded); prevalence, sensitivity, specificity, PPV, NPV, likelihood ratios, answer sentence; hint when sensitivity and PPV diverge |
| Predictive values | `<div class="epir-ppv" data-prev="7.4" data-sens="79" data-spez="90" data-n="1000" data-disease="…" data-scenarios="Label|prev|sens|spez;…"></div>` | sliders prevalence × sensitivity × specificity with a natural-frequency tree (SVG) and PPV/NPV |
| Study simulator | `<div class="epir-sample" data-prev="18.1" data-n="100"></div>` | sliders n × true prevalence; draws 1/100/1000 studies (Math.random, unseeded on purpose) and shows the histogram of estimates, their SD (= empirical standard error) and the formula value √(p(1−p)/n) |
| Design detective | `<div class="epir-design"><script type="application/json">{ "items": [...] }</script></div>` | two questions per fictive study description (who assigns the exposure, time direction) → design and measure |
| Ordering task | `<div class="epir-order"><script type="application/json">{ "items": [...], "shown": [...], "code": false }</script></div>` | one `<select>` per item instead of drag-and-drop; `code: true` renders items as code lines (Parsons problem) |
| DAG roles | `<div class="epir-dag"><script type="application/json">{ "scenarios": [...] }</script></div>` | three nodes with arrows (SVG); learner assigns exposure / outcome / confounder / mediator; feedback names the violated confounder condition |
| Stratifier | `<div class="epir-strat"><script type="application/json">{ "scenarios": [...] }</script></div>` | two editable stratum tables (a, n1, c, n0) → crude RR, stratum RRs, Mantel-Haenszel RR and a rule-of-thumb classification (thresholds are didactic conventions, labelled as such); scenario buttons |

No state is stored; reloading the page resets all widgets. See the JSDoc header of the script for the
full field list.
