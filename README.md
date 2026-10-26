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
module/02_*.qmd             module 2 – diagnostic tests (graded exercise + Shinylive app)
data/simulate_ckd_cohort.R  simulation script for the synthetic cohort (seed 20261026)
data/ckd_kohorte.csv        synthetic dataset (n = 2000) – NOT real patient data
data/codebook.md            variable definitions and expected summary values
R/epir_plots.R              ggplot2 helpers for build-time figures (icon array, swimmer plot, person-time bars)
assets/epir-widgets.js      dependency-free widgets: quiz (MC + numeric), measure calculator, pool model
assets/includes/            HTML include that loads the widget script on every page
styles.scss                 theme tweaks (accent colour, callout classes, widget styles)
_extensions/                quarto-live (r-wasm/live) and shinylive (quarto-ext/shinylive)
.github/workflows/          CI: render + deploy to GitHub Pages
```

## Local setup

Requirements: [Quarto](https://quarto.org) >= 1.4 (developed with 1.10.19), R >= 4.3,
R packages `shinylive`, `knitr`, `rmarkdown`, `dplyr`, `readr`, `ggplot2`, `ragg`.
The render must run in a UTF-8 locale (`LANG=C.UTF-8`), otherwise German umlauts in the
build-time figures are garbled.

```bash
# R packages (shinylive needs libarchive-dev on Debian/Ubuntu)
Rscript -e 'install.packages(c("shinylive","knitr","rmarkdown","dplyr","readr","ggplot2","ragg"))'

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

1. Create a GitHub repository and push the contents of this folder as the repository root.
2. Repository → Settings → Pages → Source: **GitHub Actions**.
3. Adjust the two `TODO` entries in `_quarto.yml` (`site-url`, GitHub link in the navbar).
4. Every push to `main` runs `.github/workflows/publish.yml` (render + deploy).
   First run takes ~5 min (R package installation); the site is then available at
   `https://<org>.github.io/<repo>/`.

The Shinylive runtime (~66 MB) is copied into `_site/site_libs` at render time and is
served as static files. Nothing is executed on the server.

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
initialises three widget types declared as raw HTML blocks in the `.qmd` files:

| Widget | Markup | Notes |
|---|---|---|
| Quiz | `<div class="epir-quiz"><script type="application/json">{ "items": [...] }</script></div>` | item types `mc` (single choice, per-option feedback) and `num` (numeric answer with tolerance; optional `near` list for targeted feedback on typical wrong answers) |
| Calculator | `<div class="epir-calc" data-mode="prev|cuminc|rate" data-cases="…" data-denom="…" data-years="…" data-lock="true"></div>` | prevalence / cumulative incidence / incidence rate with German answer sentence |
| Pool model | `<div class="epir-pool" data-inc="18.5" data-dur="10"></div>` | steady-state relation prevalence odds = incidence rate × mean duration; shows approximation and exact value |

No state is stored; reloading the page resets all widgets. See the JSDoc header of the script for the
full field list.
