/*
 * EpiR lightweight widgets (no dependencies).
 * License: MIT (see LICENSE). UI text is German (learning content).
 *
 * Widgets are declared in the .qmd files as HTML blocks:
 *
 *  1. Quiz:   <div class="epir-quiz"><script type="application/json">{...}</script></div>
 *             items: {type:"mc", question, options:[{text, correct, feedback}]}
 *                    {type:"num", question, answer, tol, unit, feedback_correct,
 *                                 feedback_wrong, near:[{value, tol, feedback}]}
 *  2. Calc:   <div class="epir-calc" data-mode="prev|cuminc|rate"
 *                  data-cases="362" data-denom="2000" data-years="5"></div>
 *  3. Pool:   <div class="epir-pool" data-inc="18.5" data-dur="10"></div>
 *             (steady-state model: prevalence odds = incidence rate x mean duration)
 */
(function () {
  "use strict";

  // ---------- helpers ----------------------------------------------------
  // German decimal comma; thousands grouped with a narrow no-break space
  // (DIN 5008 style, e.g. "18 100"), to avoid confusing "." with a decimal point.
  const fmtDE = (x, digits) =>
    new Intl.NumberFormat("de-DE", {
      minimumFractionDigits: digits,
      maximumFractionDigits: digits,
    })
      .formatToParts(x)
      .map((p) => (p.type === "group" ? "\u202f" : p.value))
      .join("");

  const fmtInt = (x) => fmtDE(Math.round(x), 0);

  function el(tag, attrs, children) {
    const node = document.createElement(tag);
    if (attrs) {
      for (const [k, v] of Object.entries(attrs)) {
        if (k === "class") node.className = v;
        else if (k === "text") node.textContent = v;
        else if (k === "html") node.innerHTML = v;
        else node.setAttribute(k, v);
      }
    }
    (children || []).forEach((c) => c && node.appendChild(c));
    return node;
  }

  // Parse a German or English decimal input ("8,0" or "8.0")
  function parseNum(str) {
    if (str == null) return NaN;
    const s = String(str).trim().replace(/\s/g, "").replace(",", ".");
    if (s === "") return NaN;
    return Number(s);
  }

  let uid = 0;
  const nextId = (prefix) => `${prefix}-${++uid}`;

  // ---------- 1. Quiz ----------------------------------------------------
  function initQuiz(root) {
    const defNode = root.querySelector('script[type="application/json"]');
    if (!defNode) return;
    let def;
    try {
      def = JSON.parse(defNode.textContent);
    } catch (e) {
      root.appendChild(el("p", { class: "epir-error", text: "Quiz-Definition fehlerhaft (JSON)." }));
      return;
    }
    const items = def.items || [];
    const state = { checked: new Array(items.length).fill(false), correct: new Array(items.length).fill(false) };

    const container = el("div", { class: "epir-quiz-body" });
    if (def.title) container.appendChild(el("h4", { class: "epir-quiz-title", text: def.title }));
    if (def.intro) container.appendChild(el("p", { class: "epir-quiz-intro", html: def.intro }));

    const summary = el("p", { class: "epir-quiz-summary", "aria-live": "polite" });

    items.forEach((item, idx) => {
      const itemId = nextId("epir-q");
      const block = el("fieldset", { class: "epir-quiz-item" });
      block.appendChild(
        el("legend", { html: `<span class="epir-quiz-num">Frage ${idx + 1}</span> ${item.question}` })
      );

      const feedback = el("div", { class: "epir-feedback", "aria-live": "polite", role: "status" });
      const allBtn = el("button", { type: "button", class: "btn btn-sm btn-link epir-allbtn", text: "Erklärung zu allen Antworten anzeigen" });
      allBtn.hidden = true;
      const checkBtn = el("button", { type: "button", class: "btn btn-sm btn-primary epir-checkbtn", text: "Prüfen" });

      if (item.type === "mc") {
        const opts = el("div", { class: "epir-options" });
        item.options.forEach((opt, j) => {
          const optId = `${itemId}-o${j}`;
          const row = el("div", { class: "epir-option" }, [
            el("input", { type: "radio", name: itemId, id: optId, value: String(j) }),
            el("label", { for: optId, html: opt.text }),
          ]);
          opts.appendChild(row);
        });
        block.appendChild(opts);

        checkBtn.addEventListener("click", () => {
          const sel = block.querySelector(`input[name="${itemId}"]:checked`);
          if (!sel) {
            feedback.className = "epir-feedback epir-info";
            feedback.innerHTML = "Bitte wählen Sie zuerst eine Antwort aus.";
            return;
          }
          const j = Number(sel.value);
          const opt = item.options[j];
          const ok = !!opt.correct;
          state.checked[idx] = true;
          state.correct[idx] = ok;
          feedback.className = "epir-feedback " + (ok ? "epir-success" : "epir-warning");
          feedback.innerHTML = `<strong>${ok ? "Richtig." : "Nicht die gesuchte Antwort."}</strong> ${opt.feedback || ""}`;
          block.querySelectorAll(".epir-option").forEach((r, k) => {
            r.classList.toggle("epir-chosen", k === j);
          });
          allBtn.hidden = false;
          updateSummary();
        });

        allBtn.addEventListener("click", () => {
          const list = el("ul", { class: "epir-alllist" });
          item.options.forEach((opt) => {
            list.appendChild(
              el("li", {
                class: opt.correct ? "epir-li-correct" : "epir-li-wrong",
                html: `<span class="epir-tag">${opt.correct ? "richtig" : "falsch"}</span> ${opt.text}<br><span class="epir-why">${opt.feedback || ""}</span>`,
              })
            );
          });
          feedback.appendChild(list);
          allBtn.hidden = true;
        });
      } else if (item.type === "num") {
        const inputId = `${itemId}-in`;
        const row = el("div", { class: "epir-numrow" }, [
          el("label", { for: inputId, text: item.label || "Ihre Antwort:" }),
          el("input", { type: "text", inputmode: "decimal", id: inputId, class: "form-control form-control-sm epir-numinput", autocomplete: "off", placeholder: item.placeholder || "Zahl" }),
          item.unit ? el("span", { class: "epir-unit", text: item.unit }) : null,
        ]);
        block.appendChild(row);
        allBtn.hidden = true;

        checkBtn.addEventListener("click", () => {
          const v = parseNum(block.querySelector(`#${inputId}`).value);
          if (!Number.isFinite(v)) {
            feedback.className = "epir-feedback epir-info";
            feedback.innerHTML = "Bitte geben Sie eine Zahl ein (Dezimaltrennzeichen Komma oder Punkt).";
            return;
          }
          const tol = item.tol != null ? item.tol : 0;
          const ok = Math.abs(v - item.answer) <= tol + 1e-12;
          state.checked[idx] = true;
          state.correct[idx] = ok;
          if (ok) {
            feedback.className = "epir-feedback epir-success";
            feedback.innerHTML = `<strong>Richtig.</strong> ${item.feedback_correct || ""}`;
          } else {
            let msg = item.feedback_wrong || "";
            // targeted feedback for typical wrong answers
            (item.near || []).some((n) => {
              if (Math.abs(v - n.value) <= (n.tol != null ? n.tol : 0) + 1e-12) {
                msg = n.feedback;
                return true;
              }
              return false;
            });
            feedback.className = "epir-feedback epir-warning";
            feedback.innerHTML = `<strong>Nicht richtig.</strong> ${msg}`;
          }
          updateSummary();
        });
      }

      block.appendChild(el("div", { class: "epir-quiz-actions" }, [checkBtn, allBtn]));
      block.appendChild(feedback);
      container.appendChild(block);
    });

    const resetBtn = el("button", { type: "button", class: "btn btn-sm btn-outline-secondary", text: "Selbsttest zurücksetzen" });
    resetBtn.addEventListener("click", () => {
      root.innerHTML = "";
      root.appendChild(defNode);
      initQuiz(root);
    });

    function updateSummary() {
      const n = items.length;
      const done = state.checked.filter(Boolean).length;
      const right = state.correct.filter(Boolean).length;
      summary.textContent = done === 0 ? "" : `Zwischenstand: ${right} von ${done} geprüften Fragen richtig (${n} Fragen insgesamt).`;
    }

    container.appendChild(summary);
    container.appendChild(el("div", { class: "epir-quiz-footer" }, [resetBtn]));
    root.appendChild(container);
  }

  // ---------- 2. Calculator ---------------------------------------------
  const CALC_MODES = {
    prev: {
      label: "Prävalenz",
      num: "Personen mit Erkrankung (zum Zeitpunkt t)",
      den: "Alle Personen der Population (zum Zeitpunkt t)",
      denUnit: "Personen",
      time: false,
    },
    cuminc: {
      label: "Kumulative Inzidenz (Risiko)",
      num: "Neue Fälle im Zeitraum",
      den: "Personen unter Risiko zu Beginn",
      denUnit: "Personen",
      time: true,
    },
    rate: {
      label: "Inzidenzrate",
      num: "Neue Fälle",
      den: "Personenzeit unter Risiko",
      denUnit: "Personenjahre",
      time: false,
    },
  };

  function initCalc(root) {
    const id = nextId("epir-calc");
    let mode = root.dataset.mode in CALC_MODES ? root.dataset.mode : "prev";
    const lockMode = root.dataset.lock === "true";

    const modeSel = el("select", { class: "form-select form-select-sm", id: `${id}-mode`, "aria-label": "Maßzahl wählen" });
    Object.entries(CALC_MODES).forEach(([k, m]) => {
      const o = el("option", { value: k, text: m.label });
      if (k === mode) o.selected = true;
      modeSel.appendChild(o);
    });
    if (lockMode) modeSel.disabled = true;

    const numIn = el("input", { type: "text", inputmode: "decimal", class: "form-control form-control-sm", id: `${id}-num`, value: root.dataset.cases || "" });
    const denIn = el("input", { type: "text", inputmode: "decimal", class: "form-control form-control-sm", id: `${id}-den`, value: root.dataset.denom || "" });
    const yrsIn = el("input", { type: "text", inputmode: "decimal", class: "form-control form-control-sm", id: `${id}-yrs`, value: root.dataset.years || "5" });

    const numLab = el("label", { for: `${id}-num` });
    const denLab = el("label", { for: `${id}-den` });
    const yrsLab = el("label", { for: `${id}-yrs`, text: "Länge des Zeitraums (Jahre)" });
    const yrsRow = el("div", { class: "epir-calc-row" }, [yrsLab, yrsIn]);

    const out = el("div", { class: "epir-calc-out", "aria-live": "polite" });

    function render() {
      const m = CALC_MODES[mode];
      numLab.textContent = m.num;
      denLab.textContent = `${m.den} (${m.denUnit})`;
      yrsRow.hidden = !m.time;

      const a = parseNum(numIn.value);
      const b = parseNum(denIn.value);
      const yrs = parseNum(yrsIn.value);
      out.innerHTML = "";

      if (!Number.isFinite(a) || !Number.isFinite(b) || b <= 0 || a < 0) {
        out.appendChild(el("p", { class: "epir-info", text: "Bitte Zähler und Nenner als Zahlen eingeben (Nenner > 0)." }));
        return;
      }
      if (mode !== "rate" && a > b) {
        out.appendChild(el("p", { class: "epir-warning", text: "Der Zähler ist größer als der Nenner – ein Anteil kann nicht über 1 (100 %) liegen. Prüfen Sie, ob Zähler und Nenner zur selben Population gehören." }));
        return;
      }
      const q = a / b;
      const rows = [];
      if (mode === "rate") {
        rows.push(["Rate", `${fmtDE(q, 4)} Fälle pro Personenjahr`]);
        rows.push(["pro 1\u202f000 Personenjahre", `${fmtDE(1000 * q, 1)}`]);
        rows.push(["pro 100\u202f000 Personenjahre", `${fmtDE(100000 * q, 0)}`]);
      } else {
        rows.push(["Anteil", fmtDE(q, 4)]);
        rows.push(["Prozent", `${fmtDE(100 * q, 1)} %`]);
        rows.push(["pro 1\u202f000 Personen", fmtDE(1000 * q, 1)]);
        rows.push(["pro 100\u202f000 Personen", fmtDE(100000 * q, 0)]);
      }
      const table = el("table", { class: "epir-calc-table" });
      rows.forEach(([k, v]) => table.appendChild(el("tr", null, [el("th", { text: k }), el("td", { text: v })])));
      out.appendChild(table);

      let sentence;
      if (mode === "prev") {
        sentence = `Antwortsatz: „Zum Untersuchungszeitpunkt hatten ${fmtDE(100 * q, 1)} % der ${fmtInt(b)} untersuchten Personen die Erkrankung (Punktprävalenz; das entspricht ${fmtInt(100000 * q)} pro 100\u202f000).“ – Kein Zeitraum in der Einheit: Die Prävalenz ist eine Momentaufnahme.`;
      } else if (mode === "cuminc") {
        const t = Number.isFinite(yrs) && yrs > 0 ? `innerhalb von ${fmtDE(yrs, yrs % 1 === 0 ? 0 : 1)} Jahren` : "im angegebenen Zeitraum";
        sentence = `Antwortsatz: „Von ${fmtInt(b)} zu Beginn nicht erkrankten Personen erkrankten ${fmtInt(a)} ${t}; die kumulative Inzidenz (das Risiko) beträgt ${fmtDE(100 * q, 1)} % ${t}.“ – Der Zeitraum gehört immer dazu.`;
      } else {
        sentence = `Antwortsatz: „Es traten ${fmtDE(1000 * q, 1)} neue Fälle pro 1\u202f000 Personenjahre auf (${fmtInt(a)} Fälle in ${fmtDE(b, 0)} Personenjahren).“ – Eine Rate ist kein Prozentwert: Der Nenner ist Zeit unter Risiko, nicht eine Personenzahl.`;
      }
      out.appendChild(el("p", { class: "epir-sentence", text: sentence }));
    }

    modeSel.addEventListener("change", () => {
      mode = modeSel.value;
      render();
    });
    [numIn, denIn, yrsIn].forEach((i) => i.addEventListener("input", render));

    root.appendChild(
      el("div", { class: "epir-calc-body" }, [
        el("div", { class: "epir-calc-row" }, [el("label", { for: `${id}-mode`, text: "Maßzahl" }), modeSel]),
        el("div", { class: "epir-calc-row" }, [numLab, numIn]),
        el("div", { class: "epir-calc-row" }, [denLab, denIn]),
        yrsRow,
        out,
      ])
    );
    render();
  }

  // ---------- 3. Pool model (prevalence ≈ incidence × duration) ---------
  function initPool(root) {
    const id = nextId("epir-pool");
    const inc0 = parseNum(root.dataset.inc) || 18.5; // per 1000 person-years
    const dur0 = parseNum(root.dataset.dur) || 10; // years

    const incIn = el("input", { type: "range", min: "0.5", max: "60", step: "0.5", value: String(inc0), id: `${id}-inc`, class: "form-range" });
    const durIn = el("input", { type: "range", min: "0.5", max: "40", step: "0.5", value: String(dur0), id: `${id}-dur`, class: "form-range" });
    const incVal = el("output", { for: `${id}-inc`, class: "epir-slider-val" });
    const durVal = el("output", { for: `${id}-dur`, class: "epir-slider-val" });

    // SVG tank
    const W = 320, H = 220;
    const svgNS = "http://www.w3.org/2000/svg";
    const svg = document.createElementNS(svgNS, "svg");
    svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
    svg.setAttribute("class", "epir-pool-svg");
    svg.setAttribute("role", "img");
    svg.setAttribute("aria-label", "Becken-Modell: Zufluss Neuerkrankungen, Füllstand Prävalenz, Abfluss Heilung oder Tod");
    const mk = (tag, attrs) => {
      const n = document.createElementNS(svgNS, tag);
      Object.entries(attrs).forEach(([k, v]) => n.setAttribute(k, v));
      return n;
    };
    const tankX = 100, tankY = 40, tankW = 120, tankH = 150;
    const fill = mk("rect", { x: tankX, y: tankY + tankH, width: tankW, height: 0, class: "epir-pool-fill" });
    const inflow = mk("path", { d: "", class: "epir-pool-arrow" });
    const outflow = mk("path", { d: "", class: "epir-pool-arrow epir-pool-arrow-out" });
    svg.appendChild(fill);
    svg.appendChild(mk("rect", { x: tankX, y: tankY, width: tankW, height: tankH, class: "epir-pool-tank" }));
    svg.appendChild(inflow);
    svg.appendChild(outflow);
    const label = (x, y, txt, cls) => {
      const t = mk("text", { x, y, class: cls || "epir-pool-label" });
      t.textContent = txt;
      svg.appendChild(t);
      return t;
    };
    label(10, 30, "Zufluss:");
    label(10, 46, "Neuerkrankungen");
    label(10, 60, "(Inzidenz)");
    label(232, 170, "Abfluss:");
    label(232, 186, "Heilung oder Tod");
    label(232, 200, "(1 / Dauer)");
    const fillLabel = label(tankX + tankW / 2, tankY - 12, "", "epir-pool-value");
    fillLabel.setAttribute("text-anchor", "middle");
    label(tankX + tankW / 2, H - 8, "Bestand: Erkrankte (Prävalenz)", "epir-pool-label epir-pool-center").setAttribute("text-anchor", "middle");

    const out = el("div", { class: "epir-pool-out", "aria-live": "polite" });

    function render() {
      const inc = Number(incIn.value);
      const dur = Number(durIn.value);
      incVal.textContent = `${fmtDE(inc, 1)} pro 1\u202f000 Personenjahre`;
      durVal.textContent = `${fmtDE(dur, 1)} Jahre`;
      const odds = (inc / 1000) * dur;
      const pExact = odds / (1 + odds);
      const pApprox = odds;

      // tank fill proportional to exact prevalence (0–100 %)
      const h = Math.max(0, Math.min(tankH, tankH * pExact));
      fill.setAttribute("y", tankY + tankH - h);
      fill.setAttribute("height", h);
      fillLabel.textContent = `${fmtDE(100 * pExact, 1)} %`;
      // arrow widths scale with flows (visual only)
      const inW = 2 + Math.min(14, inc / 3);
      const outW = 2 + Math.min(14, 20 / dur);
      inflow.setAttribute("d", `M 20 80 L ${tankX - 4} 80`);
      inflow.setAttribute("stroke-width", inW);
      outflow.setAttribute("d", `M ${tankX + tankW + 4} 150 L 300 150`);
      outflow.setAttribute("stroke-width", outW);

      out.innerHTML = "";
      const t = el("table", { class: "epir-calc-table" });
      t.appendChild(el("tr", null, [el("th", { text: "Näherung P ≈ Inzidenzrate × Dauer" }), el("td", { text: `${fmtDE(100 * pApprox, 1)} %` })]));
      t.appendChild(el("tr", null, [el("th", { text: "Exakt (Prävalenz-Odds = Inzidenzrate × Dauer)" }), el("td", { text: `${fmtDE(100 * pExact, 1)} %` })]));
      out.appendChild(t);
      if (pApprox > 0.1) {
        out.appendChild(el("p", { class: "epir-info epir-small", text: "Bei Prävalenzen über etwa 10 % weicht die Näherung spürbar von der exakten Formel ab – dann ist die Odds-Form zu verwenden." }));
      }
    }

    const scenario = (txt, fn) => {
      const b = el("button", { type: "button", class: "btn btn-sm btn-outline-primary", text: txt });
      b.addEventListener("click", () => {
        fn();
        render();
      });
      return b;
    };
    const buttons = el("div", { class: "epir-pool-buttons" }, [
      scenario("Bessere Therapie: Krankheitsdauer verdoppeln", () => {
        durIn.value = String(Math.min(40, Number(durIn.value) * 2));
      }),
      scenario("Prävention: Inzidenz halbieren", () => {
        incIn.value = String(Math.max(0.5, Number(incIn.value) / 2));
      }),
      scenario("Schnelle Heilung: Dauer auf 1 Jahr", () => {
        durIn.value = "1";
      }),
      scenario("Zurücksetzen", () => {
        incIn.value = String(inc0);
        durIn.value = String(dur0);
      }),
    ]);

    incIn.addEventListener("input", render);
    durIn.addEventListener("input", render);

    root.appendChild(
      el("div", { class: "epir-pool-body" }, [
        el("div", { class: "epir-pool-controls" }, [
          el("div", { class: "epir-slider" }, [el("label", { for: `${id}-inc`, text: "Inzidenzrate" }), incIn, incVal]),
          el("div", { class: "epir-slider" }, [el("label", { for: `${id}-dur`, text: "Mittlere Krankheitsdauer" }), durIn, durVal]),
          buttons,
        ]),
        el("div", { class: "epir-pool-vis" }, [svg, out]),
      ])
    );
    render();
  }

  // ---------- boot ---------------------------------------------------------
  function boot() {
    document.querySelectorAll(".epir-quiz").forEach(initQuiz);
    document.querySelectorAll(".epir-calc").forEach(initCalc);
    document.querySelectorAll(".epir-pool").forEach(initPool);
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", boot);
  else boot();
})();
