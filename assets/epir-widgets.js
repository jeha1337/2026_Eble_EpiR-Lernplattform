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
 *  4. 2x2:    <div class="epir-2x2" data-a="29" data-b="136" data-c="102" data-d="1371"
 *                  data-years="5" data-exposed="mit Diabetes" data-unexposed="ohne Diabetes"
 *                  data-outcome="neue CKD" data-design="cohort|casecontrol"></div>
 *             (cohort 2x2 table -> risks, RR, OR, risk difference, NNT/NNH + answer sentence)
 *  5. RelAbs: <div class="epir-relabs" data-base="7" data-rr="2.5"></div>
 *             (sliders: baseline risk x RR -> absolute risk difference and NNT/NNH)
 *  6. Dx:     <div class="epir-dx" data-tp="175" data-fn="46" data-fp="179" data-tn="1600"
 *                  data-disease="Albuminurie" data-test="Teststreifen"></div>
 *             (diagnostic 2x2 table -> prevalence, Sens, Spez, PPV, NPV, LR + answer sentence)
 *  7. PPV:    <div class="epir-ppv" data-prev="11" data-sens="79" data-spez="90" data-n="1000"
 *                  data-disease="Albuminurie" data-scenarios="Label|prev|sens|spez;..."></div>
 *             (sliders prevalence x sensitivity x specificity -> natural-frequency tree, PPV/NPV)
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

  // Direction note for NNT/NNH (used by 2x2 and rel/abs widgets)
  const NN_NOTE = "Lesart: RR > 1 – die Exposition schadet, der Kehrwert der Risikodifferenz heißt NNH (Exponierte pro zusätzlichem Ereignis). RR < 1 – die Exposition schützt (z. B. eine Behandlung), derselbe Kehrwert heißt NNT (Behandelte pro verhindertem Ereignis). RR = 1 – keine Risikodifferenz, NNT/NNH → ∞. Für Expositionen in Beobachtungsstudien wird auch der Begriff „number needed to be exposed“ (NNE) verwendet.";
  const NN_LARGE = 1000;   // above this, the absolute difference is practically negligible

  // ---------- 4. 2x2 table calculator (RR, OR, RD, NNT) --------------------
  function initTwoByTwo(root) {
    const id = nextId("epir-2x2");
    const d0 = root.dataset;
    const expLab = d0.exposed || "exponiert";
    const unexpLab = d0.unexposed || "nicht exponiert";
    const outLab = d0.outcome || "Ereignis";
    const outNo = d0.outcomeNo || `kein ${outLab}`;
    const years = parseNum(d0.years);
    const design = d0.design === "casecontrol" ? "casecontrol" : "cohort";

    const mkIn = (key, val) =>
      el("input", { type: "text", inputmode: "numeric", class: "form-control form-control-sm epir-cell", id: `${id}-${key}`, value: val == null ? "" : String(val), "aria-label": key });
    const aIn = mkIn("a", d0.a), bIn = mkIn("b", d0.b), cIn = mkIn("c", d0.c), dIn = mkIn("d", d0.d);

    const table = el("table", { class: "epir-2x2-table" });
    table.appendChild(el("tr", null, [el("th", { text: "" }), el("th", { text: outLab }), el("th", { text: outNo }), el("th", { text: "gesamt" })]));
    const sumExp = el("td", { class: "epir-sum" }), sumUnexp = el("td", { class: "epir-sum" });
    table.appendChild(el("tr", null, [el("th", { text: expLab }), el("td", null, [aIn]), el("td", null, [bIn]), sumExp]));
    table.appendChild(el("tr", null, [el("th", { text: unexpLab }), el("td", null, [cIn]), el("td", null, [dIn]), sumUnexp]));

    const out = el("div", { class: "epir-calc-out", "aria-live": "polite" });
    const pp = (x, digits) => `${fmtDE(100 * x, digits == null ? 1 : digits)} %`;

    function render() {
      const a = parseNum(aIn.value), b = parseNum(bIn.value), c = parseNum(cIn.value), d = parseNum(dIn.value);
      out.innerHTML = "";
      const ok = [a, b, c, d].every((v) => Number.isFinite(v) && v >= 0);
      sumExp.textContent = ok ? fmtInt(a + b) : "";
      sumUnexp.textContent = ok ? fmtInt(c + d) : "";
      if (!ok) {
        out.appendChild(el("p", { class: "epir-info", text: "Bitte alle vier Felder als Zahlen (≥ 0) ausfüllen." }));
        return;
      }
      if (a + b === 0 || c + d === 0) {
        out.appendChild(el("p", { class: "epir-warning", text: "Jede Gruppe braucht mindestens eine Person." }));
        return;
      }
      const t = el("table", { class: "epir-calc-table" });
      const row = (k, v, cls) => t.appendChild(el("tr", { class: cls || "" }, [el("th", { text: k }), el("td", { text: v })]));
      const timeTxt = Number.isFinite(years) && years > 0 ? ` in ${fmtDE(years, years % 1 === 0 ? 0 : 1)} Jahren` : "";

      if (design === "casecontrol") {
        if (b === 0 || c === 0) {
          out.appendChild(el("p", { class: "epir-warning", text: "Für das Odds Ratio dürfen die Felder b und c nicht 0 sein." }));
          return;
        }
        const or = (a * d) / (b * c);
        row("Odds der Exposition bei Fällen (a/b)", fmtDE(a / b, 3));
        row("Odds der Exposition bei Kontrollen (c/d)", fmtDE(c / d, 3));
        row("Odds Ratio (OR)", fmtDE(or, 2), "epir-row-key");
        out.appendChild(t);
        out.appendChild(el("p", { class: "epir-warning epir-small", text: "Fall-Kontroll-Design: Die Zeilensummen bestimmen die Forschenden (Zahl der Fälle und Kontrollen) – Risiken, RR und NNT lassen sich daraus nicht berechnen. Nur das OR ist interpretierbar." }));
        out.appendChild(el("p", { class: "epir-sentence", text: `Antwortsatz: „Die Odds, exponiert zu sein, waren bei den Fällen ${fmtDE(or, 2)}-mal so hoch wie bei den Kontrollen (OR = ${fmtDE(or, 2)}).“` }));
        return;
      }

      const r1 = a / (a + b), r0 = c / (c + d);
      row(`Risiko ${expLab}`, `${fmtInt(a)} / ${fmtInt(a + b)} = ${pp(r1)}`);
      row(`Risiko ${unexpLab}`, `${fmtInt(c)} / ${fmtInt(c + d)} = ${pp(r0)}`);
      if (r0 === 0) {
        out.appendChild(t);
        out.appendChild(el("p", { class: "epir-warning", text: "Kein Ereignis in der Vergleichsgruppe: RR und OR sind nicht definiert (Division durch 0). Die Risikodifferenz ist trotzdem berechenbar: " + pp(r1 - r0) + "." }));
        return;
      }
      const rr = r1 / r0;
      const rd = r1 - r0;
      const or = b > 0 && c > 0 ? (a * d) / (b * c) : NaN;
      row("Relatives Risiko (RR)", fmtDE(rr, 2), "epir-row-key");
      if (Number.isFinite(or)) row("Odds Ratio (OR)", fmtDE(or, 2));
      row(rd >= 0 ? "Risikodifferenz (absolute Risikoerhöhung, ARI)" : "Risikodifferenz (absolute Risikoreduktion, ARR)", `${fmtDE(100 * Math.abs(rd), 1)} Prozentpunkte`, "epir-row-key");
      if (rd !== 0) {
        const nn = 1 / Math.abs(rd);
        const big = nn > NN_LARGE ? " – praktisch kein Effekt" : "";
        row(rd > 0 ? "Number needed to harm (NNH)" : "Number needed to treat (NNT)", `${fmtDE(nn, 1)} → aufgerundet ${fmtInt(Math.ceil(nn - 1e-9))}${timeTxt}${big}`);
      } else {
        row("NNT / NNH", "∞ (kein Unterschied zwischen den Gruppen)");
      }
      out.appendChild(t);
      if (rd <= 0) out.appendChild(el("p", { class: "epir-info epir-small", text: NN_NOTE }));

      if (Number.isFinite(or) && Math.max(r1, r0) > 0.1 && Math.abs(or - rr) / rr > 0.05) {
        out.appendChild(el("p", { class: "epir-info epir-small", text: `OR (${fmtDE(or, 2)}) und RR (${fmtDE(rr, 2)}) weichen hier spürbar voneinander ab, weil das Ereignis nicht selten ist (${pp(Math.max(r1, r0))} in einer Gruppe). Die Faustregel OR ≈ RR gilt nur bei seltenen Ereignissen (etwa < 10 %).` }));
      }
      const dir = rd > 0 ? "höher" : "niedriger";
      let sentence = `Antwortsatz: „Das Risiko für ${outLab}${timeTxt} betrug ${pp(r1)} ${expLab} und ${pp(r0)} ${unexpLab}. Es war ${expLab} ${fmtDE(rr, 2)}-mal so hoch (RR = ${fmtDE(rr, 2)}); absolut lag es um ${fmtDE(100 * Math.abs(rd), 1)} Prozentpunkte ${dir}.`;
      if (rd !== 0) {
        const nn = Math.ceil(1 / Math.abs(rd) - 1e-9);
        sentence += rd > 0
          ? ` Auf ${fmtInt(nn)} Personen ${expLab} kommt${timeTxt} ein zusätzliches Ereignis (NNH = ${fmtInt(nn)}).“`
          : ` ${fmtInt(nn)} Personen müssen${timeTxt} behandelt werden, um ein Ereignis zu verhindern (NNT = ${fmtInt(nn)}).“`;
      } else sentence += " Da sich die Risiken nicht unterscheiden, gibt es keine NNT/NNH (→ ∞).“";
      out.appendChild(el("p", { class: "epir-sentence", text: sentence }));
    }
    [aIn, bIn, cIn, dIn].forEach((i) => i.addEventListener("input", render));
    root.appendChild(el("div", { class: "epir-calc-body" }, [table, out]));
    render();
  }

  // ---------- 5. Relative vs. absolute (sliders) --------------------------
  function initRelAbs(root) {
    const id = nextId("epir-relabs");
    const base0 = parseNum(root.dataset.base) || 7;   // baseline risk in %
    const rr0 = parseNum(root.dataset.rr) || 2.5;
    const baseIn = el("input", { type: "range", min: "0.1", max: "50", step: "0.1", value: String(base0), id: `${id}-base`, class: "form-range" });
    const rrIn = el("input", { type: "range", min: "0.1", max: "5", step: "0.01", value: String(rr0), id: `${id}-rr`, class: "form-range" });
    const baseVal = el("output", { for: `${id}-base`, class: "epir-slider-val" });
    const rrVal = el("output", { for: `${id}-rr`, class: "epir-slider-val" });

    // SVG: two bars of 100 persons each (10x10 icon grid), coloured by outcome
    const svgNS = "http://www.w3.org/2000/svg";
    const W = 340, H = 190;
    const svg = document.createElementNS(svgNS, "svg");
    svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
    svg.setAttribute("class", "epir-pool-svg");
    svg.setAttribute("role", "img");
    svg.setAttribute("aria-label", "Zwei Gruppen zu je 100 Personen; markierte Felder zeigen die Zahl der Betroffenen ohne und mit Exposition");
    const mk = (tag, attrs) => { const n = document.createElementNS(svgNS, tag); Object.entries(attrs).forEach(([k, v]) => n.setAttribute(k, v)); return n; };
    const cells = [[], []];
    const grid = (gx, label) => {
      const t = mk("text", { x: gx + 60, y: 14, "text-anchor": "middle", class: "epir-pool-label" }); t.textContent = label; svg.appendChild(t);
      for (let i = 0; i < 100; i++) {
        const r = mk("rect", { x: gx + (i % 10) * 12, y: 24 + Math.floor(i / 10) * 12, width: 10, height: 10, rx: 1.5, class: "epir-ia-cell" });
        svg.appendChild(r);
        cells[label === "ohne Exposition" ? 0 : 1].push(r);
      }
    };
    grid(20, "ohne Exposition"); grid(190, "mit Exposition");
    const cap0 = mk("text", { x: 80, y: H - 30, "text-anchor": "middle", class: "epir-pool-value" });
    const cap1 = mk("text", { x: 250, y: H - 30, "text-anchor": "middle", class: "epir-pool-value" });
    const capSub = mk("text", { x: W / 2, y: H - 8, "text-anchor": "middle", class: "epir-pool-label" });
    svg.appendChild(cap0); svg.appendChild(cap1); svg.appendChild(capSub);

    const out = el("div", { class: "epir-pool-out", "aria-live": "polite" });
    function render() {
      const base = Number(baseIn.value) / 100;
      const rr = Number(rrIn.value);
      const exp = Math.min(1, base * rr);
      baseVal.textContent = `${fmtDE(100 * base, 1)} % (von 100 Personen: ${fmtDE(100 * base, 1)})`;
      rrVal.textContent = `RR = ${fmtDE(rr, 2)}`;
      const n0 = Math.round(100 * base), n1 = Math.round(100 * exp);
      cells[0].forEach((r, i) => r.setAttribute("class", i < n0 ? "epir-ia-cell epir-ia-case" : "epir-ia-cell"));
      cells[1].forEach((r, i) => r.setAttribute("class", i < n1 ? "epir-ia-cell epir-ia-case" : i < n0 ? "epir-ia-cell epir-ia-case" : "epir-ia-cell"));
      // extra cases (beyond baseline) get the accent colour
      if (n1 > n0) cells[1].forEach((r, i) => { if (i >= n0 && i < n1) r.setAttribute("class", "epir-ia-cell epir-ia-extra"); });
      if (n1 < n0) cells[1].forEach((r, i) => { if (i >= n1 && i < n0) r.setAttribute("class", "epir-ia-cell epir-ia-avoided"); });
      cap0.textContent = `${fmtDE(100 * base, 1)} von 100`;
      cap1.textContent = `${fmtDE(100 * exp, 1)} von 100`;
      const rd = exp - base;
      capSub.textContent = rd >= 0 ? `orange = zusätzliche Fälle: ${fmtDE(100 * rd, 1)} von 100` : `hell umrandet = verhinderte Fälle: ${fmtDE(-100 * rd, 1)} von 100`;
      out.innerHTML = "";
      const t = el("table", { class: "epir-calc-table" });
      const row = (k, v) => t.appendChild(el("tr", null, [el("th", { text: k }), el("td", { text: v })]));
      row("Relatives Risiko", `${fmtDE(rr, 2)} (${rr >= 1 ? "+" : "−"}${fmtDE(100 * Math.abs(rr - 1), 0)} % relativ)`);
      row("Risiko mit Exposition", `${fmtDE(100 * exp, 1)} %`);
      row(rd === 0 ? "Risikodifferenz" : rd > 0 ? "Absolute Risikoerhöhung" : "Absolute Risikoreduktion", `${fmtDE(100 * Math.abs(rd), 1)} Prozentpunkte`);
      if (rd !== 0) {
        const nn = Math.ceil(1 / Math.abs(rd) - 1e-9);
        row(rd > 0 ? "NNH (Exponierte pro zusätzlichem Ereignis)" : "NNT (Behandelte pro verhindertem Ereignis)", `${fmtInt(nn)}${nn > NN_LARGE ? " – praktisch kein Effekt" : ""}`);
      } else {
        row("NNT / NNH", "∞ (kein Unterschied zwischen den Gruppen)");
      }
      out.appendChild(t);
      out.appendChild(el("p", { class: "epir-info epir-small", text: NN_NOTE }));
      if (exp >= 1) out.appendChild(el("p", { class: "epir-warning epir-small", text: "Basisrisiko × RR übersteigt 100 % – ein so hohes RR ist bei diesem Basisrisiko nicht möglich. Das ist ein weiterer Grund, warum RR und Basisrisiko zusammengehören." }));
    }
    const scenario = (txt, fn) => { const b = el("button", { type: "button", class: "btn btn-sm btn-outline-primary", text: txt }); b.addEventListener("click", () => { fn(); render(); }); return b; };
    const buttons = el("div", { class: "epir-pool-buttons" }, [
      scenario("Seltenes Ereignis: Basisrisiko 1 %", () => { baseIn.value = "1"; }),
      scenario("Kohorte: Basisrisiko 6,9 %, RR 2,54", () => { baseIn.value = "6.9"; rrIn.value = "2.54"; }),
      scenario("Schutzfaktor: RR 0,5", () => { rrIn.value = "0.5"; }),
      scenario("Zurücksetzen", () => { baseIn.value = String(base0); rrIn.value = String(rr0); }),
    ]);
    baseIn.addEventListener("input", render); rrIn.addEventListener("input", render);
    root.appendChild(el("div", { class: "epir-pool-body" }, [
      el("div", { class: "epir-pool-controls" }, [
        el("div", { class: "epir-slider" }, [el("label", { for: `${id}-base`, text: "Basisrisiko (ohne Exposition)" }), baseIn, baseVal]),
        el("div", { class: "epir-slider" }, [el("label", { for: `${id}-rr`, text: "Relatives Risiko" }), rrIn, rrVal]),
        buttons,
      ]),
      el("div", { class: "epir-pool-vis" }, [svg, out]),
    ]));
    render();
  }

  // ---------- 6. Diagnostic 2x2 table (Sens, Spez, PPV, NPV) --------------
  // Colours as in R/epir_plots.R (epir_dx_cols): orange = disease present,
  // petrol = disease absent; light tones = test result wrong.
  const DX_COLS = { TP: "#d9822b", FN: "#f3cfa6", FP: "#9fd0d0", TN: "#0b6e6e" };

  function initDx(root) {
    const id = nextId("epir-dx");
    const d0 = root.dataset;
    const disease = d0.disease || "Krankheit";
    const testLab = d0.test || "Test";
    const mkIn = (key, val) =>
      el("input", { type: "text", inputmode: "numeric", class: "form-control form-control-sm epir-cell", id: `${id}-${key}`, value: val == null ? "" : String(val), "aria-label": key });
    const tpIn = mkIn("tp", d0.tp), fnIn = mkIn("fn", d0.fn), fpIn = mkIn("fp", d0.fp), tnIn = mkIn("tn", d0.tn);
    const cell = (inp, key) => { const td = el("td", { class: "epir-dx-cell" }, [el("span", { class: "epir-dx-tag", text: key }), inp]); td.style.background = DX_COLS[key === "RP" ? "TP" : key === "RN" ? "TN" : key]; if (key === "RP" || key === "RN") td.classList.add("epir-dx-dark"); return td; };

    const table = el("table", { class: "epir-2x2-table epir-dx-table" });
    table.appendChild(el("tr", null, [el("th", { text: "" }), el("th", { text: `${disease} vorhanden` }), el("th", { text: `${disease} nicht vorhanden` }), el("th", { text: "Zeile gesamt" })]));
    const sumPos = el("td", { class: "epir-sum" }), sumNeg = el("td", { class: "epir-sum" });
    const sumSick = el("td", { class: "epir-sum" }), sumWell = el("td", { class: "epir-sum" }), sumAll = el("td", { class: "epir-sum" });
    table.appendChild(el("tr", null, [el("th", { text: `${testLab} positiv` }), cell(tpIn, "RP"), cell(fpIn, "FP"), sumPos]));
    table.appendChild(el("tr", null, [el("th", { text: `${testLab} negativ` }), cell(fnIn, "FN"), cell(tnIn, "RN"), sumNeg]));
    table.appendChild(el("tr", { class: "epir-sum-row" }, [el("th", { text: "Spalte gesamt" }), sumSick, sumWell, sumAll]));

    const out = el("div", { class: "epir-calc-out", "aria-live": "polite" });
    const pp = (x, digits) => `${fmtDE(100 * x, digits == null ? 1 : digits)} %`;

    function render() {
      const tp = parseNum(tpIn.value), fn = parseNum(fnIn.value), fp = parseNum(fpIn.value), tn = parseNum(tnIn.value);
      out.innerHTML = "";
      const ok = [tp, fn, fp, tn].every((v) => Number.isFinite(v) && v >= 0);
      [sumPos, sumNeg, sumSick, sumWell, sumAll].forEach((c) => (c.textContent = ""));
      if (!ok) { out.appendChild(el("p", { class: "epir-info", text: "Bitte alle vier Felder als Zahlen (≥ 0) ausfüllen." })); return; }
      const n = tp + fn + fp + tn;
      sumPos.textContent = fmtInt(tp + fp); sumNeg.textContent = fmtInt(fn + tn);
      sumSick.textContent = fmtInt(tp + fn); sumWell.textContent = fmtInt(fp + tn); sumAll.textContent = fmtInt(n);
      if (tp + fn === 0 || fp + tn === 0) { out.appendChild(el("p", { class: "epir-warning", text: "Für Sensitivität und Spezifität braucht es Personen mit und ohne " + disease + "." })); return; }
      const sens = tp / (tp + fn), spez = tn / (fp + tn), prev = (tp + fn) / n;
      const ppv = tp + fp > 0 ? tp / (tp + fp) : NaN, npv = fn + tn > 0 ? tn / (fn + tn) : NaN;
      const t = el("table", { class: "epir-calc-table" });
      const row = (k, v, cls) => t.appendChild(el("tr", { class: cls || "" }, [el("th", { text: k }), el("td", { text: v })]));
      row("Prävalenz (Vortestwahrscheinlichkeit)", `${fmtInt(tp + fn)} / ${fmtInt(n)} = ${pp(prev)}`);
      row("Sensitivität (Spalte „vorhanden“)", `${fmtInt(tp)} / ${fmtInt(tp + fn)} = ${pp(sens)}`, "epir-row-key");
      row("Spezifität (Spalte „nicht vorhanden“)", `${fmtInt(tn)} / ${fmtInt(fp + tn)} = ${pp(spez)}`, "epir-row-key");
      row("Positiver Vorhersagewert PPV (Zeile „positiv“)", Number.isFinite(ppv) ? `${fmtInt(tp)} / ${fmtInt(tp + fp)} = ${pp(ppv)}` : "nicht definiert (kein positives Ergebnis)");
      row("Negativer Vorhersagewert NPV (Zeile „negativ“)", Number.isFinite(npv) ? `${fmtInt(tn)} / ${fmtInt(fn + tn)} = ${pp(npv)}` : "nicht definiert (kein negatives Ergebnis)");
      if (spez < 1 && sens > 0) row("Likelihood-Ratios (Vertiefung)", `LR+ = ${fmtDE(sens / (1 - spez), 2)}, LR− = ${fmtDE((1 - sens) / spez, 2)}`);
      out.appendChild(t);
      let sentence = `Antwortsatz: „Der ${testLab} erkennt ${pp(sens, 0)} der Personen mit ${disease} (Sensitivität) und ${pp(spez, 0)} der Personen ohne ${disease} (Spezifität).`;
      if (Number.isFinite(ppv)) sentence += ` Bei einer Prävalenz von ${pp(prev)} hat von 100 Personen mit positivem Ergebnis etwa ${fmtInt(100 * ppv)} tatsächlich ${disease} (PPV = ${pp(ppv)})`;
      if (Number.isFinite(npv)) sentence += `; von 100 Personen mit negativem Ergebnis haben etwa ${fmtInt(100 * (1 - npv))} dennoch ${disease} (NPV = ${pp(npv)}).“`; else sentence += ".“";
      out.appendChild(el("p", { class: "epir-sentence", text: sentence }));
      if (Number.isFinite(ppv) && Math.abs(ppv - sens) > 0.1) {
        out.appendChild(el("p", { class: "epir-info epir-small", text: `Sensitivität (${pp(sens)}) und PPV (${pp(ppv)}) sind hier deutlich verschieden: Die Sensitivität liest die Spalte (alle Kranken), der PPV die Zeile (alle Testpositiven). Der PPV hängt von der Prävalenz ab, die Sensitivität nicht.` }));
      }
    }
    [tpIn, fnIn, fpIn, tnIn].forEach((i) => i.addEventListener("input", render));
    root.appendChild(el("div", { class: "epir-calc-body" }, [table, out]));
    render();
  }

  // ---------- 7. Predictive values vs. prevalence (sliders + tree) ---------
  function initPpv(root) {
    const id = nextId("epir-ppv");
    const d = root.dataset;
    const prev0 = parseNum(d.prev) || 10, sens0 = parseNum(d.sens) || 80, spez0 = parseNum(d.spez) || 90;  // all in %
    const N = Math.round(parseNum(d.n) || 1000);
    const disease = d.disease || "Krankheit";
    const mkRange = (key, min, max, step, val) => el("input", { type: "range", min: String(min), max: String(max), step: String(step), value: String(val), id: `${id}-${key}`, class: "form-range" });
    const prevIn = mkRange("prev", 0.5, 60, 0.1, prev0), sensIn = mkRange("sens", 50, 100, 0.5, sens0), spezIn = mkRange("spez", 50, 100, 0.5, spez0);
    const prevVal = el("output", { for: `${id}-prev`, class: "epir-slider-val" }), sensVal = el("output", { for: `${id}-sens`, class: "epir-slider-val" }), spezVal = el("output", { for: `${id}-spez`, class: "epir-slider-val" });

    // natural-frequency tree as SVG
    const svgNS = "http://www.w3.org/2000/svg";
    const W = 360, H = 200;
    const svg = document.createElementNS(svgNS, "svg");
    svg.setAttribute("viewBox", `0 0 ${W} ${H}`); svg.setAttribute("class", "epir-pool-svg epir-tree-svg"); svg.setAttribute("role", "img");
    svg.setAttribute("aria-label", `Häufigkeitsbaum für ${N} Personen: Aufteilung nach ${disease} und Testergebnis`);
    const mk = (tag, attrs) => { const n = document.createElementNS(svgNS, tag); Object.entries(attrs).forEach(([k, v]) => n.setAttribute(k, v)); return n; };
    const nodes = {};
    const box = (key, x, y, w, fill, dark) => {
      const g = mk("g", {});
      g.appendChild(mk("rect", { x: x - w / 2, y: y - 16, width: w, height: 32, rx: 4, fill, stroke: "#6b7680", "stroke-width": fill === "#ffffff" ? 1 : 0 }));
      const t1 = mk("text", { x, y: y - 3, "text-anchor": "middle", class: "epir-tree-num", fill: dark ? "#fff" : "#222" });
      const t2 = mk("text", { x, y: y + 10, "text-anchor": "middle", class: "epir-tree-lab", fill: dark ? "#fff" : "#222" });
      g.appendChild(t1); g.appendChild(t2); svg.appendChild(g); nodes[key] = { t1, t2 };
    };
    const line = (x1, y1, x2, y2) => svg.appendChild(mk("line", { x1, y1, x2, y2, stroke: "#8a949e", "stroke-width": 1 }));
    line(180, 36, 90, 86); line(180, 36, 270, 86); line(90, 118, 45, 158); line(90, 118, 135, 158); line(270, 118, 225, 158); line(270, 118, 315, 158);
    box("root", 180, 20, 120, "#ffffff", false);
    box("sick", 90, 102, 120, DX_COLS.TP, true); box("well", 270, 102, 120, DX_COLS.TN, true);
    box("TP", 45, 174, 84, DX_COLS.TP, true); box("FN", 135, 174, 84, DX_COLS.FN, false);
    box("FP", 225, 174, 84, DX_COLS.FP, false); box("TN", 315, 174, 84, DX_COLS.TN, true);

    const out = el("div", { class: "epir-calc-out", "aria-live": "polite" });
    const setNode = (k, num, lab) => { nodes[k].t1.textContent = num; nodes[k].t2.textContent = lab; };

    function render() {
      const prev = parseNum(prevIn.value) / 100, sens = parseNum(sensIn.value) / 100, spez = parseNum(spezIn.value) / 100;
      prevVal.textContent = `Prävalenz ${fmtDE(100 * prev, 1)} % (${fmtInt(N * prev)} von ${fmtInt(N)})`;
      sensVal.textContent = `Sensitivität ${fmtDE(100 * sens, 1)} %`;
      spezVal.textContent = `Spezifität ${fmtDE(100 * spez, 1)} %`;
      const sick = N * prev, well = N - sick;
      const tp = sick * sens, fn = sick - tp, tn = well * spez, fp = well - tn;
      const r = (x) => fmtInt(x);   // rounded to whole persons for the tree
      setNode("root", fmtInt(N), "Personen");
      setNode("sick", r(sick), `mit ${disease}`); setNode("well", r(well), `ohne ${disease}`);
      setNode("TP", r(tp), "Test positiv"); setNode("FN", r(fn), "Test negativ");
      setNode("FP", r(fp), "Test positiv"); setNode("TN", r(tn), "Test negativ");
      const ppv = tp / (tp + fp), npv = tn / (tn + fn);
      out.innerHTML = "";
      const t = el("table", { class: "epir-calc-table" });
      const row = (k, v, cls) => t.appendChild(el("tr", { class: cls || "" }, [el("th", { text: k }), el("td", { text: v })]));
      row("Testpositive", `${r(tp)} richtig + ${r(fp)} falsch = ${r(tp + fp)}`);
      row("PPV", `${r(tp)} / ${r(tp + fp)} ≈ ${fmtDE(100 * ppv, 1)} %`, "epir-row-key");
      row("Testnegative", `${r(tn)} richtig + ${r(fn)} falsch = ${r(tn + fn)}`);
      row("NPV", `${r(tn)} / ${r(tn + fn)} ≈ ${fmtDE(100 * npv, 1)} %`, "epir-row-key");
      out.appendChild(t);
      out.appendChild(el("p", { class: "epir-sentence", text: `Antwortsatz: „Von 100 Personen mit positivem Test haben in dieser Gruppe etwa ${fmtInt(100 * ppv)} tatsächlich ${disease}; von 100 mit negativem Test ${100 * (1 - npv) < 0.5 ? "weniger als 1" : "etwa " + fmtInt(100 * (1 - npv))}.“` }));
      out.appendChild(el("p", { class: "epir-info epir-small", text: "Baum und Tabelle sind auf ganze Personen gerundet; PPV und NPV werden aus den ungerundeten Werten berechnet." }));
    }
    const scenario = (txt, fn) => { const b = el("button", { type: "button", class: "btn btn-sm btn-outline-primary", text: txt }); b.addEventListener("click", () => { fn(); render(); }); return b; };
    const buttons = el("div", { class: "epir-pool-buttons" });
    // scenarios from data-scenarios: "Label|prev|sens|spez;Label|prev|sens|spez"
    (d.scenarios || "").split(";").map((x) => x.trim()).filter(Boolean).forEach((sc) => {
      const [lab, p, se, sp] = sc.split("|");
      buttons.appendChild(scenario(lab, () => { if (p) prevIn.value = p; if (se) sensIn.value = se; if (sp) spezIn.value = sp; }));
    });
    buttons.appendChild(scenario("Zurücksetzen", () => { prevIn.value = String(prev0); sensIn.value = String(sens0); spezIn.value = String(spez0); }));
    [prevIn, sensIn, spezIn].forEach((i) => i.addEventListener("input", render));
    root.appendChild(el("div", { class: "epir-pool-body" }, [
      el("div", { class: "epir-pool-controls" }, [
        el("div", { class: "epir-slider" }, [el("label", { for: `${id}-prev`, text: "Prävalenz in der untersuchten Gruppe" }), prevIn, prevVal]),
        el("div", { class: "epir-slider" }, [el("label", { for: `${id}-sens`, text: "Sensitivität des Tests" }), sensIn, sensVal]),
        el("div", { class: "epir-slider" }, [el("label", { for: `${id}-spez`, text: "Spezifität des Tests" }), spezIn, spezVal]),
        buttons,
      ]),
      el("div", { class: "epir-pool-vis" }, [svg, out]),
    ]));
    render();
  }

  // ---------- boot ---------------------------------------------------------
  function boot() {
    document.querySelectorAll(".epir-quiz").forEach(initQuiz);
    document.querySelectorAll(".epir-calc").forEach(initCalc);
    document.querySelectorAll(".epir-pool").forEach(initPool);
    document.querySelectorAll(".epir-2x2").forEach(initTwoByTwo);
    document.querySelectorAll(".epir-relabs").forEach(initRelAbs);
    document.querySelectorAll(".epir-dx").forEach(initDx);
    document.querySelectorAll(".epir-ppv").forEach(initPpv);
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", boot);
  else boot();
})();
