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
 *             optional per item: tag  = short badge shown before the question
 *                                       (e.g. "Wiederholung · Modul 1", "Code lesen")
 *                                code = R code shown as a <pre> block below the question
 *                                       ("predict the output" items, PRIMM step "Predict")
 *  9. Estimate: <div class="epir-estimate" data-id="m1-prev" data-unit="von 100" data-min="0"
 *                    data-max="100" data-next="Abschnitt „Durchgerechnetes Beispiel“">Frage …</div>
 *               <div class="epir-estimate-reveal" data-ref="m1-prev" data-answer="18" data-unit="von 100"
 *                    data-diff-unit="Personen" data-answer-text="18 von 100 (Punktprävalenz 18,1 %)"></div>
 *               optional on the reveal: data-tol="0.1" (absolute band for "Sehr nah dran"; default max(1, 10 %))
 *             (prequestion: the learner writes down a guess before the concept is taught; the
 *              reveal box later compares guess and actual value. Nothing is stored or sent.)
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
 *  8. Sample: <div class="epir-sample" data-prev="18.1" data-n="100"></div>
 *             (study simulator: draw 1 / 100 / 1000 studies of size n from a population with
 *              a given prevalence -> histogram of the estimates, SD = standard error)
 * 10. Design: <div class="epir-design"><script type="application/json">{"items":[...]}</script></div>
 *             item: {text, assign:"life"|"researcher", time:"forward"|"backward"|"snapshot",
 *                    design, measure, explain, why_assign?, why_time?}
 *             ("design detective": two questions per fictive study description -> design + measure)
 * 11. Order:  <div class="epir-order"><script type="application/json">{...}</script></div>
 *             {prompt, items:[{text, feedback}] in CORRECT order, shown:[display order, 1-based],
 *              code:false, success}
 *             (ordering task without drag-and-drop: one <select> per item, keyboard friendly;
 *              used for the evidence hierarchy in module 5a and as Parsons problems for code)
 * 12. DAG:    <div class="epir-dag"><script type="application/json">{...}</script></div>
 *             {scenarios:[{title, nodes:[{id, text, role:"exposition"|"outcome"|"confounder"|"mediator"}],
 *                          edges:[[from, to], ...], explain}]}
 *             (three fixed nodes with arrows; the learner assigns a role to every node;
 *              feedback names the first wrong node and the violated confounder condition)
 * 13. Strat:  <div class="epir-strat"><script type="application/json">{...}</script></div>
 *             {scenarios:[{key, label, exposure:[without, with], outcome, strata:[s1, s2],
 *                          tables:[[a, n1, c, n0], [a, n1, c, n0]], lesson}]}
 *             (two editable stratum tables a/n1 and c/n0 -> crude RR, stratum RRs, Mantel-Haenszel RR
 *              and a rule-of-thumb classification; thresholds are didactic conventions, see module 5b)
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
      const tagHtml = item.tag ? `<span class="epir-item-tag">${item.tag}</span> ` : "";
      block.appendChild(
        el("legend", { html: `<span class="epir-quiz-num">Frage ${idx + 1}</span> ${tagHtml}${item.question}` })
      );
      if (item.code) {
        // "Predict the output" items: code is shown read-only; the learner answers before running anything.
        const pre = el("pre", { class: "epir-quiz-code" }, [el("code", { class: "sourceCode r", text: item.code })]);
        block.appendChild(pre);
      }

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
      // exact values for PPV/NPV
      const sickX = N * prev, wellX = N - sickX;
      const tpX = sickX * sens, fnX = sickX - tpX, tnX = wellX * spez, fpX = wellX - tnX;
      const ppv = tpX / (tpX + fpX), npv = tnX / (tnX + fnX);
      // stepwise integer rounding for the tree: every branch sums exactly to its parent
      const sick = Math.round(sickX), well = N - sick;
      const tp = Math.round(sick * sens), fn = sick - tp, tn = Math.round(well * spez), fp = well - tn;
      const r = (x) => fmtInt(x);
      setNode("root", fmtInt(N), "Personen");
      setNode("sick", r(sick), `mit ${disease}`); setNode("well", r(well), `ohne ${disease}`);
      setNode("TP", r(tp), "Test positiv"); setNode("FN", r(fn), "Test negativ");
      setNode("FP", r(fp), "Test positiv"); setNode("TN", r(tn), "Test negativ");
      out.innerHTML = "";
      const t = el("table", { class: "epir-calc-table" });
      const row = (k, v, cls) => t.appendChild(el("tr", { class: cls || "" }, [el("th", { text: k }), el("td", { text: v })]));
      row("Testpositive", `${r(tp)} richtig + ${r(fp)} falsch = ${r(tp + fp)}`);
      row("PPV", `${r(tp)} / ${r(tp + fp)} ≈ ${fmtDE(100 * ppv, 1)} %`, "epir-row-key");
      row("Testnegative", `${r(tn)} richtig + ${r(fn)} falsch = ${r(tn + fn)}`);
      row("NPV", `${r(tn)} / ${r(tn + fn)} ≈ ${fmtDE(100 * npv, 1)} %`, "epir-row-key");
      out.appendChild(t);
      out.appendChild(el("p", { class: "epir-sentence", text: `Antwortsatz: „Von 100 Personen mit positivem Test haben in dieser Gruppe etwa ${fmtInt(100 * ppv)} tatsächlich ${disease}; von 100 mit negativem Test ${100 * (1 - npv) < 0.5 ? "weniger als 1" : "etwa " + fmtInt(100 * (1 - npv))}.“` }));
      out.appendChild(el("p", { class: "epir-info epir-small", text: "Baum und Tabelle sind Schritt für Schritt auf ganze Personen gerundet (jeder Ast summiert sich zum übergeordneten); PPV und NPV werden exakt aus den ungerundeten Werten berechnet und können deshalb um Zehntelprozent vom Bruch im Baum abweichen." }));
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


  // ---------- 10. Design detective ----------------------------------------
  const DESIGN_ASSIGN = [
    { v: "life", t: "Das Leben: Die Personen sind exponiert oder nicht, ohne Zutun der Forschenden (Beobachtungsstudie)." },
    { v: "researcher", t: "Die Forschenden: Sie teilen die Exposition (z. B. Behandlung) per Zufall zu (Experiment)." },
  ];
  const DESIGN_TIME = [
    { v: "forward", t: "Vorwärts: von der Exposition zur Erkrankung (Gruppen nach Exposition gebildet, Erkrankung abgewartet oder nachverfolgt)." },
    { v: "backward", t: "Rückwärts: von der Erkrankung zur Exposition (Erkrankte und Nicht-Erkrankte ausgewählt, frühere Exposition erfragt)." },
    { v: "snapshot", t: "Momentaufnahme: Exposition und Erkrankung gleichzeitig gemessen." },
  ];
  function initDesign(root) {
    const script = root.querySelector("script[type='application/json']");
    if (!script) return;
    let data;
    try { data = JSON.parse(script.textContent); } catch (e) { root.appendChild(el("p", { class: "epir-error", text: "Widget-Daten fehlerhaft." })); return; }
    const items = data.items || [];
    const wrap = el("div", { class: "epir-design-list" });
    let solved = 0;
    const summary = el("div", { class: "epir-quiz-summary", "aria-live": "polite" });
    items.forEach((it, i) => {
      const id = nextId("epir-design");
      const fs = el("fieldset", { class: "epir-quiz-item epir-design-item" });
      fs.appendChild(el("legend", { html: `<span class="epir-quiz-num">Studie ${i + 1}.</span> <em>Fiktive Beschreibung:</em> ${it.text}` }));
      const group = (name, label, opts) => {
        const g = el("div", { class: "epir-design-q" });
        g.appendChild(el("div", { class: "epir-design-qlabel", text: label }));
        opts.forEach((o) => {
          const rid = `${id}-${name}-${o.v}`;
          g.appendChild(el("label", { class: "epir-option", for: rid }, [
            el("input", { type: "radio", name: `${id}-${name}`, id: rid, value: o.v }),
            el("span", { text: o.t }),
          ]));
        });
        return g;
      };
      const gA = group("assign", "1. Wer teilt die Exposition zu?", DESIGN_ASSIGN);
      const gT = group("time", "2. In welche Zeitrichtung wird beobachtet?", DESIGN_TIME);
      const btn = el("button", { type: "button", class: "btn btn-sm btn-primary", text: "Prüfen" });
      const out = el("div", { class: "epir-feedback", "aria-live": "polite", role: "status" });
      let done = false;
      btn.addEventListener("click", () => {
        const a = fs.querySelector(`input[name='${id}-assign']:checked`);
        const t = fs.querySelector(`input[name='${id}-time']:checked`);
        if (!a || !t) { out.className = "epir-feedback epir-info"; out.textContent = "Bitte beide Fragen beantworten."; return; }
        const okA = a.value === it.assign, okT = t.value === it.time;
        if (okA && okT) {
          out.className = "epir-feedback epir-success";
          out.innerHTML = `<strong>Richtig: ${it.design}.</strong> Passende Maßzahl: ${it.measure}. ${it.explain || ""}`;
          if (!done) { done = true; solved += 1; summary.textContent = `${solved} von ${items.length} Studien richtig eingeordnet.`; }
          fs.querySelectorAll("input").forEach((inp) => { inp.disabled = true; });
          btn.disabled = true;
          return;
        }
        const parts = [];
        if (a.value === "researcher" && t.value !== "forward") {
          parts.push("Wenn die Forschenden die Exposition zuteilen, kann man nur nach vorn beobachten – rückwärts oder gleichzeitig ist dann nicht möglich.");
        }
        if (!okA) parts.push(it.why_assign || (it.assign === "life" ? "Hier hat niemand per Zufall zugeteilt – die Exposition ist im Alltag entstanden. Das ist eine Beobachtungsstudie." : "Hier teilen die Forschenden per Los zu – das ist ein Experiment."));
        if (!okT) parts.push(it.why_time || "Prüfen Sie, ob die Gruppen nach Exposition (vorwärts), nach Erkrankung (rückwärts) oder gar nicht gebildet wurden (Momentaufnahme).");
        out.className = "epir-feedback epir-warning";
        out.innerHTML = `<strong>Noch nicht.</strong> ${parts.join(" ")}`;
      });
      fs.appendChild(gA); fs.appendChild(gT);
      fs.appendChild(el("div", { class: "epir-quiz-actions" }, [btn]));
      fs.appendChild(out);
      wrap.appendChild(fs);
    });
    root.appendChild(wrap);
    root.appendChild(summary);
  }

  // ---------- 11. Ordering task (no drag-and-drop) --------------------------
  function initOrder(root) {
    const script = root.querySelector("script[type='application/json']");
    if (!script) return;
    let data;
    try { data = JSON.parse(script.textContent); } catch (e) { root.appendChild(el("p", { class: "epir-error", text: "Widget-Daten fehlerhaft." })); return; }
    const items = data.items || [];
    const n = items.length;
    if (n < 2) return;
    // plain text for messages: strip markup, except for code lines (they may contain "<")
    const plain = (t) => (data.code ? t : t.replace(/<[^>]+>/g, ""));
    const esc = (t) => t.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
    // display order: 1-based indices into the correct order; fixed so every learner sees the same list
    let shown = Array.isArray(data.shown) && data.shown.length === n ? data.shown.map((k) => k - 1) : items.map((_, i) => i);
    const id = nextId("epir-order");
    if (data.prompt) root.appendChild(el("p", { class: "epir-order-prompt", html: data.prompt }));
    const list = el("ol", { class: "epir-order-list" + (data.code ? " epir-order-code" : "") });
    const selects = [];
    shown.forEach((k) => {
      const sel = el("select", { class: "form-select form-select-sm epir-order-select", "aria-label": `Position für: ${plain(items[k].text)}` });
      sel.appendChild(el("option", { value: "", text: "–" }));
      for (let p = 1; p <= n; p++) sel.appendChild(el("option", { value: String(p), text: String(p) }));
      selects.push({ sel, k });
      list.appendChild(el("li", { class: "epir-order-item" }, [
        sel,
        data.code ? el("pre", { class: "epir-quiz-code epir-order-codeline", text: items[k].text }) : el("span", { class: "epir-order-text", html: items[k].text }),
      ]));
    });
    const btn = el("button", { type: "button", class: "btn btn-sm btn-primary", text: "Reihenfolge prüfen" });
    const out = el("div", { class: "epir-feedback", "aria-live": "polite", role: "status" });
    btn.addEventListener("click", () => {
      const chosen = selects.map(({ sel, k }) => ({ k, pos: sel.value === "" ? NaN : Number(sel.value) }));
      if (chosen.some((c) => Number.isNaN(c.pos))) { out.className = "epir-feedback epir-info"; out.textContent = "Bitte jedem Eintrag eine Position geben."; return; }
      const seen = new Map();
      for (const c of chosen) { if (seen.has(c.pos)) { out.className = "epir-feedback epir-info"; out.textContent = `Position ${c.pos} wurde zweimal vergeben – jede Position nur einmal.`; return; } seen.set(c.pos, c.k); }
      // first wrongly placed item in terms of the correct sequence
      let firstWrong = -1;
      for (let p = 1; p <= n; p++) { if (seen.get(p) !== p - 1) { firstWrong = p; break; } }
      if (firstWrong === -1) {
        out.className = "epir-feedback epir-success";
        out.innerHTML = `<strong>Richtig.</strong> ${data.success || ""}`;
        const ul = el("ol", { class: "epir-alllist" });
        items.forEach((it) => ul.appendChild(el("li", { html: `${data.code ? "<code>" + esc(it.text) + "</code>" : it.text}${it.feedback ? ' <span class="epir-why">– ' + it.feedback + "</span>" : ""}` })));
        out.appendChild(ul);
        return;
      }
      const shouldBe = items[firstWrong - 1];
      const placedThere = items[seen.get(firstWrong)];
      out.className = "epir-feedback epir-warning";
      out.innerHTML = `<strong>Ab Position ${firstWrong} stimmt die Reihenfolge nicht.</strong> Dort steht „${data.code ? "<code>" + esc(placedThere.text) + "</code>" : plain(placedThere.text)}“, erwartet wird „${data.code ? "<code>" + esc(shouldBe.text) + "</code>" : plain(shouldBe.text)}“. ${shouldBe.feedback || ""}`;
    });
    root.appendChild(list);
    root.appendChild(el("div", { class: "epir-quiz-actions" }, [btn]));
    root.appendChild(out);
  }

  // ---------- 12. DAG role assignment (module 5b) ---------------------------
  function initDag(root) {
    const script = root.querySelector("script[type='application/json']");
    if (!script) return;
    let data;
    try { data = JSON.parse(script.textContent); } catch (e) { root.appendChild(el("p", { class: "epir-error", text: "Widget-Daten fehlerhaft." })); return; }
    const roles = [
      ["exposition", "Exposition"], ["outcome", "Outcome"], ["confounder", "Confounder"], ["mediator", "Mediator"],
    ];
    const roleLabel = (r) => (roles.find((x) => x[0] === r) || [r, r])[1];
    // node layout: E left, O right, Z top (same picture as the figures in module 5b)
    const pos = { 0: [70, 150], 1: [330, 150], 2: [200, 45] };
    (data.scenarios || []).forEach((sc, si) => {
      const id = nextId("epir-dag");
      const box = el("div", { class: "epir-dag-scenario" });
      box.appendChild(el("p", { class: "epir-dag-title", html: `<strong>Szenario ${si + 1}:</strong> ${sc.title || ""}` }));
      const nodes = sc.nodes || [];
      if (nodes.length !== 3) return;
      const idx = (nid) => nodes.findIndex((nd) => nd.id === nid);
      // SVG picture
      const ns = "http://www.w3.org/2000/svg";
      const svg = document.createElementNS(ns, "svg");
      svg.setAttribute("viewBox", "0 0 400 200"); svg.setAttribute("class", "epir-dag-svg"); svg.setAttribute("role", "img");
      svg.setAttribute("aria-label", `Kausaldiagramm mit den Knoten ${nodes.map((nd) => nd.text).join(", ")}`);
      const defs = document.createElementNS(ns, "defs");
      defs.innerHTML = `<marker id="${id}-arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M 0 0 L 10 5 L 0 10 z" fill="#333"/></marker>`;
      svg.appendChild(defs);
      (sc.edges || []).forEach(([f, t]) => {
        const a = pos[idx(f)], b = pos[idx(t)];
        if (!a || !b) return;
        const dx = b[0] - a[0], dy = b[1] - a[1], len = Math.hypot(dx, dy), r = 34;
        const line = document.createElementNS(ns, "line");
        line.setAttribute("x1", a[0] + dx / len * r); line.setAttribute("y1", a[1] + dy / len * r);
        line.setAttribute("x2", b[0] - dx / len * r); line.setAttribute("y2", b[1] - dy / len * r);
        line.setAttribute("stroke", "#333"); line.setAttribute("stroke-width", "2.2"); line.setAttribute("marker-end", `url(#${id}-arrow)`);
        svg.appendChild(line);
      });
      nodes.forEach((nd, i) => {
        const [x, y] = pos[i];
        const g = document.createElementNS(ns, "g");
        const c = document.createElementNS(ns, "circle");
        c.setAttribute("cx", x); c.setAttribute("cy", y); c.setAttribute("r", 30); c.setAttribute("class", "epir-dag-node");
        g.appendChild(c);
        const label = document.createElementNS(ns, "text");
        label.setAttribute("x", x); label.setAttribute("y", y + 4); label.setAttribute("text-anchor", "middle"); label.setAttribute("class", "epir-dag-nodelabel");
        label.textContent = String(i + 1);
        g.appendChild(label);
        const t = document.createElementNS(ns, "text");
        t.setAttribute("x", x); t.setAttribute("y", i === 2 ? y - 38 : y + 50); t.setAttribute("text-anchor", "middle"); t.setAttribute("class", "epir-dag-text");
        t.textContent = nd.text;
        g.appendChild(t);
        svg.appendChild(g);
      });
      box.appendChild(svg);
      // one select per node
      const form = el("div", { class: "epir-dag-form" });
      const selects = nodes.map((nd, i) => {
        const sel = el("select", { class: "form-select form-select-sm", id: `${id}-n${i}` });
        sel.appendChild(el("option", { value: "", text: "– Rolle wählen –" }));
        roles.forEach(([v, lab]) => sel.appendChild(el("option", { value: v, text: lab })));
        form.appendChild(el("label", { class: "epir-dag-row", for: `${id}-n${i}` }, [el("span", { class: "epir-dag-num", text: String(i + 1) }), el("span", { text: nd.text }), sel]));
        return sel;
      });
      box.appendChild(form);
      const btn = el("button", { type: "button", class: "btn btn-sm btn-primary", text: "Prüfen" });
      const out = el("div", { class: "epir-feedback", "aria-live": "polite", role: "status" });
      btn.addEventListener("click", () => {
        const chosen = selects.map((s) => s.value);
        if (chosen.some((v) => v === "")) { out.className = "epir-feedback epir-info"; out.textContent = "Bitte jedem Knoten eine Rolle zuordnen."; return; }
        // Exposition and Outcome are fixed by the question: check them first, then the third variable
        const order = [0, 1, 2];
        for (const i of order) {
          if (chosen[i] !== nodes[i].role) {
            out.className = "epir-feedback epir-warning";
            out.innerHTML = `<strong>Knoten ${i + 1} („${nodes[i].text}“) ist nicht ${roleLabel(chosen[i])}.</strong> ${nodes[i].why || ""}`;
            return;
          }
        }
        out.className = "epir-feedback epir-success";
        out.innerHTML = `<strong>Richtig.</strong> ${sc.explain || ""}`;
        selects.forEach((s) => (s.disabled = true));
        btn.disabled = true;
      });
      box.appendChild(el("div", { class: "epir-quiz-actions" }, [btn]));
      box.appendChild(out);
      root.appendChild(box);
    });
  }

  // ---------- 13. Stratifier: crude vs. stratum-specific vs. Mantel-Haenszel RR ----
  function initStrat(root) {
    const script = root.querySelector("script[type='application/json']");
    if (!script) return;
    let data;
    try { data = JSON.parse(script.textContent); } catch (e) { root.appendChild(el("p", { class: "epir-error", text: "Widget-Daten fehlerhaft." })); return; }
    const scenarios = data.scenarios || [];
    if (!scenarios.length) return;
    const id = nextId("epir-strat");
    // didactic conventions (declared as such in module 5b, not statistical tests)
    const HOMOG = Number.isFinite(data.homog_ratio) ? data.homog_ratio : 1.25; // max(RR)/min(RR) <= 1.25 -> "ungefähr gleich"
    const CONF = Number.isFinite(data.conf_change) ? data.conf_change : 0.10; // |crude - MH| / crude >= 10 % -> confounding
    const rr2 = (x) => fmtDE(x, 2);
    const pp = (x) => `${fmtDE(100 * x, 1)} %`;

    let current = scenarios[0];
    const head = el("div", { class: "epir-strat-head" });
    const btnRow = el("div", { class: "epir-strat-buttons" });
    const labels = { exp: [], strata: [] };
    const inputs = [[], []]; // per stratum: a, n1, c, n0
    const mkIn = (key) => el("input", { type: "text", inputmode: "numeric", class: "form-control form-control-sm epir-cell", "aria-label": key });

    const tables = el("div", { class: "epir-strat-tables" });
    const tableEls = [0, 1].map((s) => {
      const t = el("table", { class: "epir-2x2-table epir-strat-table" });
      const cap = el("caption", { class: "epir-strat-cap" });
      labels.strata[s] = cap;
      t.appendChild(cap);
      const outTh = el("th", { text: "Fälle" });
      t.appendChild(el("tr", null, [el("th", { text: "" }), outTh, el("th", { text: "Personen" }), el("th", { text: "Risiko" })]));
      labels["out" + s] = outTh;
      const rows = ["mit", "ohne"].map((k, r) => {
        const th = el("th");
        labels.exp.push(th);
        const i1 = mkIn(r === 0 ? "a" : "c"), i2 = mkIn(r === 0 ? "n1" : "n0");
        const risk = el("td", { class: "epir-sum" });
        inputs[s].push(i1, i2);
        inputs[s]["risk" + r] = risk;
        return el("tr", null, [th, el("td", null, [i1]), el("td", null, [i2]), risk]);
      });
      rows.forEach((r) => t.appendChild(r));
      const rrRow = el("tr", { class: "epir-strat-rr" }, [el("th", { text: "RR im Stratum" }), el("td", { colspan: "3", class: "epir-strat-rrval" })]);
      inputs[s].rr = rrRow.lastChild;
      t.appendChild(rrRow);
      tables.appendChild(t);
      return t;
    });
    const out = el("div", { class: "epir-calc-out", "aria-live": "polite" });

    function load(sc) {
      current = sc;
      head.innerHTML = `<strong>${sc.label}</strong>${sc.question ? " – " + sc.question : ""}`;
      [0, 1].forEach((s) => {
        labels.strata[s].textContent = `Stratum: ${sc.strata[s]}`;
        labels["out" + s].textContent = sc.outcome || "Fälle";
        sc.tables[s].forEach((v, j) => (inputs[s][j].value = String(v)));
      });
      labels.exp.forEach((th, i) => (th.textContent = sc.exposure[i % 2 === 0 ? 1 : 0]));
      render();
    }

    function render() {
      out.innerHTML = "";
      const S = [0, 1].map((s) => inputs[s].slice(0, 4).map((inp) => parseNum(inp.value)));
      let valid = true;
      S.forEach((v, s) => {
        const [a, n1, c, n0] = v;
        const ok = v.every((x) => Number.isFinite(x) && x >= 0 && Number.isInteger(x)) && n1 > 0 && n0 > 0 && a <= n1 && c <= n0;
        inputs[s].risk0.textContent = ok ? pp(a / n1) : "";
        inputs[s].risk1.textContent = ok ? pp(c / n0) : "";
        inputs[s].rr.textContent = ok && c > 0 ? rr2((a / n1) / (c / n0)) : (ok ? "nicht definiert" : "");
        if (!ok) valid = false;
      });
      if (!valid) { out.appendChild(el("p", { class: "epir-warning", text: "Bitte ganze Zahlen ≥ 0 eintragen; die Fälle dürfen die Personenzahl der Zeile nicht übersteigen, und jede Zeile braucht mindestens eine Person." })); return; }
      // crude table
      const A = S[0][0] + S[1][0], N1 = S[0][1] + S[1][1], C = S[0][2] + S[1][2], N0 = S[0][3] + S[1][3];
      if (C === 0 || S.some((v) => v[2] === 0)) { out.appendChild(el("p", { class: "epir-warning", text: "Ohne Fälle in einer Vergleichsgruppe ist das RR nicht definiert (Division durch 0)." })); return; }
      const rCrude = (A / N1) / (C / N0);
      const rrS = S.map(([a, n1, c, n0]) => (a / n1) / (c / n0));
      let num = 0, den = 0;
      S.forEach(([a, n1, c, n0]) => { const N = n1 + n0; num += a * n0 / N; den += c * n1 / N; });
      const mh = num / den;
      const ratio = Math.max(...rrS) / Math.min(...rrS);
      const change = Math.abs(rCrude - mh) / rCrude;
      const shareExp = S.map(([a, n1, c, n0]) => n1 / (n1 + n0));
      const riskStr = S.map(([a, n1, c, n0]) => (a + c) / (n1 + n0));

      const tbl = el("table", { class: "epir-2x2-table epir-strat-crude" });
      tbl.appendChild(el("caption", { class: "epir-strat-cap", text: "Alle zusammen (rohe Tabelle = Summe der Strata)" }));
      tbl.appendChild(el("tr", null, [el("th", { text: "" }), el("th", { text: current.outcome || "Fälle" }), el("th", { text: "Personen" }), el("th", { text: "Risiko" })]));
      tbl.appendChild(el("tr", null, [el("th", { text: current.exposure[1] }), el("td", { text: fmtInt(A) }), el("td", { text: fmtInt(N1) }), el("td", { text: pp(A / N1) })]));
      tbl.appendChild(el("tr", null, [el("th", { text: current.exposure[0] }), el("td", { text: fmtInt(C) }), el("td", { text: fmtInt(N0) }), el("td", { text: pp(C / N0) })]));
      out.appendChild(tbl);

      const res = el("div", { class: "epir-calc-body" });
      res.appendChild(el("p", { html: `<strong>Rohes RR:</strong> ${rr2(rCrude)} &nbsp;·&nbsp; <strong>Stratum-RRs:</strong> ${current.strata[0]}: ${rr2(rrS[0])}, ${current.strata[1]}: ${rr2(rrS[1])} &nbsp;·&nbsp; <strong>Mantel-Haenszel-RR (adjustiert):</strong> ${rr2(mh)}` }));
      res.appendChild(el("p", { class: "epir-small", html: `Mantel-Haenszel: Zähler Σ a·n₀/N = ${fmtDE(num, 2)}, Nenner Σ c·n₁/N = ${fmtDE(den, 2)}, Quotient ${rr2(mh)}.` }));
      res.appendChild(el("p", { class: "epir-small", html: `Prüfung der Confounder-Bedingungen: Anteil „${current.exposure[1]}“ je Stratum ${pp(shareExp[0])} vs. ${pp(shareExp[1])}; Risiko je Stratum (ohne Rücksicht auf die Exposition) ${pp(riskStr[0])} vs. ${pp(riskStr[1])}.` }));
      // classification
      let cls, txt;
      if (ratio > HOMOG) {
        cls = "epir-feedback epir-warning";
        txt = `<strong>Einordnung: Effektmodifikation.</strong> Die Stratum-RRs unterscheiden sich deutlich (Verhältnis ${rr2(ratio)} > ${fmtDE(HOMOG, 2)}). Dann ist ein gemeinsames adjustiertes RR irreführend – beide Stratum-RRs getrennt berichten.`;
      } else if (change >= CONF) {
        cls = "epir-feedback epir-warning";
        txt = `<strong>Einordnung: Confounding.</strong> Die Stratum-RRs sind ungefähr gleich (Verhältnis ${rr2(ratio)} ≤ ${fmtDE(HOMOG, 2)}), aber das adjustierte RR weicht um ${fmtDE(100 * change, 0)} % vom rohen ab (≥ ${fmtDE(100 * CONF, 0)} %). Das adjustierte RR ${rr2(mh)} ist die bessere Antwort.`;
      } else {
        cls = "epir-feedback epir-success";
        txt = `<strong>Einordnung: kein relevantes Confounding.</strong> Stratum-RRs ungefähr gleich (Verhältnis ${rr2(ratio)}) und adjustiertes RR nur ${fmtDE(100 * change, 0)} % vom rohen entfernt (< ${fmtDE(100 * CONF, 0)} %). Das rohe RR genügt.`;
      }
      res.appendChild(el("div", { class: cls, html: txt }));
      res.appendChild(el("p", { class: "epir-small epir-muted", text: `Schwellen (Konvention dieses Moduls, kein statistischer Test): Stratum-RRs „ungefähr gleich“, wenn größeres/kleineres RR ≤ ${fmtDE(HOMOG, 2)}; „Confounding“, wenn sich rohes und adjustiertes RR um mindestens ${fmtDE(100 * CONF, 0)} % unterscheiden.` }));
      if (current.lesson) res.appendChild(el("p", { class: "epir-why", html: current.lesson }));
      out.appendChild(res);
    }

    scenarios.forEach((sc) => {
      const b = el("button", { type: "button", class: "btn btn-sm btn-outline-primary", text: sc.label });
      b.addEventListener("click", () => load(sc));
      btnRow.appendChild(b);
    });
    const reset = el("button", { type: "button", class: "btn btn-sm btn-outline-secondary", text: "Zurücksetzen" });
    reset.addEventListener("click", () => load(current));
    btnRow.appendChild(reset);
    inputs.flat().forEach((inp) => inp.addEventListener("input", render));

    root.appendChild(btnRow);
    root.appendChild(head);
    root.appendChild(tables);
    root.appendChild(out);
    load(scenarios[0]);
  }

  // ---------- boot ---------------------------------------------------------

  // ---------- 8. Study simulator (sampling variation of a prevalence) -----
  // Random numbers come from Math.random() without a seed: every visit shows
  // different studies on purpose. Quiz items never depend on widget output.
  function initSample(root) {
    const id = nextId("epir-sample");
    const prev0 = parseNum(root.dataset.prev) || 18.1;   // true prevalence in %
    const n0 = parseInt(root.dataset.n, 10) || 100;
    const nIn = el("input", { type: "range", min: "20", max: "1000", step: "10", value: String(n0), id: `${id}-n`, class: "form-range" });
    const pIn = el("input", { type: "range", min: "1", max: "50", step: "0.1", value: String(prev0), id: `${id}-p`, class: "form-range" });
    const nVal = el("output", { for: `${id}-n`, class: "epir-slider-val" });
    const pVal = el("output", { for: `${id}-p`, class: "epir-slider-val" });

    const svgNS = "http://www.w3.org/2000/svg";
    const W = 360, H = 230, L = 14, R = 10, AX = H - 34, TOP = 62;
    const svg = document.createElementNS(svgNS, "svg");
    svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
    svg.setAttribute("class", "epir-pool-svg");
    svg.setAttribute("role", "img");
    svg.setAttribute("aria-label", "Verteilung der geschätzten Prävalenzen über viele simulierte Studien; blaue Linie: wahre Prävalenz; orange Marke: letzte Studie");
    const mk = (tag, attrs) => { const n = document.createElementNS(svgNS, tag); Object.entries(attrs).forEach(([k, v]) => n.setAttribute(k, v)); return n; };
    const gBars = mk("g", {});
    const gAxis = mk("g", {});
    const trueLine = mk("line", { class: "epir-sample-true", y1: TOP - 8, y2: AX });
    const trueLab = mk("text", { y: TOP - 14, "text-anchor": "middle", class: "epir-pool-label epir-sample-true-lab" });
    const lastMark = mk("path", { class: "epir-sample-last" });
    const lastLab = mk("text", { y: 14, "text-anchor": "middle", class: "epir-pool-value epir-sample-last-lab" });
    const lastSub = mk("text", { y: 30, "text-anchor": "middle", class: "epir-pool-label" });
    const emptyLab = mk("text", { x: W / 2, y: (TOP + AX) / 2, "text-anchor": "middle", class: "epir-pool-label" });
    emptyLab.textContent = "Noch keine Studie gezogen – klicken Sie auf einen Button.";
    [gBars, gAxis, trueLine, trueLab, lastMark, lastLab, lastSub, emptyLab].forEach((n) => svg.appendChild(n));

    let est = [];      // estimates in percent
    let xmax = 50;
    const out = el("div", { class: "epir-pool-out", "aria-live": "polite" });
    const sx = (v) => L + (v / xmax) * (W - L - R);

    function params() { return { n: Number(nIn.value), p: Number(pIn.value) / 100 }; }
    function drawStudy(n, p) { let k = 0; for (let i = 0; i < n; i++) if (Math.random() < p) k++; return k; }
    function sd(a) { if (a.length < 2) return NaN; const m = a.reduce((x, y) => x + y, 0) / a.length; return Math.sqrt(a.reduce((x, y) => x + (y - m) * (y - m), 0) / (a.length - 1)); }
    function quant(a, q) { const s = [...a].sort((x, y) => x - y); const pos = (s.length - 1) * q; const lo = Math.floor(pos), hi = Math.ceil(pos); return s[lo] + (s[hi] - s[lo]) * (pos - lo); }

    function render() {
      const { n, p } = params();
      const seTheory = 100 * Math.sqrt(p * (1 - p) / n);
      nVal.textContent = `n = ${fmtInt(n)} Personen je Studie`;
      pVal.textContent = `${fmtDE(100 * p, 1)} % (wahre Prävalenz in der Bevölkerung)`;
      // x axis: 0 .. xmax, wide enough for +-5 theoretical SE
      xmax = Math.min(100, Math.max(30, Math.ceil((100 * p + 5 * seTheory) / 10) * 10));
      gAxis.innerHTML = ""; gBars.innerHTML = "";
      gAxis.appendChild(mk("line", { x1: L, x2: W - R, y1: AX, y2: AX, class: "epir-sample-axis" }));
      const step = xmax <= 40 ? 5 : 10;
      for (let v = 0; v <= xmax; v += step) {
        gAxis.appendChild(mk("line", { x1: sx(v), x2: sx(v), y1: AX, y2: AX + 4, class: "epir-sample-axis" }));
        const t = mk("text", { x: sx(v), y: AX + 15, "text-anchor": "middle", class: "epir-pool-label" }); t.textContent = `${v}`; gAxis.appendChild(t);
      }
      const xt = mk("text", { x: W / 2, y: H - 4, "text-anchor": "middle", class: "epir-pool-label" }); xt.textContent = "geschätzte Prävalenz der einzelnen Studie (%)"; gAxis.appendChild(xt);
      trueLine.setAttribute("x1", sx(100 * p)); trueLine.setAttribute("x2", sx(100 * p));
      trueLab.setAttribute("x", sx(100 * p)); trueLab.textContent = `wahr: ${fmtDE(100 * p, 1)} %`;
      // histogram of estimates, 1-percentage-point bins
      const counts = new Map();
      est.forEach((e) => { const b = Math.min(xmax, Math.round(e)); counts.set(b, (counts.get(b) || 0) + 1); });
      const maxC = Math.max(1, ...counts.values());
      const bw = Math.max(1.5, (W - L - R) / xmax - 0.6);
      counts.forEach((c, b) => {
        const h = ((AX - TOP) * c) / maxC;
        gBars.appendChild(mk("rect", { x: sx(b) - bw / 2, y: AX - h, width: bw, height: h, class: "epir-sample-bar" }));
      });
      emptyLab.style.display = est.length ? "none" : "";
      if (est.length) {
        const last = est[est.length - 1];
        const x = sx(Math.min(xmax, last));
        lastMark.setAttribute("d", `M ${x} ${TOP - 4} l -6 -9 l 12 0 z`);
        lastMark.style.display = ""; lastLab.style.display = ""; lastSub.style.display = "";
        lastLab.setAttribute("x", Math.max(60, Math.min(W - 60, x)));
        lastSub.setAttribute("x", Math.max(60, Math.min(W - 60, x)));
        lastLab.textContent = `letzte Studie: ${fmtDE(last, 1)} %`;
        lastSub.textContent = `${fmtInt(Math.round((last / 100) * n))} von ${fmtInt(n)} Personen`;
      } else { lastMark.style.display = "none"; lastLab.style.display = "none"; lastSub.style.display = "none"; }
      // numbers
      out.innerHTML = "";
      const t = el("table", { class: "epir-calc-table" });
      const row = (k, v) => t.appendChild(el("tr", null, [el("th", { text: k }), el("td", { text: v })]));
      row("Gezogene Studien", fmtInt(est.length));
      if (est.length >= 2) {
        const m = est.reduce((a, b) => a + b, 0) / est.length;
        row("Mittel aller Schätzungen", `${fmtDE(m, 1)} %`);
        row("Streuung der Schätzungen (SD)", `${fmtDE(sd(est), 1)} Prozentpunkte`);
        if (est.length >= 40) row("95 % der Studien lagen zwischen", `${fmtDE(quant(est, 0.025), 1)} % und ${fmtDE(quant(est, 0.975), 1)} %`);
        row("Kleinste / größte Schätzung", `${fmtDE(Math.min(...est), 1)} % / ${fmtDE(Math.max(...est), 1)} %`);
      }
      row("Erwartete Streuung (Standardfehler, Formel)", `${fmtDE(seTheory, 1)} Prozentpunkte`);
      out.appendChild(t);
      if (est.length >= 100) {
        out.appendChild(el("p", { class: "epir-success epir-small", text: `Die Streuung der Schätzungen über viele Studien heißt Standardfehler. Bei n = ${fmtInt(n)} liegt er hier bei etwa ${fmtDE(sd(est), 1)} Prozentpunkten (Formel: ${fmtDE(seTheory, 1)}). Vervierfachen Sie n und ziehen Sie erneut – die Streuung sollte sich etwa halbieren.` }));
      } else if (est.length > 0) {
        out.appendChild(el("p", { class: "epir-info epir-small", text: "Jede Studie liefert eine andere Zahl – obwohl die wahre Prävalenz unverändert ist. Ziehen Sie 100 oder 1000 Studien, um das Muster der Streuung zu sehen." }));
      }
    }
    const btn = (txt, fn) => { const b = el("button", { type: "button", class: "btn btn-sm btn-outline-primary", text: txt }); b.addEventListener("click", () => { fn(); render(); }); return b; };
    const draw = (k) => { const { n, p } = params(); for (let i = 0; i < k; i++) est.push((100 * drawStudy(n, p)) / n); };
    const buttons = el("div", { class: "epir-pool-buttons" }, [
      btn("1 Studie ziehen", () => draw(1)),
      btn("100 Studien ziehen", () => draw(100)),
      btn("1000 Studien ziehen", () => draw(1000)),
      btn("Zurücksetzen", () => { est = []; nIn.value = String(n0); pIn.value = String(prev0); }),
    ]);
    const resetOnChange = () => { est = []; render(); };
    nIn.addEventListener("input", resetOnChange); pIn.addEventListener("input", resetOnChange);
    root.appendChild(el("div", { class: "epir-pool-body" }, [
      el("div", { class: "epir-pool-controls" }, [
        el("div", { class: "epir-slider" }, [el("label", { for: `${id}-n`, text: "Stichprobengröße n (Personen je Studie)" }), nIn, nVal]),
        el("div", { class: "epir-slider" }, [el("label", { for: `${id}-p`, text: "Wahre Prävalenz" }), pIn, pVal]),
        buttons,
        el("p", { class: "epir-small", text: "Ändern Sie n oder die Prävalenz, beginnt die Sammlung von vorn." }),
      ]),
      el("div", { class: "epir-pool-vis" }, [svg, out]),
    ]));
    render();
  }

  // ---------- 9. Estimate (prequestion) ----------------------------------
  // One guess per data-id, kept in memory only (no storage, no transfer).
  const estimates = new Map();

  function initEstimate(root) {
    const id = root.dataset.id;
    if (!id) return;
    const unit = root.dataset.unit || "";
    const min = root.dataset.min != null ? Number(root.dataset.min) : -Infinity;
    const max = root.dataset.max != null ? Number(root.dataset.max) : Infinity;
    const next = root.dataset.next || "";
    const inputId = nextId("epir-est");

    const prompt = el("div", { class: "epir-estimate-prompt", html: root.innerHTML });
    root.innerHTML = "";
    const input = el("input", { type: "text", inputmode: "decimal", id: inputId, class: "form-control form-control-sm epir-numinput", autocomplete: "off", placeholder: "Zahl" });
    const btn = el("button", { type: "button", class: "btn btn-sm btn-primary", text: "Schätzung notieren" });
    const out = el("div", { class: "epir-feedback", "aria-live": "polite", role: "status" });
    const row = el("div", { class: "epir-numrow" }, [
      el("label", { for: inputId, text: "Meine Schätzung:" }),
      input,
      unit ? el("span", { class: "epir-unit", text: unit }) : null,
      btn,
    ]);

    function submit() {
      const v = parseNum(input.value);
      if (!Number.isFinite(v) || v < min || v > max) {
        out.className = "epir-feedback epir-info";
        out.innerHTML = Number.isFinite(min) && Number.isFinite(max)
          ? `Bitte geben Sie eine Zahl zwischen ${fmtDE(min, 0)} und ${fmtDE(max, 0)} ein.`
          : "Bitte geben Sie eine Zahl ein.";
        return;
      }
      estimates.set(id, v);
      out.className = "epir-feedback epir-success";
      out.innerHTML = `<strong>Notiert: ${fmtDE(v, Number.isInteger(v) ? 0 : 1)}${unit ? " " + unit : ""}.</strong> Ob Sie richtig liegen, sehen Sie weiter unten${next ? " im " + next : ""} – lesen Sie bis dahin weiter.`;
      document.dispatchEvent(new CustomEvent("epir-estimate", { detail: { id, value: v } }));
    }
    btn.addEventListener("click", submit);
    input.addEventListener("keydown", (e) => { if (e.key === "Enter") { e.preventDefault(); submit(); } });

    root.appendChild(el("div", { class: "epir-estimate-head", html: '<span class="epir-item-tag">Erst schätzen</span> Noch nicht wissen ist hier normal – eine grobe Zahl reicht.' }));
    root.appendChild(prompt);
    root.appendChild(row);
    root.appendChild(out);
  }

  function initEstimateReveal(root) {
    const ref = root.dataset.ref;
    if (!ref) return;
    const answer = Number(root.dataset.answer);
    const answerText = root.dataset.answerText || String(root.dataset.answer);
    const unit = root.dataset.unit || "";
    const diffUnit = root.dataset.diffUnit || "";
    // "close enough" band: data-tol (absolute), default max(1, 10 % of the answer) as in modules 1-5a
    const tol = root.dataset.tol != null && Number.isFinite(Number(root.dataset.tol)) ? Number(root.dataset.tol) : Math.max(1, 0.1 * Math.abs(answer));
    function render() {
      const v = estimates.get(ref);
      if (v == null) {
        root.innerHTML = `<span class="epir-item-tag">Auflösung</span> Tatsächlich: <strong>${answerText}</strong>. Sie haben am Modulanfang keine Schätzung notiert – das ist in Ordnung; beim nächsten Modul lohnt sich der Versuch.`;
        return;
      }
      const diff = v - answer;
      const absDiff = Math.abs(diff);
      const digits = Number.isInteger(v) && Number.isInteger(answer) ? 0 : 1;
      let verdict;
      if (absDiff <= tol) verdict = "Sehr nah dran.";
      else if (diff > 0) verdict = `Ihre Schätzung lag um ${fmtDE(absDiff, digits)}${diffUnit ? " " + diffUnit : ""} zu hoch.`;
      else verdict = `Ihre Schätzung lag um ${fmtDE(absDiff, digits)}${diffUnit ? " " + diffUnit : ""} zu niedrig.`;
      root.innerHTML = `<span class="epir-item-tag">Auflösung</span> Ihre Schätzung: <strong>${fmtDE(v, digits)}${unit ? " " + unit : ""}</strong> · tatsächlich: <strong>${answerText}</strong>. ${verdict} Eine Abweichung ist kein Fehler – sie zeigt, wo das Konzept die Intuition korrigiert.`;
    }
    document.addEventListener("epir-estimate", (e) => { if (e.detail && e.detail.id === ref) render(); });
    render();
  }

  // Open a collapsed Quarto callout when the page is entered via a hash that points
  // to it or to a wrapper div around it (e.g. module/01_...html#niere-kurz).
  function openCalloutFromHash() {
    const id = decodeURIComponent(location.hash || "").slice(1);
    if (!id) return;
    const target = document.getElementById(id);
    if (!target) return;
    const header = target.querySelector(".callout-header.collapsed[data-bs-toggle='collapse']");
    const body = target.querySelector(".callout-collapse");
    if (!header || !body) return;
    header.classList.remove("collapsed");
    header.setAttribute("aria-expanded", "true");
    body.classList.add("show");
    target.scrollIntoView({ block: "start" });
  }

  function boot() {
    openCalloutFromHash();
    window.addEventListener("hashchange", openCalloutFromHash);
    document.querySelectorAll(".epir-quiz").forEach(initQuiz);
    document.querySelectorAll(".epir-estimate").forEach(initEstimate);
    document.querySelectorAll(".epir-estimate-reveal").forEach(initEstimateReveal);
    document.querySelectorAll(".epir-calc").forEach(initCalc);
    document.querySelectorAll(".epir-pool").forEach(initPool);
    document.querySelectorAll(".epir-2x2").forEach(initTwoByTwo);
    document.querySelectorAll(".epir-relabs").forEach(initRelAbs);
    document.querySelectorAll(".epir-dx").forEach(initDx);
    document.querySelectorAll(".epir-ppv").forEach(initPpv);
    document.querySelectorAll(".epir-sample").forEach(initSample);
    document.querySelectorAll(".epir-design").forEach(initDesign);
    document.querySelectorAll(".epir-order").forEach(initOrder);
    document.querySelectorAll(".epir-dag").forEach(initDag);
    document.querySelectorAll(".epir-strat").forEach(initStrat);
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", boot);
  else boot();
})();
