# Codebook: `ckd_kohorte.csv`

**Synthetischer Datensatz – keine echten Patientendaten.** Erzeugt mit `data/simulate_ckd_cohort.R` (Seed 20261026). Szenario: bevölkerungsbasierte Kohorte, Erwachsene 40–85 Jahre, Screening auf chronische Nierenkrankheit (CKD) zu Studienbeginn, Nachbeobachtung bis 5 Jahre.

Eine Zeile = eine teilnehmende Person (n = 2000).

| Variable | Typ | Beschreibung | Werte / Einheit |
|---|---|---|---|
| `id` | character | Pseudonyme Teilnehmer-ID | `P0001` … `P2000` |
| `alter` | integer | Alter bei Studienbeginn | Jahre (40–85) |
| `geschlecht` | character | Geschlecht | `w`, `m` |
| `diabetes` | integer | Diabetes mellitus bekannt | 0 = nein, 1 = ja |
| `hypertonie` | integer | Arterielle Hypertonie bekannt | 0 = nein, 1 = ja |
| `egfr` | numeric | Geschätzte glomeruläre Filtrationsrate (eGFR) bei Studienbeginn | mL/min/1,73 m² |
| `uacr` | numeric | Albumin-Kreatinin-Quotient im Urin (UACR) bei Studienbeginn, Referenzstandard für Albuminurie | mg/g |
| `ckd_baseline` | integer | CKD bei Studienbeginn, vereinfachte Definition: `egfr < 60` **oder** `uacr >= 30` | 0 = nein, 1 = ja |
| `teststreifen_pos` | integer | Ergebnis eines Urin-Teststreifens auf Albumin (Screening-Test). Simuliert mit Sensitivität 0,80 und Spezifität 0,90 gegenüber der Referenz `uacr >= 30` | 0 = negativ, 1 = positiv |
| `followup_jahre` | numeric | Beobachtungszeit bis inzidente CKD oder Zensierung. **Nur** für Personen ohne CKD bei Studienbeginn; sonst leer (`NA`) | Jahre (0–5) |
| `ckd_inzident` | integer | Neu aufgetretene CKD während der Nachbeobachtung. **Nur** für Personen ohne CKD bei Studienbeginn; sonst `NA` | 0 = nein (zensiert), 1 = ja |

## Hinweise für Lehrende

- Die CKD-Definition ist bewusst vereinfacht (Einzelmessung statt Persistenz über ≥ 3 Monate). In den Modulen wird dies als Vereinfachung gekennzeichnet; die vollständige KDIGO-Definition ist in der jeweiligen Quellenangabe verlinkt.
- Die Testgüte des Teststreifens ist fix simuliert und entspricht nicht einem konkreten Produkt.
- Erwartete Kennzahlen (bei unverändertem Seed): CKD-Prävalenz ≈ 0,18; Inzidenzrate ≈ 18–19 pro 1000 Personenjahre; Sensitivität ≈ 0,79; Spezifität ≈ 0,90. Bei Änderung des Simulationsskripts müssen die Auto-Grading-Toleranzen in den Modulen überprüft werden.

## Lizenz

Daten und Skript: CC BY 4.0 (Daten) bzw. MIT (Skript), siehe `LICENSE` und `LICENSE-CONTENT` im Projektstamm.
