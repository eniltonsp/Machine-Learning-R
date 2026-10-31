# Data contract

Inputs are complaint-level rows, not aggregate company indicators. Repeated company names are expected. Company summaries such as `Zeeng Reclame Aqui.csv` require a separate indicator contract and must not be passed as individual complaints.

| Canonical column | Portuguese aliases | Meaning / constraint |
|---|---|---|
| company | Empresa | Required nonempty company name |
| status | Status | Required nonempty lifecycle status |
| date | Data | Optional complaint date; ISO or DD/MM/YYYY |
| company_id | IdEmpresa | Optional source company identifier |
| complaint_id | reclamacao_id | Optional unique complaint identifier; checked for training |
| city | Localidade, Localizacao | Optional location; do not infer state from city alone |
| state | Estado | Optional in dashboard, required model column |
| text | Reclamacao | Optional free text; may contain personal information |
| title | Titulo | Optional complaint title |
| segment | Segmento, Empresa Ramo | Required model column |
| days_response | Qtd dias resposta | Nonnegative numeric, missing values allowed for training imputation |
| days_resolution | Qtd dias conclusao | Nonnegative numeric, retrospective |
| rating | Nota Cliente | Numeric from 0 to 10, retrospective |
| return_intent | Voltaria a fazer negocio | Individual Yes/No or Sim/Não, complete for training |

Numeric decimal commas are accepted within quoted fields or semicolon-separated input. Thousands separators in model features are not accepted. Category case and spelling are preserved: upstream data should use a consistent vocabulary. Portuguese source statuses are mapped to English: Resolvido → Resolved; Não Resolvido → Unresolved; Respondida → Answered; Não respondida → Unanswered; Em réplica → In discussion. English canonical statuses are accepted directly. Unknown statuses are flagged and preserved.

Company resolution share over all complaints = Resolved / all rows. Evaluated resolution share = Resolved / (Resolved + Unresolved); undefined when denominator is zero. Neither is automatically identical to the Reclame Aqui official company indicator.

Dates without values remain in overview totals and are omitted from the timeline. Invalid supplied dates fail validation. No automatic deletion, deduplication or city-only state join occurs. Same-named municipalities can belong to different states.

Readers preserve quoted multiline text. R-exported TSV files use backslash-escaped quotes; the reader enables that convention by default for tabs, and the upload interface provides an explicit toggle. Standard CSV uses doubled quotes. Duplicate canonical names, malformed input and out-of-range model numbers are rejected. Exported character cells starting with formula-like characters are prefixed with an apostrophe.
