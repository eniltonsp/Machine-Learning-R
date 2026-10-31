# Provenance and evidence boundaries

Author: Enilton de Oliveira Machado. Original academic development: FIAP Big Data MBA, 2021. Sole implementation is the author's statement. This modernisation preserves original source under `legacy/` and adds a separate working application.

Source review of the latest attachments found 11,699 records across 14 companies in the complaint CSV, 8,574 records across nine companies in the prepared TXT, and 18 companies in the aggregate indicator CSV. These are distinct snapshots and must not be silently combined. The TXT additionally contains extraction date and elapsed-day fields; these are not a replacement for individual response/completion dates or return-intent labels. Geographic reference files contain 5,570 municipality rows and 27 federative units. The attachment named `Reclame Aqui - lista` without extension is a PDF report, not a machine-readable training table.

`source-profile.json` records attachment filenames, row counts, column names and SHA-256 checksums only. It contains no consumer text. Raw source attachments and legacy report documents are not redistributed. Hashes identify the reviewed inputs, not authenticity of the underlying observations.

The demo CSV was generated deterministically with Python standard-library random seed 20261031, 600 rows, four fictional company labels and dates in 2025. These records are fictional and unsuitable for validating a model. No scraped consumer narratives are included in the public example.

Modernisation does not retroactively establish 2021 functionality, deployment, business adoption or economic impact. Actual results should be linked to a source commit, dataset fingerprint, environment and evaluation report. Do not apply the historical HTML metrics to newly trained models.
