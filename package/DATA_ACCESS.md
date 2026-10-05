# Data access and local handling

This is a code-only review draft with public variable metadata and explicitly allowlisted aggregate reference tables and audit metadata. The HRS data files must be obtained independently through [HRS Data Downloads](https://hrsdata.isr.umich.edu/data-products). The user needs their own registration and must follow the [current HRS Conditions of Use](https://hrsdata.isr.umich.edu/data-products/conditions-of-use), including any additional restrictions for the selected product. Do not share credentials or redistribute HRS inputs through this package.

The source importer expects exactly one case-insensitive match for each required file anywhere under the chosen private data root:

| Product | Files |
|---|---|
| 2010 Core Final Release Version 6.0 | `h10i_r.da`, `h10i_r.sas` |
| 2014 Core Final Release Version 2.0 | `h14i_r.da`, `h14i_r.sas`, `h14a_r.da`, `h14a_r.sas`, `h14c_r.da`, `h14c_r.sas`, `h14pr_r.da`, `h14pr_r.sas` |
| 2022 Tracker Version 1 | `trk2022tr_r.csv` |

The SAS files provide fixed-column metadata; the R parser does not execute SAS. Tracker analysis uses named CSV columns, not the earlier ASCII importer. Variable metadata are in `reported_version/02_Analyse/metadata/stage2_variables.csv`. Some optional provenance or format-audit tools inspect additional public setup/codebook files when present. A file name and plausible version are not independent proof of release integrity; retain download records and local input hashes.

The [HRS AI and LLM Use Policy](https://hrsdata.isr.umich.edu/data-products/ai-llm-use-policy), checked 4 October 2026, permits code assistance and suitable metadata/aggregate work but prohibits person-level data use by LLMs. Keep raw data and derived person-level records outside any AI-accessible or indexed working directory. Execute real-data analysis manually in a permitted local R environment. Do not send participant rows, fitted objects carrying records, private logs, or identifier crosswalks to an AI tool.

Scripts differ in output sensitivity. `hrs_provenance_overlap_audit.R` creates an identifier crosswalk; `hrs_reporting_patch.R` creates exact local outputs/model specifications; figure-replay scripts can make participant-level plots. These scripts are included so reviewers can inspect the implemented logic, but none of those real outputs is included. Use a private output root and review output suitability before any sharing.

Bundled CSV exceptions are allowlisted: variable metadata, runtime versions, `table2_source.csv` (15 model/metric aggregate rows), `tableS7_tuning.csv` (selected fold configurations and rounded sample/event counts), and `fold_counts.csv` (120 reconstructed fold-role rows with rounded counts, in the corrected overlay). The historical aggregate tables are references for pre-correction reconstruction, not corrected results. The corrected overlay also contains the released impact-audit STATUS and PRIMARY_REPLAY_STATUS files, runtime versions, source hashes and aggregate primary apparent reweighting table. These are run metadata and model-level metrics, with no participant records. All test records are invented inside source code.

No public code license has been assigned. Authors must agree permitted distribution and reuse before release. HRS data permissions are separate from any later code license.
