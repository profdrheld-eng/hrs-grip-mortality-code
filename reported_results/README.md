# Corrected manuscript results, 5 October 2026

These six aggregate CSV files are exact copies of the corrected manuscript table sources. They contain model-level estimates, intervals and conditional exploratory tests, not participant rows. SHA256.json records the selected bytes.

Run `python3 -B audit/verify_results.py` from the repository root to check hashes, keyed model contrasts, primary paired differences, interval order, the complete 360-row Holm grid and full-training learning-curve point agreement. These checks establish internal agreement among released aggregates. They do not independently reproduce fits from private HRS inputs or prove confidence-interval coverage.

Table 2 is the primary nested optimism-corrected analysis. Table 3 is exploratory out-of-fold prediction. Table S2 uses conditional basic 90% intervals. S3 contains hypothetical margin tests, not proof of clinical equivalence. S4 uses 500-draw conditional learning-curve intervals, whereas Table 3 uses 5,000 draws. S8 is descriptive containment of primary 95% intervals. Do not interchange these interval types or analysis populations.

Historical fixtures inside package/ remain for exact replay guards. These corrected results are the current reporting reference. Figures and complete manuscript text are outside this code repository.
