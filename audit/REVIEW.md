# Critical repository re-audit, 5 October 2026

## Review verdict

One confirmed repository portability defect was reproduced: Git automatic newline conversion invalidated the strict package hashes on checkout. Two documentation/traceability weaknesses were also repaired: missing current corrected aggregate reporting references and insufficiently explicit core-code explanation. No new error in the inspected core statistical calculations was established by the additional checks. This is not an absolute guarantee of error freedom, clinical validity or acceptance.

The review covers the actual GitHub revision 13a446ca739fd99185e4dbe1c88ec1d42c5a3223 and its targeted revision. It is a focused scientific/code review by the same assistant, not a separate human computer scientist or independent full raw-data replication.

## R2-REPO-05: newline conversion invalidates provenance, substantial, confirmed defect

Location: repository root previously lacked .gitattributes, while package/verify_local_package.py requires exact bytes for every manifest entry. A fresh clone with core.autocrlf=true failed at the missing/extra/modified-file gate. It could prevent reviewers from even materializing the code. The fix sets -text for the complete package and the hash-verified reporting aggregates. It does not weaken any verifier or rewrite analytical source. Verification requires both normal and core.autocrlf=true fresh clones to pass unchanged package and aggregate verifiers. This does not establish Windows runtime support.

## R2-REPO-06: current corrected result references absent, reporting weakness, repaired

Location: original repository retained historical metric fixtures for replay, but did not expose a clearly labeled current corrected aggregate reference set. Six corrected CSV sources are now included under reported_results/ with exact provenance hashes and explicit interval-type distinctions. Read-only independent arithmetic checks cover 15 primary rows, 12 variants, 40 paired contrasts, 360 conditional margin tests, Holm families, learning-curve full-fraction points and primary interval-containment flags. The negative tests intentionally alter values and update their hashes, so arithmetic tests cannot pass merely because hashes are regenerated. This does not reproduce model fits from private inputs.

## R2-REPO-07: explanatory comments versus immutable replay sources, resolved design constraint

Location: core analysis and recovery source hashes. Editing comments inside frozen sources changes their exact identity and can break historical replay checks. Four annotated Markdown views add 36 substantive comments without editing executable analytical bytes. A verifier strips only inserted REVIEW NOTE comments and requires exact source agreement. The user-approved AI statement documents assistance with coding, comments and reproducibility checks. The views are clearly non-executable documentation, avoiding a second competing analysis version.

## Statistical checks and counterarguments

- Weighted AUC: independent quadratic case-control concordance versus optimized sorting on 200 randomized invented samples, including ties, zero weights and row permutations.
- IPCW: both implementations versus an independent step-product definition on 300 invented tied-time cases across lower/midpoint/upper conventions. Existing tests additionally use survival-package references.
- Neural AFT: inspected log-interval likelihood, gradient construction, parameter order and risk transform. Existing finite-difference, probability and zero-tree XGBoost checks are rerun in the full suite.
- Data flow: inspected follow-up coarsening, death-source rules, household resampling, training-only preprocessing, inner tuning and augmentation. All full-package R scripts parse. Existing artificial-data tests cover import/schema rejection, group leakage, generator separation and replay guards. Original measurement validity and unobserved selection bias are not proven.
- Inference: inspected paired fixed-prediction resampling, basic versus percentile intervals, centered boundary tests and within-metric/margin Holm adjustment. Missing full training/tuning uncertainty, marginal independent censoring assumptions and hypothetical clinical margins remain scientific limitations, not repaired by the repository.
- Side effects: reviewed package materialization/verification/process cleanup and recovery output paths. No network transmission was added. User-supplied historical source/run directories are trusted local inputs. Recovery logs and participant-level outputs must stay private. Strict source/replay guards remain active.

## Boundaries and release status

All 76 originally packaged analytical source/fixture files remain byte-identical to the prior verified package. Only explanatory/package documentation and additional read-only checks/released aggregates were added. No fresh HRS model fitting, new model family, new tuning grid or manuscript numerical change was introduced.

MIT licensing and Copyright (c) 2026 Steffen Held follow the author decision. No separate DOI is planned. The final repository revision should be cited by release tag and full commit hash. Public availability is distinct from private upload and must be verified when enabled.

Verification completed locally: 26 R scripts, 24 package helper tests, six aggregate-validation tests in normal and optimized Python, 200 randomized AUC and 300 IPCW checks. The newline-conversion failure was reproduced before the fix and the exact same package verifier passed after the Git-attribute fix. All six reporting CSVs and four annotation views also passed in that fresh checkout. The manuscript-to-Word/source verification was rerun successfully. Known unchanged limits include no fresh-machine dependency installation test, no full Windows/Linux execution test, no independent second private-data analysis, no full-pipeline equivalence calibration and no guarantee of absence of every possible defect. No new full HRS run is justified solely by the confirmed newline/documentation findings.
