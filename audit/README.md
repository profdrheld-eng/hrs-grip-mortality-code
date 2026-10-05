# Additional review checks

From the repository root:

```sh
python3 -B audit/verify_results.py
python3 -B audit/test_verify_results.py
python3 -O -B audit/test_verify_results.py
python3 -B audit/verify_annotations.py
python3 -B package/materialize_version.py corrected ../hrs-audit-work
Rscript audit/statistical_oracles.R ../hrs-audit-work
```

Use a new materialization destination. The additional R checks use only invented records. They compare the sorting-based weighted AUC to a quadratic case-control oracle on 200 randomized fixtures and compare both IPCW functions to a separate step-product definition on 300 tied-time fixtures. Existing package tests also compare to survival-package references and check neural gradients by finite differences.

The result verifier checks released CSV hashes and arithmetic. Its six regression tests deliberately corrupt a contrast, Holm probability, model key, primary interval-containment flag or file bytes. Expected rejection also holds under Python optimization. It does not calculate new model fits, prove input measurement validity or validate conditional confidence-interval coverage.

The scientific implementation inside package/ remains unchanged. Synthetic tests do not replace the author's real-data correction/replay checks. See the repository review report for scope and remaining limits.
