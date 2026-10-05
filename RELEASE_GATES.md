# Release gates

## Required before public release

- Author approval of the exact repository revision and the disclosed scientific limitations.
- Recorded author decision: MIT, Copyright (c) 2026 Steffen Held. No separate code DOI. Manuscript authorship remains separately confirmed.
- Code, metadata and aggregate-only content review. HRS access terms remain separate from code licensing.
- Package integrity checks, synthetic R tests, Python table validation and package-tool tests on a fresh checkout.
- Record the tested environment and results without implying a fresh-machine install or private-data reproduction.
- Publish a fixed release with tag and full commit hash after the repository review. Then verify access without relying on the author's authenticated session.
- Only then replace manuscript, S13/S14 and cover-letter access placeholders with the actual URL, tag and full commit hash.

## Deliberate limits

Historical analysis code and comparison fixtures are retained for traceability. The corrected implementation is the supported execution choice. Historical source snapshots are needed by strict recovery checks and are not alternative current results. Do not suppress mismatches to make a rerun pass.

No automatic cloud analysis or CI installation is configured. Existing tests use invented data. Additional real-data runs must be executed privately by an authorized researcher. Confidential inputs are never needed to browse or inspect the code.

Public access is not completed by creating a private repository. No DOI is planned under the author decision. Absence of confirmed defects is not a guarantee of error freedom or journal acceptance.
