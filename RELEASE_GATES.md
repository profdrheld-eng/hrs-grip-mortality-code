# Release gates

## Required before public release

- Author approval of the exact repository revision and the disclosed scientific limitations.
- Author agreement on reuse license and code contributors/citation metadata. Manuscript authorship and software authorship need not be identical.
- Code, metadata and aggregate-only content review. HRS access terms remain separate from code licensing.
- Package integrity checks, synthetic R tests, Python table validation and package-tool tests on a fresh checkout.
- Record the tested environment and results without implying a fresh-machine install or private-data reproduction.
- Publish a fixed release and archive that version with a DOI after approval. Then verify access without relying on the author's authenticated session.
- Only then replace manuscript, S13/S14 and cover-letter access placeholders with the actual URL, version and DOI.

## Deliberate limits

Historical analysis code and comparison fixtures are retained for traceability. The corrected implementation is the supported execution choice. Historical source snapshots are needed by strict recovery checks and are not alternative current results. Do not suppress mismatches to make a rerun pass.

No automatic cloud analysis or CI installation is configured. Existing tests use invented data. Additional real-data runs must be executed privately by an authorized researcher. Confidential inputs are never needed to browse or inspect the code.

Public access and DOI are not completed by creating a private repository. Absence of confirmed defects is not a guarantee of error freedom or journal acceptance.
