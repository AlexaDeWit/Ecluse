# Temporary focused acceptance

The `focused-acceptance` job in `bench.yml` runs only on `measure/refactor-focused-validation`. Do not merge it into a product branch.
The `focused_source` dispatch input requires the reviewed candidate's full 40-character commit SHA. There is no default source. Set `focused_validation=true` to select this job.
Every patch also requires the original file's recorded SHA-256. A source correction requires review of any affected patch.

Task owns the serial sequence. The workflow supplies one ARM runner, the pinned toolchain, and artifact upload.
The workflow passes the candidate directory through Task's `--dir` option.
Preflight captures its log and exit status before checking identity, source contracts, or tools.
The complete integration target retains its declaration audit. The redirect mutation accepts exactly one named failed case.
Core runtime checks require a non-empty Hspec summary. Pending tests, build failures and timeouts cannot qualify.

The compiler processes use separate checkouts and builds with the same temporary parent.
Both first examples create directories and wait on separate FIFOs before either process proceeds.
The harness records overlapping directory lifetimes, separate native statuses, every observed directory, and final cleanup.
A second pair injects one assertion failure inside the left directory bracket. Both restored copies then run the original spec.

The six concurrency groups run three times with distinct Hspec seeds and one GHC capability.
The focused build adds `-rtsopts` in `dist-focused`. These runs are not performance evidence.
Hspec seeds do not establish the Hedgehog seed. The raw logs retain any reported property seed.
Four caller controls delay entry with MVars. A separate renewal control holds the backend response before the next clock step.

`cases.json` lists each patch, selected example, and expected result for review.
The Task file explicitly schedules those cases. The native generator smoke case must execute without pending outcomes.
No extra worker or shutdown mutation replaces the committed controls in the full integration run.
Artifacts retain the source and tree, exact patches, build and runtime logs, native statuses, and restored outcomes.
A failed stage stops qualification. The upload step keeps available partial evidence.

The candidate must descend from these reviewed support commits:

- #1672: `eda18683ca81e0f38ef5ee64ced7e373514f868c`.
- #1674: `d6caea0fe85624275a4256db6eb37a174a797d4e`.
- #1673: `aee7742706e9b8fcb4b270afce05ada3ac493244`.

The harness owns the required runtime identities in `required-*-controls.txt`.
Unit logs must contain each required example once and have zero failures or pending results.
This includes eight version-loader controls, three ecosystem comparisons, the delayed-refresh control, and the generator consistency case.
Integration reports must contain each required identity once with status `passed`.
Only the redirect mutation requires `integration/egress/native-003` to fail. Its separate check still requires the named 302-to-503 assertion.
These requirements supplement the complete integration audit. A candidate cannot remove them from its own catalogue.
