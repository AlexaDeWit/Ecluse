# Temporary focused acceptance

The `focused-acceptance` job in `bench.yml` runs only on `measure/refactor-focused-validation`. Do not merge it into a product branch.
The `focused_source` dispatch input requires the reviewed candidate's full 40-character commit SHA. There is no default source. Set `focused_validation=true` to select this job.
The candidate is pinned to `ab5f4af6e55b90ae79392f3d10a9505295f1239f` in `source-commit.txt`.
Every patch also requires the original file's recorded SHA-256. A source correction requires review of any affected patch.

The workflow currently invokes the fixed `remaining` entrypoint and labels its artifact `focused-remaining`.
It runs source preflight, the core build, and clean lease, reader and queue controls before eight attempts.
Those attempts are delayed renewal, both R1 field mutations, and the five queue mutations.
It does not execute compiler, full integration, or earlier mutation checks. Their evidence must come from separate runs of the same source.
The `run` entrypoint retains the complete plan. Both entrypoints share `remaining-controls` and `remaining-attempts` without duplicating assertions.
Each entrypoint records its scope in `scope.txt`.

Task owns the serial sequence. The workflow supplies one ARM runner, the pinned toolchain, and artifact upload.
The workflow passes the candidate directory through Task's `--dir` option.
Preflight captures its log and exit status before checking identity, source contracts, or tools.
The complete integration target retains its declaration audit. The redirect mutation accepts exactly one named failed case.
Core runtime checks require a non-empty Hspec summary. Pending tests, build failures and timeouts cannot qualify.

The compiler processes use separate checkouts and builds with the same temporary parent.
Both first examples create directories and wait on separate FIFOs before either process proceeds.
The parent keeps FIFO writers open, and each example reads exactly one token byte without waiting for EOF.
The harness records overlapping directory lifetimes, separate native statuses, every observed directory, and final cleanup.
Failure cleanup joins both process owners before qualification. Those owners stop and join the native process groups.
The parent records each gate's lifecycle and removes its FIFO after the children stop. Regular evidence files remain available for upload.
A second pair injects one assertion failure inside the left directory bracket. Both restored copies then run the original spec.

The six concurrency groups run three times with distinct Hspec seeds and one GHC capability.
The focused build adds `-rtsopts` in `dist-focused`. These runs are not performance evidence.
The pinned hspec-hedgehog 0.3.0.0 bridge derives Hedgehog seeds from Hspec replay arguments.
Each run records its Hspec seed and retains the native failure report.
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
- Final test correction: `307e532a3c7863adf11e5ecc4bc6b033c2361b86`.

The harness owns the required runtime identities in `required-*-controls.txt`.
Unit logs must contain each required example once and have zero failures or pending results.
This includes eight version-loader controls, three ecosystem comparisons, the delayed-refresh control, and the generator consistency case.
Integration reports must contain each required identity once with status `passed`.
Only the redirect mutation requires `integration/egress/native-003` to fail. Its separate check still requires the named 302-to-503 assertion.
These requirements supplement the complete integration audit. A candidate cannot remove them from its own catalogue.

The R1 controls select the original generated-packument property and the original generated-Simple-index property.
They use seeds 701 and 702, respectively, for each clean, perturbed, and restored run.
Each scratch patch changes one field in a non-empty successful walker result. It preserves the byte count, result class, and field count.
The independent json-stream reference and both 2,000-case declarations remain unchanged.
The other two PyPI properties retain their existing checks and are not these mutation targets.
Clean and restored controls must report one passing example and 2,000 successful tests.
A rejected mutation must report one failed example, the expected property name, a Hedgehog failure, and the emitted-field marker.
Build errors, missing selections, pending cases and runtime timeouts cannot satisfy that check.
The complete integration runs still require worker `native-002`, now with the acknowledgement check from the final test correction.

The queue controls use three production-decoder patches for #1326.
They allow a missing legacy filename, a missing traceparent, or a string traceContext, respectively.
Five separate mutation runs cover the legacy filename example and both schema variants of each trace example.
Each patched decoder must pass both valid-control examples before the named rejection runs.
The expected failure must show that the decoder returned a MirrorJob instead of refusing the intended field.
The fixtures, URL validation, filename safety check and other trace fields remain unchanged.
Restored controls rerun both valid examples and the selected rejection. The clean group independently requires all nine field-control identities.
