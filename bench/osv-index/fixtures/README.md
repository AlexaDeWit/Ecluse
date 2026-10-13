# Metadata comparison fixture

`captured-npm-snapshot.json` is the unchanged npm corpus A warm-up snapshot from
[run 38378957858](https://github.com/AlexaDeWit/Ecluse/actions/runs/38378957858).
That run completed its native observations but failed metadata qualification.
Its rejected results remain evidence, not qualified performance measurements.

The fixture retains the actual authority-only and full-URL metadata fields.
Controls compare the snapshot with itself and mutate one metadata fact at a time.
The recorded row and schema fields stay unchanged in those controls.
