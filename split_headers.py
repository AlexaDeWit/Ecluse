import re

worker_hs = "core/src/Ecluse/Core/Worker.hs"
integrity_hs = "core/src/Ecluse/Core/Worker/Integrity.hs"
loop_hs = "core/src/Ecluse/Core/Worker/Loop.hs"
job_hs = "core/src/Ecluse/Core/Worker/Job.hs"

def read_file(path):
    with open(path, "r", encoding="utf-8") as f:
        return f.read()

def write_file(path, content):
    with open(path, "w", encoding="utf-8") as f:
        f.write(content)

# 1. Update Worker.hs
w_content = read_file(worker_hs)
new_w_content = re.sub(
    r"== The integrity gate is the security crux.*?See @docs",
    """See individual modules for detailed behaviour:
* "Ecluse.Core.Worker.Integrity" for the security gate on artifact digests.
* "Ecluse.Core.Worker.Loop" for supervision and graceful shutdown.
* "Ecluse.Core.Worker.Job" for ack semantics within the visibility budget.

See @docs""",
    w_content,
    flags=re.DOTALL
)
write_file(worker_hs, new_w_content)

# 2. Update Integrity.hs
int_doc = """{- | The integrity gate is the security crux of the worker.

A mirrored artifact is later served from the private upstream __without re-running
the rules__, so a corrupt or tampered artifact must never enter it. Verification is
therefore the gate: a hash __mismatch fails the job with no publish__ and is logged
loudly. Because the digest is the __serve-time-admitted__ one carried on the job,
the worker mirrors exactly the bytes the rules cleared -- an upstream packument
mutated in the enqueue → process window cannot substitute a different artifact.
-}
"""
int_content = read_file(integrity_hs)
write_file(integrity_hs, int_doc + int_content)

# 3. Update Loop.hs
loop_doc = """{- | Loop robustness and supervision for the worker.

The loop is wrapped so a single bad iteration cannot kill the worker thread: a
transient @receive@ / fetch / publish error, or an undecodable body, is caught,
logged, and the loop backs off and continues. (Job-level "retry is don't ack" is a
separate concern -- it governs whether one message redelivers; it does not protect
the loop, since an escaping exception would still tear the thread down.) The
composition root holds the worker under @concurrently_@ alongside the server, so a
genuinely fatal error propagates and takes the process down (fail-stop), while
transient faults self-recover here. A successful poll advances the 'WorkerHeartbeat',
so a stalled loop is visible to the liveness probe.

Shutdown tears the loop down cleanly: the composition root runs it under
@concurrently_@ within its resource bracket, so process teardown cancels the loop
thread and an in-flight, un-acked message simply redelivers -- safe, because
publishing is idempotent (a version already present is success).
-}
"""
loop_content = read_file(loop_hs)
write_file(loop_hs, loop_doc + loop_content)

# 4. Update Job.hs
job_doc = """{- | Ack within the visibility budget during job processing.

A received message is hidden only for the queue's visibility window. The worker
acks on success; before a publish that may run long it calls
'Ecluse.Core.Queue.extendVisibility' to hold the message before the window lapses; on a
transient failure it does __not__ ack, so the message redelivers. A batch is
processed __sequentially__, so each job has the full visibility budget rather than
competing with its batch-mates for it.
-}
"""
job_content = read_file(job_hs)
write_file(job_hs, job_doc + job_content)

print("Split complete")
