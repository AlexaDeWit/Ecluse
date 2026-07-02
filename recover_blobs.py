import subprocess
import os

res = subprocess.run(["git", "fsck", "--lost-found"], capture_output=True, text=True)
blobs = []
for line in res.stdout.splitlines():
    if line.startswith("dangling blob"):
        blobs.append(line.split()[2])

for blob in blobs:
    res = subprocess.run(["git", "cat-file", "-p", blob], capture_output=True)
    content = res.stdout
    try:
        text = content.decode('utf-8')
        if "module Ecluse.Pilot.Export" in text and "module Ecluse.Pilot.ExportSpec" not in text and "module Ecluse.Pilot.S3ExportSpec" not in text:
            print("Found Export.hs in blob", blob)
            with open("src/Ecluse/Pilot/Export.hs", "w") as f:
                f.write(text)
        elif "module Ecluse.Pilot.ExportSpec" in text:
            print("Found ExportSpec.hs in blob", blob)
            os.makedirs("test/unit/Ecluse/Pilot", exist_ok=True)
            with open("test/unit/Ecluse/Pilot/ExportSpec.hs", "w") as f:
                f.write(text)
        elif "module Ecluse.Pilot.S3ExportSpec" in text:
            print("Found S3ExportSpec.hs in blob", blob)
            os.makedirs("test/integration/Ecluse/Pilot", exist_ok=True)
            with open("test/integration/Ecluse/Pilot/S3ExportSpec.hs", "w") as f:
                f.write(text)
    except:
        pass
