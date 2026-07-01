import re

with open("core/test/unit/Ecluse/Worker/IntegritySpec.hs", "r") as f:
    lines = f.readlines()

new_lines = []
in_imports = False
import_lines = []

for line in lines:
    if line.startswith("import "):
        in_imports = True
        import_lines.append(line)
    elif in_imports and line.startswith(" "):
        import_lines.append(line)
    elif in_imports and line.startswith(")"):
        import_lines.append(line)
    elif in_imports and line.strip() == "":
        pass # ignore empty lines in imports block for now
    else:
        if in_imports:
            # Done collecting imports
            new_lines.append("import Test.Hspec\n")
            new_lines.append("import Data.Text qualified as T\n")
            new_lines.append("import Ecluse.Core.Package (HashAlg (Blake2b, MD5, SHA1, SHA256, SRI))\n")
            new_lines.append("import Ecluse.Core.Package qualified as Pkg\n")
            new_lines.append("import Ecluse.Core.Worker (IntegrityResult (IntegrityMismatch, IntegrityVerified), verifyIntegrity)\n")
            new_lines.append("import Ecluse.Test.Package (unsafeHash)\n")
            new_lines.append("import Ecluse.Worker.Support\n")
            new_lines.append("\n")
            in_imports = False
        new_lines.append(line)

with open("core/test/unit/Ecluse/Worker/IntegritySpec.hs", "w") as f:
    f.writelines(new_lines)
