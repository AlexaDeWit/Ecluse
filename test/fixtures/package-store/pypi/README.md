# PyPI package-store fixtures

These six archives contain independent Core Metadata for two versions.
Each version has two wheel filenames and one source distribution.
The metadata uses a distinct `Requires-Python` value for every file.
`SHA256SUMS` pins the expected bytes independently of the production publisher.

The test embeds these bytes, so discovery performs no archive generation or file reads.
These archives exercise metadata extraction. They are not installer fixtures.

Regenerate with Python 3.13.15 from the repository root, then review every digest change.
The standard library is needed only for regeneration, not for builds or test execution.
The existing E2E builders use Python ZIP and native tar generation too.
Their runtime IO does not fit pure contract discovery.

```python
import gzip, hashlib, io, tarfile, zipfile
from pathlib import Path
root = Path('test/fixtures/package-store/pypi')
rows=[]
for version, requirements in [('1.0.0', ['>=3.8', '>=3.9', '>=3.10']), ('2.0.0', ['>=3.11', '>=3.12', '>=3.13'])]:
    for tag, requirement in zip(['sdist', 'py2.py3-none-any', 'py3-none-any'], requirements):
        metadata = f'Metadata-Version: 2.4\nName: contract-package\nVersion: {version}\nRequires-Python: {requirement}\nSummary: {version} {tag}\n\n'.encode()
        data=io.BytesIO()
        if tag == 'sdist':
            filename=f'contract-package-{version}.tar.gz'
            with tarfile.open(fileobj=data, mode='w', format=tarfile.USTAR_FORMAT) as archive:
                entry=tarfile.TarInfo(f'contract-package-{version}/PKG-INFO')
                entry.size=len(metadata)
                entry.mtime=0
                entry.mode=0o644
                archive.addfile(entry, io.BytesIO(metadata))
            payload=gzip.compress(data.getvalue(), mtime=0)
        else:
            filename=f'contract_package-{version}-{tag}.whl'
            with zipfile.ZipFile(data, 'w', compression=zipfile.ZIP_STORED) as archive:
                entry=zipfile.ZipInfo(f'contract_package-{version}.dist-info/METADATA', (2026,1,1,0,0,0))
                entry.external_attr=0o644 << 16
                archive.writestr(entry,metadata)
            payload=data.getvalue()
        (root/filename).write_bytes(payload)
        rows.append((version,filename,requirement,hashlib.sha256(payload).hexdigest()))
(root/'SHA256SUMS').write_text(''.join(f'{digest}  {name}\n' for _,name,_,digest in rows))
```

Copy the reviewed digests into `Ecluse.Test.Contract.PackageStore.PyPI.Fixtures`.
