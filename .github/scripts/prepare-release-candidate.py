#!/usr/bin/env python3
"""Prepare and sign the Maven and SVN files for an Apache release candidate."""

import argparse
import hashlib
import json
import shutil
import subprocess
from pathlib import Path


PRIMARY_SUFFIXES = (".jar", ".pom", ".module", ".war", ".aar", ".zip", ".tar.gz")
ARCHIVE_PATTERNS = {
    "admin": "*-admin-bin.tar.gz",
    "bootstrap": "*-bootstrap-bin.tar.gz",
    "source": "*-src.zip",
}


def is_primary(path: Path) -> bool:
    name = path.name
    if name.startswith("maven-metadata") or name.endswith(".lastUpdated"):
        return False
    if name.endswith((".asc", ".md5", ".sha1", ".sha256", ".sha512")):
        return False
    return name.endswith(PRIMARY_SUFFIXES)


def digest(path: Path, algorithm: str) -> str:
    hasher = hashlib.new(algorithm)
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            hasher.update(chunk)
    return hasher.hexdigest()


def sign(path: Path, key_id: str, passphrase_file: Path) -> None:
    signature = Path(str(path) + ".asc")
    command = [
        "gpg", "--batch", "--yes", "--pinentry-mode", "loopback",
        "--passphrase-file", str(passphrase_file), "--local-user", key_id,
        "--armor", "--detach-sign", "--output", str(signature), str(path),
    ]
    subprocess.run(command, check=True)
    subprocess.run(["gpg", "--batch", "--verify", str(signature), str(path)],
                   check=True, stdout=subprocess.DEVNULL)


def checksum(path: Path, algorithm: str, suffix: str) -> None:
    value = digest(path, algorithm)
    path.with_name(path.name + suffix).write_text(f"{value}  {path.name}\n", encoding="utf-8")


def copy_distribution(source: Path, destination: Path, version: str, kind: str) -> Path:
    if kind == "admin":
        name = f"apache-shenyu-{version}-admin-bin.tar.gz"
    elif kind == "bootstrap":
        name = f"apache-shenyu-{version}-bootstrap-bin.tar.gz"
    else:
        name = f"apache-shenyu-{version}-src.zip"
    target = destination / name
    target.write_bytes(source.read_bytes())
    return target


def create_clean_repository(repository: Path, version: str, destination: Path) -> Path:
    """Copy only the requested version into a fresh Maven repository tree."""
    if destination.exists():
        raise SystemExit(f"Clean staging repository already exists: {destination}")
    destination.mkdir(parents=True)
    selected = 0
    for source in repository.rglob("*"):
        if not source.is_file():
            continue
        relative = source.relative_to(repository)
        if version not in relative.parts:
            continue
        if source.name.startswith("maven-metadata") or source.name in {"_remote.repositories"} \
                or source.name.endswith(".lastUpdated"):
            continue
        target = destination / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, target)
        selected += 1
    if not selected:
        raise SystemExit(f"No Maven artifacts for version {version} were found in {repository}")
    return destination


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--repository", required=True, type=Path)
    parser.add_argument("--staging-repository", required=True, type=Path)
    parser.add_argument("--version", required=True)
    parser.add_argument("--svn-dir", required=True, type=Path)
    parser.add_argument("--key-id", required=True)
    parser.add_argument("--passphrase-file", required=True, type=Path)
    parser.add_argument("--manifest", required=True, type=Path)
    args = parser.parse_args()

    repository = args.repository.resolve()
    if not repository.is_dir():
        raise SystemExit(f"Maven repository does not exist: {repository}")
    args.svn_dir.mkdir(parents=True, exist_ok=False)
    staging_repository = create_clean_repository(repository, args.version, args.staging_repository.resolve())

    primary = sorted(path for path in staging_repository.rglob("*") if path.is_file() and is_primary(path))
    if not primary:
        raise SystemExit("No Maven primary artifacts were found")

    for path in primary:
        sign(path, args.key_id, args.passphrase_file)
        checksum(path, "md5", ".md5")
        checksum(path, "sha1", ".sha1")

    distributions = {}
    for kind, pattern in ARCHIVE_PATTERNS.items():
        if kind == "source":
            archive_patterns = [f"*{args.version}-src.zip"]
        else:
            # Maven assembly installations may retain the module artifactId in
            # the repository filename, while the SVN package uses the Apache
            # canonical name. Accept both input forms and rename below.
            archive_patterns = [
                f"apache-shenyu-{args.version}-{kind}-bin.tar.gz",
                f"shenyu-{kind}-dist-{args.version}-{kind}-bin.tar.gz",
            ]
        candidates = sorted({
            candidate
            for archive_pattern in archive_patterns
            for candidate in staging_repository.rglob(archive_pattern)
        })
        if len(candidates) != 1:
            raise SystemExit(f"Expected one {kind} distribution matching {archive_patterns}, found {len(candidates)}")
        target = copy_distribution(candidates[0], args.svn_dir, args.version, kind)
        sign(target, args.key_id, args.passphrase_file)
        checksum(target, "sha512", ".sha512")
        distributions[kind] = target.name

    manifest = {
        "version": args.version,
        "maven_primary_count": len(primary),
        "maven_primary": [str(path.relative_to(staging_repository)) for path in primary],
        "svn_files": sorted(path.name for path in args.svn_dir.iterdir()),
        "distributions": distributions,
    }
    args.manifest.write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    print(json.dumps({"maven_primary_count": len(primary), "svn_files": manifest["svn_files"]}))


if __name__ == "__main__":
    main()
