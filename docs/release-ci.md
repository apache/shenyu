# Release Candidate CI

The `release-candidate` workflow prepares an Apache ShenYu release candidate from a release ref. It reuses the existing full `ci.yml` release-candidate matrix, then signs the validated Maven artifacts and distributions, uploads and closes a Sonatype staging repository, imports the distributions into Apache SVN `dist/dev`, and optionally pushes a signed candidate tag.

The workflow does not promote Maven staging, move SVN files into `dist/release`, create a final GitHub Release, or send email. Those actions remain community vote gates.

## Required repository secrets

Configure these secrets in the Apache `apache/shenyu` repository. They are consumed only by the manual workflow run.

| Secret | Purpose |
| --- | --- |
| `APACHE_LDAP_USERNAME` | Apache LDAP username for Nexus and SVN |
| `APACHE_LDAP_PASSWORD` | Apache LDAP password for Nexus and SVN |
| `APACHE_GPG_PRIVATE_KEY` | ASCII-armored private signing key |
| `APACHE_GPG_PASSPHRASE` | Passphrase for the signing key; use an empty secret only for an unprotected key |

The signing key fingerprint and Sonatype staging profile are workflow inputs, so the workflow summary records exactly which public key and profile were used. The private key and passphrase are never written to artifacts or the step summary.

Protect each `<version>-release` branch in the Apache repository and create the `apache-release-candidate` GitHub Environment with at least one required reviewer. The staging job verifies both the branch protection flag and a nonempty reviewer rule before LDAP and GPG secrets are made available. The workflow also resolves `release_ref` as an existing branch, rejects SNAPSHOT POMs, rejects an already-existing candidate tag or SVN directory, and correlates the dispatched CI run with a unique identifier before downloading artifacts.

## Run a candidate

Open **Actions → release-candidate → Run workflow** and provide:

| Input | Example |
| --- | --- |
| `release_ref` | `2.7.2-release` |
| `release_version` | `2.7.2` |
| `candidate` | `RC4` |
| `nexus_profile_id` | `711e5cf06a7654` |
| `gpg_key_id` | `4EBB813D10EEDB27D8D891AF5E300845B5CFD2A7` |
| `push_signed_tag` | `true` |

The selected ref must already contain the non-SNAPSHOT release POM and all release fixes. The workflow first dispatches `ci.yml` with `release_candidate=true` and waits for the complete candidate matrix, including artifact legal-file checks, source rebuild, Java 17/21 tests, RAT and integration checks. It stops before signing or publishing if any required job fails.

The release-candidate path in `ci.yml` enables Javadocs, Maven remote legal resources, unsigned release packaging, version-filtered repository archiving, project LICENSE/NOTICE checks, runtime dependency license audits, and the pinned `shenyu-watcher` distribution check. Older cached ShenYu versions are excluded before signing or Nexus upload.

The completed workflow summary reports:

- the validated commit and candidate tag;
- the Sonatype staging repository ID and URL;
- the Apache SVN `dist/dev/shenyu/<version>-<candidate>` URL and commit revision;
- the full CI run URL and an evidence artifact containing the generated manifest and upload logs.

The staging repository is closed with `autoReleaseAfterClose=false`, so the vote can inspect it before any promotion. The SVN directory is imported under `dist/dev` and is safe for the vote process.

## After the vote

Use the staging ID from the workflow summary to release the same closed repository in the Nexus staging UI. Move the exact SVN candidate directory to `https://dist.apache.org/repos/dist/release/shenyu/<version>` only after the vote passes and the checksums/signatures have been independently verified. Create the final `v<version>` GitHub Release and send the `[ANNOUNCE]` email after Maven and SVN are public.

Do not rerun the candidate workflow with the same tag or SVN path. Start a new candidate label when a vote is cancelled.
