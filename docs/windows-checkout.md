<!--
  Licensed to the Apache Software Foundation (ASF) under one or more
  contributor license agreements.  See the NOTICE file distributed with
  this work for additional information regarding copyright ownership.
  The ASF licenses this file to You under the Apache License, Version 2.0
  (the "License"); you may not use this file except in compliance with
  the License.  You may obtain a copy of the License at

      https://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-->

# Check out ShenYu on Windows

ShenYu contains deeply nested Java module paths. Git for Windows can report
`Filename too long` during checkout when the absolute path exceeds its default
limit. A short destination helps, but some repository paths can still exceed
that limit. Enable Git's long-path support before the initial checkout.

## Clone with a repository-local setting

Run these commands in PowerShell, using an unused destination under a short,
writable parent directory. Replace the URL with your fork URL when contributing.

```powershell
git clone -c core.longpaths=true https://github.com/apache/shenyu.git C:\src\shenyu
git -C C:\src\shenyu config --show-origin --get core.longpaths
git -C C:\src\shenyu status --short
```

The configuration check should print `true` from the new repository's `.git/config`;
the status should be empty. The clone option persists in that repository and is
applied before checkout. It does not require an administrator terminal or change
the settings of other repositories. See the
[Git for Windows long-path guidance](https://github.com/git-for-windows/build-extra/blob/main/ReleaseNotes.md#known-issues).

## Recover a failed initial checkout

If cloning downloaded the repository but reported that checkout failed, first
enable the local setting:

```powershell
git -C C:\src\shenyu config --local core.longpaths true
```

Only for that fresh failed clone, **before making any local edits**, restore its
index and working tree from the downloaded commit:

```powershell
git -C C:\src\shenyu restore --source=HEAD --staged --worktree -- :/
git -C C:\src\shenyu status --short
```

The restore command overwrites tracked local changes and staged changes. If you
have already started editing, preserve your work and clone into another unused
directory with the first command instead. No new download is needed for the
fresh-clone recovery.

## Build tools and Windows policy

After checkout, install JDK 17 or newer and set `JAVA_HOME` to its installation
directory. Use the repository's Windows Maven wrapper from PowerShell:

```powershell
Set-Location C:\src\shenyu
.\mvnw.cmd -version
.\mvnw.cmd clean install -DskipTests
```

Git's setting covers Git operations. Editors, archive tools and other build
tools have their own path handling, so keep the checkout root short. If another
application still fails, consult its long-path support before changing Windows
policy. The Windows **Enable Win32 long paths** policy requires application
opt-in and does not make every tool support long paths. See
[Microsoft's path-length documentation](https://learn.microsoft.com/en-us/windows/win32/fileio/maximum-file-path-limitation).
