# HANDOFF

Date: 2026-09-11 (supersedes the 2026-09-02 handoff)
Written after rebasing the branch onto upstream v0.42c and rebuilding both
packages as `onion3`.

## Current State

- Working directory: `/mnt/STORAGE16T/Workspace_STORAGE16T/tomboy-ng`
  (sibling trees `../KControls` and `../LCL_GTK4/lazarus`)
- Branch: `gtk4-build-editor-fallback`, **rebased onto `origin/master`
  `08de213`** (v0.42c). Nine commits on top of it; the last one carries this
  file and the manifest.
- PR: https://github.com/tomboy-notes/tomboy-ng/pull/350. The rebased branch
  was force-pushed to `fork` on 2026-09-11; GitHub then reported the PR as
  MERGEABLE with 9 commits. The pre-rebase tip `14cde76` is kept locally as
  `backup/pre-rebase-2026-09-11` and was pushed to the PR earlier the same
  day, so GitHub still has it in the PR's force-push history.
- Working tree clean apart from git-ignored build outputs.

Current local packages, both in `package/`:

| Package | Built from | Depends |
| --- | --- | --- |
| `tomboy-ng_0.42c+onion3-0_amd64GTK4.deb` | KControls `36ffeec`, LCL 4.4+dfsg-4 | `libgtk-4-1 (>= 4.6), libnotify4 (>= 0.7), libc6 (>= 2.34)` |
| `tomboy-ng_0.42c+onion3-0_amd64Qt5.deb` | KControls `36ffeec`, LCL 4.4+dfsg-4 | `libqt5pas1 (>= 2.15), libc6 (>= 2.34), libnotify-bin` |

Reproduction details are in `BUILD_MANIFEST.md`; the manual test list is
`doc/widgetset-regression-checklist.md`. The older `0.42+onion2` GTK4 and
`0.42+onion1` Qt5 packages are still on disk. They are not broken, only
superseded (v0.42 sources, older KControls); delete them when convenient.
The `0.42+onion1` GTK4 package with the LCL close-request bug was deleted
on 2026-09-11.

## What changed on 2026-09-11

### 1. Rebase onto upstream v0.42c

`origin/master` had moved 86 commits (v0.42b, v0.42c). Only
`source/Tomboy_NG.lpi` conflicted, because upstream re-saved it in the
Lazarus 4 format: `<Item Name="...">` entries without `Count=` or `ItemNN`
numbering. The GTK4 modes were re-added in that format:

- `<Item Name="GTK4">` producing `tomboy-ng-gtk4-dbg` and
  `<Item Name="ReleaseGTK4">` producing `tomboy-ng-gtk4`, placed just before
  `SharedMatrixOptions`.
- Shared-matrix `Item8` (ID `518374026915`) maps `GTK4,ReleaseGTK4` to
  `gtk4`; `SharedMatrixOptions Count` 7 -> 8.
- The LPI diff against `master` is again purely additive (the only removed
  line is that `Count="7"`).

Upstream renamed its GTK3 modes to `ReleaseGTK3` / `GTK3Debug` with binary
`tomboy-ng-gtk3` and package arch tag `amd64GTK3`. To match, the GTK4 arch
tag in `package/package.bash` is now `amd64GTK4` (was `amd64Gtk4`), which is
why the package file name changed case.

History was tidied during the rebase:

- The first commit (the original PR commit) now contains the correct,
  additive LPI directly, plus the About-dialog tag below. It no longer
  removes the GTK3 mode.
- The former "Restore the GTK3 build mode" commit only renames the
  regression checklist now and was reworded to say so.
- Three source files had lost their trailing blank line relative to
  upstream; restored so the PR diff carries no whitespace noise.

### 2. About dialog

`mainunit.pas` appends `, GTK4` to the About text under `LCLGTK4`, next to
the existing GTK3/GTK2/Qt tags. Verified: `--version` on the packaged binary
prints `0.42c+onion3`.

### 3. Rebuilt as `onion3`

Both widgetsets were rebuilt from the rebased tree with
`build_widgetset_clean.sh` and packaged with the `DebOnly:` path. Verified for
both packages: correct toolkit linkage inside the package, `--version`
reports the package version, 82 entries, packaged binary byte-identical to
the one in `source/`, headless smoke test passed.

Both packages were rebuilt once more at 11:17 from KControls `36ffeec`,
which only adds a HANDOFF.md commit over `3770e33`; all three binaries came
out byte-identical to the 11:04 build, so the packages differ only in
timestamps. The 2026-09-02 packages used KControls `74d3257`; the commits in
between are KMemo fixes: grapheme-cluster stepping across
block boundaries, absolute word-movement targets, UTF-16 surrogate handling,
a KGrid editor measurement, and tests/docs. They were only verified here by
build and smoke test.

## Things upstream added that GTK4 does not yet share

v0.42b/c added several `{$ifdef LCLGTK3}` workarounds. None were extended
to GTK4, because nobody has checked whether GTK4 needs them:

- `editbox.pas`: the note save thread is disabled on GTK3
  ("Gtk3 has problems with PostMessage in thread"). GTK4 still saves in a
  thread. **Watch note saving on GTK4 during the regression walk**; if it
  hangs or crashes, add `LCLGTK4` to that condition.
- `editbox.pas`: new notes get `Width := 600` on GTK3.
- `settings.pas`: two spin edits are widened by 24 px on GTK3, and the
  fixed-font mono check uses `Font.IsMonoSpace` on GTK3 instead of the
  `i`/`w` width comparison.

Also: `package.bash`'s full-build loop lists `ReleaseGTK3` but not
`ReleaseGTK4`. That is deliberate for now, since the full path would need a
GTK4-enabled LCL on the maintainer's box; `DebOnly:ReleaseGTK4` is the GTK4
packaging path.

## Toolchain

Installed Lazarus 4.4 packages are all `4.4+dfsg-4` (`lazarus-src-4.4`,
`lcl`, `lcl-gtk4`, `lcl-qt5`), FPC 3.2.2. `dfsg-4` carries the GTK4
close-request fix (LCL_GTK4 commit `49195ea`; see the 2026-09-02 notes in
git history for the diagnosis). To confirm the installed unit has it:

```
objdump -dr /usr/lib/lazarus/4.4/lcl/units/x86_64-linux/gtk4/gtk4widgets.o \
  | awk '/GTK4CLOSEQUERY\$\$BOOLEAN>:$/{f=1} f&&/R_X86_64/{print} f&&/ret/{exit}'
```

It must list a relocation to `TWINCONTROL_..._HANDLEALLOCATED`. Plain
`objdump -d` does not show it (the calls are unresolved relocations in the
`.o`), which is why the earlier recipe looked negative.

## Open items

- **Regression testing has not been walked item by item.** Build,
  linkage, packaging and an 8 second headless start are verified. The
  Korean IME and selection checks in `doc/widgetset-regression-checklist.md`
  still need a manual pass on both `onion3` packages, now also covering the
  newer KControls revision and the GTK3-only workarounds listed above.
- Old packages `tomboy-ng_0.42+onion2-0_amd64Gtk4.deb` and
  `tomboy-ng_0.42+onion1-0_amd64Qt5.deb` are superseded; delete once the
  `onion3` pair has been tried.
- KControls fixes and the LCL GTK4 close-request fix are both unmerged
  upstream, so these packages remain unofficial local builds.
- Tracing helpers from the 2026-09-02 diagnosis (`run-gtk4-gdb.sh`,
  `gdb-trace.gdb`, `monitor.sh`) lived in a session scratchpad and are gone;
  the approach is described in the 2026-09-02 version of this file
  (`git show 14cde76:HANDOFF.md`).

## Suggested next steps, in order

1. Install the `onion3` pair and walk `doc/widgetset-regression-checklist.md`
   on both; record the result here. Pay attention to note saving on GTK4.
2. If GTK4 needs any of the upstream GTK3 workarounds, extend the `ifdef`s
   and rebuild.
3. Delete the superseded `onion1`/`onion2` packages.
4. Watch PR #350 for maintainer feedback.
