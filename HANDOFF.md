# HANDOFF

Date: 2026-09-02 (supersedes the 2026-07-21 handoff)
Last updated 2026-09-02 evening, after the GTK4 close-request fix, the
`onion2` rebuild, and the commits recording both.

## Current State

- Working directory: `/mnt/STORAGE16T/Workspace_STORAGE16T/tomboy-ng`
  (moved from `/mnt/USERS/onion/DATA_ORIGN/Workspace/` on 2026-09-02, together
  with the sibling trees `../KControls` and `../LCL_GTK4/lazarus`)
- Branch: `gtk4-build-editor-fallback`
- PR: https://github.com/tomboy-notes/tomboy-ng/pull/350 (still at `a63001e`)
- Seven commits on top of `a63001e`, **not pushed** to `fork` yet:
  `8a91339` LPI (GTK3 restored, GTK4 added), `db0ace9` build script,
  `2724b1b` packaging, `a330957` regression checklist, `c9cb258` build
  script fixes, `4f9e6fb` docs, plus the commit carrying this update.
- Working tree clean. No traced tomboy-ng instance is running; the last gdb
  session was stopped after the fix was confirmed.

Current local packages: GTK4 `0.42+onion2` and Qt5 `0.42+onion1`, both in
`package/`. Reproduction details are in `BUILD_MANIFEST.md`; the manual test
list is `doc/widgetset-regression-checklist.md`.

## What changed since the PR was opened

### 1. LPI build modes — corrected

The commit in PR #350 **replaced** the GTK3 build mode with GTK4 instead of
adding one: `Item22` was renamed and the GTK3 shared-matrix entry (ID
`429153174386`) was repurposed. Upstream still uses GTK3, so that was a
regression.

Now:

- `Item22` is GTK3 again, byte for byte as on `master`.
- GTK4 is `Item25`, ReleaseGTK4 is `Item26`.
- A new shared-matrix entry `Item8` (ID `518374026915`) maps
  `GTK4,ReleaseGTK4` to `gtk4`. Counts bumped to 26 / 8.
- The LPI diff against `master` is now purely additive.

Binary names also collided: `ReleaseGTK4` produced `tomboy-ng-64`, the same
name `ReleaseLin64` (GTK2) uses, and `GTK4` produced `tomboy-ng`, the same as
`Default`. Packaging would have shipped a GTK4 binary as the GTK2 package.
Targets are now `tomboy-ng-gtk4` and `tomboy-ng-gtk4-dbg`.

### 2. Build script — generalised to gtk4 and qt5

`build_widgetset_clean.sh` replaces the gtk4-only logic;
`build_gtk4_clean.sh` is now a thin wrapper so existing references keep
working.

- `WIDGETSET=gtk4|qt5` selects build modes and the linkage check.
- Build root and Lazarus primary config path are **per widgetset**
  (`/tmp/tomboy-ng-<ws>-build`). A shared config would let KControls units
  built for one widgetset leak into the other.
- The Lazarus directory is discovered (installed Lazarus first) instead of
  defaulting to a hard-coded personal path.
- `LOCAL_VERSION_SUFFIX` appends a local suffix to the version.
- The script prints the KControls and Lazarus revisions it used.

### 3. Packaging — GTK4 support and a package-only path

`package/package.bash`:

- `ReleaseGTK4` added to `ModeParamArch` (`amd64Gtk4`), `ModeParamBin`
  (`tomboy-ng-gtk4`) and the `DebianPackage` case.
- `DebOnly:<BuildMode>` packages an already built binary and exits. The full
  script otherwise cross-builds Windows and Raspberry Pi targets and signs
  RPMs, none of which is wanted here. It runs before the Lazarus config
  lookup, since packaging does not need one.
- The `libc6` floor is now read off the binary being packaged (highest
  `GLIBC_` symbol version) instead of being a fixed guess. For these builds
  that corrected Qt5 from `>= 2.14` to `>= 2.34`.
- `LOCAL_VERSION_SUFFIX` support, empty by default.

`.gitignore` now also covers the generated packages (`package/*.deb`,
`*.tgz`, `*.rpm`, `package/BUILD/`), the generated `package/changelog`, and the
new `source/tomboy-ng-gtk4` / `-gtk4-dbg` binaries.

### 4. GTK4 "window goes blank after reopen" — root cause in LCL, fixed there

Symptom: open Search from the tray menu, close it, open it again: either
nothing appears or an empty untitled window appears. Traced with the debug
build under gdb (GTK criticals `gtk_widget_get_allocation: GTK_IS_WIDGET`
during `TCustomForm.Show`, and a live inspection showing the form handle's
`FWidget` pointing at a fresh bare `GtkWindow` whose only child was a
`GtkTooltipWindow`).

Cause: `TGtk4Window.Gtk4CloseQuery` in LCL GTK4 returned False from the
`close-request` signal, so GTK4 ran its default handler and destroyed the
window even though tomboy-ng's `FormCloseQuery` chose to hide it. The next
`Show` used the freed widget. Not a tomboy-ng bug; the `SearchForm.Hide;
SearchForm.Show` sequence in `mainunit.pas` is fine.

Fix: LCL_GTK4 commit `49195ea` (close-request returns True once LCL handled
`LM_CLOSEQUERY`), shipped as `lcl-gtk4 4.4+dfsg-4` and installed on
2026-09-02 20:03. tomboy-ng was rebuilt as `0.42+onion2` and the reopen
sequence verified by hand.

### 5. Build script: partial builds

`BUILD_MODES=GTK4` used to end with "expected binary was not built" because
the expected list was fixed per widgetset. Expected binaries and the smoke
test target are now derived from the modes built (`c9cb258`).

## Toolchain change

The system Lazarus was rebuilt from the workspace tree `../LCL_GTK4/lazarus`
twice on 2026-09-02: `4.4+dfsg-3` (17:00, used for `onion1`) and
`4.4+dfsg-4` (19:57, adds the close-request fix, used for the GTK4 `onion2`
build). The private tree is only needed to produce those packages; builds use
the installed `/usr/lib/lazarus/4.4`. Whether the installed units carry the
fix can be checked with `objdump` (see `BUILD_MANIFEST.md`).

## Results

| Package | Built from | Depends |
| --- | --- | --- |
| `package/tomboy-ng_0.42+onion2-0_amd64Gtk4.deb` | KControls `74d3257`, LCL dfsg-4 | `libgtk-4-1 (>= 4.6), libnotify4 (>= 0.7), libc6 (>= 2.34)` |
| `package/tomboy-ng_0.42+onion1-0_amd64Qt5.deb` | KControls `8e41d45`, LCL dfsg-3 | `libqt5pas1 (>= 2.15), libc6 (>= 2.34), libnotify-bin` |

KControls `74d3257` differs from `8e41d45` only in docs and test-script
paths. Verified for both: correct toolkit linkage inside the package,
`--version` reports the package version, 82 entries, the packaged binary is
byte-identical to the one built in `source/`. The GTK4 `onion2` binary was
also run under gdb against the real config: the Search close/reopen sequence
now works.

The older `package/tomboy-ng_0.42+onion1-0_amd64Gtk4.deb` still exists but
carries the LCL bug; delete it once nothing needs it.

## Open items

- **Regression testing: a first pass looks good, not yet exhaustive.** The
  user tried both builds on 2026-09-02 and reported no problems. What was
  verified mechanically here is build, linkage, packaging and an 8 second
  headless start. The itemised Korean IME and selection checks in
  `doc/widgetset-regression-checklist.md` have not been walked through one by
  one, so treat individual KControls fixes (Qt5 hit-test drift, GTK4 first key
  after Ctrl+V) as untested until someone ticks them off.
- `package/tomboy-ng_0.42+onion1-0_amd64Gtk4.deb` (dfsg-3 LCL, has the
  close-request bug) is still on disk next to the good `onion2` package.
  Delete it so nobody installs it by mistake. `package/*.deb` is git-ignored.
- Qt5 is still `onion1`. It does not need the LCL GTK4 fix, but if a single
  suffix per drop is wanted, rebuild it with `LOCAL_VERSION_SUFFIX=onion2`
  (see `BUILD_MANIFEST.md`) and update the manifest table.
- The seven local commits are not pushed. PR #350 still shows the GTK3
  build mode removed; push before maintainers look at it.
- **PR #350 conflicts with upstream.** `origin/master` moved 86 commits
  (v0.42b, v0.42c). Only `source/Tomboy_NG.lpi` conflicts, but structurally:
  upstream re-saved the LPI in the Lazarus 4 format (`<Item Name="...">`,
  no `Count=`/`ItemNN` numbering), so the GTK4 modes and the shared-matrix
  entry must be re-added in that format after rebasing. Upstream also added
  `ReleaseGTK3` / `tomboy-ng-gtk3` / `amd64GTK3`, which the `tomboy-ng-gtk4`
  naming here already matches.
- Tracing helpers used for the diagnosis live in the session scratchpad
  (`run-gtk4-gdb.sh`, `gdb-trace.gdb`, `monitor.sh`); they are not part of
  the repository. The approach: run the debug build under gdb with
  breakpoints on `g_return_if_fail_warning` and `g_logv` (level & 0x18) that
  print a backtrace and continue, plus a 10 s loop logging the process's X
  windows. `ptrace_scope=1` means gdb must start the process; it cannot
  attach later.
- KControls fixes and the LCL GTK4 close-request fix are both unmerged
  upstream, so these packages are unofficial local builds.

## Suggested next steps, in order

1. Delete the stale `onion1` GTK4 package.
2. Push the branch to `fork` (`git push fork gtk4-build-editor-fallback`).
3. Rebase onto `origin/master` (v0.42c) and re-add the GTK4 build modes in
   the new LPI format; re-verify with `WIDGETSET=gtk4 ./build_widgetset_clean.sh`.
4. Walk `doc/widgetset-regression-checklist.md` on both packages and record
   the result here.
