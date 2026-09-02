# Widgetset Regression Checklist (GTK4 / Qt5)

Use this checklist after updating KControls, LCL, or the Lazarus install.
The KControls fixes are not upstream yet, so the exact revision matters — see
`BUILD_MANIFEST.md`.

## Build

- `WIDGETSET=gtk4 LOCAL_VERSION_SUFFIX=onion1 ./build_widgetset_clean.sh`
- `WIDGETSET=qt5  LOCAL_VERSION_SUFFIX=onion1 ./build_widgetset_clean.sh`
- Run them one after the other, never in parallel: both write
  `source/lib/x86_64-linux`.
- The script fails if a binary does not link to the expected toolkit, so a
  clean exit already proves linkage.
- Confirm both builds report the same KControls revision in their header.
- `RUN_SMOKE_TEST=1` starts the built binary with a throwaway config
  directory. It needs a usable `DISPLAY` or `xvfb-run`.

## Editor input (both widgetsets)

- Create or open a note.
- Type Korean text with IME composition.
- Type `?` after Korean text, for example `별 이상은 없는거?`.
- Backspace and Delete around Korean text.
- Select Korean text and overwrite it with another character.
- Undo and redo after each of the above.
- Paste plain text, and formatted text where available.

## GTK4 specific

These are the KControls / LCL GTK4 fixes; verify they hold in the real editor,
not just in the KControls test harness.

- Press Ctrl+V, then immediately type a character — the first key after a
  command key must not be swallowed.
- Copy multibyte text out of a note into another application, and back.
- Open a combo box in Settings: the dropdown must open once per click, and
  hovering must not commit a selection.
- Long CJK paragraphs: click and drag to select — the selection must follow
  the pointer with no drift.

## Qt5 specific

- Long Korean paragraph (100+ characters, no spaces): click at one point and
  drag to another, then copy. The copied range must match what is highlighted.
  This is the hit-test drift fix (A1) and is the main reason for the Qt5
  rebuild.
- Repeat with mixed Korean and emoji.

## Runtime behaviour

- Start with no existing instance; start again while the first is running.
- Open the note list; open, edit, close, and reopen a note.
- Check `--debug-log=PATH` output if an editor fallback or save issue appears.

## Known diagnostic point

`TUndo_Redo.GetSelectedRTF` logs when KMemo's selected-only RTF snapshot fails
and falls back to plain text. With the KControls RTF fixes in place this should
no longer fire; if it appears in the debug log, the RTF writer regressed.

## Packaging

- `cd package`
- `LOCAL_VERSION_SUFFIX=onion1 bash package.bash /usr/lib/lazarus/4.4 DebOnly:ReleaseGTK4`
- `LOCAL_VERSION_SUFFIX=onion1 bash package.bash /usr/lib/lazarus/4.4 DebOnly:ReleaseQT5`
- Check each package:
  `dpkg-deb -f <deb> Version Architecture Depends`, then extract it and confirm
  `usr/bin/tomboy-ng` links to the expected toolkit and reports the right
  version.
