# GTK4 Regression Checklist

Use this checklist after updating LCL-gtk4, KControls, or the local Lazarus tree.

## Build

- Run `./build_gtk4_clean.sh`.
- Confirm both `source/tomboy-ng` and `source/tomboy-ng-64` link to `libgtk-4.so`.
- Optionally run `RUN_SMOKE_TEST=1 ./build_gtk4_clean.sh` to start the GTK4 binary with an isolated temporary config directory. This needs a usable `DISPLAY` or `xvfb-run`; display/session failures are reported as warnings after the build and linkage checks pass.

## Editor Input

- Create or open a note.
- Type Korean text with IME composition.
- Type `?` after Korean text, for example `별 이상은 없는거?`.
- Test Backspace and Delete around Korean text.
- Select Korean text and overwrite it with another character.
- Undo and redo after each of the above operations.
- Paste plain text and formatted text if available.

## Runtime Behavior

- Start the application with no existing instance.
- Start it again while the first instance is running.
- Open the note list.
- Open, edit, close, and reopen a note.
- Check `--debug-log=PATH` output if an editor fallback or save issue appears.

## Known Diagnostic Point

`TUndo_Redo.GetSelectedRTF` logs when KMemo's selected-only RTF snapshot fails and falls back to plain text. The fallback is intended to keep typing uninterrupted; formatting may be missing only from that single undo snapshot.
