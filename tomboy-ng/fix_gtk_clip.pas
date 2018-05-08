unit fix_gtk_clip;

{

  When your GTK2 app exits, it's clipboard becomes empty. Bad for usual user.
  This unit is a dirty fix, add it to "uses" somewhere.

  http://wiki.freepascal.org/Clipboard

}
 
{$mode objfpc}{$H+}
 
interface
 
uses
  gtk2, gdk2, Clipbrd;
 
implementation
 
var
  c: PGtkClipboard;
  t: string;
 
finalization
  c := gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
  t := Clipboard.AsText;
  gtk_clipboard_set_text(c, PChar(t), Length(t));
  gtk_clipboard_store(c);
end.
