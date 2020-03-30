unit LoadNote;
{
 * Copyright (C) 2017 David Bannon
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}

{   This unit is responsible for loading a note into the passed Richmemo. The
	note is expected to be in Tomboy's XML format.
	Note that the class expects a few things to be passed to it, after creation
	that it will need before you call LoadNote().
}

{	20170928 - showed it how to set the title during loading rather than afterwards.
	saves about 200mS in a big (20K) file.

	20171003 - Added a line in load file to drop any CR (#13) on the floor. Otherwise
	on Windows, we were reading two newlines, one for the CR and one for the LF.
	Its not worth an ifdef, we'll only see #13 on windows I assume ?
    Set the title, as loaded by this unit, to be FontTitle big. ??

	20171007 - enabled bullets.
	20171112 - added code to restore < and >
    2018/01/31 - and &
    2018/03/18  Nothing
    2018/03/18  Added a test it AddText to ensure we don't put an empty text block in. Issue #27
    2018/07/27  Called ReplaceAngles() on string assigned to Title.
    2018/08/15  ReplaceAngles() works with bytes, not char, so don't use UTF8Copy and UTF8Length ....
    2018/10/13  Altered LoadFile() so Tabs are allowed through
    2019/04/29  Restore note's previous previous position and size.
    2019/07/21  Use Sett.TitleColour;
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, KMemo;


type

	{ TBLoadNote }

 TBLoadNote = class
      private
         InContent : boolean;
         FirstTime : boolean;		// Set when first line (Title) is added to KMemo
         Bold : boolean;
         Italic : boolean;
         HighLight : boolean;
         Underline : boolean;
         Strikeout : boolean;
         FixedWidth : boolean;
         InBullet, BulletOwing : boolean;
         InStr : ANSIString;
         KM : TKMemo;
         procedure AddText(AddPara : Boolean);
         Procedure ReadTag(fs : TFileStream);
		 function ReplaceAngles(const Str: AnsiString): AnsiString;
      public
         FontSize : integer;            // Must be set externally after creation
         // FontName : ANSIstring;			// Must be set externally after creation
         Title    : ANSIString; 		// Read from the note being loaded.
         // BulletString : ANSIString;		// as above
         FontNormal : integer;			// as above
         CreateDate : ANSIString;
         X, Y : integer;
         Height, Width : integer;
         procedure LoadFile(FileName : ANSIString; RM : TKMemo);

    end;

implementation

uses Graphics,     		// For some font style defs
    LazUTF8,
    Settings,			// User settings and some defines across units.
    LazLogger;

procedure TBLoadNote.LoadFile(FileName : ANSIString; RM : TKMemo);
var
  	fs : TFileStream;
    ch : char = ' ';
    Blocks : longint = 0;
begin
  	KM := RM;
    FirstTime := True;
  	fs := TFileStream.Create(Utf8ToAnsi(FileName), fmOpenRead or fmShareDenyNone);
    try
       while fs.Position < fs.Size do begin
         fs.read(ch, 1);
         if Ch = #13 then fs.read(ch, 1);   // drop #13 on floor. Silly Windows double newline.
         if Ch = #9 then Ch := ' ';         // ToDo : this is temp cludge, KMemo cannot handle tabs, they
                                            // come in via pasted text, better fix during the paste process.
                                            // This might mess with UTF8 ??
         if (Ch = '<') or ((Ch < ' ') and (Ch <> #09)) then begin     // 09 is Tab, let it through
             if (Ch < ' ') then             // thats a newline
                 	AddText(True)
             else ReadTag(fs);
             inc(Blocks);
             InStr := '';
          end else
                InStr := InStr + ch;
        end;
    finally
        FreeAndNil(fs);
    end;
end;

	{ This procedure writes note content to the KMemo in EditBox. It relies on
    the Global constants (in the Settings Unit) to tell it about style, and
    size. The Regional InStr has what to write. }
procedure TBLoadNote.AddText(AddPara : Boolean);

//const
(* {$ifdef LINUX}                         // Font names are determined in settings
 MonospaceFont = 'monospace';
 {$ifend}
 {$ifdef WINDOWS}
 MonospaceFont = 'Lucida Console';
 {$ifend}
 {$ifdef DARWIN}
 MonospaceFont = 'Lucida Console';
 {$ifend}
*)

var
    FT : TFont;
    PB : TKMemoParagraph;
    TB : TKMemoTextBlock ;
    //T1, T2 : qword;
begin
  if not InContent then exit;
  if (InStr = '') and (not AddPara) then exit;
  if InStr <> '' then begin
    FT := TFont.Create();
      if FirstTime then begin                 // Title
  	    FT.Style := [fsUnderline];
        Title := ReplaceAngles(InStr);
        FT.Size := Sett.FontTitle;
        FT.Color := Sett.TitleColour;
      end else begin
        FT.Style := [];
        FT.Size:= FontSize;

      end;
      TB := KM.Blocks.AddTextBlock(ReplaceAngles(InStr));  // We have to scan InStr for &lt; and &gt;  being < and >
      if Bold then FT.Style := FT.Style + [fsBold];
      if Italic then FT.Style := FT.Style + [fsItalic];
      if HighLight then TB.TextStyle.Brush.Color := Sett.HiColour;
      if Underline then FT.Style := Ft.Style + [fsUnderline];
      if Strikeout then FT.Style := Ft.Style + [fsStrikeout];
      if FixedWidth then FT.Name := Sett.FixedFont;
      if FixedWidth then FT.Pitch := fpFixed;
      if not FixedWidth then FT.Name := Sett.UsualFont;    // Because 'FixedWidth := false;' does not specify a font to return to
      // if Sett.DarkTheme then Ft.Color:=Sett.DarkTextColour;
      Ft.Color:=Sett.TextColour;
      TB.TextStyle.Font := Ft;
      FT.Free;
  end;
  if AddPara then begin
  	PB := KM.Blocks.AddParagraph;
    // BulletOwing means we had a bullet tag but encountered the /tag before writing to KMemo
	if InBullet or BulletOwing then begin
           PB.Numbering := pnuBullets;
           PB.NumberingListLevel.FirstIndent := -20;    // Note, these numbers need match SettBullet() in editbox
           PB.NumberingListLevel.LeftIndent := 30;
           BulletOwing := False;
	end;
  end;
  if FirstTime then begin
      FirstTime := false;
      KM.Blocks.DeleteEOL(0);
  end;
end;

function TBLoadNote.ReplaceAngles(const Str : AnsiString) : AnsiString;
var
    index : longint = 1;
    Start : longint = 1;
begin
  // Don't use UTF8 functions here, we are working with bytes !
  Result := '';
    while Index <= Length(Str) do begin
      if '&lt;' = Copy(Str, Index, 4) then begin
      		Result := Result + Copy(Str, Start, Index - Start) + '<';
            inc(Index);
            Start := Index + 3;
            Continue;
	  end;
      if '&gt;' = Copy(Str, Index, 4) then begin
      		Result := Result + Copy(Str, Start, Index - Start) + '>';
            inc(Index);
            Start := Index + 3;
            Continue;
	  end;
      if '&amp;' = Copy(Str, Index, 5) then begin
      		Result := Result + Copy(Str, Start, Index - Start) + '&';
            inc(Index);
            Start := Index + 4;
            Continue;
	  end;
      inc(Index);
	end;
    Result := Result + Copy(Str, Start, Index - Start);
end;


Procedure TBLoadNote.ReadTag(fs : TFileStream);    // we are here because '<'
var
    Buff : String;
    Ch : char = ' ';
begin
  Addtext(False);    // Write the text we have so far with existing params
  Buff := '';   // now, lets set new params or get other data
  while fs.Position < fs.Size do begin
      fs.read(Ch, 1);
      if Ch = '>' then exit;
      //if Ch = #9 then ch := ' ';         // just how UTF8 is that I wonder ?  KMemo cannot cope with a tab .....
      Buff := Buff + Ch;
      case Buff of
            'note-content version="0.1"' : InContent := true;
            'note-content version="0.3"' : InContent := true;
            '/note-content' : InContent := false;
            'bold' : Bold := True;
            '/bold' : Bold := False;
            'italic' : Italic := True;
            '/italic' : Italic := false;
            'highlight' : HighLight := true;
            '/highlight' : HighLight := false;
            'underline' : Underline := true;
            '/underline' : Underline := false;
            'strikeout' : Strikeout := true;
            '/strikeout' : Strikeout := false;
            'monospace' : FixedWidth := true;
            '/monospace' : FixedWidth := false;
            'size:small' : FontSize := Sett.FontSmall;
            '/size:small' : FontSize := Sett.FontNormal;
            'size:large' : FontSize := Sett.FontLarge;
            '/size:large' : FontSize := Sett.FontNormal;
            'size:huge' : FontSize := Sett.FontHuge;
            '/size:huge' : FontSize := Sett.FontNormal;
            'list-item dir="ltr"' : begin InBullet := true; BulletOwing := True; end;
            '/list-item' : InBullet := false;
            '/create-date' : CreateDate := InStr;
            '/x' : X := strtointDef(InStr, 20);
            '/y' : Y := strtointDef(InStr, 20);
            '/width' : Width := strtointdef(InStr, 300);
            '/height' : height := strtointdef(InStr, 200);
      end;
  end;
end;

end.
