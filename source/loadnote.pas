unit LoadNote;
{   Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------

    This unit is responsible for loading a note into the passed Richmemo. The
	note is expected to be in Tomboy's XML format.
	Note that the class expects a few things to be passed to it, after creation
	that it will need before you call LoadNote().

    History -
	20170928 - showed it how to set the title during loading rather than afterwards.
	saves about 200mS in a big (20K) file.

	20171003 - Added a line in load file to drop any CR (#13) on the floor. Otherwise
	on Windows, we were reading two newlines, one for the CR and one for the LF.
	Its not worth an ifdef, we'll only see #13 on windows I assume ?
    Set the title, as loaded by this unit, to be FontTitle big. ??

	2017/10/07 - enabled bullets.
	2017/11/12 - added code to restore < and >
    2018/01/31 - and &
    2018/03/18  Nothing
    2018/03/18  Added a test it AddText to ensure we don't put an empty text block in. Issue #27
    2018/07/27  Called ReplaceAngles() on string assigned to Title.
    2018/08/15  ReplaceAngles() works with bytes, not char, so don't use UTF8Copy and UTF8Length ....
    2018/10/13  Altered LoadFile() so Tabs are allowed through
    2019/04/29  Restore note's previous previous position and size.
    2019/07/21  Use Sett.TitleColour;
    2020/05/01  Stop using local replaceAngles(), use tb_utils.RestoreBadXMLChar()
    2021/08/27  Extensive changes to support multilevel bullets, use Tomboy or Conboy model
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
         //InBullet, BulletOwing : boolean;
         BulletLevel : integer;
         InStr : ANSIString;
         KM : TKMemo;
                            { Is passed an XML tag content, such as bold or /italics and sets up
                            the regional vars so that AddText knows how to markup the next block}
         procedure ActOnTag(buff: string);

                            { This procedure writes note content to the KMemo in EditBox. It relies on
                            the Global constants (in the Settings Unit) to tell it about style, and
                            size. The Regional InStr has what to write. A number of 'state' regional
                            vars (ie Bold, Strikeout) tell it about the active styles at present. This
                            method adds one textblock (or possibly parablock) from InStr to the kmemo.
                            It gets called when LoadFile() encounters a newline or the start of a Tag.}
         procedure AddText(AddPara : Boolean);

                             { called when ReadTag encounters a <list>, process through to corresponding </list>
                             including any intermediat <list></list> pairs. Ignores any newlines in content during
                             this period. Drops any content that is not between <list-item..> tags.
                             We arrive here after having read the first <list and will read all necessary content
                             so ReadTag.ch will not contain the trailing '>'. Remove trailing newline before returning.
                             InStr should be empty and BulletLevel should be zero.}
         procedure ReadList(fs: TFileStream);

                            { Gets called when LoadFile finds the start of a tag. It immediatly calls
                            AddText to flush any existing content to Kmemo and then looks at tag.}
         Procedure ReadTag(fs : TFileStream);

      public
         FontSize : integer;            // Must be set externally after creation
         // FontName : ANSIstring;			// Must be set externally after creation
         Title    : ANSIString; 		// Read from the note being loaded.
         // BulletString : ANSIString;		// as above
         FontNormal : integer;			// as above
         CreateDate : ANSIString;
         X, Y : integer;
         Height, Width : integer;
                            { Public : the main, lets do it method }
         procedure LoadFile(FileName : ANSIString; RM : TKMemo);

    end;

implementation

uses Graphics,     		// For some font style defs
    LazUTF8,
    Settings,			// User settings and some defines across units.
    TB_Utils,
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
         if (Ch = '<') or (Ch < ' ') then begin     // start of tag or ctrl char
             if (Ch < ' ') then             // thats a newline (other ctrl ? drop on floor)
                 	AddText(True)           // flush through to kMemo, new paragraph
             else begin
                 AddText(false);            // flush through to kmemo
                 ReadTag(fs);               // deals with _only_ tag unless its a list tag !
             end;
             inc(Blocks);
             InStr := '';                   // AddText does that ???? Maybe not in every case ?
          end else
                InStr := InStr + ch;
        end;
    finally
        FreeAndNil(fs);
    end;
    //debugln('TBLoadNote.LoadFile Height=' + inttostr(Height) + ' Width=' + inttostr(Width));
end;


procedure TBLoadNote.AddText(AddPara : Boolean);
var
    FT : TFont;
    PB : TKMemoParagraph;
    TB : TKMemoTextBlock ;
    //T1, T2 : qword;
begin
  if not InContent then exit;
  if (InStr = '') and (not AddPara) then exit;
  // if to here, we have content to flush or a new para has been requested.
  //debugln('TBLoadNote.AddText bulletlevel=' + inttostr(bulletLevel) + ', BOLD=' + booltostr(Bold, true) + ' and InStr=[' + ']');
  if InStr <> '' then begin
      FT := TFont.Create();
      if FirstTime then begin                 // Title
  	    FT.Style := [fsUnderline];
        //Title := ReplaceAngles(InStr);
        Title := RestoreBadXMLChar(InStr);     // SyncUtils Function
        FT.Size := Sett.FontTitle;
        FT.Color := Sett.TitleColour;
      end else begin
        FT.Style := [];
        FT.Size:= FontSize;
      end;
      TB := KM.Blocks.AddTextBlock(RestoreBadXMLChar(InStr));
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
  InStr := '';
  if AddPara then begin
  	    PB := KM.Blocks.AddParagraph;
        if BulletLevel > 0 then begin
            {$if declared(pnuArrowBullets)}         // Note IDE assumes true, versions of KControls earlier than Late August 2021 are FALSE
            case BulletLevel of
                1 : begin
                      //debugln('AddText - BulletLevel One');
                      PB.Numbering:=BulletOne;
                      PB.NumberingListLevel.FirstIndent:=-20;      // Ahh ! some magic numbers ?
                      PB.NumberingListLevel.LeftIndent := 30;      // Note, these numbers need match SettBullet() in editbox
                    end;
                2 :   begin
                        //debugln('AddText - BulletLevel Two');
                        PB.Numbering:=pnuNone;
                        PB.Numbering := BulletTwo;
                        PB.NumberingListLevel.FirstIndent:=-20;
                        PB.NumberingListLevel.LeftIndent := 50;
                    end;
                3 : begin
                        PB.Numbering:=pnuNone;
                        PB.Numbering := BulletThree;
                        PB.NumberingListLevel.FirstIndent:=-20;
                        PB.NumberingListLevel.LeftIndent := 70;
                    end;
                4 : begin
                        PB.Numbering:=pnuNone;
                        PB.Numbering := BulletFour;
                        PB.NumberingListLevel.FirstIndent:=-20;
                        PB.NumberingListLevel.LeftIndent := 90;
                    end;
                 5 : begin
                        PB.Numbering:=pnuNone;
                        PB.Numbering := BulletFive;
                        PB.NumberingListLevel.FirstIndent:=-20;
                        PB.NumberingListLevel.LeftIndent := 110;
                    end;
                 6,7,8,9 : begin
                        PB.Numbering:=pnuNone;
                        PB.Numbering := BulletSix;
                        PB.NumberingListLevel.FirstIndent:=-20;
                        PB.NumberingListLevel.LeftIndent := 130;
                    end;
                otherwise
                    debugln('LoadNote.AddText - BulletLevel otherwise, ' + inttostr(BulletLevel));                                     // we just stop at 4
            end;
            BulletLevel := 0;
            {$else}
            PB.Numbering := pnuBullets;
            PB.NumberingListLevel.FirstIndent := -20;    // Note, these numbers need match SettBullet() in editbox
            PB.NumberingListLevel.LeftIndent := 30;
            {$endif}
        end;
    end;
  if FirstTime then begin
      FirstTime := false;
      KM.Blocks.DeleteEOL(0);
  end;
end;


procedure TBLoadNote.ActOnTag(buff : string);
begin
  case Buff of
      'note-content' : InContent := true;
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
      '/create-date' : CreateDate := InStr;
      '/x' : X := strtointDef(InStr, 20);
      '/y' : Y := strtointDef(InStr, 20);
      '/width' : Width := strtointdef(InStr, 300);
      '/height' : height := strtointdef(InStr, 200);
       'text', 'note' : ;                             // a block of tags we ignore here.
      'x', 'y', 'title', '/title', '?xml', 'last-change-date', '/last-change-date', 'width', 'height', '/text' : ;
      'create-date', 'cursor-position', '/cursor-position', 'selection-bound-position', '/selection-bound-position' : ;
      'open-on-startup', '/open-on-startup', '/note', 'last-metadata-change-date', '/last-metadata-change-date' : ;
      'tag', '/tag', 'tags', '/tags' : ;
      // Note we do not process AND should not get 'list', '/list', 'list-item', '/list-item' here.
  otherwise debugln('TBLoadNote.ActOnTag ERROR sent an unrecognised tag [' + Buff + ']');
  end;
end;

procedure TBLoadNote.ReadList(fs : TFileStream);
var
    Buff : String;
    Ch : char = ' ';
    ST  : string = '';
    ListCount : integer = 1;

            function FindNextTag(OnIt : boolean) : boolean;
            begin
                Buff := '';
                Result := false;
                if (not OnIt) and (fs.Position < fs.Size) then fs.read(Ch, 1);
                while fs.Position < fs.Size do begin
                    if ch='<' then break;
                    fs.read(Ch, 1);
                end;
                if Ch <> '<' then begin
                  debugln('TBLoadNote.ReadList - ERROR, early exit from FindNextTag');
                  exit(false);
                end;
                while fs.Position < fs.Size do begin             // Capture the tag
                    fs.read(Ch, 1);
                    if (Ch = '>') or (Ch = ' ') then break;      // end of the content we need.
                    Buff := Buff + ch;
                end;
                if Ch in [' ', '>'] then begin
                    while (ch<>'>') and (fs.Position < fs.Size) do begin
                        fs.read(Ch, 1);
                        if Ch = '>' then break;
                    end;
                    Result := Ch = '>';
                end else debugln('TBLoadNote.ReadList - ERROR failed to find end of list');
                //if result then debugln('FindNextTag = ' + buff );
            end;

            procedure ListSt2KMemo();
            var
                i : integer = 1;
                ATag : string = '';
            begin
               // debugln('++++++++++ LstSt2KMemo : ' + St);
                InStr := '';
                BulletLevel := ListCount;
                while i <= St.length do begin
                    if St[i] = '<' then begin       // start of a tag
                        while i < St.length do begin
                            inc(i);
                            if St[i] = '>' then break
                            else ATag := ATag + St[i];
                        end;
                        if  St[i] <> '>' then begin
                            debugln('TBLoadNote.ReadList ERROR missing > in ' + St);
                            exit;
                        end;
                        AddText(False);
                        //debugln('call ActOnTag with ' + ATag);
                        ActOnTag(ATag);
                        ATag := '';
                        inc(i);
                    end else begin
                        InStr := InStr + St[i];
                        inc(i);
                    end;
                end;
                AddText(True);
                BulletLevel := 0;
                St := '';
                //debugln('++++++++++ Leaving LstSt2KMemo : ' + St);
            end;

begin
    if (InStr <> '') or (BulletLevel <> 0) then debugln('--------------- Bugger --------------');
    // Find the next tag, should always be list-item, ignore anything between
    //debugln('----------- We have just entered ReadList ---------');
    try
        if FindNextTag(False) and (Buff='list-item') then
            // Anything up to next list related tag is content.
            while fs.Position < fs.Size do begin
                fs.read(Ch, 1);
                if Ch in [#10, #13] then continue;           // ignore newline in list mode
                if ch='<' then begin
                    if FindNextTag(True) then begin
                        // debugln('ReadList, tag=[' + Buff + '] and St=' + St);
                        case Buff of
                            'list'       :  begin
                                                if St <> '' then ListSt2KMemo();
                                                inc(ListCount);
                                            end;
                            '/list'      :  if ListCount = 1 then exit else dec(ListCount);
                            '/list-item' :  if St <> '' then ListSt2KMemo();
                            'list-item'  : ;                                   // I THINK we don't need do anything with that ??
                            otherwise St := St + '<' + Buff + '>';  // put it back where you found it
                        end;
                    end;
                end else
                    St := St + Ch;
            end else exit;
        debugln('TBLoadNote.ReadList - ERROR, hit bottom of method');
    finally
         { We are left here with a trailing newline, remove it but we don't
          know if the note was created in Unix or Windows }
         if fs.Position < fs.Size then begin
            fs.read(Ch, 1);
            if ch <> #10 then begin                   // Linux and mac, what we expect
                if ch = #13 then begin                // B8#$# Windows
                    fs.read(Ch, 1);                   // OK, is probably #13#10
                    if ch <> #10 then                 // Woops, stop messing here !
                        fs.Seek(-1, fsFromCurrent);   // That should never happen
                end else fs.Seek(-1, fsFromCurrent);  // note #10, not #13 poke it back and run away !
            end;
         end;
         //debugln('----------- We have just left ReadList ---------');
    end;
end;


Procedure TBLoadNote.ReadTag(fs : TFileStream);    // we are here because '<'
var
    Buff : String;
    Ch : char = ' ';
begin
    //Addtext(False);    // Write the text we have so far with existing params
    Buff := '';   // now, lets set new params or get other data
    while fs.Position < fs.Size do begin
        fs.read(Ch, 1);
        if (Ch = '>') or (Ch = ' ') then begin     // we will exit after case statement
            // if InContent then debugln('ReadTag - Testing ' + Buff);
            if Buff = 'list' then
                ReadList(fs)
            else begin
                // debugln('Sending tag to ActOnTag =' + Buff);
                ActOnTag(Buff);
            end;
            while Ch <> '>' do fs.read(Ch, 1);          // eat everything else in the tag
            exit;
        end;
        Buff := Buff + Ch;
    end;
end;
{ When we hit a List or a /list-item, if there is content in InStr, flush it. }
end.
