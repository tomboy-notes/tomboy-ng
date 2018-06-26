unit SaveNote;

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

{  This unit is responsible for saving a note in the Tomboy XML format.
    After creation, the class needs to be told the current FontNormal
	size and the CreatDate if any. If the supplied CreatDate is '', it will
	stamp it Now().
    All the work is done in the Save(..) function, it needs to be passed
	the name of a file (that may or may not exist) and the RichMemo its
	getting its content from.
}

{	HISTORY
	20170927 - added Hyperlink to blocks to be saved.
	2017/11/4 - replaced GetLocalTime() with one from TB_Sync, it puts minutes
				into the time offset figure, eg +11:00. Old notes written
				with previous vesions will fail with file sync until rewritten.
	2017/11/12  Added code to replace < and > with char codes.
	2017/12/02	Fixed a bug were we were skipping newline where there were 2 in a row
	2017/12/02  Extensive changes to ensure font setting spanning part of a bullet
				list are saved correctly.
	2017/12/02	Restructured AddTag to ensure tags laid out in correct order.
	2017/12/02	changed the way that we ensure there are no hanging tags at end of
				a note.
	2017/12/10  Fix a bug in BulletList() whereby font changes were not preserving
				previous queued format changes. Possibly. This is not robust code.
	2018/01/01  Yet another bug fix for BulletList(), this time I've got it !
	2018/01/25  Changes to support Notebooks
    2018/01/31  Added code to reprocess &
    2018/05/12  Extensive changes - MainUnit is now just that. Only change here relates
                to naming of MainUnit and SearchUnit.
    2018/06/26  Some <italic> tags an an 's' at the end.  Changed the test for when
                FixedWidth turns on in AddTag().
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, KMemo, Graphics, LazLogger;

type

    { TBSaveNote }

 TBSaveNote = class

       private
            ID : ANSIString;
            FSize : integer;
			Bold : boolean;
			Italics : boolean;
			HiLight : boolean;
                        Underline : boolean;
                        Strikeout : boolean;
                        FixedWidth : boolean;
            PrevFSize : integer;
			PrevBold : boolean;
			PrevItalics : boolean;
			PrevHiLight : boolean;
                        PrevUnderline : boolean;
                        PrevStrikeout : boolean;
                        PrevFixedWidth : boolean;
			InList : boolean;
            KM : TKMemo;
            function AddTag(const FT : TKMemoTextBlock; var Buff : ANSIString) : ANSIString;
			function BlockAttributes(Bk: TKMemoBlock): AnsiString;
			procedure BulletList(var Buff: ANSIString);
			function FontAttributes(const Ft : TFont; Ts : TKMemoTextStyle): ANSIString;
			function RemoveBadCharacters(const InStr: ANSIString): ANSIString;
            function SetFontXML(Size : integer; TurnOn : boolean) : string;
          	function Header() : ANSIstring;
         	Function Footer() : ANSIstring;
            function GetLocalTime():ANSIstring;
            function NoteBookTags() : ANSIString;
       public
            TimeStamp : string;
            Title : ANSIString;
      //      FontNormal : integer;		// Needs to be set after class created (could get from settings??)
            CreateDate : ANSIString;
            procedure SaveNewTemplate(NotebookName: ANSIString);
         	procedure Save(FileName : ANSIString; KM1 : TKMemo);
    end;


implementation

uses FileUtil               // Graphics needed for font style defines
    ,LazUTF8
    ,Settings				// User settings and some defines across units.
    ,SearchUnit				// So we have access to NoteBookList
    ,LazFileUtils           // For ExtractFileName...
    {$ifdef LINUX}, Unix {$endif} ;              // We call a ReReadLocalTime()

const
  {$ifdef LINUX}
  MonospaceFont = 'monospace';
  {$ifend}
  {$ifdef WINDOWS}
  MonospaceFont = 'Lucida Console';
  {$ifend}
  {$ifdef DARWIN}
  MonospaceFont = 'Lucida Console';
  {$ifend}

function TBSaveNote.SetFontXML(Size : integer; TurnOn : boolean) : string;
begin
    Result := '';
	if Size = Sett.FontHuge then
         if TurnOn then Result  := '<size:huge>' else Result  := '</size:huge>';
    if Size = sett.FontLarge then
         if TurnOn then Result  := '<size:large>' else Result  := '</size:large>';
    if Size = Sett.FontSmall then
         if TurnOn then Result  := '<size:small>' else Result  := '</size:small>';
end;


function TBSaveNote.AddTag(const FT : TKMemoTextBlock; var Buff : ANSIString) : ANSIString;
begin
    // Important that we keep the tag order consistent. Good xml requires no cross over
    // tags. If the note is to be readable by Tomboy, must comply. (EditBox does not care)
    // Tag order -
    // FontSize HiLite Ital Bold Bullet TEXT BulletOff BoldOff ItalOff HiLiteOff FontSize
	// Processing Order is the reverese -
    // ListOff BoldOff ItalicsOff HiLiteOff FontSize HiLite Ital Bold List

  // When Bold Turns OFF
    if (Bold and (not (fsBold in FT.TextStyle.Font.Style))) then begin
        Buff := Buff + '</bold>';
        Bold := false;
    end;

    // When Italic turns OFF
    if (Italics and (not (fsItalic in FT.TextStyle.Font.Style))) then begin
		if Bold then Buff := Buff + '</bold>';
        Buff := Buff + '</italic>';
        if Bold then Buff := Buff + '<bold>';
        Italics := false;
    end;

    // When Highlight turns OFF
    if (HiLight and (not (FT.TextStyle.Brush.Color = HiColor))) then begin
		if Bold then Buff := Buff + '</bold>';
        if Italics then Buff := Buff + '</italic>';
        Buff := Buff + '</highlight>';
        if Italics then Buff := Buff + '<italic>';
        if Bold then Buff := Buff + '<bold>';
        HiLight := false;
    end;

    // When Underline turns OFF
    if (Underline and (not (fsUnderline in FT.TextStyle.Font.Style))) then begin
                  if Bold then Buff := Buff + '</bold>';
                  if Italics then Buff := Buff + '</italic>';
                  if HiLight then Buff := Buff + '</highlight>';
                  Buff := Buff + '</underline>';
                  if HiLight then Buff := Buff + '<highlight>';
                  if Italics then Buff := Buff + '<italic>';
                  if Bold then Buff := Buff + '<bold>';
                  Underline := false;
    end;

    // When Strikeout turns OFF
    if (Strikeout and (not (fsStrikeout in FT.TextStyle.Font.Style))) then begin
                  if Bold then Buff := Buff + '</bold>';
                  if Italics then Buff := Buff + '</italic>';
                  if HiLight then Buff := Buff + '</highlight>';
                  if Underline then Buff := Buff + '</underline>';
                  Buff := Buff + '</strikeout>';
                  if Underline then Buff := Buff + '<underline>';
                  if HiLight then Buff := Buff + '<highlight>';
                  if Italics then Buff := Buff + '<italic>';
                  if Bold then Buff := Buff + '<bold>';
                  Strikeout := false;
    end;

    // When FixedWidth turns OFF
    //if (FixedWidth <> (FT.TextStyle.Font.Pitch = fpFixed) or (FT.TextStyle.Font.Name = MonospaceFont)) then begin
    if (FixedWidth and ((FT.TextStyle.Font.Pitch <> fpFixed) or (FT.TextStyle.Font.Name <> MonospaceFont))) then begin
                  if Bold then Buff := Buff + '</bold>';
                  if Italics then Buff := Buff + '</italic>';
                  if HiLight then Buff := Buff + '</highlight>';
                  if Underline then Buff := Buff + '</underline>';
                  if Strikeout then Buff := Buff + '</strikeout>';
                  Buff := Buff + '</monospace>';
                  if Strikeout then Buff := Buff + '<strikeout>';
                  if Underline then Buff := Buff + '<underline>';
                  if HiLight then Buff := Buff + '<highlight>';
                  if Italics then Buff := Buff + '<italic>';
                  if Bold then Buff := Buff + '<bold>';
                  FixedWidth := false;
    end;

    // When Font size changes
    if (FSize <> FT.TextStyle.Font.Size) and (FT.TextStyle.Font.Size <> Sett.FontTitle) then begin
		if Bold then Buff := Buff + '</bold>';
        if Italics then Buff := Buff + '</italic>';
        if HiLight then Buff := Buff + '</highlight>';
                              // if strikeout, underline, fixedwidth here?
        Buff := Buff + SetFontXML(FSize, false);
        Buff := Buff + SetFontXML(FT.TextStyle.Font.Size, true);
        if HiLight then Buff := Buff + '<highlight>';
        if Italics then Buff := Buff + '<italic>';
        if Bold then Buff := Buff + '<bold>';
        FSize := FT.TextStyle.Font.Size;
    end;

    // FixedWidth turns ON
    if (FixedWidth and (FT.TextStyle.Font.Name = MonospaceFont) or (FT.TextStyle.Font.Pitch = fpFixed)) then begin
        if Bold then Buff := Buff + '</bold>';
        if Italics then Buff := Buff + '</italic>';
        if HiLight then Buff := Buff + '</highlight>';
        if Underline then Buff := Buff + '</underline>';
        if Strikeout then Buff := Buff + '</strikeout>';
        Buff := Buff + '<monospace>';
        if Strikeout then Buff := Buff + '<strikeout>';
        if Underline then Buff := Buff + '<underline>';
        if HiLight then Buff := Buff + '<highlight>';
        if Italics then Buff := Buff + '<italic>';
        if Bold then Buff := Buff + '<bold>';
        FixedWidth := true;
    end;

    // Strikeout turns ON
    if ((not Strikeout) and (fsStrikeout in FT.TextStyle.Font.Style)) then begin
        if Bold then Buff := Buff + '</bold>';
        if Italics then Buff := Buff + '</italic>';
        if HiLight then Buff := Buff + '</highlight>';
        if Underline then Buff := Buff + '</underline>';
        Buff := Buff + '<strikeout>';
        if Underline then Buff := Buff + '<underline>';
        if HiLight then Buff := Buff + '<highlight>';
        if Italics then Buff := Buff + '<italic>';
        if Bold then Buff := Buff + '<bold>';
        Strikeout := true;
    end;


    // Underline turns ON
    if ((not Underline) and (fsUnderline in FT.TextStyle.Font.Style)) then begin
       if Bold then Buff := Buff + '</bold>';
       if Italics then Buff := Buff + '</italic>';
       if HiLight then Buff := Buff + '</highlight>';
       Buff := Buff + '<underline>';
       if HiLight then Buff := Buff + '<highlight>';
       if Italics then Buff := Buff + '<italic>';
       if Bold then Buff := Buff + '<bold>';
       Underline := true;
    end;

    // Highlight turns ON
    if ((not HiLight) and (FT.TextStyle.Brush.Color = HiColor)) then begin
        if Bold then Buff := Buff + '</bold>';
        if Italics then Buff := Buff + '</italic>';
        Buff := Buff + '<highlight>';
        if Italics then Buff := Buff + '<italic>';
        if Bold then Buff := Buff + '<bold>';
        HiLight := true;
    end;

    // Italic turns On
    if ((not Italics) and (fsItalic in FT.TextStyle.Font.Style)) then begin
        if Bold then Buff := Buff + '</bold>';
        Buff := Buff + '<italic>';
        if Bold then Buff := Buff + '<bold>';
        Italics := true;
    end;

    // Bold turns On
    if ((not Bold) and (fsBold in FT.TextStyle.Font.Style)) then begin
        Buff := Buff + '<bold>';
        Bold := true;
    end;
    Result := Buff;
end;

	{ This function takes an existing parsed string and wraps it in the necessary
      bullet tags but has to resolve any pending formatting tags first and restore
      then afterwards. Its horrible. If you are debugging this, I am truly sorry.
    }
    // ListOff BoldOff ItalicsOff HiLiteOff FontSize HiLite Ital Bold List

procedure TBSaveNote.BulletList(var Buff : ANSIString);
var
   StartStartSt, StartEndSt, EndStartSt, EndEndSt : ANSIString;
begin
	//writeln('Status Bold=', Bold=True, ' PBold=', PrevBold=True, ' High=', HiLight=True, ' PHigh=', PrevHiLight=True);
    StartStartSt := '';
    StartEndSt := '';
    EndStartSt := '';
    EndEndSt := '';
    if PrevBold then begin
        StartStartSt := '</bold>';     // Starting String, Start
        StartEndSt := '<bold>';        // Starting String, End
	end;
	if Bold then begin
        EndStartSt := '</bold>';		// End String, start of it
        EndEndSt := '<bold>';			// End String, end of it
    end;
    if PrevItalics then begin
        StartStartSt := StartStartSt + '</italic>';
        StartEndSt := '<italic>' + StartEndSt;
	end;
	if Italics then begin
        EndStartSt := EndStartSt + '</italic>';
        EndEndSt := '<italic>' + EndEndSt;
	end;
    if PrevHiLight then begin
        StartStartSt := StartStartSt + '</highlight>';
        StartEndSt := '<highlight>' + StartEndSt;
	end;
	if HiLight then begin
        EndStartSt := EndStartSt + '</highlight>';
        EndEndSt := '<highlight>' + EndEndSt;
    if PrevUnderline then begin
        StartStartSt := StartStartSt + '</underline>';
        EndEndSt := '<underline>' + EndEndSt;
        end;
        if Underline then begin
        EndStartSt := EndStartSt + '</underline>';
        EndEndSt := '<underline>' + EndEndSt;
        end;
    if PrevStrikeout then begin
        StartStartSt := StartStartSt + '</strikeout>';
        EndEndSt := '<strikeout>' + EndEndSt;
        end;
        if Strikeout then begin
        EndStartSt := EndStartSt + '</strikeout>';
        EndEndSt := '<strikeout>' + EndEndSt;
        end;
    if PrevFixedWidth then begin
        StartStartSt := StartStartSt + '</monospace>';
        EndEndSt := '<monospace>' + EndEndSt;
        end;
        if FixedWidth then begin
        EndStartSt := EndStartSt + '</monospace>';
        EndEndSt := '<monospace>' + EndEndSt;
        end;
    if PrevFSize <> Sett.FontNormal then begin
        StartStartSt := StartStartSt + SetFontXML(PrevFSize, False);
        StartEndSt := SetFontXML(PrevFSize, True) + StartEndSt;
	end;
    if FSize <> Sett.FontNormal then begin
        EndStartSt := EndStartSt + SetFontXML(FSize, False);
        EndEndSt := SetFontXML(FSize, True) + EndEndSt;
	end;
end;

    {writeLn('Buff at Start [' + Buff + ']');
    writeln('StartStart    [' + StartStartSt + ']');
    writeln('StartEnd      [' + StartEndSt + ']');
    writeln('EndStart      [' + EndStartSt + ']');
    writeln('EndEnd        [' + EndEndSt + ']');}

    Buff := StartStartSt + '<list><list-item dir="ltr">' + StartEndSt
    		+ Buff + EndStartSt + '</list-item></list>' + EndEndSt;

    //writeLn('Buff at End [' + Buff + ']');         // **************************************
end;

function TBSaveNote.RemoveBadCharacters(const InStr : ANSIString) : ANSIString;
// It appears that Tomboy only processes <, > and &
var
   //Res : ANSIString;
   Index : longint = 1;
   Start : longint = 1;
begin
    Result := '';
   while Index <= UTF8length(InStr) do begin
   		if InStr[Index] = '<' then begin
             Result := Result + UTF8Copy(InStr, Start, Index - Start);
             Result := Result + '&lt;';
             inc(Index);
             Start := Index;
			 continue;
		end;
  		if InStr[Index] = '>' then begin
             Result := Result + UTF8Copy(InStr, Start, Index - Start);
             Result := Result + '&gt;';
             inc(Index);
             Start := Index;
			 continue;
		end;
  		if InStr[Index] = '&' then begin
             Result := Result + UTF8Copy(InStr, Start, Index - Start);
             Result := Result + '&amp;';
             inc(Index);
             Start := Index;
			 continue;
		end;

        inc(Index);
   end;
   Result := Result + UTF8Copy(InStr, Start, Index - Start);
end;

function TBSaveNote.BlockAttributes(Bk : TKMemoBlock) : AnsiString;
begin
   Result := '';
   if fsBold in TKMemoTextBlock(BK).TextStyle.Font.Style then
   Result := Result + ' Bold ';
   if fsItalic in TKMemoTextBlock(BK).TextStyle.Font.Style then
   Result := Result + ' Italic ';
   if TKMemoTextBlock(BK).TextStyle.Brush.Color = HiColor then
   Result := Result + ' HighLight ';
   Result := Result + inttostr(TKMemoTextBlock(BK).TextStyle.Font.Size);
   if fsUnderline in TKMemoTextBlock(BK).TextStyle.Font.Style then
   Result := Result + ' Underline ';
   if fsStrikeout in TKMemoTextBlock(BK).TextStyle.Font.Style then
   Result := Result + ' Strikeout ';
   if TKMemoTextBlock(BK).TextStyle.Font.Pitch = fpFixed then
   Result := Result + ' FixedWidth ';

end;

function TBSaveNote.FontAttributes(const Ft : TFont; Ts : TKMemoTextStyle) : ANSIString;
begin
   Result := '';
   if fsBold in Ft.Style then
   Result := Result + ' Bold ';
   if fsItalic in Ft.Style then
   Result := Result + ' Italic ';
   if Ts.Brush.Color = HiColor then
   Result := Result + ' HighLight ';
   Result := Result + inttostr(Ft.Size);
   if fsUnderline in Ft.Style then
   Result := Result + ' Underline ';
   if fsStrikeout in Ft.Style then
   Result := Result + ' Strikeout ';
   if Ft.Pitch = fpFixed then
   Result := Result + ' FixedWidth ';
end;

procedure TBSaveNote.SaveNewTemplate(NotebookName : ANSIString);
var
   GUID : TGUID;
   OutStream:TFilestream;
   Buff { TemplateID }: ANSIString;
begin
   CreateGUID(GUID);
   Title := NotebookName  + ' Template';
   ID := copy(GUIDToString(GUID), 2, 36) + '.note';
   SearchForm.NoteLister.AddNoteBook(ID, NotebookName, True);
   Outstream :=TFilestream.Create(Sett.NoteDirectory + ID, fmCreate);
   try
   		Buff := Header();
        OutStream.Write(Buff[1], length(Buff));
        Buff := Title + #10#10#10;
        OutStream.Write(Buff[1], length(Buff));
        Buff := Footer();
        OutStream.Write(Buff[1], length(Buff));
   finally
       OutStream.Free;
   end;
end;

procedure TBSaveNote.Save(FileName : ANSIString; KM1 : TKMemo);
var
   Buff : ANSIstring = '';
   OutStream:TFilestream;
   BlockNo : integer = 0;
   Block : TKMemoBlock;
   // BlankFont : TFont;
 begin
    KM := KM1;
    FSize := Sett.FontNormal;
	Bold := false;
 	Italics := False;
 	HiLight := False;
        Underline := False;
 	InList := false;
    ID := ExtractFileNameOnly(FileName) + '.note';
            // Must deal with an empty list !
    try
        outstream :=TFilestream.Create(FileName, fmCreate);
        // Write and WriteBuffer accept a buffer, not a string !  Need to start at pos 1
        // when sending string or ANSIstring otherwise it uses first byte which makes it look like a binary file.
        // http://free-pascal-general.1045716.n5.nabble.com/Creating-text-files-with-TFileStream-td2824859.html
        Buff := Header();
        OutStream.Write(Buff[1], length(Buff));
        Buff := '';
        try
            repeat
                PrevFSize := FSize;
				PrevBold := Bold;
 				PrevItalics := Italics;
 				PrevHiLight := HiLight;
                                PrevUnderline := Underline;
                                PrevStrikeout := Strikeout;
                                PrevFixedWidth := FixedWidth;
                PrevFSize := FSize;
                repeat
                    Block := KM1.Blocks.Items[BlockNo];
                    if Block.ClassNameIs('TKMemoParagraph') then break;	// two newlines
                    if Block.ClassNameIs('TKMemoTextBlock') then begin
                         if Block.Text.Length > 0 then begin
                        	AddTag(TKMemoTextBlock(Block), Buff);
                        	Buff := Buff + RemoveBadCharacters(Block.Text);
						 end;
					end;
                    if Block.ClassNameIs('TKMemoHyperlink') then begin
                        AddTag(TKMemoHyperlink(Block), Buff);
                        Buff := Buff + RemoveBadCharacters(Block.Text);
                    end;
                    inc(BlockNo);
                    if BlockNo >= KM1.Blocks.Count then break;

				until KM1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph');
                if BlockNo >= KM1.Blocks.Count then break;
                if  TKMemoParagraph(KM1.Blocks.Items[BlockNo]).Numbering = pnuBullets then
                     BulletList(Buff);
                Buff := Buff + LineEnding;
                OutStream.Write(Buff[1], length(Buff));
                Buff := '';
                inc(BlockNo);
                if BlockNo >= KM1.Blocks.Count then break;
			until false;

            { At this point we may have unsaved content in Buff cos last block was not
              a Para. But it cannot be Bullet. If it was a Para, Buff is empty. But we
              could still have hanging xml tags. So either case, send it to add tag with
              an empty Font.
            }
            Buff := '';
            if Bold then Buff := '</bold>';
            if Italics then Buff := Buff + '</italic>';
            if HiLight then Buff := Buff + '</highlight>';
            if Underline then Buff := Buff + '</underline>';
            if Strikeout then Buff := Buff + '</strikeout>';
            if FixedWidth then Buff := Buff + '</monospace>';
            if FSize <> Sett.FontNormal then
                 Buff := Buff + SetFontXML(FSize, False);
            if length(Buff) > 0 then
                  OutStream.Write(Buff[1], length(Buff));
            Buff := Footer();
            OutStream.Write(Buff[1], length(Buff));

         Except     { TODO 1 : Must test this to see what happens with an empty
         				list of blocks. Probably makes sense to not save anything
                        that does not have at least one TKMemotextBlock  }
            on EListError do begin
            	Buff := Footer();
            	OutStream.Write(Buff[1], length(Buff));
            end;
        end;
 	finally
        OutStream.Free;
    end;
end;


function TBSaveNote.GetLocalTime: ANSIstring;
var
   ThisMoment : TDateTime;
   Res : ANSIString;
   Off : longint;
begin
   // Note this function is duplicated in TB_Sync.
    {$ifdef LINUX}
    ReReadLocalTime();    // in case we are near daylight saving time changeover
    {$endif}
    ThisMoment:=Now;
    Result := FormatDateTime('YYYY-MM-DD',ThisMoment) + 'T'
                   + FormatDateTime('hh:mm:ss.zzz"0000"',ThisMoment);
    Off := GetLocalTimeOffset();
    if (Off div -60) >= 0 then Res := '+'
	else Res := '-';
	if abs(Off div -60) < 10 then Res := Res + '0';
	Res := Res + inttostr(abs(Off div -60)) + ':';
       	if (Off mod 60) = 0 then
		Res := res + '00'
	else Res := Res + inttostr(abs(Off mod 60));
    Result := Result + res;
end;

function TBSaveNote.NoteBookTags: ANSIString;
var
        SL : TStringList;
        Index : Integer;
begin
   Result := '';
   if SearchForm.NoteLister = nil then exit;
   SL := TStringList.Create;
   if SearchForm.NoteLister.GetNotebooks(SL, ID) then begin  // its a template
   		Result := '  <tags>'#10'    <tag>system:template</tag>'#10;
        if SL.Count > 0 then
        	Result := Result + '    <tag>system:notebook:' + SL[0] + '</tag>'#10'  </tags>'#10;
   end else
   		if SL.Count > 0 then begin					// its a Notebook Member
        	Result := '  <tags>'#10;
        	for Index := 0 to SL.Count -1 do		// here, we auto support multiple notebooks.
        		Result := Result + '    <tag>system:notebook:' + SL[Index] + '</tag>'#10;
        	Result := Result + '  </tags>'#10;
		end;
    SL.Free;
end;

function TBSaveNote.Header: ANSIstring;
var
   S1, S2, S3, S4 : ANSIString;
begin
  S1 := '<?xml version="1.0" encoding="utf-8"?>'#10'<note version="0.3" xmlns:link="';
  S2 := 'http://beatniksoftware.com/tomboy/link" xmlns:size="http://beatniksoftware.com/tomboy/size"';
  S3 := ' xmlns="http://beatniksoftware.com/tomboy">'#10'  <title>';
  S4 := '</title>'#10'  <text xml:space="preserve"><note-content version="0.1">';
  Result := S1 + S2 + S3 + Title + S4;
end;


function TBSaveNote.Footer: ANSIstring;
var
   S1, S2, S3, S4, S5, S6 : string;

begin
  TimeStamp := GetLocalTime();   // get actual time date in format like Tomboy's
  S1 := '</note-content></text>'#10'  <last-change-date>';
  S2 := '</last-change-date>'#10'  <last-metadata-change-date>';
  S3 := '</last-metadata-change-date>'#10'  <create-date>';
  S4 := '</create-date>'#10'  <cursor-position>1</cursor-position>'#10'  <selection-bound-position>1</selection-bound-position>'#10;
  S5 := '  <width>1000</width>'#10'  <height>626</height>'#10'  <x>0</x>'#10'  <y>0</y>'#10;
  S6 := '  <open-on-startup>False</open-on-startup>'#10'</note>';
  if CreateDate = '' then CreateDate := TimeStamp;
  Result := S1 + TimeStamp + S2 + TimeStamp + S3 + CreateDate + S4 + S5 + NoteBookTags + S6;
end;

end.
