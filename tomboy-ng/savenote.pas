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
	the name of a file (that may or may not exist) and the KMemo its
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
    2018/07/14  Fixed a misplaced 'end' in BulletList() that was skipping some of its tests.
    2018/07/27  Call RemoveBadCharacters(Title) in Header()
    2018/08/02  Fix to fixed width, better brackets and a 'not' where needed.
    2018/08/15  ReplaceAngles() works with bytes, not char, so don't use UTF8Copy and UTF8Length ....
    2018/12/04  Don't save hyperlinks's underline, its not real !
    2018/12/29  Small improvements in time to save a file.
    2019/04/29  Restore note's previous previous position and size.
    2019/05/06  Support saving pos and open on startup in note.
    2019/06/07  Removed unused, historical func to clean xml
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, KMemo, Graphics, LazLogger;


{type TNoteLocation = record
  X, Y, Width, Height : integer;
end;}

type TNoteUpdateRec = record
     CPos : shortstring;
     X, Y : shortstring;
     Width, Height : shortstring;
     OOS : shortstring;
     FFName : string;
     LastChangeDate : string;   // if '', its a content save, generate a new timestamp
     ErrorStr : string;         // '' if all OK, not used everywhere....
end;


type

    { TBSaveNote }

 TBSaveNote = class

       private
            OutStream:TMemoryStream;
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
            function AddTag(const FT : TKMemoTextBlock; var Buff : ANSIString; CloseOnly : boolean = False) : ANSIString;
			function BlockAttributes(Bk: TKMemoBlock): AnsiString;
			procedure BulletList(var Buff: ANSIString);
            procedure CopyLastFontAttr();
			function FontAttributes(const Ft : TFont; Ts : TKMemoTextStyle): ANSIString;
			//function RemoveBadCharacters(const InStr: ANSIString): ANSIString;
            function SetFontXML(Size : integer; TurnOn : boolean) : string;
          	function Header() : ANSIstring;
         	//function Footer(Loc: TNoteLocation): ANSIstring;
            function Footer(Loc : TNoteUpdateRec) : string;
            //function GetLocalTime():ANSIstring;

            // Assembles a list of tags this note is a member of, list is
            // used in all note's footer - needs ID to have been set
            // function NoteBookTags() : ANSIString;
       public
            TimeStamp : string;
            Title : ANSIString;
            // set to orig createdate if available, if blank, we'll use now()
            CreateDate : ANSIString;
            procedure SaveNewTemplate(NotebookName: ANSIString);
         	procedure ReadKMemo(FileName : ANSIString; KM1 : TKMemo);
            function WriteToDisk(const FileName: ANSIString; var NoteLoc: TNoteUpdateRec
                ): boolean;
            constructor Create;
            destructor Destroy;  override;
    end;


implementation

uses FileUtil               // Graphics needed for font style defines
    ,LazUTF8
    ,Settings				// User settings and some defines across units.
    ,SearchUnit				// So we have access to NoteBookList
    ,LazFileUtils           // For ExtractFileName...
    ,SyncUtils;             // For removebadxmlcharacters()

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

constructor TBSaveNote.Create;
begin
    OutStream := Nil;
end;

destructor TBSaveNote.Destroy;
begin
    if OutStream <> Nil then begin
        debugln('ERROR - ID=' + ID + '  outstream was not routinly freed .....');
        OutStream.Free;
        OutStream := Nil;
    end;
end;

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


function TBSaveNote.AddTag(const FT : TKMemoTextBlock; var Buff : ANSIString; CloseOnly : boolean = False) : ANSIString;
{var
   TestVar : Boolean;}
begin
    // Important that we keep the tag order consistent. Good xml requires no cross over
    // tags. If the note is to be readable by Tomboy, must comply. (EditBox does not care)
    // Tag order -
    // FontSize HiLite Ital Bold Bullet TEXT BulletOff BoldOff ItalOff HiLiteOff FontSize
	// Processing Order is the reverese -
    // ListOff BoldOff ItalicsOff HiLiteOff FontSize HiLite Ital Bold List

    //debugln(BlockAttributes(FT));

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
    if (HiLight and (not (FT.TextStyle.Brush.Color = Sett.HiColour))) then begin
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
                  //TestVar := (FT.TextStyle.Font.Pitch <> fpFixed);
                  //TestVar := (FT.TextStyle.Font.Name <> MonospaceFont);
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
        if Underline then Buff := Buff + '</underline>';                      //
        if Strikeout then Buff := Buff + '</strikeout>';                      //
                              // if strikeout, underline, fixedwidth here?

        Buff := Buff + SetFontXML(FSize, false);
        // better for pretty tags but generates invalid tags ! See below ....
        Buff := Buff + SetFontXML(FT.TextStyle.Font.Size, true);

        if Strikeout then Buff := Buff + '<strikeout>';                         //
        if Underline then Buff := Buff + '<underline>';                         //

        if HiLight then Buff := Buff + '<highlight>';
        if Italics then Buff := Buff + '<italic>';
        if Bold then Buff := Buff + '<bold>';
        FSize := FT.TextStyle.Font.Size;
    end;

    if CloseOnly then exit(Buff);
    // this is not ideal, it should happen after we have closed all fonts, before
    // we write new sizes but that difficult as we have only one flag, "FSize"
    // difficulity is that font size change is two step, other things are On/Off
    // Result is that xml tag for a new font jumps up blank lines. Not pretty
    // be nice to find another way..... DRB

    // FixedWidth turns ON
    if ((not FixedWidth) and ((FT.TextStyle.Font.Name = MonospaceFont) or (FT.TextStyle.Font.Pitch = fpFixed))) then begin
        //TestVar := (FT.TextStyle.Font.Name = MonospaceFont);
        //TestVar :=  (FT.TextStyle.Font.Pitch = fpFixed);
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
       if not FT.ClassNameIs('TKMemoHyperlink') then begin                            // Hyperlinks also have underline, don't save
           if Bold then Buff := Buff + '</bold>';
           if Italics then Buff := Buff + '</italic>';
           if HiLight then Buff := Buff + '</highlight>';
           Buff := Buff + '<underline>';
           if HiLight then Buff := Buff + '<highlight>';
           if Italics then Buff := Buff + '<italic>';
           if Bold then Buff := Buff + '<bold>';
           Underline := true;
       end;
    end;

    // Highlight turns ON
    if ((not HiLight) and (FT.TextStyle.Brush.Color = Sett.HiColour)) then begin
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
    end;
    if PrevUnderline then begin
        StartStartSt := StartStartSt + '</underline>';
        StartEndSt := '<underline>' + StartEndSt;
        // EndEndSt := '<underline>' + EndEndSt;
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
        StartEndSt := '<monospace>' + StartEndSt;
        // EndEndSt := '<monospace>' + EndEndSt;
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

    {writeLn('Buff at Start [' + Buff + ']');
    writeln('StartStart    [' + StartStartSt + ']');
    writeln('StartEnd      [' + StartEndSt + ']');
    writeln('EndStart      [' + EndStartSt + ']');
    writeln('EndEnd        [' + EndEndSt + ']');          }

    Buff := StartStartSt + '<list><list-item dir="ltr">' + StartEndSt
    		+ Buff + EndStartSt + '</list-item></list>' + EndEndSt;

    //writeLn('Buff at End [' + Buff + ']');         // **************************************
end;

// This is just a debug function.
function TBSaveNote.BlockAttributes(Bk : TKMemoBlock) : AnsiString;
begin
   Result := TKMemoTextBlock(BK).ClassName;
   if fsBold in TKMemoTextBlock(BK).TextStyle.Font.Style then
   Result := Result + ' Bold ';
   if fsItalic in TKMemoTextBlock(BK).TextStyle.Font.Style then
   Result := Result + ' Italic ';
   if TKMemoTextBlock(BK).TextStyle.Brush.Color = Sett.HiColour then
   Result := Result + ' HighLight ';
   Result := Result + ' size=' + inttostr(TKMemoTextBlock(BK).TextStyle.Font.Size);
   if fsUnderline in TKMemoTextBlock(BK).TextStyle.Font.Style then
        Result := Result + ' Underline ';
   if fsStrikeout in TKMemoTextBlock(BK).TextStyle.Font.Style then
   Result := Result + ' Strikeout ';
   if TKMemoTextBlock(BK).TextStyle.Font.Pitch = fpFixed then
   Result := Result + ' FixedWidth ';
   if TKMemoTextBlock(BK).ClassNameIs('TKMemoTextBlock') then Result := Result + ' [' + TKMemoTextBlock(BK).Text + ']';
   //else Result := 'Not Text';
end;

    // I suspect this function is no longer used.
function TBSaveNote.FontAttributes(const Ft : TFont; Ts : TKMemoTextStyle) : ANSIString;
begin
   Result := '';
   if fsBold in Ft.Style then
   Result := Result + ' Bold ';
   if fsItalic in Ft.Style then
   Result := Result + ' Italic ';
   if Ts.Brush.Color = Sett.HiColour then
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
   OStream:TFilestream;
   Buff { TemplateID }: ANSIString;
   Loc :  TNoteUpdateRec {TNoteLocation};
begin
   CreateGUID(GUID);
   Title := NotebookName  + ' Template';
   ID := copy(GUIDToString(GUID), 2, 36) + '.note';
   SearchForm.NoteLister.AddNoteBook(ID, NotebookName, True);
   Ostream :=TFilestream.Create(Sett.NoteDirectory + ID, fmCreate);
   Loc.Y := '20'; Loc.X := '20'; Loc.Height := '200'; Loc.Width:='300';
   Loc.OOS := 'False'; Loc.CPos:='1';
   try
   		Buff := Header();
        OStream.Write(Buff[1], length(Buff));
        Buff := Title + #10#10#10;
        OStream.Write(Buff[1], length(Buff));
        Buff := Footer(Loc);
        OStream.Write(Buff[1], length(Buff));
   finally
       OStream.Free;
   end;
end;

procedure TBSaveNote.CopyLastFontAttr();
begin
  PrevFSize := FSize;
  PrevBold := Bold;
   PrevItalics := Italics;
   PrevHiLight := HiLight;
  PrevUnderline := Underline;
  PrevStrikeout := Strikeout;
  PrevFixedWidth := FixedWidth;
  PrevFSize := FSize;
end;

procedure TBSaveNote.ReadKMemo(FileName : ANSIString; KM1 : TKMemo);
var
   Buff : ANSIstring = '';
   // OutStream:TFilestream;
   BlockNo : integer = 0;
   Block : TKMemoBlock;
   NextBlock : integer;
   // BlankFont : TFont;
 begin
    KM := KM1;
    FSize := Sett.FontNormal;
    Bold := false;
     Italics := False;
     HiLight := False;
    Underline := False;
     InList := false;
    FixedWidth := False;
    ID := ExtractFileNameOnly(FileName) + '.note';
    // ID needs to be set so we can get list of notebooks for the footer.
    // Must deal with an empty list !
//    try
        outstream :=TMemoryStream.Create({FileName, fmCreate});
        // Write and WriteBuffer accept a buffer, not a string !  Need to start at pos 1
        // when sending string or ANSIstring otherwise it uses first byte which makes it look like a binary file.
        // http://free-pascal-general.1045716.n5.nabble.com/Creating-text-files-with-TFileStream-td2824859.html
        Buff := Header();
        OutStream.Write(Buff[1], length(Buff));
        Buff := '';
        try
            repeat
                CopyLastFontAttr();
                repeat
                    Block := KM1.Blocks.Items[BlockNo];
                    // debugln('Block=' + inttostr(BlockNo) + ' ' +BlockAttributes(Block));

                    if Block.ClassNameIs('TKMemoParagraph') then break;	// discard end prev para
                    if Block.ClassNameIs('TKMemoTextBlock') then begin
                         if Block.Text.Length > 0 then begin
                        	AddTag(TKMemoTextBlock(Block), Buff);
                        	Buff := Buff + RemoveBadXMLCharacters(Block.Text);
						 end;
					end;
                    if Block.ClassNameIs('TKMemoHyperlink') then begin
                        AddTag(TKMemoHyperlink(Block), Buff);
                        Buff := Buff + RemoveBadXMLCharacters(Block.Text);
                    end;
                    //debugln('Block=' + inttostr(BlockNo) + ' ' +BlockAttributes(Block));
                    inc(BlockNo);
                    if BlockNo >= KM1.Blocks.Count then break;

                    // debugln('Inner Buff=[' + Buff + ']');

				until KM1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph');
                if BlockNo >= KM1.Blocks.Count then break;
                if  TKMemoParagraph(KM1.Blocks.Items[BlockNo]).Numbering = pnuBullets then
                     BulletList(Buff);

                // Add tags about to terminate to end of line, pretty XML
                // However does not work for font size changes !

                // Note - para blocks CAN have font attributs (eg, underline etc).
                // debugln('Outer 1 Buff=[' + Buff + ']');
                // Now, look ahead and see if we need close things ....
                // This makes bad decision for font size changes, we end up with empty tags but does no real harm.
                NextBlock := BlockNo + 1;
                while NextBlock < KM1.Blocks.Count do begin
                    if KM1.Blocks.Items[NextBlock].ClassNameIs('TKMemoTextBlock') then begin
                        AddTag(TKMemoTextBlock(KM1.Blocks.Items[NextBlock]), Buff, True);
                        break;
                    end else inc(NextBlock);
                end;
                Buff := Buff + LineEnding;
                // debugln('Outer Buff=[' + Buff + ']');
                OutStream.Write(Buff[1], length(Buff));
                Buff := '';
                // debugln('Block=' + inttostr(BlockNo) + ' ' +BlockAttributes(KM1.Blocks.Items[BlockNo]));
                inc(BlockNo);
                if BlockNo >= KM1.Blocks.Count then break;
			until false;

            { At this point we may have unsaved content in Buff cos last block was not
              a Para. But it cannot be Bullet. If it was a Para, Buff is empty. But we
              could still have hanging xml tags. So either case, send it to add tag with
              an empty Font.
            }
            if Buff <> '' then
                OutStream.Write(Buff[1], length(Buff));
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
            //Buff := Footer();
            //OutStream.Write(Buff[1], length(Buff));

         Except     { TODO 1 : Must test this to see what happens with an empty
         				list of blocks. Probably makes sense to not save anything
                        that does not have at least one TKMemotextBlock  }
            on EListError do begin
                debugln('ERROR - EListError while writing note to stream.');
                { we now do footer in the WriteToDisk()
            	Buff := Footer();
            	OutStream.Write(Buff[1], length(Buff)); }
            end;
        end;
{ 	finally
        OutStream.Free;
    end;       }
end;

// gets called (from outside) after all content assembled.  Its done from outside
// as the calling unit has control of KMemo's locking.
function TBSaveNote.WriteToDisk(const FileName: ANSIString; var NoteLoc : TNoteUpdateRec) : boolean;
var
   Buff : string = '';
   TmpName : string;
   {$ifdef WINDOWS}FileAttr : longint;
   ErrorMsg : string; {$endif}
begin

    Result := True;
    // we write out the footer here so we can do the searching to notebook stuff
    // after we have released to lock on KMemo.
    Buff := Footer(NoteLoc);
    OutStream.Write(Buff[1], length(Buff));
    // We save the file in tmp, when closed,
    // move it over to the actual position. That will prevent, to some extent, poweroff
    // crashes messing with files.  May generate an EStreamError

    {$define STAGEDWRITE}
    {$ifdef STAGEDWRITE}
        TmpName := AppendPathDelim(Sett.NoteDirectory) + 'tmp';
        if not DirectoryExists(TmpName) then
           if not CreateDir(AppendPathDelim(tmpname)) then begin
                NoteLoc.ErrorStr:='Failed Create Dir';
                exit(False);
            end;
        TmpName := TmpName + pathDelim + extractFileName(FileName);
        try
            OutStream.SaveToFile(TmpName);
        finally
            OutStream.Free;
            OutStream := nil;
        end;
        {$ifdef WINDOWS}
            if FileExists(FileName) then    // will not be there if its a new note.
                if not SafeWindowsDelete(FileName, ErrorMsg) then
                   exit(false);
        {$endif}
        result := RenameFileUTF8(TmpName, FileName);    // Unix ok to over write, windows is not !
    {$else}        // thats the ifdef StagedWrite, here we write directly to note file.
        try
            OutStream.SavetoFile(FileName);
        finally
            OutStream.Free;
            OutStream := nil;
        end;
    {$endif}                                        // thats the ifdef StagedWrite
    if not Result then NoteLoc.ErrorStr:='Failed Rename';
end;

{                    // moved to note lister
function TBSaveNote.NoteBookTags(): ANSIString;
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
}

function TBSaveNote.Header(): ANSIstring;
var
   S1, S2, S3, S4 : ANSIString;
begin
  S1 := '<?xml version="1.0" encoding="utf-8"?>'#10'<note version="0.3" xmlns:link="';
  S2 := 'http://beatniksoftware.com/tomboy/link" xmlns:size="http://beatniksoftware.com/tomboy/size"';
  S3 := ' xmlns="http://beatniksoftware.com/tomboy">'#10'  <title>';
  S4 := '</title>'#10'  <text xml:space="preserve"><note-content version="0.1">';
  Result := S1 + S2 + S3 + RemoveBadXMLCharacters(Title) + S4;
end;


function TBSaveNote.Footer(Loc : TNoteUpdateRec {TNoteLocation}): ANSIstring;
var
   S1, S2, S3, S4, S5, S6 : string;
begin
  if Loc.LastChangeDate = '' then
    TimeStamp := GetCurrentTimeStr()   // get actual time date in format like Tomboy's
  else TimeStamp := Loc.LastChangeDate;
  S1 := '</note-content></text>'#10'  <last-change-date>';
  S2 := '</last-change-date>'#10'  <last-metadata-change-date>';
  S3 := '</last-metadata-change-date>'#10'  <create-date>';
  S4 := '</create-date>'#10'  <cursor-position>' + Loc.CPos + '</cursor-position>'#10'  <selection-bound-position>1</selection-bound-position>'#10;
  S5 := '  <width>' + Loc.Width + '</width>'#10'  <height>' + Loc.Height + '</height>'#10'  <x>'
        + Loc.X + '</x>'#10'  <y>' + Loc.Y + '</y>'#10;
  S6 := '  <open-on-startup>' + Loc.OOS + '</open-on-startup>'#10'</note>';
  if CreateDate = '' then CreateDate := TimeStamp;
  if SearchForm.NoteLister <> Nil then
        Result := S1 + TimeStamp + S2 + TimeStamp + S3 + CreateDate + S4 + S5
            + SearchForm.NoteLister.NoteBookTags(ID) + S6
  else
        Result := S1 + TimeStamp + S2 + TimeStamp + S3 + CreateDate + S4 + S5 + S6;
  // ToDo : That will mean no Notebook tags in single note mode, is that an issue ?
  // Most singe notes are out of their repo so won't have notebooks anyway but we could
  // save any tag list and restore it on save ??
end;

end.
