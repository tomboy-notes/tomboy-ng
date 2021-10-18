unit SaveNote;

{   Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------

    This unit is responsible for saving a note in the Tomboy XML format.
    After creation, the class needs to be told the current FontNormal
	size and the CreatDate if any. If the supplied CreatDate is '', it will
	stamp it Now().
    All the work is done in the Save(..) function, it needs to be passed
	the name of a file (that may or may not exist) and the KMemo its
	getting its content from.

    New Model - Saving is now a threaded operation and that happens in EditBox
    The class is created in EditBox, then ReadKMemo is called, it puts an XML
    version of the note into the passed StringList. The class can then be freed.
    ReadKMemo decides to put xml into either the StringList or a stream (that
    can be saved from here) on basis of if a StringList is passed or not.

    ----- Tag order -----
    List, Bold, Italics HiLite Underline Strikeout Monospace Fontsize
    Processing Order is the reverese, so, and and of line might contain (in extreame case) -
    ListOff BoldOff ItalicsOff HiLiteOff UnderOff StrikeOff MonoOff _FontSize_ MonoSpace Strikeout Underline HiLite Ital Bold List
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
    2020/07/17  Esc bad XML in Template name.
    2020/08/08  Added a BOM, a Byte Order Mark, at start of a note.
    2020/08/10  Removed BOM, no advantage I can find, undefined risk.
    2021/08/28  Can now save multilevel bullets
    2021/10/05  Bug where a line in a monospace block was getting a </monospace> even if the
                next line was monospace, Linux. Because that individual line wraping is
                useful, I now do it in notenormal.
    2021/10/15  Serious change, now terminate font size changes at end of line where necessary
                vastly better xml.

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
     FFName : string;           // path, ID and .note, not used in Single Note Mode
     LastChangeDate : string;   // if '', its a content save, generate a new timestamp
     CreateDate : string;       // if its '', its a new note, use LastChangeDate
     ErrorStr : string;         // '' if all OK, not used everywhere....
     // ToDo : remove the field ErrorStr
end;


type

    { TBSaveNote }

 TBSaveNote = class

       private
            OutStream:TMemoryStream;
            ID : ANSIString;
            FSize : integer;             // Current Font Size, compare to Sett.Font*
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
                    { This function key to whole parser. It uses a set of regional vars that remember
                    the current style attributes and compare it to a newly arriving block, writing
                    tags accordingly. Special case when CloseOnly is true, we terminate all active styles
                    and reactivate them (NoteNormal will move the reactivated tags to next line later).
                    Absolutly vital that tag order be observed, crossed ove r tgas are a very bad thing.}
            function AddTag(const FT : TKMemoTextBlock; var Buff : ANSIString; CloseOnly : boolean = False) : ANSIString;
			function BlockAttributes(Bk: TKMemoBlock): AnsiString;
			procedure BulletList(Level: TKMemoParaNumbering; var Buff: ANSIString);
            procedure CopyLastFontAttr();
            function SetFontXML(Size : integer; TurnOn : boolean) : string;

       public
            // TimeStamp : string;            // abandonded in SaveThread mode
            // Title : ANSIString;
            // set to orig createdate if available, if blank, we'll use now()
            CreateDate : ANSIString;
         	procedure ReadKMemo(FileName: ANSIString; Title: string; KM1: TKMemo;
					STL: TStringList = nil);
            function WriteToDisk(const FileName: ANSIString; var NoteLoc: TNoteUpdateRec
                ): boolean;
            constructor Create;
            destructor Destroy;  override;
    end;

    function Footer(Loc : TNoteUpdateRec) : string;
    function Header(Title : String) : ANSIstring;
    procedure SaveNewTemplate(NotebookName: ANSIString);


implementation

uses FileUtil               // Graphics needed for font style defines
    ,LazUTF8
    ,Settings				// User settings and some defines across units.
    ,SearchUnit				// So we have access to NoteBookList
    ,LazFileUtils           // For ExtractFileName...
    ,tb_utils
    {$ifdef WINDOWS},SyncUtils{$endif} ;          // For SafeWindowsDelete

const
  {$ifdef LINUX}
  MonospaceFont = 'Monospace';    // until Oct 2021, this was monospace, caused each line in a mono block to be individually wrapped on Linux
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
    // List, Bold, Italics HiLite Underline Strikeout Monospace Fontsize
    // Processing Order is the reverese -
    // ListOff BoldOff ItalicsOff HiLiteOff UnderOff StrikeOff MonoOff FontSize MonoSpace Strikeout Underline HiLite Ital Bold List

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
    // if we are currently in fixedwidth AND the next block is not. Not because either Pitch is not pfFixed OR Name is not Monospace
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

    // When Font size changes     OR end of line where we cloe off any pending, we rely on
    // notenormal to later move the re-turn ons down to next line.
    if CloseOnly OR ((FSize <> FT.TextStyle.Font.Size) and (FT.TextStyle.Font.Size <> Sett.FontTitle)) then begin
        if Bold then Buff := Buff + '</bold>';
        if Italics then Buff := Buff + '</italic>';
        if HiLight then Buff := Buff + '</highlight>';
        if Underline then Buff := Buff + '</underline>';
        if Strikeout then Buff := Buff + '</strikeout>';
        if FixedWidth then Buff := Buff + '</monospace>';

        // we need to do this if FSize is Small, Large or Huge OR Font has changed
        if (FSize in [Sett.FontSmall, Sett.FontLarge, Sett.FontHuge])                    // ToDo : Oct2021 Seriously untested code !!!!
        or ((FSize <> FT.TextStyle.Font.Size) and (FT.TextStyle.Font.Size <> Sett.FontTitle)) then begin
                Buff := Buff + SetFontXML(FSize, false);
                // better for pretty tags but generates invalid tags ! See below ....
                Buff := Buff + SetFontXML(FT.TextStyle.Font.Size, true);
        end;

        if FixedWidth then Buff := Buff + '<monospace>';
        if Strikeout then Buff := Buff + '<strikeout>';
        if Underline then Buff := Buff + '<underline>';
        if HiLight then Buff := Buff + '<highlight>';
        if Italics then Buff := Buff + '<italic>';
        if Bold then Buff := Buff + '<bold>';
        FSize := FT.TextStyle.Font.Size;
    end;

    if CloseOnly then exit(Buff);        // if CloseOnly, ie end of line, nothing is turning on.

    // This comment no longer applies, leave hear until well tested !!!!!!!
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

procedure TBSaveNote.BulletList(Level : TKMemoParaNumbering; var Buff : ANSIString);
var
   StartStartSt, StartEndSt, EndStartSt, EndEndSt : ANSIString;
   iLevel : integer;
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
        StartEndSt := '<strikeout>' + StartEndSt;
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

    // writeLn('Buff at Start [' + Buff + ']');
    // writeln('StartStart    [' + StartStartSt + ']');
    // writeln('StartEnd      [' + StartEndSt + ']');
    // writeln('EndStart      [' + EndStartSt + ']');
    // writeln('EndEnd        [' + EndEndSt + ']');

    Buff := StartEndSt + Buff + EndStartSt;
    case Level of
        BulletOne   : iLevel := 1;
        BulletTwo   : iLevel := 2;
        BulletThree : iLevel := 3;
        BulletFour  : iLevel := 4;
        BulletFive  : iLevel := 5;
        BulletSix   : iLevel := 6;
        BulletSeven : iLevel := 7;
        BulletEight : iLevel := 8;
        otherwise iLevel := 8;
    end;
    while iLevel > 0 do begin
        Buff := '<list><list-item dir="ltr">' + Buff + '</list-item></list>';
        dec(iLevel);
    end;
    Buff := StartStartSt + Buff + EndEndSt;

    {Buff := StartStartSt + '<list><list-item dir="ltr">' + StartEndSt
    		+ Buff + EndStartSt + '</list-item></list>' + EndEndSt;}

    // writeLn('Buff at End [' + Buff + ']');         // **************************************
    // writeln('---');
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

procedure SaveNewTemplate(NotebookName : ANSIString);
var
   GUID : TGUID;
   OStream:TFilestream;
   Buff{, ID} : ANSIString;
   Loc :  TNoteUpdateRec {TNoteLocation};
begin
   CreateGUID(GUID);
   //Title := NotebookName  + ' Template';
   Loc.FFName := copy(GUIDToString(GUID), 2, 36) + '.note';
   SearchForm.NoteLister.AddNoteBook(Loc.FFname, NotebookName, True);
   Ostream :=TFilestream.Create(Sett.NoteDirectory + Loc.FFName, fmCreate);
   Loc.Y := '20'; Loc.X := '20'; Loc.Height := '200'; Loc.Width:='300';
   Loc.OOS := 'False'; Loc.CPos:='1';
   try
   		Buff := Header(RemoveBadXMLCharacters(NotebookName)  + ' Template');
        OStream.Write(Buff[1], length(Buff));
        Buff := RemoveBadXMLCharacters(NotebookName)  + ' Template' + #10#10#10;
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

    // NEW : if passed a created StringList, we write to the list rather than to the
    // Memory Buffer. Still need to deal with Header and Footer in a line by line mode.
procedure TBSaveNote.ReadKMemo(FileName : ANSIString; Title : string; KM1 : TKMemo; STL : TStringList = nil);
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
    if STL = nil then
        outstream :=TMemoryStream.Create({FileName, fmCreate});
        // Write and WriteBuffer accept a buffer, not a string !  Need to start at pos 1
        // when sending string or ANSIstring otherwise it uses first byte which makes it look like a binary file.
        // http://free-pascal-general.1045716.n5.nabble.com/Creating-text-files-with-TFileStream-td2824859.html
    Buff := Header(Title);
    if STL = Nil then
        OutStream.Write(Buff[1], length(Buff))
    else STL.Add(Buff);
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

                if  TKMemoParagraph(KM1.Blocks.Items[BlockNo]).Numbering <> pnuNone then
                     BulletList(TKMemoParagraph(KM1.Blocks.Items[BlockNo]).Numbering, Buff);

                {if  TKMemoParagraph(KM1.Blocks.Items[BlockNo]).Numbering = pnuBullets then
                     BulletList(Buff);    }

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
                if STL = Nil then begin
                    Buff := Buff + LineEnding;
                    OutStream.Write(Buff[1], length(Buff))
                end else STL.Add(Buff);
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
            if Buff <> '' then begin
                if STL = Nil then
                    OutStream.Write(Buff[1], length(Buff))
                else STL.Add(Buff);
			end;
			Buff := '';
            if Bold then Buff := '</bold>';
            if Italics then Buff := Buff + '</italic>';
            if HiLight then Buff := Buff + '</highlight>';
            if Underline then Buff := Buff + '</underline>';
            if Strikeout then Buff := Buff + '</strikeout>';
            if FixedWidth then Buff := Buff + '</monospace>';
            if FSize <> Sett.FontNormal then
                 Buff := Buff + SetFontXML(FSize, False);
            if length(Buff) > 0 then begin
                  if STL = Nil then
                    OutStream.Write(Buff[1], length(Buff))
                  else STL.Add(Buff);
            end;
         Except
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
   TmpName : string = '';
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

    //{$define STAGEDWRITE}
    //{$ifdef STAGEDWRITE}
    {$ifdef WINDOWS}        // temp kludge until I understand the problem RenameFileUTF has with smb shares.
                            // this will now work OK on all linux file systems but without the write, delete.move process.
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
        //showmessage('T=' + TmpName + ' and F=' + FileName);
        result := RenameFileUTF8(TmpName, FileName);    // Unix ok to over write, windows is not !
        // ToDo : Note that the above line seems to fail on a smb shared directory.  Must get to bottom ....
    {$else}        // thats the ifdef StagedWrite, here we write directly to note file.
        try
            OutStream.SavetoFile(FileName);
        finally
            OutStream.Free;
            OutStream := nil;
        end;
    {$endif}                                        // thats the ifdef StagedWrite
    if not Result then NoteLoc.ErrorStr:='Failed Rename T=' + TmpName + ' and F=' + FileName;
end;

function Header(Title : string): ANSIstring;
var
   S1, S2, S3, S4 : ANSIString;
begin  // Add a BOM at the start, not essencial, Tomboy did it, makes the note no longer a plain text file. ??
  //S1 := #239#187#191'<?xml version="1.0" encoding="utf-8"?>'#10'<note version="0.3" xmlns:link="';
  S1 := '<?xml version="1.0" encoding="utf-8"?>'#10'<note version="0.3" xmlns:link="';
  S2 := 'http://beatniksoftware.com/tomboy/link" xmlns:size="http://beatniksoftware.com/tomboy/size"';
  S3 := ' xmlns="http://beatniksoftware.com/tomboy">'#10'  <title>';
  S4 := '</title>'#10'  <text xml:space="preserve"><note-content version="0.1">';
  Result := S1 + S2 + S3 + RemoveBadXMLCharacters(Title) + S4;
end;

{   Sets TimeStamp, a public SaveNote var that is later used by EditBox to set the value
    stored in NoteLister. And NoteLister is not thread safe.
    This method also reads NoteLister for NoteBookTags. Maybe or maybe NOT safe.
}

function Footer(Loc : TNoteUpdateRec): ANSIstring;
var
   S1, S2, S3, S4, S5, S6 : string;
begin
  if Loc.LastChangeDate = '' then begin
       debugln('------------------------------------------------------------------------');
       debugln('ERROR, ERROR passed an blank change date to Footer, we dont do that here');
       debugln('------------------------------------------------------------------------');
       Loc.LastChangeDate := TB_GetLocalTime();     // no, thats just a temp fix, do something about it
  end;
  (*
  if Loc.LastChangeDate = '' then
    TimeStamp := TB_GetLocalTime()   // get actual time date in format like Tomboy's
  else TimeStamp := Loc.LastChangeDate;   *)
  S1 := '</note-content></text>'#10'  <last-change-date>';
  S2 := '</last-change-date>'#10'  <last-metadata-change-date>';
  S3 := '</last-metadata-change-date>'#10'  <create-date>';
  S4 := '</create-date>'#10'  <cursor-position>' + Loc.CPos + '</cursor-position>'#10'  <selection-bound-position>1</selection-bound-position>'#10;
  S5 := '  <width>' + Loc.Width + '</width>'#10'  <height>' + Loc.Height + '</height>'#10'  <x>'
        + Loc.X + '</x>'#10'  <y>' + Loc.Y + '</y>'#10;
  S6 := '  <open-on-startup>' + Loc.OOS + '</open-on-startup>'#10'</note>';
  if Loc.CreateDate = '' then Loc.CreateDate := Loc.LastChangeDate;
  if SearchForm.NoteLister <> Nil then
        Result := S1 + Loc.LastChangeDate + S2 + Loc.LastChangeDate + S3 + Loc.CreateDate + S4 + S5
            + SearchForm.NoteLister.NoteBookTags(ExtractFileName(Loc.FFName)) + S6
  else
        Result := S1 + Loc.LastChangeDate + S2 + Loc.LastChangeDate + S3 + Loc.CreateDate + S4 + S5 + S6;
  // That will mean no Notebook tags in single note mode, is that an issue ?
  // Most singe notes are out of their repo so won't have notebooks anyway but we could
  // save any tag list and restore it on save ??
end;

end.
