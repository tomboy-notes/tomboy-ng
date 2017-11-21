unit EditBox;

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

{	This form represents one note being read or edited. It keeps its note in
	a KMemo component, its there using the native (ie GTK or Windows)
	display system.

	This form uses two other units, SaveNote and LoadNote that know about the
	Tomboy's XML format. As an important design target, we must be fully
	compatible with Tomboys format. And why not ?
}

{	HISTORY
	2017/9/26 - Added Locks around call to load a file into KMemo. Speed up from
	14 sec to 82mS when loading greybox note.

	20170928 - Made CheckforLinks maintain one copy of the Text to search instead
	of a new copy for each search term when MakeAllLinks() was doing it. CheckforLinks()
	now takes up 200 mS instead of 600mS for 20K Greybox note file.

	20170928 A whole lot of changes around OnChange event to speed up the response
	time around links.

	20171003 - Started Testing under Windows.
	Added a bit of code to MakeAllLink() to allow for the fact that windows
	has two characters as a line ending, CR/LF, #13#10. KMemo SelectionIndex code
	allows one char for a Paragraph. Linux/OSX being Unix are fine, one char newline.
	A bit of a tidy up.

	20171005 - Fixed a bug where a new note did not get its file path from Settings.

	20171005 - Fixed a bug where new note is not getting its title set properly,
	we now read the title of the note when saving, also pop it into the Caption
	at the same time.

	20171005 - Complete rewrite of the AlterFont() procedure, this one works
	all the time !

	20171007 - Bullets, good on Windows, less than so on Unix

	20171008 - Added right click popup menu and Cut, Copy, Paste, Select All
	but cannot cut o copy to work o Linux. Will try other platforms.
	Hmm, flaky on all three platforms. But seens to work OK in small test !!!
	20171008 - Added a line in FormShow to set the default font, KMemo1.Font.Size:=
	Lets see if that works.....

	20171010 - on upgrade to the e4ec.. late september ver of kC, Bullet issue gone ! This
	code here now has simplified and better working bullets

	2017/10/15 - now call MainUnit.IndexNotes after saving. This behaviour needs to
	be moved to a seperate thread.....

	2017/11/03 restructure CheckForLinks() and MakeAllLinks() because of  UTF8 issues.
	Abandon completely the model of copy note text to a PChar and scanning
  	that, will now rely on UTF8Pos() searching Kmemo1.Blocks.Text. Big change, watch ....

	2017/11/05 Converted GetAFileName() to use GUID, sorry about that ! I think
    the old method produced usable names but was not in Tomboy style. Should be OK....

	2017/11/07 Fixes to CheckForLinks() and friends so it can again, handle the same
	link mentioned several times in a note. And remember, UTF8Pos() does not like being
	told to start at zero. Oh, yes, I remember, now!
}


{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, { FileUtil,} Forms, Controls, Graphics, Dialogs, ExtCtrls,
    Menus, StdCtrls, Buttons, kmemo;

type

    { TEditBoxForm }

    TEditBoxForm = class(TForm)
        ButtLink: TBitBtn;
        ButtText: TBitBtn;
        ButtTools: TBitBtn;
        ButtDelete: TBitBtn;
        ButtNotebook: TBitBtn;
        ButtSearch: TBitBtn;
		FindDialog1: TFindDialog;
        KMemo1: TKMemo;
		Label1: TLabel;
        MenuBold: TMenuItem;
        MenuItalic: TMenuItem;
        MenuHighLight: TMenuItem;
        MenuHuge: TMenuItem;
        MenuBullet: TMenuItem;
		MenuItem1: TMenuItem;
		MenuItemSelectAll: TMenuItem;
		MenuItem5: TMenuItem;
		MenuItemDelete: TMenuItem;
		MenuItemPaste: TMenuItem;
		MenuItemCopy: TMenuItem;
		MenuItemFind: TMenuItem;
        MenuItem3: TMenuItem;
		MenuItemCut: TMenuItem;
        MenuItemSync: TMenuItem;
        MenuItemExportHTML: TMenuItem;
        MenuSmall: TMenuItem;
        MenuItem2: TMenuItem;
        MenuNormal: TMenuItem;
        MenuLarge: TMenuItem;
        MenuFixedWidth: TMenuItem;
		PopupMenuRightClick: TPopupMenu;
        PopupMenuTools: TPopupMenu;
        PopupMenuText: TPopupMenu;
		TaskDialogDelete: TTaskDialog;
		Timer1: TTimer;
		procedure ButtDeleteClick(Sender: TObject);
        procedure ButtSearchClick(Sender: TObject);
        procedure ButtTextClick(Sender: TObject);
        procedure ButtToolsClick(Sender: TObject);
		procedure FindDialog1Find(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure FormDestroy(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure KMemo1Change(Sender: TObject);
        	{ Watchs for  backspace affecting a bullet point, thats all }
		procedure KMemo1KeyDown(Sender: TObject; var Key: Word;
				Shift: TShiftState);
		procedure KMemo1MouseDown(Sender: TObject; Button: TMouseButton;
				Shift: TShiftState; X, Y: Integer);
        procedure MenuBoldClick(Sender: TObject);
        procedure MenuBulletClick(Sender: TObject);
        procedure MenuHighLightClick(Sender: TObject);
        procedure MenuHugeClick(Sender: TObject);
        procedure MenuItalicClick(Sender: TObject);
		procedure MenuItemCopyClick(Sender: TObject);
		procedure MenuItemCutClick(Sender: TObject);
		procedure MenuItemFindClick(Sender: TObject);
		procedure MenuItemPasteClick(Sender: TObject);
		procedure MenuItemSelectAllClick(Sender: TObject);
		procedure MenuItemSyncClick(Sender: TObject);
        procedure MenuItemWriteClick(Sender: TObject);
        procedure MenuLargeClick(Sender: TObject);
        procedure MenuNormalClick(Sender: TObject);
        procedure MenuSmallClick(Sender: TObject);
		procedure Timer1Timer(Sender: TObject);

    private
        CreateDate : string;		// Will be '' if new note

        Ready : boolean;
        LastFind : longint;			// Used in Find functions.
        // FontName : string;			// Set in OnShow, const after that  ???
        // FontNormal : integer; 		// as above
        { To save us checking the title if user is well beyond it }
        BlocksInTitle : integer;
        { Alters the Font of Block as indicated }
        procedure AlterBlockFont(const BlockNo: longint; const Command : integer;
            const NewFontSize: integer=0);
        { Alters the font etc of selected area as indicated }
        procedure AlterFont(const Command : integer; const NewFontSize: integer = 0);
		procedure ClearLinks(const StartScan : longint =0; EndScan : longint = 0);
        { Clears links near where user is working }
        procedure ClearNearLink(const CurrentPos: longint);
        { Returns a long random file name, Not checked for clashes }
        function GetAFilename() : ANSIString;
        procedure CheckForLinks(const StartScan : longint = 1; EndScan : longint = 0);
        { Returns the title, that is the first line of note }
        function GetTitle(out TheTitle: ANSIString): boolean;
        procedure ImportNote(FileName : string);
        { Searches for all occurances of Term in the KMemo text, makes them Links }
		procedure MakeAllLinks(const Term: ANSIString; const StartScan : longint =1; EndScan : longint = 0);
        { Makes the passed location a link if its not already one }
		procedure MakeLink(const Link: ANSIString; const Index, Len: longint);
        { Makes the top line look like a title. }
        procedure MarkTitle();
        { Responds when user clicks on a hyperlink }
		procedure OnUserClickLink(sender: TObject);
		procedure SaveTheNote;
    public
        NoteFileName, NoteTitle : string;
        Dirty : boolean;
    end;

var
    EditBoxForm: TEditBoxForm;

// Note that the various font sizes are declared in Settings;


implementation

{$R *.lfm}

{ TEditBoxForm }
uses //RichMemoUtils,     // Provides the InsertFontText() procedure.
    LazUTF8,
    LCLType,			// For the MessageBox
    keditcommon,        // Holds some editing defines
    settings,			// User settings and some defines used across units.
    MainUnit,              // Is the main starting unit and the search tool.
    SaveNote,      		// Knows how to save a Note to disk in Tomboy's XML
	LoadNote,           // Will know how to load a Tomboy formatted note.
    SyncGUI;


{  ---- U S E R   C L I C K   F U N C T I O N S ----- }


procedure TEditBoxForm.ButtTextClick(Sender: TObject);
begin
    PopupMenuText.PopUp;
end;

procedure TEditBoxForm.ButtToolsClick(Sender: TObject);
begin
    PopupMenuTools.PopUp;
end;

procedure TEditBoxForm.ButtSearchClick(Sender: TObject);
begin
	RTSearch.Show;
end;

procedure TEditBoxForm.ButtDeleteClick(Sender: TObject);
var
    St : string;
begin
    St := Caption;
   if IDYES = Application.MessageBox('Delete this Note', PChar(St),
   									MB_ICONQUESTION + MB_YESNO) then begin
		Timer1.Enabled := False;
   		if NoteFileName <> '' then
	   		    RTSearch.DeleteNote(NoteFileName);
        Dirty := False;
		Close;
   end;
end;

procedure TEditBoxForm.MenuBulletClick(Sender: TObject);
var
      BlockNo : longint = 1;
      LastBlock,  Blar : longint;
begin
      // if not KMemo1.SelAvail then exit();
      // Need a better test of valid  selection than that !

      BlockNo := Kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, Blar);
      LastBlock := Kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelEnd, Blar);

      if (BlockNo = LastBlock) and (BlockNo > 1) and
      		KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin
            	dec(LastBlock);
                dec(BlockNo);
			end;

      // OK, we are now in a TextBlock, possibly both start and end there. Must mark
      // next para as numb and then all subsquent ones until we do the one after end.
      repeat
    	inc(BlockNo);
        if BlockNo >= Kmemo1.Blocks.count then	// no para after block (yet)
            Kmemo1.Blocks.AddParagraph();
    	if KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then
    		TKMemoParagraph(KMemo1.Blocks.Items[BlockNo]).Numbering := pnuBullets;
      until (BlockNo > LastBlock) and KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph');

      { There is an issue on Linux and Mac that causes a crash when you turn Bullets off
        programaticly (ie TKMemoParagraph(KMemo1.Blocks.Items[6]).Numbering := pnuNone;)
        so we avoid that here, if user wants bullets off, they can backspace over the bullet
        marker itself.
      }
end;


procedure TEditBoxForm.KMemo1MouseDown(Sender: TObject; Button: TMouseButton;
		Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbRight then PopupMenuRightClick.PopUp;
end;


{ -------------- U S E R   F O N T    C H A N G E S ----------------}

const
 ChangeSize   = 1;     // Used by AlterFont(..) and its friends.
 ChangeBold   = 2;
 ChangeItalic = 3;
 ChangeColor  = 4;

{ This complex function will set font size, Bold or Italic or Color depending on the
  constant passed as first parameter. NewFontSize is ignored (and can be ommitted)
  if Command is ChangeBold or ChangeItalic, then toggle. If the function finds
  that the first char of selection already has that attribute it negates it,
  ie size becomes normal or no bold, no italics.

  It has to deal with several possible combinations and does so in three parts -
  1. Dealing with what happens around the SelStart. Possibly splitting once or twice
  2. Dealing with any complete blocks between start and end.
  3. Dealing with the stuff around the end. If its not already been done by 1.

  The actual Commands are defined above and are not used outside this unit.

  Consider possible ways this function can be called -
  a. With selstart at first char in a block, Selend at end of same block.
  b. Selstart at other than first char, selend at end of same block.
  c. Selstart after first char and selend before last char of same block.
  d, e, f. as above but spanning blocks.
  a. & d. Require no splitting.  Just apply change to block or blocks.
  b. & e. Needs one split. Split at SelStart and Apply to new and subsquent if any.
  c. & f. Needs two splits. Split at SelStar and SelEnd-1, then as above.

  So, decide what blocks we apply to, then apply. Sounds easy.
}

procedure TEditBoxForm.AlterFont(const Command : integer; const NewFontSize : integer = 0);
var
	FirstBlockNo, LastBlockNo, IntIndex, LastChar, FirstChar : longint;
	SplitStart : boolean = false;
begin
    Ready := False;
	LastChar := Kmemo1.RealSelEnd;			// SelEnd points to first non-selected char
    FirstChar := KMemo1.RealSelStart;
	FirstBlockNo := Kmemo1.Blocks.IndexToBlockIndex(FirstChar, IntIndex);
    if IntIndex <> 0 then			// Not Starting on block boundary.
		SplitStart := True;
    LastBlockNo := Kmemo1.Blocks.IndexToBlockIndex(LastChar, IntIndex);
    if IntIndex <> (length(Kmemo1.Blocks.Items[LastBlockNo].Text) -1) then 	// Not Last char in block
        LastBlockNo := KMemo1.SplitAt(LastChar) -1;       // we want whats before the split.
    while LastBlockNo > FirstBlockNo do begin
        AlterBlockFont(LastBlockNo, Command, NewFontSize);
        dec(LastBlockNo);
    end;
    // Now, only First Block to deal with
    if SplitStart then
		FirstBlockNo := KMemo1.SplitAt(FirstChar);
    AlterBlockFont(FirstBlockNo, Command, NewFontSize);
    KMemo1.SelEnd := LastChar + 1;	// Any splitting above seems to subtly alter SelEnd, reset.
	Ready := True;
end;


	{  Takes a Block number and applies changes to that block }
procedure TEditBoxForm.AlterBlockFont(const BlockNo : longint; const Command : integer; const NewFontSize : integer = 0);
var
	Block : TKMemoTextBlock;
begin
	Block := TKMemoTextBlock(KMemo1.Blocks.Items[BlockNo]);
    if (Command = ChangeSize) and (NewFontSize = Sett.FontNormal) then begin  // Don't toggle, just set to FontNormal
         Block.TextStyle.Font.Size := Sett.FontNormal;
         exit();
    end;
    case Command of
		ChangeSize :	if Block.TextStyle.Font.Size = NewFontSize then begin
						Block.TextStyle.Font.Size := Sett.FontNormal;
					end else begin
 						Block.TextStyle.Font.Size := NewFontSize;
					end;
		ChangeBold :   	if fsBold in Block.TextStyle.Font.style then begin
						Block.TextStyle.Font.Style := Block.TextStyle.Font.Style - [fsBold];
					end else begin
						Block.TextStyle.Font.Style := Block.TextStyle.Font.Style + [fsBold];
					end;
		ChangeItalic :
					if fsItalic in Block.TextStyle.Font.style then begin
						Block.TextStyle.Font.Style := Block.TextStyle.Font.Style - [fsItalic];
					end else begin
						Block.TextStyle.Font.Style := Block.TextStyle.Font.Style + [fsItalic];
					end;
		ChangeColor : 	if Block.TextStyle.Font.Color = NormalColor then begin
                        Block.TextStyle.Font.Color := HiColor;
                    end else begin
                        Block.TextStyle.Font.Color := NormalColor;
                    end;
	end;
end;

procedure TEditBoxForm.MenuHighLightClick(Sender: TObject);
begin
    AlterFont(ChangeColor);
    { TODO 1 : OK, real highlight is quite hard, lets just color text for now.
    }
end;

procedure TEditBoxForm.MenuLargeClick(Sender: TObject);
begin
   AlterFont(ChangeSize, Sett.FontLarge);
end;

procedure TEditBoxForm.MenuNormalClick(Sender: TObject);
begin
   AlterFont(ChangeSize, Sett.FontNormal);	// Note, this won't toggle !
end;

procedure TEditBoxForm.MenuSmallClick(Sender: TObject);
begin
    AlterFont(ChangeSize, Sett.FontSmall);
end;



procedure TEditBoxForm.MenuHugeClick(Sender: TObject);
begin
   AlterFont(ChangeSize, Sett.FontHuge);
end;

procedure TEditBoxForm.MenuBoldClick(Sender: TObject);
begin
	AlterFont(ChangeBold);
end;

procedure TEditBoxForm.MenuItalicClick(Sender: TObject);
begin
	AlterFont(ChangeItalic);
end;


{ ------- S T A N D A R D    E D I T I N G    F U N C T I O N S ----- }

procedure TEditBoxForm.FindDialog1Find(Sender: TObject);
var
	FindStart : longint;
    Ptr, EndP : PChar;			// Will generate "not used" warnings in Unix
    NumbCR : longint = 0;
begin
   	FindStart := UTF8Pos(TFindDialog(Sender).FindText, KMemo1.Blocks.text, LastFind);
   	if FindStart > 0 then begin
		{ TODO : Must allow for Windows extra CR in newline }
        {$ifdef WINDOWS}                // does no harm in Unix but a bit slow ?
        Ptr := PChar(KMemo1.Blocks.text);
        EndP := Ptr + FindStart-1;
        while Ptr < EndP do begin
            if Ptr^ = #13 then inc(NumbCR);
            inc(Ptr);
		end;
        {$endif}
        LastFind := FindStart + 1;
        KMemo1.SelStart := FindStart -1 -NumbCR;
        KMemo1.SelLength := length(TFindDialog(Sender).FindText);
   	end;
end;

procedure TEditBoxForm.MenuItemFindClick(Sender: TObject);
begin
    LastFind := 1;
//    FindDialog1.Options:=[frHideMatchCase, frHideWholeWord];
    FindDialog1.Options:=[frHideMatchCase];
	FindDialog1.Execute;
end;

procedure TEditBoxForm.MenuItemCopyClick(Sender: TObject);
begin
	KMemo1.ExecuteCommand(ecCopy);
end;

procedure TEditBoxForm.MenuItemCutClick(Sender: TObject);
begin
   KMemo1.ExecuteCommand(ecCut);
end;

procedure TEditBoxForm.MenuItemPasteClick(Sender: TObject);
begin
    Ready := False;
    KMemo1.ExecuteCommand(ecPaste);
    Ready := True;
end;

procedure TEditBoxForm.MenuItemSelectAllClick(Sender: TObject);
begin
	KMemo1.ExecuteCommand(ecSelectAll);
end;

procedure TEditBoxForm.MenuItemSyncClick(Sender: TObject);
begin
	if Dirty then SaveTheNote();
   FormSync.NoteDirectory := Sett.NoteDirectory;
   FormSync.LocalConfig := Sett.LocalConfig;
   FormSync.RemoteRepo := Sett.RemoteRepo;
   FormSync.SetupFileSync := False;
   FormSync.ShowModal;					// we don't care about result ...
end;

{ - - - H O U S E   K E E P I N G   F U C T I O N S ----- }

procedure TEditBoxForm.Timer1Timer(Sender: TObject);
begin
    Timer1.Enabled:=False;
	// showmessage('Time is up');
    SaveTheNote();
    Label1.Caption := 'c';
end;

procedure TEditBoxForm.FormShow(Sender: TObject);
begin
    Timer1.Enabled := False;
    KMemo1.Font.Size := Sett.FontNormal;
    Kmemo1.Clear;
    if Sett.RemoteRepo <> '' then
        MenuItemSync.Enabled := True
    else MenuItemSync.Enabled := False;
    if length(NoteTitle) > 0 then begin
       Caption := NoteTitle;
       ImportNote(NoteFileName);
    end else begin
        CreateDate := '';
        KMemo1.Clear;
        Caption := 'untitled note';
        Ready := true;
    end;
    Dirty := False;
end;

	{ This gets called when the TrayMemu quit entry is clicked }
    { No it does not, only when user manually closes this form. }
procedure TEditBoxForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    Release;
end;


{ This gets called when the main app goes down, presumably also in a controlled
  powerdown ?   Seems a good place to save if we are dirty.... }
procedure TEditBoxForm.FormDestroy(Sender: TObject);
begin
    if Dirty then begin
        // writeln('Going to save.');
        SaveTheNote();
        // writeln('Saved');
	end;
end;

	{ Sets TheTitle to the first line of the KMemo, returning true is it
      found something.
    }
function TEditBoxForm.GetTitle(out TheTitle : ANSIString) : boolean;
var
    BlockNo : longint = 0;
begin
    Result := False;
    TheTitle := '';
	while Kmemo1.Blocks.Items[BlockNo].ClassName = 'TKMemoTextBlock' do begin
        TheTitle := TheTitle + Kmemo1.Blocks.Items[BlockNo].Text;
       	inc(BlockNo);
        if BlockNo >= Kmemo1.Blocks.Count then break;
    end;                            // Stopped at first TKMemoParagraph if it exists.
    if TheTitle <> '' then Result := True;
end;


    { Makes sure the first (and only the first) line is marked as Title
      Title should be Blue, Underlined and FontTitle big.
      Note that when a new note is loaded from disk, this function is not called,
      the Load unit knows how to do it itself. Saves 200ms with a big (20K) note. }

procedure TEditBoxForm.MarkTitle();
var
	FT : TFont;
    BlockNo : integer = 0;
    AtTheEnd : Boolean = False;
begin
  	if Not Ready then exit();
    { if there is more than one block, and the first, [0], is a para, delete it.}
    if KMemo1.Blocks.Count <= 2 then exit();	// Don't try to mark title until more blocks.

    Ready := false;
    Kmemo1.Blocks.LockUpdate;

    if Kmemo1.Blocks.Items[BlockNo].ClassName = 'TKMemoParagraph' then
          Kmemo1.Blocks.DeleteEOL(0);

    FT := TFont.Create();
    FT.Size := Sett.FontTitle;
    FT.Style := [fsUnderline];
    FT.Color := clBlue;

	try
    	while Kmemo1.Blocks.Items[BlockNo].ClassName = 'TKMemoTextBlock' do begin
           	TKMemoTextBlock(Kmemo1.Blocks.Items[BlockNo]).TextStyle.Font := FT;
           	inc(BlockNo);
            if BlockNo >= Kmemo1.Blocks.Count then begin
                AtTheEnd := True;
                break;
            end;
       	end;                                // Stopped at first TKMemoParagraph if it exists.
        BlocksInTitle := BlockNo;
        FT.Size := Sett.FontNormal;
        FT.Style := [];
        FT.Color := clBlack;

        if not AtTheEnd then begin
        	inc(BlockNo);
            	// Make sure user has not smeared Title charactistics to next line
        	while fsUnderline in TKMemoTextBlock(Kmemo1.Blocks.Items[BlockNo]).TextStyle.Font.Style do begin
        		TKMemoTextBlock(Kmemo1.Blocks.Items[BlockNo]).TextStyle.Font := FT;
            	inc(BlockNo);
            	if BlockNo >= KMemo1.Blocks.Count then break;
        	end;
        end;
	finally
		KMemo1.Blocks.UnLockUpdate;			// Clean up, needs to be in try.. finally loop.
    	FT.Free;
    	Ready := True;
	end;
end;


{ -----------  L I N K    R E L A T E D    F U N C T I O N S  ---------- }

	{ Makes a link at passed position as long as it does not span beyond a block.
      And if it does span beyond one block, I let that go through to the keeper.
      Making a Hyperlink, deleting the origional text is a very slow process so we
      make heroic efforts to avoid having to do so.
    }
procedure TEditBoxForm.MakeLink(const Link : ANSIString; const Index, Len : longint);
var
	Hyperlink: TKMemoHyperlink;
	Cnt : integer = 0;
	BlockNo, Blar : longint;
	DontSplit : Boolean = false;
begin
	// Is it already a Hyperlink ?
    BlockNo := KMemo1.Blocks.IndexToBlockIndex(Index, Blar);
    if KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKHyperlink') then exit();
	// Is it all in the same block ?
    if BlockNo <> Kmemo1.Blocks.IndexToBlockIndex(Index + Len -1, Blar) then exit();
    if length(Kmemo1.Blocks.Items[BlockNo].Text) = length(Link) then DontSplit := True;
    KMemo1.Select(Index, 0);
    while Cnt < Len do begin                 // The ~.DeleteChar() function takes an Index but if
  		KMemo1.Blocks.DeleteChar(Index);    // there is a Selected Area, it deletes that instead. Nasty !
  		inc(Cnt);
	end;
	if not DontSplit then
		BlockNo := KMemo1.SplitAt(Index);
	Hyperlink := TKMemoHyperlink.Create;
	Hyperlink.Text := Link;
	Hyperlink.OnClick := @OnUserClickLink;
	KMemo1.Blocks.AddHyperlink(Hyperlink, BlockNo);
    KMemo1.Select(Index, Len);
end;

{ Searches for all occurances of Term in the KMemo text. Does
  not bother with single char terms. Some potential to speed up
  things here I am sure.
}
procedure TEditBoxForm.MakeAllLinks(const Term : ANSIString; const StartScan : longint =1; EndScan : longint = 0);
var
	Offset, NumbCR   : longint;
    Ptr, EndP : PChar;                  // Will generate "not used" warning in Unix
begin
    // CRCount := 0;
    Offset := UTF8Pos(Term, KMemo1.Blocks.text, StartScan);
    while Offset > 0 do begin
    	NumbCR := 0;
        {$ifdef WINDOWS}                // does no harm in Unix but a bit slow ?
        Ptr := PChar(KMemo1.Blocks.text);
        EndP := Ptr + Offset-1;
        while Ptr < EndP do begin
            if Ptr^ = #13 then inc(NumbCR);
            inc(Ptr);
		end;
        {$endif}
		MakeLink(Term, Offset -1 -NumbCR, UTF8length(Term));
        Offset := UTF8Pos(Term, KMemo1.Blocks.text, Offset+1);
        if EndScan > 0 then
        	if Offset> EndScan then break;
    end;
end;


{ OK, update early Nov, restructured CheckForLinks() and MakeAllLinks() because of
  UTF8 issues.
}

procedure TEditBoxForm.CheckForLinks(const StartScan : longint =1; EndScan : longint = 0);
var
	Ptr : PChar;
    Searchterm : ANSIstring;
    // BlockNo : longint;
    // TS1, TS2, TS3, TS4 : TTimeStamp;           // Temp time stamping to test speed
begin
    // TS1:=DateTimeToTimeStamp(Now);
	if not Ready then exit();
    if StartScan >= length(KMemo1.Blocks.text) then exit();   // prevent crash when memo almost empty
    if EndScan > length(KMemo1.Blocks.text) then EndScan := length(KMemo1.Blocks.text);

    // TS2:=DateTimeToTimeStamp(Now);
    Ready := False;
	RtSearch.StartSearch();
    KMemo1.Blocks.LockUpdate;
    while RTSearch.NextNoteTitle(SearchTerm) do
        if SearchTerm <> NoteTitle then
        	MakeAllLinks(SearchTerm, StartScan, EndScan);
    KMemo1.Blocks.UnLockUpdate;
    Ready := True;
end;

procedure TEditBoxForm.ClearNearLink(const CurrentPos : longint);
var
    BlockNo,  Blar : longint;
    LinkText  : ANSIString;
begin
	{ if are in or next to a link block, remove link }
    BlockNo := KMemo1.Blocks.IndexToBlockIndex(CurrentPos, Blar);
    Ready := False;
    LinkText := Kmemo1.Blocks.Items[BlockNo].Text;              // debug
    if KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoHyperlink') then begin
        LinkText := Kmemo1.Blocks.Items[BlockNo].Text;
    	if Not RTSearch.IsThisaTitle(LinkText) then begin
        	KMemo1.Blocks.LockUpdate;                         // I don't think we should lock here.
    		Kmemo1.Blocks.Delete(BlockNo);
    		KMemo1.Blocks.AddTextBlock(Linktext, BlockNo);
        	KMemo1.Blocks.UnlockUpdate;
        end;
    end;
    BlockNo := KMemo1.Blocks.IndexToBlockIndex(CurrentPos-1, Blar);

    if KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoHyperlink') then begin
        LinkText := Kmemo1.Blocks.Items[BlockNo].Text;
        if Not RTSearch.IsThisaTitle(LinkText) then begin
        	KMemo1.Blocks.LockUpdate;
    		Kmemo1.Blocks.Delete(BlockNo);
    		KMemo1.Blocks.AddTextBlock(Linktext, BlockNo);
        	KMemo1.Blocks.UnlockUpdate;
        end;
    end;
    Ready := True;
end;


	{ Scans across whole note removing any links it finds. Block containing link
      must be removed and new non-link block created in its place.
      Note that the scaning is very quick, gets bogged down doing the remove/add

      This function is not needed at present but leave it here in case its
      useful after user chooses to not display links. }
procedure TEditBoxForm.ClearLinks(const StartScan : longint =0; EndScan : longint = 0);
var
      BlockNo, EndBlock, Blar : longint;
      LinkText : ANSIString;
begin
    Ready := False;
    BlockNo := KMemo1.Blocks.IndexToBlockIndex(StartScan, Blar); // DANGER, we must adjust StartScan to block boundary
    EndBlock := KMemo1.Blocks.IndexToBlockIndex(EndScan, Blar);	 // DANGER, we must adjust EndScan to block boundary
    KMemo1.Blocks.LockUpdate;
    while BlockNo <= EndBlock do begin							// DANGER, must check these block numbers work
        if Kmemo1.Blocks.Items[BlockNo].ClassName = 'TKMemoHyperlink' then begin
            LinkText := Kmemo1.Blocks.Items[BlockNo].Text;
            Kmemo1.Blocks.Delete(BlockNo);
            KMemo1.Blocks.AddTextBlock(Linktext, BlockNo);
		end;
        inc(BlockNo);
	end;
    KMemo1.Blocks.UnLockUpdate;
    Ready := True;
end;

procedure TEditBoxForm.OnUserClickLink(sender : TObject);
begin
	RTSearch.OpenNote(TKMemoHyperlink(Sender).Text);
end;

	{ Any change to the note and this gets called. So, vital it be quick }
procedure TEditBoxForm.KMemo1Change(Sender: TObject);
var
    CurserPos, StartScan, EndScan, BlockNo, Blar : longint;
    TempTitle : ANSIString;
    // TS1, TS2, TS3, TS4 : TTimeStamp;           // Temp time stamping to test speed
begin
    if not Ready then exit();           // don't do any of this while starting up.
    if not Dirty then Timer1.Enabled := true;
    Dirty := true;
    Label1.Caption := 'd';
    // Memo1.Clear;
    // PopUpMenuTools.Items.Items[2].Enabled := true; // That is to allow saving
    CurserPos := KMemo1.RealSelStart;
    StartScan := CurserPos - LinkScanRange;
    if StartScan < length(Caption) then StartScan := length(Caption);
    EndScan := CurserPos + LinkScanRange;
    if EndScan > length(KMemo1.Text) then EndScan := length(KMemo1.Text);
    // TS1:=DateTimeToTimeStamp(Now);

    BlockNo := KMemo1.Blocks.IndexToBlockIndex(CurserPos, Blar);

    if ((BlocksInTitle + 3) > BlockNo) then begin
        // We don't check title if user is not close the it.
    	MarkTitle();
    	GetTitle(TempTitle);
        Caption := TempTitle;
    end;

    // OK, if we are in the first or second (?) block, no chance of a link anyway.
    if BlockNo < 2 then begin
        if KMemo1.Blocks.Count = 0 then 		// But bad things happen if its really empty !
            KMemo1.Blocks.AddParagraph();
    	exit();
    end;
    if Sett.ShowIntLinks then begin
    	ClearNearLink(CurserPos);
    	// TS2:=DateTimeToTimeStamp(Now);
    	CheckForLinks(StartScan, EndScan);
    	// TS3:=DateTimeToTimeStamp(Now);
    end;
    KMemo1.SelStart := CurserPos;
    KMemo1.SelEnd := CurserPos;
    // Memo1.append('Clear ' + inttostr(TS2.Time-TS1.Time) + 'ms  Check ' + inttostr(TS3.Time-TS2.Time));
end;


procedure TEditBoxForm.KMemo1KeyDown(Sender: TObject; var Key: Word;
		Shift: TShiftState);
var
  blar,BlockNo, Spot  : longint;
begin
	if Key = 8 then begin		// We are watching for a BS on a Bullet Marker
		Spot := KMemo1.RealSelStart;
		BlockNo := kmemo1.Blocks.IndexToBlockIndex(Spot, blar);
        Ready := False;
        if kmemo1.blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then
        	if TKMemoParagraph(kmemo1.blocks.Items[BlockNo]).Numbering = pnuBullets then begin
                Key := 0;               // Dont want it passed on, we'll do it from here.
                TKMemoParagraph(kmemo1.blocks.Items[BlockNo]).Numbering := pnuNone;
			end;
		 Ready := True;
         KMemo1.SelStart := Spot;
	 end;
end;



	{ --- I M P O R T I N G   and   E X P O R T I N G    F U N C T I O N S  ---  }

procedure TEditBoxForm.ImportNote(FileName: string);
var
    Loader : TBLoadNote;
 	// TS1, TS2, TS3, TS4 : TTimeStamp;           // Temp time stamping to test speed
begin
    // TS1:=DateTimeToTimeStamp(Now);
    Loader := TBLoadNote.Create();
    Loader.FontNormal:= Sett.FontNormal;
    // Loader.FontName := FontName;
    Loader.FontSize:= Sett.FontNormal;
    KMemo1.LockUpdate;                               // this block, 300mS, 20k Note
    KMemo1.Clear;                                    // consistenly faster on a beat up
    Loader.LoadFile(FileName, KMemo1);         		 // old Mac, slower on a new Windows
    KMemo1.UnlockUpdate;
    // TS2 := DateTimeToTimeStamp(Now);
    Createdate := Loader.CreateDate;
    Ready := true;
    Caption := Loader.Title;
    // TS3 := DateTimeToTimeStamp(Now);
    if Sett.ShowIntLinks then
    	CheckForLinks();                     			// 200mS, 20k note
    // TS4 := DateTimeToTimeStamp(Now);
    Loader.Free;
    KMemo1.Blocks.AddParagraph(); 				// This is a problem, OSX needs this.

     { TODO : The Mac seems to need some kick along after we release the LockUpdate.
    Adding a block works, if the note aleady ends with a newline, then, easy, we remove
    it and then add one. But how to remove ?? My early sandbox tests saw a similar
    problem, Linux also needed that kick. But not in the real app, I assumed something
    else, such as writing links was providing that kick. }

    // Memo1.append('Load ' + inttostr(TS2.Time-TS1.Time) + 'ms, CheckLink ' + inttostr(TS4.Time-TS3.Time));
end;

procedure TEditBoxForm.MenuItemWriteClick(Sender: TObject);
begin
    SaveTheNote();
end;

procedure TEditBoxForm.SaveTheNote();
var
 	Saver : TBSaveNote;
begin
    if Sett.NotesReadOnly then begin
    //    showmessage('Sorry, Settings say we dont save.');
        exit();       { TODO : Remove this when we start doing auto save, right now, be scared ! }
	end;
  	if length(NoteFileName) = 0 then
        NoteFileName := Sett.NoteDirectory + GetAFilename();
    Saver := TBSaveNote.Create();
    if not GetTitle(Saver.Title) then begin
        Saver.Destroy;
        exit();
    end;
    Caption := Saver.Title;
    Saver.CreateDate := CreateDate;
    Saver.Save(NoteFileName, KMemo1);
    RTSearch.UpdateList(Caption, Saver.TimeStamp, NoteFileName);
    Saver.Destroy;
    Dirty := false;
    // RTSearch.IndexNotes();

end;


function TEditBoxForm.GetAFilename() : ANSIString;
var
  GUID : TGUID;
begin
   CreateGUID(GUID);
   Result := copy(GUIDToString(GUID), 2, 36) + '.note';
end;

end.

