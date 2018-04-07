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

	2017/11/29 Issue #4, fixed AlterFont() and AlterBlockFont() so that when doing
	Bold, Italics, Coloured we toggle on the basis of first character, not the
	first character of each block.

	2017/11/30 Issue #12. An new note created by user clicking Link in another note
	is now auto saved. And the selected text from the first note now becomes, immediatly
	a link.

	2017/12/02 Little fix to AlterFont to ensure a selected bit of text remains
	selected after a font change.

	2017/12/28  Added and then removed a ToDo, does not need to get pushed.

	2017/01/08  Extensive changes to the way we handle backspace around Bullets.
				I like what it does now but need to test on Win/Mac ....

	2017/01/08  This Unit now has a public variable, Verbose that will tell tales....
    2017/01/08  Added a test so we don't mess with backspace if there is some selected
                text. Mac users, bless them, don't have a delete key. They use a
                key labeled 'delete' thats really a backspace.
	2017/01/09  Hmm, fixed a bug in new code that let BS code mess around in header.
	2018/01/25  Changes to support Notebooks
	2018/01/27  if playing in a bullet and there is not a trailing nonbullet para marker,
				thats bad, so I now auto add one. Thats case y - however its still not
				perfect, really should add a test to see if we overran text looking
				for an unfound para.
    2018/01/29  Noted a crazy note that ended up with an empty hyperlink in title
                and that messed MarkTitle() so altered its test to now find first
                para marker rather than first non text block
    2018/02/01  Lock KMemo1 before saving. Noted a very occasional crash when first saving a new note.
    2018/02/04  Added some ifdef to suppress needless warnings
    2018/02/09  Export as RTF and TXT, untested on mac + windows
    2018/02/17  Moved housekeeping stuff in a method and now call that method from
                a timer, reset by user activity. Same with Save time too.
                Should speed things up.
    2018/02/18  Minor correction to Ctrl-Shift-F shortcut
    2018/03/03  Changes housekeeping timer from 4sec to 2sec - change in object inspector
    2018/03/17  Lockupdate was applied to KMemo, not KMemo.Blocks, in ImportNote(), 800ms hit !
                Related to Mac's need for for a paragraph 'kick' after loading.
                Changed CheckForLinks() so it keeps a single copy of KMemo.Blocks.Text for its
                complete run, passing it to MakeAllLinks() rather than creating a new
                copy each iteration. Appears to deliver a usefull speedup !  Test !!
                But must, apparently unlock before calling some other functions.
    2018/03/18  Removed the add a para on opening at last !
	2018/04/07	A UTF8 correction in MakeAllLinks() to how we count the #13 in Windows.


}


{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, { FileUtil,} Forms, Controls, Graphics, Dialogs, ExtCtrls,
    Menus, StdCtrls, Buttons, kmemo, LazLogger ;

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
        MenuItemSpell: TMenuItem;
		MenuItemExportRTF: TMenuItem;
		MenuItemExportPlainText: TMenuItem;
		MenuItemPrint: TMenuItem;
		MenuItemSelectAll: TMenuItem;
		MenuItem5: TMenuItem;
		MenuItemDelete: TMenuItem;
		MenuItemPaste: TMenuItem;
		MenuItemCopy: TMenuItem;
		MenuItemFind: TMenuItem;
        MenuItem3: TMenuItem;
		MenuItemCut: TMenuItem;
        MenuItemSync: TMenuItem;
        MenuItemExport: TMenuItem;
        MenuSmall: TMenuItem;
        MenuItem2: TMenuItem;
        MenuNormal: TMenuItem;
        MenuLarge: TMenuItem;
        MenuFixedWidth: TMenuItem;
		PopupMenuRightClick: TPopupMenu;
        PopupMenuTools: TPopupMenu;
        PopupMenuText: TPopupMenu;
		TaskDialogDelete: TTaskDialog;
		TimerSave: TTimer;
        TimerHousekeeping: TTimer;
		procedure ButtDeleteClick(Sender: TObject);
		procedure ButtLinkClick(Sender: TObject);
		procedure ButtNotebookClick(Sender: TObject);
        procedure ButtSearchClick(Sender: TObject);
        procedure ButtTextClick(Sender: TObject);
        procedure ButtToolsClick(Sender: TObject);
		procedure FindDialog1Find(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure FormDestroy(Sender: TObject);
        	{ gets called under a number of conditions, easy one is just a re-show,
              or for a new note or a new note with a title from Link button
              or for an existing note where we get note file name
              or a new note from template where we have a note filename but IsTemplate
              also set, here we discard file name and make a new one. }
        procedure FormShow(Sender: TObject);
        procedure KMemo1Change(Sender: TObject);
        	{ Watchs for  backspace affecting a bullet point, and ctrl x,c,v }
		procedure KMemo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure KMemo1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure MenuBoldClick(Sender: TObject);
        procedure MenuBulletClick(Sender: TObject);
        procedure MenuHighLightClick(Sender: TObject);
        procedure MenuHugeClick(Sender: TObject);
        procedure MenuItalicClick(Sender: TObject);
		procedure MenuItemCopyClick(Sender: TObject);
		procedure MenuItemCutClick(Sender: TObject);
        procedure MenuItemExportPlainTextClick(Sender: TObject);
        procedure MenuItemExportRTFClick(Sender: TObject);
		procedure MenuItemFindClick(Sender: TObject);
		procedure MenuItemPasteClick(Sender: TObject);
		procedure MenuItemSelectAllClick(Sender: TObject);
        procedure MenuItemSpellClick(Sender: TObject);
		procedure MenuItemSyncClick(Sender: TObject);
        procedure MenuItemWriteClick(Sender: TObject);
        procedure MenuLargeClick(Sender: TObject);
        procedure MenuNormalClick(Sender: TObject);
        procedure MenuSmallClick(Sender: TObject);
		procedure TimerSaveTimer(Sender: TObject);
        procedure TimerHousekeepingTimer(Sender: TObject);

    private
        CreateDate : string;		// Will be '' if new note
        // CtrlKeyDown : boolean;
        Ready : boolean;
        LastFind : longint;			// Used in Find functions.
        // FontName : string;			// Set in OnShow, const after that  ???
        // FontNormal : integer; 		// as above
        { To save us checking the title if user is well beyond it }
        BlocksInTitle : integer;
        { Alters the Font of Block as indicated }
        procedure AlterBlockFont(const FirstBlockNo : longint; const BlockNo: longint; const Command : integer;
            const NewFontSize: integer=0);
        { Alters the font etc of selected area as indicated }
        procedure AlterFont(const Command : integer; const NewFontSize: integer = 0);
		procedure ClearLinks(const StartScan : longint =0; EndScan : longint = 0);
        { Clears links near where user is working }
        procedure ClearNearLink(const CurrentPos: longint);
        procedure DoHousekeeping();
        { Returns a long random file name, Not checked for clashes }
        function GetAFilename() : ANSIString;
        procedure CheckForLinks(const StartScan : longint = 1; EndScan : longint = 0);
        { Returns with the title, that is the first line of note, returns False if title is empty }
        function GetTitle(out TheTitle: ANSIString): boolean;
        procedure ImportNote(FileName : string);
        { Searches for all occurances of Term in the KMemo text, makes them Links }
		procedure MakeAllLinks(const MText : ANSIString; const Term: ANSIString; const StartScan : longint =1; EndScan : longint = 0);
        { Makes the passed location a link if its not already one }
		procedure MakeLink(const Link: ANSIString; const Index, Len: longint);
        { Makes the top line look like a title. }
        procedure MarkTitle();
        { Returns true if current cursor is 'near' a bullet item. That could be because we are
  		on a Para Marker thats a Bullet and/or either Leading or Trailing Para is a Bullet.
  		We return with IsFirstChar true if we are on the first visible char of a line (not
  		necessarily a bullet line). If we return FALSE, passed parameters may not be set. }
		function NearABulletPoint(out Leading, Under, Trailing, IsFirstChar, NoBulletPara: Boolean;
            	out BlockNo, TrailOffset, LeadOffset: longint): boolean;
        { Responds when user clicks on a hyperlink }
		procedure OnUserClickLink(sender: TObject);
        { Saves the note in KMemo1, must have title but can make up a file name if needed }
		procedure SaveTheNote();
        	{ Return a string with a title for new note "New Note 2018-01-24 14:46.11" }
        function NewNoteTitle() : ANSIString;
                 { Saves the note as text or rtf, consulting user about path and file name }
        procedure SaveNoteAs(TheExt: string);
    public
        NoteFileName, NoteTitle : string;
        Dirty : boolean;
        Verbose : boolean;
        TemplateIs : AnsiString;
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
    SyncGUI,
    LazFileUtils,		// For ExtractFileName()
    Spelling,
    NoteBook;


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
		TimerSave.Enabled := False;
   		if NoteFileName <> '' then
	   		    RTSearch.DeleteNote(NoteFileName);
        Dirty := False;
		Close;
   end;
end;

procedure TEditBoxForm.ButtLinkClick(Sender: TObject);
var
    ThisTitle : ANSIString;
    Index : integer;
begin
	if KMemo1.Blocks.RealSelLength > 1 then begin
         ThisTitle := KMemo1.SelText;
        // Titles must not start or end with space or contain low characters
        while ThisTitle[1] = ' ' do UTF8Delete(ThisTitle, 1, 1);
        while ThisTitle[UTF8Length(ThisTitle)] = ' ' do UTF8Delete(ThisTitle, UTF8Length(ThisTitle), 1);
        Index := Length(ThisTitle);
        While Index > 0 do begin
            if ThisTitle[Index] < ' ' then delete(ThisTitle, Index, 1);
            dec(Index);
		end;
		// showmessage('[' + KMemo1.SelText +']' + LineEnding + '[' + ThisTitle + ']' );
        if UTF8Length(ThisTitle) > 1 then begin
        	RTSearch.OpenNote(ThisTitle);
            KMemo1Change(self);
		end;
	end;
end;

procedure TEditBoxForm.ButtNotebookClick(Sender: TObject);
begin
    	NotebookPick.FullFileName := NoteFileName;
        NotebookPick.Title := NoteTitle;
		if mrOK = NotebookPick.ShowModal then dirty := True;
end;

procedure TEditBoxForm.MenuBulletClick(Sender: TObject);
var
      BlockNo : longint = 1;
      LastBlock,  Blar : longint;
begin
      // if not KMemo1.SelAvail then exit();
      // Need a better test of valid  selection than that !
      if not Dirty then TimerSave.Enabled := true;
      Dirty := true;
      Label1.Caption := 'd';
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

  AlterFont() is the entry point, it identifies and, if necessary splits blocks
  and calls AlterBlockFont() to do the changes, block by block.
  The decision as to turning [Colour,Bold,Italics] on or off SHOULD be made in
  AlterFont based on first char of selection and passed to AlterBlockFont.
}

procedure TEditBoxForm.AlterFont(const Command : integer; const NewFontSize : integer = 0);
var
	FirstBlockNo, LastBlockNo, IntIndex, LastChar, FirstChar : longint;
	SplitStart : boolean = false;
begin
    Ready := False;
    if not Dirty then TimerSave.Enabled := true;
    Dirty := true;
    Label1.Caption := 'd';
	LastChar := Kmemo1.RealSelEnd;			// SelEnd points to first non-selected char
    FirstChar := KMemo1.RealSelStart;
	FirstBlockNo := Kmemo1.Blocks.IndexToBlockIndex(FirstChar, IntIndex);
    if IntIndex <> 0 then			// Not Starting on block boundary.
		SplitStart := True;
    LastBlockNo := Kmemo1.Blocks.IndexToBlockIndex(LastChar, IntIndex);
    if IntIndex <> (length(Kmemo1.Blocks.Items[LastBlockNo].Text) -1) then 	// Not Last char in block
        LastBlockNo := KMemo1.SplitAt(LastChar) -1;       // we want whats before the split.
    while LastBlockNo > FirstBlockNo do begin
        AlterBlockFont(FirstBlockNo, LastBlockNo, Command, NewFontSize);
        dec(LastBlockNo);
    end;
    // Now, only First Block to deal with
    if SplitStart then
		FirstBlockNo := KMemo1.SplitAt(FirstChar);
    AlterBlockFont(FirstBlockNo, FirstBlockNo, Command, NewFontSize);
    KMemo1.SelEnd := LastChar;	// Any splitting above seems to subtly alter SelEnd, reset.
    KMemo1.SelStart := FirstChar;
	Ready := True;
end;


	{  Takes a Block number and applies changes to that block }
procedure TEditBoxForm.AlterBlockFont(const FirstBlockNo : longint; const BlockNo : longint; const Command : integer; const NewFontSize : integer = 0);
var
	Block, FirstBlock : TKMemoTextBlock;

begin
    FirstBlock := TKMemoTextBlock(KMemo1.Blocks.Items[FirstBlockNo]);
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
		ChangeBold :   	if fsBold in FirstBlock.TextStyle.Font.style then begin
						Block.TextStyle.Font.Style := Block.TextStyle.Font.Style - [fsBold];
					end else begin
						Block.TextStyle.Font.Style := Block.TextStyle.Font.Style + [fsBold];
					end;
		ChangeItalic :
					if fsItalic in FirstBlock.TextStyle.Font.style then begin
						Block.TextStyle.Font.Style := Block.TextStyle.Font.Style - [fsItalic];
					end else begin
						Block.TextStyle.Font.Style := Block.TextStyle.Font.Style + [fsItalic];
					end;
		ChangeColor : 	if FirstBlock.TextStyle.Font.Color = NormalColor then begin
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
    {$ifdef WINDOWS}
    Ptr, EndP : PChar;			// Will generate "not used" warnings in Unix
    {$endif}
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
   if not Dirty then TimerSave.Enabled := true;
   Dirty := true;
   Label1.Caption := 'd';
end;

procedure TEditBoxForm.MenuItemExportPlainTextClick(Sender: TObject);
begin
     SaveNoteAs('txt');
end;

procedure TEditBoxForm.MenuItemExportRTFClick(Sender: TObject);
begin
   SaveNoteAs('rtf');
end;

procedure TEditBoxForm.SaveNoteAs(TheExt : string);
var
    SaveExport : TSaveDialog;
begin
     SaveExport := TSaveDialog.Create(self);
     SaveExport.DefaultExt := TheExt;
     if Sett.ExportPath <> '' then
        SaveExport.InitialDir := Sett.ExportPath
     else begin
          {$ifdef UNIX}
          SaveExport.InitialDir :=  GetEnvironmentVariable('HOME');
          {$endif}
          {$ifdef WINDOWS}
          SaveExport.InitialDir :=  GetEnvironmentVariable('HOMEPATH');
          {$endif}
     end;
     SaveExport.Filename := StringReplace(Caption, #32, '', [rfReplaceAll]) + '.' + TheExt;
     if SaveExport.Execute then begin
        if 'txt' = TheExt then
           KMemo1.SaveToTXT(SaveExport.FileName)
        else if 'rtf' = TheExt then
           KMemo1.SaveToRTF(SaveExport.FileName);
        Sett.ExportPath := ExtractFilePath(SaveExport.FileName);  // Hmm, UTF8 ?
     end;
     //showmessage(SaveExport.FileName);
     SaveExport.Free;
end;

procedure TEditBoxForm.MenuItemPasteClick(Sender: TObject);
begin
    Ready := False;
    KMemo1.ExecuteCommand(ecPaste);
   if not Dirty then TimerSave.Enabled := true;
   Dirty := true;
   Label1.Caption := 'd';
   Ready := True;
end;

procedure TEditBoxForm.MenuItemSelectAllClick(Sender: TObject);
begin
	KMemo1.ExecuteCommand(ecSelectAll);
end;

procedure TEditBoxForm.MenuItemSpellClick(Sender: TObject);
var
    SpellBox : TFormSpell;
begin
    SpellBox := TFormSpell.Create(Application);
    // SpellBox.Top := Placement + random(Placement*2);
    // SpellBox.Left := Placement + random(Placement*2);
    SpellBox.TextToCheck:= KMemo1.Blocks.Text;
    SpellBox.TheKMemo := KMemo1;
    SpellBox.ShowModal;
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

procedure TEditBoxForm.TimerSaveTimer(Sender: TObject);
begin
    TimerSave.Enabled:=False;
	// showmessage('Time is up');
    SaveTheNote();
    Label1.Caption := 'c';
end;



{	FormShow gets called under a number of conditions Title    Filename       Template
	- Re-show, everything all loaded.  Ready = true   yes      .              .
      just exit
    - New Note                                        no       no             no
      GetNewTitle(), add CR, CR, Ready, MarkTitle(O),
      zero dates.
    - New Note from Template                          no       yes, dispose   yes   R1
      ImportNote(), null out filename
    - New Note from Link Button, save immediatly      yes      no             no
      cp Title to Caption and to KMemo, Ready,
      MarkTitle().
    - Existing Note from eg Tray Menu, Searchbox      yes      yes            no    R1
      ImportNote()
}

procedure TEditBoxForm.FormShow(Sender: TObject);
var
    ItsANewNote : boolean = false;
begin
    if Ready then exit();				// its a "re-show" event. Already have a note loaded.
    //Label1.Caption := '';
    TimerSave.Enabled := False;
    KMemo1.Font.Size := Sett.FontNormal;
    Kmemo1.Clear;
    MenuItemSync.Enabled := (Sett.RemoteRepo <> '');
    if NoteFileName = '' then begin		// might be a new note or a new note from Link
        if NoteTitle = '' then              // New Note
			NoteTitle := NewNoteTitle();
        ItsANewNote := True;
	end else begin
    	ImportNote(NoteFileName);		// also sets Caption and Createdate
        if TemplateIs <> '' then begin
            NoteFilename := '';
            NoteTitle := NewNoteTitle();
            ItsANewNote := True;
		end;
	end;
    if ItsANewNote then begin
        CreateDate := '';
        Caption := NoteTitle;
    	KMemo1.Blocks.AddParagraph();
    	KMemo1.Blocks.AddParagraph();
        if kmemo1.blocks.Items[0].ClassNameIs('TKMemoParagraph') then
        	Kmemo1.Blocks.DeleteEOL(0);
        if kmemo1.blocks.Items[0].ClassNameIs('TKMemoTextBlock') then
            Kmemo1.Blocks.DeleteEOL(0);
        KMemo1.Blocks.AddTextBlock(NoteTitle, 0);
	end;
    Ready := true;
    MarkTitle();
    KMemo1.SelStart := KMemo1.Text.Length;  // set curser pos to end
    KMemo1.SelEnd := Kmemo1.Text.Length;
    KMemo1.SetFocus;
    Dirty := False;
    Label1.Caption := 'c';
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
        // debugln('Going to save.');
        SaveTheNote();
        // debugln('Saved');
	end;
    RTSearch.NoteClosing(NoteFileName);
end;

function TEditBoxForm.GetTitle(out TheTitle : ANSIString) : boolean;
var
    BlockNo : longint = 0;
    //TestSt : ANSIString;
begin
    Result := False;
    TheTitle := '';
    while Kmemo1.Blocks.Items[BlockNo].ClassName <> 'TKMemoParagraph' do begin
	// while Kmemo1.Blocks.Items[BlockNo].ClassName = 'TKMemoTextBlock' do begin
        TheTitle := TheTitle + Kmemo1.Blocks.Items[BlockNo].Text;
       	inc(BlockNo);
        //TestSt := Kmemo1.Blocks.Items[BlockNo].ClassName;
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
    	// while Kmemo1.Blocks.Items[BlockNo].ClassName = 'TKMemoTextBlock' do begin
        while Kmemo1.Blocks.Items[BlockNo].ClassName <> 'TKMemoParagraph' do begin
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
//    KMemo1.Select(Index, 0);
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
//    KMemo1.Select(Index, Len);
end;

{ Searches for all occurances of Term in the KMemo text. Does
  not bother with single char terms. Some potential to speed up
  things here I am sure.
}
procedure TEditBoxForm.MakeAllLinks(const MText : ANSIString; const Term : ANSIString; const StartScan : longint =1; EndScan : longint = 0);
var
	Offset, NumbCR   : longint;
    {$ifdef WINDOWS}
    Ptr, EndP : PChar;                  // Will generate "not used" warning in Unix
    {$endif}
begin
    // CRCount := 0;
    Offset := UTF8Pos(Term, MText, StartScan);
    while Offset > 0 do begin
    	NumbCR := 0;
        {$ifdef WINDOWS}                // does no harm in Unix but a bit slow ?
        Ptr := PChar(Mtext);
        EndP := Ptr + length(UTF8Copy(MText, 1, Offset-1));

        //EndP := Ptr + Offset-1;
        while Ptr < EndP do begin
            if Ptr^ = #13 then inc(NumbCR);
debugln('ch ' + inttostr(ord(Ptr^)));
            inc(Ptr);
		end;
debugln('Corrected for ' + inttostr(NumbCR) + ' #13s');
        {$endif}
		MakeLink(Term, Offset -1 -NumbCR, UTF8length(Term));
        Offset := UTF8Pos(Term, MText, Offset+1);
        if EndScan > 0 then
        	if Offset> EndScan then break;
    end;
end;


procedure TEditBoxForm.CheckForLinks(const StartScan : longint =1; EndScan : longint = 0);
var
    Searchterm : ANSIstring;
    Len : longint;
    MemoText : AnsiString;
begin
	if not Ready then exit();
    Len := length(KMemo1.Blocks.text);      // saves 7mS by calling length() only once ! But still 8mS
    if StartScan >= Len then exit;   // prevent crash when memo almost empty
    if EndScan > Len then EndScan := Len;
    Ready := False;
	RtSearch.StartSearch();
    KMemo1.Blocks.LockUpdate;
    MemoText := KMemo1.Blocks.text;     // Make one copy and use it repeatadly, actual text does not change
    while RTSearch.NextNoteTitle(SearchTerm) do
        if SearchTerm <> NoteTitle then
        	MakeAllLinks(MemoText, SearchTerm, StartScan, EndScan);
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


procedure TEditBoxForm.DoHousekeeping();
var
    CurserPos, StartScan, EndScan, BlockNo, Blar : longint;
    TempTitle : ANSIString;
    // TS1, TS2, TS3, TS4 : TTimeStamp;           // Temp time stamping to test speed
begin
  CurserPos := KMemo1.RealSelStart;
  StartScan := CurserPos - LinkScanRange;
  if StartScan < length(Caption) then StartScan := length(Caption);
  EndScan := CurserPos + LinkScanRange;
  if EndScan > length(KMemo1.Text) then EndScan := length(KMemo1.Text);   // Danger - should be KMemo1.Blocks.Text !!!
  // TS1:=DateTimeToTimeStamp(Now);

  BlockNo := KMemo1.Blocks.IndexToBlockIndex(CurserPos, Blar);

  if ((BlocksInTitle + 3) > BlockNo) then begin
      // We don't check title if user is not close to it.
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
  //Debugln('Housekeeper called');

  // Memo1.append('Clear ' + inttostr(TS2.Time-TS1.Time) + 'ms  Check ' + inttostr(TS3.Time-TS2.Time));

  { Some notes about timing, 'medium' powered Linux laptop, 20k note.
    Checks and changes to Title - less than mS
    ClearNearLinks (none present) - less than mS
    CheckForLinks (none present) - 180mS, thats mostly used up by MakeLinks()
    	but length(KMemo1.Blocks.text) needs about 7mS too.

    Can do better !
  }
end;

procedure TEditBoxForm.TimerHousekeepingTimer(Sender: TObject);
begin
    TimerHouseKeeping.Enabled := False;
    DoHouseKeeping();
    { TODO : Hmm, do we have race conditions here ?  A change happens during housekeeping, it will restart timer, thats OK. }
end;

	{ Any change to the note text and this gets called. So, vital it be quick }
procedure TEditBoxForm.KMemo1Change(Sender: TObject);
begin
    if not Ready then exit();           // don't do any of this while starting up.
    //if not Dirty then TimerSave.Enabled := true;
    Dirty := true;
    Label1.Caption := 'd';

    TimerHouseKeeping.Enabled := False;
    TimerHouseKeeping.Enabled := True;
    TimerSave.Enabled := False;
    TimerSave.Enabled := True;
    //DoHouseKeeping();
end;

function TEditBoxForm.NearABulletPoint(out Leading, Under, Trailing, IsFirstChar, NoBulletPara : Boolean;
        								out BlockNo, TrailOffset, LeadOffset : longint ) : boolean;
	// on medium linux laptop, 20k note this function takes less than a mS
var
  PosInBlock, Index, CharCount : longint;
begin
  Under := False;
  NoBulletPara := False;
  BlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, PosInBlock);
  if kmemo1.blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin
  		Under := (TKMemoParagraph(kmemo1.blocks.Items[BlockNo]).Numbering = pnuBullets);
        NoBulletPara := not Under;
  end;
  Index := 1;
  CharCount := PosInBlock;
  while BlockNo >= Index do begin
	if kmemo1.blocks.Items[BlockNo-Index].ClassNameIs('TKMemoParagraph') then break;
  	CharCount := CharCount + kmemo1.blocks.Items[BlockNo-Index].Text.Length;
	inc(Index);

    // Danger - what if we don't find one going left ?
  end;
  if BlockNo < Index then begin
      Result := False;
      if Verbose then debugln('Returning False as we appear to be playing in Heading.');
      exit();
  end
  else Leading := (TKMemoParagraph(kmemo1.blocks.Items[BlockNo-Index]).Numbering = pnuBullets);
  IsFirstChar := (CharCount = 0);
  LeadOffset := Index;
  Index := 0;
  while true do begin
    // must not call Classnameis with blockno = count
    if Verbose then
        debugln('Doing para seek, C=' + inttostr(KMemo1.Blocks.Count) + ' B=' + inttostr(BlockNo) + ' I=' + inttostr(Index));
    inc(Index);
    if (BlockNo + Index) >= (Kmemo1.Blocks.Count) then begin
        if Verbose then debugln('Overrun looking for a para marker.');
        // means there are no para markers beyond here.  So cannot be TrailingBullet
    		{KMemo1.Blocks.AddParagraph();
    		KMemo1.ExecuteCommand(ecUp);
   			KMemo1.ExecuteCommand(ecLineEnd);  }
        Index := 0;
        break;
    end;
	if kmemo1.blocks.Items[BlockNo+Index].ClassNameIs('TKMemoParagraph') then break;
  end;
  TrailOffset := Index;
  if TrailOffset > 0 then
  	Trailing := (TKMemoParagraph(kmemo1.blocks.Items[BlockNo+Index]).Numbering = pnuBullets)
  else Trailing := False;
  Result := (Leading or Under or Trailing);
  if Verbose then begin
	debugln('IsNearBullet -----------------------------------');
    Debugln('      Result      =' + booltostr(Result, true));
    Debugln('      Leading     =' + booltostr(Leading, true));
    Debugln('      Under       =' + booltostr(Under, true));
    Debugln('      Trailing    =' + booltostr(Trailing, true));
    Debugln('      IsFirstChar =' + booltostr(IsFirstChar, true));
    Debugln('      NoBulletPara=' + booltostr(NoBulletPara, true));
  end;
end;

{	To behave like end users expect when pressing BackSpace we have to alter KMemo's way of thinking.

a	If the cursor is at the end of a Bullet Text, KMemo would remove the Bullet
    Marker, we stop that and remove the last character of the visible string.

b   If the cursor is at the begininng of a Bullet Text we must cancel the bullet (which is at the
    end of the Text) and not merge this line with one above. We know this is the case if the
    trailing paragraph marker is bullet AND we are the first char of the first block of the text.

c   If the cursor is on next line after a bullet, on a para marker that is not a bullet and there
	is no text on that line after the cursor, all we do is delete that para marker.

d   Again, we are on first char of the line after a bullet, this line is not a bullet itself
	and it has some text after the cursor. We merge that text up to the bullet line above,
    retaining its bulletness. So, mark trailing para bullet, delete leading.


x	A blank line, no bullet between two bullet lines. Use BS line should dissapear.
    That is, delete para under cursor, move cursor to end line above. This same as c

y   There is nothing after our bullet para marker. So, on an empty bulletline, user presses
	BS to cancel bullet but that cancels bullet and moves us up to next (bulleted) line.
    It has to, there is nowhere else to go. Verbose shows this as a case c ????

     	Lead Under Trail First OnPara(not bulleted)
    a     ?    T     ?    F        remove the last character of the visible string to left.
    b     ?    F     T    T    F   Cursor at start, cancel bullet, don't merge

    x     T    F     T    T    T   Just delete this para. if Trailing move cursor to end of line above.
    c     T    F     F    T    T   Just delete this para. if Trailing move cursor to end of line above.
    y     T    T     F    T    F   Like c but add a para and move down. Not happy .....
    d     T    F     F    T    F   mark trailing para as bullet, delete leading.
    e     T    T     T    T    F   must remove Bullet for para under cursor

    Special case where curser is at end of a bullet and there is no para beyond there ?
    So, its should act as (a) but did, once, act as (d) ?? Needs more testing ......
}

procedure TEditBoxForm.KMemo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  TrailOffset, BlockNo, {BlockIndex,} LeadOffset  : longint;
  LeadingBullet, UnderBullet, TrailingBullet, FirstChar {, NearBullet} : boolean;
  NoBulletPara : boolean = false;
begin
    if not Ready then exit();
    if [ssCtrl] = Shift then begin
       if key = ord('F') then begin MenuItemFindClick(self); Key := 0; exit(); end;
       if key = ord('N') then begin RTSearch.TrayMenuNewClick(self); Key := 0; exit(); end;
    end;
    if [ssCtrl, ssShift] = Shift then begin
       if key = ord('F') then begin ButtSearchClick(self); Key := 0; exit(); end;
    end;
    if Key <> 8 then exit();    // We are watching for a BS on a Bullet Marker
    // Mac users don't have a del key, they use a backspace key thats labled 'delete'. Sigh...
    if KMemo1.Blocks.RealSelEnd > KMemo1.Blocks.RealSelStart then exit();
    if not NearABulletPoint(LeadingBullet, UnderBullet, TrailingBullet, FirstChar, NoBulletPara,
    				BlockNo, TrailOffset, LeadOffset) then exit();
    if (not FirstChar) and (not UnderBullet) then exit();
    // We do have to act, don't pass key on.
    Key := 0;
    Ready := False;
    // KMemo1.Blocks.LockUpdate;  Dont lock because we move the cursor down here.
    	if UnderBullet and (not FirstChar) then begin   // case a
            KMemo1.ExecuteCommand(ecDeleteLastChar);
            if Verbose then debugln('Case a');
            Ready := True;
            exit();
        end;
        // anything remaining must have FirstChar
        if TrailingBullet and (not NoBulletPara) then begin	// case b
            if Verbose then debugln('Case b or e');
            if UnderBullet then  						// case e
              	TrailOffset := 0;
            if kmemo1.blocks.Items[BlockNo+TrailOffset].ClassNameIs('TKMemoParagraph') then
            	TKMemoParagraph(kmemo1.blocks.Items[BlockNo+TrailOffset]).Numbering := pnuNone
            else DebugLn('ERROR - this case b block should be a para');
            Ready := True;
            exit();
        end;
        // anything remaining is outside bullet list, looking in. Except if Trailing is set...
        if  kmemo1.blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin
            KMemo1.Blocks.Delete(BlockNo);		// delete this blank line.
            if TrailingBullet then begin
            	KMemo1.ExecuteCommand(ecUp);
            	KMemo1.ExecuteCommand(ecLineEnd);
                if Verbose then debugln('Case x');
			end else begin
            	if UnderBullet then begin				// this test is wrong, real test is are we at end of text ?
                    if Verbose then DebugLn('Case y');
                    KMemo1.Blocks.AddParagraph();		// Maybe only need add that if at end of text, NearABulletPoint() could tell us ?
                    KMemo1.ExecuteCommand(ecDown);
                end else
            		if Verbose then debugln('Case c');
            end;
        end else begin				// merge the current line into bullet above.
            if kmemo1.blocks.Items[BlockNo+TrailOffset].ClassNameIs('TKMemoParagraph') then
            	TKMemoParagraph(kmemo1.blocks.Items[BlockNo+TrailOffset]).Numbering := pnuBullets
            else DebugLn('ERROR - this case d block should be a para');
            if  kmemo1.blocks.Items[BlockNo-Leadoffset].ClassNameIs('TKMemoParagraph') then begin
            	KMemo1.Blocks.Delete(BlockNo-LeadOffset);
            	if Verbose then debugln('Case d');
        	end;
    	end;
    Ready := True;
    // most of the intevention paths through this method take ~180mS on medium powered linux laptop
end;



	{ --- I M P O R T I N G   and   E X P O R T I N G    F U N C T I O N S  ---  }

procedure TEditBoxForm.ImportNote(FileName: string);
var
    Loader : TBLoadNote;
 	T1 : qword;          // Temp time stamping to test speed
begin
    // Timing numbers below using MyRecipes on my Acer linux laptop. For local comparison only !
    T1 := gettickcount64();
    Loader := TBLoadNote.Create();
    Loader.FontNormal:= Sett.FontNormal;
    // Loader.FontName := FontName;
    Loader.FontSize:= Sett.FontNormal;
    KMemo1.Blocks.LockUpdate;
    KMemo1.Clear;
    Loader.LoadFile(FileName, KMemo1);                        // 340mS
    KMemo1.Blocks.UnlockUpdate;                             // 370mS
    Createdate := Loader.CreateDate;
    Ready := true;
    Caption := Loader.Title;
    if Sett.ShowIntLinks then
    	CheckForLinks();                     		// 360mS
    Loader.Free;
    TimerHouseKeeping.Enabled := False;     // we have changed note but no housekeeping reqired
    debugln('Load Note=' + inttostr(gettickcount64() - T1) + 'mS');
end;

procedure TEditBoxForm.MenuItemWriteClick(Sender: TObject);
begin
    SaveTheNote();
end;

procedure TEditBoxForm.SaveTheNote();
var
 	Saver : TBSaveNote;
    SL : TStringList;
    // TestI : integer;
begin
  	if length(NoteFileName) = 0 then
        NoteFileName := Sett.NoteDirectory + GetAFilename();
    Saver := TBSaveNote.Create();
    KMemo1.Blocks.LockUpdate;
    try
       if not GetTitle(Saver.Title) then begin
           Saver.Destroy;
           exit();
       end;
       {debugln('Saving Note, length of title =' + inttostr(length(Caption)) + ' No blocks =' + inttostr(KMemo1.Blocks.Count));
       for TestI := 0 to KMemo1.Blocks.Count -1 do begin
           debugln('Block=' + inttostr(TestI) + ' Type=' + KMemo1.Blocks[TestI].ClassName);
       end;      }

       Caption := Saver.Title;
       Saver.CreateDate := CreateDate;
       if TemplateIs <> '' then begin
          SL := TStringList.Create();
          SL.Add(TemplateIs);
          RTSearch.NoteLister.SetNotebookMembership(ExtractFileNameOnly(NoteFileName) + '.note', SL);
          SL.Free;
          TemplateIs := '';
	   end;
       // debugln('about to save');
       Saver.Save(NoteFileName, KMemo1);
       // debugln('saved');
       RTSearch.UpdateList(Caption, Saver.TimeStamp, NoteFileName, self);
       // debugln('List updated');
	   Saver.Destroy;
       // debugln('All saved OK');
    finally
              Dirty := false;
              KMemo1.Blocks.UnLockUpdate;
    end;
end;

function TEditBoxForm.NewNoteTitle(): ANSIString;
{ var
    ThisMoment : TDateTime;  }
begin
  // ThisMoment:=Now;
  Result := 'New Note ' + FormatDateTime('YYYY-MM-DD hh:mm:ss.zzz', Now);
end;



function TEditBoxForm.GetAFilename() : ANSIString;
var
  GUID : TGUID;
begin
   CreateGUID(GUID);
   Result := copy(GUIDToString(GUID), 2, 36) + '.note';
end;

end.

