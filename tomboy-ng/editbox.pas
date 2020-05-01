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
 * the following conditWSpace - StartingAtions: 
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
    2018/04/11  Replaced a loom and delchar() with setting selection and calling ClearSelection im MakeLink
                Added a function to deal with Delete menu selection.
                Restored selection properly after housekeeping.
    2018/04/12  Added a function to set the KMemo to readonly (for when the Sync Process
                has replaced or deleted the on disk copy of this note).  SetReadOnly();
    2018/04/13  Added calls to start Housekeeping and Save times when editing inside bullets !
    2018/04/13  Now call NotebookPick Form dynamically and ShowModal to ensure two notes don't share.
    2018/05/02  Enabled untested code to print.
    2018/05/03  Now put a * ahead of note name to indicate its unsaved.
    2018/05/04  Use CleanCaption() when using Caption elsewhere.
    2018/05/07  Bug in MarkDirty(), now always enable SaveTimer
    2018/05/07  Added a paste command into FormShow() that appears to fix strange bug where the first
                copy (as in Copy and paste) fails. This is a nasty fudge, perhapse related to
                http://bugs.freepascal.org/view.php?id=28679    Linux only ?
    2018/05/12  Extensive changes - MainUnit is now just that.
    2018/05/16  Disable Print menu option in Cocoa.
    2018/06/13  Drop copy on selection and add Ben's Underline, strikethrough and Fixedwidth !
    2018/06/13  Reinstate copy on selection, middle button click, Linux & (in app only) Windows only
    2018/06/22  DRB added LoadSingleNote and related to do just that. Needs more testing.
    2018/07/05  Changed MonospaceFont to 'Monaco' on the Mac, apparently universal...
    2018/07/20  Force copy on selection paste to always paste to left of a newline.
    2018/07/23  If a note has no title in content but does have one in xml, caption is
                left blank and that crashes things that look for * in first char. Fixed
    2018/07/23  Fixed a bug that crashed when deleting a note in SingleNoteMode.
    2018/08/18  Added ^F4 to quit.  Prevented undefined ^keys being passed into Kmemo
    2018/08/20  Above edit dropped ^X, ^C, ^V before kmemo sees them, fixed, refactored a bit
    2019/08/22  Add a whole lot more keys that KMemo auto supports, see AddKey(...) in keditcommon.pas
    2018/10/13  Kmemo1KeyDown now deals with a Tab.
    2018/10/20  Added --save-exit, only in single note mode.
    2018/10/28  Support Backup management, snapshots and new sync Model.
    2018/11/29  Now check if Spell is configured before calling its GUI
    2018/12/02  Change to Bullet code, now support ALT+RGHT and ALT+Left, now can toggle bullet mode
    2018/12/03  Use command key instead of control on the Mac
    2018/12/04  Links to other notes no longer case sensitive, a potential link needs to be surrounded by white-ish space
    2018/12/05  Move highlight shortcut key on the Mac to Alt-H because Apple uses Cmd-H
    2018/12/06  Drop all Ctrl Char on floor for the Mac. See if we are missing anything ?
    2018/12/06  Added Ctrl 1, 2, 3, 4 as small, normal, large and huge fnt.
                ---- This is not put on menus or doced anywhere, an experiment ------
    2018/12/29  Small improvements in time to save a file.
    2019/01/15  Added Calculator, Ctrl-E for evaluate. Need to truncate floats .....
    2019/01/16  Tidy up of float display
    2019/01/17  Added tan() to list of functions in Calc, go public with Ctrl1,2,3,4
    2019/01/19  Can tolerate, in places, an imageblock
    2019/02/01  ButtLinkClick() now provides a template name iff current note is a Notebook Member.
                However, its the first notebook listed, if user has allowed multiple
                notebooks per note, maybe not what they want. Maybe a selection list ?
    2019/02/12  Fixed UTF8 bug in MakeAllLinks(), a touch faster now too !
    2019/02/23  Bug in column calc - how this that slip through ?
    2019/03/13  Better local search capability and go to first term if opening result of Search
    2019/04/13  Lockupdate while setting whole note text colour.
    2019/04/18  Replaced TBitBtns with Speedbuttons to fix memory leak in Cocoa
    2019/04/29  Restore note's previous previous position and size.
    2019/05/06  Support saving pos and open on startup in note.
    2019/05/14  Display strings all (?) moved to resourcestrings
    2019/06/12  Removed panel behind speedbuttons, Cocoa did not like them !
    2019/06/14  Ensure top of new window is never less than 10 pixels down.
    2019/07/19  Test that a note is not being deleted before we update on exit.
    2019/07/20  Cleaned up MarkTitle() and extended the range its used for.
    2019/07/21  MarkTitle now uses Sett.* colours.
    2019/07/25  Added menu item under tools to open Settings #93 (part)
    2019/09/07  User can now select a note font.
    2019/09/21  CleanUTF8 removes some bad UTF8 char when importing some RTF files.
    2019/09/21  AdjustFormPosition() now enforces some minium position/size. Issue #103
    2019/10/11  Enabling of printing under Cocoa
    2019/11/30  Now support web links.
    2019/12/11  Heavily restructured Startup, Main Menu everywhere !
    2019/12/17  Links are no longer converted to lower case.
    2019/12/18  LinkScanRange moved here from Settings, now 100, was 50
    2019/12/22  Extensive changes to ClearNearLink() to ensure links are not smeared.
    2020/01/02  Enabled Ctrl-Shift left or right arrow selecting or extending selecton by word.
    2020/01/07  Use SaveTheNote() even when existing app with a clean note, UpdateNote() not used now
    2020/01/12  More agressive adjustmenst to form position at opening a note Windows and Mac only
    2020/01/28  Dont call SearchForm.UpdateList() when we are closing a clean note.
    2020/03/11  In FormDestroy, we always save, EXCEPT if in SingleNoteMode, then only if dirty.
    2020/03/27  Don't save a new, unwritten to note, also prevent 2 saves on a Ctrl-F4
    2020/03/27  Set a cleared highlight to correct background colour.
                No longer toggle when changing font sizes, set it to what user asks.
    2020/04/01  Removed line that exited KMemo1KeyDown in readonly mode, prevented cursor keys working.
}


{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, { FileUtil,} Forms, Controls, Graphics, Dialogs, ExtCtrls,
    Menus, StdCtrls, Buttons, kmemo, LazLogger, PrintersDlgs,
    clipbrd, lcltype,      // required up here for copy on selection stuff.
    fpexprpars,         // for calc stuff ;
    SaveNote;      		// Knows how to save a Note to disk in Tomboy's XML



type

    { TEditBoxForm }

    TEditBoxForm = class(TForm)
		FindDialog1: TFindDialog;
        KMemo1: TKMemo;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        MenuBold: TMenuItem;
        MenuItalic: TMenuItem;
        MenuHighLight: TMenuItem;
        MenuHuge: TMenuItem;
        MenuBullet: TMenuItem;
        MenuItem1: TMenuItem;
        MenuItem4: TMenuItem;
        MenuItemSettings: TMenuItem;
        MenuItemEvaluate: TMenuItem;
        MenuItemIndex: TMenuItem;
        MenuItemExportMarkdown: TMenuItem;
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
        MenuUnderline: TMenuItem;
        MenuStrikeout: TMenuItem;
        PanelReadOnly: TPanel;
        PopupMainTBMenu: TPopupMenu;
		PopupMenuRightClick: TPopupMenu;
        PopupMenuTools: TPopupMenu;
        PopupMenuText: TPopupMenu;
        PrintDialog1: TPrintDialog;
        ButtMainTBMenu: TSpeedButton;
        SpeedButtonDelete: TSpeedButton;
        SpeedButtonLink: TSpeedButton;
        SpeedButtonNotebook: TSpeedButton;
        SpeedButtonSearch: TSpeedButton;
        SpeedButtonText: TSpeedButton;
        SpeedButtonTools: TSpeedButton;
		TaskDialogDelete: TTaskDialog;
		TimerSave: TTimer;
        TimerHousekeeping: TTimer;
        procedure ButtMainTBMenuClick(Sender: TObject);
        procedure ButtTBMenuClick(Sender: TObject);
  procedure FindDialog1Find(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
        procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
        	{ gets called under a number of conditions, easy one is just a re-show,
              or for a new note or a new note with a title from Link button
              or for an existing note where we get note file name
              or a new note from template where we have a note filename but IsTemplate
              also set, here we discard file name and make a new one. }
        procedure FormShow(Sender: TObject);
        procedure KMemo1Change(Sender: TObject);
        	{ Watchs for  backspace affecting a bullet point, and whole lot of ctrl, shift, alt
              combinations. For things we let KMemo handle, just exit, for things we handle
              must set key to 0 after doing so. }
		procedure KMemo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure KMemo1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure KMemo1MouseUp(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
        procedure MenuBoldClick(Sender: TObject);
        procedure MenuBulletClick(Sender: TObject);
        procedure MenuFixedWidthClick(Sender: TObject);
        procedure MenuHighLightClick(Sender: TObject);
        procedure MenuHugeClick(Sender: TObject);
        procedure MenuItalicClick(Sender: TObject);
        procedure MenuItemEvaluateClick(Sender: TObject);
        procedure MenuItemExportMarkdownClick(Sender: TObject);
        procedure MenuItemIndexClick(Sender: TObject);
        procedure MenuItemSettingsClick(Sender: TObject);
        procedure MenuUnderlineClick(Sender: TObject);
        procedure MenuStrikeoutClick(Sender: TObject);
		procedure MenuItemCopyClick(Sender: TObject);
		procedure MenuItemCutClick(Sender: TObject);
        procedure MenuItemDeleteClick(Sender: TObject);
        procedure MenuItemExportPlainTextClick(Sender: TObject);
        procedure MenuItemExportRTFClick(Sender: TObject);
		procedure MenuItemFindClick(Sender: TObject);
		procedure MenuItemPasteClick(Sender: TObject);
        procedure MenuItemPrintClick(Sender: TObject);
		procedure MenuItemSelectAllClick(Sender: TObject);
        procedure MenuItemSpellClick(Sender: TObject);
		procedure MenuItemSyncClick(Sender: TObject);
        procedure MenuItemWriteClick(Sender: TObject);
        procedure MenuLargeClick(Sender: TObject);
        procedure MenuNormalClick(Sender: TObject);
        procedure MenuSmallClick(Sender: TObject);
        procedure SpeedButtonDeleteClick(Sender: TObject);
        procedure SpeedButtonLinkClick(Sender: TObject);
        procedure SpeedButtonNotebookClick(Sender: TObject);
        procedure SpeedButtonSearchClick(Sender: TObject);
        procedure SpeedButtonTextClick(Sender: TObject);
        procedure SpeedButtonToolsClick(Sender: TObject);
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
        // Set True by the delete button so we don't try and save it.
        DeletingThisNote : boolean;
        procedure AdjustFormPosition();
        { Alters the Font of Block as indicated }
        procedure AlterBlockFont(const FirstBlockNo, BlockNo: longint;
				const Command: integer; const NewFontSize: integer=0);
        { Alters the font etc of selected area as indicated }
        procedure AlterFont(const Command : integer; const NewFontSize: integer = 0);
        { If Toggle is true, sets bullets to what its currently no. Otherwise sets to TurnOn}
        procedure BulletControl(const Toggle, TurnOn: boolean);
        { Looks between StartS and EndS, marking any http link. Byte, not char indexes.
          A weblink has leading and trailing whitespace, starts with http:// or https://
          and has a dot and char after the dot. We expect kmemo1 is locked at this stage.}
        procedure CheckForHTTP(const PText: pchar; const StartS, EndS: longint);
        procedure CleanUTF8();
        function ColumnCalculate(out AStr: string): boolean;
        function ComplexCalculate(out AStr: string): boolean;
        procedure ExprTan(var Result: TFPExpressionResult;
            const Args: TExprParameterArray);
        function FindIt(Term: string; GoForward, CaseSensitive: boolean
            ): boolean;
        function FindNumbersInString(const AStr: string; out AtStart, AtEnd: string
            ): boolean;
        procedure InsertDate();
        function ParagraphTextTrunc(): string;
        function RelativePos(const Term: ANSIString; const MText: PChar;
            StartAt: integer): integer;
        function PreviousParagraphText(const Backby: integer): string;
        function SimpleCalculate(out AStr: string): boolean;
        // procedure CancelBullet(const BlockNo: longint; const UnderBullet: boolean);

		procedure ClearLinks(const StartScan : longint =0; EndScan : longint = 0);
        { Looks around current block looking for link blocks. If invalid, 'unlinks' them.
          Http or local links, we need to clear the colour and underline of any text nearby
          that have been 'smeared' from user editing at the end of a link. When this happens,
          new text appears within the link block, bad .....  }
        procedure ClearNearLink(const StartS, EndS: integer);
        function DoCalculate(CalcStr: string): string;
        procedure DoHousekeeping();
        { Returns a long random file name, Not checked for clashes }
        function GetAFilename() : ANSIString;
        procedure CheckForLinks(const StartScan : longint = 1; EndScan : longint = 0);
        { Returns with the title, that is the first line of note, returns False if title is empty }
        function GetTitle(out TheTitle: ANSIString): boolean;
        procedure ImportNote(FileName : string);
        procedure InitiateCalc();
        { Test the note to see if its Tomboy XML, RTF or Text. Ret .T. if its a new note. }
        function LoadSingleNote() : boolean;
        { Searches for all occurances of Term in the KMemo text, makes them Links
          Does not bother with single char terms. Expects KMemo1 to be already locked.}
        procedure MakeAllLinks(const PText: PChar; const Term: ANSIString;
            const StartScan: longint=1; EndScan: longint=0);

        { Makes a link at passed position as long as it does not span beyond a block.
            And if it does span beyond one block, I let that go through to the keeper.
            Making a Hyperlink, deleting the origional text is a very slow process so we
            make heroic efforts to avoid having to do so. Index is char count, not byte.
            Its a SelectionIndex.  Note we no longer need pass this p the Link, remove ? }
		procedure MakeLink(const Index, Len: longint);

                            { Makes sure the first (and only the first) line is marked as Title
                            Title should be Blue, Underlined and FontTitle big.
                            Note that when a new note is loaded from disk, this function is not called,
                            the Load unit knows how to do it itself. Saves 200ms with a big (20K) note. }
        procedure MarkTitle();
        { Returns true if current cursor is 'near' a bullet item. That could be because we are
  		on a Para Marker thats a Bullet and/or either Leading or Trailing Para is a Bullet.
  		We return with IsFirstChar true if we are on the first visible char of a line (not
  		necessarily a bullet line). If we return FALSE, passed parameters may not be set. }
		function NearABulletPoint(out Leading, Under, Trailing, IsFirstChar, NoBulletPara: Boolean;
            	out BlockNo, TrailOffset, LeadOffset: longint): boolean;
        { Responds when user clicks on a hyperlink }
		procedure OnUserClickLink(sender: TObject);
        // A method called by this or other apps to get what we might have selected
        procedure PrimaryCopy(const RequestedFormatID: TClipboardFormat;
            Data: TStream);
        // Pastes into KMemo whatever is returned by the PrimarySelection system.
        procedure PrimaryPaste(SelIndex: integer);
        { Saves the note in KMemo1, must have title but can make up a file name if needed
          If filename is invalid, bad GUID, asks user if they want to change it (they do !)
          WeAreClosing indicates that the whole application is closing (not just this note)
          We always save the note on FormDestroy or application exit, even if not dirty to
          update the position and OOS data.  We used to call UpdateNote in the hope its quicker
          but it forgets to record notebook membership. Revist some day ....}
		procedure SaveTheNote(WeAreClosing: boolean=False);
        	{ Return a string with a title for new note "New Note 2018-01-24 14:46.11" }
        function NewNoteTitle() : ANSIString;
                 { Saves the note as text or rtf, consulting user about path and file name }
        procedure SaveNoteAs(TheExt: string);
        procedure MarkDirty();
        function CleanCaption() : ANSIString;
        procedure SetBullet(PB: TKMemoParagraph; Bullet: boolean);
        // Advises other apps we can do middle button paste
        procedure SetPrimarySelection;
        // Cancels any indication we can do middle button paste 'cos nothing is selected
        procedure UnsetPrimarySelection;
        function UpdateNote(NRec: TNoteUpdaterec): boolean;
    public
        // Set by the calling process.
        SingleNoteMode : Boolean;
        NoteFileName, NoteTitle : string;
        Dirty : boolean;
        Verbose : boolean;
        SearchedTerm : string;  // If not empty, opening is associated with a search, go straight there.
        // If a new note is a member of Notebook, this holds notebook name until first save.
        TemplateIs : AnsiString;
            { Will mark this note as ReadOnly and not to be saved because the Sync Process
              has either replaced or deleted this note OR we are using it as an internal viewer.
              Can still read and copy content. Viewer users don't need big ugly yellow warning}
        procedure SetReadOnly(ShowWarning : Boolean = True);
    end;

var
    EditBoxForm: TEditBoxForm;

// Note that the various font sizes are declared in Settings;


implementation

{$R *.lfm}

{ TEditBoxForm }
uses //RichMemoUtils,     // Provides the InsertFontText() procedure.
    LazUTF8,
    //LCLType,			// For the MessageBox
    keditcommon,        // Holds some editing defines
    settings,			// User settings and some defines used across units.
    SearchUnit,         // Is the main starting unit and the search tool.

	LoadNote,           // Will know how to load a Tomboy formatted note.
    {SyncGUI,}
    LazFileUtils,		// For ExtractFileName()
    Spelling,
    NoteBook,
    MainUnit,            // Not needed now for anything other than MainForm.Close()
    SyncUtils,          // Just for IDLooksOK()
    K_Prn,              // Custom print unit.
    Markdown,
    Index,              // An Index of current note.
    math,
    //LazFileUtils,       // ToDo : UTF8 complient, unlike FileUtil ...
    FileUtil, strutils,         // just for ExtractSimplePath ... ~#1620
    // XMLRead, DOM, XMLWrite;     // For updating locations on a clean note (May 19, not needed ?)
    LCLIntf;            // OpenUrl(


const
        LinkScanRange = 100;	// when the user changes a Note, we search +/- around
     							// this value for any links that need adjusting.

{  ---- U S E R   C L I C K   F U N C T I O N S ----- }



procedure TEditBoxForm.SpeedButtonTextClick(Sender: TObject);
begin
   PopupMenuText.PopUp;
end;

procedure TEditBoxForm.SpeedButtonToolsClick(Sender: TObject);
begin
   PopupMenuTools.PopUp;
end;

procedure TEditBoxForm.SpeedButtonSearchClick(Sender: TObject);
begin
    SearchForm.Show;
end;

procedure TEditBoxForm.SpeedButtonDeleteClick(Sender: TObject);
var
    St : string;
begin
    if KMemo1.ReadOnly then exit();
    St := CleanCaption();
   if IDYES = Application.MessageBox('Delete this Note', PChar(St),
   									MB_ICONQUESTION + MB_YESNO) then begin
		TimerSave.Enabled := False;
        if SingleNoteMode then
            DeleteFileUTF8(NoteFileName)
   		else if NoteFileName <> '' then
	   		    SearchForm.DeleteNote(NoteFileName);
        Dirty := False;
        DeletingThisNote := True;
		Close;
   end;
end;

procedure TEditBoxForm.SpeedButtonLinkClick(Sender: TObject);
var
    ThisTitle : ANSIString;
    Index : integer;
    SL : TStringList;
begin
   if KMemo1.ReadOnly then exit();
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
            SL := TStringList.Create;
            SearchForm.NoteLister.GetNotebooks(SL, ExtractFileNameOnly(NoteFileName));      // that should be just ID
            if SL.Count > 0 then
                SearchForm.OpenNote(ThisTitle, '', SL.Strings[0])
        	else SearchForm.OpenNote(ThisTitle);
            KMemo1Change(self);
            SL.Free;
		end;
	end;
end;

procedure TEditBoxForm.SpeedButtonNotebookClick(Sender: TObject);
var
    NotebookPick : TNotebookPick;
begin
    NotebookPick := TNotebookPick.Create(Application);
    NotebookPick.FullFileName := NoteFileName;
    NotebookPick.Title := NoteTitle;
    NotebookPick.ChangeMode := False;
    NotebookPick.Top := Top;
    NotebookPick.Left := Left;
    if mrOK = NotebookPick.ShowModal then MarkDirty();
    NotebookPick.Free;
end;

procedure TEditBoxForm.BulletControl(const Toggle, TurnOn : boolean);
var
      BlockNo : longint = 1;
      LastBlock,  Blar : longint;
      BulletOn : boolean;
      FirstPass : boolean = True;
begin
    if not Toggle then begin    // We'll set it all to TurnOn
        FirstPass := False;     // So its not changed
        BulletOn := not TurnOn;
    end;
    if KMemo1.ReadOnly then exit();
    MarkDirty();
    BlockNo := Kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, Blar);
    LastBlock := Kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelEnd, Blar);

    if (BlockNo = LastBlock) and (BlockNo > 1) and
        KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin
        dec(LastBlock);
        dec(BlockNo);
    end;
    // Don't change any trailing empty lines.
    while KMemo1.Blocks.Items[LastBlock].ClassNameIs('TKMemoParagraph') do
        if LastBlock > BlockNo then dec(LastBlock)
        else break;

    // OK, we are now in a TextBlock, possibly both start and end there. Must mark
    // next para as numb and then all subsquent ones until we do the one after end.
    repeat
        inc(BlockNo);
        if BlockNo >= Kmemo1.Blocks.count then	// no para after block (yet)
            Kmemo1.Blocks.AddParagraph();
        if KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin
            if FirstPass then begin
                FirstPass := False;
                BulletOn := (TKMemoParagraph(KMemo1.Blocks.Items[BlockNo]).Numbering = pnuBullets);
            end;
            SetBullet(TKMemoParagraph(KMemo1.Blocks.Items[BlockNo]), not BulletOn);
            // TKMemoParagraph(KMemo1.Blocks.Items[BlockNo]).Numbering := pnuBullets;
        end;
    until (BlockNo > LastBlock) and KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph');
end;

procedure TEditBoxForm.MenuBulletClick(Sender: TObject);
{var
      BlockNo : longint = 1;
      LastBlock,  Blar : longint;
      BulletOn : boolean;
      FirstPass : boolean = True;       }
begin
    BulletControl(True, False);
    exit();

{    if KMemo1.ReadOnly then exit();
    MarkDirty();
    BlockNo := Kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, Blar);
    LastBlock := Kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelEnd, Blar);

    if (BlockNo = LastBlock) and (BlockNo > 1) and
        KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin
        dec(LastBlock);
        dec(BlockNo);
    end;
    // Don't change any trailing empty lines.
    while KMemo1.Blocks.Items[LastBlock].ClassNameIs('TKMemoParagraph') do
        if LastBlock > BlockNo then dec(LastBlock)
        else break;

    // OK, we are now in a TextBlock, possibly both start and end there. Must mark
    // next para as numb and then all subsquent ones until we do the one after end.
    repeat
        inc(BlockNo);
        if BlockNo >= Kmemo1.Blocks.count then	// no para after block (yet)
            Kmemo1.Blocks.AddParagraph();
        if KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin
            if FirstPass then begin
                FirstPass := False;
                BulletOn := (TKMemoParagraph(KMemo1.Blocks.Items[BlockNo]).Numbering = pnuBullets);
            end;
            SetBullet(TKMemoParagraph(KMemo1.Blocks.Items[BlockNo]), not BulletOn);
            // TKMemoParagraph(KMemo1.Blocks.Items[BlockNo]).Numbering := pnuBullets;
        end;
    until (BlockNo > LastBlock) and KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph');   }
end;

procedure TEditBoxForm.KMemo1MouseDown(Sender: TObject; Button: TMouseButton;
		Shift: TShiftState; X, Y: Integer);
begin
    //{$ifdef LCLCOCOA}
    if ssCtrl in Shift then PopupMenuRightClick.popup;
    //{$else}
	if Button = mbRight then PopupMenuRightClick.PopUp;
    //{$endif}
end;


// ------------------  COPY ON SELECTION METHODS for LINUX and Windows ------

procedure TEditBoxForm.KMemo1MouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
{$IFNDEF DARWIN}    // Mac cannot do Primary Paste, ie XWindows Paste
var
    Point : TPoint;
    LinePos : TKmemoLinePosition; {$endif}
begin
    {$IFNDEF DARWIN}
    if Button = mbMiddle then begin
      Point := TPoint.Create(X, Y); // X and Y are pixels, not char positions !
      LinePos := eolEnd;
      while X > 0 do begin          // we might be right of the eol marker.
            KMemo1.PointToIndex(Point, true, true, LinePos);
            if LinePos = eolInside then break;
            dec(Point.X);
      end;
      PrimaryPaste(KMemo1.PointToIndex(Point, true, true, LinePos));
      exit();
    end;
    if KMemo1.SelAvail and
        (Kmemo1.Blocks.SelLength <> 0) then
            SetPrimarySelection()
        else
            UnsetPrimarySelection();
    {$endif}
end;

procedure TEditBoxForm.SetPrimarySelection;
var
  FormatList: Array [0..1] of TClipboardFormat;
begin
  if (PrimarySelection.OnRequest=@PrimaryCopy) then exit;
  FormatList[0] := CF_TEXT;
  try
    PrimarySelection.SetSupportedFormats(1, @FormatList[0]);
    PrimarySelection.OnRequest:=@PrimaryCopy;
  except
  end;
end;

procedure TEditBoxForm.UnsetPrimarySelection;
begin
  if PrimarySelection.OnRequest=@PrimaryCopy then
    PrimarySelection.OnRequest:=nil;
end;

procedure TEditBoxForm.PrimaryCopy(
  const RequestedFormatID: TClipboardFormat;  Data: TStream);
var
  s : string;
begin
    S := KMemo1.Blocks.SelText;
    if RequestedFormatID = CF_TEXT then
        if length(S) > 0 then
            Data.Write(s[1],length(s));
end;

procedure TEditBoxForm.PrimaryPaste(SelIndex : integer);
var
  Buff : string;
begin
    if PrimarySelection.HasFormat(CF_TEXT) then begin  // I don't know if this is useful at all.
        Buff := PrimarySelection().AsText;
        if Buff <> '' then begin
            KMemo1.Blocks.InsertPlainText(SelIndex, Buff);
            KMemo1.SelStart := SelIndex;
            Kmemo1.SelEnd := SelIndex + length(Buff);
        end;
    end;
end;

procedure TEditBoxForm.InsertDate();
var
  I : integer;
begin
    // showmessage(FormatDateTime('YYYY-MM-DD hh:mm:ss', now()));
    KMemo1.ExecuteCommand(ecInsertString, pchar(FormatDateTime(' YYYY-MM-DD hh:mm:ss ', now())));
    for I := 0 to 20 do
        KMemo1.ExecuteCommand(ecRight);
end;

{ -------------- U S E R   F O N T    C H A N G E S ----------------}

const
 ChangeSize   = 1;     // Used by AlterFont(..) and its friends.
 ChangeBold   = 2;
 ChangeItalic = 3;
 ChangeColor  = 4;
 ChangeFixedWidth = 5;
 ChangeStrikeout = 6;
 ChangeUnderline = 7;
 //DefaultFontName = 'default';             Font names determined in Settings
 //{$ifdef LINUX}
   //MonospaceFont = 'monospace';
 //{$else}
   //MonospaceFont = 'Lucida Console';
   //MonospaceFont = 'Monaco';        // might be a better choice
 //{$ifend}

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
    if KMemo1.ReadOnly then exit();
    Ready := False;
    MarkDirty();
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
procedure TEditBoxForm.AlterBlockFont(const FirstBlockNo, BlockNo : longint; const Command : integer; const NewFontSize : integer = 0);
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
		{ChangeSize :	if Block.TextStyle.Font.Size = NewFontSize then begin
						Block.TextStyle.Font.Size := Sett.FontNormal;
					end else begin
 						Block.TextStyle.Font.Size := NewFontSize;
					end;  }
        ChangeSize : Block.TextStyle.Font.Size := NewFontSize;
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
        ChangeFixedWidth :
                    if FirstBlock.TextStyle.Font.Name <> Sett.FixedFont then begin
                       Block.TextStyle.Font.Pitch := fpFixed;
                       Block.TextStyle.Font.Name := Sett.FixedFont;
                    end else begin
                       Block.TextStyle.Font.Pitch := fpVariable;
	                    Block.TextStyle.Font.Name := Sett.UsualFont;
                    end;

        ChangeStrikeout :
					if fsStrikeout in FirstBlock.TextStyle.Font.style then begin
						Block.TextStyle.Font.Style := Block.TextStyle.Font.Style - [fsStrikeout];
					end else begin
						Block.TextStyle.Font.Style := Block.TextStyle.Font.Style + [fsStrikeout];
					end;
        ChangeUnderline :
					if fsUnderline in FirstBlock.TextStyle.Font.style then begin
						Block.TextStyle.Font.Style := Block.TextStyle.Font.Style - [fsUnderline];
					end else begin
						Block.TextStyle.Font.Style := Block.TextStyle.Font.Style + [fsUnderline];
					end;
		ChangeColor :
                    if FirstBlock.TextStyle.Brush.Color <> Sett.HiColour then begin
                        Block.TextStyle.Brush.Color := Sett.HiColour;
                    end else begin
                        Block.TextStyle.Brush.Color := Sett.BackGndColour; { clDefault; }
                    end;
	end;
end;

procedure TEditBoxForm.MenuHighLightClick(Sender: TObject);
begin
    AlterFont(ChangeColor);
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

procedure TEditBoxForm.MenuItemEvaluateClick(Sender: TObject);
begin
   InitiateCalc();
end;

procedure TEditBoxForm.MenuItemExportMarkdownClick(Sender: TObject);
begin
  FormMarkDown.TheKMemo := KMemo1;
  FormMarkDown.Caption:= CleanCaption();
  FormMarkDown.Show;
end;

procedure TEditBoxForm.MenuItemIndexClick(Sender: TObject);
var
    IForm : TFormIndex;
begin
    IForm := TFormIndex.Create(Self);
    IForm.TheKMemo := KMemo1;
    IForm.Left := Left;
    IForm.Top := Top;
    IForm.ShowModal;
    if IForm.SelectedBlock >= 0 then begin
        KMemo1.SelStart := KMemo1.Blocks.BlockToIndex(KMemo1.Blocks.Items[IForm.SelectedBlock]);
        KMemo1.SelLength := 0;
    end;
    IForm.Free;
    KMemo1.SetFocus;
end;

procedure TEditBoxForm.MenuItemSettingsClick(Sender: TObject);
begin
    Sett.show;
end;

procedure TEditBoxForm.MenuUnderlineClick(Sender: TObject);
begin
    AlterFont(ChangeUnderline);
end;

procedure TEditBoxForm.MenuStrikeoutClick(Sender: TObject);
begin
        AlterFont(ChangeStrikeout);
end;

procedure TEditBoxForm.MenuFixedWidthClick(Sender: TObject);
begin
       AlterFont(ChangeFixedWidth);
end;

{ ------- S T A N D A R D    E D I T I N G    F U N C T I O N S ----- }

    // Locates if it can Term and selects it. Ret False if not found.
    // Uses regional var, LastFind to start its search from, set to 1 for new search
function TEditBoxForm.FindIt(Term : string; GoForward, CaseSensitive : boolean) : boolean;
var
    NewPos : integer = 0;
    {$ifdef WINDOWS}
    Ptr, EndP : PChar;
    {$endif}
    NumbCR : integer = 0;
begin
    Result := False;
    if GoForward then begin
        if CaseSensitive then
            NewPos := PosEx(Term, KMemo1.Blocks.Text, LastFind + 1)
        else
            NewPos := PosEx(uppercase(Term), uppercase(KMemo1.Blocks.Text), LastFind + 1);
    end else begin
        if CaseSensitive then
            NewPos := RPosEx(Term, KMemo1.Blocks.Text, LastFind)
        else
            NewPos := RPosEx(uppercase(Term), uppercase(KMemo1.Blocks.Text), LastFind);
    end;
    //Memo1.append('Pos = ' + inttostr(NewPos) + '  Found=' + inttostr(FoundPos));
    if NewPos > 0 then begin
        {$ifdef WINDOWS}                // does no harm in Unix but a bit slow ?
        Ptr := PChar(KMemo1.Blocks.text);
        EndP := Ptr + NewPos-1;
        while Ptr < EndP do begin
            if Ptr^ = #13 then inc(NumbCR);
            inc(Ptr);
        end;
        {$endif}
        KMemo1.SelStart := UTF8Length(pchar(KMemo1.Blocks.Text), NewPos-1) - NumbCR;
        LastFind := NewPos;
        KMemo1.SelLength := UTF8length(Term);
        Result := True;
    end;
end;

procedure TEditBoxForm.ButtMainTBMenuClick(Sender: TObject);
begin
    PopupMainTBMenu.Popup;
end;

procedure TEditBoxForm.ButtTBMenuClick(Sender: TObject);
begin

end;

procedure TEditBoxForm.FindDialog1Find(Sender: TObject);
begin
    FindIt(FindDialog1.FindText,
            frDown in FindDialog1.Options, frMatchCase in FindDialog1.Options);
    // If above returns false, no more to be found, but how to tell user ?
end;



procedure TEditBoxForm.FormActivate(Sender: TObject);
begin
    if SingleNoteMode then begin
        SpeedbuttonSearch.Enabled := False;
        SpeedButtonLink.Enabled := False;
        MenuItemSync.Enabled := False;
        SpeedButtonNotebook.Enabled := False;
    end;
end;

procedure TEditBoxForm.MenuItemFindClick(Sender: TObject);
begin
    LastFind := 1;
    FindDialog1.Options := FindDialog1.Options + [frHideWholeWord, frEntireScope, frDown];
	FindDialog1.Execute;
end;

procedure TEditBoxForm.MenuItemCopyClick(Sender: TObject);
begin
	KMemo1.ExecuteCommand(ecCopy);
end;

procedure TEditBoxForm.MenuItemCutClick(Sender: TObject);
begin
    if KMemo1.ReadOnly then exit();
    KMemo1.ExecuteCommand(ecCut);
    MarkDirty();
    //if not Dirty then TimerSave.Enabled := true;
    //Dirty := true;
    //Label1.Caption := 'd';
end;

procedure TEditBoxForm.MenuItemDeleteClick(Sender: TObject);
begin
    if KMemo1.ReadOnly then exit();
    // KMemo1.ExecuteCommand(ecClearSelection);
    KMemo1.Blocks.ClearSelection;
    MarkDirty();
    //if not Dirty then TimerSave.Enabled := true;
    //Dirty := true;
    //Label1.Caption := 'd';
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
     SaveExport.Filename := StringReplace(CleanCaption(), #32, '', [rfReplaceAll]) + '.' + TheExt;
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

procedure TEditBoxForm.MarkDirty();
begin
    {if not Dirty then} TimerSave.Enabled := true;
    Dirty := true;
    if Caption = '' then Caption := '*'
    else if Caption[1] <> '*' then
        Caption := '* ' + Caption;
end;


function TEditBoxForm.CleanCaption(): ANSIString;
begin
    if Caption = '' then exit('');
    if Caption[1] = '*' then
        Result := Copy(Caption, 3, 256)
    else Result := Caption;
end;

procedure TEditBoxForm.SetReadOnly(ShowWarning : Boolean = True);
begin
   if ShowWarning then PanelReadOnly.Height:= 60;
   KMemo1.ReadOnly := True;
end;

procedure TEditBoxForm.MenuItemPasteClick(Sender: TObject);
begin
    if KMemo1.ReadOnly then exit();
    Ready := False;
    KMemo1.ExecuteCommand(ecPaste);
    MarkDirty();
    Ready := True;
end;


procedure TEditBoxForm.MenuItemPrintClick(Sender: TObject);
var
    KPrint : TKprn;
begin
    if PrintDialog1.Execute then begin
      KPrint := TKPrn.Create;
      KPrint.PrintKmemo(KMemo1);
      FreeandNil(KPrint);
    end;
end;

procedure TEditBoxForm.MenuItemSelectAllClick(Sender: TObject);
begin
	KMemo1.ExecuteCommand(ecSelectAll);
end;

procedure TEditBoxForm.MenuItemSpellClick(Sender: TObject);
var
    SpellBox : TFormSpell;
begin
    if KMemo1.ReadOnly then exit();
    if Sett.SpellConfig then begin
        SpellBox := TFormSpell.Create(Application);
        // SpellBox.Top := Placement + random(Placement*2);
        // SpellBox.Left := Placement + random(Placement*2);
        SpellBox.TextToCheck:= KMemo1.Blocks.Text;
        SpellBox.TheKMemo := KMemo1;
        SpellBox.ShowModal;
    end else showmessage('Sorry, spelling not configured');
end;

procedure TEditBoxForm.MenuItemSyncClick(Sender: TObject);
begin
    if KMemo1.ReadOnly then exit();
	if Dirty then SaveTheNote();
    Sett.Synchronise();
end;

{ - - - H O U S E   K E E P I N G   F U C T I O N S ----- }

procedure TEditBoxForm.TimerSaveTimer(Sender: TObject);
begin
    TimerSave.Enabled:=False;
	// showmessage('Time is up');
    SaveTheNote();
end;

procedure TEditBoxForm.CleanUTF8();

        function BitSet(Value : byte; TheBit : integer) : boolean;      // theBit 0-7
        begin
            Result := ((Value shr TheBit) and 1) = 1;
        end;

        function CleanedUTF8(var TheText : string) : boolean;
        var cnt : integer = 1;
            NumbBytes : integer = 0;
            i : integer;
        begin
            Result := false;
            while Cnt <= TheText.Length do begin
                if BitSet(byte(TheText[cnt]), 7) then begin
                    // OK, we have a utf8 code. It will need at least one extra byte, maybe 2 or 3
                    NumbBytes := 1;
                    if BitSet(byte(TheText[cnt]), 5) then inc(NumbBytes);
                    if BitSet(byte(TheText[cnt]), 4) then inc(NumbBytes);
                    if Cnt + NumbBytes > TheText.Length then begin      // enough bytes remaining ....
                        delete(TheText, Cnt, 1);
                        Result := true;
                        continue;
                    end;
                    for i := 1 to NumbBytes do begin            // are they the right sort of bytes ?
                        if not BitSet(byte(TheText[cnt + i]), 7) then begin
                            delete(TheText, Cnt, 1);            //
                            NumbBytes := -1;                    // so the dec below does not skip a char
                            Result := true;
                            break;
                        end;
                    end;
                    Cnt := Cnt + NumbBytes;
                end;
                inc(cnt);
            end;
        end;

var
    i : integer = 0;
    AStr : string;
    TB : TKMemoTextBlock;
begin
   KMemo1.blocks.LockUpdate;
    while i < Kmemo1.blocks.count do begin
        AStr := Kmemo1.Blocks.Items[i].text;
        if KMemo1.Blocks.Items[i].ClassNameis('TKMemoTextBlock')
            or KMemo1.Blocks.Items[i].ClassNameIs('TKMemoHyperlink') then begin
                if CleanedUTF8(AStr) then begin
                    TB := KMemo1.Blocks.AddTextBlock(AStr, i);
                    TB.TextStyle.Font := TKMemoTextBlock(KMemo1.blocks.Items[i+1]).TextStyle.Font;
                    TB.TextStyle.Brush := TKMemoTextBlock(KMemo1.blocks.Items[i+1]).TextStyle.Brush;
                    KMemo1.Blocks.Delete(i+1);
                end;
        end;
        inc(i);
    end;
    KMemo1.blocks.UnLockUpdate;
end;

function TEditBoxForm.LoadSingleNote() : boolean;
var
    SLNote : TStringList;
    FileType : string;
begin
    { Here we do some checks of the file name the user put on command line.
      If the file is not present, we assume that want to make a new note by that name.
      If its a Tomboy note (and all we test for is 'xml' in first line, 'tomboy' in
      second, then proceed normally.
      Note that the rtf import is not working but it loads fine as text, rtf being the
      kmemo's underlying lang.
      If we load a Text file, I either append or change extension to .note as by
      default, it becomes a note.
    }
    Result := False;
{
    debugln('Path = [' + ExtractFilePath(NoteFileName) + ']');
    debugln('Filename = [' + ExtractFileNameOnly(NoteFileName) + ']');
    if DirectoryExistsUTF8(ExtractFilePath(NoteFileName)) then
        debugln('Dir is writable');
    debugln('New name =' + AppendPathDelim(ExtractFilePath(NoteFileName)) +
        ExtractFileNameOnly(NoteFileName) + '.note');    }

    FileType := '';
    if not FileExistsUTF8(NoteFileName) then FileType := 'new'
    else begin
          try
          SLNote := TStringList.Create;
          //try
              SlNote.LoadFromFile(NoteFileName);
              if (UTF8Pos('xml', SLNote.Strings[0]) > 0)  and
                  (UTF8Pos('tomboy', SLNote.Strings[1]) > 0) then
                      FileType := 'tomboy'
              else if (UTF8Pos('{\rtf1', SLNote.Strings[0]) > 0) then
                      FileType := 'rtf'
              else
                    if FileIsText(NoteFileName) then
                        FileType := 'text';        // Wow, thats brave !
          //except on

          //end;
          finally
            FreeAndNil(SLNote);
          end;
    end;
      if Verbose then debugln('Decided the file is of type ' + FileType);
      case FileType of
          'tomboy' : try ImportNote(NoteFileName); except on E: Exception do debugln('!!! EXCEPTION during IMPORT ' + E.Message); end;
     //     'rtf'    : KMemo1.LoadFromRTF(NoteFileName);  // Wrong, will write back there !
          'text', 'rtf'   : begin
                        try
                        KMemo1.LoadFromFile(NoteFileName);
                        CleanUTF8();
                        NoteFileName := AppendPathDelim(ExtractFilePath(NoteFileName)) +
                            ExtractFileNameOnly(NoteFileName) + '.note';

                        except on E: Exception do debugln('!!! EXCEPTION during LoadFromFile ' + E.Message);
                        end;
                     end;
          'new'    : begin
                        Result := True;
                        NoteTitle := NewNoteTitle();
                    end;
          ''       : debugln('Error, cannot identify that file type');
      end;

    if Application.HasOption('save-exit') then begin
        MarkDirty();
        NoteFileName := '';
        SaveTheNote();
        // debugln('hmm, this should be a close.');
        close;
    end;
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
    PanelReadOnly.Height := 1;
    TimerSave.Enabled := False;
    KMemo1.Font.Size := Sett.FontNormal;
    {$ifdef LINUX}
    //{$DEFINE DEBUG_CLIPBOARD}
    KMemo1.ExecuteCommand(ecPaste);         // this to deal with a "first copy" issue.
                                            // note, in singlenotemode it triggers a GTK Assertion
    //{$UNDEF DEBUG_CLIPBOARD}
    {$endif}
    Kmemo1.Clear;

    {
    // this is not the right place to enable or disable sync, it should happen in master
    // menu control stuff way back in main/search.  All menus should be the same. DRB 14/04/20202

    MenuItemSync.Enabled := (Sett.SyncRepoLocation.Caption <> rsSyncNotConfig)
            and (Sett.SyncRepoLocation.Caption <> '') and (Sett.SyncRepo.Checked);
    MenuItemSync.Enabled := MenuItemSync.Enabled or ((Sett.SyncNCURL.Caption <> rsSyncNotConfig)
            and (Sett.SyncNCURL.Caption <> '') and (Sett.SyncNC.Checked));
    }


    if SingleNoteMode then
            ItsANewNote := LoadSingleNote()    // Might not be Tomboy XML format
    else
        if NoteFileName = '' then begin		// might be a new note or a new note from Link
            if NoteTitle = '' then              // New Note
			    NoteTitle := NewNoteTitle();
            ItsANewNote := True;
	    end else begin
	        Caption := NoteFileName;
     	    ImportNote(NoteFileName);		// also sets Caption and Createdate
            if TemplateIs <> '' then begin
                NoteFilename := '';
                NoteTitle := NewNoteTitle();
                ItsANewNote := True;
		    end;
    end;
    //debugln('OK, back in EditBox.OnShow');
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
    if SearchedTerm <> '' then
        FindIt(SearchedTerm, True, False)
    else begin
        KMemo1.executecommand(ecEditorTop);
        KMemo1.ExecuteCommand(ecDown);          // DRB Playing
    end;
    KMemo1.Blocks.LockUpdate;
    {$ifdef windows}
    Color:= Sett.textcolour;
    {$endif}
    KMemo1.Colors.BkGnd:= Sett.BackGndColour;
    Kmemo1.Blocks.DefaultTextStyle.Font.Color:=Sett.TextColour;
    KMemo1.Blocks.UnLockUpdate;
end;

	{ This gets called when the TrayMenu quit entry is clicked }
    { No it does not, only when user manually closes this form. }
procedure TEditBoxForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
{var
    OutFile: TextFile; }
begin
{    debugln('^^^^^^^^^^ DANGER writing debug file Unix only editBox.pas#1292 ^^^^^^^^^^^^^');
    AssignFile(OutFile, '/home/dbannon/closelogEditFormClose.txt');
    Rewrite(OutFile);
    writeln(OutFile, 'FormClose just closing ' + self.NoteTitle);
    CloseFile(OutFile); }
    Release;
end;

procedure TEditBoxForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
{var
    OutFile: TextFile;      }
begin
{    debugln('^^^^^^^^^^ DANGER writing debug file Unix only editBox.pas#1304 ^^^^^^^^^^^^^');
    AssignFile(OutFile, '/home/dbannon/closelogEditForm.txt');
    Rewrite(OutFile);
    writeln(OutFile, 'FormCloseQuery just closing ' + self.NoteTitle);
    CloseFile(OutFile);   }
    CanClose := True;
end;

procedure TEditBoxForm.FormCreate(Sender: TObject);
begin
    if mainform.SingleNoteFileName = '' then
        SearchForm.RefreshMenus(mkAllMenu, PopupMainTBMenu)
    else SingleNoteMode := True;
    {$ifdef DARWIN}
    MenuBold.ShortCut      := KeyToShortCut(VK_B, [ssMeta]);
    MenuItalic.ShortCut    := KeyToShortCut(VK_I, [ssMeta]);
    MenuStrikeout.ShortCut := KeyToShortCut(VK_S, [ssMeta]);
    MenuHighLight.ShortCut := KeyToShortCut(VK_H, [ssAlt]);
    MenuFixedWidth.ShortCut:= KeyToShortCut(VK_T, [ssMeta]);
    MenuUnderline.ShortCut := KeyToShortCut(VK_U, [ssMeta]);
    MenuItemFind.ShortCut  := KeyToShortCut(VK_F, [ssMeta]);
    MenuItemEvaluate.ShortCut := KeyToShortCut(VK_E, [ssMeta]);
    {$endif}
    DeletingThisNote := False;
end;


// As UpdateNote does not record Notebook membership, abandon it for now.
// Maybe come back later and see if it can be patched, its probably quicker.
// Was only called on a clean note ....
function TEditBoxForm.UpdateNote(NRec : TNoteUpdaterec) : boolean;
var
    InFile, OutFile: TextFile;
    {NoteDateSt, }InString, TempName : string;
begin
  if not fileexists(NRec.FFName) then exit(false);     // if its not there, the note has just been deleted
  TempName := AppendPathDelim(Sett.NoteDirectory) + 'tmp';
  if not DirectoryExists(TempName) then
      CreateDir(AppendPathDelim(tempname));
  TempName := tempName + pathDelim + 'location.note';             //  generate a random name  ??
  AssignFile(InFile, NRec.FFName);
  AssignFile(OutFile, TempName);
  try
      try
          Reset(InFile);
          Rewrite(OutFile);
          while not eof(InFile) do begin
              readln(InFile, InString);
              if (Pos('<cursor-position>', InString) > 0) then break;
              writeln(OutFile, InString);
          end;
          // OK, we are looking atthe part we want to change, ignore infile, we know better.
          writeln(OutFile, '  <cursor-position>' + NRec.CPos + '</cursor-position>');
          writeln(OutFile, '  <selection-bound-position>1</selection-bound-position>');
          writeln(OutFile, '  <width>' + NRec.Width + '</width>');
          writeln(OutFile, '  <height>' + NRec.height + '</height>');
          writeln(OutFile, '  <x>' + NRec.X + '</x>');
          writeln(OutFile, '  <y>' + NRec.Y + '</y>');
          writeln(OutFile, '  <open-on-startup>' + NRec.OOS + '</open-on-startup>');

          //Must see if this note is in a notebook, if so, record here.

          writeln(OutFile, '</note>');
      finally
          CloseFile(OutFile);
          CloseFile(InFile);
      end;
  except
    on E: EInOutError do begin
        debugln('File handling error occurred updating clean note location. Details: ' + E.Message);
        exit(False);
    end;
  end;
  result := CopyFile(TempName, Nrec.FFName);    // wrap this in a Try
  if result = false then debugln('ERROR moving [' + TempName + '] to [' + NRec.FFName + ']');
end;

procedure TEditBoxForm.FormDestroy(Sender: TObject);
{var
    ARec : TNoteUpdateRec; }
begin
    UnsetPrimarySelection;                                      // tidy up copy on selection.
    if (length(NoteFileName) = 0) and (not Dirty) then exit;    // A new, unchanged note, no need to save.
    if not Kmemo1.ReadOnly then
        if not DeletingThisNote then
            if (not SingleNoteMode) or Dirty then       // We always save, except in SingleNoteMode (where we save only if dirty)
                SaveTheNote(Sett.AreClosing);           // Jan 2020, just call SaveTheNote, it knows how to record the notebook state
    SearchForm.NoteClosing(NoteFileName);

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

procedure TEditBoxForm.MarkTitle();
var
    BlockNo : integer = 0;
    //AtTheEnd : Boolean = False;
    EndBlock, blar : integer;
begin
  	if Not Ready then exit();
    { if there is more than one block, and the first, [0], is a para, delete it.}
    if KMemo1.Blocks.Count <= 2 then exit();	// Don't try to mark title until more blocks.
    Ready := false;
    Kmemo1.Blocks.LockUpdate;
    if Kmemo1.Blocks.Items[BlockNo].ClassName = 'TKMemoParagraph' then
          Kmemo1.Blocks.DeleteEOL(0);
	try
        while Kmemo1.Blocks.Items[BlockNo].ClassName <> 'TKMemoParagraph' do begin
            if Kmemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoTextBlock') then begin    // just possible its an image, ignore ....
                TKMemoTextBlock(Kmemo1.Blocks.Items[BlockNo]).TextStyle.Font.Size := Sett.FontTitle;
                TKMemoTextBlock(Kmemo1.Blocks.Items[BlockNo]).TextStyle.Font.Color := Sett.TitleColour;
                TKMemoTextBlock(Kmemo1.Blocks.Items[BlockNo]).TextStyle.Font.Style := [fsUnderline];
            end;
           	inc(BlockNo);
            if BlockNo >= Kmemo1.Blocks.Count then begin
                //AtTheEnd := True;
                break;
            end;
       	end;                                // Stopped at first TKMemoParagraph if it exists.
        BlocksInTitle := BlockNo;
        { Make sure user has not smeared Title charactistics to next line
          Scan back from cursor to end of title, if Title font, reset. }
        EndBlock := KMemo1.Blocks.IndexToBlockIndex(KMemo1.Selstart, Blar);
        while EndBlock > BlocksInTitle do begin
            if KMemo1.Blocks.Items[EndBlock].ClassNameIs('TKMemoTextBlock') and
                (TKMemoTextBlock(Kmemo1.Blocks.Items[EndBlock]).TextStyle.Font.Size = Sett.FontTitle) then begin
                    TKMemoTextBlock(Kmemo1.Blocks.Items[EndBlock]).TextStyle.Font.Size := Sett.FontNormal;
                    TKMemoTextBlock(Kmemo1.Blocks.Items[EndBlock]).TextStyle.Font.Color := Sett.TextColour;
                    TKMemoTextBlock(Kmemo1.Blocks.Items[EndBlock]).TextStyle.Font.Style := [];
                end;
            dec(EndBlock);
        end;
	finally
		KMemo1.Blocks.UnLockUpdate;
    	Ready := True;
	end;
end;


{ -----------  L I N K    R E L A T E D    F U N C T I O N S  ---------- }

procedure TEditBoxForm.MakeLink({const Link : ANSIString;} const Index, Len : longint);
var
	Hyperlink, HL: TKMemoHyperlink;
    TrueLink : string;
	BlockNo, BlockOffset, Blar{, i} : longint;
	// DontSplit : Boolean = false;
    // blk : TKMemoTextBlock;
begin
	// Is it already a Hyperlink ? We leave valid hyperlinks in place.
    BlockNo := KMemo1.Blocks.IndexToBlockIndex(Index, BlockOffset);
    if KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKHyperlink') then exit();
	// Is it all in the same block ?
    if BlockNo <> Kmemo1.Blocks.IndexToBlockIndex(Index + Len -1, Blar) then exit();
    TrueLink := utf8copy(Kmemo1.Blocks.Items[BlockNo].Text, BlockOffset+1, Len);
    if length(Kmemo1.Blocks.Items[BlockNo].Text) = Len {length(TrueLink)} then begin
         Kmemo1.Blocks.Delete(BlockNo);
         //writeln('Block deleted');
    end
    else  begin
        KMemo1.SelStart:= Index;
        KMemo1.SelLength:=Len;
        KMemo1.ClearSelection();
        BlockNo := KMemo1.SplitAt(Index);
        //writeln('Block Split');
    end;

	Hyperlink := TKMemoHyperlink.Create;
	// Hyperlink.Text := Link;
    Hyperlink.Text := TrueLink;
    // Hyperlink.TextStyle.Font.Color:= clRed {Sett.TitleColour};
    Hyperlink.Textstyle.StyleChanged   :=  true;
	Hyperlink.OnClick := @OnUserClickLink;
	HL := KMemo1.Blocks.AddHyperlink(Hyperlink, BlockNo);
    HL.TextStyle.Font.Color:= Sett.TitleColour;
    // Note the colour seems to get set to some standard that TK likes when added.

(*
i := BlockNo-2;
while I < (BlockNo + 10) do begin
    Blk := TKMemoTextBlock(KMemo1.Blocks.Items[i]);
    write('no=' + inttostr(i));
    if fsUnderline in Blk.TextStyle.Font.style then
        write(' Underlined ')
    else
        write(' plain ');
    writeln(' Type=' + KMemo1.Blocks.Items[i].ClassName + ' Text=' + KMemo1.Blocks.Items[i].Text);
    inc(i);
end; *)
end;



// Starts searching a string at StartAt for Term, returns 1 based offset from start of str if found, 0 if not. Like UTF8Pos(
function TEditBoxForm.RelativePos(const Term : ANSIString; const MText : PChar; StartAt : integer) : integer;
begin
  result := Pos(Term, MText+StartAt);
  if Result <> 0 then
      Result := Result + StartAt;
end;


procedure TEditBoxForm.MakeAllLinks(const PText : PChar; const Term : ANSIString; const StartScan : longint =1; EndScan : longint = 0);
var
	Offset, NumbCR   : longint;
    {$ifdef WINDOWS}
    Ptr, EndP : PChar;                  // Will generate "not used" warning in Unix
    {$endif}
begin
    Offset := RelativePos(Term, PText, StartScan);
    while Offset > 0 do begin
    	NumbCR := 0;
        {$ifdef WINDOWS}                // compensate for Windows silly two char newlines
        EndP := PText + Offset;
        while EndP > PText do begin
            if EndP^ = #13 then inc(NumbCR);
            dec(EndP);
        end;
        {$endif}
        if (PText[Offset-2] in [' ', #10, #13, ',', '.']) and
                        (PText[Offset + length(Term) -1] in [' ', #10, #13, ',', '.']) then
            MakeLink(UTF8Length(PText, Offset) -1 -NumbCR, UTF8length(Term));
        Offset := RelativePos(Term, PText, Offset + 1);
        if EndScan > 0 then
        	if Offset> EndScan then break;
    end;
end;


procedure TEditBoxForm.CheckForHTTP(const PText : pchar; const StartS, EndS : longint);

    function ValidWebLength(StartAt : integer) : integer;               // stupid !  Don't pass StartAt, use local vars, cheaper....
    var
        I : integer;
    begin
        I := 7;                                                         // '7' being length of 'http://'
        if not(PText[StartAt-2] in [' ', ',', #10, #13]) then exit(0);  // no leading whitespace
        while PText[StartAt+I] <> '.' do begin
            if (StartAt + I) > EndS then exit(0);                       // beyond our scan zone before dot
            if PText[StartAt+I] in [' ', ',', #10, #13] then exit(0);   // hit whitespace before a dot
            inc(I);
        end;
        inc(i);
        if (PText[StartAt+I] in [' ', ',', #10, #13]) then exit(0);     // the dot is at the end !
        while (not(PText[StartAt+I] in [' ', ',', #10, #13])) do begin
            if (StartAt + I) > EndS then exit(0);                       // beyond our scan zone before whitespace
            inc(I);
        end;
        if PText[StartAt+I-1] = '.' then
            Result := I
        else
            Result := I+1;
    end;

var
    http, Offset, NumbCR : integer;
    Len : integer;
    {$ifdef WINDOWS}EndP : PChar; {$endif}
begin
    OffSet := StartS;
    http := pos('http', PText+Offset);
    while (http <> 0) and ((http+Offset) < EndS) do begin
        if (copy(PText, Offset+http+4, 3) = '://') or (copy(PText, Offset+http+4, 4) = 's://') then begin
            Len := ValidWebLength(Offset+http);
            if Len > 0 then begin
                NumbCR := 0;
                {$ifdef WINDOWS}
                EndP := PText + Offset + http;
                while EndP > PText do begin
                    if EndP^ = #13 then inc(NumbCR);
                    dec(EndP);
                end;
                {$endif}
                MakeLink({copy(PText, Offset+http, Len), } UTF8Length(PText, OffSet + http)-1 -NumbCR, Len);
//    debugln('CheckForHTTP Index = ' + inttostr(UTF8Length(PText, OffSet + http)-1 -NumbCR) + ' and Len = ' + inttostr(Len));
            end;
            if len > 0 then
        end;
        inc(Offset, http+1);
        http := pos('http', PText + Offset);
    end;
end;


procedure TEditBoxForm.CheckForLinks(const StartScan : longint =1; EndScan : longint = 0);
var
    Searchterm : ANSIstring;
    Len, httpLen : longint;
//    Tick, Tock : qword;
    pText : pchar;
begin
	if not Ready then exit();
    if SingleNoteMode then exit();
    // There is a thing called KMemo1.Blocks.SelectableLength but it returns the number of characters, not bytes, much faster though
    // Note, we don't need Len if only doing http and its not whole note being checked (at startup). So, could save a bit ....
    Len := length(KMemo1.Blocks.text);              // saves 7mS by calling length() only once ! But still 8mS
    if StartScan >= Len then exit;                  // prevent crash when memo almost empty
    if EndScan > Len then EndScan := Len;
    if EndScan = 0 then
        httpLen := Len
    else  httpLen := EndScan;
    Ready := False;
	SearchForm.StartSearch();
    KMemo1.Blocks.LockUpdate;
    //Tick := gettickcount64();
    PText := PChar(lowerCase(KMemo1.Blocks.text));
    if Sett.CheckShowExtLinks.Checked then          // OK, what are we here for ?
        CheckForHTTP(PText, StartScan, httpLen);
    if Sett.ShowIntLinks then
        while SearchForm.NextNoteTitle(SearchTerm) do
            if SearchTerm <> NoteTitle then             // My tests indicate lowercase() has neglible overhead and is UTF8 ok.
                MakeAllLinks(PText, lowercase(SearchTerm), StartScan, EndScan);
    //Tock := gettickcount64();
    KMemo1.Blocks.UnLockUpdate;
    //debugln('MakeAllLinks ' + inttostr(Tock - Tick) + 'mS');
    Ready := True;
end;



procedure TEditBoxForm.ClearNearLink(const StartS, EndS : integer); //CurrentPos : longint);
var
    {BlockNo,}  Blar, StartBlock, EndBlock : longint;
    LinkText  : ANSIString;

    function ValidWebLink() : boolean;                  // returns true if LinkText is valid web address
    var
        DotSpot : integer;
        Str : String;
    begin
//writeln('Scanning for web address ' + LinkText);
        if pos(' ', LinkText) > 0 then exit(false);
        if (copy(LinkText,1, 8) <> 'https://') and (copy(LinkText, 1, 7) <> 'http://') then exit(false);
        Str := TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock-1]).Text;
        if (KMemo1.Blocks.Items[StartBlock-1].ClassName <> 'TKMemoParagraph') and
            not Str.EndsText(' ', Str) then exit(false);
        if (KMemo1.Blocks.Items[StartBlock+1].ClassName <> 'TKMemoParagraph') and
            (not TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock+1]).Text.StartsWith(' ')) then exit(false);
        DotSpot := pos('.', LinkText);
        if DotSpot = 0 then exit(false);
        if (DotSpot < 8) or (DotSpot > length(LinkText)-1) then exit(false);
        if LinkText.EndsWith('.') then exit(false);
        result := true;
        //writeln(' Valid http or https addess');
    end;

begin
    Ready := False;
    StartBlock := KMemo1.Blocks.IndexToBlockIndex(StartS, Blar);
    EndBlock := KMemo1.Blocks.IndexToBlockIndex(EndS, Blar);
    if StartBlock < 2 then StartBlock := 2;
    if EndBlock > Kmemo1.Blocks.Count then EndBlock := Kmemo1.Blocks.Count;
    KMemo1.Blocks.LockUpdate;
    try
    while StartBlock < EndBlock do begin
        if TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock]).TextStyle.Font.Size = Sett.FontTitle then begin
            inc(StartBlock);
            continue;
        end;
        if KMemo1.Blocks.Items[StartBlock].ClassNameIs('TKMemoHyperlink') then begin
            LinkText := Kmemo1.Blocks.Items[StartBlock].Text;
        	if not (SearchForm.IsThisaTitle(LinkText) or ValidWebLink()) then begin
                // Must check here if its also not a valid HTTP link.
//writeln('Removing link ' + LinkText);
                Kmemo1.Blocks.Delete(StartBlock);
        		KMemo1.Blocks.AddTextBlock(Linktext, StartBlock);
            end;
        end else begin
            // Must check here that its not been subject to the copying of a links colour and underline
            // we know its not a link and we know its not title. So, check color ...
            if TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock]).TextStyle.Font.Color = Sett.TitleColour then begin    // we set links to title colour
                TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock]).TextStyle.Font.Style
                    := TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock]).TextStyle.Font.Style - [fsUnderLine];
                TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock]).TextStyle.Font.Color := Sett.TextColour;
            end;
        end;
        inc(StartBlock);
    end;
    finally
        KMemo1.Blocks.UnlockUpdate;
        Ready := True;
    end;

(*          remove all this after a testing cycle.

    exit;



    BlockNo := KMemo1.Blocks.IndexToBlockIndex(CurrentPos, Blar);
    Ready := False;
    LinkText := Kmemo1.Blocks.Items[BlockNo].Text;              // debug
    if KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoHyperlink') then begin
        LinkText := Kmemo1.Blocks.Items[BlockNo].Text;
    	if Not SearchForm.IsThisaTitle(LinkText) then begin
        	KMemo1.Blocks.LockUpdate;                         // I don't think we should lock here.
    		Kmemo1.Blocks.Delete(BlockNo);
    		KMemo1.Blocks.AddTextBlock(Linktext, BlockNo);
        	KMemo1.Blocks.UnlockUpdate;
        end;
    end;
    BlockNo := KMemo1.Blocks.IndexToBlockIndex(CurrentPos-1, Blar);

    if KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoHyperlink') then begin
        LinkText := Kmemo1.Blocks.Items[BlockNo].Text;
        if Not SearchForm.IsThisaTitle(LinkText) then begin
        	KMemo1.Blocks.LockUpdate;
    		Kmemo1.Blocks.Delete(BlockNo);
    		KMemo1.Blocks.AddTextBlock(Linktext, BlockNo);
        	KMemo1.Blocks.UnlockUpdate;
        end;
    end;
    Ready := True;           *)
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
    if (copy(TKMemoHyperlink(Sender).Text, 1, 7) = 'http://') or
        (copy(TKMemoHyperlink(Sender).Text, 1, 8) = 'https://') then
            OpenUrl(TKMemoHyperlink(Sender).Text)
    else
	    SearchForm.OpenNote(TKMemoHyperlink(Sender).Text);
end;


procedure TEditBoxForm.DoHousekeeping();
var
    CurserPos, SelLen, StartScan, EndScan, BlockNo, Blar : longint;
    TempTitle : ANSIString;
    // TS1, TS2, TS3, TS4 : TTimeStamp;           // Temp time stamping to test speed
begin
    if KMemo1.ReadOnly then exit();
    CurserPos := KMemo1.RealSelStart;
    SelLen := KMemo1.RealSelLength;
    StartScan := CurserPos - LinkScanRange;
    if StartScan < length(Caption) then StartScan := length(Caption);
    EndScan := CurserPos + LinkScanRange;
    if EndScan > length(KMemo1.Text) then EndScan := length(KMemo1.Text);   // Danger - should be KMemo1.Blocks.Text !!!
    // TS1:=DateTimeToTimeStamp(Now);

    BlockNo := KMemo1.Blocks.IndexToBlockIndex(CurserPos, Blar);

    if ((BlocksInTitle + 10) > BlockNo) then begin
          // We don't check title if user is not close to it.
  	    MarkTitle();
  	    GetTitle(TempTitle);
        if Dirty then
            Caption := '* ' + TempTitle
        else
            Caption := TempTitle;
    end;

    // OK, if we are in the first or second (?) block, no chance of a link anyway.
    if BlockNo < 2 then begin
        if KMemo1.Blocks.Count = 0 then 		// But bad things happen if its really empty !
            KMemo1.Blocks.AddParagraph();
  	        exit();
    end;
    if Sett.ShowIntLinks or Sett.CheckShowExtLinks.Checked then begin
  	    ClearNearLink(StartScan, EndScan {CurserPos});
  	    // TS2:=DateTimeToTimeStamp(Now);
        CheckForLinks(StartScan, EndScan);
        // TS3:=DateTimeToTimeStamp(Now);
    end;
    KMemo1.SelStart := CurserPos;
    KMemo1.SelLength := SelLen;
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
end;


{ ---------------------- C A L C U L A T E    F U N C T I O N S ---------------}

procedure TEditBoxForm.ExprTan(var Result: TFPExpressionResult;
    const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := tan(x);
end;

function TEditBoxForm.DoCalculate(CalcStr : string) : string;
var
    FParser: TFPExpressionParser;
    parserResult: TFPExpressionResult;
begin
    result := '';
    if length(CalcStr) < 1 then exit('');
    if CalcStr[length(CalcStr)] = '=' then
        CalcStr := copy(CalcStr, 1, length(CalcStr)-1);
    FParser := TFPExpressionParser.Create(nil);
    try
        try
            FParser.Identifiers.AddFunction('tan', 'F', 'F', @ExprTan);
            FParser.Builtins := [bcMath];
            FParser.Expression := CalcStr;
            parserResult := FParser.Evaluate;
            case parserResult.ResultType of
                rtInteger : result := inttostr(parserResult.ResInteger);
                rtFloat : result := floattostrf(parserResult.ResFloat, ffFixed, 0, 3);
            end;
        finally
          FParser.Free;
        end;
    except on E: EExprParser do showmessage(E.Message);
    end;
end;

RESOURCESTRING
    rsUnabletoEvaluate = 'Unable to find an expression to evaluate';
// Called from a Ctrl-E, 'Equals', maybe 'Evaluate' ? Anyway, directs to appropriate
// methods.
procedure TEditBoxForm.InitiateCalc();
var
    AnsStr : string;
begin
    if Kmemo1.blocks.RealSelLength > 0 then begin
        if not ComplexCalculate(AnsStr) then exit;
        AnsStr := '=' + AnsStr;
    end
        else if not SimpleCalculate(AnsStr) then
            if not ColumnCalculate(AnsStr) then exit;
    if AnsStr = '' then
        showmessage(rsUnabletoEvaluate)
    else begin
        //debugln('KMemo1.SelStart=' + inttostr(KMemo1.SelStart) + 'KMemo1.RealSelStart=' + inttostr(KMemo1.RealSelStart));
        KMemo1.SelStart := KMemo1.Blocks.RealSelEnd;
        KMemo1.SelLength := 0;
        KMemo1.Blocks.InsertPlainText(KMemo1.SelStart, AnsStr);
        KMemo1.SelStart := KMemo1.SelStart + length(AnsStr);
        KMemo1.SelLength := 0;
        //debugln('KMemo1.SelStart=' + inttostr(KMemo1.SelStart) + 'KMemo1.RealSelStart=' + inttostr(KMemo1.RealSelStart));
    end;
end;

// Returns all text in a para, 0 says current one, 1 previous para etc ...
function TEditBoxForm.PreviousParagraphText(const Backby : integer) : string;
var
    BlockNo, StopBlockNo, Index : longint;
begin
     Result := '';
    StopBlockNo := KMemo1.NearestParagraphIndex;   // if we are on first line, '1'.
    Index := BackBy + 1;                           // we want to overshoot
    BlockNo := StopBlockNo;
    while Index > 0 do begin
        dec(BlockNo);
        dec(Index);
        if BlockNo < 1 then begin debugln('underrun1'); exit; end;  // its all empty up there ....
        while not Kmemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') do begin
            dec(BlockNo);
            if BlockNo < 1 then begin debugln('Underrun 2'); exit; end;
        end;
        if Index = 1 then StopBlockNo := BlockNo;       // almost there yet ?
    end;
    inc(BlockNo);
    while BlockNo < StopBlockNo do begin
        Result := Result + Kmemo1.Blocks.Items[BlockNo].Text;
        inc(BlockNo);
    end;
    //debugln('PREVIOUS BlockNo=' + inttostr(BlockNo) + '  StopBlockNo=' + inttostr(StopBlockNo));
end;


// Return content of paragraph that caret is within, up to caret pos.
function TEditBoxForm.ParagraphTextTrunc() : string;
var
    BlockNo, StopBlockNo, PosInBlock : longint;
begin
    Result := '';
    StopBlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelEnd, PosInBlock);
    if StopBlockNo < 0 then StopBlockNo := 0;
    BlockNo := StopBlockNo-1;
    while (BlockNo > 0) and (not Kmemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph')) do
        dec(BlockNo);
    // debugln('BlockNo=' + inttostr(BlockNo) + ' StopBlock=' + inttostr(StopBlockNo) + '  PosInBlock=' + inttostr(PosInBlock));
    if BlockNo > 0 then inc(BlockNo);
    if BlockNo < 0 then BlockNo := 0;
    if (BlockNo > StopBlockNo) then exit;
    while BlockNo < StopBlockNo do begin
        Result := Result + Kmemo1.Blocks.Items[BlockNo].Text;
        inc(BlockNo);
    end;
    if (PosInBlock > 0) then begin
        Result := Result + copy(KMemo1.Blocks.Items[BlockNo].Text, 1, PosInBlock);
    end;
end;

// Looks for a number at both begining and end of string. Ret empty ones if unsuccessful
function TEditBoxForm.FindNumbersInString(const AStr: string; out AtStart, AtEnd : string) : boolean;
var
    Index : integer = 1;
begin
    if AStr = '' then exit(false);
    AtStart := '';
    AtEnd := '';
    while Index <= length(AStr) do begin
        if AStr[Index] in ['0'..'9', '.'] then AtStart := AtStart + AStr[Index]
        else break;
        inc(Index);
    end;
    Index := length(AStr);
    while Index > 0 do begin
        if AStr[Index] in ['0'..'9', '.'] then AtEnd :=  AStr[Index] + AtEnd
        else break;
        dec(Index);
    end;
    result := (AtStart <> '') or (AtEnd <> '');
end;

// Tries to find a column of numbers above, trying to rhs, then lhs.
// if we find tow or more lines, use it.
function TEditBoxForm.ColumnCalculate(out AStr : string) : boolean;
var
    TheLine, AtStart, AtEnd, CalcStrStart, CalcStrEnd : string;
    Index : integer = 1;
    StartDone : boolean = False;
    EndDone : boolean = False;
begin
    AStr := '';
    CalcStrStart := '';
    CalcStrEnd := '';
    repeat
        TheLine := PreviousParagraphText(Index);
        FindNumbersInString(TheLine, AtStart, AtEnd);
        //debugln('Scanned string [' + TheLine + '] and found [' + AtStart + '] and [' + atEnd + ']');
        if AtStart = '' then
            if EndDone then break
            else StartDone := True;
        if AtEnd = '' then
            if StartDone then break
            else EndDone := True;
        if (AtStart <> '') and (not StartDone) then
            if CalcStrStart = '' then CalcStrStart := AtStart
            else CalcStrStart := CalcStrStart + ' + ' + AtStart;
        if (AtEnd <> '') and (not EndDone) then
            if CalcStrEnd = '' then CalcStrEnd := AtEnd
            else CalcStrEnd := CalcStrEnd + ' + ' + AtEnd;
        inc(Index);
    until (AtStart = '') and (AtEnd = '');
    if not EndDone then AStr := CalcStrEnd;
    if not StartDone then AStr := CalcStrStart;
    AStr := DoCalculate(AStr);
    Result := (AStr <> '');
end;

// Assumes that the current selection contains a complex calc expression.
function TEditBoxForm.ComplexCalculate(out AStr : string) : boolean;
var
    BlockNo, Temp : longint;
begin
    BlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelEnd-1, Temp);
    if kmemo1.blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin
        // debugln('Para cleanup in progress');
        Temp := KMemo1.SelLength;
        Kmemo1.SelStart := KMemo1.Blocks.RealSelStart;
        KMemo1.SelLength := Temp-1;
    end;
    if abs(KMemo1.SelLength) < 1 then exit(false);
   // debugln('Complex Calc [' + KMemo1.Blocks.SelText + ']');
   AStr := DoCalculate(KMemo1.Blocks.SelText);
   Result := (AStr <> '');
end;

const
    CalcChars : set of char =  ['0'..'9'] + ['^', '*', '-', '+', '/'] + ['.', '=', ' ', '(', ')'];

// acts iff char under curser or to left is an '='
function TEditBoxForm.SimpleCalculate(out AStr : string) : boolean;
var
    Index : longint;
    GotEquals : boolean = false;
begin
    Result := False;
    AStr := ParagraphTextTrunc();
    // look for equals
    while length(AStr) > 0 do begin
        if AStr[length(AStr)] = ' ' then begin
            delete(AStr, length(AStr), 1);
            continue;
        end;
        if AStr[length(AStr)] = '=' then begin
            delete(AStr, length(AStr), 1);
            GotEquals := True;
            continue;
        end;
        if not GotEquals then exit
        else break;
    end;
    // if to here, we have a string that used to start with =, lets see what else it has ?
    Index := length(AStr);
    if Index = 0 then exit;
    while AStr[Index] in CalcChars do begin
        dec(Index);
        if Index < 1 then break;
    end;
    delete(AStr, 1, Index);
    // debugln('SimpleCalc=[' + AStr + ']');
    AStr := DoCalculate(AStr);
    exit(AStr <> '');
end;

	{ Any change to the note text and this gets called. So, vital it be quick }
procedure TEditBoxForm.KMemo1Change(Sender: TObject);
begin
    if not Ready then exit();           // don't do any of this while starting up.
    //if not Dirty then TimerSave.Enabled := true;
    MarkDirty();
    TimerHouseKeeping.Enabled := False;
    TimerHouseKeeping.Enabled := True;
    // HouseKeeping is now driven by a timer;
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
    end else Leading := (TKMemoParagraph(kmemo1.blocks.Items[BlockNo-Index]).Numbering = pnuBullets);
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
        Debugln('      LeadOffset  =' + inttostr(LeadOffset));
        Debugln('      TrailOffset =' + inttostr(Trailoffset));
        Debugln('      BlockNo     =' + inttostr(BlockNo));

    end;
end;

{
procedure TEditBoxForm.CancelBullet(const BlockNo : longint; const UnderBullet : boolean);
begin
    debugln('Cancel this bullet');
    if UnderBullet then begin
            if Kmemo1.Blocks.Items[BlockNo].ClassNameis('TKMemoParagraph') then
                if TKMemoParagraph(KMemo1.Blocks.Items[BlockNo]).Numbering = pnuBullets then
                    SetBullet(TKMemoParagraph(kmemo1.blocks.Items[BlockNo]), False);
    end else
        if (BlockNo+1) < Kmemo1.Blocks.Count then
            if Kmemo1.Blocks.Items[BlockNo+1].ClassNameis('TKMemoParagraph') then begin
                if TKMemoParagraph(KMemo1.Blocks.Items[BlockNo+1]).Numbering = pnuBullets then
                    SetBullet(TKMemoParagraph(kmemo1.blocks.Items[BlockNo+1]), False);
            end;
end;
}

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
  TrailOffset,
  BlockNo,              // Will hold block number cursor is under.
  LeadOffset  : longint;
  LeadingBullet,        // The para immediatly previous to cursor is a bullet
  UnderBullet,          // We are under a Para and its a Bullet
  TrailingBullet,       // We are under Text but the block behind us is a Bullet.
  FirstChar  : boolean; // Cursor is under the first character of a line of text.
  NoBulletPara : boolean = false;
begin
    if not Ready then exit();                   // should we drop key on floor ????
    // don't let any ctrl char get through the kmemo on mac
    {$ifdef DARWIN}
    if [ssCtrl] = Shift then begin
        case Key of
             VK_1 : MenuSmallClick(Sender);
             VK_2 : MenuNormalClick(Sender);
             VK_3 : MenuLargeClick(Sender);
             VK_4 : MenuHugeClick(Sender);
        end;
        Key := 0;
        exit;
    end;
    if ([ssAlt, ssShift] = Shift) and ((Key = VK_RIGHT) or (Key = VK_LEFT)) then exit; // KMemo - extend selection one word left or right
    {$endif}
    {$ifndef DARWIN}
    // -------------- Shift -------------------
    if [ssShift] = shift then begin
        if (Key = VK_LEFT) or (Key = VK_RIGHT) then exit; // KMemo - extend selection one char left or right
    end;

    {$endif}
    // -------------- Control ------------------
    if {$ifdef Darwin}[ssMeta] = Shift {$else}[ssCtrl] = Shift{$endif} then begin
        case key of
            VK_Q : MainForm.close();
            VK_1 : MenuSmallClick(Sender);
            VK_2 : MenuNormalClick(Sender);
            VK_3 : MenuLargeClick(Sender);
            VK_4 : MenuHugeClick(Sender);
            VK_B : MenuBoldClick(Sender);
            VK_I : MenuItalicClick(Sender);
            VK_S : MenuStrikeOutClick(Sender);
            VK_T : MenuFixedWidthClick(Sender);
            VK_H : MenuHighLightClick(Sender);
            VK_U : MenuUnderLineClick(Sender);
            VK_F : MenuItemFindClick(self);
            VK_L : SpeedButtonLinkClick(Sender);
            VK_D : InsertDate();
            VK_N : SearchForm.OpenNote();      // MainForm.MMNewNoteClick(self);    ok as long as notes dir set .....
            VK_E : InitiateCalc();
            VK_F4 : close;                      // close just this note, normal saving will take place
            VK_M : begin FormMarkDown.TheKMemo := KMemo1; FormMarkDown.Show; end;
            VK_X, VK_C, VK_V, VK_Y, VK_A, VK_HOME, VK_END, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_PRIOR, VK_NEXT, VK_RETURN, VK_INSERT :
                exit;    // so key is not set to 0 on the way out, KMemo will handle
        end;
        Key := 0;    // so we don't get a ctrl key character in the text
        exit();
    end;
    // ------------- Alt (or Option in Mac) ------------------
    if [ssAlt] = Shift then begin
        case key of
                {$ifdef DARWIN}
            VK_H  : begin MenuHighLightClick(Sender); Key := 0; end; {$endif}
            VK_RIGHT : begin BulletControl(False, True); Key := 0; end;
            VK_LEFT  : begin BulletControl(False, False); Key := 0; end;
        end;
        exit();
    end;
    //if KMemo1.ReadOnly then begin Key := 0; exit(); end;
    // ------------------ Control and Shift ----------------
    if [ssCtrl, ssShift] = Shift then begin
       if key = ord('F') then begin SpeedButtonSearchClick(self); Key := 0; exit(); end;
       {$ifndef DARWIN}
       if (key = VK_RIGHT) or (Key = VK_LEFT) then exit;{$endif}            // KMemo knows how to do this, select word ...
       Key := 0;
    end;
    if Key = VK_TAB then begin
      KMemo1.InsertChar(KMemo1.Blocks.RealSelStart, #09);
      Key := 0;
      exit;
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
    MarkDirty();
    TimerHouseKeeping.Enabled := False;
    TimerHouseKeeping.Enabled := True;

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
                SetBullet(TKMemoParagraph(kmemo1.blocks.Items[BlockNo+TrailOffset]), False)
            	// TKMemoParagraph(kmemo1.blocks.Items[BlockNo+TrailOffset]).Numbering := pnuNone
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
                SetBullet(TKMemoParagraph(kmemo1.blocks.Items[BlockNo+TrailOffset]), True)
            	// TKMemoParagraph(kmemo1.blocks.Items[BlockNo+TrailOffset]).Numbering := pnuBullets;
            else DebugLn('ERROR - this case d block should be a para');
            if  kmemo1.blocks.Items[BlockNo-Leadoffset].ClassNameIs('TKMemoParagraph') then begin
            	KMemo1.Blocks.Delete(BlockNo-LeadOffset);
            	if Verbose then debugln('Case d');
        	end;
    	end;
    Ready := True;
    // most of the intevention paths through this method take ~180mS on medium powered linux laptop
end;

procedure TEditBoxForm.SetBullet(PB : TKMemoParagraph; Bullet : boolean);
var
  Index : integer;
  //Tick, Tock : qword;
begin
    // Note - do not play with the NumberingListLevel  thingos unless a bullet is set.
    // =========== WARNING - very ugly code that needs fixing =======================
    // I find that when you reset the NumberListLevel for one block, it changes every
    // block in the document !  ????
    // So, until  make sense of that, I'll scan over the whole document and set any
    // existing bullets to the proper indent.  On a large doc with lots of bullets,
    // this seems to take about 2mS on lower end laptop.
    KMemo1.Blocks.lockUpdate;
    try
        if Bullet then begin
            PB.Numbering:=pnuBullets;
            PB.NumberingListLevel.FirstIndent:=-20;
            PB.NumberingListLevel.LeftIndent:=30;
        end else begin
            if PB.Numbering <> pnuBullets then begin
                debugln('ERROR - changing indent before Bullet set');
                exit();
            end;
            PB.NumberingListLevel.FirstIndent:=0;
            PB.NumberingListLevel.LeftIndent:=0;
            PB.Numbering:=pnuNone;
            //Tick := gettickcount64();
            for Index := 0 to KMemo1.Blocks.Count-1 do
                if KMemo1.Blocks.Items[Index].ClassNameIs('TKMemoParagraph') then
                    if TKMemoParagraph(KMemo1.Blocks.Items[Index]).Numbering = pnuBullets then begin
                        TKMemoParagraph(KMemo1.Blocks.Items[Index]).NumberingListLevel.FirstIndent:=-20;
                        TKMemoParagraph(KMemo1.Blocks.Items[Index]).NumberingListLevel.LeftIndent:=30;
                    end;
            //Tock := gettickcount64();
            // showmessage('Scan to reset bullet indent '+ inttostr(Tock-Tick) + 'mS');
        end;
    finally
        KMemo1.Blocks.UnlockUpdate;
    end;
end;

	{ --- I M P O R T I N G   and   E X P O R T I N G    F U N C T I O N S  ---  }

    // Make sure position and size is appropriate.
procedure TEditBoxForm.AdjustFormPosition();
begin
    // First of all, deal with zero or neg settings
    if Top < 20 then Top := 20;
    if Left < 20 then Left := 20;
    if Width < 50 then width := 50;
    if Height < 50 then height := 50;
    // ensure we don't start with more than two thirds _beyond_ boundaries.
    // don't seem to need this, on Linux at least, new window is always within screen. Test on Windows/Mac
    {$ifdef LINUX}exit;{$endif}
    // Jan 2020, a possible problem in at least single note mode of notes beyond right hand edge of screen. bug #116
    if (Left + (Width div 3)) > Screen.Width then begin
        Left := Screen.Width - (Width div 3);
    end;
    if (Top + (Height div 3)) > Screen.Height then begin
        Top := Screen.Height - (Height div 3);
    end;
end;

procedure TEditBoxForm.ImportNote(FileName: string);
var
    Loader : TBLoadNote;
 	//T1 : qword;          // Temp time stamping to test speed
begin
    // Timing numbers below using MyRecipes on my Acer linux laptop. For local comparison only !
    //T1 := gettickcount64();
    Loader := TBLoadNote.Create();
    Loader.FontNormal:= Sett.FontNormal;
    // Loader.FontName := FontName;
    Loader.FontSize:= Sett.FontNormal;
    KMemo1.Blocks.LockUpdate;
    KMemo1.Clear;
    Loader.LoadFile(FileName, KMemo1);                        // 340mS
    KMemo1.Blocks.UnlockUpdate;                             // 370mS
    // debugln('Load Note=' + inttostr(gettickcount64() - T1) + 'mS');
    Createdate := Loader.CreateDate;
    Ready := true;
    Caption := Loader.Title;
    if Sett.ShowIntLinks or Sett.CheckShowExtLinks.checked then
    	CheckForLinks();                     		// 360mS
    Left := Loader.X;
    Top := Loader.Y;
    Height := Loader.Height;
    Width := Loader.Width;          // AdjustFormPosition() will fix if necessary
    AdjustFormPosition();
    Loader.Free;
    TimerHouseKeeping.Enabled := False;     // we have changed note but no housekeeping reqired
    // debugln('Load Note=' + inttostr(gettickcount64() - T1) + 'mS');
end;

procedure TEditBoxForm.MenuItemWriteClick(Sender: TObject);
begin
    if KMemo1.ReadOnly then exit();
    SaveTheNote();
end;

procedure TEditBoxForm.SaveTheNote(WeAreClosing : boolean = False);
var
 	Saver : TBSaveNote;
    SL : TStringList;
    OldFileName : string ='';
    Loc : TNoteUpdateRec;
    // T1, T2, T3, T4, T5, T6, T7 : dword;
    // TestI : integer;
begin
    // T1 := gettickcount64();
    // debugln('Saving this note' + Caption);
    Saver := Nil;
    if KMemo1.ReadOnly then exit();
  	if length(NoteFileName) = 0 then
        NoteFileName := Sett.NoteDirectory + GetAFilename();
    if Sett.NoteDirectory = CleanAndExpandDirectory(ExtractFilePath(NoteFileName)) then begin   // UTF8 OK
        //debugln('Working in Notes dir ' + Sett.NoteDirectory + ' = ' + CleanAndExpandDirectory(ExtractFilePath(NoteFileName)));
        if not NoteIDLooksOK(ExtractFileNameOnly(NoteFileName)) then
            if mrYes = QuestionDlg('Invalid GUID', 'Give this note a new GUID Filename (recommended) ?', mtConfirmation, [mrYes, mrNo], 0) then begin
                OldFileName := NoteFileName;
                NoteFileName := Sett.NoteDirectory + GetAFilename();
        end;
    end; //else debugln('NOT Working in Notes dir ' + Sett.NoteDirectory + ' <> ' + CleanAndExpandDirectory(extractFilePath(NoteFileName)));
    // We do not enforce the valid GUID file name rule if note is not in official Notes Dir
    if TemplateIs <> '' then begin
        SL := TStringList.Create();
        SL.Add(TemplateIs);
        SearchForm.NoteLister.SetNotebookMembership(ExtractFileNameOnly(NoteFileName) + '.note', SL);
        SL.Free;
        TemplateIs := '';
    end;
    Saver := TBSaveNote.Create();
    try
        Saver.CreateDate := CreateDate;
        if not GetTitle(Saver.Title) then exit();
        Caption := Saver.Title;
        // T2 := GetTickCount64();                   // 0mS
        KMemo1.Blocks.LockUpdate;                 // to prevent changes during read of kmemo
        try
           // debugln('about to save');
            Saver.ReadKMemo(NoteFileName, KMemo1);
            // T3 := GetTickCount64();               // 6mS
        finally
            KMemo1.Blocks.UnLockUpdate;
        end;
        // T4 := GetTickCount64();                   //  0mS
        Loc.Width:=inttostr(Width);
        Loc.Height:=inttostr(Height);
        Loc.X := inttostr(Left);
        Loc.Y := inttostr(Top);
        Loc.OOS := booltostr(WeAreClosing, True);
        Loc.CPos:='1';
        loc.FFName:='';
        if Dirty or SingleNoteMode then     // In SingeNoteMode, there is no NoteLister, so date is always updated.
            Loc.LastChangeDate:=''
        else
            Loc.LastChangeDate:= SearchForm.NoteLister.GetLastChangeDate(ExtractFileNameOnly(NoteFileName));
        // Use old DateSt if only metadata. Sets it to '' if not in list, Saver will sort it.
        if (not Saver.WriteToDisk(NoteFileName, Loc)) and (not WeAreClosing) then begin
            Showmessage('ERROR, cannot save,  please report [' + Loc.ErrorStr + ']');     // maybe some windows delete problem ??
            //Showmessage('Name=' + Sett.NoteDirectory + NoteFileName);
        end;
        // T5 := GetTickCount64();                   // 1mS
        // No point in calling UpDateList() if we are closing the app or just updating metadata.
        if ((not WeAreClosing) and (Loc.LastChangeDate = '')) then
            SearchForm.UpdateList(CleanCaption(), Saver.TimeStamp, NoteFileName, self);
                                        // if we have rewritten GUID, that will create new entry for it.
        // T6 := GetTickCount64();                   //  14mS
        if OldFileName <> '' then
            SearchForm.DeleteNote(OldFileName);
    finally
        if Saver <> Nil then Saver.Destroy;
        Dirty := false;
        Caption := CleanCaption();
    end;
    {T7 := GetTickCount64();                       // 0mS
    debugln('EditBox.SaveTheNote Timing ' + inttostr(T2 - T1) + ' ' + inttostr(T3 - T2) + ' ' + inttostr(T4 - T3) + ' ' +
            inttostr(T5 - T4) + ' ' + inttostr(T6 - T5) + ' ' + inttostr(T7 - T6));  }
end;

function TEditBoxForm.NewNoteTitle(): ANSIString;
begin
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
