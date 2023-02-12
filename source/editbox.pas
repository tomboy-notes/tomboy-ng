unit EditBox;

{   Copyright (C) 2017-2021 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
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
    2020/01/28  Do not call SearchForm.UpdateList() when we are closing a clean note.
    2020/03/11  In FormDestroy, we always save, EXCEPT if in SingleNoteMode, then only if dirty.
    2020/03/27  Don't save a new, unwritten to note, also prevent 2 saves on a Ctrl-F4
    2020/03/27  Set a cleared highlight to correct background colour.
                No longer toggle when changing font sizes, set it to what user asks.
    2020/04/01  Removed line that exited KMemo1KeyDown in readonly mode, prevented cursor keys working.
    2020/04/04  Fix for when SingleNoteMode is pointed to a zero length file.
    2020/05/12  Added Shift Click to select to click pos, #129
    2020/05/23  Do not poke SingleNoteFileName in during create, get it from Mainunit in OnCreate()
    2020/06/08  Disable main menu button in readonly mode.
    2020/08/06  Call a paste in ShowForm, even in SNM, assertion is better than no copying.
                Display external links in single note mode.
    2020/08/19  Fixed bug affecting end of weblink in single note mode.
    2020/10/22  Small bug where title markup can be smeared down several lines.
    2020/11/18  Added StayOnTop to Tools Popup Menu
    2021/01/06  Pre-load find dialog with SearchBox SearchTerm, Alt-F for find next
    2021/01/22  When activating a note from the search form, jump to first match is Term is not empty
    2021/01/25  Replace FindDialog with statusbar like system. Need shortcut keys defined.
    2021/01/27  Previous find is now Ctrl-Alt-F, next one is Alt-F
    2021/01/29  Use TB_Utils/TB_MakeFileName when exporting
    2021/01/31  Fix UTF8 issue in Find, check for hits in FindIt if NumbFindHits = 0
    2021/02/03  Enter Key based search model, Ctrl-Enter and Alt-Enter
    2021/02/05  Complete rewrite of Find in a way that also works for Windows.
    2021/02/15  Use CommonMark when exporting Markdown
    2021/02/17  Fix Mac only bug, not Ctrl to ssMeta F for the EditFind
    2021/06/25  Replaced TUpDown with 2 speedbuttons
    2021/07/06  Save now in separate thread, a few mS for medium note, 10mS for a big one
    2021/07/08  Calc now defaults LHS if same numb tokens LHS and RHS
    2021/07/11  SimpleCalc can now handle appearing after a text terminating '.'
    2021/07/17  Pickup Ctrl-N from EditFind.
    2021/07/31  Ensure a New Note appears middle of the screen.
    2021/08/07  Fixed a race condition on export MD if dirty.
    2021/08/07  Commented out some code in FormShow thats is unreachable ?? Filename and Template
    2021/08/27  Consolidated all Text menu events through one method
    2021/08/27  Can now edit multilevel bullets
    2021/10/26  User selectable date stamp format, inc Bold
    2021-12-18  Ctrl-D was not saving any selected text to Undoer
    2022/04/12  Stopped SpeedButtonLinkClick from asking about Notebooks, no idea why it was doing that.
    2022/04/14  bug #260, TKMemoHyperlink not TKHyperlink
    2022/05/05  bug #260, now clear all local links around cursor and create as necessary, may be slower !
                could, perhaps, no longer sort name list and check for length in MakeLink() ? Longer wins ?
    2022/08/28  CheckForLinks() no longer calls SearchForm.StartSearch(); eg 2000 notes faster to
                work directly off TheNoteLister.NoteList[i]^.TitleLow, 11-20mS compared to 19-41mS
    2022/09/08  Esc can (if set) close current note. #271
    2022/10/18  Extensive changes to Link system, faster and better behaviour #260 inc
    2022/10/30  In MakeLink, capture the colour early, even if risk we don't need it.
    2022/11/14  Add a Close button cos Qt5 hides to title bar buttons in Showmodal ????
    2022/12/30  Moved code that pokes search content into NoteLister down a few blocks so that
                we can be sure the note has been added to NoteLister first.
    2023/02/12  Set the default font name from Sett in OnShow(), issue #263
}


{$mode objfpc}{$H+}

{X$define LDEBUG}       // ToDo : Lots of ugly debug output around the Link code, remove when stable

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
    StdCtrls, Buttons, kmemo, LazLogger, clipbrd, lcltype,
    ComCtrls,           // required up here for copy on selection stuff.
    fpexprpars,         // for calc stuff ;
    SaveNote,      		// Knows how to save a Note to disk in Tomboy's XML
    PrintersDlgs,
    TBUndo;

type FontLimitedAttrib = record      // Used to save and restore attributes when
    Styles : TFontStyles;             // a hyperlink is created or unlinked.
    BackColour : TColor;             // Note we don't do underline here....
    Size : Integer;
end;

type
    { TEditBoxForm }

    TEditBoxForm = class(TForm)
        BitBtnCloseFind: TBitBtn;
        ButtMainTBMenu: TSpeedButton;
        EditFind: TEdit;
        KMemo1: TKMemo;
        LabelFindCount: TLabel;
        LabelFindInfo: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        MenuBold: TMenuItem;
		MenuFindPrev: TMenuItem;
        MenuItalic: TMenuItem;
        MenuHighLight: TMenuItem;
        MenuHuge: TMenuItem;
        MenuItem1: TMenuItem;
		MenuFindNext: TMenuItem;
        MenuItemExportPDF: TMenuItem;
        MenuItemBulletRight: TMenuItem;
        MenuItemBulletLeft: TMenuItem;
        MenuItemFindPrev: TMenuItem;
		MenuStayOnTop: TMenuItem;
        MenuItemSettings: TMenuItem;
        MenuItemEvaluate: TMenuItem;
        MenuItemIndex: TMenuItem;
        MenuItemExportMarkdown: TMenuItem;
        MenuItemSpell: TMenuItem;
		MenuItemExportRTF: TMenuItem;
		MenuItemExportPlainText: TMenuItem;
		MenuItemPrint: TMenuItem;
		MenuItemSelectAll: TMenuItem;
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
        Panel1: TPanel;
        PanelFind: TPanel;
        PanelReadOnly: TPanel;
        PopupMainTBMenu: TPopupMenu;
		PopupMenuRightClick: TPopupMenu;
        PopupMenuTools: TPopupMenu;
        PopupMenuText: TPopupMenu;
        PrintDialog1: TPrintDialog;
        SpeedClose: TSpeedButton;
		SpeedLeft: TSpeedButton;
		SpeedRight: TSpeedButton;
        SpeedButtonDelete: TSpeedButton;
        SpeedButtonLink: TSpeedButton;
        SpeedButtonNotebook: TSpeedButton;
        SpeedButtonSearch: TSpeedButton;
        SpeedButtonText: TSpeedButton;
        SpeedButtonTools: TSpeedButton;
        SpeedRollBack: TSpeedButton;
		TaskDialogDelete: TTaskDialog;
		TimerSave: TTimer;
        TimerHousekeeping: TTimer;
        procedure BitBtnCloseFindClick(Sender: TObject);
        procedure ButtMainTBMenuClick(Sender: TObject);
        procedure EditFindChange(Sender: TObject);
        procedure EditFindEnter(Sender: TObject);
        procedure EditFindExit(Sender: TObject);
        procedure EditFindKeyDown(Sender: TObject; var Key: Word;
            Shift: TShiftState);
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
        procedure KMemo1KeyPress(Sender: TObject; var Key: char);
        procedure KMemo1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure KMemo1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure KMemo1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure MenuItemExportPDFClick(Sender: TObject);
                                // All the Text menu items go through this event
        procedure MenuTextGeneralClick(Sender: TObject);
		procedure MenuFindPrevClick(Sender: TObject);
		procedure MenuFindNextClick(Sender: TObject);
        procedure MenuItemEvaluateClick(Sender: TObject);
        procedure MenuItemExportMarkdownClick(Sender: TObject);
        procedure MenuItemIndexClick(Sender: TObject);
        procedure MenuItemSettingsClick(Sender: TObject);
		procedure MenuStayOnTopClick(Sender: TObject);
		procedure MenuItemCopyClick(Sender: TObject);
		procedure MenuItemCutClick(Sender: TObject);
        procedure MenuItemDeleteClick(Sender: TObject);
        procedure MenuItemExportPlainTextClick(Sender: TObject);
        procedure MenuItemExportRTFClick(Sender: TObject);
                                // This is a landing spot for either Find menu click or Ctrl-G
		procedure MenuItemFindClick(Sender: TObject);
		procedure MenuItemPasteClick(Sender: TObject);
        procedure MenuItemPrintClick(Sender: TObject);
		procedure MenuItemSelectAllClick(Sender: TObject);
        procedure MenuItemSpellClick(Sender: TObject);
		procedure MenuItemSyncClick(Sender: TObject);
        procedure PanelFindEnter(Sender: TObject);
        procedure SpeedCloseClick(Sender: TObject);
		procedure SpeedLeftClick(Sender: TObject);
		procedure SpeedRightClick(Sender: TObject);
		procedure SpeedRollBackClick(Sender: TObject);
        procedure SpeedButtonDeleteClick(Sender: TObject);
                                // If user has some text selected, clean it up to make a Title, check if that Title
                                // already exits (open that note) and if not, open a new note with that Title.
        procedure SpeedButtonLinkClick(Sender: TObject);
        procedure SpeedButtonNotebookClick(Sender: TObject);
        procedure SpeedButtonSearchClick(Sender: TObject);
        procedure SpeedButtonTextClick(Sender: TObject);
        procedure SpeedButtonToolsClick(Sender: TObject);
		procedure TimerSaveTimer(Sender: TObject);
        procedure TimerHousekeepingTimer(Sender: TObject);

    private
        {$ifdef LDEBUG}TG1, TG2, TG3, TG4 : qword;{$endif}
        Use_Undoer : boolean;   // We allow user to disable Undo system, ONLY set during create.
        Undoer : TUndo_Redo;
        TitleHasChanged : boolean;
                                // a record of the cursor position before last click, used by shift click to select
        MouseDownPos : integer;
        CreateDate : string;	// Will be '' if new note
        Ready : boolean;
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
                                { If Toggle is true, sets bullets to what its currently not. Otherwise sets to TurnOn}
        procedure BulletControl(MoreBullet: boolean);
                                // Gets passed a string containing a copy of one or more kmemo paragraphs, seperated by
                                // #10. And a offset from the the start of the kmemo to the start of string.
                                // Will mark up any web addresses it finds that are not already so marked.
                                // Must start at start of para or space, must have at least one . and end with
                                // space or newline char (#10). Returns 0 or length, in bytes, of the link
        procedure CheckForHTTP(const Buff: string; const Offset: integer);
                                { Scans KMemo for bad utf8 characters, seems only used when importing RTF in SNM}
        procedure CleanUTF8();
        function ColumnCalculate(out AStr: string): boolean;
        function ComplexCalculate(out AStr: string): boolean;
        procedure DumpKMemo(WhereFrom: string);
        function ExitError(MSG: string): boolean;
        procedure ExprTan(var Result: TFPExpressionResult; const Args: TExprParameterArray);
                                { KMemo find function, starts searching one char after
                                SelIndex, rets True if it found at least one hit, selects it.
                                Each atomic operation takes about 2mS. Always case insensitive.}
        function FindInNote(Term: string; IncStartPos: integer): boolean;
                                { KMemo find function, starts searching back one char before
                                SelIndex, rets True if it found at least one hit, selects it.
                                Each atomic operation takes about 2mS. Always case insensitive. }
        function FindInNoteBack(Term: string): boolean;
                                { Triggers a new find, if IncStartPos is True starts search a one past cursor
                                and works for the speed button, shift-F3, Enter etc. Only false if call is
                                because user is typing and existing match might still be viable }
        procedure FindNew(IncStartPos: boolean);
        function FindNumbersInString(AStr: string; out AtStart, AtEnd: string): boolean;
        procedure InsertDate();
                                { Searches Buff for all occurances of Term, checks its surrounded appropriately
                                and calls MakeLink to mark it up as a local Link. Does NOT look directly at KMemo1 }
        procedure MakeAllLinks(const Buff: string; const Term: ANSIString; const BlockOffset: integer);
        function ParagraphTextTrunc(): string;
        function RelativePos(const Term: ANSIString; const MText: PChar; StartAt: integer): integer;
        function PreviousParagraphText(const Backby: integer): string;
        function RestoreLimitedAttributes(const BlockNo: TKMemoBlockIndex; var FontAtt: FontLimitedAttrib): boolean;
        function SaveLimitedAttributes(const BlockNo: TKMemoBlockIndex; out
            FontAtt: FontLimitedAttrib): boolean;
                                // This method will, at some stage, return after creating and starting
                                // a thread that normalises the xml in the list, adds footer and saves.
                                // The thread keeps going after the method returns doing above and then
                                // free-ing the List.
        function SaveStringList(const SL: TStringList; Loc: TNoteUpdateRec): boolean;
        function SimpleCalculate(out AStr: string): boolean;
		procedure ClearLinks(const StartScan : longint =0; EndScan : longint = 0);
                                { Looks around current block looking for link blocks. If invalid, 'unlinks' them.
                                Http or local links, we need to clear the colour and underline of any text nearby
                                that have been 'smeared' from user editing at the end of a link. When this happens,
                                new text appears within the link block, bad .....  }
        procedure ClearNearLink(const StartS, EndS: integer);
        function DoCalculate(CalcStr: string): string;
        procedure DoHousekeeping();
                                { Returns a UUID suitable for a file name }
        function GetAFilename() : ANSIString;
                                { This is entry to manage Links. If FullBody, is a freshly loaded note with no links
                                so we just scan and insert as necessary. Otherwise, its being called as the user
                                types, we grab text around current cursor position, remove any invalid links and
                                then scan and insert any that need be there. In all cases, we honour the use links
                                setting from Sett. }
        procedure CheckForLinks(const FullBody: boolean);
                                { Returns with the title, that is the first line of note, returns False if title is empty }
        function GetTitle(out TheTitle: ANSIString): boolean;
        procedure ImportNote(FileName : string);
        procedure InitiateCalc();
                                { Test the note to see if its Tomboy XML, RTF or Text. Ret .T. if its a new note. }
        function LoadSingleNote() : boolean;
                                { Checks that the indicated text falls in all TKMemoTextBlock(s), replaces a link
                                if new one is longer (greatly favours Web Links!). Exists early if it finds it
                                does not need to make any changes. Make a new block, TKMemoHypeLink, use saved
                                text and saved attributes. Expects invalid links to already have been removed.
                                Does not mess with an existing HTTP link.  Index and Len are char, not byte. }
		procedure MakeLink(const Index, Len: longint; const Term: string);
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
                                { A method called by this or other apps to get what we might have selected }
        procedure PrimaryCopy(const RequestedFormatID: TClipboardFormat; Data: TStream);
                                { Pastes into KMemo whatever is returned by the PrimarySelection system. }
        procedure PrimaryPaste(SelIndex: integer);
        	                    { Return a string with a title for new note "New Note 2018-01-24 14:46.11" }
        function NewNoteTitle() : ANSIString;
                                { Saves the note as text or rtf, consulting user about path and file name }
        procedure SaveNoteAs(TheExt: string);
        procedure MarkDirty();
        function CleanCaption() : ANSIString;
        procedure SetBullet(PB: TKMemoParagraph; Bullet: boolean);
                                // Advises other apps we can do middle button paste
        procedure SetPrimarySelection;
                                { Restores block at StartLink to Text, attempts to merge linktext back into both
                                the previous or next block if it can.
                                There is a problem here. If a link is edited making it invalid but the remainer
                                happens to also be a valid link, we don't get back to original if edit is reversed. }
        function UnlinkBlock(StartBlock: integer): integer;
                                // Cancels any indication we can do middle button paste 'cos nothing is selected
        procedure UnsetPrimarySelection;

        //function UpdateNote(NRec: TNoteUpdaterec): boolean;

    public
        SingleNoteFileName : string;    // Set by the calling process. FFN inc path, carefull, cli has a real global version
        SingleNoteMode : Boolean;
        NoteFileName : string;          // Will contain the full note name, path, ID and .note
        NoteTitle : string;             // only used during initial opening stage ?
        Dirty : boolean;
        Verbose : boolean;
        SearchedTerm : string;          // If not empty, opening is associated with a search, go straight there.
        HaveSeenOnActivate : boolean;   // Indicates we have run, eg, CheckForLinks at Activate

        TemplateIs : AnsiString;        // If a new note is a member of Notebook, this holds notebook name until first save.
                                { Will mark this note as ReadOnly and not to be saved because the Sync Process
                                has either replaced or deleted this note OR we are using it as an internal viewer.
                                Can still read and copy content. Viewer users don't need big ugly yellow warning}
        procedure SetReadOnly(ShowWarning : Boolean = True);
                                // Public: Call on a already open note if user has followed up a search with a double click
        procedure NewFind(Term: string);
                                { Saves the note in KMemo1, must have title but can make up a file name if needed
                                If filename is invalid, bad GUID, asks user if they want to change it (they do !)
                                WeAreClosing indicates that the whole application is closing (not just this note)
                                We always save the note on FormDestroy or application exit, even if not dirty to
                                update the position and OOS data.  We used to call UpdateNote in the hope its quicker
                                but it forgets to record notebook membership. Revist some day ....}
        procedure SaveTheNote(WeAreClosing: boolean=False);
    end;


Type

  { TSaveThread }

  TSaveThread = class(TThread)
  private

  protected
                            { Saves the contents of note in a single, seperate thread. It has had TheSL
                            already populated with xml note content, it now normalises it adds Footer
                            and writes it to disk. }
    procedure Execute; override;
  public
    TheSL : TStringList;
    TheLoc : TNoteUpdateRec;    // defined in SaveNote
    Constructor Create(CreateSuspended : boolean);
  end;


var
    EditBoxForm: TEditBoxForm;
    BusySaving : boolean;       // Indicates that the thread that saves the note has not, yet exited.

implementation

{$R *.lfm}

uses
    LazUTF8,
    //LCLType,			// For the MessageBox
    keditcommon,        // Holds some editing defines
    settings,			// User settings and some defines used across units.
    SearchUnit,         // Is the main starting unit and the search tool.
	LoadNote,           // Will know how to load a Tomboy formatted note.
    LazFileUtils,		// For ExtractFileName()
    RollBack,           // RollBack form
    Spelling,
    NoteBook,
    MainUnit,           // Not needed now for anything other than MainForm.Close()
    K_Prn,              // Custom print unit.
    commonmark,
    Index,              // An Index of current note.
    math,
    FileUtil,           // just for ExtractSimplePath ... ~#1620
    LCLIntf,            // OpenUrl()
    TB_Utils,
    Note_Lister,        // so we can get directly to note data.
    ResourceStr,        // We borrow some search related strings from searchform
    bufstream,
    notenormal,         // makes the XML look a little prettier
    LCLStrConsts,       // just for rsMBClose ?
    KMemo2PDF;
const
        LinkScanRange = 100;	// when the user changes a Note, we search +/- around
     							// this value for any links that need adjusting.

{$ifdef LDEBUG}var
  MyLogFile: TextFile;{$endif}

{ =============  T   S A V E   T H R E A D   ================== }


procedure TSaveThread.Execute;
var
    Normaliser : TNoteNormaliser;
    WBufStream : TWriteBufStream;
    FileStream : TFileStream;
begin
    Normaliser := TNoteNormaliser.Create;
    Normaliser.NormaliseList(TheSL);               // TheSL belongs to the thread.
    Normaliser.Free;
    TheSL.Add(Footer(TheLoc));
    // TWriteBufStream, TFileStream preferable to BufferedFileStream because of a lighter memory load.
    FileStream := TFileStream.Create(TheLoc.FFName, fmCreate);
    //FileStream := TFileStream.Create('/home/dbannon/savethread.note', fmCreate);
    WBufStream := TWriteBufStream.Create(FileStream, 4096);             // 4K seems about right on Linux.
    try
        try
            TheSL.SaveToStream(WBufStream);
        except on E:Exception do begin
                              Debugln('ERROR, failed to save note : ' + E.Message);
                              WBufStream.Free;
                              FileStream.Free;
                              TheSL.Free;
                          end;
        end;
    finally
        WBufStream.Free;
        FileStream.Free;
        TheSL.Free;
    end;
    BusySaving := False;
end;

constructor TSaveThread.Create(CreateSuspended: boolean);
begin
   inherited Create(CreateSuspended);
   FreeOnTerminate := True;
end;


// =============  U S E R   C L I C K   F U N C T I O N S  =====================


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
    //NBArray : TStringArray;
begin
    if KMemo1.ReadOnly then exit();
    if KMemo1.Blocks.RealSelLength > 1 then begin
        ThisTitle := KMemo1.SelText;
         // Titles must not start or end with space or contain low characters
        while ThisTitle[1] = ' ' do UTF8Delete(ThisTitle, 1, 1);
        while ThisTitle[UTF8Length(ThisTitle)] = ' ' do UTF8Delete(ThisTitle, UTF8Length(ThisTitle), 1);
        Index := Length(ThisTitle);
        while Index > 0 do begin
            if ThisTitle[Index] < ' ' then delete(ThisTitle, Index, 1);
            dec(Index);
		end;
		// showmessage('[' + KMemo1.SelText +']' + LineEnding + '[' + ThisTitle + ']' );
        // Note : There was code here that called SearchForm.NoteLister.GetNotebooks - no idea why ?? April 2022
        if UTF8Length(ThisTitle) > 1 then begin
            SearchForm.OpenNote(ThisTitle);
            KMemo1Change(self);
		end;
	end;
end;

procedure TEditBoxForm.SpeedButtonNotebookClick(Sender: TObject);
var
    NotebookPick : TNotebookPick;
begin
    // if its a new note that has been created from a template, then if the user looks at notebook list
    // here, before its saved, he does not see the notebook listed. So, we force a save to avoid confusion.
    // SaveTheNote() will clear the templateIS field when it does its stuff.
    if TemplateIs <> '' then
        SaveTheNote();
    NotebookPick := TNotebookPick.Create(Application);
    NotebookPick.TheMode := nbSetNoteBooks;
    NotebookPick.FullFileName := NoteFileName;
    NotebookPick.Title := NoteTitle;
    NotebookPick.ChangeMode := False;
    NotebookPick.Top := Top;
    NotebookPick.Left := Left;
    if mrOK = NotebookPick.ShowModal then MarkDirty();
    NotebookPick.Free;
end;

procedure TEditBoxForm.SpeedRollBackClick(Sender: TObject);
begin
    if FormRollBack.Visible then exit;          // Must not open model twice !
    SaveTheNote();
    FormRollBack.Left := left;
    FormRollBack.Top := top;
    FormRollBack.NoteFileName := NoteFileName;
    FormRollBack.ShownBy := self;
    FormRollBack.ShowModal;
end;


// ========================= B U L L E T S =====================================


// ToDo : when the text of a bullet has markup at the end, that markup is applied to bullet.

procedure TEditBoxForm.BulletControl({const Toggle,} MoreBullet : boolean);
var
      BlockNo : longint = 1;
      LastBlock,  Blar : longint;
      //PB : TKMemoParagraph;
begin
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
            // special case, when clearing a bullet, ie already BulletOne and want less. The
            // ParaBlock has some built in left offset that won't go way. A new block
            // gets its attributes (inc the offset) from nearby ones.
            // ToDo : log with TK or patch to provide an optonal parameter to AddParagraph(BlockNo); ??
(*            if (not MoreBullet)
            and (TKMemoParagraph(KMemo1.Blocks.Items[BlockNo]).Numbering = BulletOne) then begin
                KMemo1.Blocks.delete(BlockNo);
                KMemo1.Blocks.AddParagraph(BlockNo);
                debugln('BulletControl - Special Case');
            end else  *)
                SetBullet(TKMemoParagraph(KMemo1.Blocks.Items[BlockNo]), MoreBullet);
        end;
    until (BlockNo > LastBlock) and KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph');
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

procedure TEditBoxForm.SetBullet(PB : TKMemoParagraph; Bullet : boolean);
{var
   Index : integer;
   Tick, Tock : qword; }
begin
    // KMemo declares a number of Bullets/Paragraph number thingos. We map
    // BulletOne .. BulletEight to them in tb_utils. Change order/appearance there.
    // You cannot have different blocks using same bullet (ie pnuBullet,
    // pnuArrowBullet) with different indent levels. Its a KMemo thing.
    // The numbers here must match what we use in Loadnote, should be constants too.
    // Here I set the different bullet indents each and every time they are used.
    // ToDo : can I initialise the different bullet indents during startup ?

{   case PB.Numbering of
       pnuNone : debugln('TEditBoxForm.SetBullet pnuNone ' + booltostr(Bullet, True));
       BulletOne : debugln('TEditBoxForm.SetBullet pnuOne ' + booltostr(Bullet, True));
       BulletTwo : debugln('TEditBoxForm.SetBullet pnuTwo ' + booltostr(Bullet, True));
       BulletThree : debugln('TEditBoxForm.SetBullet pnuThree ' + booltostr(Bullet, True));
       BulletFour : debugln('TEditBoxForm.SetBullet pnuFour ' + booltostr(Bullet, True));
       BulletFive : debugln('TEditBoxForm.SetBullet pnuFive ' + booltostr(Bullet, True));
       BulletSix : debugln('TEditBoxForm.SetBullet pnuSix ' + booltostr(Bullet, True));
   end;      }

   KMemo1.Blocks.lockUpdate;
    try
            case PB.Numbering of
                pnuNone :   if Bullet then begin
                                PB.Numbering := BulletOne;
                                PB.NumberingListLevel.FirstIndent:=-20;
                                PB.NumberingListLevel.LeftIndent := 30;
                            end;
                BulletOne : begin
                                PB.Numbering:=pnuNone;
                                PB.ParaStyle.NumberingListLevel := -1;
                                if Bullet then begin
                                    PB.Numbering := BulletTwo;
                                    PB.NumberingListLevel.FirstIndent:=-20;
                                    PB.NumberingListLevel.LeftIndent := 50;
                                end;
                            end;
                BulletTwo : begin
                                PB.Numbering:=pnuNone;
                                if Bullet then begin
                                    PB.Numbering := BulletThree;
                                    PB.NumberingListLevel.FirstIndent:=-20;
                                    PB.NumberingListLevel.LeftIndent := 70;
                                end else PB.Numbering := BulletOne;
                            end;
                BulletThree : begin
                                    PB.Numbering:=pnuNone;
                                    if Bullet then begin
                                        PB.Numbering := BulletFour;
                                        PB.NumberingListLevel.FirstIndent:=-20;
                                        PB.NumberingListLevel.LeftIndent := 90;
                                    end else PB.Numbering := BulletTwo;
                              end;
                BulletFour : begin
                                    PB.Numbering:=pnuNone;
                                    if Bullet then begin
                                        PB.Numbering := BulletFive;
                                        PB.NumberingListLevel.FirstIndent:=-20;
                                        PB.NumberingListLevel.LeftIndent := 110;
                                    end else PB.Numbering := BulletThree;
                             end;
                BulletFive : begin
                                    PB.Numbering:=pnuNone;
                                    if Bullet then begin
                                        PB.Numbering := BulletSix;
                                        PB.NumberingListLevel.FirstIndent:=-20;
                                        PB.NumberingListLevel.LeftIndent := 130;
                                    end else PB.Numbering := BulletFour;
                             end;
                BulletSix :  begin
                                    PB.Numbering:=pnuNone;
                                    if Bullet then begin
                                        PB.Numbering := BulletSeven;
                                        PB.NumberingListLevel.FirstIndent:=-20;
                                        PB.NumberingListLevel.LeftIndent := 150;
                                    end else PB.Numbering := BulletFive;
                             end;
                BulletSeven : begin
                                    PB.Numbering:=pnuNone;
                                    if Bullet then begin
                                        PB.Numbering := BulletEight;
                                        PB.NumberingListLevel.FirstIndent:=-20;
                                        PB.NumberingListLevel.LeftIndent := 170;
                                    end else PB.Numbering := BulletSix;
                              end;
                BulletEight : if not Bullet then begin
                                        PB.Numbering:=pnuNone;
                                        PB.Numbering := BulletSeven;
                              end;
                end;                // end of case statement
    finally
        KMemo1.Blocks.UnlockUpdate;
    end;
end;


// ===============  COPY ON SELECTION METHODS for LINUX and Windows ============


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
    if (Button = mbLeft) and ([ssShift] = Shift) then begin
      //debugln('Start ' + dbgs(MouseDownPos) + ' to ' + dbgs(KMemo1.CaretPos));
      KMemo1.SelStart := MouseDownPos;
      KMemo1.SelEnd := KMemo1.CaretPos;
    end;
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
    // A primary paste will always have new content, never overwrites anything.
    if PrimarySelection.HasFormat(CF_TEXT) then begin  // I don't know if this is useful at all.
        Buff := PrimarySelection().AsText;
        if Buff <> '' then begin
            Undoer.AddTextInsert(SelIndex, Buff);
            KMemo1.Blocks.InsertPlainText(SelIndex, Buff);
            KMemo1.SelStart := SelIndex;
            Kmemo1.SelEnd := SelIndex + length(Buff);
        end;
    end;
    //DumpKMemo('After Primary Paste');
    { There is an issue when pasting multiline text, it possiblt relates to direction
    of selection, fixed by commenting out, at #13005 kmemo.pas, TKMemoBlocks.InsertString() -
                        if not (Block is TKMemoContainer) then
                            NormalToEOL(LocalIndex);
    I am sure that commenting out is not appropriate, leaving my working copy
    like that until I can workout what effect it has.
    Seem to note some surprising values for LocalIndex, related somehow.
    }
end;

{ we insert datestring at selstart and optionall mark it small / italics
}
procedure TEditBoxForm.InsertDate();
var
  I : integer;
  Buff : string;
begin
    Buff := TB_DateStamp(Sett.ComboDateFormat.ItemIndex);
    //Undoer.RecordInitial(0);
    Undoer.AddTextInsert(KMemo1.Blocks.SelStart, Buff, True);
    KMemo1.ExecuteCommand(ecInsertString, pchar(Buff));
    if Sett.CheckStampItalics.checked or sett.CheckStampSmall.Checked then begin
        KMemo1.SelStart := Kmemo1.SelStart + 1;
        KMemo1.Sellength := Buff.Length-2;      // we do not want to get the spaces.
        if Sett.CheckStampItalics.checked then
            AlterFont(3);
        if Sett.CheckStampBold.checked then
            AlterFont(2);
        if Sett.CheckStampSmall.checked then
            AlterFont(1, Sett.FontSmall);
        KMemo1.SelStart := Kmemo1.SelStart -1 + Buff.Length;
        KMemo1.Sellength := 0;
    end else
        for I := 0 to Buff.Length-1 do
            KMemo1.ExecuteCommand(ecRight);        // move cursor
end;


// =================== U S E R   F O N T    C H A N G E S ======================


const
 ChangeSize   = 1;     // Used by AlterFont(..) and its friends.
 ChangeBold   = 2;
 ChangeItalic = 3;
 ChangeColor  = 4;
 ChangeFixedWidth = 5;
 ChangeStrikeout = 6;
 ChangeUnderline = 7;


{ This function will set font size, Bold or Italic or Color depending on the
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
    if Use_Undoer then Undoer.RecordInitial(0);
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
    if Use_Undoer then Undoer.AddMarkup();
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

// A method for responding to all text menu clicks. Could extend it to whole lot more ...
procedure TEditBoxForm.MenuTextGeneralClick(Sender: TObject);
begin
    case TMenuItem(sender).Name of
        'MenuItemBulletRight' : BulletControl(True);
        'MenuItemBulletLeft'  : BulletControl(False);
        'MenuHighLight'       : AlterFont(ChangeColor);
        'MenuLarge'           : AlterFont(ChangeSize, Sett.FontLarge);  // Note, fonts won't toggle !
        'MenuNormal'          : AlterFont(ChangeSize, Sett.FontNormal);
        'MenuSmall'           : AlterFont(ChangeSize, Sett.FontSmall);
        'MenuHuge'            : AlterFont(ChangeSize, Sett.FontHuge);
        'MenuBold'            : AlterFont(ChangeBold);
        'MenuItalic'          : AlterFont(ChangeItalic);
        'MenuUnderline'       : AlterFont(ChangeUnderline);
        'MenuStrikeout'       : AlterFont(ChangeStrikeout);
        'MenuFixedWidth'      : AlterFont(ChangeFixedWidth);
    end;
end;

procedure TEditBoxForm.MenuItemIndexClick(Sender: TObject);
var
    IForm : TFormIndex;
begin
    IForm := TFormIndex.Create(Self);
    IForm.ModalResult := mrNone;
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

procedure TEditBoxForm.MenuStayOnTopClick(Sender: TObject);
begin
    if MenuStayOnTop.Checked then begin
        FormStyle := fsNormal;
        MenuStayOnTop.Checked := false;
	end else begin
        FormStyle := fsSystemStayOnTop;
        MenuStayOnTop.Checked := true;
	end;
end;

procedure TEditBoxForm.FormActivate(Sender: TObject);
{$ifdef LDEBUG}var
    Tick, Tock : integer; {$endif}
begin
    if not HaveSeenOnActivate then begin;       // Only the first Activate
        Ready := False;
        {$ifdef LDEBUG}Tick := gettickcount64();{$endif}
        CheckForLinks(True);
        {$ifdef LDEBUG}
        Tock := gettickcount64();
        debugln('+++++++++++ OnActivate CheckForLinks() ' + inttostr(Tock - Tick) + 'mS' + ' HaveSeen=' + booltostr(HaveSeenOnActivate, true));
        {$endif}
        TimerHouseKeeping.Enabled := False;
        if SingleNoteMode then begin
            SpeedbuttonSearch.Enabled := False;
            SpeedButtonLink.Enabled := False;
            MenuItemSync.Enabled := False;
            SpeedButtonNotebook.Enabled := False;
        end;
        HaveSeenOnActivate := True;             // ToDo : a regional, convert to typed const
        Ready := True;
    end;
end;


// --------------------- LOCAL   F I N D    M E T H O D S -------------------------

{ Overview of local find process (local is note is 'Find', Searching all notes is 'Seach') :
  Works when the PanelFind is visible or not, gets called at note open if a search is underway.

  New Model, September 2022. SWYT, or maybe FWYT (Find While You Type)

  We select the found text but return focus back to the searchbox (so text is not
  overwritten). Tab, Ctrl-F (conditional) and closing Search Window moves focus to
  kmemo AND (??) deselects text leaving cursor at start of found item.

  Enter in the find box finds !

  Ctrl-N must still make a new note.

  New system is much more Atomic, each event is self contained. User may be typing
  in EditSearch, each keypress will trigger a forward Find. Two small speed buttons
  back and forward just pass contents of edit to appropriate find method. No state
  and no total no. hits for current search term.

  When a user is typing, we should first try and match the word the cursor is on,
  only moving ahead if user presses Enter, the Right Button, shift-F3, ctrl-shift-G

}

const SearchPanelHeight = 39;

procedure TEditBoxForm.PanelFindEnter(Sender: TObject);
begin
    EditFind.SetFocus;
end;

procedure TEditBoxForm.SpeedCloseClick(Sender: TObject);
begin
    close;
end;

procedure TEditBoxForm.MenuFindNextClick(Sender: TObject);
begin
    SpeedRightClick(Self);
end;

procedure TEditBoxForm.MenuFindPrevClick(Sender: TObject);
begin
   SpeedLeftClick(Self);
end;

procedure TEditBoxForm.NewFind(Term : string);      // Public, called from SearchForm
begin
    EditFind.Text := Term;
    FindInNote(Term, 0);                            // If we don't find it, no warning !
end;

procedure TEditBoxForm.MenuItemFindClick(Sender: TObject);
// works in two modes, Toggle or Always _activate_ Find
begin
    if PanelFind.Height > 5 then begin
        if Sett.CheckFindToggles.Checked then begin
            PanelFind.Height := 1;                            // Hide it
            Kmemo1.SetFocus;
        end else begin
            EditFind.SetFocus;
            exit;
        end;
    end  else  begin
        PanelFind.Height := SearchPanelHeight;
        if KMemo1.RealSelLength > 0 then
            EditFind.Text := KMemo1.SelText;
        LabelFindInfo.Caption := {$ifdef DARWIN}
                rsFindNavRightHintMac + ', ' + rsFindNavLeftHintMac
                {$else}rsFindNavRightHint + ', ' + rsFindNavLeftHint{$endif};
        EditFind.SetFocus;
    end;
end;

procedure TEditBoxForm.BitBtnCloseFindClick(Sender: TObject);
begin
   PanelFind.Height := 1;
   Kmemo1.SetFocus;
end;

procedure TEditBoxForm.EditFindExit(Sender: TObject);

begin
	if EditFind.Text = '' then begin
        EditFind.Hint:=rsSearchHint;
        EditFind.Text := rsMenuSearch;
        EditFind.SelStart := 1;
        EditFind.SelLength := length(EditFind.Text);
    end;
    EditFind.color := clGray;
end;

procedure TEditBoxForm.EditFindChange(Sender: TObject);
begin
    FindNew(False);               // User keystrokes do not jump forward until necessary
end;

procedure TEditBoxForm.EditFindEnter(Sender: TObject);
begin
    editFind.Color:= clDefault;
end;

procedure TEditBoxForm.EditFindKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    // This needs to be a keydown else we get the trailing edge of key event that opened panel
    // Note that user typing normal characters are caught in TEditBoxForm.EditFindChange();
    if (Key = VK_RETURN) then begin                                 // Enter
        Key := 0;
        SpeedRightClick(self);
        exit;
    end;
    if Key = VK_F3 then begin                                       // F3  inc Shift
        Key := 0;
        if [ssShift] = Shift then SpeedLeftClick(self)
        else SpeedRightClick(self);
        exit;
    end;
    // ---- Control keys and ...
    if (( {$ifdef DARWIN}[ssMeta]{$else}[ssCtrl]{$endif} = Shift) ) then begin
        case Key of
          VK_F: begin
                    Key := 0;
                    if Sett.CheckFindToggles.checked then begin
                        MenuItemFindClick(Sender);
                        KMemo1.SetFocus;                       // ToDo : check this logic
                    end;
                    exit;
                end;
          VK_N: begin
                    Key := 0;
                    SearchForm.OpenNote('');
                    exit;
                end;
          VK_G: begin
                    Key := 0;
                    if [ssCtrl, ssShift] = Shift then
                        SpeedLeftClick(self)
                    else  SpeedRightClick(self);
                end;
        end;
    end;
end;


function  TEditBoxForm.ExitError(MSG : string) : boolean;
begin
   result := false;
   debugln('ERROR from tomboy-ng EditBox Unit : ' + MSG);
end;

procedure TEditBoxForm.SpeedLeftClick(Sender: TObject);      // think btPrev
// var   Res : Boolean = false;
begin
   if (EditFind.Caption = '') or (EditFind.Caption = rsMenuSearch) then exit;
   LabelFindInfo.Caption := '';
   if not FindInNoteBack(lowercase(EditFind.Text)) then
      LabelFindInfo.Caption := rsNotAvailable;
    if PanelFind.Height = SearchPanelHeight then
        EditFind.SetFocus;

(*   Res := FindIt(EditFind.Text, KMemo1.SelStart, False, False);
    if Res then LabelFindInfo.Caption := ''
    else begin
        LabelFindInfo.Caption := rsNotAvailable;    // perhaps user has deleted the only term in the note ?
        NumbFindHits := 0;
        LabelFindCount.caption := '';                       // this is set to data by GetFindHits()
    end;
    KMemo1.setfocus;   *)
end;


procedure TEditBoxForm.FindNew(IncStartPos : boolean);
var Found : boolean;
begin
    if (EditFind.Caption = '') or (EditFind.Caption = rsMenuSearch) then exit;
    LabelFindInfo.Caption := '';
    if IncStartPos then
        Found := FindInNote(lowercase(EditFind.Text), 1)
    else Found := FindInNote(lowercase(EditFind.Text), 0);
    if not Found then
       LabelFindInfo.Caption := rsNotAvailable;
    if PanelFind.Height = SearchPanelHeight then
        EditFind.SetFocus;
end;

procedure TEditBoxForm.SpeedRightClick(Sender: TObject);    // think btNext
// var   Res : Boolean = false;
begin
    FindNew(True);

(*   Res := FindIt(EditFind.Text, KMemo1.SelStart+1, true, False);
   if Res then LabelFindInfo.Caption := ''
   else begin
       LabelFindInfo.Caption := rsNotAvailable;    // perhaps user has deleted the only term in the note ?
       NumbFindHits := 0;
       LabelFindCount.caption := '';                       // this is set to data by GetFindHits()
   end;
   KMemo1.setfocus;  *)
end;

function TEditBoxForm.FindInNoteBack(Term : string) : boolean;
var
    StartingCursor, StartBlock, IndexBlock, IntIndex, Found : integer;
    LoopCount : integer = 0;
    SearchString : string = '';
begin
    Result := False;
    StartingCursor := Kmemo1.Blocks.RealSelStart;
    StartBlock := Kmemo1.Blocks.IndexToBlockIndex(StartingCursor, IntIndex);
    if not KMemo1.Blocks[StartBlock].ClassNameIs('TKMemoParagraph') then begin
        SearchString := lowercase(Kmemo1.Blocks[StartBlock].Text);
        UTF8delete(SearchString, IntIndex+1, 99999);       // Remove already searched content
    end {else if StartBlock = Kmemo1.Blocks.Count then dec(StartBlock)};
    IndexBlock := StartBlock -1;                           // and if we start at block zero ?
    if IndexBlock < 0 then
        IndexBlock := KMemo1.Blocks.Count-1;
    while true do begin
        if IndexBlock < 0 then exit(ExitError('IndexBlock < 0'));
        if IndexBlock >= Kmemo1.Blocks.Count then exit(ExitError('IndexBlock too high'));
        if KMemo1.Blocks[IndexBlock].ClassNameIs('TKMemoParagraph') or (IndexBlock = 0) then begin
            Found := UTF8RPos(Term, SearchString);         // 1 based
            if Found > 0 then begin
                Result := True;
                Kmemo1.SelStart := Kmemo1.Blocks.BlockToIndex(KMemo1.Blocks[IndexBlock]) + Found;
                Kmemo1.SelLength := UTF8Length(Term);
                Break;                                // we found one, might be only one in a loop
            end;
            SearchString := '';
        end;
        if IndexBlock = StartBlock then
            exit(Term = lowercase(Kmemo1.Blocks.SelText));                                          // looped around to where we started.
        if IndexBlock = 0 then begin
            IndexBlock := KMemo1.Blocks.Count-1;           // Possibly slow ......
            inc(LoopCount);
            if LoopCount > 1 then exit(False);              // Special case, empty note !
        end;
        if not KMemo1.Blocks[IndexBlock].ClassNameIs('TKMemoParagraph') then
            SearchString := lowercase(Kmemo1.Blocks[IndexBlock].Text) + SearchString;
        dec(IndexBlock);
    end;
end;

function TEditBoxForm.FindInNote(Term : string; IncStartPos : integer) : boolean;
var
    StartingCursor, StartBlock, IndexBlock, IntIndex, Found : integer;
    OffSets : integer = 0;   // Allow for size of any paragraphs we skip over with no hit
    SearchString : string = '';
begin
   // We always start one char after the current cursor position. Find the next
   // match and highlight it. If we get back to cursor position, its a NOFIND.
   StartingCursor := Kmemo1.Blocks.RealSelStart;
   StartBlock := Kmemo1.Blocks.IndexToBlockIndex(StartingCursor, IntIndex);
   if not KMemo1.Blocks[StartBlock].ClassNameIs('TKMemoParagraph') then begin
        SearchString := lowercase(Kmemo1.Blocks[StartBlock].Text);
        UTF8delete(SearchString, 1, IntIndex+IncStartPos);  // Remove already searched content, plus 1 if user not typing
        inc(Offsets, IncStartPos);
   end else inc(Offsets);
   // inc(Offsets);                                            // account for either the Para or the skipped char in text
   IndexBlock := StartBlock+1;                              // Next block, if not there, drop through
   if IndexBlock >= KMemo1.Blocks.Count then begin          // On very last line and block
       IndexBlock := 0;                                     // Loop around.
       Offsets := 0;
       StartingCursor := 0;
   end;
   while IndexBlock < KMemo1.Blocks.Count do begin
        if KMemo1.Blocks[IndexBlock].ClassNameIs('TKMemoParagraph') and (SearchString <> '') then begin                        // ****
            Found := UTF8Pos(Term, SearchString);           // 1 based
            if Found > 0 then begin
                Kmemo1.SelStart := StartingCursor + Offsets + Found -1;
                Kmemo1.SelLength := UTF8Length(Term);
                //exit(Kmemo1.SelStart <> StartingCursor);    // False if its the same one we started on !
                exit(True);
            end;
            OffSets += utf8Length(SearchString) + 1;        // +1 for the paragraph block                                    // ****
            SearchString := '';
        end else begin
            if not KMemo1.Blocks[IndexBlock].ClassNameIs('TKMemoParagraph') then
                SearchString += lowercase(Kmemo1.Blocks[IndexBlock].Text)
            else inc(OffSets);                              // Must allow for Para marked                              // ****
        end;
        if IndexBlock = StartBlock then begin               // Hmm, thats were we started and its still not there ?
            Exit(Term = lowercase(Kmemo1.Blocks.SelText));
        end;
        inc(IndexBlock);                                                                                                       // ****
        if IndexBlock = KMemo1.Blocks.Count then begin                                                                         // ****
            IndexBlock := 0;                                // Loop around.
            Offsets := 0;
            StartingCursor := 0;
        end;
   end;
   Result := False;            // Here because user has initiated a new Find for something that does not exist.
   KMemo1.SelStart := StartingCursor;
end;


{ ------- S T A N D A R D    E D I T I N G    F U N C T I O N S ----- }

procedure TEditBoxForm.ButtMainTBMenuClick(Sender: TObject);
begin
    PopupMainTBMenu.Popup;
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
    //LabelFindInfo.Caption := 'd';
end;

procedure TEditBoxForm.MenuItemDeleteClick(Sender: TObject);
begin
    if KMemo1.ReadOnly then exit();
    // KMemo1.ExecuteCommand(ecClearSelection);
    Undoer.AddPasteOrCut(True);
    KMemo1.Blocks.ClearSelection;
    MarkDirty();
    //if not Dirty then TimerSave.Enabled := true;
    //Dirty := true;
    //LabelFindInfo.Caption := 'd';
end;

procedure TEditBoxForm.MenuItemExportPlainTextClick(Sender: TObject);
begin
     SaveNoteAs('txt');
end;

procedure TEditBoxForm.MenuItemExportRTFClick(Sender: TObject);
begin
   SaveNoteAs('rtf');
end;

procedure TEditBoxForm.MenuItemExportMarkdownClick(Sender: TObject);
begin
    SaveNoteAs('md');
end;

procedure TEditBoxForm.MenuItemExportPDFClick(Sender: TObject);
begin
    SaveNoteAs('pdf');
end;

procedure TEditBoxForm.SaveNoteAs(TheExt : string);
var
    SaveExport : TSaveDialog;
    MDContent : TStringList;
    ExpComm   : TExportCommon;
    FName : string;
    SleepCount : integer =0;
begin
    if not BusySaving then          // In case a save has just started.
        SaveTheNote();              // This should return quickly, before save thread is finished.
    while BusySaving do begin       // So, we wait until BusySaving is clear before proceeding
        sleep(20);      // 20mS
        inc(SleepCount);
        if SleepCount > 1000 then begin               // 20 seconds ? huge note, slow hardware ??
            showmessage('Excessive delay in saving this note');
            exit;
        end;
    end;
    SaveExport := TSaveDialog.Create(self);
    SaveExport.DefaultExt := TheExt;
    if Sett.ExportPath <> '' then
        SaveExport.InitialDir := Sett.ExportPath
    else begin
        if SingleNoteMode then
            SaveExport.InitialDir := ExtractFilePath(SingleNoteFileName)
        else begin
            {$ifdef UNIX}
            SaveExport.InitialDir :=  GetEnvironmentVariable('HOME');
            {$endif}
            {$ifdef WINDOWS}
            SaveExport.InitialDir :=  GetEnvironmentVariable('HOMEPATH');
            {$endif}
        end;
    end;
    //debugln('TEditBoxForm.SaveNoteAs Filename 1 = ' + CleanCaption());
    //debugln('TEditBoxForm.SaveNoteAs Filename 2 = ' + TB_MakeFileName(CleanCaption()));
    SaveExport.Filename := TB_MakeFileName(CleanCaption());
    if SaveExport.Execute then begin
        case TheExt of
            'txt' : KMemo1.SaveToTXT(SaveExport.FileName);
            'rtf' : KMemo1.SaveToRTF(SaveExport.FileName);
            'md'  : begin
                    MDContent := TStringList.Create;
                    ExpComm := TExportCommon.Create;
                    try
                        ExpComm.NotesDir := Sett.NoteDirectory;
                        if SingleNoteMode then
                            FName := NoteFileName
                        else FName := ExtractFileNameOnly(NoteFileName);
                        if ExpComm.GetMDcontent( FName, MDContent) then
                            MDContent.SaveToFile(SaveExport.FileName)
                        else showmessage('Failed to convert to MarkDown');
                    finally
                        ExpComm.Free;
                        MDContent.Free;
                    end;
                    end;

            'pdf' : begin
                        FormKMemo2PDF.TheKMemo := KMemo1;
                        FormKMemo2PDF.DefaultFont := sett.UsualFont;
                        FormKMemo2PDF.Show;
                        FormKMemo2PDF.BringToFront;
                    end;
        end;
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
   ButtMainTBMenu.Enabled:= False;      // in helpnote mode, menu is not being updated.
end;

procedure TEditBoxForm.MenuItemPasteClick(Sender: TObject);
begin
    if KMemo1.ReadOnly then exit();
    Undoer.AddPasteOrCut();
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


// ============== H O U S E   K E E P I N G   F U C T I O N S ==================

procedure TEditBoxForm.FormCreate(Sender: TObject);
begin
    Use_Undoer := Sett.CheckUseUndo.checked;    // Note, user must close and repen if they change this setting
    if Use_Undoer then
        Undoer := TUndo_Redo.Create(KMemo1)
    else Undoer := Nil;
    SpeedClose.Caption := ' ' + copy(rsMBClose, 2, 20) + ' ';    // chop off the initial '&'
    SingleNoteFileName := MainUnit.SingleNoteFileName();
    if SingleNoteFileName = '' then
        SearchForm.RefreshMenus(mkAllMenu, PopupMainTBMenu)
    else begin
        SingleNoteMode := True;
        ButtMainTBMenu.Enabled := false;
    end;
    //PanelFind.Visible := False;
    PanelFind.Height := 1;                      // That is, hide it for now, visible a problem on Mac
    PanelFind.Caption := '';
    {$ifdef WINDOWS}PanelFind.Color := Sett.AltColour;{$endif}    // so we see black text, windows cannot change some colours !
    {$ifdef DARWIN}
    SpeedRight.Hint := rsFindNavRightHintMac;
    SpeedLeft.Hint := rsFindNavLeftHintMac;
    {$else}
    SpeedRight.Hint := rsFindNavRightHint;
    SpeedLeft.Hint := rsFindNavLeftHint;
    {$endif}
    LabelFindCount.caption := '';
    EditFind.Text := rsMenuSearch;
    {$ifdef DARWIN}
    MenuBold.ShortCut      := KeyToShortCut(VK_B, [ssMeta]);
    MenuItalic.ShortCut    := KeyToShortCut(VK_I, [ssMeta]);
    MenuStrikeout.ShortCut := KeyToShortCut(VK_S, [ssMeta]);
    MenuHighLight.ShortCut := KeyToShortCut(VK_H, [ssAlt]);
    MenuFixedWidth.ShortCut:= KeyToShortCut(VK_T, [ssMeta]);
    MenuUnderline.ShortCut := KeyToShortCut(VK_U, [ssMeta]);
    MenuItemFind.ShortCut  := KeyToShortCut(VK_F, [ssMeta]);
    MenuItemEvaluate.ShortCut := KeyToShortCut(VK_E, [ssMeta]);
    MenuFindNext.shortcut := KeyToShortCut(VK_G, [ssMeta]);
    MenuFindPrev.shortcut := KeyToShortCut(VK_G, [ssShift, ssMeta]);
    {$endif}
    DeletingThisNote := False;
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
    if Ready then exit;                         // its a "re-show" event. Already have a note loaded.
    // Ready := False;                             // But it must be false aready, it was created FALSE
    PanelReadOnly.Height := 1;
    TimerSave.Enabled := False;
    KMemo1.Font.Size := Sett.FontNormal;
    KMemo1.Font.Name := Sett.UsualFont;
    {$ifdef LCLGTK2}
    KMemo1.ExecuteCommand(ecPaste);   // this to deal with a "first copy" issue on Linux.
    // above line generates a gtk2 assertion but only in single note mode.  I suspect
    // thats because its a modal form and in normal use, this window is not modal.
    // If we don't make above call in SNM, we get the same assertion sooner or later, as soon
    // as we select some text so may as well get it over with. No need to do it in Qt5, Win, Mac
    {$endif}
    Kmemo1.Clear;
    if SingleNoteMode then
            ItsANewNote := LoadSingleNote()     // Might not be Tomboy XML format
    else
        if NoteFileName = '' then begin		    // might be a new note or a new note from Link
            if NoteTitle = '' then              // New Note
			    NoteTitle := NewNoteTitle();
            ItsANewNote := True;
	    end else begin
            Caption := NoteFileName;
     	    ImportNote(NoteFileName);		    // also sets Caption and Createdate
        end;
    if ItsANewNote then begin
        left := (screen.Width div 2) - (width div 2);
        top := (screen.Height div 2) - (height div 2);
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

    MarkTitle();
    KMemo1.SelStart := KMemo1.Text.Length;      // set curser pos to end
    KMemo1.SelEnd := Kmemo1.Text.Length;
    KMemo1.SetFocus;
    {    if SearchedTerm <> '' then begin
        //FindDialog1.FindText:= SearchedTerm;
        EditFind.Text := SearchedTerm;
        FindIt(SearchedTerm, True, False)
	end else } begin
        KMemo1.executecommand(ecEditorTop);
        KMemo1.ExecuteCommand(ecDown);
    end;
    KMemo1.Blocks.LockUpdate;
    {$ifdef windows}
    // Color:= Sett.textcolour;
    if Sett.DarkTheme then Color := Sett.BackGndColour;
    {$endif}
    PanelFind.Color := Sett.AltColour;
    Panel1.Color := Sett.AltColour;
    KMemo1.Colors.BkGnd:= Sett.BackGndColour;
    Kmemo1.Blocks.DefaultTextStyle.Font.Color  := Sett.TextColour;
    Kmemo1.Blocks.DefaultTextStyle.Brush.Color := Sett.BackGndColour;
    KMemo1.Blocks.UnLockUpdate;
    Ready := true;
    Dirty := False;
end;


    { called when user manually closes this form. }
procedure TEditBoxForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    Release;
end;

procedure TEditBoxForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
    CanClose := True;
end;

procedure TEditBoxForm.FormDestroy(Sender: TObject);
begin
    if Undoer <> Nil then Undoer.free;
    UnsetPrimarySelection;                                      // tidy up copy on selection.
    if (length(NoteFileName) = 0) and (not Dirty) then exit;    // A new, unchanged note, no need to save.
    if not Kmemo1.ReadOnly then
        if not DeletingThisNote then
            if (not SingleNoteMode) or Dirty then       // We always save, except in SingleNoteMode (where we save only if dirty)
                SaveTheNote(Sett.AreClosing);           // Jan 2020, just call SaveTheNote, it knows how to record the notebook state
    SearchForm.NoteClosing(NoteFileName);
end;

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


procedure TEditBoxForm.TimerSaveTimer(Sender: TObject);
begin
    TimerSave.Enabled:=False;
	// showmessage('Time is up');
    SaveTheNote();
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
              if SLNote.count = 0 then               // to deal with a file created, eg with touch
                  FileType := 'text'
              else
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
        close;
    end;
end;


function TEditBoxForm.GetTitle(out TheTitle : ANSIString) : boolean;
var
    BlockNo : longint = 0;
begin
    Result := False;
    TheTitle := '';
    while Kmemo1.Blocks.Items[BlockNo].ClassName <> 'TKMemoParagraph' do begin
        TheTitle := TheTitle + Kmemo1.Blocks.Items[BlockNo].Text;
       	inc(BlockNo);
        if BlockNo >= Kmemo1.Blocks.Count then break;
    end;
    if TheTitle <> '' then Result := True;
end;

procedure TEditBoxForm.MarkTitle();
var
    BlockNo : integer = 0;
    EndBlock, blar : integer;
begin
//  	if Not Ready then exit();               // ToDo : what is effect of disabling this ?
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
                break;
            end;
       	end;                                // Stopped at first TKMemoParagraph if it exists.
        BlocksInTitle := BlockNo;
        { Make sure user has not smeared Title charactistics to next line
          Scan back from cursor to end of title, if Title font, reset. }
        EndBlock := KMemo1.Blocks.IndexToBlockIndex(KMemo1.Selstart, Blar);
        while (EndBlock < 10) and (EndBlock <  (KMemo1.Blocks.Count -2)) do inc(EndBlock);    // in case user has smeared several lines down.
        while EndBlock > BlocksInTitle do begin
            if (TKMemoTextBlock(Kmemo1.Blocks.Items[EndBlock]).TextStyle.Font.Size = Sett.FontTitle) then begin
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

// This is a debug method, take care, it uses writeln and will kill Windows !
procedure TEditBoxForm.DumpKMemo(WhereFrom : string);
var
    i : integer;
begin
    Writeln('TEditBoxForm.DumpKMemo from ' + WhereFrom);
    for i := 0 to Kmemo1.Blocks.Count-1 do begin
        writeln(Inttostr(i) + ' ' + KMemo1.Blocks.Items[i].ClassName + ' = ' + KMemo1.Blocks.Items[i].Text);
        if KMemo1.Blocks.Items[i].ClassNameIs('TKMemoTextBlock')
                or KMemo1.Blocks.Items[i].ClassNameIs('TKMemoHyperlink') then
            writeln('       Bold=' + booltostr(fsBold in TKMemoTextBlock(KMemo1.Blocks.Items[i]).TextStyle.Font.Style, True));
    end;

end;


// ==============  L I N K    R E L A T E D    F U N C T I O N S  ==============


{   Kmemo1.blocks.linetext returns a UTF8 (ANSI) string that has
    newlines recorded as a two byte 194 182 marker. They show up on console
    etc as ¶ - Thus the string "180°c¶", Length 6 chars and 8 bytes is -
        1 [1]
        2 [8]
        3 [0]
        4 194
        5 176         // those two are the temperature symbol, one extra byte
        6 [c]
        7 194
        8 182         // those two are the newline, one extra byte
}

{
  Housekeeping may call CheckForLinks(FullBody);
  If FullBody, we scan over whold note setting Links where necessary.
  Else, we grab a small lump of text arround the cursor position, finishing
  on line boundaries and using its size clear any invalid links in KMemo1.
  Then we scan that lump of text for anything that looks like a link.
  Locking for all of this action is done in CheckForLinks().


  CheckForLinks(StartScan, EndScan);
        ClearNearLink(StartScan, EndScan);
        unlinks all TKMemoHyperlinks that are not valid Weblinks Unlocks
            Calls UNLinkBlock()
            UnLinkBlock deletes the TKMemoHyperLink and puts back a block containing the
            origional text, merging to surrounding text is possible if attributes match.
            Will copy some text attributes from a link and reappy them to the text
            after unlinking.

        loops over all Titles in NoteLister calling MakeAllLinks() for each one.

        Calls MakeAllLinks()
            is called for each Title in NoteLister, it searches for the Title in range
            and then checks its surrounded by deliminators. If so, calls MakeLink() on it.

        Calls CheckForHTTP()
            Scans over range looking for something that starts with http ....
            Makes a link if it finds something valid using MakeLink()


        Both call MakeLink()
            Does some sanity tests, might decide the offered local link is longer
            than an existing one and use it instead. Otherwise it
            exits if there is already a hyperlink starting there, else makes one.
            Copy the existing text attributes to the TKMemoHyperlink.


    MyRecipes, release mode, 2000 notes, 2 local links,
    Clear 91ms  Check 147mS fancy total 140mS
    New Model, October 2022, 10 or so links at top of MyRecipes,
    ~100mS if directly affecting a link, 3-5mS otherwise.
}


function TEditBoxForm.SaveLimitedAttributes(const BlockNo : TKMemoBlockIndex; out FontAtt : FontLimitedAttrib) : boolean;
begin
(*    debugln('SaveLimitedAttributes - using block ' + inttostr(BlockNo)
        + ' color=' + colortostring(TKMemoTextBlock(kmemo1.Blocks.Items[BlockNo]).TextStyle.Brush.Color)
        + ' FPcolor=' + colortostring(FPcolorToTColor(TKMemoTextBlock(kmemo1.Blocks.Items[BlockNo]).TextStyle.Brush.FPColor)));
    debugln('SaveLimitedAttributes - Next  block ' + inttostr(BlockNo+1)
        + ' color=' + colortostring(TKMemoTextBlock(kmemo1.Blocks.Items[BlockNo+1]).TextStyle.Brush.Color)
        + ' FPcolor=' + colortostring(FPcolorToTColor(TKMemoTextBlock(kmemo1.Blocks.Items[BlockNo+1]).TextStyle.Brush.FPColor)));   *)

    result := Kmemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoHyperlink')
           or Kmemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoTextBlock');
    if not result then begin                                                    // Probably unnecessary ....
        FontAtt.BackColour := KMemo1.Colors.BkGnd;
        FontAtt.Size := Sett.FontNormal;
        FontAtt.Styles := [];
        debugln('SaveLimitedAttributes - using default values');
    end;
    FontAtt.Styles := TKMemoTextBlock(kmemo1.Blocks.Items[BlockNo]).TextStyle.Font.Style;
    FontAtt.Size := TKMemoTextBlock(kmemo1.Blocks.Items[BlockNo]).TextStyle.Font.Size;
    FontAtt.BackColour := TKMemoTextBlock(kmemo1.Blocks.Items[BlockNo]).TextStyle.Brush.Color;
end;

function TEditBoxForm.RestoreLimitedAttributes(const BlockNo : TKMemoBlockIndex; var FontAtt : FontLimitedAttrib) : boolean;
begin
    result := Kmemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoHyperlink')
          or Kmemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoTextBlock');
    if not result then exit;
    if fsUnderline in TKMemoTextBlock(kmemo1.Blocks.Items[BlockNo]).TextStyle.Font.Style then
        FontAtt.Styles := FontAtt.Styles + [fsUnderline]
    else FontAtt.Styles := FontAtt.Styles - [fsUnderline];
    TKMemoTextBlock(kmemo1.Blocks.Items[BlockNo]).TextStyle.Font.Style := FontAtt.Styles;
    TKMemoTextBlock(kmemo1.Blocks.Items[BlockNo]).TextStyle.Brush.Color := FontAtt.BackColour;
    TKMemoTextBlock(kmemo1.Blocks.Items[BlockNo]).TextStyle.Font.Size := FontAtt.Size;

end;


procedure TEditBoxForm.MakeLink(const Index, Len : longint; const Term : string);              // ToDo : A lot of clean up required
var
	Hyperlink : TKMemoHyperlink;
    TrueLink, AText : string;
	BlockNoS, BlockNoE, i : integer;
    BlockOffset : integer;          // A zero based count of characters ahead of char pointed by Index
    FontAtt : FontLimitedAttrib;
begin

    if Index = 0 then exit;         // Thats this note's title, skip it !

    BlockNoE := KMemo1.Blocks.IndexToBlockIndex(Index+Len-1, BlockOffset);      // Block where proposed link Ends
    BlockNoS := KMemo1.Blocks.IndexToBlockIndex(Index, BlockOffset);            // Block where proposed link starts
    SaveLimitedAttributes(BlockNoS, FontAtt);                                   // Record the existing colours asap !
    if KMemo1.Blocks.Items[BlockNoS].ClassNameIs('TKMemoHyperLink') then begin
        AText := lowercase(KMemo1.Blocks.Items[BlockNoS].Text);
        if AText.StartsWith('http') then exit;                                  // Already checked by Clean...
        if AText = Term then exit;                                              // Already there
    end;
    //debugln('MakeLink Index=' + Index.ToString + ' Len=' + Len.ToString + ' BlockNoS=' + BlockNoS.ToString + ' BlockNoE=' + BlockNoE.ToString);
    //debugln('MakeLink S=[' + KMemo1.Blocks.Items[BlockNoS].Text + '] E=[' + KMemo1.Blocks.Items[BlockNoE].Text + ']');
    i := BlockNoS;
    while i < BlockNoE do begin
        // debugln('MakeLink a Block Content is [' + KMemo1.Blocks.Items[i].Text + ']');
        if KMemo1.Blocks.Items[i].ClassNameIs('TKMemoHyperlink') then begin     // is there a link there already ?
            if KMemo1.Blocks.Items[i].text.StartsWith('http') then exit;        // Leave existing web links alone, already checked.
            if KMemo1.Blocks.Items[i].text.Length >= Len then exit;             // Leave it alone, is already at least as long
            // debugln('MakeLink Unlinking ' + KMemo1.Blocks.Items[i].text);
            UnlinkBlock(i);                                                     // Existing shorter, we will replace
            BlockNoE := KMemo1.Blocks.IndexToBlockIndex(Index+Len-1, BlockOffset);      // Sadly, we need to start this loop again
            BlockNoS := KMemo1.Blocks.IndexToBlockIndex(Index, BlockOffset);            // and keep iterating until we have clear space
            i := BlockNoS;
        end;
        if KMemo1.Blocks.Items[i].ClassNameIs('TKMemoParagraph') then exit;     // Thats an ERROR !
        inc(i);
    end;

    //SaveLimitedAttributes(BlockNoS, FontAtt);
    TrueLink := utf8copy(Kmemo1.Blocks.Items[BlockNoS].Text, BlockOffset+1, Len);   // Thats the bit thats in first block, possibly everything
    // The below might leave a empty block. Messy to delete here but ....
    if BlockNoE > BlockNoS then begin                                           // Multiple blocks, two or more ....
        TKMemoTextBlock(Kmemo1.Blocks.Items[BlockNoS]).Text
            := copy(Kmemo1.Blocks.Items[BlockNoS].Text, 1, UTF8Length(Kmemo1.Blocks.Items[BlockNoS].Text)- UTF8Length(TrueLink));
        i := 0;
        while BlockNoE > (BlockNoS+i) do begin                                  // ie two blocks, run loop once
            TrueLink := TrueLink + Kmemo1.Blocks.Items[BlockNoS+i+1].Text;      // +1 to get next block
            inc(i);                                                             // i must be > 0 because we are multiblock
        end;                                                                    // That will get all of last block's text, probably excessive

        //debugln('MakeLink - link is split over multiple blocks. [' + TrueLink + ']');
//        if UTF8length(TrueLink) > Len then begin            // There is content in last blocks text beyond end of link.
        // The below might leave a empty block. Messy to delete here but ....
            //debugln('MakeLink - Endblock before [' + Kmemo1.Blocks.Items[BlockNoS+i].Text + '] TrueLink=[' + TrueLink + ']');
            //debugln('MakeLink Lengths -TrueLink=' + UTF8length(TrueLink).ToString + ' Len=' + Len.ToString + ' Text=' + Kmemo1.Blocks.Items[BlockNoS+i].Text.Length.ToString);
            TKMemoTextBlock(Kmemo1.Blocks.Items[BlockNoS+i]).Text               //  BlockNoS+i is last block containing some of link
                := utf8copy(Kmemo1.Blocks.Items[BlockNoS+i].Text, 1 + Kmemo1.Blocks.Items[BlockNoS+i].Text.Length - UTF8length(TrueLink) + len, 9999);
            delete(TrueLink, Len+1, 999);                                         // Get rid of that excess, +1 to start deleting after link text
            dec(i);
            while i > 1 do begin
                dec(i);
                Kmemo1.Blocks.Delete(BlockNoS+1);
            end;
//        end else Debugln('SORRY, dont know how to deal with link ending at end of block yet');   // we just let empty block happen, ???
        inc(BlockNoS);          // Assumes we have left BlockNoS in place, removing trailing text, point to spot after existing value
    end else begin                                                              // All the proposed link was in the BlockNoS
        BlockNoS := KMemo1.SplitAt(Index);
        TKMemoTextBlock(Kmemo1.Blocks.Items[BlockNoS]).Text
                := string(Kmemo1.Blocks.Items[BlockNoS].Text).Remove(0, Len);
        if Kmemo1.Blocks.Items[BlockNoS].Text = '' then
            KMemo1.blocks.Delete(BlockNoS);                                     // Link went to very end of orig block.
        if Kmemo1.Blocks.Items[BlockNoS-1].Text = '' then begin                 // Link must have started at beginning of orig block
            KMemo1.blocks.Delete(BlockNoS-1);
            dec(BlockNoS)
        end;
    end;
    // When we get to here, Link text (and any blocks completely spanned by link text) have been removed
    // and BlockNoS points to where link need be pushed into. Might have some empty text blocks ....
(*    AText := 'MakeLink ready to insert [' + TrueLink + ']';
    if KMemo1.Blocks.Items[BlockNoS-1].ClassNameIs('TKMemoTextBlock') then
        Atext := AText + ' after [' + KMemo1.Blocks.Items[BlockNoS-1].Text + ']'
    else Atext := AText + ' after a non text block';
    if KMemo1.Blocks.Items[BlockNoS].ClassNameIs('TKMemoTextBlock') then
        Atext := AText + ' before [' + KMemo1.Blocks.Items[BlockNoS].Text + ']'
    else Atext := AText + ' before a non text block';
    debugln(AText);      *)
    {$ifdef LDEBUG}TG2 := gettickcount64();{$endif}
    Hyperlink := TKMemoHyperlink.Create;
    Hyperlink.Text := TrueLink;
    Hyperlink.Textstyle.StyleChanged   :=  true;
    Hyperlink.OnClick := @OnUserClickLink;
    {HL := }KMemo1.Blocks.AddHyperlink(Hyperlink, BlockNoS);
    RestoreLimitedAttributes(BlockNoS, FontAtt);
    //debugln('MakeLink MADELINK BlockNoS=' + BlockNoS.Tostring + ' Text=[' + TrueLink +'] Att Col=' +  ColorToString(FPColortoTColor(FontAtt.FPBackColour)));
    HyperLink.Textstyle.Font.Color := Sett.LinkColour;
    {$ifdef LDEBUG}TG3 := gettickcount64();
    TG1 := TG1 + (TG3-Tg2);{$endif}
end;


// Starts searching a string at StartAt for Term, returns 1 based offset from start of str if found, 0 if not. Like UTF8Pos(
function TEditBoxForm.RelativePos(const Term : ANSIString; const MText : PChar; StartAt : integer) : integer;
begin
  result := Pos(Term, MText+StartAt);
  if Result <> 0 then
      Result := Result + StartAt;
end;

procedure TEditBoxForm.MakeAllLinks(const Buff : string; const Term : ANSIString; const BlockOffset : integer);
var
	Offset   : Integer;     // The char position of a search term in Buffer
    ByteBeforeTerm, ByteAfterTerm : integer;
begin
//    if pos('bgc 11', TheMainNoteLister.NoteList[i]^.TitleLow) > 0 then
//    if Term = 'bgc 11' then
//        debugln('NewMakeAllLinks found it =========================' + Term);

    Offset := UTF8Pos(Term, Buff);
    while Offset > 0 do begin
//        debugln('NewMakeAllLinks Acting on Term=' + Term);
        ByteBeforeTerm := UTF8CodepointToByteIndex(PChar(Buff), length(Buff), Offset)-1;
        ByteAfterTerm  := UTF8CodepointToByteIndex(PChar(Buff), length(Buff), Offset+length(Term));

        if ((Offset = 1) or (Buff[ByteBeforeTerm] in [' ', #10, ',', '.'])) and
            (((Offset + length(Term)) = length(Buff)) or (Buff[ByteAfterTerm] in [' ', #10, ',', '.'])) then begin
//            debugln('NewMakeAllLinks calling MakeLink() when Term is [' + Term + ']');
            MakeLink(BlockOffset + Offset -1, UTF8length(Term), Term);                // MakeLink takes a Char Index !
            TimerHouseKeeping.Enabled := False;
        end;
        Offset := UTF8Pos(Term, Buff, Offset + 1);
    end;
end;



procedure TEditBoxForm.CheckForHTTP(const Buff : string; const Offset : integer);
var
    http : integer;
    Len : integer = 1;

    // Returns the length of the char that b is first byte of
{    function LengthUTF8Char(b : byte) : integer;
    begin
      case b of
                0..191   : Result := 1;
                192..223 : Result := 2;
                224..239 : Result := 3;
                240..247 : Result := 4;
      end;
    end;   }


    function ValidWebLength() : integer;
    var
        ADot : boolean = false;
    begin  // we arrive here with http >= 1
        Result := 7;
        if (not((http = 1)
            or (Buff[http-1] in [' ', ',', #10])))
                then exit(0);                                            // invalid start
        while (not(Buff[http+result] in [' ', ',', #10])) do begin       // that might be end of link
            if (http+result) >= length(Buff) then exit(0);               // invalid end
            if Buff[http+result] = '.' then ADot := True;
            inc(result);                                                // next byte
        end;
        if Buff[http+result-1] = '.' then exit(0);                // The dot was at the end.
        if (result < 13) or (not ADot) then exit(0);
    end;

begin
    http := pos('http', Buff);                              // note that http is in bytes, '1' means first byte of Buff
    while (http <> 0) do begin
        if (UTF8copy(Buff, http, 7) = 'http://') or (copy(Buff, http, 8) = 'https://') then begin
            Len := ValidWebLength();                        // reads http and Buff
            if Len > 0 then begin
//                debugln('CheckForHTTP Calling MakeLink() Offset=' + Offset.Tostring + '+' + (UTF8Length(pchar(Buff), http-1)).ToString + ' Len=' + (UTF8Length(pchar(Buff)+http, Len)).ToString);
                MakeLink(OffSet + UTF8Length(pchar(Buff), http-1), UTF8Length(pchar(Buff)+http, Len), '');  // must pass char values, not byte
            end;
        end;
        http := UTF8pos('http', Buff, http+Len+1);          // find next one, +1 to ensure moving on
    end;
end;


procedure TEditBoxForm.CheckForLinks(const FullBody : boolean);
var
    Content : string = '';
    BuffOffset, LineNumb : integer;
    EndScan : Integer = 0;
    i : integer;
    {$ifdef LDEBUG}T1, T2, T3, T4, T5 : qword;{$endif}

    // Puts one para of kMemo, starting at LineNumb, into Content. Ret Starting offset.
    function GrabPara() : integer;
    begin
       Content := '';
       Result := KMemo1.Blocks.LineStartIndex[LineNumb];
       while LineNumb < KMemo1.Blocks.LineCount do begin
           Content := Content + lowercase(Kmemo1.blocks.LineText[LineNumb]);
           inc(LineNumb);
           if Content[high(Content)] = #182 then begin          // Line returns with two char line ending
               delete(Content, High(Content), 1);               // delete the 182
               Content[High(Content)] := #10;                   // replace the 194
               break;
           end;
       end;
    end;

    // Returns the 0 based UTF8 Char index to the start of the passed back St,
    // So, the char 17 in St is actually char 17 + Return in KMemo, Note that Lines
    // returns a string with a two char newline, 194 182 we replace that with
    // a one char #10 to match the KMemo Selection Index.
    function GrabContent() : integer;
    var
        Li : TKMemoLineIndex;
        TSt : string;
    begin
        Li := Kmemo1.blocks.IndexToLineIndex(Kmemo1.Blocks.SelStart);// Thats the line index we are currently on
        Result := Kmemo1.Blocks.SelStart - Kmemo1.blocks.LineStartIndex[Li];     // How far we are to the right of start of line, CHARACTERS
        i := -1;
        while (Li + i) > -1 do begin                                 // Zero is an acceptable line index
            TSt := Kmemo1.blocks.LineText[LI+i];                     // Get the preceeding text
            if TST[length(TST)] = #182 then begin                    // Replace the 2 byte Newline marker
                delete(TSt, length(TST),1);
                TST[length(TST)] := #10
            end;
            Content := lowercase(TSt) + Content;
            Result := Result + UTF8length(TSt);
            if Result > LinkScanRange then break;
            dec(i);
        end;
        i := 0;
        while (Li + i) < KMemo1.blocks.LineCount do begin           // Going forward
            TSt := Kmemo1.blocks.LineText[LI+i];
            if TST[length(TST)] = #182 then begin
                delete(TSt, length(TST),1);
                TST[length(TST)] := #10
            end;
            Content := Content + lowercase(TSt);
            if (UTF8length(Content) - Result) > LinkScanRange then break;
            inc(i);
        end;
        Result := Kmemo1.Blocks.SelStart - Result;
        EndScan := UTF8Length(Content) + Result;
    end;


begin
    {$ifdef LDEBUG}
    AssignFile(MyLogFile, 'log.txt');
    rewrite(MyLogFile);
    TG1 := 0;
    {$endif}
    if (FullBody) then begin                // Scan and do links in whole note, no unlinking required
        LineNumb := 0;
        while LineNumb < KMemo1.Blocks.LineCount do begin
            BuffOffset := GrabPara();       // Updates LineNumb, puts a para in Content, rets UTF8 count from start of Kmemo
            KMemo1.Blocks.LockUpdate;
            if Sett.ShowIntLinks and (not SingleNoteMode) then
                for i := 0 to TheMainNoteLister.NoteList.Count-1 do
                    if TheMainNoteLister.NoteList[i]^.Title <> NoteTitle then begin
                        if length(Content) > 3 then begin                // Two significent char plus a newline
                            {$ifdef LDEBUG}writeln(MyLogFile, 'FB ' + TheMainNoteLister.NoteList[i]^.TitleLow);{$endif}
//                            if pos('bgc 11', TheMainNoteLister.NoteList[i]^.TitleLow) > 0 then
//                                debugln('CheckForLinks found a BGC 11 =========================' + TheMainNoteLister.NoteList[i]^.Title);
                            MakeAllLinks(Content, TheMainNoteLister.NoteList[i]^.TitleLow, BuffOffset);
                        end;
                    end;
            if Sett.CheckShowExtLinks.Checked then
                if length(Content) > 12 then
                    CheckForHTTP(Content, BuffOffset);
            KMemo1.Blocks.UnLockUpdate;    // we lock and unlock frequenty here, slow but avoids "wind up"
        end;                               // end of Fullbody Scan
    end else begin                         // Just scan +/- LinkScanRange of current cursor
        {$ifdef LDEBUG}T1 := gettickcount64();{$endif}
        KMemo1.blocks.LockUpdate;
        BuffOffset := GrabContent();                                    // Also sets EndScan
        {$ifdef LDEBUG}T2 := gettickcount64();{$endif}
        ClearNearLink(BuffOffset, EndScan);                             // Parameters in utf8char, not bytes
        {$ifdef LDEBUG}T3 := gettickcount64();{$endif}
        if Sett.ShowIntLinks and (not SingleNoteMode) then begin
            for i := 0 to TheMainNoteLister.NoteList.Count-1 do begin
                  {$ifdef LDEBUG}writeln(MyLogFile, 'BV ' + TheMainNoteLister.NoteList[i]^.TitleLow);{$endif}
//                if pos('bgc', TheMainNoteLister.NoteList[i]^.TitleLow) > 0 then
//                    debugln('CheckForLinks found a BGC =========================' + TheMainNoteLister.NoteList[i]^.Title);
                if TheMainNoteLister.NoteList[i]^.Title <> NoteTitle then begin
                    MakeAllLinks(Content, TheMainNoteLister.NoteList[i]^.TitleLow, BuffOffset);
                end;
            end;
             TimerHouseKeeping.Enabled := False;
        end;
        {$ifdef LDEBUG}T4  := gettickcount64();{$endif}
        if Sett.CheckShowExtLinks.Checked then
                CheckForHTTP(Content, BuffOffset);                           // Mark any unmarked web links
        KMemo1.blocks.UnLockUpdate;
        {$ifdef LDEBUG}T5  := gettickcount64();
        debugln('CheckForLinks Timing of MakeLink TG1=' + TG1.tostring + ' ' + (T2-T1).ToString + 'mS '  + (T3-T2).ToString + 'mS '  + (T4-T3).ToString + 'mS '  + (T5-T4).ToString + 'mS ');
        {$endif}
    end;
    {$ifdef LDEBUG}CloseFile(MyLogFile);{$endif}
end;


function TEditBoxForm.UnlinkBlock(StartBlock : integer) : integer;
var
    Existing : string;
    ChangedOne : boolean = false;
    Blk : TKMemoTextBlock;
    FontAtt : FontLimitedAttrib;

    function CanMergeBlocks(LinkBlock, TextBlock : integer) : boolean;  // Merges two blocks IFF they are same sort, font, size etc
    begin
        if (KMemo1.Blocks.Items[TextBlock].ClassNameIs('TKMemoTextBlock')
            or KMemo1.Blocks.Items[TextBlock].ClassNameIs('TKMemoHyperlink'))
        and (KMemo1.Blocks.Items[LinkBlock].ClassNameIs('TKMemoTextBlock')
            or KMemo1.Blocks.Items[LinkBlock].ClassNameIs('TKMemoHyperlink'))
        then
            result :=
                ((LinkBlock) < KMemo1.Blocks.count) and
                ((TextBlock) < KMemo1.Blocks.count) and
                (TKMemoTextBlock(KMemo1.Blocks.Items[TextBlock]).TextStyle.Font.Size =
                    TKMemoTextBlock(KMemo1.Blocks.Items[LinkBlock]).TextStyle.Font.Size)
                and
                ((fsBold in TKMemoTextBlock(KMemo1.Blocks.Items[TextBlock]).TextStyle.Font.Style) =
                    (fsBold in TKMemoTextBlock(KMemo1.Blocks.Items[LinkBlock]).TextStyle.Font.Style))
                and
                ((fsItalic in TKMemoTextBlock(KMemo1.Blocks.Items[TextBlock]).TextStyle.Font.Style) =
                    (fsItalic in TKMemoTextBlock(KMemo1.Blocks.Items[LinkBlock]).TextStyle.Font.Style))
                and
                ((fsStrikeOut in TKMemoTextBlock(KMemo1.Blocks.Items[TextBlock]).TextStyle.Font.Style) =
                    (fsStrikeOut in TKMemoTextBlock(KMemo1.Blocks.Items[LinkBlock]).TextStyle.Font.Style))
                and
                ( TKMemoTextBlock(KMemo1.Blocks.Items[TextBlock]).TextStyle.Brush.Color
                    = TKMemoTextBlock(KMemo1.Blocks.Items[LinkBlock]).TextStyle.Brush.Color)

        else Result := False;                                                  // ie cannot merge !
    end;

begin
    if not KMemo1.Blocks.Items[StartBlock].ClassNameIs('TKMemoHyperlink') then begin
        debugln('ERROR UnLink pointed at "not a Link" ');
        exit(StartBlock);
    end;

    // In the first two cases here, link text is shown with attributes of block it was merged into
   if CanMergeBlocks(StartBlock, StartBlock-1) then begin
        Existing := KMemo1.Blocks.Items[StartBlock].Text;
        Kmemo1.Blocks.Delete(StartBlock);
        dec(StartBlock);
        TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock]).Text := KMemo1.Blocks.Items[StartBlock].Text + Existing;
        ChangedOne := True;
        {$ifdef LDEBUG}
        debugln('UnLinkBlock Merge Left StartBlock=' + StartBlock.Tostring + ' Existing=' + Existing);
        {$endif}
   end;

   if CanMergeBlocks(StartBlock, StartBlock+1) then begin
        Existing := KMemo1.Blocks.Items[StartBlock].Text;
        Kmemo1.Blocks.Delete(StartBlock);
        TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock]).Text := Existing + KMemo1.Blocks.Items[StartBlock].Text;
        ChangedOne := True;
        {$ifdef LDEBUG}
        debugln('UnLinkBlock Merge Right StartBlock=' + StartBlock.Tostring + ' Existing=' + Existing);
        {$endif}
   end;

   if not changedOne then begin
        Existing := KMemo1.Blocks.Items[StartBlock].Text;
        SaveLimitedAttributes(StartBlock, FontAtt);
        Kmemo1.Blocks.Delete(StartBlock);
        Blk := KMemo1.Blocks.AddTextBlock(Existing, StartBlock);
        RestoreLimitedAttributes(StartBlock, FontAtt);
        Blk.TextStyle.Font.Style := Blk.TextStyle.Font.Style - [fsBold, fsItalic];
        Blk.TextStyle.Font.Size := Sett.FontNormal;
        {$ifdef LDEBUG}
        debugln('UnLinkBlock No Merge StartBlock=' + StartBlock.Tostring + ' Existing=' + Existing);
        {$endif}
   end;
   Result := StartBlock;           // only changed with merge left
end;


procedure TEditBoxForm.ClearNearLink(const StartS, EndS : integer); inline;
// note that kmemo is locked before we get here.
var
    Blar, StartBlock, EndBlock : longint;
    LinkText  : ANSIString;

    function ValidWebLink() : boolean;     // returns true if LinkText is valid web address
    var
        DotSpot : integer;
        Str : String;
    begin
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
    end;

    function ValidLocalLink() : boolean;
    begin
        if SingleNoteMode then exit(False);
        if not TheMainNoteLister.IsThisaTitle(LinkText) then exit(False);
        Result :=
            KMemo1.Blocks.Items[StartBlock-1].ClassNameIs('TKMemoParagraph')
                or TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock-1]).Text.EndsWith(' ');
        Result := Result
            and (
                (KMemo1.Blocks.count = StartBlock+1)
                or
                (KMemo1.Blocks.Items[StartBlock+1].ClassNameIs('TKMemoParagraph')
                or
                (TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock+1]).Text[1]              // !!!!!!!!!!
                    in [' ', ',']))  );
    end;

begin
    Ready := False;
    EndBlock := KMemo1.Blocks.IndexToBlockIndex(EndS, Blar);
    StartBlock := KMemo1.Blocks.IndexToBlockIndex(StartS, Blar);
    if StartBlock < 2 then StartBlock := 2;
    if EndBlock > Kmemo1.Blocks.Count then EndBlock := Kmemo1.Blocks.Count;
    try
    while StartBlock < EndBlock do begin
        if TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock]).TextStyle.Font.Size = Sett.FontTitle then begin
            inc(StartBlock);
            continue;                                                           // We are NOT dealing with title here
        end;
        if KMemo1.Blocks.Items[StartBlock].ClassNameIs('TKMemoHyperlink') then begin
            LinkText := lowercase(Kmemo1.Blocks.Items[StartBlock].Text);                              // ! trim()
            // Only if its not a valid link, remove it.
            if not (ValidWebLink() or ValidLocalLink()) then begin              // LocalLinks ignored in SingleNoteMode
                StartBlock := UnLinkBlock(StartBlock);
                if EndBlock > Kmemo1.Blocks.Count then EndBlock := Kmemo1.Blocks.Count;
            end;
        end else begin
            // Must check here that its not been subject to the copying of a links colour and underline
            // we know its not a link and we know its not title. So, if TextBlock, check color ...
            // Note : as of Jan 2023, title and link can be different colours
            with TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock]) do
                if KMemo1.Blocks.Items[StartBlock].ClassNameIs('TKMemoTextBlock')  // Only an issue with TextBlocks
                    and (TextStyle.Font.Color = Sett.LinkColour ) then begin
                    TextStyle.Font.Style := TextStyle.Font.Style - [fsUnderLine];
                    TextStyle.Font.Color := Sett.TextColour;
                end;
        end;
        inc(StartBlock);
    end;
    finally
        Ready := True;
    end;
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
    while BlockNo <= EndBlock do begin							 // DANGER, must check these block numbers work
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
    CurserPos, SelLen, BlockNo, Blar : longint;
    TempTitle : ANSIString;
    {$ifdef LDEBUG}TS1, TS2  : qword;{$endif}
begin
    if KMemo1.ReadOnly then exit();
    Ready := False;
    CurserPos := KMemo1.RealSelStart;
    SelLen := KMemo1.RealSelLength;
    BlockNo := KMemo1.Blocks.IndexToBlockIndex(CurserPos, Blar);
    if ((BlocksInTitle + 10) > BlockNo) then begin
        // We don't check title if user is not close to it.
  	    MarkTitle();
  	    GetTitle(TempTitle);
        if not ((TempTitle = caption) or ('* ' + TempTitle = Caption)) then
            TitleHasChanged := True;
        if Dirty then
            Caption := '* ' + TempTitle
        else
            Caption := TempTitle;
    end;
    // OK, if we are in the first or second (?) block, no chance of a link anyway.
    if BlockNo < 2 then begin
        if KMemo1.Blocks.Count = 0 then 		// But bad things happen if its really empty !
            KMemo1.Blocks.AddParagraph();
            Ready := True;
  	        exit();
    end;
    if Sett.ShowIntLinks or Sett.CheckShowExtLinks.Checked then begin
        {$ifdef LDEBUG}TS1 := gettickcount64();{$endif}
        CheckForLinks(False);                   // does its own locking
        TimerHouseKeeping.Enabled := False;
        {$ifdef LDEBUG}TS2 := gettickcount64();
        debugln('------------- DoHousekeeping Update Links ' + inttostr(TS2-TS1) + 'ms');{$endif}
    end;
    KMemo1.SelStart := CurserPos;
    KMemo1.SelLength := SelLen;
    Ready := True;
end;

procedure TEditBoxForm.TimerHousekeepingTimer(Sender: TObject);
begin
    TimerHouseKeeping.Enabled := False;
    DoHouseKeeping();
end;




{ ---------------------- C A L C U L A T E    F U N C T I O N S ---------------}


procedure TEditBoxForm.MenuItemEvaluateClick(Sender: TObject);
begin
   InitiateCalc();
end;

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
function TEditBoxForm.FindNumbersInString(AStr: string; out AtStart, AtEnd : string) : boolean;
var
    Index : integer = 1;
begin
    if AStr = '' then exit(false);
    AtStart := '';
    AtEnd := '';
    while AStr[length(AStr)] = ' ' do delete(AStr, Length(AStr), 1); // remove trailing spaces
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
    TheLine,  CalcStrStart, CalcStrEnd : string;
    AtStart, AtEnd : string;    // strings that hold a token, if found at start or end of line
    Index : integer = 1;
    StartDone : boolean = False;
    EndDone : boolean = False;
begin
    AStr := '';                 // The string we will do our calc on
    CalcStrStart := '';
    CalcStrEnd := '';
    repeat                      // until we have a unusable line both left and right.
        TheLine := PreviousParagraphText(Index);
        FindNumbersInString(TheLine, AtStart, AtEnd);
        //debugln('Scanned string [' + TheLine + '] and found [' + AtStart + '] and [' + atEnd + ']');

        if AtEnd = '' then
            if StartDone then break
            else EndDone := True;
        if AtStart = '' then
            if EndDone then break
            else StartDone := True;     // record that no more tokens at Start will be used
        if (AtStart <> '') and (not StartDone) then
            if CalcStrStart = '' then CalcStrStart := AtStart
            else CalcStrStart := CalcStrStart + ' + ' + AtStart;
        if (AtEnd <> '') and (not EndDone) then
            if CalcStrEnd = '' then CalcStrEnd := AtEnd
            else CalcStrEnd := CalcStrEnd + ' + ' + AtEnd;
        inc(Index);
    until (AtStart = '') and (AtEnd = '');    // Note, we break before that situation anyway !
    if not EndDone then
        AStr := CalcStrEnd;
    if not StartDone then
        AStr := CalcStrStart;
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
    debugln('SimpleCalc=[' + AStr + ']');
    // Special case exists, if the calc string was following some text terminated with
    // a '.', we end up a string starting with '. ' and thats bad.
    if copy(AStr, 1, 2) = '. ' then
        delete(AStr, 1, 2);
    AStr := DoCalculate(AStr);
    exit(AStr <> '');
end;


// ===================== K M E M O   K E Y   S T R O K E S =====================


procedure TEditBoxForm.KMemo1MouseDown(Sender: TObject; Button: TMouseButton;
		Shift: TShiftState; X, Y: Integer);
begin
    MouseDownPos := KMemo1.CaretPos;    // regional record in case we are doing shift click
    //debugln('Mousedown ' + dbgs(KMemo1.CaretPos));
    //{$ifdef LCLCOCOA}
    if ssCtrl in Shift then PopupMenuRightClick.popup;
    //{$else}
	if Button = mbRight then PopupMenuRightClick.PopUp;
    //{$endif}
end;


	{ Any change to the note text and this gets called. So, vital it be quick }
procedure TEditBoxForm.KMemo1Change(Sender: TObject);
begin
    if not Ready then exit();           // don't do any of this while starting up.
    MarkDirty();
    TimerHouseKeeping.Enabled := False;
    TimerHouseKeeping.Enabled := True;
    // HouseKeeping is now driven by a timer;
end;

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
    if not Ready then begin       // Will this help with issue #279
        if [ssCtrl] = shift then
            Key := 0;
        exit();                   // should we drop key on floor ????
    end;
    // don't let any ctrl char get through the kmemo on mac
    {$ifdef DARWIN}
    if [ssCtrl] = Shift then begin
        case Key of
             VK_1 : AlterFont(ChangeSize, Sett.FontSmall);
             VK_2 : AlterFont(ChangeSize, Sett.FontNormal);
             VK_3 : AlterFont(ChangeSize, Sett.FontLarge);
             VK_4 : AlterFont(ChangeSize, Sett.FontHuge);
        end;
        Key := 0;
        exit;
    end;
    if ([ssAlt, ssShift] = Shift) and ((Key = VK_RIGHT) or (Key = VK_LEFT)) then exit; // KMemo - extend selection one word left or right
    {$endif}

    if (Key = VK_ESCAPE) and Sett.CheckEscClosesNote.Checked then close;        // Will do normal save stuff first.

    // Record this event in the Undoer if its ssShift or empty set, rest are ctrl, meta etc ....

    if Use_Undoer and (([ssShift] = Shift) or ([] = Shift)) then             // while we pass presses like this to undoer, not all are
        Undoer.RecordInitial(Key);                                           // used, onKeyPress must follow and it gets only text type keys.


    {$ifndef DARWIN}
    // -------------- Shift -------------------
    if [ssShift] = shift then begin
        if (Key = VK_LEFT) or (Key = VK_RIGHT) then exit; // KMemo - extend selection one char left or right
        if (Key = VK_F3) then
        begin
            key := 0;
            if (EditFind.Text <> rsMenuSearch) then SpeedLeftClick(self);
       end;
    end;
    {$endif}

    // -------------- Control ------------------
    if {$ifdef Darwin}[ssMeta] = Shift {$else}[ssCtrl] = Shift{$endif} then begin
        case key of
            VK_Return, VK_G :  begin
                key := 0;
                if (EditFind.Text <> rsMenuSearch) then SpeedRightClick(self);
            end;
            VK_Q : MainForm.close();
            VK_1 : AlterFont(ChangeSize, Sett.FontSmall);
            VK_2 : AlterFont(ChangeSize, Sett.FontNormal);
            VK_3 : AlterFont(ChangeSize, Sett.FontLarge);
            VK_4 : AlterFont(ChangeSize, Sett.FontHuge);
            VK_B : AlterFont(ChangeBold);
            VK_I : AlterFont(ChangeItalic);
            VK_S : AlterFont(ChangeStrikeOut);
            VK_T : AlterFont(ChangeFixedWidth);
            VK_H : AlterFont(ChangeColor);
            VK_U : AlterFont(ChangeUnderLine);
            VK_F : begin Key := 0; MenuItemFindClick(self); end;
            VK_L : SpeedButtonLinkClick(Sender);
            VK_V : begin if Use_Undoer then Undoer.AddPasteOrCut(); exit; end;         // Must exit to prevent setting Key to 0
            VK_X : begin if Use_Undoer then Undoer.AddPasteOrCut(True); exit; end;     // Must exit to prevent setting Key to 0
            VK_Z : if Use_Undoer then Undoer.UnDo;                                     // Note : Ctrl-Z does not go through to KMemo
            VK_Y : if Use_Undoer then Undoer.Redo;                                     // Note : Ctrl-Y does not go through to KMemo
            VK_D : InsertDate();
            VK_N : SearchForm.OpenNote('');
            VK_E : InitiateCalc();
            VK_F4 : close;                      // close just this note, normal saving will take place
            VK_C, VK_A, VK_HOME, VK_END, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_PRIOR, VK_NEXT, VK_INSERT : exit;
        end;
        Key := 0;    // so we don't get a ctrl key character in the text
        exit();
    end;
    { Control-Z bug
    hex 1A, 26 is reported to be getting past above filter and ending up in a note #279.
    }

    // ------------- Alt (or Option in Mac) ------------------
    if [ssAlt] = Shift then begin
        case key of
            {$ifdef DARWIN}
            VK_H  : begin AlterFont(ChangeColor); ; Key := 0; end; {$endif}
            VK_RIGHT : begin BulletControl(True); Key := 0; end;
            VK_LEFT  : begin BulletControl(False); Key := 0; end;
            VK_Return :  if (EditFind.Text <> rsMenuSearch) then begin Key := 0; SpeedLeftClick(self); end;
        end;
        exit();
    end;

    // ------------------ Control and Shift (or, Mac, Command and Shift) ----------------
    if {$ifdef Darwin}[ssMeta, ssShift]{$else}[ssCtrl, ssShift]{$endif} = Shift then begin
        case Key of
            VK_F : SpeedButtonSearchClick(self);                            // Search all notes
            VK_G : if (EditFind.Text <> rsMenuSearch) then SpeedLeftClick(self);
            {$ifndef DARWIN}
            VK_RIGHT, VK_LEFT : exit;   // KMemo knows how to do this, select word ...
            {$endif}
       end;
       Key := 0;
       exit();
    end;

    if Key = VK_TAB then begin                                            // ToDo : Tabs do not work as expected
      KMemo1.InsertChar(KMemo1.Blocks.RealSelStart, ' ');
      KMemo1.InsertChar(KMemo1.Blocks.RealSelStart, ' ');
      KMemo1.InsertChar(KMemo1.Blocks.RealSelStart, ' ');
      KMemo1.InsertChar(KMemo1.Blocks.RealSelStart, ' ');
      Key := 0;
      exit;
    end;

    if Key = VK_F3 then begin
        key := 0;
        if (EditFind.Text <> rsMenuSearch) then SpeedRightClick(self);
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

    // KMemo1.Blocks.LockUpdate;  Do not lock because we move the cursor down here.
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

procedure TEditBoxForm.KMemo1KeyPress(Sender: TObject; var Key: char);
begin
    if Use_Undoer then Undoer.AddKeyPress(Key);
end;

procedure TEditBoxForm.KMemo1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Use_Undoer then Undoer.AddKeyUp(Key, Shift);
end;



// ======= I M P O R T I N G   and   E X P O R T I N G    F U N C T I O N S  =======



procedure TEditBoxForm.ImportNote(FileName: string);
var
    Loader : TBLoadNote;
 	//T1, T2, T3, T4, T5 : qword;          // Temp time stamping to test speed
begin
    // Timing numbers below using MyRecipes on my Dell linux laptop. For local comparison only !
    // Note QT5 times quite a lost faster, Loading is slow and so is resizing !  Sept 2022
    //T1 := gettickcount64();
    Loader := TBLoadNote.Create();
    Loader.FontNormal:= Sett.FontNormal;                    // 0mS
    Loader.FontSize:= Sett.FontNormal;
    KMemo1.Blocks.LockUpdate;
    KMemo1.Clear;
    Loader.LoadFile(FileName, KMemo1);                      // 140mS  (197mS GTK2)
    //KMemo1.Blocks.UnlockUpdate;
    Createdate := Loader.CreateDate;
    //Ready := true;
    Caption := Loader.Title;
//    if Sett.ShowIntLinks or Sett.CheckShowExtLinks.checked then
//    	CheckForLinks(True);                  		         // 12mS (14ms GTK2)
    KMemo1.Blocks.UnlockUpdate;                              // moved down from 6 lines up to cover CheckForLinks
    Left := Loader.X;
    Top := Loader.Y;
    Height := Loader.Height;                                 // 84mS (133mS GTK2) Height and Widt                                      h
    Width := Loader.Width;                                   // AdjustFormPosition() will fix if necessary
    AdjustFormPosition();
    Loader.Free;                                             // 0mS
    TimerHouseKeeping.Enabled := False;     // we have changed note but no housekeeping reqired
    //debugln('Load Note=' + inttostr(T2 - T1) + 'mS ' + inttostr(T3 - T2) + 'mS ' + inttostr(T4 - T3) + 'mS ' + inttostr(T5 - T4) + 'mS ');
    //debugln('Total=' + inttostr(T5 - T1) + 'mS ');

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



// ===================================== S A V I N G ===========================

{ When changes are made to note content, a timer is triggered and extended as user
types. Sooner or later, the timer demands SaveTheNote() be called. It locks kmemo,
builds a list of its content using Saver, TBSaveNote (which is badely named, it
generates the xml to save but does not, itself, save to disk. Unlocks. It calls
SearchForm.UpdateList() OR if its because we are closing the note and note is
clean, goes diret to TheMainNoteLister with just an updated LCD. Note, the note
has not hit the disk yet !

That list is passed to SaveStringList, it makes a new thread that normalises
content of string list and then writes it to disk.
}

{$define SAVETHREAD}

function TEditBoxForm.SaveStringList(const SL: TStringList; Loc : TNoteUpdateRec) : boolean;
var
    {$ifdef SAVETHREAD}
    TheSaveThread : TSaveThread;
    {$else}
    Normaliser : TNoteNormaliser;
    WBufStream : TWriteBufStream;
    FileStream : TFileStream;
    {$ENDIF}
begin
    if BusySaving then exit(False);
    BusySaving := True;
    Result := True;
    {$ifdef SAVETHREAD}
    TheSaveThread := TSaveThread.Create(true);
    TheSaveThread.TheLoc := Loc;
    TheSaveThread.TheSL := Sl;
    TheSaveThread.Start;
    // It will clean up after itself.
    {$else}
    Normaliser := TNoteNormaliser.Create;
    Normaliser.NormaliseList(SL);
    Normaliser.Free;
    SL.Add(Footer(Loc));
    // TWriteBufStream, TFileStream preferable to BufferedFileStream because of a lighter memory load.
    FileStream := TFileStream.Create(Loc.FFName, fmCreate);
    //FileStream := TFileStream.Create('/home/dbannon/savethread.note', fmCreate);
    WBufStream := TWriteBufStream.Create(FileStream, 4096);             // 4K seems about right on Linux.
    try
      try
          SL.SaveToStream(WBufStream);
      except on E:Exception do begin
                              Debugln('ERROR, failed to save note : ' + E.Message);
                              WBufStream.Free;
                              FileStream.Free;
                              SL.Free;
                          end;
      end;
    finally
      WBufStream.Free;
      FileStream.Free;
      SL.Free;
    end;
    BusySaving := False;
    {$ENDIF}
end;

procedure TEditBoxForm.SaveTheNote(WeAreClosing : boolean = False);
var
    Title : string;
 	Saver : TBSaveNote;
    SL : TStringList;
    OldFileName : string ='';
    Loc : TNoteUpdateRec;
    NoteContent : string = '';                       // Might hold a lowercase text version of note for searcing purposes.
    LineNumb   : integer = 0;
    FName      : string;
    //ItsANewNote : boolean = false;
    //T1, T2, T3, T4, T5, T6, T7 : qword;            // Timing shown is for One Large Note.

begin
    if BusySaving then begin
        MainForm.ShowNotification('Failed to Auto Save', 3000);   // inform user via notifications
//        ShowMessage('ERROR, unable to save ' + NoteFileName);   // No, don't do that, it stops the process
        exit;
    end;
    //T1 := gettickcount64();
    Saver := Nil;
    if KMemo1.ReadOnly then exit();
  	if length(NoteFileName) = 0 then begin
        NoteFileName := Sett.NoteDirectory + GetAFilename();
        //ItsANewNote := True;
    end;
    if (not WeAreClosing)
        and (Sett.NoteDirectory = CleanAndExpandDirectory(ExtractFilePath(NoteFileName))) then begin   // Check name of Repo note, not SNM. UTF8 OK
        if not IDLooksOK(ExtractFileNameOnly(NoteFileName)) then
            if mrYes = QuestionDlg('Invalid GUID', 'Give this note a new GUID Filename (recommended) ?', mtConfirmation, [mrYes, mrNo], 0) then begin
                OldFileName := NoteFileName;
                NoteFileName := Sett.NoteDirectory + GetAFilename();
                Loc.LastChangeDate:= TheMainNoteLister.GetLastChangeDate(ExtractFileNameOnly(OldFileName));
                SearchForm.UpdateList(CleanCaption(), Loc.LastChangeDate, NoteFileName, self);     // some timewasting menu rewrite ??
                Debugln('We have just registered a new name for that note with invalid GUID');
            end;
        end;
    if TemplateIs <> '' then begin
        SL := TStringList.Create();
        SL.Add(TemplateIs);
        TheMainNoteLister.SetNotebookMembership(ExtractFileNameOnly(NoteFileName) + '.note', SL);
        SL.Free;
        TemplateIs := '';
    end;
    Saver := TBSaveNote.Create();
    Saver.CreateDate := CreateDate;
    if not GetTitle(Title) then exit();
    // If title has changed, we make a backup copy.
    if TitleHasChanged then begin
        SearchForm.BackupNote(NoteFileName, 'ttl');
        TitleHasChanged := False;
	end;
    Caption := Title;
    KMemo1.Blocks.LockUpdate;                                   // to prevent changes during read of kmemo
    //T2 := GetTickCount64();
    SL := TStringList.Create();
    try
        Saver.ReadKMemo(NoteFileName, Title, KMemo1, SL);       // Puts all the content into the StringList, SL
        //T3 := GetTickCount64();
        if Dirty and (not SingleNoteMode) and Sett.AutoSearchUpdate then begin  // SLOW ! Replace the search Content in note lister
            while LineNumb < KMemo1.Blocks.LineCount do begin
                NoteContent := NoteContent + lowercase(Kmemo1.blocks.LineText[LineNumb]);
                inc(LineNumb);
                if NoteContent[high(NoteContent)] = #182 then begin          // Line returns with two char line ending
                    delete(NoteContent, High(NoteContent), 1);               // delete the 182
                    NoteContent[High(NoteContent)] := #10;                   // replace the 194
                end;
            end;
        end;
        //T4 := GetTickCount64();
    finally
        KMemo1.Blocks.UnLockUpdate;
        if Saver <> Nil then Saver.Destroy;
        Caption := CleanCaption();
    end;
    // At this stage, a new note does not exist in NoteLister, so, call the Content updater later

    //T5 := GetTickCount64();
    Loc.Width:=inttostr(Width);
    Loc.Height:=inttostr(Height);
    Loc.X := inttostr(Left);
    Loc.Y := inttostr(Top);
    Loc.OOS := booltostr(WeAreClosing, True);
    Loc.CPos:='1';
    loc.FFName := NoteFileName;
    loc.CreateDate := CreateDate;
    if Dirty or SingleNoteMode then begin                       // In SingeNoteMode, there is no NoteLister, so date is always updated.
        Loc.LastChangeDate:= TB_GetLocalTime();
        SearchForm.UpdateList(CleanCaption(), Loc.LastChangeDate, NoteFileName, self);     // 6mS - 8mS timewasting menu rewrite ??  No usually now
    end else
        Loc.LastChangeDate                                      // Must be closing.
            := TheMainNoteLister.GetLastChangeDate(ExtractFileNameOnly(NoteFileName));
    // Dec 2022 - I moved the following block down here to ensure a new note is in note lister before setting content
    if Dirty and (not SingleNoteMode) and Sett.AutoSearchUpdate then begin      // This is quick,
        LineNumb := TheMainNoteLister.NoteList.Count -1;                        // start searching at end of list cos thats where new notes live
        FName := ExtractFileName(NoteFileName);
        while LineNumb > -1 do begin
            if TheMainNoteLister.NoteList[LineNumb]^.ID = FName then begin
                TheMainNoteLister.NoteList[LineNumb]^.Content := NoteContent;
                break;
            end;
            dec(LineNumb);
        end;
    end;

    if SaveStringList(SL, Loc) then Dirty := False;             // Note, thats not a guaranteed good save,
    //T6 := GetTickCount64();

    //debugln('Save Note Initial=' + inttostr(T2-T1) + ' Saver=' + inttostr(T3-T2)
    //            + ' BuildContent=' + Inttostr(T4-T3) + ' ContentToNoteLister=' + inttostr(T5-T4) + ' SendToSaveStringList=' + (T6-T5).tostring);
    // Save Note Initial=0 Saver=4 BuildContent=4 ContentToNoteLister=0 SendToSaveStringList=0  with locking disabled
    // ToDo : Building search content is pretty slow ??
    // Move into SaveThread would require passing kmemo there too but thats probably the answer
    // maybe review the lines approach ? Saver is doing a lot more ....
    (*
    {$ifdef SAVETHREAD}
    debugln('Total time to save threaded is ' + inttostr(T5-T1));
    {$else}
    debugln('Total time to save UN-threaded is ' + inttostr(T5-T1));
    {$endif}      *)
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

// As UpdateNote does not record Notebook membership, abandon it for now.
// Maybe come back later and see if it can be patched, its probably quicker.
// Was only called on a clean note ....
(*
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
end;  *)

