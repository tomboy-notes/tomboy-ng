unit EditBox;

{   Copyright (C) 2017-2024 David Bannon

    License:
    This code is licensed under MIT License, see the file License.txt
    or https://spdx.org/licenses/MIT.html  SPDX short identifier: MIT
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
    2023/02/14  Fixed bug in column calculater, was ignoring negitive terms.
    2023/03/11  Allow Qt to set Text and Background colour, force Gray for Inactive
                background (in LoadNote) cos Kmemo get it wrong
    2023/04/08  Get a note's height and width before populating KMemo, saves 200mS on a big note !
    2023/04/09  Keep Find-in-note prompt there unless not found.
    2023/12/30  Added ability to display Back Links (Links button with nothing selected)
    2024/01/17  Altered the colour of Backlinks Panel to clMenu AFTER building all except MacOS and Packman
    2024/01/23  More or less finished rewrite of Bullet code, addition of indent. Needs testing.
    2024/02/05  Set the yellow read only warning panel to height = 0 instead of 1. This will need cross platform testing !
    2024/03/18  FormActivate code run once depended on a typed constant, wrong, they are shared over all instances.
    2024/03/19  Altered MakeLink to accept byte as Len, added File Link capability.
    2024/03/20  Close the backlinks 'window' before triggering the other note.
    2024/03/24  FileLink in a basic form
    2024/04/14  Extensive rewrite about Links, especially FileLink, remove click on "file://".
    2024/06/07  UTF8 bug, where missed UTF8 char ahead of file link.
    2024/10/05  Made DeletingThisNote public so SearchForm can delete an Open note.
    2024/10/16  Fixed the way that Save on Quit works, no contention !
    2024/12/24  Altered Indent to work (a little) like Tomboy, embedded Tab #9 char at start line
}


{$mode objfpc}{$H+}

{.$define LDEBUG}       // ToDo : Lots of ugly debug output around the Link code, remove when stable

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
    StdCtrls, Buttons, kmemo, clipbrd, lcltype,
    ComCtrls,           // required up here for copy on selection stuff.
    fpexprpars,         // for calc stuff ;
    SaveNote,      		// Knows how to save a Note to disk in Tomboy's XML
    PrintersDlgs,
    TBUndo, LazLogger;

type FontLimitedAttrib = record      // Used to save and restore attributes when
    Styles : TFontStyles;            // a hyperlink is created or unlinked.
    BackColour : TColor;             // Note we don't do underline here....
    Size : Integer;
end;


type TiLActionRec = record                     // a record used to describe how we insert a file link.
                    LeadingSpace : boolean;
                    TrailingSpace : boolean;
                    NewTextBlock  : boolean;   // we must insert new text block before indicated one
                    BlockNo : integer;         // this block to act on.
                    Offset : integer;          // pos in text to insert at
                    end;

type
    { TEditBoxForm }

    TEditBoxForm = class(TForm)
        BitBtnBackLinks: TBitBtn;
        BitBtnCloseFind: TBitBtn;
        ButtMainTBMenu: TSpeedButton;
        EditFind: TEdit;
        KMemo1: TKMemo;
        LabelBackLinks: TLabel;
        LabelFindCount: TLabel;
        LabelFindInfo: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        ListBoxBackLinks: TListBox;
        MenuBold: TMenuItem;
		MenuFindPrev: TMenuItem;
        MenuItalic: TMenuItem;
        MenuHighLight: TMenuItem;
        MenuHuge: TMenuItem;
        MenuItem1: TMenuItem;
		MenuFindNext: TMenuItem;
        MenuItemToolsBackLinks: TMenuItem;
        MenuItem4: TMenuItem;
        MenuItem5: TMenuItem;
        MenuItem6: TMenuItem;
        MenuItemToolsLinks: TMenuItem;
        MenuItemToolsInsertFileLink: TMenuItem;
        MenuItemToolsInsertDirLink: TMenuItem;
        MenuItemInsertFileLink: TMenuItem;
        MenuItemInsertDirLink: TMenuItem;
        MenuItemSelectLink: TMenuItem;
        MenuItemCopyPlain: TMenuItem;
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
        OpenDialogFileLink: TOpenDialog;
        PanelButtons: TPanel;
        PanelBackLinks: TPanel;
        PanelFind: TPanel;
        PanelReadOnly: TPanel;
        PopupMainTBMenu: TPopupMenu;
        PopupMenuSymbols: TPopupMenu;
		PopupMenuRightClick: TPopupMenu;
        PopupMenuTools: TPopupMenu;
        PopupMenuText: TPopupMenu;
        PrintDialog1: TPrintDialog;
        SelectDirectoryForLink: TSelectDirectoryDialog;
        Separator1: TMenuItem;
        SpeedSymbol: TSpeedButton;
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
//		TaskDialogDelete: TTaskDialog;           just why was this here ?  Messes with Windows
		TimerSave: TTimer;
        TimerHousekeeping: TTimer;
        procedure BitBtnBackLinksClick(Sender: TObject);
        procedure BitBtnCloseFindClick(Sender: TObject);
        procedure ButtMainTBMenuClick(Sender: TObject);
        procedure EditFindChange(Sender: TObject);
        procedure EditFindEnter(Sender: TObject);
        procedure EditFindExit(Sender: TObject);
        procedure EditFindKeyDown(Sender: TObject; var Key: Word;
            Shift: TShiftState);
        procedure FormActivate(Sender: TObject);
        procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
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
        procedure KMemo1Click(Sender: TObject);
        	                    { Watchs for  backspace affecting a bullet point, and whole lot of ctrl, shift, alt
                                combinations. For things we let KMemo handle, just exit, for things we handle
                                must set key to 0 after doing so. }
		procedure KMemo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure KMemo1KeyPress(Sender: TObject; var Key: char);
        procedure KMemo1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure KMemo1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; {%H-}X, Y: Integer);
        procedure KMemo1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure ListBoxBackLinksClick(Sender: TObject);
        procedure MenuItemExportPDFClick(Sender: TObject);
        procedure MenuItemCopyPlainClick(Sender: TObject);
        procedure MenuItemFileLinkClick(Sender: TObject);
        procedure MenuItemToolsLinksClick(Sender: TObject);
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
        procedure SpeedSymbolClick(Sender: TObject);
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

        procedure AddItemMenu(TheMenu: TPopupMenu; Item: string; mtTag: integer; OC: TNotifyEvent);
        procedure AdjustFormPosition();
                                { Alters the Font of Block as indicated }
        procedure AlterBlockFont(const FirstBlockNo, BlockNo: longint;
				const Command: integer; const NewFontSize: integer=0);
                                { Alters the font etc of selected area as indicated }
        procedure AlterFont(const Command : integer; const NewFontSize: integer = 0);
                                // Returns True if it used the BS message on a line with or near a bullet para.
                                // Must deal with a BS when on ch 0 of a Bullet para (reduce bullet) or, if preceeding
                                // para is already a bullet, we ensure merged para is same bullet level.
        function BackSpaceBullet(): boolean;
                                // Called when right click menu responds to InsertFileLink or InsertDirLink
                                // Builds a FileLink at the current cursor position.
                                // Always a new link, the resulting link will always start at indicated
                                // position and we provide the FileLinkToken and whitespace left and
                                // right if necessary, we just manipulate text in that block.
                                // We will just insert necessary text, housekeeping will markup the actual link.
        function BuildFileLink(ItsFile: Boolean; CharNo: integer = 0): boolean;

                                // Will increase (MoreBullet=True) or decrease bullet level.
        procedure BulletControl(MoreBullet: boolean);
                                // Gets passed a string containing a copy of one or more kmemo paragraphs, seperated by
                                // #10. And a offset from the the start of the kmemo to the start of string.
                                // Will mark up any web or file addresses it finds that are not already so marked.
                                // Http must start at start of para or space, must have at least one . and end with
                                // space or newline char (#10). Returns 0 or length, in bytes, of the link
                                // File links must start with file://
        procedure CheckForExtLinks(const Buff: string; const Offset: integer);
                                { Scans KMemo for bad utf8 characters, seems only used when importing RTF in SNM}
        procedure CleanUTF8();
        function ColumnCalculate(out AStr: string): boolean;
        function ComplexCalculate(out AStr: string): boolean;
                                // Displays the right click menu after determining
                                // what should be enabled and what not. Key is where
                                // "current position" is, a right click will have
                                // moved the cursor if nothing was selected.
        procedure DoRightClickMenu();
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
        procedure GetBlockIndexes(Index: integer; out FirstChar, blkLen: integer);
        procedure InsertDate();
                                // Does its thing when user clicks a file://something file or dir link.
        function OpenFileLink(LinkText: string): boolean;
                                { Searches Buff for all occurances of Term, checks its surrounded appropriately
                                and calls MakeLink to mark it up as a local Link. Does NOT look directly at KMemo1 }
        procedure MakeAllLinks(const Buff: string; const Term: ANSIString; const BlockOffset: integer);
        function ParagraphTextTrunc(): string;
        procedure PopulateSymbolMenu(AMenu: TPopupMenu);
        procedure QuestionToken(MDContent : TStringList; NoteFName,
            NoteSaveName : string);
        function RelativePos(const Term: ANSIString; const MText: PChar; StartAt: integer): integer;
        function PreviousParagraphText(const Backby: integer): string;
        function RestoreLimitedAttributes(const BlockNo: TKMemoBlockIndex; var FontAtt: FontLimitedAttrib): boolean;
        function SaveLimitedAttributes(const BlockNo: TKMemoBlockIndex; out
            FontAtt: FontLimitedAttrib): boolean;
                                // This method will, at some stage, return after creating and starting
                                // a thread that normalises the xml in the list, adds footer and saves.
                                // The thread keeps going after the method returns doing above and then
                                // free-ing the List.
        function SaveStringList(const SL : TStringList; Loc : TNoteUpdateRec;
            WeAreClosing : boolean) : boolean;
        procedure SetTheColors;
        procedure ShowBackLinks();
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

                                { Returns with the title, that is the first line of note, returns False if title is empty }
        function GetTitle(out TheTitle: ANSIString): boolean;
                                { Clears KMemo, sets colors, marks up title and loads the note into KMemo }
        procedure ImportNote(FileName : string);
        procedure InitiateCalc();
                                { Test the note to see if its Tomboy XML, RTF or Text. Ret .T. if its a new note. }
        function LoadSingleNote() : boolean;
                                { Checks that the indicated text falls in all TKMemoTextBlock(s), replaces a link
                                if new one is longer (greatly favours Web Links!). Exists early if it finds it
                                does not need to make any changes. Make a new block, TKMemoHypeLink, use saved
                                text and saved attributes. Expects invalid links to already have been removed.
                                Does not mess with an existing HTTP link.  Index is Char, Len in bytes. }
		procedure MakeLink(const Index, Len: longint; const Term: string;
            InsertMode: boolean = false);
                                { Makes sure the first (and only the first) line is marked as Title
                                Title should be Blue, Underlined and FontTitle big.
                                Note that when a new note is loaded from disk, this function is not called,
                                the Load unit knows how to do it itself. Saves 200ms with a big (20K) note. }
        procedure MarkTitle();
                                { Returns true if current cursor is 'near' a bullet item. That could be because we are
  		                        on a Para Marker thats a Bullet and/or either Leading or Trailing Para is a Bullet.
  		                        We return with IsFirstChar true if we are on the first visible char of a line (not
  		                        necessarily a bullet line). If we return FALSE, passed parameters may not be set. }
//		function NearABulletPoint(out Leading, Under, Trailing, IsFirstChar, NoBulletPara: Boolean;
//                        out BlockNo, TrailOffset, LeadOffset: longint): boolean;

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

        function CleanCaption() : ANSIString;
                                // Ratchets up or down the passed Paragraph's Bullet level. Bullet=T means increase level.
        procedure SetBullet(PB: TKMemoParagraph; Bullet: boolean;
            Target: TKMemoParaNumbering=pnuNone);
                                // Advises other apps we can do middle button paste
        procedure SetPrimarySelection;
        procedure SymbolMenuClicked(Sender: TObject);
                                // returns the type insert action we must do, and passes back advice about leading
                                // and trailing space, need for a new block. Reports only, does not act.
        function TestLinkInsert(IndexNo: integer; out ILA: TiLActionRec): boolean;
                                { Restores block at StartLink to Text, attempts to merge linktext back into both
                                the previous or next block if it can.
                                There is a problem here. If a link is edited making it invalid but the remainer
                                happens to also be a valid link, we don't get back to original if edit is reversed. }
        function UnlinkBlock(StartBlock: integer): integer;
                                // Cancels any indication we can do middle button paste 'cos nothing is selected
        procedure UnsetPrimarySelection;
                                // Tells us where we are with respect inserting or selecting links.
                                // Returns T if we can insert a link at the current cursor position.
                                // and sets OnPara or Start of Para if appropriate.
                                // If either set, then we must be on a space and probably will need to split a
                                // block to insert a link.
                                // [prev block ] --- might be part of next line too.
                                // [some text][ more text][P]
                                //  |            |  |   |  ^--- We are on a para,
                                //  |            |  |   ^-------Last char of a block (length = Offset+1), if next bk para then WhiteRight=T
                                //  |            |  ^------------On a space, mid block, WhiteLeft=F. WhiteRight=T, if Hypa Left the LinkLeft=T
                                //  |            ^---------------In text, no link allowed but if block is Hypa then InLink=T
                                //  ^----------------------------Offset=0, if Bk before is para, WhiteLeft=T, else look at last char prev block
        function CanInsertFileLink(out SoP, OnPara, InLink: boolean): boolean;

    public
        SingleNoteFileName : string;    // Set by the calling process. FFN inc path, carefull, cli has a real global version
        SingleNoteMode : Boolean;       // Set true if a MainUnit.SingleNoteFileName is provided, in Create()
        NoteFileName : string;          // Will contain the full note name, path, ID and .note
        NoteTitle : string;             // only used during initial opening stage ?
        Dirty : boolean;
        Verbose : boolean;
        SearchedTerm : string;          // If not empty, opening is associated with a search, go straight there.
        HaveSeenOnActivate : boolean;   // Indicates we have run, eg, CheckForLinks at Activate


        BusySaving : boolean;       // Indicates that the thread that saves the note has not, yet exited.

                                    // If a new note is a member of Notebook, this holds notebook name until first save.
        TemplateIs : AnsiString;

                                 // Set True by the delete button so we don't try and save it. Or set from
                                 // SearchForm because, again, we are deleting this note, no point in saving
        DeletingThisNote : boolean;
                               // Public : Declares note needs saving and starts timer.
        procedure MarkDirty();

                                { Public : Will mark this note as ReadOnly and not to be saved because the Sync Process
                                has either replaced or deleted this note OR we are using it as an internal viewer.
                                Can still read and copy content. Viewer users don't need big ugly yellow warning}
        procedure SetReadOnly(ShowWarning : Boolean = True);
                                // Public: Call on a already open note if user has followed up a search with a double click
        procedure NewFind(Term: string);
                                { Public : Saves the note in KMemo1, must have title but can make up a file name if needed
                                If filename is invalid, bad GUID, asks user if they want to change it (they do !)
                                WeAreClosing indicates that the whole application is closing (not just this note)
                                We always save the note on FormDestroy or application exit, even if not dirty to
                                update the position and OOS data.  We used to call UpdateNote in the hope its quicker
                                but it forgets to record notebook membership. Revist some day ....}
        procedure SaveTheNote(WeAreClosing: boolean=False);
                                { Public : This is entry to manage Links. If FullBody, is a freshly loaded note with no links
                                so we just scan and insert as necessary. Otherwise, its being called as the user
                                types, we grab text around current cursor position, remove any invalid links and
                                then scan and insert any that need be there. In all cases, we honour the use links
                                setting from Sett. }
        procedure CheckForLinks(const FullBody: boolean);
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
    //Title : string;             // for debugging purposes, remove ???
    TheForm : TEditBoxForm;
    TheSL : TStringList;
    TheLoc : TNoteUpdateRec;    // defined in SaveNote
    Constructor Create(CreateSuspended : boolean);
  end;

var
    EditBoxForm: TEditBoxForm;
//    BusySaving : boolean;       // Indicates that the thread that saves the note has not, yet exited.

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
//    LCLStrConsts,       // just for rsMBClose ?
    KMemo2PDF,
    tb_symbol{, libfontconfig};

const
        LinkScanRange = 100;	// when the user changes a Note, we search +/- around
     							// this value for any links that need adjusting.

        FileLinkToken = 'file://';
        FileLinkTokenLen = 7;


{$ifdef LDEBUG}var
  MyLogFile: TextFile;{$endif}

{ =============  T   S A V E   T H R E A D   ================== }


procedure TSaveThread.Execute;
var
    Normaliser : TNoteNormaliser;
    WBufStream : TWriteBufStream;
    FileStream : TFileStream;
    SleepCnt : integer = 0;
begin
    while (LockSyncingNow or LockSavingNow) do begin          // declared and mostly used in Settings
        sleep(100);
        inc(SleepCnt);
        if SleepCnt > Sleeps then begin
            PostMessage(sett.Handle, WM_SYNCMESSAGES,  WM_SAVETIMEOUT, 0);    // ToDo : if this ever happens, we should mark note as dirty again !  How ?
            TheForm.BusySaving := False;
            {$ifdef LINUX}                                   // writeln seems more reliable from a thread than debugln ?
            writeln('TSaveThread.Execute --- ERROR ---, failed to get Save Lock, Filename : ' + TheLoc.FFName + ' - ', TheForm.Caption) ;
            {$endif}
            exit;
        end;
    end;
    LockSavingNow := True;
//    debugln('TSaveThread.Execute Started ' + FormatDateTime('hh:nn:ss.zzz', Now()) + ' form=' + TheForm.Caption); // ToDo : remove me
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
                            {$ifdef LINUX}
                            writeln('TSaveThread.Execute ---- ERROR ---, failed to save : ' , TheForm.Caption) ;
                            writeln('            FileName : '+ TheLoc.FFName + ' - ', E.Message);
                            {$else}
                            debugln('TSaveThread.Execute --- ERROR ---, failed to save : ' , TheForm.Caption) ;
                            debugln('            FileName : '+ TheLoc.FFName + ' - ', E.Message);
                            {$endif}
                             WBufStream.Free;
                             FileStream.Free;
                             TheSL.Free;
                             PostMessage(sett.Handle, WM_SYNCMESSAGES,  WM_SAVEERROR, 0);   // Will release Lock
                             exit;
                          end;
        end;
    finally
        WBufStream.Free;
        FileStream.Free;
        TheSL.Free;
        if not Sett.AreClosing then
            TheForm.BusySaving := False;       // Only necessary if not closing, maybe dangerous if closing.
//        debugln('TSaveThread.Execute Finished ' + FormatDateTime('hh:nn:ss.zzz', Now()) + ' form=' + TheForm.Caption); // ToDo : remove me
    end;
    PostMessage(sett.Handle, WM_SYNCMESSAGES,  WM_SAVEFINISHED, 0);             // Hmm, no error reporting happening here ?
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
var SoP, OnPara, InLink, CanInsert : boolean;
begin
    CanInsert := CanInsertFileLink(SoP, OnPara, InLink);
//    debugln('DoRightClickMenu() CanInsert=' + booltostr(CanInsert, True) + ' Sop=' + booltostr(Sop, True)
//        + ' OnPara=' + booltostr(OnPara, True) + ' InLink=' + booltostr(InLink, True));
    MenuItemToolsInsertFileLink.Enabled := CanInsert;
    MenuItemToolsInsertDirLink.Enabled  := CanInsert;
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
{ When a note is deleted, a number of different paths may happen -
  If the note is already listed in a notebook because it was created from notebook
  template, it may not yet be in TNoteLister.NoteList. Thats an issue #312
  - SearchForm.DeleteNote(NoteFileName);
    - remove from sync lists
    - TheMainNoteLister.DeleteNote(ShortFileName);
    - back it up
    - index and refresh
    (i appears I don't remove a note ID from the NotebookList ???
}

procedure TEditBoxForm.BitBtnBackLinksClick(Sender: TObject);
begin
    PanelBackLinks.Visible := False;
end;

procedure TEditBoxForm.ListBoxBackLinksClick(Sender: TObject);
begin
    if (ListBoxBackLinks.ItemIndex >= 0) and (ListBoxBackLinks.ItemIndex < ListBoxBackLinks.Count) then begin
        if (ListBoxBackLinks.Items[ListBoxBackLinks.ItemIndex] <> 'Cancel')
            and (ListBoxBackLinks.Items[ListBoxBackLinks.ItemIndex] <> 'Notes that Link to here')  then begin
                PanelBackLinks.Visible := False;
                SearchForm.OpenNote(ListBoxBackLinks.Items[ListBoxBackLinks.ItemIndex]
                    ,'','',True, True, CleanCaption() );
                    // ,'','',True, True, ListBoxBackLinks.Items[ListBoxBackLinks.ItemIndex]);
        end else
    end;
end;

procedure TEditBoxForm.ShowBackLinks();
var
    //BackLinks : TFormBackLinks;
    Stl : TStringList;
    //BackTitle : string = '';
begin
    if not Sett.AutoSearchUpdate then begin
       showmessage('Back Links only available in Search While You Type mode');
       exit;
    end;
    Stl := TStringList.Create;
    try
        TheMainNoteLister.SearchContent(lowercase(NoteTitle), Stl);
        if Stl.Count > 0 then
            LabelBackLinks.Caption := rsNotesLinked
        else LabelBackLinks.Caption := rsNoOtherNotes;
        PanelBackLinks.Visible := True;
        PanelBackLinks.BringToFront;
        ListBoxBackLinks.Items := Stl;
   finally
        Stl.Free;
   end;
end;

procedure TEditBoxForm.SpeedButtonLinkClick(Sender: TObject);
var
    ThisTitle : ANSIString;
    Index : integer;

//  Abandon the click on "file://" model for now. Too complicated to explain IMHO
(*    function SetupFileLink(CurrCursor : integer) : boolean;
    var BlockNo, BlockOffset, TextLen : integer;
    begin
       Result := False;
       BlockNo := KMemo1.Blocks.IndexToBlockIndex(CurrCursor, BlockOffset);
       if KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoHyperLink') then begin
           if KMemo1.Blocks.Items[BlockNo].Text = FileLinkToken then begin
                // move cursor to end of Link Block's text
                TextLen := UTF8Length(KMemo1.Blocks.Items[BlockNo].Text);
                KMemo1.SelStart := KMemo1.Blocks.SelStart + TextLen - BlockOffset;
                KMemo1.SelEnd := KMemo1.SelStart;
                debugln('TEditBoxForm.SpeedButtonLinkClick cursor BK=' + BlockNo.tostring + ' Cu=' + CurrCursor.tostring);
                BuildFileLink(True, '', CurrCursor);           // default here is file. Only way to get dir is right click menu.
                Result := True;
                exit;
           end;
       end;
    end;        *)

begin
    { May be called with some text selected in which case it will try and create a
    new note with that title. If nothing selected, it populates and shows the
    BackLinks panel. A click on an item there will open that (backlinked) note. }


    if KMemo1.ReadOnly then exit();
    if KMemo1.Blocks.RealSelLength > 1 then begin
         // ToDo : we should force a legal link here. eg, if no whitespace abound it, it will make a new note but text will not be linked.
         // so, if necessary, force a space between ajoining text ?  But is that what the end user expects ?
         // or should we refuse to make a link where the term does not qualify ?
        ThisTitle := KMemo1.SelText;
         // Titles must not start or end with space or contain low characters
        ThisTitle := trim(ThisTitle);
//        while ThisTitle[1] = ' ' do UTF8Delete(ThisTitle, 1, 1);
//        while ThisTitle[UTF8Length(ThisTitle)] = ' ' do UTF8Delete(ThisTitle, UTF8Length(ThisTitle), 1);
        Index := Length(ThisTitle);
        while Index > 0 do begin
            if ThisTitle[Index] < ' ' then delete(ThisTitle, Index, 1);
            dec(Index);
		end;
        if UTF8Length(ThisTitle) > 1 then begin
            SearchForm.OpenNote(ThisTitle);
            KMemo1Change(self);
		end;
    end else begin
//        if not (SetUpFileLink(KMemo1.Blocks.SelStart)
//                    or SetUpFileLink(KMemo1.Blocks.SelStart-1)) then
        ShowBackLinks;
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


procedure TEditBoxForm.BulletControl(MoreBullet : boolean);
var
    BlockNo : longint = 1;
    FirstParaBlockNo, LastParaBlockNo, LocalIndex : integer;
begin
    if KMemo1.ReadOnly then exit();
    MarkDirty();
    BlockNo := Kmemo1.Blocks.IndexToBlockIndex(KMemo1.Blocks.RealSelStart, LocalIndex);
    FirstParaBlockNo := Kmemo1.Blocks.GetNearestParagraphBlockIndex(BlockNo);
    BlockNo := Kmemo1.Blocks.IndexToBlockIndex(KMemo1.Blocks.RealSelEnd, LocalIndex);
    LastParaBlockNo := Kmemo1.Blocks.GetNearestParagraphBlockIndex(BlockNo);
    while FirstParaBlockNo <= LastParaBlockNo  do begin
        // Skip a block if its got LeftPadding AND Numbering is pnuNone
        // SetBullet if LeftPadding = 0 OR Numbering > pnuNone
        if ((TKMemoParagraph(KMemo1.Blocks.Items[FirstParaBlockNo]).Numbering > pnuNone)
            or (TKMemoParagraph(KMemo1.Blocks.Items[FirstParaBlockNo]).ParaStyle.LeftPadding = 0)) then
                SetBullet(TKMemoParagraph(KMemo1.Blocks.Items[FirstParaBlockNo]), MoreBullet);
        inc(FirstParaBlockNo);                                                  // but thats just a text block, so ...
        FirstParaBlockNo := Kmemo1.Blocks.GetNearestParagraphBlockIndex(FirstParaBlockNo);
        if FirstParaBlockNo < 0 then break;
    end;
end;

// Here we deal with a BS at the start of a line. We should have already checked thats were we are.
//

function TEditBoxForm.BackSpaceBullet() : boolean;
var
    BlockNo, PosInBlock : longint;                  // refers to current cursor position
    ParaBlockNo : integer = -1;
    IsBulletPara : boolean = false;                 // The para the cursor is in.
begin
    Result := false;
    ParaBlockNo := Kmemo1.NearestParagraphIndex;                                      // Thats para blockno. That controls line appearance.
    if ParaBlockNo < 2 then exit;                                                     // Dont mess with title.
    BlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, PosInBlock);      // Block with cursor
//    if PosInBlock > 0 then exit;                                                // not the sort of BS we are looking for.    XXX unneeded, we know we are on first char

    // we may be on a bullet or an indent. Importantly, the PREVIOUS line might be bullet or Indent.
    // when we BS (from first char), we must ALWAYS cp previous para characters [.Numbering, .parastyle.leftoffset]
    // unless we have existing bullet/Insert on current.


    // ======== BS when on first char of bullet line, Reduce Bullet one level ========
    if TKMemoParagraph(KMemo1.Blocks[ParaBlockNo]).Numbering <> pnuNone then begin    // ie, this is a bullet para.
        IsBulletPara := True;
        if kmemo1.Blocks.Items[BlockNo-1].ClassNameIs('TkMemoParagraph') then begin   // Cursor is on first char of a bullet para.
            SetBullet(TKMemoParagraph(KMemo1.Blocks[ParaBlockNo]), False);
            // debugln('TEditBoxForm.BackSpaceBullet - reduce bullet level');
            exit(True);                                                               // My work here is done.
        end;
    end;

    // =============== BS on a Indent, reduce indent by one level ==============
    if (not IsBulletPara) and (TKMemoParagraph(KMemo1.Blocks[ParaBlockNo]).ParaStyle.LeftPadding > 0) then begin
         TKMemoParagraph(KMemo1.Blocks[ParaBlockNo]).ParaStyle.LeftPadding
                := TKMemoParagraph(KMemo1.Blocks[ParaBlockNo]).ParaStyle.LeftPadding - IndentWidth;
         // debugln('TEditBoxForm.BackSpaceBullet - reduce indent level');
         exit(True);
    end;

    // If previous para is already a bullet or Indent and current one is not, we set current one to  same
    // level as previous and allow BS to do the rest. BlockNo would have been set above
    // and we know we are on the first char of the paragraph.
    if (BlockNo > 0) and (not IsBulletPara) then begin                                // Cursor is on first char of a non-bullet para.
        if kmemo1.Blocks.Items[BlockNo-1].ClassNameIs('TkMemoParagraph')             // previous block is a para ......
                and (TKMemoParagraph(KMemo1.Blocks[BlockNo-1]).ParaStyle.LeftPadding > 0) then begin    // ie above line is bullet or indent


//             and (TKMemoParagraph(KMemo1.Blocks[BlockNo-1]).Numbering <> pnuNone)     // and previous block is bullet !
//                    or (TKMemoParagraph(KMemo1.Blocks[BlockNo-1]).ParaStyle.LeftPadding > 0) then begin     // both tests unnecessary, we probably just need leftpadding ...


            TKMemoParagraph(KMemo1.Blocks[ParaBlockNo]).Numbering := TKMemoParagraph(KMemo1.Blocks[BlockNo-1]).Numbering;
            TKMemoParagraph(KMemo1.Blocks[ParaBlockNo]).ParaStyle.LeftPadding := TKMemoParagraph(KMemo1.Blocks[BlockNo-1]).ParaStyle.LeftPadding;

//            writeln(TKMemoParagraph(KMemo1.Blocks[ParaBlockNo]).ParaStyle.LeftPadding);

            // SetBullet(TKMemoParagraph(KMemo1.Blocks[ParaBlockNo]), True, TKMemoParagraph(KMemo1.Blocks[BlockNo-1]).Numbering);
            debugln('TEditBoxForm.BackSpaceBullet - cp upper bullet level to current line, leftpadding=' + inttostr(TKMemoParagraph(KMemo1.Blocks[ParaBlockNo]).ParaStyle.LeftPadding));
            exit(False);                                                             // lets BS go through to KMemo.
         end;
    end;
end;


// ================ No, still not right !  A delete at end of Indent line uses following line's bullet/indent char, not current !!

(*
function TEditBoxForm.NearABulletPoint(out Leading, Under, Trailing, IsFirstChar, NoBulletPara : Boolean;            // ToDo : No longer needed ?
        								out BlockNo, TrailOffset, LeadOffset : longint ) : boolean;
	// on medium linux laptop, 20k note this function takes less than a mS
    // Jan 2024, I define Under and meaning cursor anywhere in a bullet para, not sure what it was before.
var
    PosInBlock, Index, CharCount : longint;
    ParaBlockNo : integer;
begin


   exit;



    Under := False;
    NoBulletPara := False;
    BlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, PosInBlock);      // Block with cursor
    if BlockNo < 2 then exit(False);                                                  // we are in title
    IsFirstChar := PosInBlock = 0;
    ParaBlockNo := Kmemo1.NearestParagraphIndex;                                // First para marker after cursor pos.
    Under := TKMemoParagraph(KMemo1.Blocks[ParaBlockNo]).Numbering = pnuBullets;

//    Under := Kmemo1.Blocks.GetNearestParagraphBlock(ABlockNo).Numbering = pnuBullets; // Gets the following one, not the nearest !
    NoBulletPara := not Under;                                                        // Why have both ?????

{    if kmemo1.blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin
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
    end else  }

    Index := 1;

    while (BlockNo-Index) > 1 do begin                 // Look for previous paragraph marker.
        if  kmemo1.blocks.Items[BlockNo-Index].ClassNameIs('TKMemoParagraph') then begin
            Leading := TKMemoParagraph(kmemo1.blocks.Items[BlockNo]).Numbering = pnuBullets;
            break;
        end;
        inc(Index);
    end;
    Index := 1;
    while (ParaBlockNo + Index) < Kmemo1.Blocks.Count do begin
        if  kmemo1.blocks.Items[BlockNo+Index].ClassNameIs('TKMemoParagraph') then begin
            Trailing := TKMemoParagraph(kmemo1.blocks.Items[BlockNo]).Numbering = pnuBullets;
            break;
        end;
        inc(Index);
    end;

    // are we done yet ?





//    Leading := (TKMemoParagraph(kmemo1.blocks.Items[BlockNo-Index]).Numbering = pnuBullets);

//    IsFirstChar := (CharCount = 0);
//    LeadOffset := Index;
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
end;                  *)

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
    Marker, we stop that and remove the last character of the visible string. (can no longer select Para)  n.a 2024

b   If the cursor is at the begininng of a Bullet Text we must cancel the bullet (which is at the
    end of the Text) and not merge this line with one above. We know this is the case if the
    trailing paragraph marker is bullet AND we are the first char of the first block of the text.   Confirmed

c   If the cursor is on next line after a bullet, on a para marker that is not a bullet and there
	is no text on that line after the cursor, all we do is delete that para marker.
                                                                                                    Confirmed
d   Again, we are on first char of the line after a bullet, this line is not a bullet itself
	and it has some text after the cursor. We merge that text up to the bullet line above,
    retaining its bulletness. So, mark trailing para bullet, delete leading.


x	A blank line, no bullet between two bullet lines. Use BS line should dissapear.
    That is, delete para under cursor, move cursor to end line above. This same as c               Confirmed but not c

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

procedure TEditBoxForm.SetBullet(PB : TKMemoParagraph; Bullet : boolean; Target : TKMemoParaNumbering = pnuNone);
{var
   Index : integer;
   Tick, Tock : qword; }
   procedure ShowBulletLevel();
   begin
   case PB.Numbering of
       pnuNone : debugln('TEditBoxForm.SetBullet pnuNone ' + booltostr(Bullet, True));
       BulletOne : debugln('TEditBoxForm.SetBullet pnuOne ' + booltostr(Bullet, True));
       BulletTwo : debugln('TEditBoxForm.SetBullet pnuTwo ' + booltostr(Bullet, True));
       BulletThree : debugln('TEditBoxForm.SetBullet pnuThree ' + booltostr(Bullet, True));
       BulletFour : debugln('TEditBoxForm.SetBullet pnuFour ' + booltostr(Bullet, True));
       BulletFive : debugln('TEditBoxForm.SetBullet pnuFive ' + booltostr(Bullet, True));
       BulletSix : debugln('TEditBoxForm.SetBullet pnuSix ' + booltostr(Bullet, True));
   end;
   end;

   procedure SetBulletParameters(Bull: TKMemoParaNumbering; FirstIndent, LeftIndent : integer);
   begin

        if Verbose then debugln('In SetBulletParameter numbering is #', inttostr(ord(PB.Numbering))
            + ' Indent set to ', inttostr(LeftIndent));
        if Bull > pnuNone then begin
            PB.Numbering := pnuNone;
            PB.Numbering := Bull;
            PB.NumberingListLevel.FirstIndent := FirstIndent;
            PB.NumberingListLevel.LeftIndent:= LeftIndent;
        end else begin
            PB.Numbering := pnuNone;
            PB.ParaStyle.FirstIndent := 0;
            PB.ParaStyle.LeftPadding := 0;
        end;
   end;

begin
    // KMemo declares a number of Bullets/Paragraph number thingos. We map
    // BulletOne .. BulletEight to them in tb_utils. Change order/appearance there.
    // You cannot have different blocks using same bullet (ie pnuBullet,
    // pnuArrowBullet) with different indent levels. Its a KMemo thing.
    // The numbers here must match what we use in Loadnote, should be constants too.
    // Here I set the different bullet indents each and every time they are used.
    // ToDo : can I initialise the different bullet indents during startup ?


   //ShowBulletLevel();
//   if Verbose then debugln('In SetBullet Start and PB numbering is #', inttostr(ord(PB.Numbering)));
//   DumpKMemo('Start of SetBullet()');
   KMemo1.Blocks.lockUpdate;
   if Target <> pnuNone then
        PB.Numbering := Pred(Target);                       // Preload with pred so we get Target
    try
        case PB.Numbering of
            pnuNone :     if Bullet then
                               SetBulletParameters(BulletOne, -20, 30);
            BulletOne :   if Bullet then
                               SetBulletParameters(BulletTwo, -20, 50)
                          else begin
                               SetBulletParameters(pnuNone, 0, 10);   // Note, numeric parametrs ignored in "less bullet"
                               //PB.ParaStyle.LeftPadding := 0;
                          end;
            BulletTwo :   if Bullet then
                               SetBulletParameters(BulletThree, -20, 70)
                          else SetBulletParameters(BulletOne, -20, 30);
            BulletThree : if Bullet then
                               SetBulletParameters(BulletFour, -20, 90)
                          else SetBulletParameters(BulletTwo, -20, 50);
            BulletFour :  if Bullet then
                               SetBulletParameters(BulletFive, -20, 110)
                          else SetBulletParameters(BulletThree, -20, 70);
            BulletFive :  if Bullet then
                               SetBulletParameters(BulletSix, -20, 130)
                          else SetBulletParameters(BulletFour, -20, 90);
            BulletSix :   if Bullet then
                               SetBulletParameters(BulletSeven, -20, 150)
                          else SetBulletParameters(BulletFive, -20, 110);
            BulletSeven : if Bullet then
                               SetBulletParameters(BulletEight, -20, 170)
                          else SetBulletParameters(BulletSix, -20, 130);
            BulletEight : if not Bullet then
                               SetBulletParameters(BulletSeven, -20, 150);
            end;                // end of case statement


(*            case PB.Numbering of
                pnuNone :   if Bullet then begin
                                PB.Numbering := BulletOne;
                                PB.NumberingListLevel.FirstIndent:=-20;
                                PB.NumberingListLevel.LeftIndent := 30;
                            end;
                BulletOne : begin
                                //PB.Numbering:=pnuNone;
                                //PB.ParaStyle.NumberingListLevel := -1;
                                if Bullet then begin
                                    PB.Numbering := BulletTwo;
                                    PB.NumberingListLevel.FirstIndent:=-20;
                                    PB.NumberingListLevel.LeftIndent := 50;
                                end else begin
                                    PB.NumberingListLevel.FirstIndent := 0;     // Must set before turning off PB.Numbering
                                    PB.Numbering := pnuNone;               // new, Jan 2024
                                    PB.ParaStyle.NumberingListLevel := -1;
                                    PB.ParaStyle.LeftPadding := 0;
                                end;
                            end;
                BulletTwo : begin
                                //PB.Numbering:=pnuNone;
                                if Bullet then begin
                                    PB.Numbering := BulletThree;
                                    PB.NumberingListLevel.FirstIndent:=-20;
                                    PB.NumberingListLevel.LeftIndent := 70;
                                end else PB.Numbering := BulletOne;
                            end;
                BulletThree : begin
                                  //  PB.Numbering:=pnuNone;
                                    if Bullet then begin
                                        PB.Numbering := BulletFour;
                                        PB.NumberingListLevel.FirstIndent:=-20;
                                        PB.NumberingListLevel.LeftIndent := 90;
                                    end else PB.Numbering := BulletTwo;
                              end;
                BulletFour : begin
                                    //PB.Numbering:=pnuNone;
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
                end;                // end of case statement        *)
    finally
        KMemo1.Blocks.UnlockUpdate;
    end;

//    DumpKMemo('End of SetBullet()');
    //if Verbose then debugln('In SetBullet End   and PB numbering is #', inttostr(ord(PB.Numbering)));
    //ShowBulletLevel();
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
    if Button = mbMiddle then begin            // middle button or primary paste
        Point := TPoint.Create(X, Y);          // X and Y are pixels, not char positions !
        LinePos := eolEnd;
        while X > 0 do begin                   // we might be right of the eol marker.
            KMemo1.PointToIndex(Point, true, true, LinePos);
            if LinePos = eolInside then break;
            dec(Point.X);
        end;
        PrimaryPaste(KMemo1.PointToIndex(Point, true, true, LinePos));
        exit();
    end;
    if KMemo1.SelAvail and                     // if not paste, better update primary
        (Kmemo1.Blocks.SelLength <> 0) then
            SetPrimarySelection()
        else
            UnsetPrimarySelection();
    {$endif}
    if (Button = mbLeft) and ([ssShift] = Shift) then begin  // select from existing pos to clicked pos
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
{$ifdef TDEBUG}var
    Tick, Tock : integer; {$endif}
// const AlreadyCalled: boolean = False;           // Typed Constant, remembered but shared with all instances !
begin
    // writeln('TEditBoxForm.FormActivate AlreadyCalled is ', booltostr(HaveSeenOnActivate, True));
    if not HaveSeenOnActivate then begin;
        Ready := False;
        {$ifdef LCLGTK3}debugln('TEditBoxForm.FormActivate');{$endif}
        {$ifdef TDEBUG}Tick := gettickcount64();{$endif}
        if Sett.ShowIntLinks then CheckForLinks(True);
        {$ifdef TDEBUG}
        Tock := gettickcount64();
        debugln('+++++++++++ OnActivate CheckForLinks() ' + inttostr(Tock - Tick) + 'mS' + ' HaveSeen=' + booltostr(HaveSeenOnActivate, true));
        {$endif}
        if SearchedTerm <> '' then               // We must do search after CheckForLinks
            NewFind(SearchedTerm);
        TimerHouseKeeping.Enabled := False;
        if SingleNoteMode then begin
            SpeedbuttonSearch.Enabled := False;
            SpeedButtonLink.Enabled := False;
            MenuItemSync.Enabled := False;
            SpeedButtonNotebook.Enabled := False;
        end;
        HaveSeenOnActivate := True;
        Ready := True;
        {$ifdef LCLGTK3}debugln('TEditBoxForm.FormActivate finished');{$endif}
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
                        KMemo1.SetFocus;
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
   if FindInNoteBack(lowercase(EditFind.Text)) then
        LabelFindInfo.Caption := {$ifdef DARWIN}
            rsFindNavRightHintMac + ', ' + rsFindNavLeftHintMac
            {$else}rsFindNavRightHint + ', ' + rsFindNavLeftHint{$endif}
      else LabelFindInfo.Caption := rsNotAvailable;
    if PanelFind.Height = SearchPanelHeight then
        EditFind.SetFocus;
end;


procedure TEditBoxForm.FindNew(IncStartPos : boolean);
var Found : boolean;
begin
    if (EditFind.Caption = '') or (EditFind.Caption = rsMenuSearch) then exit;
    if IncStartPos then
        Found := FindInNote(lowercase(EditFind.Text), 1)
    else Found := FindInNote(lowercase(EditFind.Text), 0);
    if Found then
        LabelFindInfo.Caption := {$ifdef DARWIN}
            rsFindNavRightHintMac + ', ' + rsFindNavLeftHintMac
            {$else}rsFindNavRightHint + ', ' + rsFindNavLeftHint{$endif}
      else
        LabelFindInfo.Caption := rsNotAvailable;
    if PanelFind.Height = SearchPanelHeight then
        EditFind.SetFocus;
end;

procedure TEditBoxForm.SpeedRightClick(Sender: TObject);    // think btNext
begin
    FindNew(True);
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
                KMemo1.ExecuteCommand(ecDown);
                KMemo1.ExecuteCommand(ecDown);
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
end;

procedure TEditBoxForm.MenuItemDeleteClick(Sender: TObject);
begin
    if KMemo1.ReadOnly then exit();
    // KMemo1.ExecuteCommand(ecClearSelection);
    Undoer.AddPasteOrCut(True);
    KMemo1.Blocks.ClearSelection;
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

procedure TEditBoxForm.MenuItemCopyPlainClick(Sender: TObject);
begin
    Clipboard.AsText := KMemo1.Blocks.SelText;
end;

    // Called if MDContent appears to have one of more possible tokens.
    // Looks to see if it can find a <title>.tokens file, if so, askes user
    // if they want to use it. If yes, does subsitutions.      -- no questioned asked at present !
    // See function FindToken() for better docs.

procedure TEditBoxForm.QuestionToken(MDContent : TStringList; NoteFName, NoteSaveName : string);
var
    Tokens : TStringList;
    TokensFName, AToken, NewText, St, AName : string;
    MDIndex : integer = 0;
    TokIndex : integer = 0;

    function DoTokens() : boolean;   // resolve any tokens in passed string
    // Looks at content in St, if necessary, changes it returning True.
    begin
        Result := false;
        TokIndex := 0;
        while FindToken(St, TokIndex, AToken) do begin
//           WRITELN('DoToken found possible token : ' + AToken);
           NewText := Tokens.Values[AToken];
           if NewText = '' then begin
               debugln('Note : TEditBoxForm.QuestionToken did not match token : ' + AToken);
           end else  begin
//               WRITELN('TEditBoxForm.QuestionToken replacing ', AToken, ' with ', NewText);
               St := St.Replace(AToken, NewText, []);
               Result := True;
//               debugln('Now looks like ' + St);
           end;
        end;
    end;

begin
//    TokensFName := CleanCaption() + '.tokens';
    TokensFName := ChangeFileExt(NoteFName, '.tokens');  // that should cover single note mode
    if not FileExists(TokensFName) then begin
        debugln('Note : TEditBoxForm.QuestionToken did not find token file : ', TokensFName);
        TokensFName := ExtractFilePath(NoteSaveName) + TB_MakeFileName(CleanCaption()) + '.tokens';
        if not FileExists(TokensFName) then begin
            debugln('Note : TEditBoxForm.QuestionToken did not find token file : ', TokensFName);
            exit;
        end;
    end;
    debugln('Note : TEditBoxForm.QuestionToken is using token file : ', TokensFName);
    // if to here, TokensFName has name of real file.
    Tokens := TStringList.Create;
    try
        Tokens.LoadFromFile(TokensFName);
        while MDIndex < Tokens.Count do begin       // Resolved nested Tokens
            AName := Tokens.Names[MDIndex];
            if AName <> '' then begin
                St := Tokens.Values[AName];
                if DoTokens() then
                    Tokens.Values[AName] := St;;
            end;
            inc(MDIndex);
        end;
        MDIndex := 0;
        while MDIndex < MDContent.Count do begin
            St := MDContent[MDIndex];
            if DoTokens() then
                MDContent[MDIndex] := St;
             inc(MDIndex);
        end;
    finally
        Tokens.Free;
    end;
end;

procedure TEditBoxForm.SaveNoteAs(TheExt : string);
var
    SaveExport : TSaveDialog;
    MDContent : TStringList;
    ExpComm   : TExportCommon;
    FName : string;
    SleepCount : integer =0;
    Form2pdf: TFormKMemo2pdf = nil;
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
            SaveExport.InitialDir := Sett.HomeDir;
        end;
    end;
    //debugln('TEditBoxForm.SaveNoteAs Filename 1 = ' + CleanCaption());
    //debugln('TEditBoxForm.SaveNoteAs Filename 2 = ' + TB_MakeFileName(CleanCaption()));
    SaveExport.Filename := TB_MakeFileName(CleanCaption()) + '.' + TheExt;
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
                            if ExpComm.GetMDcontent( FName, MDContent) then begin
                                if ExpComm.HasToken then
                                    QuestionToken(MDContent, FName, SaveExport.FileName);
                                MDContent.SaveToFile(SaveExport.FileName)
                            end else showmessage('Failed to convert to MarkDown ' + ExpComm.ErrorMsg);
                        finally
                            ExpComm.Free;
                            MDContent.Free;
                        end;
                    end;
            'pdf' : begin
                        // LoadFontConfigLib('libfontconfig.so.1',false);
                        Form2pdf := TFormKMemo2pdf.create(self);
                        Form2pdf.ParentLeft := Left;        // Qt needs it done this way ???
                        Form2pdf.ParentTop := Top;
                        Form2PDF.TheKMemo := KMemo1;
                        Form2PDF.DefaultFont := sett.UsualFont;
                        Form2PDF.FFileName := SaveExport.FileName;
                        Form2PDF.TheTitle := Caption;
                        Form2PDF.ShowModal;
                        Form2pdf.free;
                    end;
        end;
    end;
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


// ================================ S Y M B O L S  =============================

procedure TEditBoxForm.SymbolMenuClicked(Sender: TObject);
begin
    if (TMenuItem(Sender).Caption = rsMenuSettings) then begin
        if FormSymbol.ShowModal = mrOK then begin
            Sett.SaveSettings(self);
            PopulateSymbolMenu(PopupMenuSymbols);
            // debugln('Make out we saved the symbols');
        end;
    end else begin
        KMemo1.ExecuteCommand(ecInsertString, pchar(TMenuItem(Sender).Caption));
        KMemo1.ExecuteCommand(ecRight);
    end;
end;

procedure TEditBoxForm.AddItemMenu(TheMenu : TPopupMenu; Item : string; mtTag : integer; OC : TNotifyEvent);
var
    MenuItem : TMenuItem;
begin
    if Item = '-' then begin
        TheMenu.Items.AddSeparator;
        exit();
    end;
    MenuItem := TMenuItem.Create(Self);
    MenuItem.Tag := ord(mtTag);             // for 'File' entries, this identifies the function to perform.
    MenuItem.Caption := Item;
    MenuItem.OnClick := OC;
    TheMenu.Items.Add(MenuItem);
end;

procedure TEditBoxForm.PopulateSymbolMenu(AMenu : TPopupMenu);
var
    i : integer;
begin
   i := AMenu.Items.Count;
   while i > 0 do begin            // Remove any existing entries first
       dec(i);
       AMenu.Items.Delete(i);
   end;
   for i := 0 to high(SymArray) do begin
        AddItemMenu(AMenu, SymArray[i].Sym, i,  @SymbolMenuClicked);
   end;
   AddItemMenu(AMenu, '-', -1,  @SymbolMenuClicked);
   AddItemMenu(AMenu, rsMenuSettings, -1,  @SymbolMenuClicked);
end;

// ============== H O U S E   K E E P I N G   F U C T I O N S ==================

procedure TEditBoxForm.FormCreate(Sender: TObject);
begin
    MenuItemInsertDirLink.Caption := rsInsertDirLink;           // This to stop duplicates in .po files
    MenuItemInsertFileLink.Caption := rsInsertFileLink;
    MenuItemToolsInsertDirLink.Caption := rsInsertDirLink;
    MenuItemToolsInsertFileLink.Caption := rsInsertFileLink;

    PopulateSymbolMenu(PopupMenuSymbols);
    Use_Undoer := Sett.CheckUseUndo.checked;    // Note, user must close and repen if they change this setting
    if Use_Undoer then
        Undoer := TUndo_Redo.Create(KMemo1)
    else Undoer := Nil;
//    SpeedClose.Caption := ' ' + copy(rsMBClose, 2, 20) + ' ';    // chop off the initial '&'
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

procedure TEditBoxForm.SpeedSymbolClick(Sender: TObject);
begin
    PopupMenuSymbols.popup;
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


procedure TEditBoxForm.SetTheColors;
begin
   KMemo1.Blocks.LockUpdate;
   {$ifdef windows}
   // Color:= Sett.textcolour;
   if Sett.DarkTheme then Color := Sett.BackGndColour;
   {$endif}
   if Sett.DarkThemeSwitch then begin
       PanelFind.Color := Sett.AltColour;
       PanelButtons.Color := Sett.AltColour;
   end;
   KMemo1.Colors.SelTextFocused := Sett.TextColour;
   KMemo1.Colors.SelText := Sett.TextColour;               // when looses focus
   KMemo1.Colors.BkGnd:= Sett.BackGndColour;
   KMemo1.Colors.SelBkGnd := Sett.AltBackGndColor;         // Selected backgnd when loses focus
   KMemo1.Colors.SelBkGndFocused := Sett.AltBackGndColor;  // Selected backgnd with focus
   Kmemo1.Blocks.DefaultTextStyle.Font.Color  := Sett.TextColour;
   Kmemo1.Blocks.DefaultTextStyle.Brush.Color := Sett.BackGndColour;
   KMemo1.Blocks.UnLockUpdate;
end;

procedure TEditBoxForm.FormShow(Sender: TObject);
var
    ItsANewNote : boolean = false;
//    Tick, Tock, Tuck : qword;
begin
     {$ifdef LCLGTK3}debugln('TEditBoxForm.FormShow started');{$endif}
    if Ready then exit;                         // its a "re-show" event. Already have a note loaded.
    // Ready := False;                             // But it must be false aready, it was created FALSE
//    Tick := GetTickCount64();
    PanelReadOnly.Height := 0;                  // '1' is visible as a yellow line in dark theme
    TimerSave.Enabled := False;
    KMemo1.Font.Size := Sett.FontNormal;
    KMemo1.Font.Name := Sett.UsualFont;
//    KMemo1.Colors.SelBkGnd := Sett.BackGndColour;
    {$ifdef LCLGTK2}
    KMemo1.ExecuteCommand(ecPaste);   // this to deal with a "first copy" issue on Linux.
    // above line generates a gtk2 assertion but only in single note mode.  I suspect
    // thats because its a modal form and in normal use, this window is not modal.
    // If we don't make above call in SNM, we get the same assertion sooner or later, as soon
    // as we select some text so may as well get it over with. No need to do it in Qt5, Win, Mac
    {$endif}
    Kmemo1.Clear;      // Note clear and setcolors() will be called again in importNote() but quick ...
    SetTheColors();
    if SingleNoteMode then begin
        ItsANewNote := LoadSingleNote();     // Might not be Tomboy XML format
        if ItsANewNote then
           NoteTitle := ExtractFileNameOnly(NoteFileName);  // remove path and extension
    end else
        if NoteFileName = '' then begin		    // might be a new note or a new note from Link
            if NoteTitle = '' then              // New Note
			    NoteTitle := NewNoteTitle();
            ItsANewNote := True;
	    end else begin
            Caption := NoteFileName;
     	    ImportNote(NoteFileName);		    // also sets Caption and Createdate, clears 260mS
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
        MarkTitle();
	end;
//    Tock := gettickcount64();
//    MarkTitle();                                // 70mS, ImortNote() does it all now. Not needed for new note

    KMemo1.SelStart := KMemo1.Text.Length;      // set curser pos to end
    KMemo1.SelEnd := Kmemo1.Text.Length;
    KMemo1.SetFocus;
    KMemo1.executecommand(ecEditorTop);
    KMemo1.ExecuteCommand(ecDown);
    Ready := true;
    Dirty := False;
    PanelBackLinks.Visible := false;
    {$ifdef LCLGTK3}debugln('TEditBoxForm.FormShow finished');{$endif}
//    writeln('TEditBoxForm.FormShow 4 SelIndex = ', Kmemo1.SelStart);

//    Tuck := GetTickCount64();
//    debugln('TEditBoxForm.FormShow ' + inttostr(Tock - Tick) + '  ' + inttostr(Tuck - Tock) + ' Total=' + inttostr(Tuck - Tick));
end;


    { called when user manually closes this form. }
procedure TEditBoxForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
//    debugln('TEditBoxForm.FormClose called ' + DateTimeToStr(Now) + ' form=' + Caption); // ToDo : remove me
    Release;
end;

procedure TEditBoxForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
//    debugln('TEditBoxForm.FormCloseQuery called ' + DateTimeToStr(Now) + ' form=' + Caption); // ToDo : remove me
    CanClose := True;
end;

procedure TEditBoxForm.FormDestroy(Sender: TObject);
begin
//    debugln('TEditBoxForm.FormDestroy called ' + FormatDateTime('hh:nn:ss.zzz', Now()) + ' form=' + Caption); // ToDo : remove me
    if Undoer <> Nil then Undoer.free;
    UnsetPrimarySelection;                                      // tidy up copy on selection.
    if (length(NoteFileName) = 0) and (not Dirty) then exit;    // A new, unchanged note, no need to save.
    if not Kmemo1.ReadOnly then
        if not DeletingThisNote then
            if (not SingleNoteMode) or Dirty then       // We always save, except in SingleNoteMode (where we save only if dirty)
                SaveTheNote(Sett.AreClosing);           // Jan 2020, just call SaveTheNote, it knows how to record the notebook state
//    debugln('TEditBoxForm.FormDestroy marking Nil ' + FormatDateTime('hh:nn:ss.zzz', Now()) + ' form=' + Caption); // ToDo : remove me
    SearchForm.NoteClosing(NoteFileName);
    // In single note mode, form is modal, don't call Application.ProcessMessage at this stage
    if not SingleNoteMode then
        Application.ProcessMessages;
//    debugln('TEditBoxForm.FormDestroy finished ' + FormatDateTime('hh:nn:ss.zzz', Now()) + ' form=' + Caption); // ToDo : remove me
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
    {$ifdef LCLGTK3}debugln('TEditBoxForm.timerSaveTimer');{$endif}
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
//    Tick, Tock, Tuck : qword;           // ToDo : remove
begin
//  	if Not Ready then exit();               // ToDo : what is effect of disabling this ?
    { if there is more than one block, and the first, [0], is a para, delete it.}
    if KMemo1.Blocks.Count <= 2 then exit();	// Don't try to mark title until more blocks.
    Ready := false;

    Kmemo1.Blocks.LockUpdate;
    if Kmemo1.Blocks.Items[BlockNo].ClassName = 'TKMemoParagraph' then
          Kmemo1.Blocks.DeleteEOL(0);
	try
//        Tick := GetTickCount64();       // this while loop can take 70mS when loading a big note, 100mS when updating title in a big note
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
//        Tock := GetTickCount64();
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
//    Tuck := GetTickCount64();
//    debugln('TEditBoxForm.MarkTitle ' + inttostr(Tock - Tick) + '  ' + inttostr(Tuck - Tock) + ' Total=' + inttostr(Tuck - Tick));
end;

// This is a debug method, take care, it uses writeln and will kill Windows !
procedure TEditBoxForm.DumpKMemo(WhereFrom : string);
var
    i, x : integer;
    S : string;
begin
    Debugln('  TEditBoxForm.DumpKMemo from ' + WhereFrom);
    for i := 0 to Kmemo1.Blocks.Count-1 do begin
        S := '  ' + inttostr(i) + ' ';
        if KMemo1.Blocks.Items[i].ClassNameIs('TKMemoTextBlock') then begin
            S := S + 'text : ' + KMemo1.Blocks.Items[i].Text;
            S := S + ' ';
            if fsUnderline in TKMemoTextBlock(KMemo1.Blocks[i]).TextStyle.Font.Style then
                S := S + 'u '
            else S := S + '  ';
            for x := 1 to length( KMemo1.Blocks.Items[i].Text) do
                S := S + ' ' + inttostr(ord(KMemo1.Blocks.Items[i].Text[x]));
        end;
        if KMemo1.Blocks.Items[i].ClassNameIs('TKMemoHyperlink') then begin
            S := S + 'Link : ' + KMemo1.Blocks.Items[i].Text;
            for x := 1 to length( KMemo1.Blocks.Items[i].Text) do
                S := S + ' ' + inttostr(ord(KMemo1.Blocks.Items[i].Text[x]));
        end;
        if KMemo1.Blocks.Items[i].ClassNameIs('TKMemoParagraph') then begin
            S := S + 'para numb=' + inttostr(ord(TKMemoParagraph(KMemo1.blocks.Items[i]).Numbering));
            //if TKMemoParagraph(KMemo1.blocks.Items[i]).Numbering > pnuNone then begin
            //if I in [5, 7] then begin
                S := S + ' LeftPad=' +  inttostr(TKMemoParagraph(KMemo1.blocks.Items[i]).ParaStyle.LeftPadding);
                S := S + ' Indent=' +  inttostr(TKMemoParagraph(KMemo1.blocks.Items[i]).ParaStyle.FirstIndent);
                S := S + ' AppPad=' +  inttostr(TKMemoParagraph(KMemo1.blocks.Items[i]).ParaStyle.AllPaddingsLeft);

            //end;
        end;
        debugln(S);
//        writeln(Inttostr(i) + ' ' + KMemo1.Blocks.Items[i].ClassName + ' = ' + KMemo1.Blocks.Items[i].Text);
//        if KMemo1.Blocks.Items[i].ClassNameIs('TKMemoTextBlock')
//                or KMemo1.Blocks.Items[i].ClassNameIs('TKMemoHyperlink') then
//            writeln('       Bold=' + booltostr(fsBold in TKMemoTextBlock(KMemo1.Blocks.Items[i]).TextStyle.Font.Style, True));
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


// --------- F I L E and D I R E C T O R Y  L I N K  related methods -----------


function TEditBoxForm.TestLinkInsert(IndexNo : integer; out ILA : TiLActionRec) : boolean;
var
    BlockOffset, LeftBlockNo : integer;          // LeadingSpace, trailingSpace, NewTextBlock, BlockNo, Offset
begin
    ILA.BlockNo := KMemo1.Blocks.IndexToBlockIndex(IndexNo, ILA.Offset);
    if ILA.BlockNo < 2 then begin
        ILA.BlockNo := -1;
        exit(False);
    end;
    ILA.NewTextBlock := false;
    LeftBlockNo := KMemo1.Blocks.IndexToBlockIndex(IndexNo-1, BlockOffset);

//    debugln('TEditBoxForm.TestLinkInsert Block is ' + KMemo1.blocks[ILA.BlockNo].ClassName + ' and ' + KMemo1.blocks[ILA.BlockNo-1].ClassName);

    if KMemo1.blocks[ILA.BlockNo].ClassNameIs('TKMemoTextBlock') then begin

        if KMemo1.blocks[ILA.BlockNo].Text.IsEmpty  then                             // untested
            debugln('TEditBoxForm.TestLinkInsert WARNING empty text block ' + dbgs(ILA.BlockNo));

//        debugln('tBoxForm.TestLinkInsert ch=[' + KMemo1.blocks[ILA.BlockNo].Text[ILA.Offset+1] + '] -1=['
//                + KMemo1.blocks[ILA.BlockNo].Text[ILA.Offset] + ']');         // warning, this debug unhappy at end if file

        ILA.TrailingSpace := (KMemo1.blocks[ILA.BlockNo].Text[ILA.Offset+1] <> ' ');   // byte op, BUT WHAT ABOUT AN EMPTY BLOCK ???
        if (ILA.Offset = 0) then begin                                               // we must look at previous block
            if KMemo1.blocks[LeftBlockNo].ClassNameIs('TKMemoParagraph') then begin
                ILA.LeadingSpace := false;                                           // on first char of a para
                exit(True);
            end else begin
                ILA.LeadingSpace := not KMemo1.blocks[LeftBlockNo].Text.EndsWith(' ');  // ASSUMES ITS TEXT - WHAT IF ITS HYPER ?
                exit(True);
            end;
        end else begin
            ILA.LeadingSpace := (KMemo1.blocks[ILA.BlockNo].Text[ILA.Offset] <> ' ');   // byte op
            exit(True);
        end;
    end;                                                                                // end of "if TKMemoTextBlock"
    // we are on a non-text block, 'TKMemoParagraph' or 'TKMemoHyperLink'
    if KMemo1.blocks[ILA.BlockNo].ClassNameIs('TKMemoParagraph') then begin
        // if there is text block to left, append to it, if not, new block
        if KMemo1.blocks[LeftBlockNo].ClassNameIs('TKMemoTextBlock') then begin         // text block to left.
            ILA.LeadingSpace := not KMemo1.blocks[LeftBlockNo].Text.EndsWith(' ');
            ILA.TrailingSpace := False;
            ILA.Offset := length(KMemo1.blocks[LeftBlockNo].Text);
            ILA.BlockNo := LeftBlockNo;
//            debugln('TEditBoxForm.TestLinkInsert ' + KMemo1.blocks[LeftBlockNo].ClassName);
            exit(True);
        end;
        if KMemo1.blocks[LeftBlockNo].ClassNameIs('TKMemoHyperLink') then begin         // Hyper block to left always add space
            ILA.LeadingSpace := true;
            ILA.TrailingSpace := True;
            ILA.NewTextBlock := True;
            exit(True);
        end;
        if KMemo1.blocks[LeftBlockNo].ClassNameIs('TKMemoParagraph') then begin         // Para block to left must insert text block
            ILA.LeadingSpace := false;
            ILA.TrailingSpace := false;
            ILA.NewTextBlock := true;
            exit(True);
        end;
    end;                                                                                // end of "if TKMemoParagraph"
    Result := False;
    // Now, I want to know if char at CurrCursor and at CurrCursor-1 is whitespace.
    // that is space or newline. Cannot use KMemo1.text[CurrCursor] cos CurrCursor is char count, not byte.
    // Possibilities -
    // 1. We are in a .text and offset is > 0, easy we look directly.
    // 2. We are in a .text and offset = 0, must look at previous block. Insert at beginning.
    // 3. We are on a para block and previous block is text, we'll append to previous block
    // 4. We are on para block and previous is also a Para, must insert new text block before it.
end;

// --- revised, deals only with new links !!!!!!!!
function TEditBoxForm.BuildFileLink(ItsFile : Boolean; CharNo: integer = 0): boolean;
var
    AFileName{, HomeDir} : string;
    Blk : TKMemoTextBlock;
    ILR : TiLActionRec;
begin
    result := True;
    if CharNo = 0 then
        CharNo := Kmemo1.RealSelStart;
    if ItsFile then begin                           // its either File or Directory
        OpenDialogFileLink.InitialDir := Sett.HomeDir;
        if not OpenDialogFileLink.Execute then
            exit;
        AFileName := TrimFilename(OpenDialogFileLink.FileName)
    end else begin
        SelectDirectoryForLink.InitialDir := Sett.HomeDir;
        if not SelectDirectoryForLink.execute then
           exit;
        AFileName := TrimFileName(SelectDirectoryForLink.FileName);
    end;
    if copy(AFileName, 1, length(Sett.HomeDir)) = Sett.HomeDir then begin       // is it absolute ?
        AFileName := AFileName.Remove(0, Length(Sett.HomeDir));                 // make it relative, cleaner for user to see.
        if AFileName[1] in ['/', '\'] then                                      // don't think this is possible any more ??
            AFileName := AFileName.Remove(0,1);
    end;
    if TestLinkInsert(CharNo, ILR) then begin
        AFileName := FileLinkToken + '"' + AFileName + '"';       // we always add inverted double commas here ...
        if ILR.LeadingSpace then AFileName := ' ' + AFileName;
        if ILR.TrailingSpace then AFileName := AFileName + ' ';
//        debugln('TEditBoxForm.BuildFileLink before  text=[' + KMemo1.Blocks[ILR.BlockNo].Text + ']');
        if ILR.NewTextBlock then begin
          Blk := KMemo1.Blocks.AddTextBlock(AFileName, ILR.BlockNo);
          Blk.TextStyle.Font.Underline := False;
//          debugln('TEditBoxForm.BuildFileLink - insert block');
        end else begin
            KMemo1.Blocks[ILR.BlockNo].InsertString(AFileName, ILR.Offset);
        end;
//        debugln('TEditBoxForm.BuildFileLink after  text=[' + KMemo1.Blocks[ILR.BlockNo].Text + ']');
        KMemo1.SelStart := KMemo1.SelStart + utf8length(AFileName);
        KMemo1.SelLength := 0;
        MarkDirty();
//        DumpKMemo('BuildFileLink Inserted=[' + AFileName + ']');
    end;
end;


function TEditBoxForm.OpenFileLink(LinkText : string) : boolean;
var
    i : integer;
    Msg : string;
    {$ifdef WINDOWS}        // in Windows, any file is potentially executable, we'll test by extension
    function WindowsFileIsExecutable() : boolean;                          // True if file has extension mentioned in PATHEXT
    var WinExeExt, Extension : string;
    begin
        WinExeExt := GetEnvironmentVariable('PATHEXT');
        // WinExeExt := '.COM;.EXE;.BAT;.CMD;.VBS;.VBE;.JS;.JSE;.WSF;.WSH';   // just for testing purposes !!!!
        if WinExeExt = '' then
            exit(false);        // var not set, we will not test or warn for executable
        WinExeExt := WinExeExt + ';';
        Extension := ExtractFileExt(LinkText);
        if Extension = '' then
            exit(False);        // Again, not enough info, will not test or warn for executable
        Extension := uppercase(Extension) + ';';
        Result := (pos(Extension, WinExeExt) <> 0);
    end;
    {$endif}

begin
    result := True;
    if LinkText.StartsWith(FileLinkToken) then
        LinkText := LinkText.Remove(0, FileLinkTokenLen)
    else exit;                                                // thats an error, should not happen.
    if (LinkText = '') or (LinkText = '""') then begin
        showmessage(' Empty Link ');
        exit;
    end;                                                      // OK, we know its not empty, but does it use " ?
    if (LinkText[1] = '"') then begin                         // Ah, wrapped in ".."
        LinkText := LinkText.Remove(0, 1);                    // Lazarus code will re-wrap the text later in process
        i := LinkText.IndexOf('"', 0);                        // Must have a second "
        if i = -1 then begin
            showmessage('Badly formed link : '#10 + LinkText);
            exit;
        end;
        LinkText := LinkText.Remove(i, 99);                   // remove second " and anything after it too
    end;
    if LinkText = '' then begin                               // Probably redundant .....
        showmessage('Empty Link');
        exit;
    end;

    { its not an absolute path (and therefore needs $HOME) if it does not start with a slash or eg c:/

    So, prepend $HOME if it does not start with either a slash or ?:  !
    }


    // Is it an absolute path, if not, prepend $HOME
    if not (LinkText[1] in ['\', '/']) then begin         // Relative path if first char after token is not a slash
        {$ifdef WINDOWS}                                  // it might still be an absolute path, starts with eg c:\ ?
        if (length(LinkText) < 4)                         // too short
            or (LinkText[2] <> ':') then                  // not a drive specifier, not much of a test but its windows !
                LinkText := Sett.HomeDir + LinkText;
        {$else}
        LinkText := Sett.HomeDir + LinkText;
        {$endif}
    end;
    if not (FileExists(LinkText) or DirectoryExists(LinkText)) then begin
        showmessage('File does not exist : '#10 + LinkText);
        exit;
    end;

        {$ifdef WINDOWS}
        // 'Executable' on Windows is a dogs breakfast. https://forum.lazarus.freepascal.org/index.php/topic,24615.0.html
        if WindowsFileIsExecutable() then begin
        {$else}
        if FileIsExecutable(LinkText) then begin
        {$endif}
             Msg := 'Link is an executable file.';
             if IDYES <> Application.MessageBox('Open an executable ?'
                     ,pchar(Msg) , MB_ICONQUESTION + MB_YESNO) then
                 exit;
         end;
        if not OpenDocument(LinkText) then
             showmessage('Sorry, cannot open '#10 + LinkText);

end;

    // ToDo : look at issues below, #2 particulary important, no error if OS cannot open link
(*  Two problems here.
        1. Windows has no idea about a file being executable. Best we can do is look
        at extension (if present), but the file might be quoted here without an extension,
        so quite error prone. Given Windows habit of hiding extensions, what else can we do ?

        2. On Linux (at least), Lazarus does not check the exit status of the executable used to
        launch. Its down in LazUTF8, needs to raise exception if OS cannot find
        something to open a file.
        Hmm, I wonder if windows even has a exit code ?

   This ultimatly, on Linux at least, ends up with a call the TProcessUTF8.execute from
   UTF8Process.pas, procedure RunCmdFromPath(const ProgramFilename, CmdLineParameters: string);
   and that calls xdg-open passing name of file to open. If xdg-open cannot find something
   to open the passed file, it returns a error status of 4 ("the action failed"). In either
   case xdg-open exits immediatly so that exit status is available. But its not checked and no
   indication of an error is passed back up to calling system.
   In my case, passing a sqlite file as a parameter (and nothing available to open an sqlite
   file) results in a silent failure.
   *)

// returns with the index of first char and text length for block that Index appears in.
// All are in char, not bytes.
procedure TEditBoxForm.GetBlockIndexes(Index : integer; out FirstChar, blkLen : integer);
var BlockNo, Offset : integer;
begin
   BlockNo := KMemo1.Blocks.IndexToBlockIndex(Index, Offset);
   FirstChar := Index - Offset;
   BlkLen := UTF8Length(KMemo1.Blocks.Items[BlockNo].Text);
end;

procedure TEditBoxForm.MenuItemFileLinkClick(Sender: TObject);
var SelStart, SelLen : integer;
begin
    case TMenuItem(sender).Name of
        'MenuItemInsertDirLink' : BuildFileLink(False);
        'MenuItemInsertFileLink' : BuildFileLink(True);
        'MenuItemSelectLink' : begin
               GetBlockIndexes(KMemo1.RealSelStart, SelStart, SelLen);
               KMemo1.SelStart := SelStart;
               KMemo1.SelLength := SelLen;
        end;
    end;
end;

procedure TEditBoxForm.MenuItemToolsLinksClick(Sender: TObject);
begin
    case TMenuItem(sender).Name of
        'MenuItemToolsInsertDirLink'  : BuildFileLink(False);
        'MenuItemToolsInsertFileLink' : BuildFileLink(True);
        'MenuItemToolsBackLinks'      : ShowBackLinks();
    end;
end;

function TEditBoxForm.CanInsertFileLink(out SoP, OnPara, InLink  : boolean) : boolean;
var Offset, BlockNo, Index : integer;
begin
    Result := False;
    InLink:=False; SoP:=False; OnPara:=False;
    Index := Kmemo1.blocks.RealSelStart;
    BlockNo := KMemo1.Blocks.IndexToBlockIndex(Index, Offset);
    if BlockNo < 2 then exit(False);                           // In Title, don't play here.
    if KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin
        OnPara := True;
        exit(True);
    end;
    InLink := Kmemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoHyperlink');
    if InLink then exit(false);
    if (Offset = 0) and KMemo1.Blocks.Items[BlockNo-1].ClassNameIs('TKMemoParagraph') then begin
        SoP := True;
        exit(True);
    end;
    if  KMemo1.Blocks.Items[BlockNo].Text[Offset+1] = ' ' then exit(True);
    // Hmm, what about if we are to right of a space ? Bad luck !
end;

procedure TEditBoxForm.DoRightClickMenu();
var SoP, OnPara, InLink, CanInsert : boolean;
begin
    CanInsert := CanInsertFileLink(SoP, OnPara, InLink);
//    debugln('DoRightClickMenu() CanInsert=' + booltostr(CanInsert, True) + ' Sop=' + booltostr(Sop, True)
//        + ' OnPara=' + booltostr(OnPara, True) + ' InLink=' + booltostr(InLink, True));
    MenuItemInsertFileLink.Enabled := CanInsert;
    MenuItemInsertDirLink.Enabled  := CanInsert;
    MenuItemSelectLink.enabled := InLink;
    PopupMenuRightClick.popup
end;


// ----------------------- G E N E R A L   L I N K  M E T H O D S --------------

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
        // debugln('SaveLimitedAttributes - using default values');
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


procedure TEditBoxForm.MakeLink(const Index, Len : longint; const Term : string; InsertMode : boolean=false);
// Note : the HTTP check does not pass the Term, gives an empty string instead !!!!
var
	Hyperlink : TKMemoHyperlink;
    TrueLink, AText : string;
	BlockNoS, BlockNoE, i : integer;
    BlockOffset : integer;          // A zero based count of characters ahead of char pointed by Index
    FontAtt : FontLimitedAttrib;
    ULen : integer;                 // Length, in Char, of Link Text
begin
    //DumpKMemo('TEditBoxForm.MakeLink START term=' + Term + ' vvvvvvvvv');       // ToDo : comment
    if Term <> '' then              // We must use UTF8 number to calculate BlockNoE
        ULen := UTF8Length(Term)
    else ULen := Len;               // But with http, it does not matter, no UTF8 in web addresses !

    if Index = 0 then exit;         // Thats this note's title, skip it !
    BlockNoE := KMemo1.Blocks.IndexToBlockIndex(Index+ULen-1, BlockOffset);     // Block where proposed link Ends
    BlockNoS := KMemo1.Blocks.IndexToBlockIndex(Index, BlockOffset);            // Block where proposed link starts
    SaveLimitedAttributes(BlockNoS, FontAtt);                                   // Record the existing colours asap !
    //debugln('TEditBoxForm.MakeLink S=' + BlockNoS.ToString + ' E=' + BlockNoE.ToString);
    if KMemo1.Blocks.Items[BlockNoS].ClassNameIs('TKMemoHyperLink') then begin
        AText := lowercase(KMemo1.Blocks.Items[BlockNoS].Text);
        if AText.StartsWith('http') then exit;                                  // Already checked by Clean...
        if AText = Term then exit;                                              // Already there
    end;
    i := BlockNoS;
        while i <= BlockNoE do begin
            if KMemo1.Blocks.Items[i].ClassNameIs('TKMemoHyperlink') then begin     // is there a link there already ?
                if KMemo1.Blocks.Items[i].text.StartsWith('http') then exit;        // Leave existing web links alone, already checked.
                if KMemo1.Blocks.Items[i].text.Length >= Len then exit;             // Leave it alone, is already at least as long
                UnlinkBlock(i);                                                     // Existing shorter, we will replace
                //debugln('TEditBoxForm.MakeLink Unlinked ' + i.tostring);
                BlockNoE := KMemo1.Blocks.IndexToBlockIndex(Index+Len-1, BlockOffset);      // Sadly, we need to start this loop again
                BlockNoS := KMemo1.Blocks.IndexToBlockIndex(Index, BlockOffset);            // and keep iterating until we have clear space
                i := BlockNoS;
            end;
            if KMemo1.Blocks.Items[i].ClassNameIs('TKMemoParagraph') then begin     // Thats an ERROR !
                //debugln('TEditBoxForm.MakeLink exiting early !!!!');
                exit;
            end;
            inc(i);
        end;
    { BlockOffset is number of char (not bytes) before we start linking. So, '0' says link starts at left edge.
      KMemo gives us a Char count (not bytes)
      So, we copy the leading, not to be linked string of char, and get the byte length.
    }
    BlockOffset := length(UTF8Copy(Kmemo1.Blocks.Items[BlockNoS].Text, 1, BlockOffset));
    TrueLink := copy(Kmemo1.Blocks.Items[BlockNoS].Text, BlockOffset+1, Len);   // copy Len BYTES ! Add 1 because copy() is one based.
    // The below might leave a empty block. Messy to delete here but ....
    if BlockNoE > BlockNoS then begin                                           // Multiple blocks, two or more ....
        TKMemoTextBlock(Kmemo1.Blocks.Items[BlockNoS]).Text
            := copy(Kmemo1.Blocks.Items[BlockNoS].Text, 1, UTF8Length(Kmemo1.Blocks.Items[BlockNoS].Text)- UTF8Length(TrueLink));
        i := 0;
        while BlockNoE > (BlockNoS+i) do begin                                  // ie two blocks, run loop once
            TrueLink := TrueLink + Kmemo1.Blocks.Items[BlockNoS+i+1].Text;      // +1 to get next block
            inc(i);                                                             // i must be > 0 because we are multiblock
        end;                                                                    // That will get all of last block's text, probably excessive
        // The below might leave a empty block. Messy to delete here but ....
        TKMemoTextBlock(Kmemo1.Blocks.Items[BlockNoS+i]).Text                   //  BlockNoS+i is last block containing some of link
            := utf8copy(Kmemo1.Blocks.Items[BlockNoS+i].Text, 1 + Kmemo1.Blocks.Items[BlockNoS+i].Text.Length - UTF8length(TrueLink) + len, 9999);
        delete(TrueLink, Len+1, 999);                                           // Get rid of that excess, +1 to start deleting after link text
        dec(i);
        while i > 1 do begin
            dec(i);
            Kmemo1.Blocks.Delete(BlockNoS+1);
        end;
        inc(BlockNoS);          // Assumes we have left BlockNoS in place, removing trailing text, point to spot after existing value
        // DumpKMemo('TEditBoxForm.MakeLink AFTER merging blocks');
    end else begin                                                              // All the proposed link was in the BlockNoS
        BlockNoS := KMemo1.SplitAt(Index);
        // Chop of everything after the required Text. But len is a char count, not a byte count.

        // Note : Makelink no longer does insert.
        TKMemoTextBlock(Kmemo1.Blocks.Items[BlockNoS]).Text                     // remove content after the link
                := string(Kmemo1.Blocks.Items[BlockNoS].Text).Remove(0, Length(TrueLink));    // bytes !
//        Test :=  TKMemoTextBlock(Kmemo1.Blocks.Items[BlockNoS]).Text;
        if Kmemo1.Blocks.Items[BlockNoS].Text = '' then
            KMemo1.blocks.Delete(BlockNoS);                                     // Link went to very end of orig block.
        if Kmemo1.Blocks.Items[BlockNoS-1].Text = '' then begin                 // Link must have started at beginning of orig block
            KMemo1.blocks.Delete(BlockNoS-1);
            dec(BlockNoS);
//            Test :=  TKMemoTextBlock(Kmemo1.Blocks.Items[BlockNoS]).Text;
        end;
    end;
    // When we get to here, Link text (and any blocks completely spanned by link text) have been removed
    // and BlockNoS points to where link need be pushed into. Might have some empty text blocks ....
    {$ifdef LDEBUG}TG2 := gettickcount64();{$endif}
    //debugln('TEditBoxForm.MakeLink() 1 BlockNoS=' + dbgs(BlockNoS) + ' TrueLink=[' + TrueLink + ']');   // ToDo : comment
    Hyperlink := TKMemoHyperlink.Create;
    Hyperlink.Text := TrueLink;
    //debugln('TEditBoxForm.MakeLink() 2 BlockNoS=' + dbgs(BlockNoS) + ' Link=[' + Hyperlink.Text + ']');   // ToDo : comment
    Hyperlink.Textstyle.StyleChanged   :=  true;
    Hyperlink.OnClick := @OnUserClickLink;
    {HL := }KMemo1.Blocks.AddHyperlink(Hyperlink, BlockNoS);   // BlockNoS points to the block cut from the chain and attached to the link block.
    RestoreLimitedAttributes(BlockNoS, FontAtt);
    HyperLink.Textstyle.Font.Color := Sett.LinkColour;
    {$ifdef LDEBUG}TG3 := gettickcount64();
    TG1 := TG1 + (TG3-Tg2);{$endif}
    //DumpKMemo('TEditBoxForm.MakeLink END term=' + Term + ' ^^^^^^^^^^');
end;


// Starts searching a string at StartAt for Term, returns 1 based offset from start of str if found, 0 if not. Like UTF8Pos(
function TEditBoxForm.RelativePos(const Term : ANSIString; const MText : PChar; StartAt : integer) : integer;
begin
  result := Pos(Term, MText+StartAt);
  if Result <> 0 then
      Result := Result + StartAt;
end;

procedure TEditBoxForm.MakeAllLinks(const Buff : string; const Term : ANSIString; const BlockOffset : integer);
// Passed a text only version of the note in Buff
var
	Offset   : Integer;     // The char position of a search term in Buffer
    ByteBeforeTerm, ByteAfterTerm : integer;
begin
    Offset := UTF8Pos(Term, Buff);
    while Offset > 0 do begin                                                   // will be zero when UTF8Pos() fails to find term
//        debugln('NewMakeAllLinks Acting on Term=' + Term);
        ByteBeforeTerm := UTF8CodepointToByteIndex(PChar(Buff), length(Buff), Offset)-1;
        ByteAfterTerm  := UTF8CodepointToByteIndex(PChar(Buff), length(Buff), Offset+UTF8length(Term));  // Term may contain UTF8 char
//        debugln('TEditBoxForm.MakeAllLinks Offset=' + inttostr(Offset) + ' len=' + inttostr(length(Term)) + ' Buff=[' + Buff + ']');
        if ((Offset = 1) or (Buff[ByteBeforeTerm] in [' ', #10, ',', '.'])) and
            (((Offset + length(Term)) >= length(Buff))                          // Line ends at end of link term
                or (Buff[ByteAfterTerm] in [' ', #10, ',', '.'])) then begin    // a suitable seperator at end of link term
            MakeLink(BlockOffset + Offset -1, length(Term), Term);          // MakeLink takes a Char Index and a byte Len
            TimerHouseKeeping.Enabled := False;
        end;
        Offset := UTF8Pos(Term, Buff, Offset + 1);
    end;
end;



procedure TEditBoxForm.CheckForExtLinks(const Buff : string; const Offset : integer);
var
    http, FileLink : integer;
    Len : integer = 1;
    LinkText : string;

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
                then exit(0);                                     // invalid start
        while (not(Buff[http+result] in [' ', ',', #10])) do begin // that might be end of link
            if (http+result) >= length(Buff) then exit(0);        // invalid end
            if Buff[http+result] = '.' then ADot := True;
            inc(result);                                          // next byte
        end;
        if Buff[http+result-1] = '.' then exit(0);                // The dot was at the end.
        if (result < 13) or (not ADot) then exit(0);
    end;

    function FindNextFileLink() : integer;                   // Ret 0 based index of Token, sets Len
    var i : integer;
    begin
        Len := FileLinkTokenLen;                             // At this stage, a count of valid bytes in the link
        Result := Buff.IndexOf(FileLinkToken, FileLink);
        if Result = -1 then begin                                   // Nothing to see here folks
            Len := 0;
            exit;
        end;
        // Result now points (zero based) to the start of token. We don't mark up unless we have whitspace or eol somewhere after token
        // If Result is zero, the space is at start of line. Len tells us how much to add to start searching.
        // Further, if text after Token starts with ", we don't markup until there is a closing "

        if length(Buff) = Result + Len then           // Just the Token, mark it up.
            exit;
        if Buff[Result+Len+1] in [' ', #10] then                   // Just the Token, mark it up.      ###
            exit;
        if Buff[Result + Len+1] = '"' then begin                   // Deal with links containing ""
            i := Buff.IndexOf('"', Result + Len+1);                // get second ", zero based.
            if i >= 0 then                                         // A second " ?, adjust Len, else just markup the token
                Len := i - Result +1
            else inc(Len);                                         // And markup the first " so user knows we are listening
        end else
            while not (Buff[Result+1+len] in [' ', #10]) do begin  // OK, no spaces allowed, want space or newline, +1 cos Result is zero based
                if (result+1+len) >= length(Buff) then begin       // overrun
                    Len := 0;
                    exit;
                end;
                inc(Len);
            end;
//        if Len > 0 then writeln('FindNextFileLink - Result=', Result, ' Len=', Len, ' [', copy(Buff, Result+1, Len), ']');
    end;


begin
    // First check for web addresses
    http := pos('http', Buff);                              // note that http is in bytes, '1' means first byte of Buff
    while (http <> 0) do begin
        if (UTF8copy(Buff, http, 7) = 'http://') or (copy(Buff, http, 8) = 'https://') then begin
            Len := ValidWebLength();                        // reads http and Buff
            if Len > 0 then begin
//                debugln('CheckForHTTP Calling MakeLink() Offset=' + Offset.Tostring + '+' + (UTF8Length(pchar(Buff), http-1)).ToString + ' Len=' + (UTF8Length(pchar(Buff)+http, Len)).ToString);
                MakeLink(OffSet + UTF8Length(pchar(Buff), http-1), len, '');  // must pass char values, not byte
            end;
        end;
        http := UTF8pos('http', Buff, http+Len+1);          // find next one, +1 to ensure moving on
    end;
    // Then check for local file links
    FileLink := 0;           // start searching from start of this para, but zero based this time !
    repeat
        FileLink := FindNextFileLink();
        if Len > 0 then begin
            LinkText := copy(Buff, FileLink+1, Len);      // all in bytes, not char
            //debugln(#10'TEditBoxForm.CheckForHTTP FILELinkText=[' + LinkText + '] len=' + dbgs(Len)); // ToDo : comment
            MakeLink(Offset + UTF8Length(pchar(Buff), FileLink), len, LinkText);      // 1st Param is Char count, Len is bytes
        end;
        inc(FileLink);
    until Len < 1;
end;

// ============================================================================================================
(* procedure TestMyFunction();
var Buff : string; FileLink, Len, Res : integer;
begin

  FileLink := 2;
  Buff := 'file://';
  Res := FindNextFileLink(buff,  FileLink, Len);
  writeln('Link=[', copy(Buff, Res+1, Len), '] Res=', Res, ' FL=', FileLink, ' Len=', Len);

  Buff := 'file://myfile  file://another ';
  Res := FindNextFileLink(buff,  FileLink, Len);
  writeln('Link=[', copy(Buff, Res+1, Len), '] Res=', Res, ' FL=', FileLink, ' Len=', Len);

  FileLink := 0;
  Buff := 'file://"some silly space"  '#10;
  Res := FindNextFileLink(buff,  FileLink, Len);
  writeln('Link=[', copy(Buff, Res+1, Len), '] Res=', Res, ' FL=', FileLink, ' Len=', Len);

  Buff := 'file://"some silly space  ';
  Res := FindNextFileLink(buff,  FileLink, Len);
  writeln('Link=[', copy(Buff, Res+1, Len), '] Res=', Res, ' FL=', FileLink, ' Len=', Len);
end;         *)
// ======================================================================================================================

procedure TEditBoxForm.CheckForLinks(const FullBody : boolean);
//{$define TDEBUG}
var
    Content : string = '';
    BuffOffset, LineNumb, BlockNo : integer;
    EndScan : Integer = 0;
    i : integer;
    {$ifdef TDEBUG}T0:qword=0;T1:qword=0;T2:qword=0;T3:qword=0;T4:qword=0;T5:qword=0;{$endif}


    // Puts one para of kMemo, starting at LineNumb, into Content. Ret Starting offset.
    function GrabPara() : integer;
    begin
       Content := '';
       Result := KMemo1.Blocks.LineStartIndex[LineNumb];
       while LineNumb < KMemo1.Blocks.LineCount do begin
           Content := Content + lowercase(Kmemo1.blocks.LineText[LineNumb]);
           dec(LineNumb);
           //inc(LineNumb);
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

    // Puts all the content of the para before Bk into Content string.
    // Lowercases it and terminates with a #10 char.
    function GetPrevPara(const Bk :integer) : integer;
    var
        Index : integer;
    begin
        Index := Bk;
        Content := '';
        while not KMemo1.Blocks[Index].ClassNameIs('TKMemoParagraph') do begin
            dec(Index);
            if Index <= 0 then break;
        end;
        Result := Index;
        // OK, here, BK is either Para or we have reached the very beginning.
        while Index <= Bk do begin
            if (Index > -1) and (not KMemo1.Blocks[Index].ClassNameIs('TKMemoParagraph')) then
                Content := Content + lowercase(KMemo1.Blocks[Index].Text);
            inc(Index);
        end;
        Content := Content + #10;
    end;



begin
   //TestMyFunction;
    if SingleNoteMode then exit;
    {$ifdef LDEBUG}
    AssignFile(MyLogFile, 'log.txt');
    rewrite(MyLogFile);
    TG1 := 0;
    {$endif}
    if (FullBody) then begin                        // Initial Scan and do links in whole note, no unlinking required
        LineNumb := KMemo1.Blocks.LineCount -1;
        {$ifdef TDEBUG}T0 := gettickcount64();{$endif}
        { We iterate over the KMemo, loading a line, checking it for Links and hyperlinks, then do next line }

        KMemo1.Blocks.LockUpdate;
        BlockNo := GetPrevPara(KMemo1.Blocks.Count -1);
        while BlockNo > 0 do begin
            BlockNo := GetPrevPara(BlockNo-1);
            // At this point, BlockNo points to Par marker before our text.
//          if BlockNo >= 0 then
            // ToDo : above suspect line, part of a debug sys ???? Has been active and accidently
            // acting on the "BuffOffSet := " line below since I reversed the direction of scanning
            // for links. I am reasonably sure it should NOT be active.
//          debugln('Char index=' + inttostr(KMemo1.Blocks.BlockToIndex(KMemo1.Blocks[BlockNo+1])));

            BuffOffSet := KMemo1.Blocks.BlockToIndex(KMemo1.Blocks[BlockNo+1]);
            for i := 0 to TheMainNoteLister.NoteList.Count-1 do                 // for each title in main list
                if TheMainNoteLister.NoteList[i]^.Title <> NoteTitle then begin // don't link to self
                    if length(Content) > 3 then begin                           // Two significent char plus a newline
                        MakeAllLinks(Content, TheMainNoteLister.NoteList[i]^.TitleLow, BuffOffset);
                    end;
                end;
            // OK, lets do HTTPS then.
            if Sett.CheckShowExtLinks.Checked then
            if length(Content) > 3 then                  // This used to be 12 before we started doing file links.
                CheckForExtLinks(Content, BuffOffset);
        end;
        KMemo1.Blocks.UnLockUpdate;

        {$ifdef TDEBUG}T1 := gettickcount64();
        debugln('CheckForLinks Timing T1=' + (T1-T0).tostring);
        {$endif}
    end else begin                                      // Just scan +/- LinkScanRange of current cursor, Edit Mode
        {$ifdef TDEBUG}T1 := gettickcount64();{$endif}
        KMemo1.blocks.LockUpdate;
        BuffOffset := GrabContent();                                         // Also sets EndScan
        {$ifdef TDEBUG}T2 := gettickcount64();{$endif}
        ClearNearLink(BuffOffset, EndScan);                                  // Parameters in utf8char, not bytes
        {$ifdef TDEBUG}T3 := gettickcount64();{$endif}
        if Sett.ShowIntLinks and (not SingleNoteMode) then begin             // draw internal links
            for i := 0 to TheMainNoteLister.NoteList.Count-1 do begin        // For each note title in the main list.
                  {$ifdef LDEBUG}writeln(MyLogFile, 'BV ' + TheMainNoteLister.NoteList[i]^.TitleLow);{$endif}
                if TheMainNoteLister.NoteList[i]^.Title <> NoteTitle then
                    MakeAllLinks(Content, TheMainNoteLister.NoteList[i]^.TitleLow, BuffOffset);
            end;
             TimerHouseKeeping.Enabled := False;
        end;
        {$ifdef TDEBUG}T4  := gettickcount64();{$endif}
        if Sett.CheckShowExtLinks.Checked then
                CheckForExtLinks(Content, BuffOffset);                           // Mark any unmarked web or file links
        KMemo1.blocks.UnLockUpdate;                                          // can take tens of mS, 100mS in a 50k note
        {$ifdef TDEBUG}T5  := gettickcount64();
        debugln('CheckForLinks Timing T1=' + (T2-T1).ToString + 'mS '  + (T3-T2).ToString + 'mS '  + (T4-T3).ToString + 'mS '  + (T5-T4).ToString + 'mS ');
        {$endif}
    end;
    //{$undef TDEBUG}
    {$ifdef LDEBUG}CloseFile(MyLogFile);{$endif}
end;


function TEditBoxForm.UnlinkBlock(StartBlock : integer) : integer;    // WRONG - ??  MUST NOT MERGE A HYPERLINK BLOCK ?????
var
    Existing : string;
    ChangedOne : boolean = false;
    Blk : TKMemoTextBlock;
    FontAtt : FontLimitedAttrib;

    function CanMergeBlocks(LinkBlock, TextBlock : integer) : boolean;  // Merges two blocks IFF they are same sort, font, size etc
    begin
        // Dont merge if Right block starts with file:// and is Link block
        if TextBlock > LinkBlock then begin
            if KMemo1.Blocks.Items[TextBlock].ClassNameIs('TKMemoHyperlink')
                and KMemo1.Blocks[TextBlock].Text.StartsWith(FileLinkToken) then
                    exit(False);
        end else
            if KMemo1.Blocks.Items[LinkBlock].ClassNameIs('TKMemoHyperlink')
                and KMemo1.Blocks[LinkBlock].Text.StartsWith(FileLinkToken) then
                    exit(False);

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
//        debugln('UnLinkBlock Merge Left StartBlock=' + StartBlock.Tostring + ' Existing=' + Existing);
   end;

   if CanMergeBlocks(StartBlock, StartBlock+1) then begin
        Existing := KMemo1.Blocks.Items[StartBlock].Text;
        Kmemo1.Blocks.Delete(StartBlock);
        TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock]).Text := Existing + KMemo1.Blocks.Items[StartBlock].Text;
        ChangedOne := True;
//        debugln('UnLinkBlock Merge Right StartBlock=' + StartBlock.Tostring + ' Existing=' + Existing);
   end;

   if not changedOne then begin
        Existing := KMemo1.Blocks.Items[StartBlock].Text;
        SaveLimitedAttributes(StartBlock, FontAtt);
        Kmemo1.Blocks.Delete(StartBlock);
        Blk := KMemo1.Blocks.AddTextBlock(Existing, StartBlock);
        RestoreLimitedAttributes(StartBlock, FontAtt);
        Blk.TextStyle.Font.Style := Blk.TextStyle.Font.Style - [fsBold, fsItalic];
        Blk.TextStyle.Font.Size := Sett.FontNormal;
//        debugln('UnLinkBlock No Merge StartBlock=' + StartBlock.Tostring + ' Existing=' + Existing);
   end;
   Result := StartBlock;           // only changed with merge left
end;


procedure TEditBoxForm.ClearNearLink(const StartS, EndS : integer); inline;
// note that kmemo is locked before we get here.
var
    Blar, StartBlock, EndBlock : longint;
    LinkText  : ANSIString;

    function ValidWebLink() : boolean;     // returns true if LinkText looks like valid web address
    var
        DotSpot : integer;
//        Str : String;
    begin
        if pos(' ', LinkText) > 0 then exit(false);
        if (copy(LinkText,1, 8) <> 'https://') and (copy(LinkText, 1, 7) <> 'http://') then exit(false);
        DotSpot := pos('.', LinkText);
        if DotSpot = 0 then exit(false);
        if (DotSpot < 8) or (DotSpot > length(LinkText)-1) then exit(false);
        if LinkText.EndsWith('.') then exit(false);
        result := true;
    end;

    // Returns true if LinkText looks like a valid file link. A valid file link
    // may be just FileLinkToken or start with it. No spaces unless wrapped in "
    function ValidFileLink() : boolean;
    begin
        Result := False;
        if LinkText = FileLinkToken then exit(True);
        if FileLinkTokenLen > length(LinkText) then begin
            if LinkText[FileLinkTokenLen+1] = '"' then begin    // Wrapped Link, spaces allowed
                if (length(LinkText) > (FileLinkTokenLen+2))
                    and LinkText.EndsWith('"') then
                    exit(True);
            end else begin                                      // NonWrapped, no spaces !
                if pos(' ', LinkText) > 0 then exit(False);
                exit(length(LinkText) > (FileLinkTokenLen+2));  // Token + 1 byte at least.
            end;

        end;
    end;

    function ValidLocalLink() : boolean;
    begin
        if not Sett.ShowIntLinks then exit(False);
        if not TheMainNoteLister.IsThisaTitle(LinkText) then exit(False);
        Result := true;

{            KMemo1.Blocks.Items[StartBlock-1].ClassNameIs('TKMemoParagraph')
                or TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock-1]).Text.EndsWith(' ');
        Result := Result
            and (
                (KMemo1.Blocks.count = StartBlock+1)
                or
                (KMemo1.Blocks.Items[StartBlock+1].ClassNameIs('TKMemoParagraph')
                or
                (TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock+1]).Text[1]              // !!!!!!!!!!
                    in [' ', ',']))  );     }
    end;

    // Any link must start after a newline or a space, end before a space, comma or newline
    function InvalidWhiteSpace() : Boolean;                     // ret True if invalid
    begin
        if SingleNoteMode then exit(True);                      // no links in Single mode, always invalid
        Result := not (
            (KMemo1.Blocks.Items[StartBlock-1].ClassNameIs('TKMemoParagraph')
                or TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock-1]).Text.EndsWith(' ') )
            and (
            (KMemo1.Blocks.count = StartBlock+1)
            or
            (KMemo1.Blocks.Items[StartBlock+1].ClassNameIs('TKMemoParagraph')
            or
            (TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock+1]).Text[1]
                in [' ', ',']))) );
    end;


begin
//    debugln('TEditBoxForm.ClearNearLink blk ' + dbgs(StartS) + ' to ' + dbgs(Ends));
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
        if KMemo1.Blocks.Items[StartBlock].ClassNameIs('TKMemoHyperlink') then begin         // Find any hyperlink
            LinkText := lowercase(Kmemo1.Blocks.Items[StartBlock].Text);                     // ! trim()
//            debugln('TEditBoxForm.ClearNearLink considering link : ' + LinkText);

            // Only if its not a valid link, remove it.
            if InvalidWhiteSpace or (not (ValidWebLink() or ValidLocalLink() or ValidFileLink())) then begin              // LocalLinks ignored in SingleNoteMode
                StartBlock := UnLinkBlock(StartBlock);
                if EndBlock > Kmemo1.Blocks.Count then EndBlock := Kmemo1.Blocks.Count;
//                debugln('TEditBoxForm.ClearNearLink remove link : ' + LinkText);
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
        // DumpKMemo('Finished ClearNearLink');
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




 //   FindFilenameOfCmd()
(*    Msg := 'Cannot open a file : ' + LinkText;
    if IDYES = Application.MessageBox('Open a note with that name ?'
                ,pchar(Msg) , MB_ICONQUESTION + MB_YESNO) then
        result := False;
end;                    *)

procedure TEditBoxForm.OnUserClickLink(sender : TObject);
begin
    if (copy(TKMemoHyperlink(Sender).Text, 1, 7) = 'http://') or
        (copy(TKMemoHyperlink(Sender).Text, 1, 8) = 'https://') then begin
            OpenUrl(TKMemoHyperlink(Sender).Text);
        exit;
    end;
    if (copy(TKMemoHyperlink(Sender).Text, 1, length(FileLinkToken)) = FileLinkToken) then begin
        OpenFileLink(TKMemoHyperlink(Sender).Text);
        exit;
    end;

    //debugln(' Passed link text is [', TKMemoHyperlink(Sender).Text, ']');
(*    if TKMemoHyperlink(Sender).Text[1] in ['~', '/', '\'] then begin    // Maybe file link ?
        if IsFileLink(TKMemoHyperlink(Sender).Text) then
            exit;                                                                                                                              // is invalid will go on to open a new note ???
    end;       *)
	SearchForm.OpenNote(TKMemoHyperlink(Sender).Text);
end;



procedure TEditBoxForm.DoHousekeeping();
var
    CurserPos, SelLen, BlockNo, Blar : longint;
    TempTitle : ANSIString;
    {$ifdef LDEBUG}TS1, TS2  : qword;{$endif}
begin
    {$ifdef LCLGTK3}debugln('TEditBoxForm.DoHousekeeping');{$endif}
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
    if (not SingleNoteMode) {and (Sett.ShowIntLinks or Sett.CheckShowExtLinks.Checked)} then begin
        {$ifdef LDEBUG}TS1 := gettickcount64();{$endif}
        CheckForLinks(False);                   // does its own locking
        TimerHouseKeeping.Enabled := False;                                     // ToDo : why don't we disable in every run through ??
        {$ifdef LDEBUG}TS2 := gettickcount64();
        debugln('------------- DoHousekeeping Update Links ' + inttostr(TS2-TS1) + 'ms');{$endif}
    end;
    KMemo1.SelStart := CurserPos;
    KMemo1.SelLength := SelLen;
    Ready := True;
    {$ifdef LCLGTK3}debugln('TEditBoxForm.DoHousekeeping done');{$endif}
end;

procedure TEditBoxForm.TimerHousekeepingTimer(Sender: TObject);
begin
    {$ifdef LCLGTK3}debugln('TEditBoxForm.TimeHousekeeping');{$endif}
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
        if AStr[Index] in ['0'..'9', '.', '-'] then
            AtStart := AtStart + AStr[Index]
        else break;
        inc(Index);
    end;
    if AtStart = '-' then Atstart := '';   // Just a - is not useful
    Index := length(AStr);
    while Index > 0 do begin
        if AStr[Index] in ['0'..'9', '.', '-'] then AtEnd :=  AStr[Index] + AtEnd
        else break;
        dec(Index);
    end;
    if AtEnd = '-' then AtEnd := '';        // Just a - is not useful
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

    function GetClickedIndex() : integer;     // A selection index for clicked position, only used for right click
    var P : TPoint; LinePos : TKmemoLinePosition;
    begin
        //P := ClientToScreen(Point(X, Y));
        P := Point(x, y);
        LinePos := eolEnd;
        while P.X > 0 do begin                   // we might be right of the eol marker.
              KMemo1.PointToIndex(P, true, true, LinePos);
              if LinePos = eolInside then break;
              dec(P.X);                        // move P.x to left until we are inside a line (or zero)
        end;
        Result := KMemo1.PointToIndex(P, true, true, LinePos);
//        debugln('KMemo1Mousedown GetClickedIndex returning ' + dbgs(Result) + ' x=' + dbgs(X)
//                + ' y=' + dbgs(y) + ' xs=' + dbgs(P.x) + ' ys=' + dbgs(P.y));
    end;

begin
    MouseDownPos := KMemo1.CaretPos;    // regional record in case we are doing shift click, KMemo1.MouseUp()
    if (ssCtrl in Shift) or (Button = mbRight) then begin
        if KMemo1.SelLength = 0 then begin
            //GetClickedIndex();
            KMemo1.SelStart := GetClickedIndex();
            KMemo1.SelLength := 0;
            //debugln('Mousedown ' + dbgs(KMemo1.CaretPos) + ' New Position=' + inttostr(KMemo1.SelStart));
        end;
        DoRightClickMenu();
    end;
{   Rule - if something is selected, then we assume that the popup menu is to work on
           that selected text. But if nothing is selected then we should assume the
           user intends the action to be where they have clicked and we move the cursor
           there. If we move index, set sellength to zero. }
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

procedure TEditBoxForm.KMemo1Click(Sender: TObject);
begin
//   debugln('Mouseclick ' + inttostr(kmemo1.RealSelStart));
end;


procedure TEditBoxForm.KMemo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
    ABlock : TKMemoParagraph;

        // Returns True if Cursor/Caret is in the note Title or its trailing Para
        function CaretInTitle() : boolean; inline;
        var
            CharCount : integer = 0;
            BlkNo : integer = 0;
        begin
            while not KMemo1.Blocks[BlkNo].ClassNameIs('TKMemoparagraph') do begin
                CharCount := CharCount + length(KMemo1.Blocks[BlkNo].Text);
                inc(BlkNo);
                if BlkNo > KMemo1.Blocks.Count then break;      // safety !
            end;
            Result := (KMemo1.CaretPos <= CharCount);
            //debugln('CaretInTitle() returning ' + booltostr(Result, True));
        end;


        // returns the para block that controls text the cursor is on right now
        function GetParagraphBlock() : TKMemoParagraph; inline;
        var
            ABlockNo : integer = -1;
            LocIndex : integer;
        begin
            ABlockNo := Kmemo1.Blocks.IndexToBlockIndex(KMemo1.Blocks.RealSelStart, LocIndex);
            if KMemo1.Blocks[ABlockNo].ClassNameIs('TKMemoParagraph') then      // if Ablockno is a para, use it, else we look for one further on
                 Result := TKMemoParagraph(KMemo1.Blocks[ABlockNo])             // works if cursor is just beyond last char in line
            else Result := Kmemo1.Blocks.GetNearestParagraphBlock(ABlockNo);    // Gets the following one, not the 'nearest' !
        end;

        procedure IndentControl(IncreaseIndent : Boolean);                      // Called when Ctrl-Tab pressed, returns True if it acted
        begin
            ABlock := GetParagraphBlock();
            if ABlock <> nil then begin                                         // if zero cannot be a bullet nor indent
                if ABlock.Numbering <> pnuNone then begin                       // do bullet as well
                    BulletControl(IncreaseIndent);
                    exit();
                end;
                if IncreaseIndent then begin
                    if (ABlock.ParaStyle.LeftPadding + IndentWidth) <= IndentMax then
                        ABlock.ParaStyle.LeftPadding := ABlock.ParaStyle.LeftPadding + IndentWidth;
                end else begin
                    if (ABlock.ParaStyle.LeftPadding - IndentWidth) >= 0 then
                        ABlock.ParaStyle.LeftPadding := ABlock.ParaStyle.LeftPadding - IndentWidth;
                end;
            end;
            Key := 0;
        end;

        // If we BS from a non indented line to an Indented line above it, we have to transfer the Indent
        // setting from the upper line to the lower line. So, we 'prepare' the lower line, the current one
        // by copy the upper one's Indent and do not set Key:=0 (so KMemo can merge them.
        // This is ONLY necessary when Back Spacing.  BulletBackSpace() takes care of it now.

    function AtStartOfLine() : boolean; inline;
    var
        ABlockNo, LocIndex : integer;
    begin
        Result := False;
        ABlockNo := Kmemo1.Blocks.IndexToBlockIndex(KMemo1.Blocks.RealSelStart, LocIndex);
        if LocIndex <> 0 then exit();
        if KMemo1.Blocks[ABlockNo-1].ClassNameIs('TKMemoParagraph') then
                Result := True;
    end;

    function AtEndOfLine() : boolean; inline;          // True if cursor is at end of line
    var
        ABlock : TKMemoBlock;
        LocIndex : integer;
    begin
        Result := False;
        ABlock := Kmemo1.Blocks.IndexToBlock(KMemo1.Blocks.RealSelStart, LocIndex);
        result := ABlock.ClassNameIs('TKMemoParagraph');
    end;

    // Deals with both Bullets and Indents, ensures bullet/indent charactistics from upper line applies
    // when two lines are merged using the delete key. User has pressed Del and we are at End of Line.
    function DeleteAtEOL(InTitle : boolean) : boolean;
    var
        ABlock   : TKMemoParagraph;
        ABlockNo, LocIndex : integer;
    begin
        Result := False;
        ABlockNo := Kmemo1.Blocks.IndexToBlockIndex(KMemo1.Blocks.RealSelStart, LocIndex);   // the para block on upper line
        ABlock := Kmemo1.Blocks.GetNearestParagraphBlock(ABlockNo + 1);                      // the para block on lower line
        if ABlock <> Nil then
            if ABlock.ClassNameIs('TKMemoParagraph') then begin
                if InTitle then begin                                          // sucking it up into title, no nothing !
                    ABlock.Numbering := pnuNone;
                    ABlock.ParaStyle.LeftPadding := 0;
                    ABlock.ParaStyle.FirstIndent := 0;                         // clear any possible bullet residue - is this necessary elsewhere ?
                 end else begin
                    ABlock.Numbering := TKMemoParagraph(KMemo1.Blocks[ABlockNo]).Numbering;
                    ABlock.ParaStyle.LeftPadding := TKMemoParagraph(KMemo1.Blocks[ABlockNo]).Parastyle.LeftPadding;
                    if TKMemoParagraph(KMemo1.Blocks[ABlockNo]).Numbering = pnuNone then
                         ABlock.ParaStyle.FirstIndent := 0;                    // clear any possible bullet residue - is this necessary elsewhere ?
                end;
                Result := True;
            end else debugln('DeleteAtEOL() ABlock is not Para');
    end;

begin
    {$ifdef LCLGTK3}debugln('TEditBoxForm.KMemo1KeyDown Key = ' + inttostr(Key));{$endif}
    if not Ready then begin       // Will this help with issue #279  ?
        if [ssCtrl] = shift then
            Key := 0;
        exit();
    end;

    if (Key = VK_BACK) then begin                // BackSpace sanity checks, deal with Github issue #331 ?
        debugln('TEditBox.KMemo1KeyDown - BS Pre - SelStart=', inttostr(KMemo1.SelStart), ' and SelEnd=', inttostr(KMemo1.SelEnd));
        if KMemo1.SelLength <> 0 then               // Treat as Delete if something selected.
            Key := VK_Delete
        else                                     // Nothing selected
            if (KMemo1.SelStart = 0) then begin  // and at start of doc, do nothing.
                Key := 0;
                exit();
            end;                                 // continue with no changes if something to remove
    end;

    // don't let any ctrl char get through the kmemo on mac
    {$ifdef DARWIN}
    if [ssCtrl] = Shift then begin
        case Key of
             VK_1 : AlterFont(ChangeSize, Sett.FontSmall);
             VK_2 : AlterFont(ChangeSize, Sett.FontNormal);
             VK_3 : AlterFont(ChangeSize, Sett.FontLarge);
             VK_4 : AlterFont(ChangeSize, Sett.FontHuge);
             VK_TAB : begin if not CaretInTitle() then IndentControl(True); Key := 0; end;    // Mac ???
        end;
        Key := 0;
        exit;
    end;
    if ([ssAlt, ssShift] = Shift) and ((Key = VK_RIGHT) or (Key = VK_LEFT)) then exit; // KMemo - extend selection one word left or right
    if ([ssShift] = shift) and (Key = VK_TAB) then begin
        if not CaretInTitle() then
              IndentControl(False);
        Key := 0;
        exit();
     end;
    {$endif DARWIN}

    if (Key = VK_ESCAPE) and Sett.CheckEscClosesNote.Checked then close;      // Will do normal save stuff first.

    if (Key = VK_DELETE) and AtEndOfLine()  then begin
            // debugln('Detected DELETE and end of line');                       // and its end of line and this line is bullet or indent ......
            if not CaretInTitle() then begin
                if DeleteAtEOL(False) then MarkDirty();
            end else if DeleteAtEOL(True) then MarkDirty();
            exit();                                                           // let Delete go through to KMemo
    end;
    // Record this event in the Undoer if its ssShift or empty set, rest are ctrl, meta etc ....

    if Use_Undoer and (([ssShift] = Shift) or ([] = Shift)) then              // while we pass presses like this to undoer, not all are
        Undoer.RecordInitial(Key);                                            // used, onKeyPress must follow and it gets only text type keys.




    {$ifndef DARWIN}
    // -------------- Shift -------------------
    if [ssShift] = shift then begin
        if (Key = VK_LEFT) or (Key = VK_RIGHT) then exit; // KMemo - extend selection one char left or right
        if (Key = VK_F3) then
        begin
            key := 0;
            if (EditFind.Text <> rsMenuSearch) then SpeedLeftClick(self);
       end;
       if (Key = VK_RETURN) then begin                    // Shift-Enter, intended to do so paragraph things but messes badly with bullets. #305
          Key := 0;
          exit();
       end;
       if (Key = VK_TAB) then begin                       // Shift-Tab, maybe anywhere in a line, first to last char.
          if not CaretInTitle() then
                IndentControl(False);
          Key := 0;
          exit();
       end;
    end;
    {$endif}

    // -------------- Control (Command on Mac) ------------------
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
            VK_M : begin Key := 0; DoRightClickMenu; end;
            VK_N : SearchForm.OpenNote('');
            VK_E : InitiateCalc();
            VK_TAB : begin if not CaretInTitle() then IndentControl(True); Key := 0; end;  // On Mac, OS hijacks sequence for its own use.
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
            VK_H      : begin AlterFont(ChangeColor); ; Key := 0; end; {$endif}
            VK_RIGHT  : begin if not CaretInTitle() then BulletControl(True);  Key := 0; end;
            VK_LEFT   : begin if not CaretInTitle() then BulletControl(False); Key := 0; end;
            VK_Return :  if (EditFind.Text <> rsMenuSearch) then begin Key := 0; SpeedLeftClick(self); end;
            VK_D      : begin BuildFileLink(False); Key := 0; end;
            VK_F      : begin BuildFileLink(True);  Key := 0; end;
        end;
        exit();
    end;

    // ------------------ Control and Shift (or, Mac, Command and Shift) ----------------
    if {$ifdef Darwin}[ssMeta, ssShift]{$else}[ssCtrl, ssShift]{$endif} = Shift then begin
        case Key of
            VK_F : SpeedButtonSearchClick(self);                            // Search all notes
            VK_G : if (EditFind.Text <> rsMenuSearch) then SpeedLeftClick(self);
            VK_Z : if Use_Undoer then Undoer.UnDo;
            {$ifndef DARWIN}
            VK_RIGHT, VK_LEFT : exit;   // KMemo knows how to do this, select word ...
            {$endif}
       end;
       Key := 0;
       exit();
    end;

    if Key = VK_TAB then begin    // Tab key sets a bullet or extends that bullet or Indent further right.
       {$ifdef LCLGTK3}debugln('TEditBoxForm.KMemo1KeyDown Key = Tab');{$endif}
       if not CaretInTitle() then begin
           ABlock := GetParagraphBlock();
           if ABlock <> nil then
               if (ABlock.ParaStyle.LeftPadding = 0) then BulletControl(True)    // Bullet is default for Tab but if already
               else IndentControl(True);                                         // one or the other, IndentControl will work it out
       end;
       Key := 0;
       exit();
    end;

    if Key = VK_F3 then begin
        key := 0;
        if (EditFind.Text <> rsMenuSearch) then SpeedRightClick(self);
    end;

    if (Key <> 8) or (not AtStartOfLine()) then                     // VK_BACK, only interested in a BS at start of line now
        exit();                                                     // KMemo will deal with everything else
    if Verbose then debugln('Dealing with a BS at start of line, might be Bullet or Indent');

    if BackSpaceBullet() then begin                                 // now handles both bullet and indent
        Key := 0;
        MarkDirty();
        exit;
    end;
    if Verbose then debugln('Letting BS go through to KMemo');
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
 //	T1, T2, T3, T4, T5 : qword;          // Temp time stamping to test speed
    W, H : integer;
begin
    // Timing numbers below using MyRecipes on my Dell linux laptop. For local comparison only !
    // Note QT5 times quite a lost faster, Loading is slow and so is resizing !  Sept 2022, updated and revised April 2023
    // Very useful to set default colors and size of form BEFORE loading note.
//    T1 := gettickcount64();
    GetHeightWidthOfNote(FileName, H, W);
    Width := W;
    Height := H;
    Loader := TBLoadNote.Create();
    Loader.FontNormal:= Sett.FontNormal;
    Loader.FontSize:= Sett.FontNormal;

    KMemo1.Blocks.LockUpdate;
    KMemo1.Clear;
    SetTheColors();
//    T2 := gettickcount64();
    Loader.LoadFile(FileName, KMemo1);                      // 100mS GTK2, without locking its all time and a lot of it !
//    T3 := gettickcount64();
    Createdate := Loader.CreateDate;
    //Ready := true;
    Caption := Loader.Title;
//    if Sett.ShowIntLinks or Sett.CheckShowExtLinks.checked then
//    	CheckForLinks(True);                  		         // 12mS (14ms GTK2)
    KMemo1.Blocks.UnlockUpdate;                              // 140mS
//    T4 := gettickcount64();
    Left := Loader.X;
    Top := Loader.Y;
    AdjustFormPosition();
    Loader.Free;                                             // 0mS
    TimerHouseKeeping.Enabled := False;     // we have changed note but no housekeeping reqired
//    T5 := gettickcount64();
//    debugln('Load Note=' + inttostr(T2 - T1) + 'mS ' + inttostr(T3 - T2) + 'mS ' + inttostr(T4 - T3) + 'mS ' + inttostr(T5 - T4) + 'mS ');
//    debugln('ImportNote Total=' + inttostr(T5 - T1) + 'mS ');
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
clean, goes direct to TheMainNoteLister with just an updated LCD. Note, the note
has not hit the disk yet !

That list is passed to SaveStringList, it makes a new thread that normalises
content of string list and then writes it to disk.
}

{$define SAVETHREAD}

function TEditBoxForm.SaveStringList(const SL: TStringList; Loc : TNoteUpdateRec; WeAreClosing : boolean) : boolean;
var
//    {$ifdef SAVETHREAD}
    TheSaveThread : TSaveThread;
//    {$else}
    Normaliser : TNoteNormaliser;
    WBufStream : TWriteBufStream;
    FileStream : TFileStream;
//    {$ENDIF}
begin
    if BusySaving then exit(False);
    BusySaving := True;
    Result := True;
    // {$ifdef SAVETHREAD}
    if not WeAreClosing then begin
        TheSaveThread := TSaveThread.Create(true);
        TheSaveThread.TheLoc := Loc;
        TheSaveThread.TheSL := Sl;
        //TheSaveThread.Title := Caption;        // better to use TheForm (as long as its still exists??)
        TheSaveThread.TheForm := self;
        TheSaveThread.Start;
        // It will clean up after itself.
    end else begin                               // When app is closing, we don't save in a thread, seems unreliable !
        // {$else}
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
          SL.Free;                  // note : this was created in calling process !
          PostMessage(sett.Handle, WM_SYNCMESSAGES,  WM_SAVEFINISHED, 0);   // Will release Lock
        end;
        BusySaving := False;
    end;
    // {$ENDIF}
end;

procedure TEditBoxForm.SaveTheNote(WeAreClosing : boolean = False);
var
    Title : string;
 	Saver : TBSaveNote;
    SL : TStringList;
    OldFileName : string ='';
    Loc : TNoteUpdateRec;
    NoteContent : string = '';                       // Might hold a lowercase text version of note for searching purposes.
    LineNumb   : integer = 0;
    FName      : string;
    ItsANewNote : boolean = false;
    //T1, T2, T3, T4, T5, T6, T7 : qword;            // Timing shown is for One Large Note.

begin

    //TheMainNoteLister.DumpNoteNoteList('TEditBoxForm.SaveTheNote ' + NoteTitle);
    if BusySaving then begin
        debugln('TEditBoxForm.SaveTheNote declined because BusySaving ' + FormatDateTime('hh:nn:ss.zzz', Now()) + ' form=' + Caption);
        MainForm.ShowNotification('Failed to Auto Save', 3000);   // inform user via notifications
        exit;
    end;
    //T1 := gettickcount64();
    Saver := Nil;
    if KMemo1.ReadOnly then exit();
  	if length(NoteFileName) = 0 then begin
        NoteFileName := Sett.NoteDirectory + GetAFilename();
        ItsANewNote := True;
    end;
    if (not WeAreClosing)                                                       // just checking for valid ID here
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
    Saver.CreateDate := CreateDate;                             // Misnamed now, does not save, just puts xml into SL
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
        Loc.LastChangeDate:= TB_GetLocalTime();                 // Next line makes a partial entry for a new note.
        SearchForm.UpdateList(CleanCaption(), Loc.LastChangeDate, NoteFileName, self);     // 6mS - 8mS timewasting menu rewrite ??  No usually now
        //debugln('TEditBoxForm.SaveTheNote - called  SearchForm.UpdateList with ' + CleanCaption());
    end else
        Loc.LastChangeDate                                      // Must be closing.
            := TheMainNoteLister.GetLastChangeDate(ExtractFileNameOnly(NoteFileName));
    // Dec 2022 - I moved the following block down here to ensure a new note is in note lister before setting content
    if Dirty and (not SingleNoteMode) and Sett.AutoSearchUpdate then begin      // This is quick,
        LineNumb := TheMainNoteLister.NoteList.Count -1;                        // start searching at end of list cos thats where new notes live
        FName := ExtractFileName(NoteFileName);
        while LineNumb > -1 do begin
            // If its a new note, we created a partial entry a few lines up.
            if TheMainNoteLister.NoteList[LineNumb]^.ID = FName then begin
                TheMainNoteLister.NoteList[LineNumb]^.Content := NoteContent;
                if ItsaNewNote then SearchForm.MarkLinkOnOpenNotes();           // In case note was created as a new Link from another note.
                break;
            end;
            dec(LineNumb);
        end;
        if LineNumb = -1 then debugln('TEditBoxForm.SaveTheNote did not find note in notelister to insert content into. ' + NoteTitle);
    end;
    if SaveStringList(SL, Loc, WeAreClosing) then Dirty := False;             // Note, thats not a guaranteed good save,

    //debugln({$I %CURRENTROUTINE%}, '() ', {$I %FILE%}, ', ', 'line:', {$I %LINE%}, ' : ', 'At end, dirty=' + booltostr(Dirty, true));
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
          // OK, we are looking at the part we want to change, ignore infile, we know better.
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

