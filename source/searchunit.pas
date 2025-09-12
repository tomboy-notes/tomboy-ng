unit SearchUnit;

{   Copyright (C) 2017-2024 David Bannon

    License:
    This code is licensed under MIT License, see the file License.txt
    or https://spdx.org/licenses/MIT.html  SPDX short identifier: MIT

    ------------------

    This form will put its icon in the System Tray and its resposible for acting
	on any of the menu choices from that tray icon.
    The form, and therefore the application, does not close if the user clicks
	the (typically top right) close box, just hides. It does not close until
	the user clicks 'close' from the System Tray Menu.

	It also displays the Search box showing all notes and manages the note_lister,
    the data structure holding info in memory of all notes.
}

{	HISTORY
	20170928 Added a function that returns true if passed string is in the
	current title list.
	20171005 - Added an ifdef Darwin to RecentNotes() to address a OSX bug that prevented
    the recent file names being updated.
	2017/10/10 - added a refresh ButtonSMenu, need to make it auto but need to look at
	timing implication for people with very big note sets first.

	2017/10/10 - added the ability to update the stringlist when a new note is
	created or an older one updated. So, recent notes list under TrayIcon is now
	updated whenever a save is made.

	2017/11/07 - switched over to using NoteLister, need to remove a lot of unused code.

	2017/11/28 - fixed a bug I introduced while restructuring  OpenNote to better
	handle a note being auto saved. This bug killed the Link ButtonSMenu in EditNote
	2017/11/29 - check to see if NoteLister is still valid before passing
	on updates to a Note's status. If we are quiting, it may not be.
	2017/12/03 Added code to clear Search box when it gets focus. Issue #9
	2017/12/05 Added tests that we have a Notes Directory before opening a new note
	or the search box. Issue #23.
	2017/12/27 Changes flowing from this no longer being the main form.
		1. Setting is now main form. This is to deal with a Cocoa issue where we
			we cannot Hide() in the OnShow event.
	2017/12/28 Ensured recent items in popup menu are marked as empty before user
				sets a notes dir.
	2017/12/29  DeleteNote() now moves file into Backup/.
	2017/12/30  Removed commented out code relting to calling Manual Sync
	2018/01/01  Added a check to see if FormSync is already visible before calling ShowModal
	2018/01/01  Added code to mark a previously sync'ed and now deleted note in local manifest.
	2018/01/01  Set goThumbTracking true so contents of scroll box glide past as
    			you move the "Thumb Slide".
	2018/01/01  Moved call to enable/disable the sync menu item into RecentMenu();
    2018/01/25  Changes to support Notebooks
    2018/01/39  Altered the Mac only function that decides when we should update
                the traymenu recent used list.
    2018/02/04  Don't show or populate the TrayIcon for Macs. Hooked into Sett's Main Menu
                for Mac and now most IconTray/Main menu items are responded to in Sett.
    2018/02/04  Now control MMSync when we do the Popup One.
    2018/04/12  Added ability to call MarkNoteReadOnly() to cover case where user has unchanged
                note open while sync process downloads or deletes that note from disk.
    2018/04/13  Taught MarkNoteReadOnly() to also delete ref in NoteLister to a sync deleted note
    2018/05/12  Extensive changes - MainUnit is now just that. Name of this unit changed.
    2018/05/20  Alterations to way we startup, wrt mainform status report.  Mark
    2018/06/04  NoteReadOnly() now checks if NoteLister is valid before calling.
    2018/07/04  Pass back some info about how the note indexing went.
    2018/08/18  Can now set search option, Case Sensitive, Any Combination from here.
    2018/08/18  Update Mainform line about notes found whenever IndexNotes() is called.
    2018/11/04  Added ProcessSyncUpdates to keep in memory model in line with on disk and recently used list
    2018/11/25  Now uses Sync.DeleteFromLocalManifest(), called when a previously synced note is deleted
    2018/12/29  Small improvements in time to save a file.
    2019/02/01  OpenNote() now assignes a new note to the notebook if one is open (ie ButtonNotebookOptions is enabled)
    2019/02/09  Move autosize stringgrid1 (back?) into UseList()
    2019/02/16  Clear ButtonSMenu now calls UseList() to ensure autosize happens.
    2019/03/13  Now pass editbox the searchterm (if any) so it can move cursor to first occurance in note
    2019/04/07  Restructured Main and Popup menus. Untested Win/Mac.
    2019/04/13  Don't call note_lister.GetNotes more than absolutly necessary.
    2019/04/15  One Clear Filters ButtonSMenu to replace Clea and Show All Notes. Checkboxes Mode instead of menu
    2019/04/16  Fixed resizing atifacts on stringGrids by turning off 'Flat' property, Linux !
    2019/08/18  Removed AnyCombo and CaseSensitive checkboxes and replaced with SearchOptionsMenu, easier translations
    2019/11/19  When reshowing an open note, bring it to current workspace, Linux only. Test on Wayland !
    2019/12/11  Heavily restructured Startup, Main Menu everywhere !
    2019/12/12  Commented out #868 that goRowHighlight to stringgridnotebook, ugly black !!!!!
    2019/12/19  Restored the File Menu names to the translate system.
    2020/01/24  Fixed a Qt5 startup issue, don't fill in RecentItems in menu before File & Help are there.
    2020/01/29  A lot of tweaks around UseList(), MMenu Recent no longer from StringGrid, ctrl updates to speed up.
    2020/01/31  LoadStringGrid*() now uses the Lazarus column mode.
                Better ctrl of Search Term highlight (but still highlit when makeing form re-visible).
                Drop Create Date and Filename from Search results string grid.
                But I still cannot control the little green triangles in stringgrid headings indicating sort.
    2020/02/01  Do not refresh the string grids automatically, turn on the refresh ButtonSMenu for user to do it.
    2020/02/19  hilight selected notebook name.
    2020/03/09  Make sure 'x' (put in by a bug) is not a valid sync repo path.
    2020/05/10  Faster search
    2020/05/19  Replaced StringGridNotebook with a ListBox
    2020/06/07  ListBoxNotebooks sorted (but not reverse sortable, that would require TListBox becoming TListView)
    2020/07/09  New help notes location.
    2020/07/17  OpenNote was checking EditSearch.test = 'search' instead of rsMenuSearch
    2020/11/14  ListViewNotes now has alternating colours, req ugly fix for Qt5 involving increasing font size
    2020/12/10  Move focus to Search Field whenever Search Form is re-shown, issue #211
    2021/01/22  When activating a note from the search form, jump to first match if Search Term is not empty
    2021/01/23  A check box to choose Auto Refresh or not.
    2021/02/11  Some debugs around Ctrl-Q, to be removed and make two listboxes respond to Ctrl-N
    2021/02/14  Direct all key down events via Form's OnKeyDown handler Ctrl-N and Ctrl-Q
    2021/07/05  UpDateList now only refreshes menu if item on top has changed
    2021/08/02  Use Template when creating new note from Template. Sigh ....
                And don't update notelister (and menus) if its a Notebook thats been edited.
    2021/09/25  Fix bug that prevented saving first note in a dir, introduced in July. Nasty.
    2021/11/03  When deleteing a notebook, remove references to it from the notes.
    2021/11/04  Changes to support new Notebook management model
    2021/12/03  Moved checkAutoRefresh to Settings, replaced with SpeedSearchOtions and menu
                Added all code necessary for Searching for note while u type, NoteIndex
    2022/04/18  Bug where searching notes in progressive mode and backspacing over search term failed
    2022/08/27  Alterations to ListViewNotesColumnClick() and ListViewNotesData() to work in OwnerData mode.
    2022/09/06  PressEnter Seach mode now OK as well, cleans up when Search form closes or hides.
    2022/09/08  Two bugs that appear when no notes present
    2022/09/08  Update Notes Found number on small splash screen  #267
    2022/09/13  Tweaks to manage the ListViewNotes sort indicators, must 'Bounce'.
    2022/10/20  Added an Import menu item to Options.
    2022/10/29  Auto remove Search Prompt from start of search term.
    2022/12/31  EditSearch now uses TestHint.
    2023/01/11  Qt5 - ListViewNotesKeyPress now forces keypress to EditSearch
    2023/01/11  Added Windows to above, BUT Mac cannot do this. So, disable on Mac.
    2023/03/17  Darken up Search Window in dark theme.
    2024/10/16  Fixed the way that Save on Quit works, no contention !
}

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
    ComCtrls, StdCtrls, ExtCtrls, Menus, Buttons, Note_Lister, lazLogger,
    ResourceStr;


// These are choices for main popup menus.
type TMenuTarget = (mtSep=1, mtNewNote, mtSearch, mtAbout=10, mtSync, mtSettings, mtMainHelp, mtHelp, mtQuit, mtRecent);

// These are the possible kinds of main menu items
type TMenuKind = (mkFileMenu, mkRecentMenu, mkHelpMenu, mkAllMenu);


type        { TSearchForm }
    TSearchForm = class(TForm)
        BitBtnMenu: TBitBtn;
        ButtonSearchOptions: TButton;
      ButtonClearSearch: TButton;
	    ButtonClearFilters: TButton;
        EditSearch: TEdit;
        LabelSearchTitle : TLabel;
        ListBoxNotebooks: TListBox;
        ListViewNotes: TListView;
		MenuEditNotebookTemplate: TMenuItem;
		MenuDeleteNotebook: TMenuItem;
        MenuCreateNoteBook: TMenuItem;
        MenuItemSearchTitleOnly : TMenuItem;
        MenuItemNoteBookMembership: TMenuItem;
        MenuItemSelectAll: TMenuItem;
        MenuItemSelectNone: TMenuItem;
        MenuItemOpenSelected: TMenuItem;
        MenuItemDeleteSelected: TMenuItem;
        MenuItemImportNote: TMenuItem;
        MenuItemCaseSensitive: TMenuItem;
        MenuItemSWYT: TMenuItem;
        MenuItemManageNBook: TMenuItem;
        MenuItem3: TMenuItem;
        MenuRenameNoteBook: TMenuItem;
		MenuNewNoteFromTemplate: TMenuItem;
        OpenDialogImport: TOpenDialog;
		Panel1: TPanel;
        Panel2: TPanel;
        PopupMenuListOptions: TPopupMenu;
        PopupMenuSearchOptions: TPopupMenu;
		PopupMenuNotebook: TPopupMenu;
		Splitter1: TSplitter;
        StatusBar1: TStatusBar;
        SelectDirectoryDialog1: TSelectDirectoryDialog;
        procedure BitBtnMenuClick(Sender: TObject);
        procedure ButtonClearSearchClick(Sender: TObject);

                                    { If a search is underway, searches.  Else, if we have
                                      an active notebook filter applied, reapply it. Failing
                                      both of the above, refreshes the Notes and Notebooks
                                      with data in Note_Lister. }

		procedure ButtonClearFiltersClick(Sender: TObject);
        procedure ButtonSearchOptionsClick(Sender: TObject);
        procedure ButtonSMenuClick(Sender: TObject);
        procedure EditSearchChange(Sender: TObject);
        procedure EditSearchEnter(Sender: TObject);
        procedure EditSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
                            // called after OnShow.
        procedure FormActivate(Sender: TObject);
		procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
        procedure FormCreate(Sender: TObject);
        procedure FormDeactivate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
        procedure FormHide(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormResize(Sender: TObject);
		procedure FormShow(Sender: TObject);
        procedure ListBoxNotebooksClick(Sender: TObject);
        procedure ListBoxNotebooksMouseUp(Sender: TObject;
            ButtonSMenu: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure ListViewNotesColumnClick(Sender: TObject; Column: TListColumn);
        procedure ListViewNotesData(Sender: TObject; Item: TListItem);
        procedure ListViewNotesDblClick(Sender: TObject);
		procedure ListViewNotesDrawItem(Sender: TCustomListView;
				AItem: TListItem; ARect: TRect; AState: TOwnerDrawState);

        procedure ListViewNotesKeyPress(Sender: TObject; var Key: char);
        procedure ListViewNotesMouseDown(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
		procedure MenuDeleteNotebookClick(Sender: TObject);
		procedure MenuEditNotebookTemplateClick(Sender: TObject);
        procedure MenuCreateNoteBookClick(Sender: TObject);
        procedure MenuItemDeleteSelectedClick(Sender: TObject);
        procedure MenuItemNoteBookMembershipClick(Sender: TObject);
        procedure MenuItemOpenSelectedClick(Sender: TObject);
        procedure MenuItemCaseSensitiveClick(Sender: TObject);
        procedure MenuItemImportNoteClick(Sender: TObject);
        procedure MenuItemManageNBookClick(Sender: TObject);
        procedure MenuItemSearchTitleOnlyClick(Sender : TObject);
        procedure MenuItemSelectAllClick(Sender: TObject);
        procedure MenuItemSelectNoneClick(Sender: TObject);
        procedure MenuItemSWYTClick(Sender: TObject);
        procedure MenuRenameNoteBookClick(Sender: TObject);
                        // Rather than opening an empty note we copy the template.
                        // save it, index it and pass the filename to OpenNote(
		procedure MenuNewNoteFromTemplateClick(Sender: TObject);
        { Recieves 2 lists from Sync subsystem, one listing deleted notes ID, the
          other downloded note ID. Adjusts Note_Lister according and marks any
          note that is currently open as read only. Does not move files around. }
        procedure ProcessSyncUpdates(const DeletedList, DownList: TStringList);
                // A proc that is called when a note is added to repo by, eg, an import.
                // The procedure's address is passed, via tb_utils, to the CLI unit so it
                // knows to call this direct if its not nil.
        procedure IndexNewNote(const FFName: string; CheckTitleClash: boolean);

    private
        MoveFocusChar : char;
        SearchTextLength : integer;     // Previous length of EditSearch text, tells us if term is growing or shrinking
        SearchActive : boolean;         // We have searched for something after most recent SearchClear


//        NIndex : TNoteIndex;
        HelpList : TStringList;
        //NumbToRefresh : integer;        // The number of notes to display if a delayed Refresh() is appropriate
        // NeedRefresh : boolean;          // Indicates a delayed Refresh() could happen
        HelpNotes : TNoteLister;
        LVSortMode : TLVSortMode;       // Sort direction for the ListViewNotes, OnData needs to know.
        procedure AddItemMenu(TheMenu: TPopupMenu; Item: string;
            mtTag: TMenuTarget; OC: TNotifyEvent; MenuKind: TMenuKind);
                                        // ListView, in OwnerData mode seems to need to get its SortIndicator bounced
                                        // after refreshing content.
        procedure BounceSortIndicator(Col: integer);

        procedure CreateMenus();
        procedure DoSearchEnterPressed();
        procedure FileMenuClicked(Sender: TObject);

        procedure InitialiseHelpFiles();
                                // Copy Template to a new name removing the <tag>system:template</tag> and setting a Title
        function MakeNoteFromTemplate(const Template: String): string;

                                // clears then Inserts file items in all main menus, note also removes help items ....
        procedure MenuFileItems(AMenu: TPopupMenu);
        procedure MenuHelpItems(AMenu: TPopupMenu);
                                // Builds a list of all the main Menus we have floating around at the moment.
        procedure MenuListBuilder(MList: TList);
        procedure RecentMenuClicked(Sender: TObject);
                                // Gets called to refresh the ListViewNotes in cases were we may not do it immediatly
                                // If ImmediateRefresh, we use the previously recorded NumbToRefresh and clear ButtonSMenu
                                // Else re do a new search or clear depending on existing search parameters.
		procedure IndexAndRefresh(DisplayOnly: boolean = false);
        function RemoveFromHelpList(const FullHelpNoteFileName: string): boolean;
        procedure RemoveNBTag(NB: string);
        //procedure RefreshNoteAndNotebooks();
        procedure ScaleListView();
        { If there is an open note from the passed filename, it will be marked read Only,
          will accept a GUID, Filename or FullFileName inc path }
        procedure MarkNoteReadOnly(const FullFileName: string);

		//procedure ShowListIndicator(St: string);

    public

        PopupTBMainMenu : TPopupMenu;
        SelectedNotebook : integer;         // Position in Notebook grid use has clicked, 0 means none.
        //AllowClose : boolean;
//        NoteLister : TNoteLister;
        NoteDirectory : string;
                            { Public : Called from EditBox when it saves a new note. If that
                            new note was created by a New Link request, its necessary to mark
                            that text as a Link. Note int marks only the potential links near
                            the cursor at that time.
                            }
        procedure MarkLinkOnOpenNotes();
                            { Tells all open notes to save their contents. Used,
                            eg before we run a sync to ensure recently changed content
                            is considered by the (File based) sync engine.}
        procedure FlushOpenNotes();
                            { Makes a backup note with last three char of manin name being
                            the PutInName that tells us where it came from, ttl - title
                            opn - just opened. Does nothing if name not UUID length.
                            Pass it a ID, Filename or FullFileName }
        procedure BackupNote(const NoteName, PutIntoName: string);
                            // Public procedure to show the help note named (without path info)
        procedure ShowHelpNote(HelpNoteName: string);
        procedure UpdateStatusBar(I: integer; SyncSt: string);
        {Just a service provided to NoteBook.pas, refresh the list of notebooks after adding or removing one}
        procedure RefreshNotebooks();
        // Fills in the Main TB popup menus. If AMenu is provided does an mkAllMenu on
        // that Menu, else applies WhichSection to all know Main TB Menus.
        procedure RefreshMenus(WhichSection: TMenuKind; AMenu: TPopupMenu=nil);
                            { Public method to move passed form to this workspace on Linux or,
                            other OS, to just bring it to the front. }
        procedure MoveWindowHere(Frm : TForm);
         	                { Puts the names of recently used notes in the indicated menu, removes esisting ones first. }
        procedure MenuRecentItems(AMenu : TPopupMenu);
       	                    { Call this so NoteLister no longer thinks of this as a Open note }
        procedure NoteClosing(const ID: AnsiString);
                            { Updates the In Memory List with passed data. Either updates existing data or inserts new }
        procedure UpdateList(const Title, LastChange, FullFileName: ANSIString; TheForm : TForm);
                            { Reads header in each note in notes directory, updating Search List and
                              the recently used list under the TrayIcon. Downside is time it takes
                              to index. use UpdateList() if you just have updates.
                              ReRunSearch might be necessary, for TheMainNoteLister, if its a re-run.}
        function IndexNotes(ReRunSearch: boolean = False): integer;
                            { Returns true when passed string is the title of an existing note }
        //function IsThisaTitle(const Term: ANSIString): boolean;

                            { Gets called with a title and filename (clicking grid), with just a title
                            (clicked a note link or recent menu item or Link ButtonSMenu) or nothing
                            (new note). If its just Title but Title does not exist, its Link
                            ButtonSMenu. DontBackUp says do not make a backup as we open because we are in
                            a Roll Back Cycle.}
        procedure OpenNote(NoteTitle: String; FullFileName: string = '';
            TemplateIs: AnsiString=''; BackUp: boolean=True; InSearch: boolean=false; STerm : string='');
        { Returns True if it put next Note Title into SearchTerm }
        //function NextNoteTitle(out SearchTerm : string) : boolean;
        { Initialises search of note titles, prior to calling NextNoteTitle() }
//        procedure StartSearch();

        { Deletes the actual file then removes the indicated note from the internal data
          about notes, updates local manifest, refreshes Grid, may get note or template }
        procedure DeleteNote(const FullFileName : ANSIString);


const
	MenuEmpty = '(empty)';

    end;

var
    SearchForm: TSearchForm;

implementation

{$R *.lfm}

//{$define LVOWNERDRAW}     // Ownerdraw of ListViewNotes gives us alternating colours but all sorts of problems
                            // I'll try a release without it and, maybe, try agin later. And maybe not.

uses MainUnit,      // Opening form, manages startup and Menus
    EditBox,
    settings,		// Manages settings.
    LCLType,		// For the MessageBox
    LazFileUtils,   // LazFileUtils needed for TrimFileName(), cross platform stuff
    sync,           // because we need it to manhandle local manifest when a file is deleted
    process,        // Linux, we call wmctrl to move note to current workspace
    LCLVersion,     // used to enable, or not, sort indicators in lcl2.0.8 or later
    NoteBook,
    tb_utils,
    {$ifdef LINUX}MvXWindow,{$endif}   // to move an XWindow (under linux) between workspaces
    cli;            // We call the ImportNote function there.




{ TSearchForm }

var
    // an array of note ID.note selected when user right clicked Notes ListView
    NoteListRightClickSel : TStringArray = ();

{ -------------   FUNCTIONS  THAT  PROVIDE  SERVICES  TO  OTHER   UNITS  ------------ }


procedure TSearchForm.ProcessSyncUpdates(const DeletedList, DownList : TStringList);
// The lists arrive here with just the 36 char ID, the following functions must be OK with that !
// Does not get called with both lists empty
var
    Index : integer;
begin
    if TheMainNoteLister <> nil then begin
        for Index := 0 to DeletedList.Count -1 do begin                         // Deleted notes
            if TheMainNoteLister.IsATemplate(DeletedList.Strings[Index]) then begin
                TheMainNoteLister.DeleteNoteBookwithID(DeletedList.Strings[Index]);
                RefreshNotebooks();
            end else begin
                MarkNoteReadOnly(DeletedList.Strings[Index]);
                TheMainNoteLister.DeleteNote(DeletedList.Strings[Index]);       // do not call this, wont do Indexes
            end;
        end;
        for Index := 0 to DownList.Count -1 do begin                            // Downloaded notes
            MarkNoteReadOnly(DownList.Strings[Index]);
            if TheMainNoteLister.IsIDPresent(DownList.Strings[Index]) then begin
                TheMainNoteLister.DeleteNote(DownList.Strings[Index]);
                //debugln('We have tried to delete ' + DownList.Strings[Index]);
            end;
            RefreshNotebooks();
            TheMainNoteLister.IndexThisNote(DownList.Strings[Index]);
            //debugln('We have tried to reindex ' + DownList.Strings[Index]);
        end;

        IndexAndRefresh();                                                      // Reruns the current search with new data in NoteList
        TheMainNoteLister.BuildDateAllIndex();                            // Must rebuild it before refreshing menus.
        RefreshMenus(mkRecentMenu);
        MainForm.UpdateNotesFound(TheMainNoteLister.NoteList.Count);

        {
        Visible        T            F          T          F
        Checked        T            T          F          F

        Refresh       Yes           n          n          n
        NeedRefresh   n            Yes        Yes        Yes
        EnableButt    n             n         Yes         n
        }

//        if Visible and Sett.AutoRefresh then        // CheckAutoRefresh.checked then
//            Refresh()
//        else begin
//            if Visible then ButtonRefresh.Enabled := True
//            else NeedRefresh := True;
//        end;
    end;
end;

procedure TSearchForm.MarkLinkOnOpenNotes();
var
    AForm : TForm;
    Blar : integer;
begin
    if assigned(TheMainNoteLister) then begin
        AForm := TheMainNoteLister.FindFirstOpenNote(Blar);
        while AForm <> Nil do begin
            TEditBoxForm(AForm).CheckForLinks(False);
            AForm := TheMainNoteLister.FindNextOpenNote(Blar);
        end;
    end;
end;

procedure TSearchForm.FlushOpenNotes();
var
    AForm : TForm;
    Blar : integer;
begin
    if assigned(TheMainNoteLister) then begin
      AForm := TheMainNoteLister.FindFirstOpenNote(Blar);
      while AForm <> Nil do begin
          if TEditBoxForm(AForm).dirty then
              TEditBoxForm(AForm).SaveTheNote();
          AForm := TheMainNoteLister.FindNextOpenNote(Blar);
      end;
    end;
end;


procedure TSearchForm.NoteClosing(const ID : AnsiString);
begin
    if TheMainNoteLister <> nil then                              // else we are quitting the app !
    	if not TheMainNoteLister.ThisNoteIsOpen(ID, nil) then     // this marks note as not open
            // maybe its a help note ?
            RemoveFromHelpList(ID);
end;

(*
procedure TSearchForm.StartSearch(); // Call before using NextNoteTitle() to list Titles.
begin                                // ToDo : not needed, Editbox now goes direct
	TheMainNoteLister.StartSearch();
  // TitleIndex := 1;
end;
*)

{ Removes the indicated NoteBook tag from any note that has it }
procedure TSearchForm.RemoveNBTag(NB : string);
var
    STL : TStringList;    // note: NoteLister.GetNotesInNoteBook does not need STL created or freed !
    i : integer = 0;
    Dummy : TForm;
begin
    if NB = '' then exit;
    if TheMainNoteLister.GetNotesInNoteBook(StL, NB) then
        while i < StL.Count do begin
            if TheMainNoteLister.IsThisNoteOpen(STL[i], Dummy) then continue;   // don't bother to do open notes. // ToDo : test this
            RemoveNoteBookTag(Sett.NoteDirectory + STL[i], NB);
            inc(i)
        end;
end;

procedure TSearchForm.DeleteNote(const FullFileName: ANSIString);
var
    NewName, ShortFileName : ANSIString;
    // LocalMan : TTomboyLocalManifest;
    LocalMan : TSync;
begin
    // debugln('DeleteNote ' + FullFileName);
    ShortFileName := ExtractFileNameOnly(FullFileName);      // an ID
    LocalMan := TSync.Create;                                // we use TSync to get access to the Sync Manifests, no netwrk traffic
    LocalMan.DebugMode:=false;
    LocalMan.ConfigDir:= Sett.LocalConfig;
    LocalMan.NotesDir:= Sett.NoteDirectory;
    if not LocalMan.DeleteFromLocalManifest(copy(ShortFileName, 1, 36)) then
        showmessage('Error marking note delete in local manifest ' + LocalMan.ErrorString);
    LocalMan.Free;
    if TheMainNoteLister.IsATemplate(ShortFileName) then begin
        // this does not remove notebook tag from any notes that were members of this note.
        // if the note is Open, thats OK, it will be saved correctly on exit.
        RemoveNBTag(TheMainNoteLister.GetNotebookName(ShortFileName));        // remove ref to the notebook from all notes
        TheMainNoteLister.DeleteNoteBookwithID(ShortFileName);
      	DeleteFileUTF8(FullFileName);
        // ButtonClearFiltersClick(self);
        RefreshNoteBooks();
    end else begin
        TheMainNoteLister.RemoveNoteFromNoteBooks(ShortFileName);
		TheMainNoteLister.DeleteNote(ShortFileName);                          // Remove from NoteList
     	NewName := Sett.NoteDirectory + 'Backup' + PathDelim + ShortFileName + '.note';
    	if not DirectoryExists(Sett.NoteDirectory + 'Backup') then
    		if not CreateDirUTF8(Sett.NoteDirectory + 'Backup') then
            	DebugLn('Failed to make Backup dir, ' + Sett.NoteDirectory + 'Backup');
        if FileExistsUTF8(NewName) then
            DeleteFileUTF8(NewName);
    	if not RenameFileUTF8(FullFileName, NewName) then begin
    		DebugLn('Failed to move ' + FullFileName + ' to ' + NewName);
            showmessage('TSearchForm.DeleteNote - failed to rename to ' + NewName);
        end;
    end;

    IndexAndRefresh(False);                                          // ToDo : if Delete updated Index, could be DisplayOnly
    TheMainNoteLister.BuildDateAllIndex();                            // Must rebuild it before refreshing menus.
    RefreshMenus(mkRecentMenu);
    MainForm.UpdateNotesFound(TheMainNoteLister.NoteList.Count);
end;

(*function TSearchForm.NextNoteTitle(out SearchTerm: string): boolean;
begin
	Result := TheMainNoteLister.NextNoteTitle(SearchTerm);
end;  *)

{function TSearchForm.IsThisaTitle(const Term : ANSIString) : boolean;
begin
	Result := TheMainNoteLister.IsThisATitle(Term);
end;  }

procedure TSearchForm.RefreshNotebooks();
var
    Index : integer;
begin
    Index := ListBoxNotebooks.ItemIndex;
    TheMainNoteLister.LoadListNotebooks(ListBoxNotebooks.Items, ButtonClearFilters.Enabled);
    if (Index > -1) and (Index <  ListBoxNotebooks.Count) then begin
        //debugln('TSearchForm.RefreshNotebooks - I='+inttostr(Index) + 'LBC=' + inttostr(ListBoxNotebooks.Count));
         ListBoxNotebooks.ItemIndex := Index;
         ListBoxNotebooksClick(self);
    end;
end;

procedure TSearchForm.UpdateStatusBar(I : integer; SyncSt: string);
begin
    //StatusBar1.SimpleText:= SyncSt;
    StatusBar1.Panels.Items[I].Text := SyncSt;
end;

procedure TSearchForm.UpdateList(const Title, LastChange, FullFileName : ANSIString; TheForm : TForm );
var
    // T1, T2, T3, T4 : qword;
    NeedUpdateMenu    : boolean = False;         // Updating the menu can be a bit slow.
    //NeedUpdateDisplay : boolean = false;         // Only set if Title has changed.
    i : integer;
    ReRunSearch : boolean = false;
    //STL : TStringList;
    //NoteBook : string;
begin
    { Called when a note is (soon to be saved) saved, date is always new, Title may have changed,
    note may be a new one. We always send date to NoteList, maybe update menu,
    maybe rerun the existing search, maybe just update display. In fact, we
    update the display unless note is not shown in ListView.    }
    // T1 := gettickcount64();
    if TheMainNoteLister = Nil then exit;				// we are quitting the app !
    // We don't do any of this if the its a notebook.
    if TheMainNoteLister.IsATemplate(ExtractFileNameOnly(FullFileName)) then exit;

    // do we need to update menus ? Its time consuming, if this is already top entry, leave alone
    i := 0;
    while i < PopupTBMainMenu.Items.Count do begin
        if PopupTBMainMenu.Items[i].Tag = ord(mtRecent) then begin      // Find first Recent Menu item
           if PopupTBMainMenu.Items[i].Caption <> Title then
               NeedUpdateMenu := True;
           break;                                                       // jump out after testing first recent
        end;
        inc(i);
    end;
    if i = PopupTBMainMenu.Items.Count then NeedUpdateMenu := True;     // dropped right through without a mtRecent
    // T2 := gettickcount64();
    if TheMainNoteLister.AlterOrAddNote(ReRunSearch, FullFileName, LastChange, Title) then begin
        if ReRunSearch then begin
            // If neither a search term nor Notebook is set, just call ClearSearch, its fast
            if (((EditSearch.Text = '') or (EditSearch.Text = rsMenuSearch))
                            and (ListBoxNoteBooks.ItemIndex < 0)) then begin
                ListViewNotes.Items.clear;                              // stops an annoying GTK2 message
                ListViewNotes.Items.Count := TheMainNoteLister.ClearSearch()
            end else begin
                IndexAndRefresh(False);
             end;
        end else                                                 // Just need to display, Indexes have been adjusted
            IndexAndRefresh(True);
    end;
    // T3 := gettickcount64();
    TheMainNoteLister.ThisNoteIsOpen(FullFileName, TheForm);
    if NeedUpDateMenu then RefreshMenus(mkRecentMenu);
    MainForm.UpdateNotesFound(TheMainNoteLister.NoteList.Count);

{    T4 := gettickcount64();
    debugln('SearchUnit.UpdateList TestMenu=' + inttostr(T2 - T1)
                    + 'mS AlterAdd=' + inttostr(T3 - T2) + 'mS Menu=' + inttostr(T4 - T3)
                    + 'mS Menu=' + booltostr(NeedUpdateMenu, True));  }

{ Timing, 2K notes, Dell, release mode.  August 2022

  Change Content        1mS
  Change Title          20mS        mostly AlterAdd but inc 5-7mS rewriting menu
                                    AlterAdd is mostly call to SearchClear (Sort)
  If there is a searchTerm, NewSearch is called, if not, ClearSearch() - both involve
  much longer Sort than when iniially run. Build is quick, ~1mS but each Sort is
  ~11mS (nominally 10x longer than first call).

  Still, updating Menu is a problem }

    // Must do some refresh() stuff here.

end;


// ----------------------------------------------------------------------------
// ---------------  H E L P    N O T E S  -------------------------------------



procedure TSearchForm.InitialiseHelpFiles();
    // Todo : this uses about 300K, 3% of extra memory, better to code up a simpler model ?
begin
    if HelpNotes <> nil then
        freeandnil(HelpNotes);
    HelpNotes := TNoteLister.Create;     // freed in OnClose event.
    HelpNotes.DebugMode := Application.HasOption('debug-index');
    // HelpNotes.WorkingDir:= MainForm.ActualHelpNotesPath;
    HelpNotes.WorkingDir:= Sett.HelpNotesPath + Sett.HelpNotesLang + PathDelim;
    HelpNotes.IndexNotes(true);
end;

function TSearchForm.RemoveFromHelpList(const FullHelpNoteFileName : string) : boolean;
var
    Index : integer;
begin
    Result := False;
    //debugln('Looking for help note ' + extractFileName(fullHelpNoteFileName));
    if HelpList <> Nil then
        if HelpList.Find(extractFileName(FullHelpNoteFileName), Index) then begin
            //debugln('Found help note ' + extractFileName(fullHelpNoteFileName));
            HelpList.Delete(Index);
            Result := True;
        end;
end;

procedure TSearchForm.ShowHelpNote(HelpNoteName: string);
var
    EBox : TEditBoxForm;
    TheForm : TForm;
    Index : integer;
begin
    if FileExists(Sett.HelpNotesPath + Sett.HelpNotesLang + PathDelim + HelpNoteName) then begin
        If HelpList = nil then begin
            HelpList := TStringList.Create;
            HelpList.Sorted:=True;
		end else begin
            if HelpList.Find(HelpNoteName, Index) then begin
                // we now try to remove entries from HelpList when a help note is closed.
                // This is far prettier when running under debugger, user does not care.
                try
                    TheForm := TEditBoxForm(HelpList.Objects[Index]);
                    debugln('Attempting a reshow of ' + HelpNoteName);
                    {$ifdef LINUX}TheForm.Hide;{$endif}             // Might be on another Linux Workspace.
          	        TheForm.Show;
                    SearchForm.MoveWindowHere(TheForm);
                    //TheForm.EnsureVisible(true);
                    exit;
				except on E: Exception do {showmessage(E.Message)};
                // If user had this help page open but then closed it entry is still in
                // list so we catch the exception, ignore it and open a new note.
                // its pretty ugly under debugger but user does not see this.
				end;
			end;
		end;
        // If we did not find it in the list and exit, above, we will make a new one.
        EBox := TEditBoxForm.Create(Application);
        EBox.SetReadOnly(False);
        EBox.SearchedTerm := '';
        EBox.NoteTitle:= '';
        EBox.NoteFileName := Sett.HelpNotesPath + Sett.HelpNotesLang + PathDelim + HelpNoteName;
        Ebox.TemplateIs := '';
        EBox.Show;
        EBox.Dirty := False;
        HelpList.AddObject(HelpNoteName, EBox);
        EBox.Top := HelpList.Count * 10;
        EBox.Left := HelpList.Count * 10;
        EBox.Width := Screen.Width div 2;      // Set sensible sizes.
        EBox.Height := Screen.Height div 2;
    end else showmessage('Unable to find ' + Sett.HelpNotesPath + Sett.HelpNotesLang + PathDelim + HelpNoteName);
end;

// ---------------------------------------------------------------------------
// -------------  M E N U    F U N C T I O N S -------------------------------
// ---------------------------------------------------------------------------

{ Menus are built and populated at end of CreateForm. }

procedure TSearchForm.CreateMenus();
begin
    InitialiseHelpFiles();
    PopupTBMainMenu := TPopupMenu.Create(self);      // LCL will dispose because of 'self'
    BitBtnMenu.PopupMenu := PopupTBMainMenu;
    MainForm.MainTBMenu := TPopupMenu.Create(self);
    MainForm.ButtMenu.PopupMenu := MainForm.MainTBMenu;
    // Add any other 'fixed' menu here.
end;


procedure TSearchForm.MenuListBuilder(MList : TList);
var
    AForm : TForm;
    Blar : integer;
begin
    if assigned(TheMainNoteLister) then begin
      AForm := TheMainNoteLister.FindFirstOpenNote(Blar);
      while AForm <> Nil do begin
          MList.Add(TEditBoxForm(AForm).PopupMainTBMenu);
          AForm := TheMainNoteLister.FindNextOpenNote(Blar);
      end;
    end;
    if assigned(PopupTBMainMenu) then
        MList.Add(PopupTBMainMenu);
    if assigned(MainForm.MainTBMenu) then
        MList.Add(MainForm.MainTBMenu);
    if (MainForm.UseTrayMenu) and assigned(MainForm.PopupMenuTray) then
        MList.Add(MainForm.PopupMenuTray);
    if assigned(Sett.PMenuMain) then
        MList.Add(Sett.PMenuMain);
end;

procedure TSearchForm.RefreshMenus(WhichSection : TMenuKind; AMenu : TPopupMenu = nil);
var
    MList : TList;
    I : integer;
    // T1, T2, T3, T4, T5, T6 : qword;
begin
    if (WhichSection = mkRecentMenu) and (PopupTBMainMenu.Items.Count = 0)
        then exit;      // This is a call during startup, File and Help are not there yet, messes with Qt5

    //debugln('In RefreshMenus');
    if AMenu <> Nil then begin
          MenuFileItems(AMenu);
          MenuHelpItems(AMenu);
          MenuRecentItems(AMenu);
          exit();
    end;

    MList := TList.Create;
    MenuListBuilder(MList);
    //T1 := gettickcount64();
    case WhichSection of
        mkAllMenu : for I := 0 to MList.Count - 1 do begin
                            MenuFileItems(TPopupMenu(MList[i]));
                            MenuHelpItems(TPopupMenu(MList[i]));
                            MenuRecentItems(TPopupMenu(MList[i]));
                        end;
        mkFileMenu : for I := 0 to MList.Count - 1 do
                            MenuFileItems(TPopupMenu(MList[i]));
        mkRecentMenu : for I := 0 to MList.Count - 1 do
                            MenuRecentItems(TPopupMenu(MList[i]));
        mkHelpMenu : for I := 0 to MList.Count - 1 do begin
                            InitialiseHelpFiles();
                            MenuHelpItems(TPopupMenu(MList[i]));
                        end;
    end;
    MList.Free;

end;

procedure TSearchForm.AddItemMenu(TheMenu : TPopupMenu; Item : string; mtTag : TMenuTarget; OC : TNotifyEvent; MenuKind : TMenuKind);
var
    MenuItem : TMenuItem;

            procedure AddHelpItem();
            var
                X : Integer = 0;
            begin
                while X < TheMenu.Items.Count do begin
                    if TheMenu.Items[X].Tag = ord(mtMainHelp) then begin
                        TheMenu.Items[X].Add(MenuItem);
                        exit;
                    end;
                    inc(X);
                end;
            end;
begin
    if Item = '-' then begin
        TheMenu.Items.AddSeparator;
        TheMenu.Items.AddSeparator;
        exit();
    end;
    MenuItem := TMenuItem.Create(Self);
    if mtTag = mtQuit then
        {$ifdef DARWIN}
        MenuItem.ShortCut:= KeyToShortCut(VK_Q, [ssMeta]);
        {$else}
        MenuItem.ShortCut:= KeyToShortCut(VK_Q, [ssCtrl]);
        {$endif}
    MenuItem.Tag := ord(mtTag);             // for 'File' entries, this identifies the function to perform.
    MenuItem.Caption := Item;
    MenuItem.OnClick := OC;
    case MenuKind of
        mkFileMenu   : TheMenu.Items.Insert(0, MenuItem);
        mkRecentMenu : TheMenu.Items.Add(MenuItem);
        mkHelpMenu   : AddHelpItem();
    end;
end;

procedure TSearchForm.MenuFileItems(AMenu : TPopupMenu);
var
    i : integer = 0;
begin
    while i < AMenu.Items.Count do begin              // Find the seperator
        if (AMenu.Items[i]).Caption = '-' then break;
        inc(i);
    end;
    dec(i);                                         // cos we want to leave the '-'
    while (i >= 0) do begin                         // Remove File Type entries
        AMenu.Items.Delete(i);                      // Because it removes Help, removes all the individual help items too.
        dec(i);
    end;
    if AMenu.Items.Count = 0 then                   // If menu empty, put in seperator
        AddItemMenu(AMenu, '-', mtSep, nil, mkFileMenu);
    AddItemMenu(AMenu, rsMenuQuit, mtQuit,  @FileMenuClicked, mkFileMenu);
    AddItemMenu(AMenu, rsMenuHelp, mtMainHelp,  nil, mkFileMenu);
    AddItemMenu(AMenu, rsMenuSettings, mtSettings, @FileMenuClicked, mkFileMenu);
    AddItemMenu(AMenu, rsMenuSync, mtSync,  @FileMenuClicked, mkFileMenu);
    AddItemMenu(AMenu, rsMenuAbout, mtAbout, @FileMenuClicked, mkFileMenu);
    AddItemMenu(AMenu, rsMenuSearch, mtSearch,  @FileMenuClicked, mkFileMenu);
    AddItemMenu(AMenu, rsMenuNewNote, mtNewNote, @FileMenuClicked, mkFileMenu);
    // Note items are in reverse order because we Insert at the top.
end;

procedure TSearchForm.MenuRecentItems(AMenu : TPopupMenu);
var
    i : integer = 1;
    //T1, T2, T3, T4 : dword;
    P : Note_Lister.PNote;
    // St : string;
begin
    //T1 := gettickcount64();
    // debugln('In MenuRecentItems ' + AMenu.Name);
    i := AMenu.Items.Count;
    while i > 0 do begin            // Remove any existing entries first
        dec(i);
        if TMenuItem(AMenu.Items[i]).Tag = ord(mtRecent) then
            AMenu.Items.Delete(i);
    end;
    //T2 := gettickcount64();

    // We must iterate over the NoteLister's DateSortList getting the ten most
    // recent notes.

    for i := 0 to 9 do begin
//        P := TheMainNoteLister.GetNote(i, smAllRecentUp);
        P := TheMainNoteLister.GetNote(i, smAllRecentUp);
        if P <> nil then
            AddItemMenu(AMenu, P^.Title, mtRecent,  @RecentMenuClicked, mkRecentMenu)
        else break;
    end;
end;

procedure TSearchForm.MenuHelpItems(AMenu : TPopupMenu);
var
  NoteTitle : string = '';
  Count : integer;

begin
    Count := AMenu.Items.Count;
    while Count > 0 do begin            // Remove any existing entries first
        dec(Count);
        if TMenuItem(AMenu.Items[Count]).Tag = ord(mtMainHelp) then begin
            AMenu.Items[Count].Clear;
            break;
        end;
    end;
    if HelpNotes = nil then exit;
    HelpNotes.StartSearch();
    while HelpNotes.NextNoteTitle(NoteTitle) do        // ToDo : go direct to HelpNotes.NoteList[i], faster ?
        AddItemMenu(AMenu, NoteTitle, mtHelp,  @FileMenuClicked, mkHelpMenu);
end;

procedure TSearchForm.FileMenuClicked(Sender : TObject);
var
    FileName : string;
    //Tick, Tock : qword;
begin
    case TMenuTarget(TMenuItem(Sender).Tag) of
        mtSep, mtRecent : showmessage('Oh, that is bad, should not happen');
        mtNewNote : if (Sett.NoteDirectory = '') then
                            ShowMessage(rsSetupNotesDirFirst)
                    else OpenNote('');
        mtSearch :  if Sett.NoteDirectory = '' then
                            showmessage(rsSetupNotesDirFirst)
                    else begin

                            MoveWindowHere(self);
                            //Tick := Gettickcount64();
                            // EnsureVisible(true);
                            //Tock := Gettickcount64();
                            //debugln('SearchForm - FileMenuClicked ' + dbgs(Tock - Tick) + 'ms  ' + dbgs(GetTickCount64() - Tock) + 'mS');
                    end;
        mtAbout :    MainForm.ShowAbout();
        mtSync :     if Sett.ValidSync then
                        Sett.Synchronise()
                     else begin
                         showmessage(rsSetupSyncFirst);
                         Sett.PageControl1.ActivePage := Sett.TabSync;
                         Sett.EnsureVisible(true);
                         Sett.Show;
                     end;
        mtSettings : begin
                            // {$ifdef LINUX}Sett.Hide;{$endif
                            if Sett.Visible then
                                MoveWindowHere(Sett)
                            else Sett.Show;
                            //Sett.EnsureVisible(true);

                     end;

        mtHelp :      begin
                        if HelpNotes.FileNameForTitle(TMenuItem(Sender).Caption, FileName) then
                            {MainForm.}ShowHelpNote(FileName)
                        else showMessage(rsCannotFindNote + TMenuItem(Sender).Caption);
                    end;
    //    mtQuit :      MainForm.close;
        mtQuit :      MainForm.BitBtnQuitClick(self);                           // ensure we go via path that stops sync threads

        // WARNING: TMenuItem.Destroy with LCLRefCount>0. Hint: Maybe the component is processing an event?
        // WARNING: TPanel.Destroy with LCLRefCount>0. Hint: Maybe the component is processing an event?
        // ToDo : on gtk2, a Quit call from an edit box generates a Warning about the
        // panel and TMenuItem (that the user just used) still having a refcount.
        // maybe use a timer to allow the events to finish and the timer triggers the close ?
    end;
end;

procedure TSearchForm.RecentMenuClicked(Sender: TObject);
begin
 	if TMenuItem(Sender).Caption <> SearchForm.MenuEmpty then
 		SearchForm.OpenNote(TMenuItem(Sender).Caption);
end;

{ Will re-run the current search on the assumption that NoteList data has changed.
  Normally thats a ClearSearch() or NewSearch(). However, if DisplayOnly, the
  Date Index has been updated and thats all needed. So, its then a display
  update.
}
procedure TSearchForm.IndexAndRefresh(DisplayOnly : boolean = false);
var
    NB, STerm : string;
begin
    If DisplayOnly then begin
        ListViewNotes.Clear;
        ListViewNotes.Items.Count := TheMainNoteLister.NoteIndexCount()
    end
    else begin
        if EditSearch.text = rsMenuSearch then
             STerm := ''
        else STerm := EditSearch.text;
        if ListBoxNoteBooks.ItemIndex < 0 then
             NB := ''
        else NB := ListBoxNotebooks.Items[ListBoxNoteBooks.ItemIndex];
        ListViewNotes.Items.Count := 0;
        if STerm.IsEmpty and NB.IsEmpty then
             ListViewNotes.Items.Count := TheMainNoteLister.ClearSearch()
        else ListViewNotes.Items.Count := TheMainNoteLister.NewSearch(STerm, NB,
                MenuItemSearchTitleOnly.Checked);
    end;
end;


// --------------- S E A R C H I N G -------------------------------------------


(* procedure TSearchForm.EditSearchExit(Sender: TObject);                          // ToDo : maybe we don't need this ?
begin

    exit;

{    if (NIndex <> nil) and (not NIndex.Busy) then begin
        NIndex.free;
        NIndex := nil;
        //writeln('TSearchForm.Edit1Exit - Edit1 exit, killing Index');
    end;
	if EditSearch.Text = '' then begin
        EditSearch.Hint:=rsSearchHint;
        EditSearch.Text := rsMenuSearch;
        EditSearch.SelStart := 1;
        EditSearch.SelLength := length(EditSearch.Text);
    end;   }
end;   *)

procedure TSearchForm.EditSearchKeyUp(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    // Must do this here to stop LCL from selecting the text on VK_RETURN
    if Key = VK_RETURN then begin
      Key := 0;
      DoSearchEnterPressed();
    end;
end;

{  Situations where this is called -
   * No NoteBook and blank edit - ClearSearch
   * No NoteBook and 1 char in edit - do nothing
   * No NoteBook and > 1 char in edit, increasing - if ActiveSearch, RefineSearch, else NewSearch
   * No NoteBook and > 1 char in edit, decreasing - NewSearch

   * NoteBook and blank edit - NewSearch with NoteBook
   * NoteBook and 1 char in edit - do nothing
   * NoteBook and > 1 char in edit, increasing - - if ActiveSearch, RefineSearch, else NewSearch with NoteBook
   * NoteBook and > 1 char in edit, decreasing - NewSearch with NoteBook
}


// Only used in Progressive Search mode, SWYT and Form is visible, length of search
// term is > 1 char.
procedure TSearchForm.EditSearchChange(Sender: TObject);
var
    NoteBook : string;
    STL : TStringList;
    Found : integer;
    //Apoint : TPoint;
    //AStr  : string;
    //T1, T2, T3 : qword;

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

begin
    if (EditSearch.Text <> '') and (EditSearch.Text <> rsMenuSearch) then
        ButtonClearSearch.Enabled := True;
    if (not Sett.AutoSearchUpdate) or (not visible) or (length(EditSearch.Text)=1) then exit;
    STL := TStringList.Create;
    try
//        if length(EditSearch.text) = 1 then exit;    // Nothing to see here folks -- Redundant Line ???

        // debugln('TSearchForm.EditSearchChange Notebooks.ItemIndex = ' + inttostr(ListBoxNoteBooks.ItemIndex));

        if ListBoxNoteBooks.ItemIndex > -1 then      // An notebook selected
            NoteBook := ListBoxNotebooks.Items[ListBoxNoteBooks.ItemIndex]
        else NoteBook := '';
        if not ((EditSearch.Text = '') or (EditSearch.Text = rsMenuSearch)) then  // else we pass an empty list.
            if Sett.SearchCaseSensitive then
                STL.AddDelimitedtext(EditSearch.Text, ' ', false)
            else STL.AddDelimitedtext(lowercase(EditSearch.Text), ' ', false);  // build list with terms grouped with "" on one line each
        if (SearchTextLength > length(EditSearch.text))                         // ie backspacing, decreasing
                    or ((SearchTextLength = length(EditSearch.text))            // search term has not changed
                        and (not MenuItemSearchTitleOnly.checked)) then begin   // user has changed SearchOnTitleOnly setting to Content
            Found := TheMainNoteLister.NewSearch(STL, NoteBook, MenuItemSearchTitleOnly.Checked);
            SearchActive := True;
        end else begin                                                          // must have added a char.
            if SearchActive then
                Found := TheMainNoteLister.RefineSearch(STL, MenuItemSearchTitleOnly.Checked)
            else begin
                // debugln('TSearchForm.EditSearchChange  NEWSEARCH CALLED');
                Found := TheMainNoteLister.NewSearch(STL, NoteBook, MenuItemSearchTitleOnly.Checked);
                SearchActive := True;
            end;
        end;
    finally
        STL.Free;
        SearchTextLength := length(EditSearch.Text);

        if length(EditSearch.Text) = 1 then
            SearchTextLength := 1
        else begin
            //T1 := GetTickCount64();

            ListViewNotes.BeginUpdate;
            ListViewNotes.Items.Count := 0;       // Fixes a GTK warning ....
            ListViewNotes.Items.Count := Found;
            ListViewNotes.EndUpdate;
            //T2 := GetTickCount64();
            UpdateStatusBar(0, inttostr(Found) + ' ' + rsNotes);
        end;

        //debugln('TSearchForm.EditSearchChange() LV Refresh =' + inttostr(T2-T1) + 'mS and ');
    end;
end;





procedure TSearchForm.DoSearchEnterPressed();
var
    // T1, T2, TS3, TS4 : qword;
    NoteBook : string;
    STL : TStringList;
//  At startup, 2K notes, RSS=41.8 Meg, do a search and jumps to 50.5meg
begin
    // T1 := gettickcount64();
    if Sett.AutoSearchUpdate then exit;         // we don't do that here.
    STL := TStringList.Create;
    try
        if length(EditSearch.text) = 1 then exit;    // Nothing to see here folks
        if ListBoxNoteBooks.ItemIndex > -1 then
            NoteBook := ListBoxNotebooks.Items[ListBoxNoteBooks.ItemIndex]
        else NoteBook := '';
        if not ((EditSearch.Text = '') or (EditSearch.Text = rsMenuSearch)) then  // else we pass an empty list.
            if Sett.SearchCaseSensitive then
                STL.AddDelimitedtext(EditSearch.Text, ' ', false)
            else STL.AddDelimitedtext(lowercase(EditSearch.Text), ' ', false);
//        if (STL.Count > 0) or (Notebook <> '') then begin
            TheMainNoteLister.LoadContentForPressEnter();                       // Loading 2K notes, 8.6Meg, RSS 43Meg to 51Meg
            ListViewNotes.Clear;
            ListViewNotes.Items.Count := TheMainNoteLister.NewSearch(STL, NoteBook, MenuItemSearchTitleOnly.Checked);  // In SWYT, 52Meg  BUT  v.34c = 42meg
            UpdateStatusBar(0, inttostr(ListViewNotes.Items.Count) + ' ' + rsNotes);
//        end;
    finally
        STL.free;
    end;
    //T2 := gettickcount64();
    //debugln('TSearchForm.DoSearchEnterPressed ' + inttostr(T2-T1) + 'mS');

(*    exit;

    // -------------------------------------------------------------------------
    if (EditSearch.Text = '') then
        ButtonClearFiltersClick(self);          // ToDo : why, won't it clear a selected Notebook ?
    if (EditSearch.Text <> rsMenuSearch) and (EditSearch.Text <> '') then begin
        ButtonClearFilters.Enabled := True;
        //TS1:=gettickcount64();
        NoteLister.SearchNotes(EditSearch.Text);   // observes sett.checkCaseSensitive

        // OK, this is not going to work......
        NoteLister.LoadListNotebooks(ListBoxNotebooks.Items, True);      *)

{  STL.AddDelimitedtext(EditSearch.Text, ' ', false) - easier than NoteLister' buildsearchterm()

this just needs to call NewSearch and poke results into ListVeiwNotes.items.count.

NoteLister.SearchNotes( should (maybe already does) ret number of notes found, save to Found.
It needs to be changed to build a new Index, similar to existing ones.
Somehow teach the ListViewNotes OnData event to get its data from the above new Index.
Call ListViewNotes.Intems.Count := Found;

}


//    end;
end;

(* procedure TSearchForm.EditSearchEnter(Sender: TObject);                         // ToDo : not needed for TextHint model
// ToDo : this should select the word, 'Search' if user clicks in field but does not ??
begin

    exit;

    if EditSearch.Text = rsMenuSearch then begin
        EditSearch.SelectAll;
    end;
end;        *)

procedure TSearchForm.FormDeactivate(Sender: TObject);
begin
    if not Sett.AutoSearchUpdate then
        TheMainNoteLister.UnLoadContent();      // We assume its necessary, probably and fast anyway
end;

procedure TSearchForm.FormHide(Sender: TObject);
begin
    if not Sett.AutoSearchUpdate then
        TheMainNoteLister.UnLoadContent();       // We assume its necessary, probably and fast anyway
end;

// --------------------- F O R M    C O N T R O L S ----------------------------

procedure TSearchForm.FormActivate(Sender: TObject);
//var tick : qword;
begin
//    {$ifdef LCLCOCOA} EditSearch.SetFocus;       // On Mac, we cannot do "jump to EditSearch on typing"
    ListViewNotes.SetFocus;
end;

procedure TSearchForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
    CanClose := False;
    hide();
end;

procedure TSearchForm.IndexNewNote(const FFName: string; CheckTitleClash : boolean);
var
    ExistingFFName : string;
begin
    if CheckTitleClash and                                         // Don't test if import initiated by CLI
        TheMainNoteLister.FileNameForTitle(GetTitleFromFFN(Sett.NoteDirectory+FFName, False), ExistingFFName) then
            if mrYes = QuestionDlg('Note Exists', 'Overwrite Note with same name ?', mtConfirmation, [mrYes, mrNo], 0) then
                DeleteNote(Sett.NoteDirectory+ExistingFFName)      // Thats the existing one, user happy to replace
            else begin                                             // If user responds 'No', then import is cancelled
                DeleteFileUTF8(Sett.NoteDirectory+FFName);         // The newly created one, not yet indexed.
                exit;
            end;

    RefreshNotebooks();
    TheMainNoteLister.IndexThisNote(FFName);     // Add it to NoteLister
    IndexAndRefresh();                           // Reruns the current search with new data in NoteList
    TheMainNoteLister.BuildDateAllIndex();       // Must rebuild it before refreshing menus.
    RefreshMenus(mkRecentMenu);
    MainForm.UpdateNotesFound(TheMainNoteLister.NoteList.Count);
end;

function TSearchForm.IndexNotes(ReRunSearch : boolean=False) : integer;     // Calling IndexAndRefresh(); after IndexNotes() seems to fix it .....
begin                                            // Gets called from other units ....
    if TheMainNoteLister <> Nil then
       freeandnil(TheMainNoteLister);
    TheMainNoteLister := TNoteLister.Create;
    TheMainNoteLister.DebugMode := Application.HasOption('debug-index');
    TheMainNoteLister.WorkingDir:=Sett.NoteDirectory;
    Result := TheMainNoteLister.IndexNotes();
    UpdateStatusBar(0, inttostr(Result) + ' ' + rsNotes);
    RefreshMenus(mkRecentMenu);
    MainForm.UpdateNotesFound(Result);      // Says how many notes found and runs over checklist.
    Sett.StartAutoSyncAndSnap();
    If RerunSearch then begin               // Reruns the current search with new data in NoteList it is
        IndexAndRefresh();                  // often necessary if we are re-running, eg after change to note repo
        RefreshNotebooks();
    end;
end;

procedure TSearchForm.FormCreate(Sender: TObject);
//var Tick : qword;
{$ifdef LCLQT5}{$ifdef LVOWNERDRAW}    var  fd: TFontData;{$endif}   {$endif}
begin
    Caption := 'tomboy-ng Search';
    {$ifdef LCLQT5}
    ListBoxNotebooks.TabStop := false;      // ToDo : ugly fix for Qt5 "list item half selected" issue.
    {$endif}
    SearchTextLength := 0;
    LVSortMode := smRecentUp;           // reflects initial state of ListViewNotes.
      HelpList := Nil;
    //Tick := GetTickCount64();
    // Caption := 'tomboy-ng Search';
    TheMainNoteLister := nil;           // Thats the one in the Note_Lister unit !
    if (SingleNoteFileName <> '') then exit;

    MoveFocusChar := char(0);         // if zero, inactive, if active, it first char in EditSearch;

    { ListView Settings }       // make extra column in Object Inspector
    ListViewNotes.ViewStyle:= vsReport;
    ListViewNotes.AutoSort := False;
    ListViewNotes.OwnerData := True;

    ListViewNotes.Column[0].Caption := rsName;
    ListViewNotes.Column[1].Caption := rsLastChange;
    ListViewNotes.AutoSortIndicator := False;

//    ListViewNotes.AutoSortIndicator := false;
    ListViewNotes.Column[1].SortIndicator := siAscending;
    ListViewNotes.AutoWidthLastColumn:= True;     // ToDo : but Qt5 in OwnerData mode demands columns of extra data, eg Filename
    ListViewNotes.ReadOnly := True;
    CreateMenus();                                // We must build an initial menu BEFORE indexing notes...
    IndexNotes();                                 // Messy but IndexNotes calls Refresh Menu and thats necessary
    ListViewNotes.Items.clear;
    ListViewNotes.Items.Count := TheMainNoteLister.ClearSearch();        // Builds NoteLister's Index files.
    TheMainNoteLister.LoadListNotebooks(ListBoxNotebooks.Items, ButtonClearFilters.Enabled);
    EditSearch.Hint:=rsSearchHint;
    EditSearch.TextHint := rsMenuSearch;
    ButtonClearSearch.Enabled := False;
    EditSearch.SelStart := 1;
    EditSearch.SelLength := length(EditSearch.Text);
    RefreshMenus(mkAllMenu);    // IndexNotes->UseList has already called RefreshMenus(mkRecentMenu) and Qt5 does not like it.
    if Mainform.UseTrayMenu then
        try
            MainForm.TrayIcon.Show;               // Gnome does not like showing it before menu is populated
        except on E: EAccessViolation do begin    // this appears to be a problem on 32bit linux. see https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/40629
            debugln('ERROR : ' + E.Message);
            debugln('TSearchForm.FormCreate - AV in libayatana, try forcing Traditional System Tray.');
            // debugln('Try restarting the app with "--useappind=no" on command line');
            showmessage('ERROR see help note "System Tray on Linux"');
            end;
        end;
    MenuItemCaseSensitive.checked := Sett.SearchCaseSensitive;
    MenuItemSearchTitleOnly.Checked := Sett.SearchTitleOnly;
    LabelSearchTitle.Visible := MenuItemSearchTitleOnly.Checked;
    MenuItemSWYT.checked := Sett.AutoSearchUpdate;
    MenuItemImportNote.Hint := rsHelpImportFile;         // Hint shows on StatusBar
    {$ifdef LVOWNERDRAW}
    ListViewNotes.OwnerDraw:= True;
    {$ifdef LCLQT5}                 // This because when ownerdrawn, we loose spacing between rows in Qt5, ugly workaround.
    fd := GetFontData( SearchForm.Font.Handle );
    ListViewNotes.Font.Height := round((fd.Height * 72 / SearchForm.Font.PixelsPerInch)) + 4;
    {$endif}
    {$endif}
    TheReindexProc := @IndexNewNote;
end;

procedure TSearchForm.FormShow(Sender: TObject);
var
    Butt : TButton;
begin
    Left := Placement + random(Placement*2);
    Top := Placement + random(Placement * 2);
    if Sett.DarkThemeSwitch then begin                    // We are not relying on OS to set dark theme, it was --dark-theme
        Color := Sett.AltColour;                                  // black is 000000, white FFFFFF
        font.Color := Sett.TextColour;                            // Sets children font colour too
        ListBoxNotebooks.Color := Sett.BackGndColour;
        EditSearch.Color := Sett.AltColour;
        splitter1.Color:= clnavy;
        for Butt in [ButtonClearFilters, ButtonSearchOptions, ButtonClearSearch ] do
            Butt.Color := Sett.AltColour;                  // Does work for Qt5, not for GTK2 (but not needed), Windows ?
        BitBtnMenu.Color := Sett.AltColour;
     end;
    ListViewNotes.Color := ListBoxNoteBooks.Color;
    ListBoxNotebooks.Hint := rsNotebookOptionRight;
    if (ListViewNotes.Column[0].SortIndicator = siNone) then begin
        BounceSortIndicator(1);
    end;
    {$ifdef LCLCOCOA}
    BitBtnMenu.Refresh;
    ListBoxNotebooks.Hint := rsNotebookOptionCtrl;
//    EditSearch.SetFocus;    // Cocoa issue, 'cos we cannot make the "on type, jump to EditSearch" work on Mac
    {$endif}
    ListViewNotes.SetFocus;
    if ListViewNotes.Items.Count > 0 then
// #891 of ll/widgetset/wscomctrls.pp  class procedure TWSCustomListView.UpdateMultiSelList(const ALV: TCustomListView; AItem: TListItem; Add: Boolean);
        ListViewNotes.ItemIndex := 0;     // ToDo : this triggers a small memory leak if the Search Screen is shown.
                                          // if I comment out these 2 lines, same thing happens if user selects a line in the listview
                                          // It requires the ListView to be in Multiselect mode, a .create in above function
                                          // does not get called if not multiselect.
end;

procedure TSearchForm.FormKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    if [{$ifdef DARWIN}ssMeta{$else}ssCtrl{$endif}] = Shift then begin
        if key = ord('N') then begin OpenNote(''); Key := 0; exit(); end;
        if key = VK_Q then begin
            // debugln('TSearchForm.FormKeyDown - Quitting because of a Ctrl-Q');
            MainForm.BitBtnQuitClick(self);
        end;
    end;
end;

procedure TSearchForm.FormResize(Sender: TObject);
begin
    ScaleListView();
end;

procedure TSearchForm.FormDestroy(Sender: TObject);
begin
    FreeAndNil(TheMainNoteLister);
    FreeAndNil(HelpNotes);
    FreeAndNil(HelpList);
end;

procedure TSearchForm.MarkNoteReadOnly(const FullFileName: string);
var
    TheForm : TForm;
begin
    if TheMainNoteLister = nil then exit;
    if TheMainNoteLister.IsThisNoteOpen(FullFileName, TheForm) then begin
       // if user opened and then closed, we won't know we cannot access
        try
       	    TEditBoxForm(TheForm).SetReadOnly();
            exit();
        except on  EAccessViolation do
       	    DebugLn('Tried to mark a closed note as readOnly, that is OK');
   	    end;
    end;
end;

procedure TSearchForm.MoveWindowHere(Frm : TForm);
begin
    if Frm = Nil then exit;
    if not Frm.Visible then Frm.Show;
    {$ifdef LINUX}               // On Linux, the target form may be in another workspace
    if not mvxwindow.MvXWinWorkSpace(Frm.Caption) then                  // drops message to console if fails
    begin                        // Normally, this seems highly unlikely ! Maybe Enlightenment DE ?
        Frm.Hide;
        Frm.show;                // This produces a noticable flicker if the form is already visible
    end;
    {$else}
    Frm.EnsureVisible(True);     // All that is necessary in a non-workspace system
    {$endif}
end;

(*                               // old code that used wmctrl, now hit X directly
{$ifdef LINUX}                   // discard this when happy with MvXWindow model.
var
    AProcess: TProcess;
    List : TStringList = nil;    {$endif}
begin
    Result := False;
    {$IFDEF LINUX}      // ToDo : Apparently, Windows now has something like Workspaces, implement .....
    //debugln('In MoveWindowHere with ', WTitle);
    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'wmctrl';
    AProcess.Parameters.Add('-R' + WTitle);
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    try
        AProcess.Execute;
        Result := (AProcess.ExitStatus = 0);        // says at least one packet got back
    except on
        E: EProcess do debugln('Is wmctrl available ? Cannot move ' + WTitle);
    end;
    List := TStringList.Create;
    List.LoadFromStream(AProcess.Output);       // just to clear it away.
    //debugln('Process List ' + List.Text);
    List.Free;
    AProcess.Free;
    {$endif}
end;         *)

procedure TSearchForm.OpenNote(NoteTitle: String; FullFileName: string;
        TemplateIs: AnsiString; BackUp: boolean; InSearch: boolean; STerm : string);
// Everything except the first parameter is optional, take care !
// Might be called with no Title (NewNote) or a Title with or without a Filename
// When called from EditBox, we may pass a Notebook Name, if its a new note that
// notebook will be associated with the new note. Otherwise, ANY request for a new
// note while a notebook is selected in SeachForm will assign the notebook to note.

// If we choose NewNoteFromTemplate TemplateIs is NOT set because we create the note
// and pass its filename into here. It already has its template associated.
var
    EBox : TEditBoxForm;
    NoteFileName : string;
    TheForm : TForm;
begin
    NoteFileName := FullFileName;
    if (NoteTitle <> '') then begin
        if FullFileName = '' then Begin
        	if TheMainNoteLister.FileNameForTitle(NoteTitle, NoteFileName) then
            	NoteFileName := Sett.NoteDirectory + NoteFileName
            else NoteFileName := '';
		end else NoteFileName := FullFileName;
        // if we have a Title and a Filename, it might be open aleady
        if TheMainNoteLister.IsThisNoteOpen(NoteFileName, TheForm) then begin          // Note is already open
            // if user opened and then closed, we won't know we cannot re-show
            try
                MoveWindowHere(TheForm);

//                TheForm.EnsureVisible(true);
                if (NoteFileName <> '') and (NoteTitle <> '') and (InSearch) then
                    if STerm = '' then
                        TEditBoxForm(TheForm).NewFind(EditSearch.Text)
                    else TEditBoxForm(TheForm).NewFind(STerm);
                exit();
			except on  EAccessViolation do
            	DebugLn('Tried to re show a closed note, that is OK');
                // We catch the exception and proceed .... but it should never happen.
			end;
        end;
    end;
    // if to here, we need open a new window. If Filename blank, its a new note
    // If we already have a template (ie notebook) then ignore the SearchForm notebook selection
    if (TemplateIs = '')
        and (NoteFileName = '') and (NoteTitle ='')
        and (ListBoxNotebooks.ItemIndex > -1) then  // a new note with notebook selected.
            TemplateIs := ListBoxNotebooks.Items[ListBoxNotebooks.ItemIndex];
	EBox := TEditBoxForm.Create(Application);
    EBox.SearchedTerm := '';
    // We will do this search after the note has settled dowm
    if (NoteFileName <> '') and (NoteTitle <> '') and (InSearch) then
        if STerm = '' then
            EBox.SearchedTerm := EditSearch.Text
        else
            EBox.SearchedTerm := Sterm;
    EBox.NoteTitle:= NoteTitle;
    EBox.NoteFileName := NoteFileName;
    Ebox.TemplateIs := TemplateIs;
    {$ifdef LCLGTK3}debugln('TSearchForm.OpenNote opening ', NoteTitle);{$endif}
    EBox.Show;
    {$ifdef LCLGTK3}debugln('TSearchForm.OpenNote opened ', NoteTitle);{$endif}
    // if we have a NoteFileName at this stage, we just opened an existing note.

    {    if (NoteFileName <> '') and (NoteTitle <> '') and (InSearch) then
        if STerm = '' then
            EBox.NewFind(EditSearch.Text)
        else
            EBox.NewFind(Sterm);         }

    if (NoteFileName <> '') and BackUp  then
        BackupNote(NoteFileName, 'opn');
    EBox.Dirty := False;
    TheMainNoteLister.ThisNoteIsOpen(NoteFileName, EBox);
    {$ifdef LCLGTK3}debugln('TSearchForm.OpenNote finished open of ', NoteTitle);{$endif}
end;

// ----------------------------- ListView Things -------------------------------

{ ListView Settings - Are set in CreateForm. AutoSort, AutoSortIndicator, AutoWidthLastColumn all true
  Make two columns, name them, leave autwith off, ReadOnly, RowSelect true
  ScrollBars ssAutoVertical, ViewStyle vsReport.
  Note that AutoSortIndicator and SortIndicator are not available in LCL2.0.6 and earlier
  So, don't set them in the form, leave at default settings and set them in a  }
  {if lcl > 2.0.6}
  { structure.  Note, the IDE gets this wrong and greys lines out it should not. }

procedure TSearchForm.ListViewNotesColumnClick(Sender: TObject; Column: TListColumn);
begin
    (* case Column.SortIndicator of
        siNone :       debugln('             Column is siNone - ' + Column.Caption);
        siDescending : debugln('             Column is siDescending - ' + Column.Caption);
        siAscending :  debugln('             Column is siAscending - ' + Column.Caption);
    end;  *)

    if Column.Caption = rsName then
        case Column.SortIndicator of
            siNone, siDescending : begin
                                LVSortMode :=  smAATitleUp;              // Ascending pointing down, AA at top
                                Column.SortIndicator := siAscending;
                           end;
            siAscending :  begin
                                LVSortMode := smAATitleDown;            // Descending pointing up, ZZ at top
                                Column.SortIndicator := siDescending;
                           end;
        end
    else
        case Column.SortIndicator of
            siDescending : begin                                        // The None case is dealt with in create and show
                                LVSortMode := smRecentUp;               // Ascending pointing down, Recent at top
                                Column.SortIndicator := siAscending;
                            end;
            siAscending   : begin
                                LVSortMode := smRecentDown;             // Decending pointing up, Oldest at top
                                Column.SortIndicator := siDescending;
                            end;
        end;
    ListViewNotes.Items.Count := 0;
    ListViewNotes.Items.Count := TheMainNoteLister.NoteIndexCount();    // Does not rebuild Indexes, just display them again
    BounceSortIndicator(column.Index);                                  // To refresh the sortIndicator
end;

procedure TSearchForm.BounceSortIndicator(Col : integer);
begin
    if ListViewNotes.Column[Col].SortIndicator = siDescending then begin
        ListViewNotes.Column[col].SortIndicator := siAscending;
        ListViewNotes.Column[col].SortIndicator := siDescending;
    end else begin
        ListViewNotes.Column[col].SortIndicator := siDescending;
        ListViewNotes.Column[col].SortIndicator := siAscending;
    end;
end;

procedure TSearchForm.ListViewNotesData(Sender: TObject; Item: TListItem);
var
    P : Note_Lister.PNote;
    St : string;
    //T1, T2, T3, T4, T5 : qword;
begin
    //T1 := GetTickCount64();
    // Will get all the entries from note list that are mentioned in the appropriate
    // index (determined by the regional LVSortMode, set by click of column headers)
    P := TheMainNoteLister.GetNote(Item.Index, LVSortMode);
    //debugln('TSearchForm.ListViewNotesData calling item ' + inttostr(Item.Index) + ' ' + P^.LastChange);

    if P <> nil then begin                                // note that QT5 requires both extra columns be created.
        Item.Caption := P^.Title;
        St := copy(P^.LastChange, 1, 16);
        if St[11] = 'T' then
            St[11] := ' ';
        // Item.SubItems.Add(P^.LastChange);
        Item.SubItems.Add(St);
        Item.SubItems.Add(P^.ID);
        //debugln('TSearchForm.ListViewNotesData calling item ' + Item.SubItems.Text);
    end else
        Item.Caption := 'ERROR';
    //T2 := GetTickCount64();
    //debugln('TSearchForm.ListViewNotesData() ' + inttostr(T2-T1) + 'mS ');

    { While in GTK2, LV asks about all rows in data set its still unmeasurable with 2000 lines }
end;

procedure TSearchForm.ListViewNotesDblClick(Sender: TObject);
var
    NoteTitle : ANSIstring;
    FullFileName : string;
    Itm : TListItem;
    //T1, T2, T3, T4, T5 : qword;
begin
    if  ListViewNotes.Selected = nil then exit;         // White space below notes ....
    // it seems we cannot trust ListViewNotes.Selected to point to correct
    // item. Maybe because I have switched to Multiselect Mode ?
    // but it only shows up after clicking Notebook filter and back ?
    // As a temp measure, I'll check here.
    // From time to time, I see the warning below, cannot reproduce it. Might relate
    // to just after a note has been deleted. The for loop below definitly returns
    // the correct note all the time, ListViewNotes.Selected sometimes the wrong one,
    // possibly the first note in the list. Going to leave it like this, 20 Aug, 2024
    //T1 := GetTickCount64();
    for Itm in ListViewNotes.Items do                   // measure time for 2k notes, 20mS max
        if Itm.Selected then begin
            FullFileName := Sett.NoteDirectory + Itm.SubItems[1];
            NoteTitle := Itm.caption;
            break;                                      // Only the first selected in this case
        end;
    //T2 := GetTickCount64();
    //debugln('TSearchForm.ListViewNotesDblClick() ' + inttostr(T2-T1) + 'mS ');
    // NoteTitle := ListViewNotes.Selected.Caption;
    // FullFileName :=  Sett.NoteDirectory + ListViewNotes.Selected.SubItems[1];

// This has continued to happen, the for loop model is reliable, stick with it !  Oct 2024
//    if NoteTitle <> ListViewNotes.Selected.Caption then
//        debugln('WARNING TSearchForm.ListViewNotesDblClick 1=', NoteTitle, ' 2=', ListViewNotes.Selected.Caption);
  	if not FileExistsUTF8(FullFileName) then begin
      	showmessage('Cannot open ' + FullFileName);
      	exit();
  	end;
  	if length(NoteTitle) > 0 then
        OpenNote(NoteTitle, FullFileName, '', True
            , ((EditSearch.Text <> '') and (EditSearch.Text <> rsMenuSearch) and Visible));
end;

procedure TSearchForm.ListViewNotesDrawItem(Sender: TCustomListView;
		AItem: TListItem; ARect: TRect; AState: TOwnerDrawState);
begin
    // Note this only works for TListView if ViewStyle is vsReport
    // (and obviously, we are in ownerdraw mode).

    {$ifdef LVOWNERDRAW}
    if Odd(AItem.Index) then
        ListViewNotes.Canvas.Brush.Color := Sett.AltColour;
    ListViewNotes.Canvas.FillRect(ARect);

    {$ifdef LCLQT5}                                                             // Note we have increased the font height for Qt5 in OnCreate()
    ListViewNotes.Canvas.TextRect(ARect, 2, ARect.Top, AItem.Caption);          // Title column
    ListViewNotes.Canvas.TextRect(ARect, ListViewNotes.Column[0].Width + 2      // LCD Column
            , ARect.Top, AItem.SubItems[0]);
    {$else}
    ListViewNotes.Canvas.TextRect(ARect, 2, ARect.Top+2, AItem.Caption);        // Title column
    ListViewNotes.Canvas.TextRect(ARect, ListViewNotes.Column[0].Width + 2      // LCD Column
            , ARect.Top+2, AItem.SubItems[0]);
    {$endif}
    {$endif}
end;

procedure TSearchForm.EditSearchEnter(Sender: TObject);
begin
    if MoveFocusChar <> char(0) then begin        // listview or listbox put something in there, use it
        EditSearch.Caption := EditSearch.Caption + MoveFocusChar;
        MoveFocusChar := char(0);
        EditSearch.SelStart := length(EditSearch.Caption);    // Windows, Mac needs this, does no harm.
        EditSearch.SelLength := 0;
    end;
end;

procedure TSearchForm.ListViewNotesKeyPress(Sender: TObject; var Key: char);
    // Also services ListBoxNotebooks
begin
    if Key = char(ord(VK_RETURN)) then ListViewNotesDblClick(Sender)
    else begin
        MoveFocusChar := Key;
        EditSearch.SetFocus();
    end;
    Key := char(0);
    { Note, the above is to ensure the char typed that triggers move of focus to
    EditSearch is not lost. GTK2 does not loose it but easier to do it for all.   }
end;


// ---------- R I G H T   C L I C K   M E N U  on  N O T E S  L I S T   --------


procedure TSearchForm.ListViewNotesMouseDown(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
          // rules for what menu items to show
    Val : array of boolean;
    Itm : TListItem;
//    Tick, Tock : qword;

    procedure SetupRightClickMenu;
    begin
        MenuItemOpenSelected.Enabled       := Val[0];
        MenuItemDeleteSelected.Enabled     := Val[1];
        MenuItemNoteBookMembership.Enabled := Val[2];
        MenuItemSelectAll.Enabled          := Val[3];
        MenuItemSelectNone.Enabled         := Val[4];
        {$ifdef LCLGTK2}                              // GTK2 resets a select all when you release the mouse.
        MenuItemSelectAll.Visible := false;
        MenuItemSelectNone.Visible := false;
        {$endif}
    end;

begin
    // ToDo : This is unfair to Windows (with lots of notes). TListVew.GetNextItem is very fast
    // in Windows, slow, iterates over whole dataset, on Linux and MacOS.
    // If https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/41798 is accepted, use it instead.
    if Button = mbRight then begin
        NoteListRightClickSel := [];                // clear it
//        Tick := gettickcount64();

(*        Itm := ListViewNotes.GetNextSelected();        // this requires the unapproved  GetNextSelected(), much faster
        while Itm <> nil do begin
            insert(Itm.SubItems[1], NoteListRightClickSel, 0);
            Itm := ListViewNotes.GetNextSelected(Itm.Index);
        end; *)

        // shortcut here ? If SelCount = 0 skip next, if = 1 then insert Selected.
        case ListViewNotes.SelCount of
            0 : ;
            1 : insert(ListViewNotes.Selected.SubItems[1], NoteListRightClickSel, 0);
        otherwise
              for Itm in ListViewNotes.Items do      // 2000 notes, 20mS - 50mS
                  if Itm.Selected then
                      insert(Itm.SubItems[1], NoteListRightClickSel, 0);     // save the selected ID.note
                      // above is insert into array, despite what Codetools thinks.
        end;


//        Tock := gettickcount64();
//debugln('TSearchForm.ListViewNotesMouseDown checked for selected ' + inttostr(Tock - Tick) + 'mS');
        if ListViewNotes.SelCount = 0 then Val := [false, false, false, true, false]
        else if ListViewNotes.SelCount = 1 then Val := [true, true, true, true, true]
        else Val := [true, true, false, true, true];        // more than one selected
        SetupRightClickMenu;
        PopupMenuListOptions.PopUp;
    end;
end;

procedure TSearchForm.MenuItemDeleteSelectedClick(Sender: TObject);
var
    St, Msg : string;
    OpenNotes : integer = 0;
    AForm : TForm;
begin
    MSG := format(rsQuestionDeleteNotes, [length(NoteListRightClickSel)]);
    for St in NoteListRightClickSel do
        if TheMainNoteLister.IsThisNoteOpen(St, AForm) then inc(OpenNotes);
    if OpenNotes > 0 then
        Msg := Msg + #10 + format(rsQuestionDeleteOpenNotes, [OpenNotes]);
    if MessageDlg('Warning', Msg, mtConfirmation, [mbYes, mbNo],0) = mrYes then begin
        ListViewNotes.BeginUpdate;
        if OpenNotes > 0 then begin                                // This will slow down a big delete IFF one is open
            for St in NoteListRightClickSel do
                if TheMainNoteLister.IsThisNoteOpen(St, AForm) then begin
                    TEditBoxForm(AForm).DeletingThisNote := True;
                    AForm.Close;                                   // Force close any open notes on hit list
                end;
            sleep(20);                                            // give them time to close, maybe a save is in progress
        end;
        for St in NoteListRightClickSel do
            DeleteNote(Sett.NoteDirectory + St);
        ListViewNotes.EndUpdate;
    end;
end;

procedure TSearchForm.MenuItemOpenSelectedClick(Sender: TObject);
var
    St : string;
begin
    // we assume that if the user has seen the menuitem, there must one or more selections
    ListViewNotes.BeginUpdate;
    for St in NoteListRightClickSel do
            OpenNote(TheMainNoteLister.GetTitle(St), Sett.NoteDirectory + St);
    ListViewNotes.EndUpdate;
end;

procedure TSearchForm.MenuItemNoteBookMembershipClick(Sender: TObject);
var
    NotebookPick : TNotebookPick;
    AForm : TForm;
begin
    if length(NoteListRightClickSel) <> 1 then exit;        // permit only exactly one note selected.
    NotebookPick := TNotebookPick.Create(Application);
    NotebookPick.TheMode := nbSetNoteBooks;
    NotebookPick.FullFileName := Sett.NoteDirectory + NoteListRightClickSel[0];
    NotebookPick.Title := TheMainNoteLister.GetTitle(NoteListRightClickSel[0]);
    NotebookPick.ChangeMode := False;
    NotebookPick.Top := Top;
    NotebookPick.Left := Left;
    if mrOK = NotebookPick.ShowModal then begin             // Note may be open (mark dirty) or not (alter file directly)
        if TheMainNoteLister.IsThisNoteOpen(NoteListRightClickSel[0], AForm) then
            TEditBoxForm(AForm).MarkDirty
        else
            ReplaceNoteBookTags(Sett.NoteDirectory + NoteListRightClickSel[0]);
    end;
    NotebookPick.Free;
end;

procedure TSearchForm.MenuItemSelectAllClick(Sender: TObject);
begin
   ListViewNotes.SelectAll;
end;

procedure TSearchForm.MenuItemSelectNoneClick(Sender: TObject);
begin
   ListViewNotes.ClearSelection;
end;


// ----------------------------------------


procedure TSearchForm.ScaleListView();
var
    Col1Width : integer;
begin
    {$ifdef LCLQT5 }
    Col1width := listviewnotes.Canvas.Font.GetTextWidth('2020-06-02 12:30:00000');    // 00 allow for apparent error in scroll width
    {$else}
     Col1width := listviewnotes.Canvas.Font.GetTextWidth('2020-06-02 12:30:000');
    {$endif}
    ListViewNotes.Column[1].Width := Col1width;
    if ListViewNotes.ClientWidth > 100 then
        ListViewNotes.Column[0].Width := ListViewNotes.ClientWidth - Col1width;
(*    debugln('TSearchForm.ScaleListView Clientwidth=' + inttostr(ListViewNotes.ClientWidth) +#10
        + ' Col-0=' + inttostr(ListViewNotes.Column[0].Width)+#10
        + ' Col-1=' + inttostr(ListViewNotes.Column[1].Width)+#10
        + ' Col-2=' + inttostr(ListViewNotes.Column[2].Width)+#10
        + ' Text W=' + inttostr(listviewnotes.Canvas.Font.GetTextWidth('2020-06-02 12:30:000'))+#10
        + ' Text QT5=' + inttostr(listviewnotes.Canvas.Font.GetTextWidth('2020-06-02 12:30:00000')));   *)
end;

procedure TSearchForm.BackupNote(const NoteName, PutIntoName : string);
var
    NewName : string;
    OldName : string;
begin
    NewName := ExtractFileNameOnly(NoteName);
    OldName := Sett.NoteDirectory + NewName + '.note';
    if not FileExistsUTF8(OldName) then exit;                   // Its a new, as yet unsave note
    if length(NewName) <> 36 then exit;                         // We only do notes with UUID names
    // We remove last four char from ID and replace with eg, -opn or -ttl.  This has
    // some loss of entropy, acceptable and allows use of existing Backup recovery.
    NewName := Sett.NoteDirectory + 'Backup' + PathDelim
                + copy(NewName, 1, 32) + '-' + PutIntoName + '.note';
    // We assume here that Sett unit has checked and created a Backup dir is necessary.
    if FileExistsUTF8(NewName) then
        if not DeleteFile(NewName) then debugln('ERROR, failed to delete ' + NewName);

    {debugln('File exists = ' + booltostr(, True));
    debugln('Dir exits = ' + booltostr(DirectoryExists(Sett.NoteDirectory + 'Backup'), True));  }

    if not CopyFile(OldName, NewName) then
        debugln('ERROR, failed to copy : ' + #10 + OldName + #10 + NewName);
    //debugln('SearchForm : BackupNote ' + #10 + OldName + #10 + NewName);
end;

{ ----------------- NOTEBOOK STUFF -------------------- }

    // This ButtonSMenu clears both search term (if any) and restores all notebooks and
    // displays all available notes.
procedure TSearchForm.ButtonClearFiltersClick(Sender: TObject);
begin
    ButtonClearFilters.Enabled := False;
    ListBoxNotebooks.ItemIndex := -1;
    EditSearch.Hint:=rsSearchHint;

    if Sett.AutoSearchUpdate then begin                 // Search While You Type
        ListViewNotes.clear;
        if (EditSearch.Text = '') or (EditSearch.Text = rsMenuSearch) then      // ie, no search term
            ListViewNotes.Items.Count := TheMainNoteLister.ClearSearch()
        else
            ListViewNotes.Items.Count := TheMainNoteLister.NewSearch(EditSearch.Text, '',
                    MenuItemSearchTitleOnly.Checked);
    end else
        DoSearchEnterPressed();
    UpdateStatusBar(0, inttostr(ListViewNotes.Items.Count) + ' ' + rsNotes);
end;

procedure TSearchForm.ListBoxNotebooksClick(Sender: TObject);
var
    STL : TStringList;
begin
    if ListBoxNotebooks.ItemIndex >= 0 then begin
        // debugln('TSearchForm.ListBoxNotebooksClick - ' + inttostr(ListBoxNotebooks.ItemIndex));
        ButtonClearFilters.Enabled := True;
//        ButtonRefreshClick(self);                           // ToDo : wots that doing ?
        STL := TStringList.Create;
        if not ((EditSearch.Text = '') or (EditSearch.Text = rsMenuSearch)) then
            STL.AddDelimitedtext(EditSearch.Text, ' ', false);                  // else we pass an empty list.
        ListViewNotes.Clear;
        ListViewNotes.Items.Count :=
            TheMainNoteLister.NewSearch(STL, ListBoxNotebooks.Items[ListBoxNotebooks.ItemIndex], MenuItemSearchTitleOnly.Checked);
        STL.Free;
        UpdateStatusBar(0, inttostr(ListViewNotes.Items.Count) + ' ' + rsNotes);
    end;
end;

    // Popup a menu when rightclick a notebook
procedure TSearchForm.ListBoxNotebooksMouseUp(Sender: TObject; ButtonSMenu: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
    HaveItem : boolean;
begin
    // debugln('TSearchForm.ListBoxNotebooksMouseDown - Selected in listboxnotebook ' + dbgs(ListBoxNotebooks.ItemIndex));
    if {$ifdef DARWIN} (ssCtrl in Shift) {$ELSE} (ButtonSMenu = mbRight) {$ENDIF}  then begin
        HaveItem := (ListBoxNotebooks.ItemIndex > -1);
        PopupMenuNotebook.Items[0].Enabled := HaveItem;
        PopupMenuNotebook.Items[1].Enabled := HaveItem;
        PopupMenuNotebook.Items[2].Enabled := HaveItem;
        PopupMenuNotebook.Items[3].Enabled := HaveItem;
        PopupMenuNotebook.Items[4].Enabled := HaveItem;
        PopupMenuNotebook.Popup;
    end;
end;


procedure TSearchForm.ButtonSMenuClick(Sender: TObject);
begin
    PopupTBMainMenu.popup;
end;

procedure TSearchForm.MenuEditNotebookTemplateClick(Sender: TObject);
var
    NotebookID : ANSIString;
begin
    NotebookID := TheMainNoteLister.NotebookTemplateID(ListBoxNotebooks.Items[ListBoxNotebooks.ItemIndex]);
    if NotebookID = '' then
    	//showmessage('Error, cannot open template for ' + StringGridNotebooks.Cells[0, StringGridNotebooks.Row])
        showmessage('Error, cannot open template for ' + ListBoxNotebooks.Items[ListBoxNoteBooks.ItemIndex])
    else
        OpenNote(ListBoxNotebooks.Items[ListBoxNoteBooks.ItemIndex] + ' Template', Sett.NoteDirectory + NotebookID);
end;

procedure TSearchForm.MenuCreateNoteBookClick(Sender: TObject);
var
    NotebookPick : TNotebookPick;
    NewNoteBookName : string = '';
    i : integer = 0;
begin
    // debugln('TSearchForm.MenuCreateNoteBook NBCount S = ' + inttostr(TheMainNoteLister.NotebookCount()) );
    NotebookPick := TNotebookPick.Create(Application);
    NotebookPick.TheMode := nbMakeNewNoteBook;
    NotebookPick.FullFileName := '';
    NotebookPick.Title := '';
    NotebookPick.ChangeMode := False;
    NotebookPick.Top := Top;
    NotebookPick.Left := Left;
    if mrOK = NotebookPick.ShowModal then
        NewNotebookName := NotebookPick.NBName;
    NotebookPick.Free;
    ButtonClearFilters.Click;
    RefreshNotebooks();                  // ??
    if  NewNoteBookName <> '' then begin
        while i < ListBoxNoteBooks.Count do begin
            if ListBoxNoteBooks.Items[i] = NewNoteBookName then
                break
            else inc(i);
        end;
        if i < ListBoxNoteBooks.Count then begin
            ListBoxNoteBooks.ItemIndex := i;
            ListBoxNoteBooks.click;
        end else
            debugln('TSearchForm.MenuCreateNoteBookClick - failed to find the new NotebookName');
    end;
    // debugln('TSearchForm.MenuCreateNoteBook NBCount E = ' + inttostr(TheMainNoteLister.NotebookCount()) );
end;



// ---------------------- S E A R C H   O P T I O N S --------------------------

procedure TSearchForm.MenuItemCaseSensitiveClick(Sender: TObject);
begin
    MenuItemCaseSensitive.Checked := not MenuItemCaseSensitive.Checked;
    Sett.SearchCaseSensitive := MenuItemCaseSensitive.Checked;
end;

procedure TSearchForm.MenuItemImportNoteClick(Sender: TObject);
var
    ImportError : string = '';
begin
    OpenDialogImport.InitialDir := Sett.HomeDir;
    OpenDialogImport.Filter := 'Text|*.txt|Markdown|*.md|Note|*.note';
    OpenDialogImport.Title := rsHelpImportFile;
    if OpenDialogImport.Execute then begin
        if TrimFilename(OpenDialogImport.FileName).EndsWith('.note') then
            ImportError := import_note(TrimFilename(OpenDialogImport.FileName))
        else if TrimFilename(OpenDialogImport.FileName).EndsWith('.md') then
            ImportError := Import_Text_MD_File(True, TrimFilename(OpenDialogImport.FileName))
        else if TrimFilename(OpenDialogImport.FileName).EndsWith('.txt') then
            ImportError := Import_Text_MD_File(False, TrimFilename(OpenDialogImport.FileName))
        else ImportError := 'Cannot import TrimFilename(OpenDialogLibrary.FileName)';
    end;
    if ImportError <> '' then
        showmessage('Cannot import TrimFilename(OpenDialogLibrary.FileName)');
end;

procedure TSearchForm.ButtonSearchOptionsClick(Sender: TObject);

begin
   PopupMenuSearchOptions.PopUp;
end;

procedure TSearchForm.MenuItemSWYTClick(Sender: TObject);
begin
    MenuItemSWYT.Checked := not MenuItemSWYT.Checked;
    Sett.AutoSearchUpdate := MenuItemSWYT.Checked;
    TheMainNoteLister.IndexNotes();
end;

procedure TSearchForm.MenuItemSearchTitleOnlyClick(Sender : TObject);
begin
    MenuItemSearchTitleOnly.Checked := not MenuItemSearchTitleOnly.Checked;
    LabelSearchTitle.Visible := MenuItemSearchTitleOnly.Checked;
    if SearchActive then EditSearchChange(Sender);
    // No, if changing from TitleOnly to Content, need to re-run the search using
    // existing search term in edit box and notebook filter (if applicible).
    // Notebook filter is not affected by swap, so if if no active search term,
    // do nothing. But if we have a search term (>2char) and we are switching form
    // Title to content (ie expect more 'hits') RefineSearch() is NOT enough
    Sett.SearchTitleOnly := MenuItemSearchTitleOnly.Checked;
    // Sett.SaveSettings(sender);
end;

procedure TSearchForm.ButtonClearSearchClick(Sender: TObject);
begin
    EditSearch.text := ''; //rsMenuSearch;
    //EditSearch.SetFocus;
    ListViewNotes.SetFocus;
    EditSearch.SelectAll;
    if Sett.AutoSearchUpdate then begin                 // Search While You Type
         ListViewNotes.Clear;
        if ListBoxNoteBooks.ItemIndex < 0 then          // ie, no Notebook selected
            ListViewNotes.Items.Count := TheMainNoteLister.ClearSearch()
        else
            ListViewNotes.Items.Count := TheMainNoteLister.NewSearch('', ListBoxNotebooks.Items[ListBoxNoteBooks.ItemIndex], MenuItemSearchTitleOnly.Checked);
    end else
        DoSearchEnterPressed();
    SearchActive := False;
    UpdateStatusBar(0, inttostr(ListViewNotes.Items.Count) + ' ' + rsNotes);
    ButtonClearSearch.Enabled := false;
end;

procedure TSearchForm.BitBtnMenuClick(Sender: TObject);
begin
    PopupTBMainMenu.popup;
end;


// ----------------

procedure TSearchForm.MenuItemManageNBookClick(Sender: TObject);
var
    NotebookPick : TNotebookPick;
begin
    NotebookPick := TNotebookPick.Create(Application);
    NotebookPick.TheMode := nbSetNotesInNoteBook;
    NotebookPick.FullFileName := '';
    NotebookPick.Title := '';
    NotebookPick.NBName := ListBoxNotebooks.Items[ListBoxNoteBooks.ItemIndex];
    NotebookPick.ChangeMode := False;
    NotebookPick.Top := Top;
    NotebookPick.Left := Left;
    // if mrOK = NotebookPick.ShowModal then MarkDirty();
    NotebookPick.ShowModal;
    NotebookPick.Free;
    ListBoxNotebooksClick(Sender);
    // ButtonClearFilters.Click;       // ToDo : this should select the new Notebook if one made
end;





procedure TSearchForm.MenuRenameNoteBookClick(Sender: TObject);
var
    NotebookPick : TNotebookPick;
begin
    NotebookPick := TNotebookPick.Create(Application);
    NotebookPick.TheMode := nbChangeName;
    try
        NotebookPick.Title := ListBoxNotebooks.Items[ListBoxNoteBooks.ItemIndex];
        NotebookPick.ChangeMode := True;
        NotebookPick.Top := Top;
        NotebookPick.Left := Left;
        if mrOK = NotebookPick.ShowModal then
            ButtonClearFilters.Click;
     finally
        NotebookPick.Free;
    end;
end;

procedure TSearchForm.MenuDeleteNotebookClick(Sender: TObject);
begin
    if IDYES = Application.MessageBox('Delete this Notebook',
    			PChar(ListBoxNotebooks.Items[ListBoxNoteBooks.ItemIndex]),
       			MB_ICONQUESTION + MB_YESNO) then begin
 		DeleteNote(Sett.NoteDirectory + TheMainNoteLister.NotebookTemplateID(ListBoxNotebooks.Items[ListBoxNoteBooks.ItemIndex]));
    end;
end;

procedure TSearchForm.MenuNewNoteFromTemplateClick(Sender: TObject);
begin
    OpenNote(
        MakeNoteFromTemplate(Sett.NoteDirectory
            + TheMainNoteLister.NotebookTemplateID(ListBoxNotebooks.Items[ListBoxNoteBooks.ItemIndex])),
        '', '');
end;


function TSearchForm.MakeNoteFromTemplate(const Template : String) : string;
var
    InFile, OutFile: TextFile;
    InString : String;
    GUID : TGUID;
    RandBit, NewGUID : string;
begin
    Result := '';
    CreateGUID(GUID);
    NewGUID := copy(GUIDToString(GUID), 2, 36);
    RandBit := copy(NewGUID, 1, 4);                 // To add to template name initially
    AssignFile(InFile, Template);
    AssignFile(OutFile, Sett.NoteDirectory + NewGUID + '.note');
    try
        try
            Reset(InFile);
            Rewrite(OutFile);
            while not eof(InFile) do begin
                readln(InFile, InString);
                if (Pos('<tag>system:template</tag>', InString) > 0) then       // skip line
                    continue;
                if (Pos('<title>', InString) > 0) then
                        InString := InString.Replace('Template', RandBit, [rfReplaceAll]);
                // Now, this might be the same line as above. <note-content version="0.3">
                // but it might be 0.1 or 0.2 even. Possible (in gnote) that this line
                // also contains note content, bad if it has the word 'Template' ....
                if (Pos('<note-content version="', InString) > 0) then
                        InString := InString.Replace('Template', RandBit, [rfReplaceAll]);
                writeln(OutFile, InString);
            end;
        finally
            CloseFile(OutFile);
            CloseFile(InFile);
        end;
        TheMainNoteLister.IndexThisNote(NewGUID);       // AAAAAH this does not refresh the indexes, particularly 'All'
        result := GetTitleFromFFN(Sett.NoteDirectory + NewGUID + '.note', false);
        IndexAndRefresh();                                                // Reruns the current search with new data in NoteList
        TheMainNoteLister.BuildDateAllIndex();                            // Must rebuild it before refreshing menus.
        RefreshMenus(mkRecentMenu);
        MainForm.UpdateNotesFound(TheMainNoteLister.NoteList.Count);

    except
        on E: EInOutError do begin
                debugln('File handling error occurred making new note from template. Details: ' + E.Message);
                exit('');
            end;
    end;
end;

end.

