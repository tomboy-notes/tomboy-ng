unit SearchUnit;

{   Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

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
	2017/10/10 - added a refresh button, need to make it auto but need to look at
	timing implication for people with very big note sets first.

	2017/10/10 - added the ability to update the stringlist when a new note is
	created or an older one updated. So, recent notes list under TrayIcon is now
	updated whenever a save is made.

	2017/11/07 - switched over to using NoteLister, need to remove a lot of unused code.

	2017/11/28 - fixed a bug I introduced while restructuring  OpenNote to better
	handle a note being auto saved. This bug killed the Link button in EditNote
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
    2018/11/25  Now uses Sync.DeleteFromLocalManifest(), called when a previously synced not is deleted, TEST !
    2018/12/29  Small improvements in time to save a file.
    2019/02/01  OpenNote() now assignes a new note to the notebook if one is open (ie ButtonNotebookOptions is enabled)
    2019/02/09  Move autosize stringgrid1 (back?) into UseList()
    2019/02/16  Clear button now calls UseList() to ensure autosize happens.
    2019/03/13  Now pass editbox the searchterm (if any) so it can move cursor to first occurance in note
    2019/04/07  Restructured Main and Popup menus. Untested Win/Mac.
    2019/04/13  Don't call note_lister.GetNotes more than absolutly necessary.
    2019/04/15  One Clear Filters button to replace Clea and Show All Notes. Checkboxes Mode instead of menu
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
    2020/02/01  Dont refresh the string grids automatically, turn on the refresh button for user to do it.
    2020/02/19  hilight selected notebook name.
    2020/03/09  Make sure 'x' (put in by a bug) is not a valid sync repo path.
    2020/05/10  Faster search
    2020/05/19  Replaced StringGridNotebook with a ListBox
    2020/06/07  ListBoxNotebooks sorted (but not reverse sortable, that would require TListBox becoming TListView)
    2020/07/09  New help notes location.
    2020/07/17  OpenNote was checking edit1.test = 'search' instead of rsMenuSearch
    2020/11/14  ListViewNotes now has alternating colours, req ugly fix for Qt5 involving increasing font size
    2020/12/10  Move focus to Search Field whenever Search Form is re-shown, issue #211
    2021/01/22  When activating a note from the search form, jump to first match if Search Term is not empty
    2021/01/23  A check box to choose Auto Refresh or not.
    2021/02/11  Some debugs around Ctrl-Q, to be removed and make two listboxes respond to Ctrl-N
    2021/02/14  Direct all key down events via Form's OnKeyDown handler Ctrl-N and Ctrl-Q
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
    {Grids, }ComCtrls, StdCtrls, ExtCtrls, Menus, Buttons, Note_Lister, lazLogger, ResourceStr;

// These are choices for main popup menus.
type TMenuTarget = (mtSep=1, mtNewNote, mtSearch, mtAbout=10, mtSync, mtTomdroid, mtSettings, mtMainHelp, mtHelp, mtQuit, mtRecent);

// These are the possible kinds of main menu items
type TMenuKind = (mkFileMenu, mkRecentMenu, mkHelpMenu, mkAllMenu);


type        { TSearchForm }
    TSearchForm = class(TForm)
			ButtonClearFilters: TButton;
		ButtonRefresh: TButton;
		CheckAutoRefresh: TCheckBox;
        CheckCaseSensitive: TCheckBox;
        Edit1: TEdit;
        ListBoxNotebooks: TListBox;
        ListViewNotes: TListView;
		MenuEditNotebookTemplate: TMenuItem;
		MenuDeleteNotebook: TMenuItem;
        MenuRenameNoteBook: TMenuItem;
		MenuNewNoteFromTemplate: TMenuItem;
		Panel1: TPanel;
        Panel2: TPanel;
		PopupMenuNotebook: TPopupMenu;
        ButtonMenu: TSpeedButton;
		Splitter1: TSplitter;
        StatusBar1: TStatusBar;
        SelectDirectoryDialog1: TSelectDirectoryDialog;
        procedure ButtonMenuClick(Sender: TObject);
                                    { If a search is underway, searches.  Else, if we have
                                      an active notebook filter applied, reapply it. Failing
                                      both of the above, refreshes the Notes and Notebooks
                                      with data in Note_Lister. }
  		procedure ButtonRefreshClick(Sender: TObject);
		procedure ButtonClearFiltersClick(Sender: TObject);
		procedure CheckAutoRefreshChange(Sender: TObject);
        procedure CheckCaseSensitiveChange(Sender: TObject);
        procedure Edit1Enter(Sender: TObject);
		procedure Edit1Exit(Sender: TObject);
        procedure Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
                            // called after OnShow.
        procedure FormActivate(Sender: TObject);
		procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
        procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormResize(Sender: TObject);
		procedure FormShow(Sender: TObject);
        procedure ListBoxNotebooksClick(Sender: TObject);
        procedure ListBoxNotebooksMouseUp(Sender: TObject;
            Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure ListViewNotesDblClick(Sender: TObject);
		procedure ListViewNotesDrawItem(Sender: TCustomListView;
				AItem: TListItem; ARect: TRect; AState: TOwnerDrawState);

        procedure ListViewNotesKeyPress(Sender: TObject; var Key: char);
		procedure MenuDeleteNotebookClick(Sender: TObject);
		procedure MenuEditNotebookTemplateClick(Sender: TObject);
        procedure MenuRenameNoteBookClick(Sender: TObject);
		procedure MenuNewNoteFromTemplateClick(Sender: TObject);
        procedure SpeedButton1Click(Sender: TObject);
        // procedure StringGrid1KeyPress(Sender: TObject; var Key: char);
        // procedure StringGrid1Resize(Sender: TObject);
		//procedure StringGridNotebooksClick(Sender: TObject);
        // procedure StringGrid1DblClick(Sender: TObject);

        { Recieves 2 lists from Sync subsystem, one listing deleted notes ID, the
          other downloded note ID. Adjusts Note_Lister according and marks any
          note that is currently open as read only. Does not move files around. }
        procedure ProcessSyncUpdates(const DeletedList, DownList: TStringList);
        //procedure StringGridNotebooksPrepareCanvas(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
        //procedure StringGridNotebooksResize(Sender: TObject);
    private
        HelpList : TStringList;
        NeedRefresh : boolean;
        HelpNotes : TNoteLister;
        procedure AddItemMenu(TheMenu: TPopupMenu; Item: string;
            mtTag: TMenuTarget; OC: TNotifyEvent; MenuKind: TMenuKind);

        procedure CreateMenus();
        procedure DoSearch();
        procedure FileMenuClicked(Sender: TObject);

        procedure InitialiseHelpFiles();
                                // clears then Inserts file items in all main menus, note also removes help items ....
        procedure MenuFileItems(AMenu: TPopupMenu);
        procedure MenuHelpItems(AMenu: TPopupMenu);
        procedure MenuListBuilder(MList: TList);
        procedure RecentMenuClicked(Sender: TObject);
		procedure Refresh();
        function RemoveFromHelpList(const FullHelpNoteFileName: string): boolean;
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
        NoteLister : TNoteLister;
        NoteDirectory : string;
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
        procedure UpdateStatusBar(SyncSt : string);
        {Just a service provided to NoteBook.pas, refresh the list of notebooks after adding or removing one}
        procedure RefreshNotebooks();
        // Fills in the Main TB popup menus. If AMenu is provided does an mkAllMenu on
        // that Menu, else applies WhichSection to all know Main TB Menus.
        procedure RefreshMenus(WhichSection: TMenuKind; AMenu: TPopupMenu=nil);
        function MoveWindowHere(WTitle: string): boolean;
         	{ Puts the names of recently used notes in the indicated menu, removes esisting ones first. }
        procedure MenuRecentItems(AMenu : TPopupMenu);
       	{ Call this NoteLister no longer thinks of this as a Open note }
        procedure NoteClosing(const ID: AnsiString);
        { Updates the In Memory List with passed data. Either updates existing data or inserts new }
        procedure UpdateList(const Title, LastChange, FullFileName: ANSIString; TheForm : TForm);
        { Reads header in each note in notes directory, updating Search List and
          the recently used list under the TrayIcon. Downside is time it takes
          to index. use UpdateList() if you just have updates. }
        function IndexNotes() : integer;
        { Returns true when passed string is the title of an existing note }
        function IsThisaTitle(const Term: ANSIString): boolean;
                            { Gets called with a title and filename (clicking grid), with just a title
                            (clicked a note link or recent menu item or Link Button) or nothing
                            (new note). If its just Title but Title does not exist, its Link
                            Button. DontBackUp says do not make a backup as we opne because we are in
                            a Roll Back Cycle.}
        procedure OpenNote(NoteTitle: String; FullFileName: string = '';
        				            TemplateIs: AnsiString = ''; BackUp: boolean = True; InSearch : boolean = false) ;
        { Returns True if it put next Note Title into SearchTerm }
        function NextNoteTitle(out SearchTerm : string) : boolean;
        { Initialises search of note titles, prior to calling NextNoteTitle() }
        procedure StartSearch();
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
    TomdroidFile,
    LCLVersion,     // used to enable, or not, sort indicators in lcl2.0.8 or later
    NoteBook;




{ TSearchForm }



{ -------------   FUNCTIONS  THAT  PROVIDE  SERVICES  TO  OTHER   UNITS  ------------ }


procedure TSearchForm.ProcessSyncUpdates(const DeletedList, DownList : TStringList);
// The lists arrive here with just the 36 char ID, the following functions must be OK with that !
var
    Index : integer;
begin
    if NoteLister <> nil then begin
        for Index := 0 to DeletedList.Count -1 do begin
            if NoteLister.IsATemplate(DeletedList.Strings[Index]) then
                NoteLister.DeleteNoteBookwithID(DeletedList.Strings[Index])
            else begin
                MarkNoteReadOnly(DeletedList.Strings[Index]);
                NoteLister.DeleteNote(DeletedList.Strings[Index]);
            end;
        end;
        for Index := 0 to DownList.Count -1 do begin
            MarkNoteReadOnly(DownList.Strings[Index]);
            if NoteLister.IsIDPresent(DownList.Strings[Index]) then begin
                NoteLister.DeleteNote(DownList.Strings[Index]);
                //debugln('We have tried to delete ' + DownList.Strings[Index]);
            end;
            NoteLister.IndexThisNote(DownList.Strings[Index]);
            //debugln('We have tried to reindex ' + DownList.Strings[Index]);
        end;
        RefreshMenus(mkRecentMenu);

        {
        Visible        T            F          T          F
        Checked        T            T          F          F

        Refresh       Yes           n          n          n
        NeedRefresh   n            Yes        Yes        Yes
        EnableButt    n             n         Yes         n
        }

        if Visible and CheckAutoRefresh.checked then
            Refresh()
        else begin
            if Visible then ButtonRefresh.Enabled := True
            else NeedRefresh := True;
        end;


 {       if Visible then begin
            if CheckAutoRefresh.Checked then
                Refresh()
            else ButtonRefresh.Enabled := True
        end else NeedRefresh := True;                      }
    end;
end;


procedure TSearchForm.FlushOpenNotes();
var
    AForm : TForm;
begin
    if assigned(NoteLister) then begin
      AForm := NoteLister.FindFirstOpenNote();
      while AForm <> Nil do begin
          if TEditBoxForm(AForm).dirty then
              TEditBoxForm(AForm).SaveTheNote();
          AForm := SearchForm.NoteLister.FindNextOpenNote();
      end;
    end;
end;


procedure TSearchForm.NoteClosing(const ID : AnsiString);
begin
    if NoteLister <> nil then         // else we are quitting the app !
    	if not NoteLister.ThisNoteIsOpen(ID, nil) then
            // maybe its a help note ?
            RemoveFromHelpList(ID);
end;

procedure TSearchForm.StartSearch(); // Call before using NextNoteTitle() to list Titles.
begin
	NoteLister.StartSearch();
  // TitleIndex := 1;
end;


procedure TSearchForm.DeleteNote(const FullFileName: ANSIString);
var
    NewName, ShortFileName : ANSIString;
    // LocalMan : TTomboyLocalManifest;
    LocalMan : TSync;
begin
    // debugln('DeleteNote ' + FullFileName);
    ShortFileName := ExtractFileNameOnly(FullFileName);
    LocalMan := TSync.Create;
    LocalMan.DebugMode:=false;
    LocalMan.ConfigDir:= Sett.LocalConfig;
    LocalMan.NotesDir:= Sett.NoteDirectory;
    if not LocalMan.DeleteFromLocalManifest(copy(ShortFileName, 1, 36)) then
        showmessage('Error marking note delete in local manifest ' + LocalMan.ErrorString);
    LocalMan.Free;
    if NoteLister.IsATemplate(ShortFileName) then begin
        NoteLister.DeleteNoteBookwithID(ShortFileName);
      	DeleteFileUTF8(FullFileName);
        ButtonClearFiltersClick(self);
    end else begin
		NoteLister.DeleteNote(ShortFileName);
     	NewName := Sett.NoteDirectory + 'Backup' + PathDelim + ShortFileName + '.note';
    	if not DirectoryExists(Sett.NoteDirectory + 'Backup') then
    		if not CreateDirUTF8(Sett.NoteDirectory + 'Backup') then
            	DebugLn('Failed to make Backup dir, ' + Sett.NoteDirectory + 'Backup');
    	if not RenameFileUTF8(FullFileName, NewName)
    		then DebugLn('Failed to move ' + FullFileName + ' to ' + NewName);
    end;
    RefreshMenus(mkRecentMenu);

            if Visible and CheckAutoRefresh.checked then
            Refresh()
        else begin
            if Visible then ButtonRefresh.Enabled := True
            else NeedRefresh := True;
        end;

//    if Visible then ButtonRefresh.Enabled := True
//    else NeedRefresh := True;
end;

function TSearchForm.NextNoteTitle(out SearchTerm: string): boolean;
begin
	Result := NoteLister.NextNoteTitle(SearchTerm);
end;

function TSearchForm.IsThisaTitle(const Term : ANSIString) : boolean;
begin
	Result := NoteLister.IsThisATitle(Term);
end;

procedure TSearchForm.RefreshNotebooks();
begin
    NoteLister.LoadListNotebooks(ListBoxNotebooks.Items, ButtonClearFilters.Enabled);
end;

procedure TSearchForm.UpdateStatusBar(SyncSt: string);
begin
    //StatusBar1.Panels[0].Text:= SyncSt;
    StatusBar1.SimpleText:= SyncSt;
end;

procedure TSearchForm.UpdateList(const Title, LastChange, FullFileName : ANSIString; TheForm : TForm );
{var
    T1, T2, T3, T4 : dword; }
begin
    if NoteLister = Nil then exit;				// we are quitting the app !
  	// Can we find line with passed file name ? If so, apply new data.
    //T1 := gettickcount64();
	if not NoteLister.AlterNote(ExtractFileNameOnly(FullFileName), LastChange, Title) then begin
        // DebugLn('Assuming a new note ', FullFileName, ' [', Title, ']');
        NoteLister.AddNote(ExtractFileNameOnly(FullFileName)+'.note', Title, LastChange);
	end;
    //T2 := gettickcount64();
    NoteLister.ThisNoteIsOpen(FullFileName, TheForm);
    //T3 := gettickcount64();
    RefreshMenus(mkRecentMenu);

            if Visible and CheckAutoRefresh.checked then
            Refresh()
        else begin
            if Visible then ButtonRefresh.Enabled := True
            else NeedRefresh := True;
        end;

//    if Visible then ButtonRefresh.Enabled := True
//    else NeedRefresh := True;
    // UseList();          // 13mS ?
    //T4 := gettickcount64();
    //debugln('SearchUnit.UpdateList ' + inttostr(T2 - T1) + ' ' + inttostr(T3 - T2) + ' ' + inttostr(T4 - T3));
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
          	        TheForm.Show;
                    SearchForm.MoveWindowHere(TheForm.Caption);
                    TheForm.EnsureVisible(true);
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
    ButtonMenu.PopupMenu := PopupTBMainMenu;
    MainForm.MainTBMenu := TPopupMenu.Create(self);
    MainForm.ButtMenu.PopupMenu := MainForm.MainTBMenu;
    // Add any other 'fixed' menu here.
end;

    // Builds a list of all the Menus we have floating around at the moment.
procedure TSearchForm.MenuListBuilder(MList : TList);
var
    AForm : TForm;
begin
    if assigned(NoteLister) then begin
      AForm := NoteLister.FindFirstOpenNote();
      while AForm <> Nil do begin
          MList.Add(TEditBoxForm(AForm).PopupMainTBMenu);
          AForm := SearchForm.NoteLister.FindNextOpenNote();
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

                        (* begin T2 := gettickcount64();          // I saw this taking longer than expected but seems fast enough now ??
                             MList.Count;
                             T3:= gettickcount64();
                             for I := 0 to MList.Count - 1 do begin
                                 T5 := gettickcount64();
                                 MenuRecentItems(TPopupMenu(MList[i]));         // 2mS - 5mS     ??
                                 T4 := gettickcount64();
                                 T6 := gettickcount64();
                                 debugln('Loop timing  ' + dbgs(T6 - T5));
                             end;
                             debugln('SearchUnit.RefreshMenus MList.count = ' + inttostr(T3 - T2) + 'ms ' + dbgs(T4 - T3));
                       end;  *)

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
    {$ifdef LINUX}
    if Sett.CheckShowTomdroid.Checked then
        AddItemMenu(AMenu, 'Tomdroid', mtTomdroid,  @FileMenuClicked, mkFileMenu);
    {$endif}
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
    j : integer;
    //T1, T2, T3, T4 : dword;
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
    i := NoteLister.Count;
    j := i -10;
    if j < 0 then j := 0;
    //T3 := gettickcount64();
    while i > j do begin
        dec(i);
        AddItemMenu(AMenu, NoteLister.GetTitle(i), mtRecent,  @RecentMenuClicked, mkRecentMenu)
    end;
    //T4 := gettickcount64();
    //debugln('TSearchForm.MenuRecentItems ' + inttostr(T2 - T1) + ' ' + inttostr(T3 - T2) + ' ' + inttostr(T4 - T3));
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
    HelpNotes.StartSearch();
    while HelpNotes.NextNoteTitle(NoteTitle) do
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

                            MoveWindowHere(Caption);
                            //Tick := Gettickcount64();
                            EnsureVisible(true);
                            //Tock := Gettickcount64();
                            Show;
                            //debugln('SearchForm - FileMenuClicked ' + dbgs(Tock - Tick) + 'ms  ' + dbgs(GetTickCount64() - Tock) + 'mS');
                    end;
        mtAbout :    MainForm.ShowAbout();
        mtSync :     if(Sett.ValidSync <> '') then Sett.Synchronise()
                     else showmessage(rsSetupSyncFirst);
        mtSettings : begin
                            MoveWindowHere(Sett.Caption);
                            Sett.EnsureVisible(true);
                            Sett.Show;
                     end;
        {$ifdef LINUX}
        mtTomdroid : if FormTomdroidFile.Visible then
                        FormTomdroidFile.BringToFront
                     else FormTomdroidFile.ShowModal;{$endif}
        mtHelp :      begin
                        if HelpNotes.FileNameForTitle(TMenuItem(Sender).Caption, FileName) then
                            {MainForm.}ShowHelpNote(FileName)
                        else showMessage(rsCannotFindNote + TMenuItem(Sender).Caption);
                    end;
        mtQuit :      MainForm.close;
    end;
end;

procedure TSearchForm.RecentMenuClicked(Sender: TObject);
begin
 	if TMenuItem(Sender).Caption <> SearchForm.MenuEmpty then
 		SearchForm.OpenNote(TMenuItem(Sender).Caption);
end;


procedure TSearchForm.ButtonRefreshClick(Sender: TObject);
begin
    Refresh();
end;


(*
procedure TSearchForm.ShowListIndicator(St : string);
// Just a debug method, disable or remove before production
var
    SortInd0, SortInd1 : TSortIndicator;
begin
    SortInd0 := ListViewNotes.Column[0].SortIndicator;
    SortInd1 := ListViewNotes.Column[1].SortIndicator;
    case SortInd0 of
        siNone : writeln(St + '--Col 0 None');
        siAscending : writeln(St + '--Col 0 Ascending');
        siDescending : writeln(St + '--Col 0 Descending');
    end;                             case SortInd1 of
        siNone : writeln(St + '--Col 1 None');
        siAscending : writeln(St + '--Col 1 Ascending');
        siDescending : writeln(St + '--Col 1 Descending');
    end;
end; *)

procedure TSearchForm.Refresh();
// This Method has issues relating to following bug reports -
// https://bugs.freepascal.org/view.php?id=38394  ListView right hand side obscoured by scroll bar
// https://bugs.freepascal.org/view.php?id=38393 ListView Qt5 shows wrong sort indicator
var
    NB : string;
    SortInd0, SortInd1 : TSortIndicator;
begin
    // see https://forum.lazarus.freepascal.org/index.php/topic,48568.msg350984/topicseen.html
    // for info about hiding the sort indicators after changing note data. We don't need to but ....
    // Note setup ListViewNotes in Create() and set its colours in ShowForm()
    //ListViewNotes.Column[1].SortIndicator := siDescending;
    SortInd0 := ListViewNotes.Column[0].SortIndicator;
    SortInd1 := ListViewNotes.Column[1].SortIndicator;
    if (Edit1.Text <> rsMenuSearch) and (Edit1.Text <> '') then
        DoSearch()
    else begin
        if (ListBoxNotebooks.ItemIndex > -1) then begin                        // if a notebook is currently selected.
            NB := ListBoxNotebooks.Items[ListBoxNotebooks.ItemIndex];
            if NB <> '' then begin
                // NoteLister.LoadNotebookGrid(StringGrid1, NB);
                NoteLister.LoadNotebookViewList(ListViewNotes, NB);
            end;
            // ToDo : there is an issue here. If user has a notebook selected when sync happens, and that
            // sync removes a notebook, a 'Refresh' will not make the deleted notebook disappear. It
            // does no go until filters are cleared.
        end else begin
            NoteLister.LoadListView(ListViewNotes, False);
            NoteLister.LoadListNotebooks(ListBoxNotebooks.Items, ButtonClearFilters.Enabled);
            ScaleListView();
        end;
        SelectedNotebook := 0;      // ie off
    end;
    ButtonRefresh.Enabled := false;
    UpdateStatusBar(inttostr(ListViewNotes.Items.Count) + ' ' + rsNotes);
    ListViewNotes.Column[0].SortIndicator := SortInd0;
    ListViewNotes.Column[1].SortIndicator := SortInd1;
    if not ((SortInd0 = siNone)
                and (SortInd1 = siDescending)) then    // default condition,  comes out of NoteLister recent first
        ListViewNotes.Sort;
    //ShowListIndicator('After refresh');
end;

procedure TSearchForm.DoSearch();
var
    TS1, {TS2, TS3,} TS4 : qword;
    Found : integer;
begin
    if (Edit1.Text = '') then
        ButtonClearFiltersClick(self);
    if (Edit1.Text <> rsMenuSearch) and (Edit1.Text <> '') then begin
        ButtonClearFilters.Enabled := True;
        TS1:=gettickcount64();
        Found := NoteLister.SearchNotes(Edit1.Text);   // observes sett.checkCaseSensitive
        // TS2:=gettickcount64();
        NoteLister.LoadListView(ListViewNotes, True);
        // ToDo : do we need to call ScaleListView here ?
        // TS3:=gettickcount64();
        NoteLister.LoadListNotebooks(ListBoxNotebooks.Items, True);
        TS4:=gettickcount64();
        StatusBar1.SimpleText := 'Search=' + inttostr(TS4 - TS1) + 'mS and we found ' + dbgs(Found) + ' notes';
        {StatusBar1.SimpleText := 'Search=' + inttostr(TS2 - TS1) + 'mS LoadSt=' + inttostr(TS3-TS2) + 'mS LoadNB='
            + inttostr(TS4 - TS3) + 'mS  and we found ' + dbgs(Found) + ' notes';}
    end;
end;

procedure TSearchForm.Edit1Exit(Sender: TObject);
begin
	if Edit1.Text = '' then begin
        Edit1.Hint:=rsSearchHint;
        Edit1.Text := rsMenuSearch;
        Edit1.SelStart := 1;
        Edit1.SelLength := length(Edit1.Text);
    end;
end;

procedure TSearchForm.Edit1KeyUp(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    // Must do this here to stop LCL from selecting the text on VK_RETURN
    if Key = VK_RETURN then begin
      Key := 0;
      DoSearch();
    end;
end;

procedure TSearchForm.FormActivate(Sender: TObject);
//var tick : qword;
begin
    if NeedRefresh then begin
        //Tick := gettickcount64();
        NeedRefresh := False;
        ButtonRefreshClick(self);
        //debugln('SearchForm - FormActivate (first run) ' + dbgs(GetTickCount64() - Tick) + 'mS');
    end;
    //debugln('Search Unit Form Activate');
    Edit1.SetFocus;
end;

procedure TSearchForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
    CanClose := False;
    hide();
end;

function TSearchForm.IndexNotes() : integer;
// var
	// TS1, TS2 : TTimeStamp;
begin
    // TS1 := DateTimeToTimeStamp(Now);
    // if not Sett.HaveConfig then exit(0);      // we assume we always have some sort of config now
    if NoteLister <> Nil then
       freeandnil(NoteLister);
    NoteLister := TNoteLister.Create;
    NoteLister.DebugMode := Application.HasOption('debug-index');
    NoteLister.WorkingDir:=Sett.NoteDirectory;
    Result := NoteLister.IndexNotes();
    UpdateStatusBar(inttostr(Result) + ' ' + rsNotes);
    if CheckAutoRefresh.Checked then
       Refresh()
    else NeedRefresh := True;                                // eg refresh ListViewNotes on next OnActivate
    RefreshMenus(mkRecentMenu);
    // TS2 := DateTimeToTimeStamp(Now);
	// debugln('TSearchForm.IndexNotes - Indexing took (mS) ' + inttostr(TS2.Time - TS1.Time));          // Dell, 2K notes, 134mS
    MainForm.UpdateNotesFound(Result);      // Says how many notes found and runs over checklist.
    Sett.StartAutoSyncAndSnap();
end;

procedure TSearchForm.FormCreate(Sender: TObject);
//var Tick : qword;
{$ifdef LCLQT5}{$ifdef LVOWNERDRAW}    var  fd: TFontData;{$endif}   {$endif}
begin
      HelpList := Nil;
    //Tick := GetTickCount64();
    Caption := 'tomboy-ng Search';
    NoteLister := nil;
    if (SingleNoteFileName <> '') then exit;
    ListViewNotes.Column[0].Caption := rsName;
    ListViewNotes.Column[1].Caption := rsLastChange;
    Edit1.Hint:=rsSearchHint;
    Edit1.Text := rsMenuSearch;
    Edit1.SelStart := 1;
    Edit1.SelLength := length(Edit1.Text);
    CreateMenus();
    IndexNotes();               // This could be a slow process, maybe a new thread ?
    RefreshMenus(mkAllMenu);    // IndexNotes->UseList has already called RefreshMenus(mkRecentMenu) and Qt5 does not like it.

    {$if (lcl_fullversion>2000600)}   //  trunk=2010000 : 2.1.0 or 2.01.00.00   2.0.6 : 2000600, note IDE greys incorrectly.
    ListViewNotes.AutoSortIndicator := True;
    // ListViewNotes.Column[1].SortIndicator := siAscending;
    ListViewNotes.Column[1].SortIndicator := siDescending;
    //debugln('Using sort indicators');
    {$endif}

    { ListView Settings }       // make extra column in Object Inspector
    ListViewNotes.AutoSortIndicator := True;
    ListViewNotes.Column[1].SortIndicator := siDescending;
    ListViewNotes.AutoSort:=True;
    ListViewNotes.SortDirection:= sdDescending;     // Most recent, ie bigger date numbers, on top
    ListViewNotes.AutoWidthLastColumn:= True;
    ListViewNotes.ViewStyle:= vsReport;
    ListViewNotes.ReadOnly := True;
    {$ifdef LVOWNERDRAW}
    ListViewNotes.OwnerDraw:= True;
    {$ifdef LCLQT5}                 // This because when ownerdrawn, we loose spacing between rows in Qt5, ugly workaround.
    fd := GetFontData( SearchForm.Font.Handle );
    ListViewNotes.Font.Height := round((fd.Height * 72 / SearchForm.Font.PixelsPerInch)) + 4;
    {$endif}
    {$endif}
end;

procedure TSearchForm.FormShow(Sender: TObject);
begin
    // if MainForm.closeASAP or (MainForm.SingleNoteFileName <> '') then exit;
    Left := Placement + random(Placement*2);
    Top := Placement + random(Placement * 2);
    CheckCaseSensitive.checked := Sett.SearchCaseSensitive;
//    {$ifdef windows}  // linux apps know how to do this themselves
    if Sett.DarkTheme then begin                                        // Note - Windows won't let us change button colour anymore.
        ListBoxNotebooks.Color := Sett.BackGndColour;
        ListBoxNoteBooks.Font.Color := Sett.TextColour;
        Edit1.Color := Sett.BackGndColour;
        Edit1.Font.Color := Sett.TextColour;
//         color := Sett.HiColour;
         Color := Sett.BackGndColour;
         font.color := Sett.TextColour;
         ListViewNotes.Color :=       clnavy;
//         ListViewNotes.Font.Color :=  Sett.HiColour;
         ListViewNotes.Font.Color :=  Sett.BackGndColour;
         splitter1.Color:= clnavy;
    end;
    CheckAutoRefresh.Checked := Sett.AutoRefresh;
    ListViewNotes.Color := ListBoxNoteBooks.Color;
    ListViewNotes.Font.Color := ListBoxNotebooks.Font.Color;
//    {$endif}
    ListBoxNotebooks.Hint := rsNotebookOptionRight;
    {$ifdef DARWIN}
    ButtonMenu.Refresh;
    ListBoxNotebooks.Hint := rsNotebookOptionCtrl;
    {$endif}      // Cocoa issue
    Edit1.SetFocus;
end;

procedure TSearchForm.FormKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
(*    if {$ifdef DARWIN}ssMeta{$else}ssCtrl{$endif} in Shift then
        if key = VK_Q then
        debugln('TSearchForm.FormKeyDown - Detected Ctrl IN Shift - Q, ignoring');      *)
    if [{$ifdef DARWIN}ssMeta{$else}ssCtrl{$endif}] = Shift then begin
        if key = ord('N') then begin OpenNote(''); Key := 0; exit(); end;
        if key = VK_Q then begin
            // debugln('TSearchForm.FormKeyDown - Quitting because of a Ctrl-Q');
            MainForm.Close();
        end;
    end;
end;

procedure TSearchForm.FormResize(Sender: TObject);
begin
    ScaleListView();
end;

procedure TSearchForm.FormDestroy(Sender: TObject);
begin
    NoteLister.Free;
    NoteLister := Nil;
    HelpNotes.Free;
    HelpNotes := Nil;
    freeandnil(HelpList);
end;

procedure TSearchForm.CheckCaseSensitiveChange(Sender: TObject);
begin
    Sett.SearchCaseSensitive:= CheckCaseSensitive.Checked;
    // Sett.CheckCaseSensitive.Checked := CheckCaseSensitive.Checked;
end;

procedure TSearchForm.Edit1Enter(Sender: TObject);
// ToDo : this should select the word, 'Search' if user clicks in field but does not ??
begin
    if Edit1.Text = rsMenuSearch then begin
        //Edit1.SelStart:=0;
        //Edit1.SelLength:= length(rsMenuSearch);
        Edit1.SelectAll;
    end;
end;

procedure TSearchForm.MarkNoteReadOnly(const FullFileName: string);
var
    TheForm : TForm;
begin
    if NoteLister = nil then exit;
    if NoteLister.IsThisNoteOpen(FullFileName, TheForm) then begin
       // if user opened and then closed, we won't know we cannot access
        try
       	    TEditBoxForm(TheForm).SetReadOnly();
            exit();
        except on  EAccessViolation do
       	    DebugLn('Tried to mark a closed note as readOnly, that is OK');
   	    end;
    end;
end;

function TSearchForm.MoveWindowHere(WTitle: string): boolean;
{$ifdef LINUX}
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
end;




procedure TSearchForm.OpenNote(NoteTitle: String; FullFileName: string;
                            TemplateIs: AnsiString; BackUp: boolean; InSearch: boolean);
// Everything except the first parameter is optional, take care !
// Might be called with no Title (NewNote) or a Title with or without a Filename
var
    EBox : TEditBoxForm;
    NoteFileName : string;
    TheForm : TForm;
begin
    NoteFileName := FullFileName;
    if (NoteTitle <> '') then begin
        if FullFileName = '' then Begin
        	if NoteLister.FileNameForTitle(NoteTitle, NoteFileName) then
            	NoteFileName := Sett.NoteDirectory + NoteFileName
            else NoteFileName := '';
		end else NoteFileName := FullFileName;
        // if we have a Title and a Filename, it might be open aleady
        if NoteLister.IsThisNoteOpen(NoteFileName, TheForm) then begin          // Note is already open
            // if user opened and then closed, we won't know we cannot re-show
            try
            	TheForm.Show;
                MoveWindowHere(TheForm.Caption);
                TheForm.EnsureVisible(true);
                if (NoteFileName <> '') and (NoteTitle <> '') and (InSearch) then
                     TEditBoxForm(TheForm).NewFind(Edit1.Text);
                exit();
			except on  EAccessViolation do
            	DebugLn('Tried to re show a closed note, that is OK');
			end;
            // We catch the exception and proceed .... but it should never happen.
        end;
    end;
    // if to here, we need open a new window. If Filename blank, its a new note
    if (NoteFileName = '') and (NoteTitle ='') and (ListBoxNotebooks.ItemIndex > -1) then  // a new note with notebook selected.
       //TemplateIs := StringGridNotebooks.Cells[0, StringGridNotebooks.Row];
        TemplateIs := ListBoxNotebooks.Items[ListBoxNotebooks.ItemIndex];
	EBox := TEditBoxForm.Create(Application);
{    if (NoteFileName <> '') and (NoteTitle <> '') and (Edit1.Text <> '') and (Edit1.Text <> rsMenuSearch) then
        // Looks like we have a search in progress, lets take user there when note opens.
        EBox.SearchedTerm := Edit1.Text
    else  }
        EBox.SearchedTerm := '';
    EBox.NoteTitle:= NoteTitle;
    EBox.NoteFileName := NoteFileName;
    Ebox.TemplateIs := TemplateIs;
    EBox.Show;
    // if we have a NoteFileName at this stage, we just opened an existing note.

    if (NoteFileName <> '') and (NoteTitle <> '') and (InSearch) then
        EBox.NewFind(Edit1.Text);

    if (NoteFileName <> '') and BackUp  then
        BackupNote(NoteFileName, 'opn');
    EBox.Dirty := False;
    NoteLister.ThisNoteIsOpen(NoteFileName, EBox);
end;

// ----------------------------- ListView Things -------------------------------

{ ListView Settings - Are set in CreateForm. AutoSort, AutoSortIndicator, AutoWidthLastColumn all true
  Make two columns, name them, leave autwith off, ReadOnly, RowSelect true
  ScrollBars ssAutoVertical, ViewStyle vsReport.
  Note that AutoSortIndicator and SortIndicator are not available in LCL2.0.6 and earlier
  So, don't set them in the form, leave at default settings and set them in a  }
  {if lcl > 2.0.6}
  { structure.  Note, the IDE gets this wrong and greys lines out it should not. }

procedure TSearchForm.ListViewNotesDblClick(Sender: TObject);
var
    NoteTitle : ANSIstring;
    FullFileName : string;
begin
    if  ListViewNotes.Selected = nil then exit;         // White space below notes ....
    NoteTitle := ListViewNotes.Selected.Caption;
    FullFileName :=  Sett.NoteDirectory + ListViewNotes.Selected.SubItems[1];
  	if not FileExistsUTF8(FullFileName) then begin
      	showmessage('Cannot open ' + FullFileName);
      	exit();
  	end;
  	if length(NoteTitle) > 0 then OpenNote(NoteTitle, FullFileName, '', True,
                            ((Edit1.Text <> '') and (Edit1.Text <> rsMenuSearch) and Visible));
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

procedure TSearchForm.ListViewNotesKeyPress(Sender: TObject; var Key: char);
begin
    if Key = char(ord(VK_RETURN)) then ListViewNotesDblClick(Sender);
end;

procedure TSearchForm.ScaleListView();
var
    Col1Width : integer;
begin
    {$ifdef LCLQT5}
    Col1width := listviewnotes.Canvas.Font.GetTextWidth('2020-06-02 12:30:00000');    // 00 allow for apparent error in scroll with
    {$else}
     Col1width := listviewnotes.Canvas.Font.GetTextWidth('2020-06-02 12:30:000');
    {$endif}
    ListViewNotes.Column[1].Width := Col1width;
    if ListViewNotes.ClientWidth > 100 then
        ListViewNotes.Column[0].Width := ListViewNotes.ClientWidth - Col1width;
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

    // This button clears both search term (if any) and restores all notebooks and
    // displays all available notes.
procedure TSearchForm.ButtonClearFiltersClick(Sender: TObject);
begin
    ButtonClearFilters.Enabled := False;
    ListBoxNotebooks.ItemIndex := -1;
    Edit1.Hint:=rsSearchHint;
    Edit1.Text := rsMenuSearch;
    ButtonRefreshClick(self);
    Edit1.SetFocus;
    Edit1.SelStart := 0;
    Edit1.SelLength := length(Edit1.Text);
    UpdateStatusBar('');
end;

procedure TSearchForm.CheckAutoRefreshChange(Sender: TObject);
begin
    Sett.AutoRefresh := CheckAutoRefresh.Checked;
end;

procedure TSearchForm.ListBoxNotebooksClick(Sender: TObject);
begin
    ButtonClearFilters.Enabled := True;
    ButtonRefreshClick(self);
    SelectedNoteBook := ListBoxNotebooks.ItemIndex;

    // Events here if there is a term search is progress are ignored because ButtonRefreshClick acts on
    // search terms first.

    //ListBoxNotebooks.Hint := 'Options for ?';
    //StringGridNotebooks.Hint := 'Options for ' + StringGridNotebooks.Cells[0, StringGridNotebooks.Row];
end;


    // Popup a menu when rightclick a notebook
procedure TSearchForm.ListBoxNotebooksMouseUp(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    // debugln('TSearchForm.ListBoxNotebooksMouseDown - Selected in listboxnotebook ' + dbgs(ListBoxNotebooks.ItemIndex));
    if {$ifdef DARWIN} (ssCtrl in Shift) {$ELSE} (Button = mbRight) {$ENDIF}
                and (ListBoxNotebooks.ItemIndex > -1) then
        PopupMenuNotebook.Popup;
end;

procedure TSearchForm.ButtonMenuClick(Sender: TObject);
begin
    //ShowListIndicator('From Menu');
    PopupTBMainMenu.popup;
end;

procedure TSearchForm.MenuEditNotebookTemplateClick(Sender: TObject);
var
    NotebookID : ANSIString;
begin
    NotebookID := NoteLister.NotebookTemplateID(ListBoxNotebooks.Items[ListBoxNotebooks.ItemIndex]);
    if NotebookID = '' then
    	//showmessage('Error, cannot open template for ' + StringGridNotebooks.Cells[0, StringGridNotebooks.Row])
        showmessage('Error, cannot open template for ' + ListBoxNotebooks.Items[ListBoxNoteBooks.ItemIndex])
    else
        OpenNote(ListBoxNotebooks.Items[ListBoxNoteBooks.ItemIndex] + ' Template', Sett.NoteDirectory + NotebookID);
end;

procedure TSearchForm.MenuRenameNoteBookClick(Sender: TObject);
var
    NotebookPick : TNotebookPick;
begin
    NotebookPick := TNotebookPick.Create(Application);
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
       			MB_ICONQUESTION + MB_YESNO) then
		DeleteNote(Sett.NoteDirectory + NoteLister.NotebookTemplateID(ListBoxNotebooks.Items[ListBoxNoteBooks.ItemIndex]));
end;

procedure TSearchForm.MenuNewNoteFromTemplateClick(Sender: TObject);
begin
    OpenNote('', '', '');
end;

procedure TSearchForm.SpeedButton1Click(Sender: TObject);
begin
    // note - image is 24x24 tpopupmenu.png from lazarus source
    MainForm.PopupMenuSearch.PopUp;
end;


end.

