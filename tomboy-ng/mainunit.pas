unit MainUnit;

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

{	This is NO LONGER the Main unit (ie the main form) for tomboy-ng. It always exists while
	RTomboy is running, when you cannot see it, its because its hidden. This
	form will put its icon in the System Tray and its resposible for acting
	on any of the menu choices from that tray icon.
    The form, and therefore the application, does not close if the user clicks
	the (typically top right) close box, just hides. It does not close until
	the user clicks 'close' from the System Tray Menu.

	It also displays the Search box showing all notes.
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
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
    Grids, ComCtrls, StdCtrls, ExtCtrls, Menus, Note_Lister, lazLogger;

type

    { TRTSearch }

    TRTSearch = class(TForm)
			ButtonNotebookOptions: TButton;
			ButtonShowAllNotes: TButton;
		ButtonRefresh: TButton;
        ButtonClearSearch: TButton;
        Edit1: TEdit;
		MenuEditNotebookTemplate: TMenuItem;
		MenuDeleteNotebook: TMenuItem;
		MenuNewNoteFromTemplate: TMenuItem;
		MenuSynchronise: TMenuItem;
        MenuItemSettings: TMenuItem;
		Panel1: TPanel;
		PopupMenuNotebook: TPopupMenu;
		Splitter1: TSplitter;
		StringGridNotebooks: TStringGrid;
        TrayMenuAbout: TMenuItem;
        MenuItem3: TMenuItem;
        TrayMenuNew: TMenuItem;
        MenuItem15: TMenuItem;
        MenuQuit: TMenuItem;
        TrayMenSearch: TMenuItem;
        MenuItem4: TMenuItem;
        TrayMenuRecent1: TMenuItem;
        TrayMenuRecent2: TMenuItem;
        TrayMenuRecent3: TMenuItem;
        TrayMenuRecent4: TMenuItem;
        TrayMenuRecent5: TMenuItem;
        TrayMenuRecent6: TMenuItem;
        TrayMenuRecent7: TMenuItem;
        TrayMenuRecent8: TMenuItem;
        TrayMenuRecent9: TMenuItem;
        TrayMenuRecent10: TMenuItem;
        PopupMenuTray: TPopupMenu;
        SelectDirectoryDialog1: TSelectDirectoryDialog;
        StringGrid1: TStringGrid;
        TrayIcon: TTrayIcon;
//        procedure ButtonOpenClick(Sender: TObject);
		procedure ButtonClearSearchClick(Sender: TObject);
		procedure ButtonNotebookOptionsClick(Sender: TObject);
  		procedure ButtonRefreshClick(Sender: TObject);
		procedure ButtonShowAllNotesClick(Sender: TObject);
		procedure Edit1EditingDone(Sender: TObject);
		procedure Edit1Enter(Sender: TObject);
		procedure Edit1Exit(Sender: TObject);
		procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
        // procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
        procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure MenuDeleteNotebookClick(Sender: TObject);
		procedure MenuEditNotebookTemplateClick(Sender: TObject);
        procedure MenuItemSettingsClick(Sender: TObject);
		procedure MenuNewNoteFromTemplateClick(Sender: TObject);
        { Takes the app down when user clicks TrayIcon menu quit }
        procedure MenuQuitClick(Sender: TObject);
		procedure MenuSynchroniseClick(Sender: TObject);
		procedure StringGridNotebooksClick(Sender: TObject);
        procedure TrayIconClick(Sender: TObject);
        procedure TrayMenSearchClick(Sender: TObject);
        procedure StringGrid1DblClick(Sender: TObject);
        procedure TrayMenuAboutClick(Sender: TObject);
        procedure TrayMenuNewClick(Sender: TObject);
        { Responds when any of the recent items is clicked in TrayIcon menu }
        procedure TrayMenuRecent1Click(Sender: TObject);
    private

        { ----- A set of two horrible methods only needed on Mac to minimise memory leaks ---- }

        	{ returns true if there is a note in top10 not present in Menu }
        function MenuItemMissing: boolean;
        	{ A butchered version of RecentMenu trying to avoid some of the Mac Memory leak issues }
        procedure RecentMenuMac();
        	{ Puts the names of recently used notes in the TrayMenu }
        procedure RecentMenu();
		function TrimDateTime(const LongDate: ANSIString): ANSIString;
        		{ Copies note data from internal list to StringGrid, sorts it and updates the
                  TrayIconMenu recently used list.  Does not 'refresh list from disk'.  }
		procedure UseList;
    public
        NoteLister : TNoteLister;
        NoteDirectory : string;
        	{ Call this NoteLister no longer thinks of this as a Open note }
        procedure NoteClosing(const ID: AnsiString);
        { Updates the List with passed data. Either updates existing data or inserts new }
        procedure UpdateList(const Title, LastChange, FullFileName: ANSIString; TheForm : TForm);
        { Reads header in each note in notes directory, updating Search List and
          the recently used list under the TrayIcon. Downside is time it takes
          to index. use UpdateList() if you just have updates. }
        procedure IndexNotes;
        { Returns true when passed string is the title of an existing note }
        function IsThisaTitle(const Term: ANSIString): boolean;
        { Gets called with a title and filename (clicking grid), with just a title
          (clicked a note link or recent menu item or Link Button) or nothing
          (new note). If its just Title but Title does not exist, its Link
          Button. }
        procedure OpenNote(NoteTitle : String = ''; FullFileName : string = ''; TemplateIs : AnsiString = '');
        { Returns True if it put next Note Title into SearchTerm }
        function NextNoteTitle(out SearchTerm : string) : boolean;
        { Initialises search of note titles, prior to calling NextNoteTitle() }
        procedure StartSearch();
        { Deletes the actual file then removes the indicated note from the internal
        data about notes, refreshes Grid }
        procedure DeleteNote(const FullFileName : ANSIString);
    end;

var
    RTSearch: TRTSearch;

implementation

{$R *.lfm}

uses EditBox,
    settings,		// Manages settings.  This Main Form, close it to kill app.
    SyncGUI,
    TB_Sync,		// So we can make changes to local manifest when a note is deleted.
    LCLType,		// For the MessageBox
    LazFileUtils;   // LazFileUtils needed for TrimFileName(), cross platform stuff


{ TRTSearch }

const
	MenuEmpty = '(empty)';

{ -------------   FUNCTIONS  THAT  PROVIDE  SERVICES  TO  OTHER   UNITS  ------------ }

procedure TRTSearch.NoteClosing(const ID : AnsiString);
begin
    if NoteLister <> nil then         // else we are quitting the app !
    	NoteLister.ThisNoteIsOpen(ID, nil);
end;

procedure TRTSearch.StartSearch(); // Call before using NextNoteTitle() to list Titles.
begin
	NoteLister.StartSearch();
  // TitleIndex := 1;
end;

procedure TRTSearch.DeleteNote(const FullFileName: ANSIString);
var
    NewName, ShortFileName : ANSIString;
    LocalMan : TTomboyLocalManifest;
begin
    ShortFileName := ExtractFileNameOnly(FullFileName);
    if NoteLister.IsATemplate(ShortFileName) then begin
        NoteLister.DeleteNoteBookwithID(ShortFileName);
      	DeleteFileUTF8(FullFileName);
        ButtonShowAllNotesClick(self);
    end else begin
		NoteLister.DeleteNote(ShortFileName);
     	NewName := Sett.NoteDirectory + 'Backup' + PathDelim + ShortFileName + '.note';
    	if not DirectoryExists(Sett.NoteDirectory + 'Backup') then
    		if not CreateDirUTF8(Sett.NoteDirectory + 'Backup') then
            	DebugLn('Failed to make Backup dir, ' + Sett.NoteDirectory + 'Backup');
    	if not RenameFileUTF8(FullFileName, NewName)
    		then DebugLn('Failed to move ' + FullFileName + ' to ' + NewName);
    end;
 	LocalMan := TTomboyLocalManifest.Create;
 	LocalMan.LocalManifestDir:= Sett.LocalConfig;
   	LocalMan.NotesDir:=Sett.NoteDirectory;
	if LocalMan.GetLocalServerID() then
		LocalMan.IDToDelete:= ShortFileName
   	else
    	DebugLn('ERROR, failed to move deleted ID in local manifest [' + ExtractFileNameOnly(FullFileName)+ ']');
    LocalMan.Free;
    UseList();
end;

function TRTSearch.NextNoteTitle(out SearchTerm: string): boolean;
begin
	Result := NoteLister.NextNoteTitle(SearchTerm);
end;

function TRTSearch.IsThisaTitle(const Term : ANSIString) : boolean;
begin
	Result := NoteLister.IsThisATitle(Term);
end;


function TRTSearch.TrimDateTime(const LongDate : ANSIString ) : ANSIString;
begin                          { TODO : Dont need this any more, delete when sure }
  Result := LongDate;
  Result[11] := ' '; 			// just looks nicer
  Result := copy(Result, 1, 19);	{ TODO : Do UTF8 version of this ? }
end;


{ Sorts List and updates the recently used list under trayicon }
procedure TRTSearch.UseList();
begin
   if ButtonNotebookOptions.Enabled then
		{ TODO :  We are in notebook mode, refresh the relevent notebook list, not the all notes one. For now, we'll do nothing but fix this ! }
        NoteLister.LoadNotebookGrid(StringGrid1, StringGridNotebooks.Cells[0, StringGridNotebooks.Row])
  	else begin
    	NoteLister.LoadStGrid(StringGrid1);
    	Stringgrid1.SortOrder := soDescending;    // Sort with most recent at top
    	StringGrid1.SortColRow(True, 1);
    	NoteLister.LoadStGridNotebooks(StringGridNotebooks);
	end;
    RecentMenu();
end;


procedure TRTSearch.UpdateList(const Title, LastChange, FullFileName : ANSIString; TheForm : TForm );
begin
    if NoteLister = Nil then exit;				// we are quitting the app !
  	// Can we find line with passed file name ? If so, apply new data.
	if not NoteLister.AlterNote(ExtractFileNameOnly(FullFileName), LastChange, Title) then begin
        DebugLn('Assuming a new note ', FullFileName, ' [', Title, ']');
        NoteLister.AddNote(ExtractFileNameOnly(FullFileName)+'.note', Title, LastChange);
	end;
    NoteLister.ThisNoteIsOpen(FullFileName, TheForm);
    UseList();
end;

procedure TRTSearch.RecentMenu();
var
      Count : integer = 1;
      MenuCaption : string;
begin
    {$ifdef Darwin}
    RecentMenuMac();		// Alt proc for memory leaking Mac
    exit();
    {$endif}
    while (Count <= 10) do begin
       if Count < StringGrid1.RowCount then
             MenuCaption := StringGrid1.Cells[0, Count]
       else  MenuCaption := MenuEmpty;
       case Count of
         1 : TrayMenuRecent1.Caption := MenuCaption;
         2 : TrayMenuRecent2.Caption := MenuCaption;
         3 : TrayMenuRecent3.Caption := MenuCaption;
         4 : TrayMenuRecent4.Caption := MenuCaption;
         5 : TrayMenuRecent5.Caption := MenuCaption;
         6 : TrayMenuRecent6.Caption := MenuCaption;
         7 : TrayMenuRecent7.Caption := MenuCaption;
         8 : TrayMenuRecent8.Caption := MenuCaption;
         9 : TrayMenuRecent9.Caption := MenuCaption;
         10 : TrayMenuRecent10.Caption := MenuCaption;
        end;
      	inc(Count);
  	end;
    if Sett.RemoteRepo = '' then
        MenuSynchronise.Enabled := False
    else MenuSynchronise.Enabled := True;
end;

procedure TRTSearch.ButtonRefreshClick(Sender: TObject);
begin
    IndexNotes();
end;

procedure TRTSearch.ButtonClearSearchClick(Sender: TObject);
begin
        NoteLister.LoadStGrid(StringGrid1);
        ButtonClearSearch.Enabled := False;
        Edit1.Text := 'Search';
end;

procedure TRTSearch.Edit1EditingDone(Sender: TObject);
begin
    	if (Edit1.Text <> 'Search') and (Edit1.Text <> '') then begin
        	NoteLister.GetNotes(Edit1.Text);
        	NoteLister.LoadSearchGrid(StringGrid1);
        	ButtonClearSearch.Enabled := True;
        end;
end;

procedure TRTSearch.Edit1Enter(Sender: TObject);
begin
	if Edit1.Text = 'Search' then Edit1.Text := '';
end;

procedure TRTSearch.Edit1Exit(Sender: TObject);
begin
	if Edit1.Text = '' then Edit1.Text := 'Search';
end;


procedure TRTSearch.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
        CanClose := True;
end;

procedure TRTSearch.IndexNotes();
// var
	// TS1, TS2 : TTimeStamp;
begin
    if not Sett.HaveConfig then exit;
    NoteLister.WorkingDir:=Sett.NoteDirectory;
    NoteLister.GetNotes();		{ TODO : we should say how many we found }
    // TS1 := DateTimeToTimeStamp(Now);
	// Edit1.Text := 'That took (mS) ' + inttostr(TS2.Time - TS1.Time);
    UseList();
end;


procedure TRTSearch.FormCreate(Sender: TObject);
begin
    if not Sett.HaveConfig then
        Sett.Show;
    // that appears to cause a memory leak in the Mac  - what ? Sett ? don't think so ....
    NoteLister := TNoteLister.Create;
    IndexNotes();
    TrayIcon.Show;
end;

procedure TRTSearch.FormDestroy(Sender: TObject);
begin
  // DebugLn('Freeing Note Lister');
  NoteLister.Free;
  NoteLister := Nil;
end;

procedure TRTSearch.FormShow(Sender: TObject);
begin
    Top := Placement + random(Placement*2);
    Left := Placement + random(Placement*2);
    ButtonClearSearch.Enabled := False;
end;


procedure TRTSearch.OpenNote(NoteTitle: String; FullFileName: string;
		TemplateIs: AnsiString);
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
        if NoteLister.IsThisNoteOpen(NoteFileName, TheForm) then begin
            // if user opened and then closed, we won't know we cannot re-show
            try
            	TheForm.Show;
                exit();
			except on  EAccessViolation do
            	DebugLn('Tried to re show a closed note, thats OK');
			end;
            // We catch the exception and proceed .... but it should never happen.
        end;
    end;
    // if to here, we need open a new window. If Filename blank, its a new note
	EBox := TEditBoxForm.Create(Application);
    EBox.NoteTitle:= NoteTitle;

    EBox.NoteFileName := NoteFileName;
    Ebox.TemplateIs := TemplateIs;
    EBox.Top := Placement + random(Placement*2);
    EBox.Left := Placement + random(Placement*2);
    EBox.Show;
    EBox.Dirty := False;
    NoteLister.ThisNoteIsOpen(NoteFileName, EBox);
    exit();

	if NoteTitle <> '' then begin  			// We have a title
	        if FullFileName = '' then begin         // but no filename ?
	            if NoteLister.FileNameForTitle(NoteTitle, NoteFileName) then
	                EBox.NoteFileName := Sett.NoteDirectory + NoteFileName
	            // otherwise, its a new note with a title, user clicked "Link"
	        end else begin
	    	    EBox.NoteFileName := FullFileName;
	        end;
	    end;
	    EBox.NoteTitle:= NoteTitle;
	    EBox.Top := Placement + random(Placement*2);
	    EBox.Left := Placement + random(Placement*2);
	    EBox.Show;
	    EBox.Dirty := False;
end;

procedure TRTSearch.StringGrid1DblClick(Sender: TObject);
var
    NoteTitle : ANSIstring;
    FullFileName : string;
begin
    { TODO : If user double clicks title bar, we dont detect that and open some other note.  }
	FullFileName := Sett.NoteDirectory + StringGrid1.Cells[3, StringGrid1.Row];
  	if not FileExistsUTF8(FullFileName) then begin
      	showmessage('Cannot open ' + FullFileName);
      	exit();
  	end;
  	NoteTitle := StringGrid1.Cells[0, StringGrid1.Row];
  	if length(NoteTitle) > 0 then
        OpenNote(NoteTitle, FullFileName);
end;


{ ----------------- NOTEBOOK STUFF -------------------- }


procedure TRTSearch.ButtonShowAllNotesClick(Sender: TObject);
begin
        ButtonNotebookOptions.Enabled := False;
        UseList();		// Sadly, this will call RecentMenu() unnecessarily, bad on a Mac....
        StringGridNoteBooks.Hint := '';
        StringGridNotebooks.Options := StringGridNotebooks.Options - [goRowHighlight];
end;

procedure TRTSearch.StringGridNotebooksClick(Sender: TObject);
begin
        ButtonNotebookOptions.Enabled := True;
        StringGridNotebooks.Options := StringGridNotebooks.Options + [goRowHighlight];
        // NoteLister.LoadNotebookGrid(StringGrid1, StringGridNotebooks.Cells[0, StringGridNotebooks.Row]);
        UseList();
        StringGridNotebooks.Hint := 'Options for ' + StringGridNotebooks.Cells[0, StringGridNotebooks.Row];
end;

procedure TRTSearch.ButtonNotebookOptionsClick(Sender: TObject);
begin
		// showmessage(StringGridNotebooks.Cells[0, StringGridNotebooks.Row]);
    PopupMenuNotebook.Popup;
end;

procedure TRTSearch.MenuEditNotebookTemplateClick(Sender: TObject);
var
    NotebookID : ANSIString;
begin
    NotebookID := NoteLister.NotebookTemplateID(StringGridNotebooks.Cells[0, StringGridNotebooks.Row]);
    if NotebookID = '' then
    	showmessage('Error, cannot open template for ' + StringGridNotebooks.Cells[0, StringGridNotebooks.Row])
    else
    	OpenNote(StringGridNotebooks.Cells[0, StringGridNotebooks.Row] + ' Template',
        		Sett.NoteDirectory + NotebookID);
end;

procedure TRTSearch.MenuDeleteNotebookClick(Sender: TObject);
begin
    if IDYES = Application.MessageBox('Delete this Notebook',
    			PChar(StringGridNotebooks.Cells[0, StringGridNotebooks.Row]),
       			MB_ICONQUESTION + MB_YESNO) then
		DeleteNote(Sett.NoteDirectory + NoteLister.NotebookTemplateID(StringGridNotebooks.Cells[0, StringGridNotebooks.Row]));
end;

procedure TRTSearch.MenuNewNoteFromTemplateClick(Sender: TObject);
begin
    OpenNote('', Sett.NoteDirectory
    		+ NoteLister.NotebookTemplateID(StringGridNotebooks.Cells[0, StringGridNotebooks.Row]),
            StringGridNotebooks.Cells[0, StringGridNotebooks.Row]);
end;


{ ----------------- TRAY MENU STUFF -------------------}


procedure TRTSearch.MenuItemSettingsClick(Sender: TObject);
begin
    Sett.Show;
    RecentMenu();
end;

procedure TRTSearch.MenuQuitClick(Sender: TObject);
begin
  	Sett.AllowClose := True; // Cos it came from the trayIcon menu, we'll do it.
    Sett.Close;      // Close Setting Form, its the Main Form and that kills everything
end;

procedure TRTSearch.MenuSynchroniseClick(Sender: TObject);
begin
    FormSync.NoteDirectory := Sett.NoteDirectory;
    FormSync.LocalConfig := Sett.LocalConfig;
    FormSync.RemoteRepo := Sett.RemoteRepo;
    FormSync.SetupFileSync := False;
    if FormSync.Visible then
        FormSync.Show
    else
    	if (FormSync.ShowModal = mrOK) then
            IndexNotes();
end;

procedure TRTSearch.TrayIconClick(Sender: TObject);
begin
    PopUpMenuTray.Popup;    // Here so a right click works as well as left.
end;

procedure TRTSearch.TrayMenSearchClick(Sender: TObject);
begin
  	if Sett.NoteDirectory = '' then
        showmessage('You have not set a notes directory. Please click Settings')
    else  Show;
end;
procedure TRTSearch.TrayMenuAboutClick(Sender: TObject);
var
    S1, S2, S3, S4, S5 : string;
begin
  S1 := 'This is v0.11 alpha of tomboy-ng, a rewrite of Tomboy Notes'#10;
  S2 := 'using Lazarus and FPC. It is not quite ready for production'#10;
  S3 := 'use unless you are very careful and have good backups.'#10;
  S5 := '';
  {$IFDEF DARWIN}
  S5 := #10#10'WARNING - the Mac has a memory leak, working on it !';
  {$ENDIF}
  S4 := 'Build date ' + {$i %DATE%} + '  TargetCPU ' + {$i %FPCTARGETCPU%} + '  OS ' + {$i %FPCTARGETOS%};
  Showmessage(S1 + S2 + S3 + S4 + S5);
end;


procedure TRTSearch.TrayMenuNewClick(Sender: TObject);
begin
  	if Sett.NoteDirectory = '' then
        showmessage('You have not set a notes directory. Please click Settings')
    else
    	OpenNote();
end;

procedure TRTSearch.TrayMenuRecent1Click(Sender: TObject);
begin
	if TMenuItem(Sender).Caption <> MenuEmpty then
		OpenNote(TMenuItem(Sender).Caption);
end;


{ ----- Horrid 2 functions needing removal when Mac Memory Leak issues fixed ----- }
function TRTSearch.MenuItemMissing() : boolean;
var
	I : integer = 1;
    Count : integer;
    Found : boolean;
begin
  	Result := True;
  	while I <= StringGrid1.RowCount do begin	// check each entry and if not found, then return True
        if I = 10 then break;
        Found := False;
   		for Count := 1 to 10 do begin
            case Count of
              1: if TrayMenuRecent1.Caption = StringGrid1.Cells[0, I] then Found := True;
              2: if TrayMenuRecent2.Caption = StringGrid1.Cells[0, I] then Found := True;
              3: if TrayMenuRecent3.Caption = StringGrid1.Cells[0, I] then Found := True;
              4: if TrayMenuRecent4.Caption = StringGrid1.Cells[0, I] then Found := True;
              5: if TrayMenuRecent5.Caption = StringGrid1.Cells[0, I] then Found := True;
              6: if TrayMenuRecent6.Caption = StringGrid1.Cells[0, I] then Found := True;
              7: if TrayMenuRecent7.Caption = StringGrid1.Cells[0, I] then Found := True;
              8: if TrayMenuRecent8.Caption = StringGrid1.Cells[0, I] then Found := True;
              9: if TrayMenuRecent9.Caption = StringGrid1.Cells[0, I] then Found := True;
              10: if TrayMenuRecent10.Caption = StringGrid1.Cells[0, I] then Found := True;
            end;
   		end;
        if not Found then exit();	// that entry not present in menu, so exit with True.
        inc(I);
    end;
    // We got here because we did 'Found' each one we checked for, so nothing missing.
    Result := False;
end;

 	// OK, I know this is ugly but this is Mac own private version, it tries to avoid
    // calling the InternalUpdate() function so as to minimise the memory leaks therein.
procedure TRTSearch.RecentMenuMac();
var
	Count : integer = 1;
	MenuCaption : string;
begin
    debugln('In Mac version of RecentMenu');
    if ButtonNotebookOptions.Enabled then exit();		// to avoid issues with memory leak, we don't update RecentMenu in Notebook Mode
    if MenuItemMissing() then begin
		Count := 1;
  		while (Count <= 10) do begin
         	if Count < StringGrid1.RowCount then
               	MenuCaption := StringGrid1.Cells[0, Count]
         	else  MenuCaption := MenuEmpty;
         	case Count of
           		1 : TrayMenuRecent1.Caption := MenuCaption;
          		2 : TrayMenuRecent2.Caption := MenuCaption;
          		3 : TrayMenuRecent3.Caption := MenuCaption;
          		4 : TrayMenuRecent4.Caption := MenuCaption;
           		5 : TrayMenuRecent5.Caption := MenuCaption;
           		6 : TrayMenuRecent6.Caption := MenuCaption;
           		7 : TrayMenuRecent7.Caption := MenuCaption;
           		8 : TrayMenuRecent8.Caption := MenuCaption;
           		9 : TrayMenuRecent9.Caption := MenuCaption;
           		10 : TrayMenuRecent10.Caption := MenuCaption;
          	end;
        	inc(Count);
    	end;
      if Sett.RemoteRepo = '' then
          MenuSynchronise.Enabled := False
      else MenuSynchronise.Enabled := True;
      TrayIcon.InternalUpdate;				// we don't see changes unless we call this, and it leaks !
      debugln('... and we did call InternalUpDate');
	end;
end;


end.

