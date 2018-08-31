unit SearchUnit;

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
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
    Grids, ComCtrls, StdCtrls, ExtCtrls, Menus, CheckLst, Note_Lister,
    lazLogger;

type

    { TSearchForm }

    TSearchForm = class(TForm)
        ButtonSearchMode: TButton;
			ButtonNotebookOptions: TButton;
			ButtonShowAllNotes: TButton;
		ButtonRefresh: TButton;
        ButtonClearSearch: TButton;
        Edit1: TEdit;
		MenuEditNotebookTemplate: TMenuItem;
		MenuDeleteNotebook: TMenuItem;
        MenuCaseSensitive: TMenuItem;
        MenuAnyCombination: TMenuItem;
		MenuNewNoteFromTemplate: TMenuItem;
		Panel1: TPanel;
        PopupSearchMode: TPopupMenu;
		PopupMenuNotebook: TPopupMenu;
		Splitter1: TSplitter;
		StringGridNotebooks: TStringGrid;
        SelectDirectoryDialog1: TSelectDirectoryDialog;
        StringGrid1: TStringGrid;
        procedure ButtonSearchModeClick(Sender: TObject);
  procedure ButtonClearSearchClick(Sender: TObject);
		procedure ButtonNotebookOptionsClick(Sender: TObject);
  		procedure ButtonRefreshClick(Sender: TObject);
		procedure ButtonShowAllNotesClick(Sender: TObject);
		procedure Edit1EditingDone(Sender: TObject);
		procedure Edit1Enter(Sender: TObject);
		procedure Edit1Exit(Sender: TObject);
		procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
        procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
            );
		procedure FormShow(Sender: TObject);
        procedure MenuAnyCombinationClick(Sender: TObject);
		procedure MenuDeleteNotebookClick(Sender: TObject);
		procedure MenuEditNotebookTemplateClick(Sender: TObject);
        procedure MenuCaseSensitiveClick(Sender: TObject);
		procedure MenuNewNoteFromTemplateClick(Sender: TObject);
		procedure StringGridNotebooksClick(Sender: TObject);
        procedure StringGrid1DblClick(Sender: TObject);
    private

		function TrimDateTime(const LongDate: ANSIString): ANSIString;
        		{ Copies note data from internal list to StringGrid, sorts it and updates the
                  TrayIconMenu recently used list.  Does not 'refresh list from disk'.  }
		procedure UseList();
    public
        //AllowClose : boolean;
        NoteLister : TNoteLister;
        NoteDirectory : string;
            { returns True if user has note open, accepts ID or simple file name }
        {function IsNoteOpen(const ID: String): boolean;  }
            { If there is an open note from the passed filename, it will be marked read Only,
              If deleted, remove entry from NoteLister, will accept a GUID, Filename or FullFileName inc path }
        procedure MarkNoteReadOnly(const FullFileName: string; const WasDeleted : boolean);
         	{ Puts the names of recently used notes in the TrayMenu }
        procedure RecentMenu();
       	{ Call this NoteLister no longer thinks of this as a Open note }
        procedure NoteClosing(const ID: AnsiString);
        { Updates the List with passed data. Either updates existing data or inserts new }
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
          Button. }
        procedure OpenNote(NoteTitle : String = ''; FullFileName : string = ''; TemplateIs : AnsiString = '');
        { Returns True if it put next Note Title into SearchTerm }
        function NextNoteTitle(out SearchTerm : string) : boolean;
        { Initialises search of note titles, prior to calling NextNoteTitle() }
        procedure StartSearch();
        { Deletes the actual file then removes the indicated note from the internal
        data about notes, refreshes Grid }
        procedure DeleteNote(const FullFileName : ANSIString);
const
	MenuEmpty = '(empty)';

    end;

var
    SearchForm: TSearchForm;

implementation

{$R *.lfm}

uses MainUnit,      // Opening form, manages startup and Menus
    EditBox,
    settings,		// Manages settings.
    TB_Sync,		// So we can make changes to local manifest when a note is deleted.
    LCLType,		// For the MessageBox
    LazFileUtils;   // LazFileUtils needed for TrimFileName(), cross platform stuff


{ TSearchForm }



{ -------------   FUNCTIONS  THAT  PROVIDE  SERVICES  TO  OTHER   UNITS  ------------ }
{
function TSearchForm.IsNoteOpen(const ID : String) : boolean;
var
    AForm : TForm;
begin
    if NoteLister = nil then begin
        debugln('Error, asking about a note when notelister is nil');
        exit(false);
    end;
    Result := NoteLister.IsThisNoteOpen(ID, AForm);
end;
}
procedure TSearchForm.NoteClosing(const ID : AnsiString);
begin
    if NoteLister <> nil then         // else we are quitting the app !
    	NoteLister.ThisNoteIsOpen(ID, nil);
end;

procedure TSearchForm.StartSearch(); // Call before using NextNoteTitle() to list Titles.
begin
	NoteLister.StartSearch();
  // TitleIndex := 1;
end;


procedure TSearchForm.DeleteNote(const FullFileName: ANSIString);
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
		LocalMan.IDToDelete:= ShortFileName;
   	// else
    //	DebugLn('ERROR, failed to move deleted ID in local manifest [' + ExtractFileNameOnly(FullFileName)+ ']');
    LocalMan.Free;
    UseList();
end;

function TSearchForm.NextNoteTitle(out SearchTerm: string): boolean;
begin
	Result := NoteLister.NextNoteTitle(SearchTerm);
end;

function TSearchForm.IsThisaTitle(const Term : ANSIString) : boolean;
begin
	Result := NoteLister.IsThisATitle(Term);
end;


function TSearchForm.TrimDateTime(const LongDate : ANSIString ) : ANSIString;
begin                          { TODO : Dont need this any more, delete when sure }
  Result := LongDate;
  Result[11] := ' '; 			// just looks nicer
  Result := copy(Result, 1, 19);
end;


{ Sorts List and updates the recently used list under trayicon }
procedure TSearchForm.UseList();
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


procedure TSearchForm.UpdateList(const Title, LastChange, FullFileName : ANSIString; TheForm : TForm );
begin
    if NoteLister = Nil then exit;				// we are quitting the app !
  	// Can we find line with passed file name ? If so, apply new data.
	if not NoteLister.AlterNote(ExtractFileNameOnly(FullFileName), LastChange, Title) then begin
        // DebugLn('Assuming a new note ', FullFileName, ' [', Title, ']');
        NoteLister.AddNote(ExtractFileNameOnly(FullFileName)+'.note', Title, LastChange);
	end;
    NoteLister.ThisNoteIsOpen(FullFileName, TheForm);
    UseList();
end;


procedure TSearchForm.RecentMenu();
var
      Count : integer = 1;
      MenuCaption : string;
begin
    //debugln('Called Recent Menu');
    while (Count <= 10) do begin
       if Count < StringGrid1.RowCount then
             MenuCaption := StringGrid1.Cells[0, Count]
       else  MenuCaption := MenuEmpty;
//       if MainForm.UseTrayMenu then   // always load up the Popup menu, its not just Tray
           case Count of
             1 : MainForm.TrayMenuRecent1.Caption := MenuCaption;
             2 : MainForm.TrayMenuRecent2.Caption := MenuCaption;
             3 : MainForm.TrayMenuRecent3.Caption := MenuCaption;
             4 : MainForm.TrayMenuRecent4.Caption := MenuCaption;
             5 : MainForm.TrayMenuRecent5.Caption := MenuCaption;
             6 : MainForm.TrayMenuRecent6.Caption := MenuCaption;
             7 : MainForm.TrayMenuRecent7.Caption := MenuCaption;
             8 : MainForm.TrayMenuRecent8.Caption := MenuCaption;
             9 : MainForm.TrayMenuRecent9.Caption := MenuCaption;
             10 : MainForm.TrayMenuRecent10.Caption := MenuCaption;
           end;
       if MainForm.UseMainMenu then
           case Count of
             1 : MainForm.MMRecent1.Caption := MenuCaption;
             2 : MainForm.MMRecent2.Caption := MenuCaption;
             3 : MainForm.MMRecent3.Caption := MenuCaption;
             4 : MainForm.MMRecent4.Caption := MenuCaption;
             5 : MainForm.MMRecent5.Caption := MenuCaption;
             6 : MainForm.MMRecent6.Caption := MenuCaption;
             7 : MainForm.MMRecent7.Caption := MenuCaption;
             8 : MainForm.MMRecent8.Caption := MenuCaption;
             9 : MainForm.MMRecent9.Caption := MenuCaption;
             10 : MainForm.MMRecent10.Caption := MenuCaption;
            end;
      	inc(Count);
  	end;
    MainForm.MMSync.Enabled := not (Sett.RemoteRepo = '');
    MainForm.TrayMenuSynchronise.Enabled := not (Sett.RemoteRepo = '');
    //Sett.MMSync.Enabled := not (Sett.RemoteRepo = '');
end;

procedure TSearchForm.ButtonRefreshClick(Sender: TObject);
begin
    IndexNotes();
end;

procedure TSearchForm.ButtonClearSearchClick(Sender: TObject);
begin
        NoteLister.LoadStGrid(StringGrid1);
        ButtonClearSearch.Enabled := False;
        Edit1.Text := 'Search';
end;

procedure TSearchForm.ButtonSearchModeClick(Sender: TObject);
begin
    PopupSearchMode.PopUp();
end;

procedure TSearchForm.MenuAnyCombinationClick(Sender: TObject);
begin
    MenuAnyCombination.Checked := not MenuAnyCombination.Checked;
    Sett.CheckAnyCombination.Checked := MenuAnyCombination.Checked;
end;

procedure TSearchForm.MenuCaseSensitiveClick(Sender: TObject);
begin
    MenuCaseSensitive.Checked:= not MenuCaseSensitive.Checked;
    Sett.CheckCaseSensitive.Checked:= MenuCaseSensitive.Checked;
end;
procedure TSearchForm.Edit1EditingDone(Sender: TObject);
//var
//   TS1, TS2, TS3, TS4 : TTimeStamp;           // Temp time stamping to test speed
begin

    { TODO : Unintended - when user clicks "Show All Notes" after using this edit box, this event is triggered. And its slow. Better to make this respond to an Enter Key from OnKeyDown  }
    	if (Edit1.Text <> 'Search') and (Edit1.Text <> '') then begin
            //TS1:=DateTimeToTimeStamp(Now);
        	NoteLister.GetNotes(Edit1.Text);        // 2.7 sec on my not so fast laptop !
            //TS2:=DateTimeToTimeStamp(Now);
            //debugln('Search ' + inttostr(TS2.Time-TS1.Time) + 'ms');    // lower, lower - 2.7S
        	NoteLister.LoadSearchGrid(StringGrid1);
        	ButtonClearSearch.Enabled := True;
        end;
end;

procedure TSearchForm.Edit1Enter(Sender: TObject);
begin
	if Edit1.Text = 'Search' then Edit1.Text := '';
end;
{ TODO : As we start with focus on Edit1 this method clears it and user sees empty box. Should say 'Search'. }

procedure TSearchForm.Edit1Exit(Sender: TObject);
begin
	if Edit1.Text = '' then Edit1.Text := 'Search';
end;

procedure TSearchForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
    CanClose := False;
    hide();
{    CanClose := AllowClose;
    if not AllowClose then hide();        }
end;

function TSearchForm.IndexNotes() : integer;
// var
	// TS1, TS2 : TTimeStamp;
begin
    // TS1 := DateTimeToTimeStamp(Now);
    MainForm.CheckStatus();
    if not Sett.HaveConfig then exit(0);
    if NoteLister = Nil then begin
       NoteLister := TNoteLister.Create;
       NoteLister.DebugMode := Application.HasOption('debug-index');
       NoteLister.WorkingDir:=Sett.NoteDirectory;
    end;
    Result := NoteLister.GetNotes();
    UseList();
    // TS2 := DateTimeToTimeStamp(Now);
	// debugln('That took (mS) ' + inttostr(TS2.Time - TS1.Time));
    MainForm.UpdateNotesFound(Result);
end;


procedure TSearchForm.FormCreate(Sender: TObject);
begin
     //AllowClose := False;
end;

procedure TSearchForm.FormDestroy(Sender: TObject);
begin
  // DebugLn('Freeing Note Lister');
  NoteLister.Free;
  NoteLister := Nil;
end;

procedure TSearchForm.FormKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
     if ssCtrl in Shift then begin
       if key = ord('N') then begin OpenNote(); Key := 0; exit(); end;
     end;
end;

procedure TSearchForm.FormShow(Sender: TObject);
begin
    Left := Placement + random(Placement*2);
    ButtonClearSearch.Enabled := False;
    // Edit1.Text:= 'Search';
    MenuCaseSensitive.Checked := Sett.CheckCaseSensitive.Checked;
    MenuAnyCombination.Checked := Sett.CheckAnyCombination.Checked;
end;


procedure TSearchForm.MarkNoteReadOnly(const FullFileName : string; const WasDeleted : boolean);
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
       	    DebugLn('Tried to mark a closed note as readOnly, thats OK');
   	    end;
    end;
    if WasDeleted then
        NoteLister.DeleteNote(FullFileName);
end;

procedure TSearchForm.OpenNote(NoteTitle: String; FullFileName: string;
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

procedure TSearchForm.StringGrid1DblClick(Sender: TObject);
var
    NoteTitle : ANSIstring;
    FullFileName : string;
    //blar : integer;
begin
    //blar := StringGrid1.Row;
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


procedure TSearchForm.ButtonShowAllNotesClick(Sender: TObject);
begin
        ButtonNotebookOptions.Enabled := False;
        UseList();
        StringGridNoteBooks.Hint := '';
        StringGridNotebooks.Options := StringGridNotebooks.Options - [goRowHighlight];
end;

procedure TSearchForm.StringGridNotebooksClick(Sender: TObject);
begin
        ButtonNotebookOptions.Enabled := True;
        StringGridNotebooks.Options := StringGridNotebooks.Options + [goRowHighlight];
        UseList();
        StringGridNotebooks.Hint := 'Options for ' + StringGridNotebooks.Cells[0, StringGridNotebooks.Row];
end;

procedure TSearchForm.ButtonNotebookOptionsClick(Sender: TObject);
begin
    PopupMenuNotebook.Popup;
end;

procedure TSearchForm.MenuEditNotebookTemplateClick(Sender: TObject);
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



procedure TSearchForm.MenuDeleteNotebookClick(Sender: TObject);
begin
    if IDYES = Application.MessageBox('Delete this Notebook',
    			PChar(StringGridNotebooks.Cells[0, StringGridNotebooks.Row]),
       			MB_ICONQUESTION + MB_YESNO) then
		DeleteNote(Sett.NoteDirectory + NoteLister.NotebookTemplateID(StringGridNotebooks.Cells[0, StringGridNotebooks.Row]));
end;

procedure TSearchForm.MenuNewNoteFromTemplateClick(Sender: TObject);
begin
    OpenNote('', Sett.NoteDirectory
    		+ NoteLister.NotebookTemplateID(StringGridNotebooks.Cells[0, StringGridNotebooks.Row]),
            StringGridNotebooks.Cells[0, StringGridNotebooks.Row]);
end;

end.

