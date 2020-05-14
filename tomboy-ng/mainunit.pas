unit Mainunit;
 {
 * Copyright (C) 2018 David Bannon
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

{$mode objfpc}{$H+}

{   HISTORY
    2018/05/12  Extensive changes - MainUnit is now just that. This is not the same
                unit that used this name previously!
    2018/05/19  Control if we allow opening window to be dismissed and show TrayIcon
                and MainMenu.
    2018/05/20  Alterations to way we startup, wrt mainform status report.
    2018/05/20  Set the recent menu items caption to be 'empty' in case user looks
                before having set a notes directory.
    2018/06/02  Added a cli switch to debug sync

    2018/06/19  Got some stuff for singlenotemode() - almost working.
    2018/06/22  As above but maybe working now ?  DRB
    2018/07/04  Display number of notes found and a warning if indexing error occured.
    2018/07/11  Added --version and --no-splash to options, this form now has a main menu
                and does not respond to clicks anywhere with the popup menu, seems GTK
                does not like sharing menus (eg between here and the trayIcon) in gtk3 !
                So, in interests of uniformity, everyone gets a Main Menu and no Popup.
    2018/11/01  Now include --debug-log in list of INTERAL switches.
    2018/12/02  Now support Alt-[Left, Right] to turn off or on Bullets.
    2018/12/03  Added show splash screen to settings, -g or an indexing error will force show
    2019/03/19  Added a checkbox to hide screen on future startups
    2019/03/19  Added setting option to show search box at startup
    2019/04/07  Restructured Main and Popup menus. Untested Win/Mac.
    2019/04/13  Mv numb notes to tick line, QT5, drop CheckStatus()
    2019/05/06  Support saving pos and open on startup in note.
    2019/05/14  Display strings all (?) moved to resourcestrings
    2019/06/11  Moved an ifdef
    2019/07/21  Added a TitleColour for dark theme
    2019/08/20  Linux only, looks for (translated) help files in config dir first.
    2019/09/6   Button to download Help Notes in non-English
    2019/09/21  Restructured model for non-english help notes, names in menus !
    2019/10/13  Prevent Dismiss if desktop is Enlightenment, in OnCreateForm()
    2019/11/05  Don't treat %f as a command line file name, its an artifact
    2019/11/08  Tidy up building GTK3 and Qt5 versions, cleaner About
    2019/11/20  Don't assign PopupMenu to TrayIcon on (KDE and Qt5)
    2019/12/08  New "second instance" model.
    2019/12/11  Heavily restructured Startup, Main Menu everywhere !
    2019/12/20  Added option --delay-start for when desktop is slow to determine its (dark) colours
    2020/03/30  Allow user to set display colours.
    2020/04/10  Make help files non modal
    2020/04/12  Force sensible sizes for help notes.
    2020/04/28  Added randomize to create, need it for getlocaltime in settings.

    CommandLine Switches

    --delay-start

    --debug-log=some.log

    --dark-theme    Windows only, over rides the registery setting.

    --gnome3    Turns on MainMenu, TrayMenu off and prevents dismmiss of this
    -g
    --debug-sync Turn on Verbose mode during sync

    --debug-index Verbose mode when reading the notes directory.

    --debug-spell  Verbose mode when setting up speller

    --config-dir=<some_dir> Directory to keep config and sync manifest in.

    -o note_fullfilename
    --open=<note_fullfilename> Opens, in standalone mode, a note. Don't start
                the SimpleIPC conversation.

    --help -h   Shows and exits (not implemented)
                something to divert debug msg to a file ??
                something to do more debugging ?

    --no-splash Dont show the small opening status/splash window on startup

    --save-exit (Single note only) after import, save and exit.

    --version   Print version no and exit.

}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
    StdCtrls, LCLTranslator, DefaultTranslator, Buttons, simpleipc;

// These are choices for main and main popup menus.
// type TMenuTarget = (mtSep=1, mtNewNote, mtSearch, mtAbout=10, mtSync, mtSettings, mtHelp, mtQuit, mtTomdroid, mtRecent);

// These are the possible kinds of main menu items
// type TMenuKind = (mkFileMenu, mkRecentMenu, mkHelpMenu);




type

    { TMainForm }

    TMainForm = class(TForm)
        ApplicationProperties1: TApplicationProperties;
        ButtMenu: TBitBtn;
        ButtonClose: TButton;
        ButtonDismiss: TButton;
        CheckBoxDontShow: TCheckBox;
        ImageSpellCross: TImage;
        ImageSpellTick: TImage;
        ImageNotesDirCross: TImage;
        ImageConfigTick: TImage;
        ImageConfigCross: TImage;
        ImageSyncCross: TImage;
        ImageNotesDirTick: TImage;
        ImageSyncTick: TImage;
        Label1: TLabel;
        LabelError: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        LabelNoDismiss1: TLabel;
        LabelNoDismiss2: TLabel;
        LabelNotesFound: TLabel;
        TrayIcon: TTrayIcon;
        procedure ButtMenuClick(Sender: TObject);
        procedure ButtonCloseClick(Sender: TObject);
        procedure ButtonConfigClick(Sender: TObject);
        procedure ButtonDismissClick(Sender: TObject);
        procedure CheckBoxDontShowChange(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormResize(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure LabelErrorClick(Sender: TObject);
        procedure TrayIconClick(Sender: TObject);
        procedure TrayMenuTomdroidClick(Sender: TObject);
    private
        HelpList : TStringList;
        CommsServer : TSimpleIPCServer;
        // Allow user to dismiss (ie hide) the opening window. Set false if we have a note error
        AllowDismiss : boolean;
        // Start SimpleIPC server listening for some other second instance.
        procedure StartIPCServer();
        procedure CommMessageReceived(Sender: TObject);
        procedure TestDarkThemeInUse();

    public
        closeASAP: Boolean;
        HelpNotesPath : string;     // full path to help notes, with trailing delim.
        AltHelpNotesPath : string;  // where non-English notes might be. Existance of Dir says use it.
        UseTrayMenu : boolean;
        PopupMenuSearch : TPopupMenu;
        PopupMenuTray : TPopupMenu;
        MainTBMenu : TPopupMenu;
        // Called by the Sett unit when it knows the true config path.
        procedure SetAltHelpPath(ConfigPath: string);
        procedure ShowAbout();
            // Ret path to where help notes are, either default English or Non-English
        function ActualHelpNotesPath() : string;
            // This procedure responds to ALL recent note menu clicks !
        procedure RecentMenuClicked(Sender: TObject);
            { Displays the indicated help note, eg recover.note, in Read Only, Single Note Mode
              First tries the AltHelpPath, then HelpPath}
        procedure ShowHelpNote(HelpNoteName: string);
            { Updates status data on MainForm, tick list }
        procedure UpdateNotesFound(Numb: integer);
        { Opens a note in single note mode. Pass a full file name, a bool that closes whole app
        on exit and one that indicates ReadOnly mode. }
        procedure SingleNoteMode(FullFileName: string; const CloseOnExit, ViewerMode : boolean);
        { Shortcut to SingleNoteMode(Fullfilename, True, False) }
        procedure SingleNoteMode(FullFileName: string);
    end;

                                // This here temp, it returns the singlenotefilename from CLI Unit.
    function SingleNoteFileName() : string;

var
    MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }


uses LazLogger, LazFileUtils, LazUTF8,
    settings, ResourceStr,
    SearchUnit,
    {$ifdef LCLGTK2}
    gtk2, gdk2,          // required to fix a bug that clears clipboard contents at close.
    {$endif}
    {$ifdef LINUX}
    Clipbrd,
    {$endif}   // Stop linux clearing clipboard on app exit.
    Editbox,    // Used only in SingleNoteMode
    Note_Lister, cli,
    Tomdroid {$ifdef windows}, registry{$endif};

var
    HelpNotes : TNoteLister;

{ =================================== V E R S I O N    H E R E =============}

const   //Version_string  = {$I %TOMBOY_NG_VER};
        AltHelp = 'alt-help';   // dir name where we store non english help notes.


function SingleNoteFileName() : string;
begin
    result := SingleNoteName;                   // Thats the global in CLI Unit
end;

procedure TMainForm.SingleNoteMode(FullFileName: string);
begin
     SingleNoteMode(FullFileName, True, False);
end;

procedure TMainForm.SingleNoteMode(FullFileName : string; const CloseOnExit, ViewerMode : boolean);
var
    EBox : TEditBoxForm;
begin
    if DirectoryExistsUTF8(ExtractFilePath(FullFileName))
        or (ExtractFilePath(FullFileName) = '') then begin
        try
            try
            EBox := TEditBoxForm.Create(Application);
            EBox.SingleNoteFileName := SingleNoteFileName();    // Thats the global from CLI Unit, via a local helper function
            EBox.NoteTitle:= '';
            EBox.NoteFileName := FullFileName;
            Ebox.TemplateIs := '';
            EBox.Dirty := False;
            if ViewerMode then
                EBox.SetReadOnly(False);
            EBox.ShowModal;
            except on E: Exception do begin debugln('!!! EXCEPTION - ' + E.Message); showmessage(E.Message); end;
            end;
        finally
            try
            FreeandNil(EBox);
            except on E: Exception do debugln('!!! EXCEPTION - What ? no FreeAndNil ?' + E.Message);
            end;
        end;
    end else begin
        DebugLn('Sorry, cannot find that directory [' + ExtractFilePath(FullFileName) + ']');
        showmessage('Sorry, cannot find that directory [' + ExtractFilePath(FullFileName) + ']');
    end;
    if CloseOnExit then Close;      // we also use singlenotemode internally in several places
end;


// ---------------- HELP NOTES STUFF ------------------

procedure TMainForm.ShowHelpNote(HelpNoteName: string);   // ToDo : consider moving this method and associated list to SearchUnit.
var
    EBox : TEditBoxForm;
    TheForm : TForm;
    Index : integer;
begin
    if FileExists(ActualHelpNotesPath() + HelpNoteName) then begin
        If HelpList = nil then begin
            HelpList := TStringList.Create;
            HelpList.Sorted:=True;
		end else begin
            if HelpList.Find(HelpNoteName, Index) then begin
                // Bring TForm(HelpList.Objects[Index]) to front
                TheForm := TForm(HelpList.Objects[Index]);
                try
                    //writeln('Attempting a reshow');
          	        TheForm.Show;
                    SearchForm.MoveWindowHere(TheForm.Caption);
                    TheForm.EnsureVisible(true);
                    exit;
				except on E: Exception do {showmessage(E.Message)};
                // If user had this help page open but then closed it entry is still in
                // list so we catch the exception, ignore it and upen a new note.
                // its pretty ugly under debugger but user does not see this.
				end;
			end;
		end;
        // If we did not find it in the list and exit, above, we will make a new one.
        EBox := TEditBoxForm.Create(Application);
        EBox.SetReadOnly(False);
        EBox.SearchedTerm := '';
        EBox.NoteTitle:= '';
        EBox.NoteFileName := ActualHelpNotesPath() + HelpNoteName;
        Ebox.TemplateIs := '';
        EBox.Show;
        EBox.Dirty := False;
        HelpList.AddObject(HelpNoteName, EBox);
        EBox.Top := HelpList.Count * 10;
        EBox.Left := HelpList.Count * 10;
        EBox.Width := Screen.Width div 2;      // Set sensible sizes.
        EBox.Height := Screen.Height div 2;
    end else showmessage('Unable to find ' + HelpNotesPath + HelpNoteName);
end;

function TMainForm.ActualHelpNotesPath(): string;
begin
    Result := HelpNotesPath;
    if DirectoryExistsUTF8(AltHelpNotesPath) then
        Result := AltHelpNotesPath;
end;

procedure TMainForm.SetAltHelpPath(ConfigPath : string);
begin
    AltHelpNotesPath := ConfigPath + ALTHELP + PathDelim;
end;



// -----------------------------------------------------------------
//            S T A R T   U P   T H I N G S
// -----------------------------------------------------------------



procedure TMainForm.FormCreate(Sender: TObject);
begin
    Randomize;                                      // used by sett.getlocaltime()
    HelpList := Nil;
    UseTrayMenu := true;
    AllowDismiss := true;
    if SingleNoteFileName = '' then
        StartIPCServer();                     // Don't bother to check is we are client, cannot be if we are here.
    {$ifdef LCLCARBON}
    UseTrayMenu := false;
    {$endif}
    {$ifdef WINDOWS}HelpNotesPath := AppendPathDelim(ExtractFileDir(Application.ExeName));{$endif}
    {$ifdef LINUX}  HelpNotesPath := '/usr/share/doc/tomboy-ng/';    {$endif}
    {$ifdef DARWIN} HelpNotesPath := ExtractFileDir(ExtractFileDir(Application.ExeName))+'/Resources/';{$endif}
    AltHelpNotesPath := HelpNotesPath + ALTHELP + PathDelim;        // Overridden in Sett for Linux
    if UseTrayMenu then begin
        PopupMenuTray := TPopupMenu.Create(Self);
        TrayIcon.PopUpMenu := PopupMenuTray;        // SearchForm will populate it when ready
        TrayIcon.Show;
    end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
    freeandnil(CommsServer);
    freeandnil(HelpNotes);
    //if HelpList <> Nil then writeln('Help List has ' + inttostr(HelpList.Count));
    freeandnil(HelpList);
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  {$ifdef LCLGTK2}
  c: PGtkClipboard;
  t: string;
  {$endif}
  // OutFile : TextFile;
  AForm : TForm;
begin
    {$ifdef LCLGTK2}
    c := gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
    t := Clipboard.AsText;
    gtk_clipboard_set_text(c, PChar(t), Length(t));
    gtk_clipboard_store(c);
    {$endif}
    Sett.AreClosing:=True;
    if assigned(SearchForm.NoteLister) then begin
      AForm := SearchForm.NoteLister.FindFirstOpenNote();
      while AForm <> Nil do begin
          AForm.close;
          AForm := SearchForm.NoteLister.FindNextOpenNote();
      end;
    end;
end;

procedure TMainForm.CommMessageReceived(Sender : TObject);
Var
    S : String;
begin
    // debugln('Here in Main.CommMessageRecieved, a message was received');
    CommsServer .ReadMessage;
    S := CommsServer .StringMessage;
    case S of
        'SHOWSEARCH' : begin
                    SearchForm.Show;
                    SearchForm.MoveWindowHere(SearchForm.Caption);
                end;
    end;
end;

procedure TMainForm.StartIPCServer();
begin
        CommsServer  := TSimpleIPCServer.Create(Nil);
        CommsServer.ServerID:='tomboy-ng';
        CommsServer.OnMessageQueued:=@CommMessageReceived;
        CommsServer.Global:=True;                  // anyone can connect
        CommsServer.StartServer({$ifdef WINDOWS}False{$else}True{$endif});  // start listening, threaded
end;


resourcestring
  // rsAnotherInstanceRunning = 'Another instance of tomboy-ng appears to be running. Will exit.';
  rsFailedToIndex = 'Failed to index one or more notes.';
  rsCannotDismiss1 = 'Sadly, on this OS or because of a Bad Note,';
  rsCannotDismiss2 = 'I cannot let you dismiss this window';
  rsCannotDismiss3 = 'Are you trying to shut me down ? Dave ?';



procedure TMainForm.FormShow(Sender: TObject);
var
    NoteID, NoteTitle : string;
begin
    if CloseASAP then begin
      close;
      exit;
    end;
    if Application.HasOption('no-splash') or (not Sett.CheckShowSplash.Checked) then begin
         if AllowDismiss then ButtonDismissClick(Self);
     end;
    Left := 10;
    Top := 40;
    TestDarkThemeInUse();
    {$ifdef windows}                // linux apps know how to do this themselves
    if Sett.DarkTheme then begin
        //color := Sett.BackGndColour;
        color := Sett.HiColour;
        font.color := Sett.TextColour;
        ButtMenu.Color := Sett.BackGndColour;
        ButtonClose.Color := Sett.BackGndColour;
        ButtonDismiss.Color := Sett.HiColour;
    end;
    {$endif}
    if SingleNoteFileName <> '' then begin      // Thats the global in CLI Unit
        SingleNoteMode(SingleNoteFileName);
        exit;
    end;
    LabelNoDismiss1.Caption:='';
    LabelNoDismiss2.Caption := '';
    if SearchForm.NoteLister.XMLError then begin
        LabelError.Caption := rsFailedToIndex;
        AllowDismiss := False;
    end else
        LabelError.Caption := '';
    if not AllowDismiss then begin
        LabelNoDismiss1.Caption := rsCannotDismiss1;
        LabelNoDismiss2.Caption := rsCannotDismiss2;
        LabelNoDismiss1.Hint:=rsCannotDismiss3;
        LabelNoDismiss2.Hint := LabelNoDismiss1.Hint;
        CheckBoxDontShow.Enabled := False;
        Visible := True;
    end else
        CheckBoxDontShow.checked := not Sett.CheckShowSplash.Checked;
    if Sett.CheckShowSearchAtStart.Checked then
        SearchForm.Show;
    if SearchForm.NoteLister.FindFirstOOSNote(NoteTitle, NoteID) then
        repeat
            SearchForm.OpenNote(NoteTitle, Sett.NoteDirectory + NoteID);
        until SearchForm.NoteLister.FindNextOOSNote(NoteTitle, NoteID) = false;
end;

resourcestring
  rsBadNotesFound1 = 'Bad notes found, goto Settings -> Snapshots -> Existing Notes.';
  rsBadNotesFound2 = 'You should do so to ensure your notes are safe.';
  rsFound = 'Found';
  rsNotes = 'notes';

procedure TMainForm.LabelErrorClick(Sender: TObject);
begin
    if LabelError.Caption <> '' then
        showmessage(rsBadNotesFound1 + #10#13 + rsBadNotesFound2);
end;

procedure TMainForm.UpdateNotesFound(Numb : integer);
begin
    LabelNotesFound.Caption := rsFound + ' ' + inttostr(Numb) + ' ' + rsNotes;
         ImageConfigCross.Left := ImageConfigTick.Left;
     ImageConfigTick.Visible := Sett.HaveConfig;
     ImageConfigCross.Visible := not ImageConfigTick.Visible;

     ImageNotesDirCross.Left := ImageNotesDirTick.Left;
     ImageNotesDirTick.Visible := Numb > 0;
     ImageNotesDirCross.Visible := not ImageNotesDirTick.Visible;

     ImageSpellCross.Left := ImageSpellTick.Left;
     ImageSpellTick.Visible := Sett.SpellConfig;
     ImageSpellCross.Visible := not ImageSpellTick.Visible;

     ImageSyncCross.Left := ImageSyncTick.Left;

     ImageSyncTick.Visible :=  (Sett.ValidSync <> '');
     ImageSyncCross.Visible := not ImageSyncTick.Visible;

     if (ImageConfigTick.Visible) then begin
        ButtonDismiss.Enabled := AllowDismiss;
     end;
end;

procedure TMainForm.ButtonDismissClick(Sender: TObject);
begin
    {$ifdef LCLCOCOA}            // ToDo : is this still necessary ? Or can Cocoa hide like other systems ?
    width := 0;
    height := 0;
    {$else}
    hide();
    {$endif}
end;

procedure TMainForm.CheckBoxDontShowChange(Sender: TObject);
var
    OldMask : boolean;
begin
    if Visible then begin
        Sett.CheckShowSplash.Checked := not Sett.CheckShowSplash.Checked;
        OldMask :=  Sett.MaskSettingsChanged;
        Sett.MaskSettingsChanged := False;
        Sett.CheckReadOnlyChange(Sender);
        Sett.MaskSettingsChanged := OldMask;
    end;
    // showmessage('change dont show and Visible=' + booltostr(Visible, True));
end;


procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
     if ssCtrl in Shift then begin
       if key = ord('N') then begin
         SearchForm.OpenNote();     // MMNewNoteClick(self);    OK as long as Notes dir is set
         Key := 0;
         exit();
       end;
     end;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
    ButtMenu.Width := (Width div 3);
    ButtonDismiss.Width := (Width div 3);
end;

    // Attempt to detect we are in a dark theme, sets relevent colours.
procedure TMainForm.TestDarkThemeInUse();
var
  Col : string;
    {$ifdef WINDOWS}  function WinDarkTheme : boolean;
    var
        RegValue : integer;
        Registry : TRegistry;
    begin
        Registry := TRegistry.Create;
        try
            Registry.RootKey := HKEY_CURRENT_USER;
            if Registry.OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize') then begin
                try
                    RegValue := Registry.ReadInteger('AppsUseLightTheme');
                except on
                    E: ERegistryException do exit(True);  // If Key present but not AppsUseLightTheme, default to Dark
                end;
                exit(RegValue = 0);
            end else
                exit(false);
        finally
            Registry.Free;
        end;
    end; {$endif}

begin
    {$ifdef windows}
    if Application.HasOption('dark-theme') then // Manual override always wins on windows !
        Sett.DarkTheme := True
    else {$endif} begin
        Sett.DarkTheme := false;
        {$ifdef WINDOWS}
        Sett.DarkTheme := WinDarkTheme();
        {$else}
        // if char 3, 5 and 7 are all 'A' or above, we are not in a DarkTheme
        Col := hexstr(qword(GetRGBColorResolvingParent()), 8);
        Sett.DarkTheme := (Col[3] < 'A') and (Col[5] < 'A') and (Col[7] < 'A');
        {$endif}
    end;
	Sett.SetColours;
end;

{ ------------- M E N U   M E T H O D S ----------------}


procedure TMainForm.ButtonConfigClick(Sender: TObject);
begin
    Sett.Show();
end;

procedure TMainForm.ButtonCloseClick(Sender: TObject);
begin
    MainForm.Close;
end;

procedure TMainForm.ButtMenuClick(Sender: TObject);
begin
    self.MainTBMenu.popup;
end;

procedure TMainForm.TrayIconClick(Sender: TObject);
begin
    PopupMenuTray.PopUp();
end;

procedure TMainForm.TrayMenuTomdroidClick(Sender: TObject);
begin
    if FormTomdroid.Visible then FormTomdroid.BringToFront
    else FormTomdroid.ShowModal;
end;

procedure TMainForm.RecentMenuClicked(Sender: TObject);
begin
 	if TMenuItem(Sender).Caption <> SearchForm.MenuEmpty then
 		SearchForm.OpenNote(TMenuItem(Sender).Caption);
end;

RESOURCESTRING
    rsAbout1 = 'This is tomboy-ng, a rewrite of Tomboy Notes using Lazarus';
    rsAbout2 = 'and FPC. While its ready for production';
    rsAbout3 = 'use, you still need to be careful and have good backups.';
    rsAboutVer = 'Version';
    rsAboutBDate = 'Build date';
    rsAboutCPU = 'TargetCPU';
    rsAboutOperatingSystem = 'OS';


procedure TMainForm.ShowAbout();
var
        Stg : string;
begin
        Stg := rsAbout1 + #10 + rsAbout2 + #10 + rsAbout3 + #10
            + rsAboutVer + ' ' + Version_String;                    // version is in cli unit.
        {$ifdef LCLCOCOA} Stg := Stg + ', 64bit Cocoa'; {$endif}
        {$ifdef LCLQT5}   Stg := Stg + ', QT5';         {$endif}
        {$ifdef LCLGTK3}  Stg := Stg + ', GTK3';        {$endif}
        {$ifdef LCLGTK2}  Stg := Stg + ', GTK2';        {$endif}
        Stg := Stg + #10 + rsAboutBDate + ' ' + {$i %DATE%} + #10
            + rsAboutCPU + ' ' + {$i %FPCTARGETCPU%} + '  '
            + rsAboutOperatingSystem + ' ' + {$i %FPCTARGETOS%}
            + ' ' + GetEnvironmentVariable('XDG_CURRENT_DESKTOP');
        Showmessage(Stg);
end;

end.

