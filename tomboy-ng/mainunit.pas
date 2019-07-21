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

    CommandLine Switches

    --debug-log=some.log

    --dark-theme    Windows only, over rides the registery setting.

    --gnome3    Turns on MainMenu, TrayMenu off and prevents dismmiss of this
    -g
    --debug-sync Turn on Verbose mode during sync

    --debug-index Verbose mode when reading the notes directory.

    --debug-spell  Verbose mode when setting up speller

    --config-dir=<some_dir> Directory to keep config and sync manifest in.

    -o note_fullfilename
    --open=<note_fullfilename> Opens, in standalone mode, a note. Does not check
                to see if another copy of tomboy-ng is open but will prevent a
                normal startup of tomboy-ng. Won't interfere with an existing copy
                however.

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
    StdCtrls, LCLTranslator, DefaultTranslator;

// These are choices for main and main popup menus.
type TMenuTarget = (mtSep=1, mtNewNote, mtSearch, mtAbout=10, mtSync, mtSettings, mtHelp, mtQuit, mtTomdroid, mtRecent);

// These are the possible kinds of main menu items
type TMenuKind = (mkFileMenu, mkRecentMenu, mkHelpMenu);

type

    { TMainForm }

    TMainForm = class(TForm)
        ApplicationProperties1: TApplicationProperties;
        ButtonDismiss: TButton;
        ButtonConfig: TButton;
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
        //procedure ApplicationProperties1EndSession(Sender: TObject);
        procedure ButtonConfigClick(Sender: TObject);
        procedure ButtonDismissClick(Sender: TObject);
        procedure CheckBoxDontShowChange(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormResize(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure LabelErrorClick(Sender: TObject);
        // procedure MMHelpTomboyClick(Sender: TObject);
        procedure TrayIconClick(Sender: TObject);
        procedure TrayMenuTomdroidClick(Sender: TObject);
    private
        CmdLineErrorMsg : string;
        // Allow user to dismiss (ie hide) the opening window. Set false if we have a note error or -g on commandline
        AllowDismiss : boolean;
        procedure AddItemToAMenu(TheMenu: TMenu; Item: string; mtTag: TMenuTarget; OC: TNotifyEvent; MenuKind: TMenuKind);

        function CommandLineError() : boolean;
            // responds to any main or mainPopup menu clicks except recent note ones.
        procedure FileMenuClicked(Sender: TObject);
        procedure FindHelpFiles();
        //procedure OnEndSessionApp(Sender: TObject);
        procedure ShowAbout();
        procedure TestDarkThemeInUse();

    public
        HelpNotesPath : string;     // full path to help notes, with trailing delim.
        UseTrayMenu : boolean;
        UseMainMenu : boolean;
        MainMenu : TMainMenu;
        FileMenu, RecentMenu : TMenuItem;
        PopupMenuSearch : TPopupMenu;
        PopupMenuTray : TPopupMenu;
        procedure ClearRecentMenuItems();
            // builds the Menus and fills in all items except recent notes. Only
            // called once, during startup except if ItsAnUpdate is True, thats triggered
            // by Sett changes to Sync is config or Tomdroid mode enabled.
        procedure FillInFileMenus(ItsAnUpdate: boolean=false);
            // Here we will add a new item to each menu, one by one. Menus must be created and
            // the top level entries, File and Recent, added to MainMenu
        procedure AddMenuItem(Item: string; mtTag: TMenuTarget; OC: TNotifyEvent; MenuKind: TMenuKind);
            // This procedure responds to ALL recent note menu clicks !
        procedure RecentMenuClicked(Sender: TObject);
            { Displays the indicated help note, eg recover.note, in Read Only, Single Note Mode }
        procedure ShowHelpNote(HelpNoteName: string);
            { Updates status data on MainForm, tick list }
        procedure UpdateNotesFound(Numb: integer);
        { Opens a note in single note mode. Pass a full file name, a bool that closes whole app
        on exit and one that indicates ReadOnly mode. }
        procedure SingleNoteMode(FullFileName: string; const CloseOnExit, ViewerMode : boolean);
        { Shortcut to SingleNoteMode(Fullfilename, True, False) }
        procedure SingleNoteMode(FullFileName: string);
    end;

var
    MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }


uses LazLogger, LazFileUtils,
    settings,
    SearchUnit,
    {$ifdef LINUX}
    gtk2, gdk2, Clipbrd,
    {$endif}   // Stop linux clearing clipboard on app exit.
    uAppIsRunning,
    Editbox,    // Used only in SingleNoteMode
    Note_Lister,
    Tomdroid {$ifdef windows}, registry{$endif};

var
    HelpNotes : TNoteLister;

{ =================================== V E R S I O N    H E R E =============}
const Version_string  = {$I %TOMBOY_NG_VER};





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
            EBox.SingleNoteMode:=True;
            EBox.NoteTitle:= '';
            EBox.NoteFileName := FullFileName;
            Ebox.TemplateIs := '';
            EBox.Top := Placement + random(Placement*2);
            EBox.Left := Placement + random(Placement*2);
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

Procedure TMainForm.ShowHelpNote(HelpNoteName : string);
begin
    if FileExists(HelpNotesPath + HelpNoteName) then
       SingleNoteMode(HelpNotesPath + HelpNoteName, False, True)
    else showmessage('Unable to find ' + HelpNotesPath + HelpNoteName);
end;


resourcestring
  rsAnotherInstanceRunning = 'Another instance of tomboy-ng appears to be running. Will exit.';
  rsFailedToIndex = 'Failed to index one or more notes.';
  rsCannotDismiss1 = 'Sadly, on this OS or because of a Bad Note,';
  rsCannotDismiss2 = 'I cannot let you dismiss this window';
  rsCannotDismiss3 = 'Are you trying to shut me down ? Dave ?';



procedure TMainForm.FormShow(Sender: TObject);
// WARNING - the options here MUST match the options list in CommandLineError()
 { ToDo : put options in a TStringList and share, less mistakes ....}
var
    NoteID, NoteTitle : string;
    Params : TStringList;
    LongOpts : array [1..12] of string = ('dark-theme', 'lang:', 'debug-log:', 'no-splash', 'version', 'gnome3', 'debug-spell',
            'debug-sync', 'debug-index', 'config-dir:','open-note:', 'save-exit');
begin
    // debugln('Form color is ' + inttostr(Color));
    if CmdLineErrorMsg <> '' then begin
        close;    // cannot close in OnCreate();
        exit;       // otherwise we execute rest of this method before closing.
    end;
    if Application.HasOption('version') then begin
        Enabled := False;
         debugln('tomboy-ng version ' + Version_String);
         close();
         exit();
     end;
     if Application.HasOption('no-splash') or (not Sett.CheckShowSplash.Checked) then begin
         if AllowDismiss then ButtonDismissClick(Self);
     end;
    Left := 10;
    Top := 40;
    if Application.HasOption('o', 'open-note') then begin
        //showmessage('Single note mode');
        SingleNoteMode(Application.GetOptionValue('o', 'open-note'));
        exit();
    end;
    TestDarkThemeInUse();

    {$ifdef windows}                // linux apps know how to do this themselves
    if Sett.DarkTheme then begin
        //color := Sett.BackGndColour;
        color := Sett.HiColor;
        font.color := Sett.TextColour;
        ButtonConfig.Color := Sett.BackGndColour;
        ButtonDismiss.Color := Sett.HiColor;
    end;
    {$endif}
    Params := TStringList.Create;
    try
        Application.GetNonOptions('hgo:', LongOpts, Params);
        {for I := 0 to Params.Count -1 do
            debugln('Extra Param ' + inttostr(I) + ' is ' + Params[I]);  }
        if Params.Count = 1 then begin
            SingleNoteMode(Params[0]);    // if we have just one extra parameter, we assume it a filename,
            exit();
        end;
        if Params.Count > 1 then begin
            debugln('Unrecognised parameters on command line');
            close;
            exit();                     // Call exit to ensure remaining part method is not executed
        end;
    finally
        FreeAndNil(Params);
    end;
    if AlreadyRunning() then begin
        showmessage(rsAnotherInstanceRunning);
        close();
        exit();
    end else begin
        if UseMainMenu then begin
            FileMenu.Visible := True;
            RecentMenu.Visible := True;
        end;
        LabelNoDismiss1.Caption:='';
        LabelNoDismiss2.Caption := '';
        // CheckStatus();
        SearchForm.IndexNotes(); // also calls Checkstatus, calls UpdateNotesFound()
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
    end;
    if UseTrayMenu then
       TrayIcon.Show;

    if SearchForm.NoteLister.FindFirstOOSNote(NoteTitle, NoteID) then
        repeat
            SearchForm.OpenNote(NoteTitle, Sett.NoteDirectory + NoteID);
        until SearchForm.NoteLister.FindNextOOSNote(NoteTitle, NoteID) = false;
end;

{procedure TMainForm.OnEndSessionApp(Sender: TObject);
var
  OutFile : TextFile;
begin
    AssignFile(OutFile, '/home/dbannon/closelogMainFormSession.txt');
    Rewrite(OutFile);
    writeln(OutFile, 'OnEndSessionApp Called');
    CloseFile(OutFile);
end;   }

resourcestring
  rsBadNotesFound1 = 'Bad notes found, goto Settings -> Snapshots -> Existing Notes.';
  rsBadNotesFound2 = 'You should do so to ensure your notes are safe.';

procedure TMainForm.LabelErrorClick(Sender: TObject);
begin
    if LabelError.Caption <> '' then
        showmessage(rsBadNotesFound1 + #10#13 + rsBadNotesFound2);
end;

procedure TMainForm.UpdateNotesFound(Numb : integer);
begin
    LabelNotesFound.Caption := 'Found ' + inttostr(Numb) + ' notes';
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
     ImageSyncTick.Visible := (Sett.LabelSyncRepo.Caption <> rsSyncNotConfig)
                and (Sett.LabelSyncRepo.Caption <> '');
     ImageSyncCross.Visible := not ImageSyncTick.Visible;

     if (ImageConfigTick.Visible and ImageNotesDirTick.Visible) then begin
        ButtonDismiss.Enabled := AllowDismiss;
     end;
end;

procedure TMainForm.ButtonDismissClick(Sender: TObject);
begin
    {$ifdef LCLCOCOA}
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

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  {$ifdef LINUX}
  c: PGtkClipboard;
  t: string;
  {$endif}
  // OutFile : TextFile;
  AForm : TForm;
begin
    {$ifdef LINUX}
    {$ifndef LCLQT5}
    c := gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
    t := Clipboard.AsText;
    gtk_clipboard_set_text(c, PChar(t), Length(t));
    gtk_clipboard_store(c);
    {$endif}
    {$endif}
    freeandnil(HelpNotes);
    // ToDo : This might be good place to hook into a closing app and flush any open notes.
    // but maybe its OnCloseSession too ?
    {
    AssignFile(OutFile, '/home/dbannon/closelogMainForm.txt');
    Rewrite(OutFile);
    writeln(OutFile, 'FormClose just closing ');
    CloseFile(OutFile);
    }
    Sett.AreClosing:=True;
    if assigned(SearchForm.NoteLister) then begin;
      AForm := SearchForm.NoteLister.FindFirstOpenNote();
      while AForm <> Nil do begin
          AForm.close;
          AForm := SearchForm.NoteLister.FindNextOpenNote();
      end;
    end;
end;

RESOURCESTRING
    {$ifdef DARWIN}
    rsMacHelp1 = 'eg   open tomboy-ng.app';
    rsMacHelp2 = 'eg   open tomboy-ng.app --args -o Note.txt|.note';
    {$endif}
    rsHelpLang = 'Force Language, supported es';
    rsHelpDebug = 'Direct debug output to SOME.LOG.';
    rsHelpHelp = 'Show this help and exit.';
    rsHelpVersion = 'Print version and exit';
    rsHelpRedHat = 'Run in RedHatGnome mode, no TrayIcon';
    rsHelpNoSplash = 'Dont show small status/splash window';
    rsHelpDebugSync = 'Show debug messages during Sync';
    rsHelpDebugIndex = 'Show debug messages during index of notes';
    rsHelpDebugSpell = 'Show debug messages during spell setup';
    rsHelpConfig = 'Create or use an alternative config';
    rsHelpSingleNote = 'Open indicated note, switch is optional';
    rsHelpSaveExit = 'After import single note, save and exit.';


function TMainForm.CommandLineError() : boolean;
// WARNING - the options here MUST match the options list in FormShow()
begin
    Result := false;
    CmdLineErrorMsg := Application.CheckOptions('hgo:l:', 'lang: debug-log: dark-theme no-splash version help gnome3 open-note: debug-spell debug-sync debug-index config-dir: save-exit');
    if Application.HasOption('h', 'help') then
        CmdLineErrorMsg := 'Show Help Message';
    if CmdLineErrorMsg <> '' then begin
       debugln('Usage - ');
       {$ifdef DARWIN}
       debugln(rsMachelp1);
       debugln(rsMacHelp2);
       {$endif}
       {$ifdef WINDOWS}debugln('   --dark-theme'); {$endif}
       debugln('   -l CCode  --lang=CCode       ' + rsHelpLang);    // syntax depends on bugfix https://bugs.freepascal.org/view.php?id=35432
       debugln('   --debug-log=SOME.LOG         ' + rsHelpDebug);
       debugln('   -h --help                    ' + rsHelpHelp);
       debugln('   --version                    ' + rsHelpVersion);
       debugln('   -g --gnome3                  ' + rsHelpRedHat);
       debugln('   --no-splash                  ' + rsHelpNoSplash);
       debugln('   --debug-sync                 ' + rsHelpDebugSync);
       debugln('   --debug-index                ' + rsHelpDebugIndex);
       debugln('   --debug-spell                ' + rsHelpDebugSpell);
       debugln('   --config-dir=PATH_to_DIR     ' + rsHelpConfig);
       debugln('   -o --open-note=PATH_to_NOTE  ' + rsHelpSingleNote);
       debugln('   --save-exit                  ' + rsHelpSaveExit);
       debugln(CmdLineErrorMsg);
       result := true;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
    // Application.OnEndSession := @OnEndSessionApp;        // ToDo : maybe this works on Windows ?
    // SetDefaultLang('es', 'locale', true);
    //color := clyellow;
    if CommandLineError() then exit;    // We will close in OnShow
    UseMainMenu := True;
    UseTrayMenu := true;
    AllowDismiss := true;
    if Application.HasOption('g', 'gnome3') then begin
        UseMainMenu := true;
        AllowDismiss := false;
        UseTrayMenu := false;
        ShowHint := False;
    end;
    {$ifdef LCLCOCOA}
    //AllowDismiss := False;
    UseMainMenu := True;
    {$endif}
    {$ifdef LCLCARBON}
    UseMainMenu := true;
    UseTrayMenu := false;
    {$endif}
    {$ifdef WINDOWS}HelpNotesPath := AppendPathDelim(ExtractFileDir(Application.ExeName));{$endif}
    {$ifdef LINUX}HelpNotesPath := '/usr/share/doc/tomboy-ng/'; {$endif}
    {$ifdef DARWIN}HelpNotesPath := ExtractFileDir(ExtractFileDir(Application.ExeName))+'/Resources/';{$endif}
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
    ButtonConfig.Width := (Width div 3);
    ButtonDismiss.Width := (Width div 3);
end;

    // Attempt to detect we are in a dark theme, sets relevent colours.
procedure TMainForm.TestDarkThemeInUse();
var
  Col : string;
   {$ifdef WINDOWS}  function WinDarkTheme : boolean;
   var
     RegValue : string='';
     Registry : TRegistry;
   begin
     Registry := TRegistry.Create;
     try
       Registry.RootKey := HKEY_CURRENT_USER;
       if Registry.OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize') then
           exit(Registry.ReadInteger('AppsUseLightTheme') = 0)
       else exit(false);
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
    if Sett.DarkTheme then begin
        debugln('Its definltly a Dark Theme');
        Sett.BackGndColour:= clBlack;
        Sett.HiColor := clDkGray;
        Sett.TextColour := clLtGray;
        Sett.TitleColour:= clTeal;
    end else begin
        Sett.BackGndColour := clCream;
        Sett.HiColor := clYellow;
        Sett.TextColour := clBlack;
        Sett.TitleColour := clBlue;
    end;

end;

procedure TMainForm.ButtonConfigClick(Sender: TObject);
begin
    Sett.Show();
end;

{procedure TMainForm.ApplicationProperties1EndSession(Sender: TObject);
var
  OutFile : TextFile;
begin
    AssignFile(OutFile, '/home/dbannon/closelogMainFormProperties.txt');
    Rewrite(OutFile);
    writeln(OutFile, 'OnEndSessionAppProperties Called');
    CloseFile(OutFile);
end; }

{ ------------- M E N U   M E T H O D S ----------------}

procedure TMainForm.FindHelpFiles();
    // Todo : this uses about 300K, 3% of extra memory, better to code up a simpler model ?
var
  NoteTitle : string;
begin
    if HelpNotes = Nil then begin
       HelpNotes := TNoteLister.Create;     // freed in OnClose event.
       HelpNotes.DebugMode := Application.HasOption('debug-index');
       HelpNotes.WorkingDir:=HelpNotesPath;
       HelpNotes.GetNotes('', true);
    end;
    HelpNotes.StartSearch();
    while HelpNotes.NextNoteTitle(NoteTitle) do
        AddMenuItem(NoteTitle, mtHelp,  @FileMenuClicked, mkHelpMenu);
end;

resourcestring
  rsSetupNotesDirFirst = 'Please setup a notes directory first';
  rsSetupSyncFirst = 'Please config sync system first';
  rsCannotFindNote = 'ERROR, cannot find ';

procedure TMainForm.FileMenuClicked(Sender : TObject);
var
    FileName : string;
begin
    case TMenuTarget(TMenuItem(Sender).Tag) of
        mtSep, mtRecent : showmessage('Oh, thats bad, should not happen');
        mtNewNote : if (Sett.NoteDirectory = '') then
                            ShowMessage(rsSetupNotesDirFirst)
                    else SearchForm.OpenNote();
        mtSearch :  if Sett.NoteDirectory = '' then
                            showmessage(rsSetupNotesDirFirst)
                    else  SearchForm.Show;

        mtAbout :    ShowAbout();
        mtSync :     if (Sett.LabelSyncRepo.Caption <> rsSyncNotConfig)
                        and (Sett.LabelSyncRepo.Caption <> '') then
                            Sett.Synchronise()
                     else showmessage(rsSetupSyncFirst);
        mtSettings : begin
                        Sett.Show;
                        //SearchForm.RecentMenu();   // ToDo : wots this for ? commented out, April 2019
                     end;
        mtTomdroid : if FormTomdroid.Visible then
                        FormTomdroid.BringToFront
                     else FormTomdroid.ShowModal;
        mtHelp :      begin
                        if HelpNotes.FileNameForTitle(TMenuItem(Sender).Caption, FileName) then
                            ShowHelpNote(FileName)
                        else showMessage(rsCannotFindNote + TMenuItem(Sender).Caption);
                    end;
        mtQuit :      close;
    end;
end;

ResourceString
  rsMenuFile = 'File';
  rsMenuRecent = 'Recent';
  rsMenuNewNote = 'New Note';
  rsMenuSearch = 'Search';
  rsMenuAbout = 'About';
  rsMenuSync = 'Synchronise';
  rsMenuSettings = 'Settings';
  rsMenuHelp = 'Help';
  rsMenuQuit = 'Quit';

procedure TMainForm.FillInFileMenus(ItsAnUpdate : boolean = false);
begin
    if not ItsAnUpdate then begin
        PopupMenuSearch := TPopupMenu.Create(Self);
        PopupMenuTray := TPopupMenu.Create(Self);
        MainMenu := TMainMenu.Create(Self);
        FileMenu := TMenuItem.Create(Self);
        RecentMenu := TMenuItem.Create(self);
        FileMenu.Caption := rsMenuFile;
        RecentMenu.Caption := rsMenuRecent;
        MainMenu.Items.Add(FileMenu);
        MainMenu.Items.Add(RecentMenu);
        TrayIcon.PopUpMenu := PopupMenuTray;
    end else begin
        PopupMenuSearch.Items.Clear;
        PopupMenuTray.Items.Clear;
        FileMenu.Clear;
        RecentMenu.Clear;
    end;
    AddMenuItem(rsMenuNewNote, mtNewNote, @FileMenuClicked, mkFileMenu);
    AddMenuItem(rsMenuSearch, mtSearch,  @FileMenuClicked, mkFileMenu);
    AddMenuItem('-', mtSep, nil, mkFileMenu);
    AddMenuItem('-', mtSep, nil, mkFileMenu);   // Recent Notes will be inserted at last Separator
    AddMenuItem(rsMenuAbout, mtAbout, @FileMenuClicked, mkFileMenu);
    if (Sett.LabelSyncRepo.Caption <> rsSyncNotConfig) then
        AddMenuItem(rsMenuSync, mtSync,  @FileMenuClicked, mkFileMenu);
    {$ifdef LINUX}
    if Sett.CheckShowTomdroid.Checked then
        AddMenuItem('Tomdroid', mtTomdroid,  @FileMenuClicked, mkFileMenu);
    {$endif}
    AddMenuItem(rsMenuSettings, mtSettings, @FileMenuClicked, mkFileMenu);
    AddMenuItem(rsMenuHelp, mtHelp,  nil, mkFileMenu);
    AddMenuItem(rsMenuQuit, mtQuit,  @FileMenuClicked, mkFileMenu);
    if ItsAnUpdate then
        SearchForm.RecentMenu;
    FindHelpFiles();
    ButtonConfig.Caption:= rsMenuSettings;
end;

procedure TMainForm.AddItemToAMenu(TheMenu : TMenu; Item : string;
                    mtTag : TMenuTarget; OC : TNotifyEvent; MenuKind : TMenuKind);
var
    MenuItem : TMenuItem;

        function InsertIntoPopup() : boolean;
        var
            X : integer;
        begin
            if not TheMenu.ClassNameIs('TPopupMenu') then
                exit(False);
            X := TheMenu.Items.Count -1;
            while X >= 0 do begin
                if TheMenu.Items[X].IsLine then begin
                    TheMenu.Items.Insert(X, MenuItem);
                    exit(True);
                end;
                dec(X);
            end;
            Result := False;
        end;

        procedure AddHelpItem();
        var
            X : Integer = 0;
        begin
            if TheMenu.ClassNameIs('TPopupMenu') then begin
                while X < TheMenu.Items.Count do begin
                    if TheMenu.Items[X].Tag = ord(mtHelp) then begin
                        TheMenu.Items[X].Add(MenuItem);
                        exit;
                    end;
                    inc(X);
                end;
            end;
            while X < FileMenu.Count do begin
                if FileMenu.Items[X].Tag = ord(mtHelp) then begin
                    FileMenu.Items[X].Add(MenuItem);
                    exit;
                end;
                inc(X);
            end;
        end;


begin
    if Item = '-' then begin
        if TheMenu.ClassNameIs('TPopupMenu') then
            TheMenu.Items.AddSeparator;
        exit;       // we only add separators to popup menus
    end;
    MenuItem := TMenuItem.Create(Self);
    MenuItem.Tag := ord(mtTag);
    MenuItem.Caption := Item;
    MenuItem.OnClick := OC;
    case MenuKind of
        mkFileMenu   : if TheMenu.ClassNameIs('TPopupMenu') then
                            TheMenu.Items.Add(MenuItem)
                            else FileMenu.Add(MenuItem);
        mkRecentMenu : if not InsertIntoPopup() then
                            RecentMenu.Add(MenuItem);
        mkHelpMenu   : AddHelpItem({TheMenu, MenuItem});
    end;
end;

procedure TMainForm.AddMenuItem(Item : string; mtTag : TMenuTarget; OC : TNotifyEvent; MenuKind : TMenuKind);
begin
    AddItemToAMenu(PopupmenuTray, Item, mtTag, OC, MenuKind);
    AddItemToAMenu(MainMenu, Item,  mtTag, OC, MenuKind);
    AddItemToAMenu(PopupMenuSearch, Item, mtTag, OC, MenuKind);
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

// will remove any recent items from all the menus.
procedure TMainForm.ClearRecentMenuItems();
var
    X : integer = 0;

    procedure RemoveRecentFromMenu(AMenu : TPopupMenu);
    begin
        X := AMenu.Items.Count;
        while X > 0 do begin
            dec(X);
            if TMenuItem(AMenu.Items[X]).Tag = ord(mtRecent) then
                AMenu.Items.Delete(X);
        end;
    end;

begin
    RecentMenu.Clear;
    RemoveRecentFromMenu(PopupMenuTray);
    RemoveRecentFromMenu(PopupMenuSearch);
end;

RESOURCESTRING
    rsAbout1 = 'This is tomboy-ng, a rewrite of Tomboy Notes using Lazarus';
    rsAbout2 = 'and FPC. While its getting close to being ready for production';
    rsAbout3 = 'use, you still need to be careful and have good backups.';
    rsAboutVer = 'Version';
    rsAboutBDate = 'Build date';
    rsAboutCPU = 'TargetCPU';
    rsAboutOperatingSystem = 'OS';


procedure TMainForm.ShowAbout();
var
        S1, S2, S3, S4, S5, S6 : string;
begin
        S1 := rsAbout1 + #10;
        S2 := rsAbout2 + #10;
        S3 := rsAbout3 + #10;
        S4 := rsAboutVer + ' ' + Version_String + #10;
        S5 := rsAboutBDate + ' ' + {$i %DATE%} + ' ' + rsAboutCPU + ' ' + {$i %FPCTARGETCPU%} + '  '
                + rsAboutOperatingSystem + ' ' + {$i %FPCTARGETOS%};
        // That may return, eg "Build date 2019/02/28 TargetCPU x86_64 OS Linux Mate"
        // or, maybe "Build date 2019/03/19 TargetCPU i386 OS Win32"
        S6 := #10;
        {$ifdef LCLCOCOA}S6 := S6 + ' 64bit Cocoa ' + rsAboutVer;{$endif}
        {$ifdef LCLQT5}S6 := S6 + ' QT5' + rsAboutVer; {$endif}
        S6 := S6 + ' ' + GetEnvironmentVariable('XDG_CURRENT_DESKTOP');
        Showmessage(S1 + S2 + S3 + S4 + S5 + S6);
end;

end.

