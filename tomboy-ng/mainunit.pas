unit Mainunit;

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




    CommandLine Switches
    --gnome3    Turns on MainMenu, TrayMenu off and prevents dismmiss of this
    -g
    --debug-sync Turn on Verbose mode during sync

    --debug-index Verbose mode when reading the notes directory.

    --config-dir=<some_dir> Directory to keep config and sync manifest in.

    -o note_fullfilename
    --open=<note_fullfilename> Opens, in standalone mode, a note. Does not check
                to see if another copy of tomboy-ng is open but will prevent a
                normal startup of tomboy-ng. Won't interfere with an existing copy
                however.

    --help -h   Shows and exits (not implemented)
                something to divert debug msg to a file ??
                something to do more debugging ?

}
interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
    StdCtrls;

type

    { TMainForm }

    TMainForm = class(TForm)
        ButtonDismiss: TButton;
        ButtonConfig: TButton;
        ImageSpellCross: TImage;
        ImageSpellTick: TImage;
        ImageNotesDirCross: TImage;
        ImageConfigTick: TImage;
        ImageConfigCross: TImage;
        ImageSyncCross: TImage;
        ImageNotesDirTick: TImage;
        ImageSyncTick: TImage;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        Label7: TLabel;
        Label8: TLabel;
        MainMenu1: TMainMenu;
        MMRecent1: TMenuItem;
        MMRecent8: TMenuItem;
        MMRecent9: TMenuItem;
        MMRecent10: TMenuItem;
        MMRecent2: TMenuItem;
        MMRecent3: TMenuItem;
        MMRecent4: TMenuItem;
        MMRecent5: TMenuItem;
        MMRecent6: TMenuItem;
        MMRecent7: TMenuItem;
        MMRecent: TMenuItem;
        MMSync: TMenuItem;
        MMSettings: TMenuItem;
        MMQuit: TMenuItem;
        MenuItem15: TMenuItem;
        MenuItem3: TMenuItem;
        MenuItem4: TMenuItem;
        TrayMenuItemSettings: TMenuItem;
        MenuQuit: TMenuItem;
        TrayMenuSynchronise: TMenuItem;
        MMAbout: TMenuItem;
        MMFile: TMenuItem;
        MMNewNote: TMenuItem;
        MMSearch: TMenuItem;
        PopupMenuTray: TPopupMenu;
        TrayIcon: TTrayIcon;
        TrayMenSearch: TMenuItem;
        TrayMenuAbout: TMenuItem;
        TrayMenuNew: TMenuItem;
        TrayMenuRecent1: TMenuItem;
        TrayMenuRecent10: TMenuItem;
        TrayMenuRecent2: TMenuItem;
        TrayMenuRecent3: TMenuItem;
        TrayMenuRecent4: TMenuItem;
        TrayMenuRecent5: TMenuItem;
        TrayMenuRecent6: TMenuItem;
        TrayMenuRecent7: TMenuItem;
        TrayMenuRecent8: TMenuItem;
        TrayMenuRecent9: TMenuItem;
        procedure ButtonConfigClick(Sender: TObject);
        procedure ButtonDismissClick(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure FormClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormShow(Sender: TObject);
        procedure MMAboutClick(Sender: TObject);
        procedure MMNewNoteClick(Sender: TObject);
        procedure MMQuitClick(Sender: TObject);
        procedure MMRecent1Click(Sender: TObject);
        procedure MMSearchClick(Sender: TObject);
        procedure MMSettingsClick(Sender: TObject);
        procedure MMSyncClick(Sender: TObject);
        procedure TrayIconClick(Sender: TObject);
    private
        CmdLineErrorMsg : string;
        AllowDismiss : boolean; // Allow user to dismiss (ie hide) the opening window.
        function CommandLineError() : boolean;
    public
        UseTrayMenu : boolean;
        UseMainMenu : boolean;
        procedure CheckStatus();
        procedure SingleNoteMode(FullFileName: string; CloseOnFinish : boolean = True);
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
    Editbox;    // Used only in SingleNoteMode

procedure TMainForm.SingleNoteMode(FullFileName : string; CloseOnFinish : boolean = True);
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
            EBox.ShowModal;
            except on E: Exception do debugln('!!! EXCEPTION - ' + E.Message);
            end;
        finally
            try
            FreeandNil(EBox);
            except on E: Exception do debugln('!!! EXCEPTION - What ? no FreeAndNil ?' + E.Message);
            end;
        end;
    end else
        DebugLn('Sorry, cannot find that directory [' + ExtractFilePath(FullFileName) + ']');
    if CloseOnFinish then Close;      // we also use singlenotemode when looking at backup files
end;

procedure TMainForm.FormShow(Sender: TObject);
// WARNING - the options here MUST match the options list in CommandLineError()
var
    I: Integer;
    Params : TStringList;   { TODO : Document this in http://wiki.freepascal.org/Command_line_parameters_and_environment_variables }
    LongOpts : array [1..5] of string = ('gnome3','debug-sync', 'debug-index', 'config-dir:','open-note:');
begin
    if CmdLineErrorMsg <> '' then begin
        close;    // cannot close in OnCreate();
        exit;       // otherwise we execute rest of this method before closing.
    end;
    Left := 10;
    Top := 40;
    if Application.HasOption('o', 'open-note') then begin
        SingleNoteMode(Application.GetOptionValue('o', 'open-note'));
        exit();
    end;
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
        showmessage('Another instance of tomboy-ng appears to be running. Will exit.');
        close();
    end else begin
        if UseMainMenu then begin                  // Only Mac users want to see this
            MMFile.Visible := True;
            MMRecent.Visible := True;
        end;
        Label7.Caption:='';
        Label8.Caption := '';
        CheckStatus();
        SearchForm.IndexNotes(); // also calls Checkstatus but safe to call anytime
        if not AllowDismiss then begin
            Label7.Caption := 'Sadly, on this OS, I cannot';
            Label8.Caption := 'let you dismiss this window';
            Label7.Hint:='are you trying to shut me down ? Dave ?';
            Label8.Hint := Label7.Hint;
        end;
    end;
end;


procedure TMainForm.CheckStatus();
begin
     ImageConfigCross.Left := ImageConfigTick.Left;
     ImageConfigTick.Visible := Sett.HaveConfig;
     ImageConfigCross.Visible := not ImageConfigTick.Visible;

     ImageNotesDirCross.Left := ImageNotesDirTick.Left;
     ImageNotesDirTick.Visible := (Sett.NoteDirectory <> '');
     ImageNotesDirCross.Visible := not ImageNotesDirTick.Visible;

     ImageSpellCross.Left := ImageSpellTick.Left;
     ImageSpellTick.Visible := Sett.SpellConfig;
     ImageSpellCross.Visible := not ImageSpellTick.Visible;

     ImageSyncCross.Left := ImageSyncTick.Left;
     ImageSyncTick.Visible := (Sett.RemoteRepo <> '');
     ImageSyncCross.Visible := not ImageSyncTick.Visible;

     if (ImageConfigTick.Visible and ImageNotesDirTick.Visible) then begin
        ButtonDismiss.Enabled := AllowDismiss;
        if UseTrayMenu then
            TrayIcon.Show;

     end;
end;

procedure TMainForm.ButtonDismissClick(Sender: TObject);
begin
    hide();
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin

end;

procedure TMainForm.FormClick(Sender: TObject);
begin
    PopupMenuTray.PopUp();
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
    {$ifdef LINUX}
  c: PGtkClipboard;
  {$endif}
  t: string;
begin
    {$ifdef LINUX}
    c := gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
    t := Clipboard.AsText;
    gtk_clipboard_set_text(c, PChar(t), Length(t));
    gtk_clipboard_store(c);
    {$endif}
end;

function TMainForm.CommandLineError() : boolean;
// WARNING - the options here MUST match the options list in FormShow()
begin
    Result := false;
    CmdLineErrorMsg := Application.CheckOptions('hgo:', 'help gnome3 open-note: debug-sync debug-index config-dir:');
    if Application.HasOption('h', 'help') then
        CmdLineErrorMsg := 'Show Help Message';
    if CmdLineErrorMsg <> '' then begin
       debugln('Usage - ');
       {$ifdef DARWIN}
       debugln('eg   open tomboy-ng.app');
       debugln('eg   open tomboy-ng.app --args -o Note.txt|.note');
       {$endif}
       debugln('   -h --help                    Show this help and exit.');
       debugln('   -g --gnome3                  Run in (non ubuntu) gnome3 mode, no Tray Icon');
       debugln('   --debug-sync                 Show whats happening during sync process');
       debugln('   --debug-index                Show whats happening during initial index of notes');
       debugln('   --config-dir=PATH_to_DIR     Create or use an alternative config');
       debugln('   -o --open-note=PATH_to_NOTE  Open indicated note, switch is optional');
       debugln(CmdLineErrorMsg);
       result := true;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
    if CommandLineError() then exit;    // We will close in OnShow
    UseMainMenu := false;
    UseTrayMenu := true;
    AllowDismiss := true;
    if Application.HasOption('g', 'gnome3') then begin
        UseMainMenu := true;
        AllowDismiss := false;
        UseTrayMenu := false;
    end;
    {$ifdef LCLCOCOA}
    AllowDismiss := False;
    UseMainMenu := True;
    {$endif}
    {$ifdef LCLCARBON}
    UseMainMenu := true;
    UseTrayMenu := false;
    {$endif}
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
     if ssCtrl in Shift then begin
       if key = ord('N') then begin
         MMNewNoteClick(self);
         Key := 0;
         exit();
       end;
     end;
end;



procedure TMainForm.ButtonConfigClick(Sender: TObject);
begin
    Sett.Show();
end;

{ ------------- M E N U   M E T H O D S ----------------}

{ note these methods handle the Main Menu (Mac only ?)
  and the TrayIcon popup menu }

procedure TMainForm.MMNewNoteClick(Sender: TObject);
begin
    if (Sett.NoteDirectory = '') then
        ShowMessage('Please setup a notes directory first')
    else SearchForm.OpenNote();
end;

procedure TMainForm.MMSettingsClick(Sender: TObject);
begin
    Sett.Show;
    SearchForm.RecentMenu();
end;

procedure TMainForm.MMSyncClick(Sender: TObject);
begin
    Sett.Synchronise();
end;

procedure TMainForm.TrayIconClick(Sender: TObject);
begin
    PopupMenuTray.PopUp();
end;



procedure TMainForm.MMQuitClick(Sender: TObject);
begin
     close;
end;

// This procedure responds to ALL recent note menu clicks !
procedure TMainForm.MMRecent1Click(Sender: TObject);
begin
 	if TMenuItem(Sender).Caption <> SearchForm.MenuEmpty then
 		SearchForm.OpenNote(TMenuItem(Sender).Caption);
end;

procedure TMainForm.MMSearchClick(Sender: TObject);
begin
    if Sett.NoteDirectory = '' then
        showmessage('You have not set a notes directory. Please click Settings')
    else  SearchForm.Show;
end;

procedure TMainForm.MMAboutClick(Sender: TObject);
var
        S1, S2, S3, S4, S5, S6 : string;
begin
        S1 := 'This is v0.16a of tomboy-ng, a rewrite of Tomboy Notes'#10;
        S2 := 'using Lazarus and FPC. While its getting close to being'#10;
        S3 := 'ready for production use, you still need to be careful and'#10;
        S4 := 'have good backups.'#10;
        S5 := 'Build date ' + {$i %DATE%} + '  TargetCPU ' + {$i %FPCTARGETCPU%} + '  OS ' + {$i %FPCTARGETOS%};
        S6 := '';
        {$ifdef LCLCOCOA}S6 := ' 64bit Cocoa Version';{$endif}
        S6 := S6 + ' ' + GetEnvironmentVariable('XDG_CURRENT_DESKTOP');
        Showmessage(S1 + S2 + S3 + S4 + S5 + S6);
end;

end.

