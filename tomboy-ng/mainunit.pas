unit Mainunit;

{$mode objfpc}{$H+}

{   HISTORY
    2018/05/12  Extensive changes - MainUnit is now just that.
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
        ImageConfigCross1: TImage;
        ImageConfigTick1: TImage;
        ImageNotesDirCross: TImage;
        ImageConfigTick: TImage;
        ImageConfigCross: TImage;
        ImageNotesDirCross1: TImage;
        ImageNotesDirTick: TImage;
        ImageNotesDirTick1: TImage;
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
        procedure CheckStatus();
    public

    end;

var
    MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

uses settings,
    SearchUnit,
    {$ifdef LINUX}fix_gtk_clip,{$endif}   // Stop linux clearing clipboard on app exit.
    uAppIsRunning;

procedure TMainForm.FormShow(Sender: TObject);
begin
    Left := 10;
    Top := 10;
    if AlreadyRunning() then begin
        showmessage('Another instance of tomboy-ng appears to be running. Will exit.');
        close();
    end else begin
//        {$ifdef DARWIN}                  // Only Mac users want to see this
        MMFile.Visible := True;
        MMRecent.Visible := True;
//        {$endif}
        Label7.Caption:='';
        Label8.Caption := '';
        CheckStatus();
        {$ifdef LCLCOCOA}
            Label7.Caption := 'Sadely, on the Mac, I cannot';
            Label8.Caption := 'let you dismiss this window';
            ButtonDismiss.Enabled := False;
            Label7.Hint:='are you trying to shut me down ? Dave ?';
            Label8.Hint := Label7.Hint;
        {$endif}
    end;
end;


procedure TMainForm.CheckStatus();
begin
     ImageConfigCross.Left := ImageConfigTick.Left;
     ImageNotesDirCross.Left := ImageNotesDirTick.Left;
     ImageConfigTick.Visible := Sett.HaveConfig;
     ImageConfigCross.Visible := not ImageConfigTick.Visible;
     ImageNotesDirTick.Visible := (Sett.NoteDirectory <> '');
     ImageNotesDirCross.Visible := not ImageNotesDirTick.Visible;
     if (ImageConfigTick.Visible and ImageNotesDirTick.Visible) then begin
        ButtonDismiss.Enabled := True;
        SearchForm.IndexNotes();
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
    SearchForm.OpenNote();
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
        S1 := 'This is v0.15 of tomboy-ng, a rewrite of Tomboy Notes'#10;
        S2 := 'using Lazarus and FPC. While its getting close to being'#10;
        S3 := 'ready for production use, you still need to be careful and'#10;
        S4 := 'have good backups.'#10;
        S5 := 'Build date ' + {$i %DATE%} + '  TargetCPU ' + {$i %FPCTARGETCPU%} + '  OS ' + {$i %FPCTARGETOS%};
        S6 := '';
        {$ifdef LCLCOCOA}S6 := ' 64bit Cocoa Version';{$endif}
        Showmessage(S1 + S2 + S3 + S4 + S5 + S6);
end;

end.

