unit BackupView;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ExtCtrls;

type

    { TFormBackupView }

    TFormBackupView = class(TForm)
        ButtonOpen: TButton;
        ButtonRecover: TButton;
        ButtonDelete: TButton;
        ButtonOK: TButton;
        Memo1: TMemo;
        Panel1: TPanel;
        procedure ButtonDeleteClick(Sender: TObject);
        procedure ButtonOpenClick(Sender: TObject);
        procedure ButtonRecoverClick(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure Memo1Change(Sender: TObject);
    private
        ExistsInRepo : boolean;
    public
        FileName : string;
        NoteTitle : string;
        NotesChanged : boolean;
    end;

var
    FormBackupView: TFormBackupView;

implementation

{$R *.lfm}

{ TFormBackupView }

uses  settings, LazFileUtils, LCLType,
    MainUnit;   // For SingleNoteMode()

procedure TFormBackupView.Memo1Change(Sender: TObject);
begin

end;

procedure TFormBackupView.FormShow(Sender: TObject);
begin
    NotesChanged := false;
    ExistsInRepo := false;
    Memo1.Clear;
    Memo1.Append('Title :');
    Memo1.Append(NoteTitle);
    Memo1.Append('Filename :');
    Memo1.Append(FileName);
    if FileExistsUTF8(Sett.NoteDirectory + FileName) then begin
        Memo1.Append('A newer version exists in main repo');
        ExistsInRepo := True;
    end else
        Memo1.Append('Not present in main repo');
end;

procedure TFormBackupView.ButtonDeleteClick(Sender: TObject);
begin
    if DeleteFileUTF8(Sett.NoteDirectory + 'Backup' + PathDelim + FileName) then
        NotesChanged := True
    else Showmessage('Cannot delete ' + Sett.NoteDirectory + 'Backup' + PathDelim + FileName);
    close;
end;

procedure TFormBackupView.ButtonOpenClick(Sender: TObject);
begin
    MainUnit.MainForm.SingleNoteMode(Sett.NoteDirectory + 'Backup' + PathDelim + FileName, False);
end;

procedure TFormBackupView.ButtonRecoverClick(Sender: TObject);
begin
    if ExistsInRepo then
        if IDYES <> Application.MessageBox('Overwrite newer version of that note', 'Note already in Repo',
            MB_ICONQUESTION + MB_YESNO) then
            exit();
    showmessage('sorry, not functional yet.');
    exit();

    //if ExistsInRepo then

    { This needs to move the file from Backup back into the main repo AND
    update the notes last change date.
    Open file, read, line by line until we ge to last change date, insert new data, continue.

    if note already exists in repo, make sure its not open, make a copy in Backup called Temp.note.
    Open backup file, read, line by line until we ge to last change date, insert new data, continue.
    if it was already exist, remove backup, rename Temp.note to actual name.
    notify Note_Lister of change.AlterNote() if ExistInRepo, else Note_Lister.AddNote()
    if note turned out to be invalid, that is we did not detect last change, restore.

    }
    //    copyfile(
   //     SearchForm.NoteLister.IsThisNoteOpen(FullFileName, TheForm)
end;

end.

