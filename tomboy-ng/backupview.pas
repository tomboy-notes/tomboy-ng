unit BackupView;

{ A unit to manage the Backup capability of tomboy-ng.
    It allows viewing, deleting or restoring a backed up note. Note, in Tomboy
    speak, Backup means backup of deleted or overwritten by sync process.
}

{ History
    2018/07/03  Finished the recver a backup note code
    2018/08/14  Update the last-metadata-change-date instead of last-change-date
                when restoring a Backup file. See Sync spec.
    2018/08/16  We now update both last-metadata-change-date AND last-change-date
                when restoring a backup file.
    2018/08/27  Now change the ID of a deleted (but not overwritten) Note to avoid Sync issues
}

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
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
        procedure Memo1Change(Sender: TObject);
    private
        ExistsInRepo : boolean;
        NeedUpDate : boolean;
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
    MainUnit,   // For SingleNoteMode()
    SearchUnit; // access the notelister object

procedure TFormBackupView.Memo1Change(Sender: TObject);
begin

end;

procedure TFormBackupView.FormShow(Sender: TObject);
begin
    NotesChanged := false;
    ExistsInRepo := false;
    NeedUpDate := False;
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
    MainUnit.MainForm.SingleNoteMode(Sett.NoteDirectory + 'Backup' + PathDelim + FileName, False, True);
end;

// OK, overwriting an existing file is not an issue (as long as its not open).
// However, if we are looking at a note that was deleted, it might be listed in
// the Local Manifest as a deleted file. That will confuse the next sync.
// So, lets just give those sort of notes a new ID.
procedure TFormBackupView.ButtonRecoverClick(Sender: TObject);
var
    AForm : TForm;
    InString : string;
    InFile, OutFile: TextFile;
    NewFName : string;
    GUID : TGUID;
begin
    if ExistsInRepo then
        if IDYES <> Application.MessageBox('Overwrite newer version of that note', 'Note already in Repo',
                    MB_ICONQUESTION + MB_YESNO) then
            exit();
    if SearchForm.NoteLister.IsThisNoteOpen(FileName, AForm) then begin
        showmessage('You have that note open, please close and try again');
        exit();
    end;
    if ExistsInRepo then begin
        if not RenameFileUTF8(Sett.NoteDirectory + FileName, Sett.NoteDirectory + 'Backup'
                    + PathDelim + FileName + 'TMP') then begin
            showmessage('Copying orig to Backup directory failed');
            exit;
        end;
    end else begin
        // Give the note a new name so that no issues about it being in delete section of Manifest.
        CreateGUID(GUID);
        NewFName := copy(GUIDToString(GUID), 2, 36) + '.note';
        if RenameFile(Sett.NoteDirectory + 'Backup' + PathDelim + Filename,
                Sett.NoteDirectory + 'Backup' + PathDelim + NewFName) then
            FileName := NewFName
        else
          Showmessage('ERROR, could not rename Backup File ' + FileName);
    end;
    // OK, if to here, user really wants it back, no reason why not.
    AssignFile(InFile, Sett.NoteDirectory + 'Backup' + PathDelim + Filename);
    AssignFile(OutFile, Sett.NoteDirectory + Filename);
    try
        try
            Reset(InFile);
            Rewrite(OutFile);
            while not eof(InFile) do begin
                readln(InFile, InString);
                if (Pos('<last-metadata-change-date>', InString) > 0) or
                        (Pos('<last-change-date>', InString) > 0)  then begin
                    if (Pos('<last-metadata-change-date>', InString) > 0) then
                        writeln(OutFile, ' <last-metadata-change-date>' +  Sett.GetLocalTime() + '</last-metadata-change-date>')
                    else writeln(OutFile, ' <last-change-date>' +  Sett.GetLocalTime() + '</last-change-date>');
                end else writeln(OutFile, InString);
            end;
        finally
            CloseFile(OutFile);
            CloseFile(InFile);
        end;
        If ExistsInRepo then
            if not RenameFileUTF8(Sett.NoteDirectory + 'Backup' + PathDelim + FileName + 'TMP',
                    Sett.NoteDirectory + 'Backup' + PathDelim + FileName) then begin
                showmessage('Failed to move temp backup file');
            end;
        NeedUpDate := false;
    except
        on E: EInOutError do
            showmessage('File handling error occurred. Details: ' + E.Message);
    end;
    Memo1.Append('OK, File recovered. You may need to do a Refresh (or restart)');
end;

procedure TFormBackupView.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    if NeedUpDate then begin
        SearchForm.RecentMenu();
        Sett.ButtonShowBackUp.click;
    end;
end;

end.

