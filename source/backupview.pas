unit BackupView;

{   Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------

    A unit to manage the Backup capability of tomboy-ng.
    It allows viewing, deleting or restoring a backed up note. Note, in Tomboy
    speak, Backup means backup of deleted or overwritten by sync process.

History
    2018/07/03  Finished the recver a backup note code
    2018/08/14  Update the last-metadata-change-date instead of last-change-date
                when restoring a Backup file. See Sync spec.
    2018/08/16  We now update both last-metadata-change-date AND last-change-date
                when restoring a backup file.
    2018/08/27  Now change the ID of a deleted (but not overwritten) Note to avoid Sync issues
    2019/05/19  Display strings all (?) moved to resourcestrings
    2020/05/11  Restructure to do the backup note display and fiddling here.
    2020/07/25  Tweak layout and select first note if there is one shown in the list.
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ExtCtrls, Note_Lister, ResourceStr;

type

    { TFormBackupView }

    TFormBackupView = class(TForm)
        ButtonOpen: TButton;
        ButtonRecover: TButton;
        ButtonDelete: TButton;
        ButtonOK: TButton;
        ListBox1: TListBox;
        Memo1: TMemo;
        Panel1: TPanel;
        procedure ButtonDeleteClick(Sender: TObject);
        procedure ButtonOpenClick(Sender: TObject);
        procedure ButtonRecoverClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
    private
  	    BUNoteLister : TNoteLister;
        //ExistsInRepo : boolean;
        //NeedUpDate : boolean;
        function RefreshBackup(): integer;
        procedure UpdateDetails(ID: string);
    public
        //FileName : string;
        //NoteTitle : string;
        //NotesChanged : boolean;
    end;

var
    FormBackupView: TFormBackupView;

implementation

{$R *.lfm}

{ TFormBackupView }

uses  settings, LazFileUtils, LCLType,
    MainUnit,   // For SingleNoteMode()
    tb_utils,
    SearchUnit; // access the notelister object

procedure TFormBackupView.FormCreate(Sender: TObject);
begin
    ButtonOpen.Enabled := False;
    ButtonRecover.Enabled := False;
    ButtonDelete.Enabled :=  False;
    BUNoteLister := nil;
end;

procedure TFormBackupView.FormDestroy(Sender: TObject);
begin
    BUNoteLister.Free;
end;

procedure TFormBackupView.FormShow(Sender: TObject);
begin
    if RefreshBackup() = 0 then
        Memo1.Append('We found no backup notes')
    else
        ListBox1.ItemIndex:=0;
end;

function TFormBackupView.RefreshBackup() : integer;
begin
    ListBox1.Clear;
    Memo1.Clear;
    if BUNoteLister <> nil then
        BUNoteLister.free;
    BUNoteLister := TNoteLister.Create;
    BUNoteLister.WorkingDir:= sett.NoteDirectory + 'Backup' + PathDelim;
    BUNoteLister.IndexNotes();
    BUNoteLister.LoadStrings(ListBox1.Items);
    result := BUNoteLister.GetNoteCount();
end;

procedure TFormBackUpView.UpdateDetails(ID : string);
begin
    Memo1.Clear;
    Memo1.Append('Title :');
    Memo1.Append(BUNoteLister.GetTitle(ID));
    Memo1.Append('Filename :');
    Memo1.Append(ID);
    Memo1.Append('Last change ' + BUNoteLister.GetLastChangeDate(ID));
    if FileExistsUTF8(Sett.NoteDirectory + ID) then begin
        Memo1.Append(rsNewerVersionExits);
        //ExistsInRepo := True;
    end else
        Memo1.Append(rsNotPresent);
    Memo1.Append(inttostr(ListBox1.SelCount) + ' notes selected');
    ButtonOpen.Enabled := (ListBox1.SelCount = 1);
    ButtonRecover.Enabled := (ListBox1.SelCount = 1);
    ButtonDelete.Enabled :=  (ListBox1.SelCount > 0);
end;

procedure TFormBackupView.ListBox1SelectionChange(Sender: TObject; User: boolean);
begin
    UpdateDetails(string(ListBox1.Items.Objects[ListBox1.ItemIndex]));
end;

procedure TFormBackupView.ButtonDeleteClick(Sender: TObject);
var
    Index : integer = 0;
begin
    while Index < ListBox1.Count do begin
        if ListBox1.Selected[Index] then
            if not DeleteFileUTF8(Sett.NoteDirectory + 'Backup' + PathDelim
                + string(ListBox1.Items.Objects[Index])) then
                    Showmessage(rsCannotDelete + Sett.NoteDirectory + 'Backup' + PathDelim
                        + string(ListBox1.Items.Objects[Index]));
        inc(Index);
    end;
    RefreshBackup();
    Memo1.Append(rsNotesDeleted);
end;

procedure TFormBackupView.ButtonOpenClick(Sender: TObject);
// Note : we only allow one at a time, multiselect will disable View
begin
    MainUnit.MainForm.SingleNoteMode(Sett.NoteDirectory + 'Backup' + PathDelim
            + string(ListBox1.Items.Objects[ListBox1.ItemIndex]), False, True);
end;



// OK, overwriting an existing file is not an issue (as long as its not open).
// However, if we are looking at a note that was deleted, it might be listed in
// the Local Manifest as a deleted file. That will confuse the next sync.
// So, lets just give those sort of notes a new ID.
procedure TFormBackupView.ButtonRecoverClick(Sender: TObject);
// Note : we only allow one at a time, multiselect will disable Recover
var
    AForm : TForm;
    InString : string;
    InFile, OutFile: TextFile;
    NewFName : string;
    GUID : TGUID;
    FileName : string;
    ExistsInRepo : boolean;
begin
    FileName := string(ListBox1.Items.Objects[ListBox1.ItemIndex]);
    ExistsInRepo := FileExistsUTF8(Sett.NoteDirectory + FileName);
    if ExistsInRepo then
        if IDYES <> Application.MessageBox(pchar(rsOverwriteNote), pchar(rsNoteAlreadyInRepo),
                    MB_ICONQUESTION + MB_YESNO) then
            exit();
    if TheMainNoteLister.IsThisNoteOpen(FileName, AForm) then begin
        showmessage(rsNoteOpen);
        exit();
    end;
    if ExistsInRepo then begin
        if not RenameFileUTF8(Sett.NoteDirectory + FileName, Sett.NoteDirectory + 'Backup'     // Move target note to backup with temp name
                    + PathDelim + FileName + 'TMP') then begin
            showmessage(rsCopyFailed);
            exit;
        end;
    end else begin
        // Give the a non existing note a new name so that no issues about it being in delete section of Manifest.
        CreateGUID(GUID);
        NewFName := copy(GUIDToString(GUID), 2, 36) + '.note';
        if RenameFile(Sett.NoteDirectory + 'Backup' + PathDelim + Filename,
                Sett.NoteDirectory + 'Backup' + PathDelim + NewFName) then
            FileName := NewFName
        else
          Showmessage(rsRenameFailed + ' ' + FileName);
    end;
    // OK, if to here, user really wants it back, no reason why not.
    AssignFile(InFile, Sett.NoteDirectory + 'Backup' + PathDelim + Filename);      // We'll copy and update dates at same time, wether exists or not
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
                        writeln(OutFile, ' <last-metadata-change-date>' +  TB_GetLocalTime() + '</last-metadata-change-date>')
                    else writeln(OutFile, ' <last-change-date>' +  TB_GetLocalTime() + '</last-change-date>');
                end else writeln(OutFile, InString);
            end;
        finally
            CloseFile(OutFile);
            CloseFile(InFile);
        end;
        TheMainNoteLister.IndexThisNote(copy(GUIDToString(GUID), 2, 36));       // why GUID, not 'Filename' ?
        // OK, lets deal with the copy of target that we put in backup.
        If ExistsInRepo then
            if not RenameFileUTF8(Sett.NoteDirectory + 'Backup' + PathDelim + FileName + 'TMP',
                    Sett.NoteDirectory + 'Backup' + PathDelim + FileName) then begin
                showmessage('Failed to move temp backup file');
            end;
        //NeedUpDate := True;
    except
        on E: EInOutError do
            showmessage('File handling error occurred. Details: ' + E.Message);
    end;
     // reindexing triggered from FormClose
    RefreshBackup(); Memo1.Append(rsRecoverOK);
end;


procedure TFormBackupView.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
        SearchForm.RefreshMenus(mkRecentMenu);
end;



end.

