unit RollBack;

{   Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------

    This form will allow a user to roll back an open note to either the backup
    made when it was opened or a backup made if the Title was changed.

    It will close the open note, swap the files as required, advise Note_lister
    and reopen.  It can toggle, repeatedly switch between.  But if end user
    changes the title, it (obviously) writes a new backup, probably not what
    they want but I cannot determine their intentions.
}

{$mode objfpc}{$H+}

interface

uses
        Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
		StdCtrls, EditBox;

type

		{ TFormRollBack }

        TFormRollBack = class(TForm)
				Label1: TLabel;
				LabelOpnTitle: TLabel;
				LabelttlTitle: TLabel;
				LabelOpn: TLabel;
				Labelttl: TLabel;
				SpeedCancel: TSpeedButton;
				SpeedRollToOpen: TSpeedButton;
				SpeedRollToTitle: TSpeedButton;
				procedure FormCreate(Sender: TObject);
				procedure FormShow(Sender: TObject);
				procedure SpeedCancelClick(Sender: TObject);
				procedure SpeedRollToOpenClick(Sender: TObject);
				procedure SpeedRollToTitleClick(Sender: TObject);
        private
				function GetNoteTitle(FullFileName: ANSIString): ANSIString;
				procedure RollBackNote(FileType, Title: string);

        public
                NoteFileName : string;  // Give me ID, Filename or FullFilename before show.
                { The EditBox that opened this form, only it can close this form and
                this form can close that EditBox }
                ShownBy : TForm;
        end;

var
        FormRollBack: TFormRollBack;

implementation

{$R *.lfm}

{ TFormRollBack }

uses LazFileUtils, SyncUtils, Settings, LazLogger, FileUtil, SearchUnit,
        laz2_DOM, laz2_XMLRead,
        ResourceStr;

procedure TFormRollBack.FormCreate(Sender: TObject);
begin
    Label1.Caption := rsRollBackIntro;
end;

procedure TFormRollBack.FormShow(Sender: TObject);
var
    ttlName, opnName : string;
    LCDstr, ErrorStr : string;
begin
    SpeedRollToOpen.Enabled:= False;
    SpeedRollToTitle.Enabled:= False;
    LabelttlTitle.Caption := '';
    ttlName := ExtractFileNameOnly(NoteFileName);
    // following must match name mangling rules from TSearchForm.BackupNote()
    opnName :=  Sett.NoteDirectory + 'Backup' + PathDelim + copy(ttlName, 1, 32) + '-opn.note';
    ttlName :=  Sett.NoteDirectory + 'Backup' + PathDelim + copy(ttlName, 1, 32) + '-ttl.note';
    if FileExistsUTF8(opnName) then begin                       // should always be there ?
        LCDStr := GetNoteLastChangeSt(opnName, ErrorStr);
        if LCDStr = '' then
            LabelOpn.Caption := ErrorStr
        else begin
            LCDStr[11] := ' ';
            LabelOpn.Caption := rsContentDated + ' ' + copy(LCDStr, 1, 16);
            LabelOpnTitle.Caption := GetNoteTitle(opnName);
            SpeedRollToOpen.Enabled := True;
        end;
	end;
    if FileExistsUTF8(ttlName) then begin
        LCDStr := GetNoteLastChangeSt(ttlName, ErrorStr);
        if LCDStr = '' then
            Labelttl.Caption := ErrorStr
        else begin
            LCDStr[11] := ' ';
            Labelttl.Caption := rsContentDated + ' ' + copy(LCDStr, 1, 16);
            LabelttlTitle.Caption := GetNoteTitle(ttlName);
            SpeedRollToTitle.Enabled := True;
        end;
	end else Labelttl.Caption := rsNotAvailable;
end;

procedure TFormRollBack.RollBackNote(FileType, Title : string);
var
    FFName : string;
    LCDStr, ErrorStr : string;
begin
    FFName := Sett.NoteDirectory + 'Backup' + PathDelim
                + copy(ExtractFileNameOnly(NoteFileName), 1, 32) + FileType + '.note';
    if not (RenameFile(FFName, FFName+'-temp') and fileexistsUTF8(FFName+'-temp')) then begin
        debugln('ERROR, failed to move : ' + FFName);
        exit;
    end;
    TEditBoxForm(ShownBy).SetReadOnly(False);               // Prevent a resave
    ShownBy.Close;
    // The editBox is a bit slow closing, make sure its disregarded. We did save before opening this form.
    SearchForm.NoteClosing(ExtractFileNameOnly(NoteFileName));                  // Maybe not necessary cos we call UpdateList below ?
    RenameFileUTF8(NoteFileName, FFName);
    RenameFileUTF8(FFName+'-temp', NoteFileName);
    LCDStr := GetNoteLastChangeSt(NoteFileName, ErrorStr);                      // Hmm, not checking for errors ?
    SearchForm.UpdateList(Title, LCDStr, NoteFileName, nil);
    SearchForm.OpenNote(Title, NoteFileName, '', False);
    close;
end;

procedure TFormRollBack.SpeedRollToOpenClick(Sender: TObject);
begin
    RollBackNote('-opn', LabelopnTitle.Caption);
end;

procedure TFormRollBack.SpeedRollToTitleClick(Sender: TObject);
begin
    RollBackNote('-ttl', LabelttlTitle.Caption);
end;

procedure TFormRollBack.SpeedCancelClick(Sender: TObject);
begin
        close;
end;
function TFormRollBack.GetNoteTitle(FullFileName : ANSIString) : ANSIString;
var
        Doc : TXMLDocument;
        Node : TDOMNode;
begin
    Result := 'ERROR, Title Not Found';
    if FileExistsUTF8(FullFileName) then begin
        try
            try
                ReadXMLFile(Doc, FullFileName);
                Node := Doc.DocumentElement.FindNode('title');
                Result := Node.FirstChild.NodeValue;
            except on EXMLReadError do
                Result := 'Note has no Title ';
                on EAccessViolation do
                    Result := 'Access Violation ' + FullFileName;
            end;
        finally
            Doc.free;
        end;
    end;
end;


end.

