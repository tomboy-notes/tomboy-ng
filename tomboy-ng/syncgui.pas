unit SyncGUI;
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

{	History
	2017/12/06	Marked FileSync debug mode off to quieten console output a little
}

{$mode objfpc}{$H+}




interface

uses
		Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
		StdCtrls, Grids, TB_Sync;

type

		{ TFormSync }

  TFormSync = class(TForm)
				ButtonSave: TButton;
				ButtonCancel: TButton;
				ButtonOK: TButton;
				Label1: TLabel;
				Label2: TLabel;
				Memo1: TMemo;
				Panel1: TPanel;
				Panel2: TPanel;
				Panel3: TPanel;
				Splitter3: TSplitter;
				StringGridReport: TStringGrid;
				procedure ButtonCancelClick(Sender: TObject);
				procedure ButtonOKClick(Sender: TObject);
    			procedure ButtonSaveClick(Sender: TObject);
				procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
				procedure FormShow(Sender: TObject);
		private
				procedure ShowReport;
            	procedure TestRepo();
        		procedure DoSetUp();

		public
              	RemoteRepo, LocalConfig, NoteDirectory : ANSIString;
              	SetupFileSync : boolean;
                function Proceed(const ClashRec : TClashRecord) : TClashDecision;
		end;

var
		FormSync: TFormSync;

implementation

{ In SetupFileSync mode, does a superficial, non writing, test OnShow() user
  can then click 'OK' and we'd do a real sync, exit and settings saved in calling
  process.
}

uses LazLogger;
{$R *.lfm}

var
    FileSync : TTomboyFileSync;
{ TFormSync }

function TFormSync.Proceed(const ClashRec : TClashRecord) : TClashDecision;
begin
	// showmessage(TheAction + ' ' + FileName);
    with TTaskDialog.Create(self) do
        try
          Caption := 'Note clash has been detected';
          Title := 'Please indicate what you would like to do';
          Text := Clashrec.Title + 'Server Note Last Change ' + ClashRec.ServerLastChange
          	+ '\n Local Note Last Change ' + ClashRec.LocalLastChange;
          FooterText := 'Note ID ' + ClashRec.NoteID;
          CommonButtons := [];
          with TTaskDialogButtonItem(Buttons.Add) do begin
            Caption := 'Download the remote note';
            ModalResult := TModalResult(cdDownload);
          end;
          with TTaskDialogButtonItem(Buttons.Add) do begin
            Caption := 'Backup and overwrite the local note';
            ModalResult := TModalResult(cdUpload);
          end;
          with TTaskDialogButtonItem(Buttons.Add) do begin
            Caption := 'Do nothing this for this sync run.';
            ModalResult := TModalResult(cdDoNothing);
          end;
          MainIcon := tdiQuestion;
          if Execute then
            Result := TClashDecision(ModalResult);
        finally
          Free;
        end;
	case Result of
		cdUpload : ShowMessage('we''ll upload');
		cdDownLoad : ShowMessage('we''ll download');
		cdDoNothing : ShowMessage('we''ll do nothing');
	end;
end;

procedure TFormSync.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
	FileSync.Free;
end;



procedure TFormSync.FormShow(Sender: TObject);
begin
	FileSync := TTomboyFileSync.Create;
    FileSync.ProceedFunction:= @Proceed;
	// FileSync.DebugMode := True;
	FileSync.NotesDir:= NoteDirectory;
	FileSync.RemoteManifestDir:=RemoteRepo;
	FileSync.LocalManifestDir:=LocalConfig;
    Memo1.Clear;
    StringGridReport.Clear;
    if SetUpFileSync then begin
        if FileSync.GetLocalServerID() then
              memo1.append('Connection already setup, are you sure you want a NEW connection ?')
        else memo1.Append('Could not find local mainfest, thats OK');
        ButtonCancel.Enabled:=True;
        ButtonOK.Enabled := False;
        ButtonSave.Enabled := True;
    	Label1.Caption := 'Testing Sync';
    	Label2.Caption:='If the report below makes sense, click Save and Sync !';
        TestRepo();
    end else begin
        ButtonCancel.Enabled:=False;
        ButtonOK.Enabled := False;
        ButtonSave.Enabled := False;
    	Label1.Caption := 'Manual Sync';
    	Label2.Caption:='Doing a manual sync, please report any surprises !';
        if FileSync.GetLocalServerID then
            Memo1.Append('Found local manifest, good !')
        else begin
	     	DebugLn('Looks like we are not setup to sync - ', FileSync.ErrorMessage);
            Label2.Caption:= 'Sorry, Sync does not appear to be setup yet';
            exit();
        end;
		if not FileSync.CheckRemoteServerID() then
	        Memo1.Append('Remote Server ID not found or does not match - ' + FileSync.ErrorMessage)
	    else
        	if FileSync.DoSync(True, True) then
                Memo1.Append('Sync completed.')
            else
	    		Memo1.Append('DoSync reported an error - ' + FileSync.ErrorMessage);
        ShowReport();
        ButtonOK.Enabled := True;
    end;
end;

procedure TFormSync.ShowReport();
var
        Index : integer;
begin
     	with FileSync.ReportList do begin
    		for Index := 0 to Count -1 do begin
                StringGridReport.InsertRowWithValues(Index, [Items[Index]^.Action, Items[Index]^.Title]);
    		end
    	end;
        StringGridReport.AutoSizeColumn(0);
        StringGridReport.AutoSizeColumn(1);
        if  FileSync.ReportList.Count = 0 then
            Memo1.Append('No notes needed syncing. You need to write more.')
        else Memo1.Append(inttostr(FileSync.ReportList.Count) + ' notes were dealt with.');
end;

procedure TFormSync.TestRepo;	// called OnShow()
var
        ServerID : ANSIString;
begin
    FileSync.TestMode := True;
    Memo1.Append('Remote Repo is ' + RemoteRepo);
    if FileSync.GetRemoteServerID(ServerID) then begin
        // Joining an existing Repo, we'll do Dummy run and ask user to confirm ?
        Memo1.Append('There is a Repo there already, suggest we join it.');
        FileSync.DoSync(False, True);
    end else
    	if FileSync.MakeConnection() then memo1.Append('Make connection True')
        else memo1.append('make connection false');
    ShowReport();
end;

procedure TFormSync.DoSetUp;
begin
    Memo1.Append('Remote Repo is ' + RemoteRepo);
    FileSync.TestMode := False;
    if not FileSync.MakeConnection() then begin
        Memo1.Append('DoSync reported an error');
        Memo1.Append(FileSync.ErrorMessage);
    end else Memo1.Append('Wow, DoSync appears to be happy');
    StringGridReport.Clear;
    ShowReport();
end;



procedure TFormSync.ButtonCancelClick(Sender: TObject);
begin
    ModalResult := mrNone;
end;

procedure TFormSync.ButtonOKClick(Sender: TObject);
begin
	ModalResult := mrOK;
end;

procedure TFormSync.ButtonSaveClick(Sender: TObject);
begin
    ButtonCancel.Enabled := False;
    ButtonSave.Enabled := False;
    DoSetUp();
    ButtonOK.Enabled := True;
end;

end.

