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
	2017/12/30  Changed above DebugMode to VerboseMode
	2017/12/30  We now call IndexNotes() after a sync. Potentially slow.
	2017/12/30  Added a seperate procedure to do manual Sync, its called
				by a timer to ensure we can see dialog before it starts.
	2018/01/01  Added ID in sync report to make it easier to track errors.
	2018/01/01  Set goThumbTracking true so contents of scroll box glide past as
    			you move the "Thumb Slide".
	2018/01/01  Changed ModalResult for cancel button to mrCancel
	2018/01/08  Tidied up message box text displayed when a sync conflict happens.
	2018/01/25  Changes to support Notebooks
    2018/01/04  Forced a screen update before manual sync so user knows whats happening.
    2018/04/12  Added ability to call MarkNoteReadOnly() to cover case where user has unchanged
                note open while sync process downloads or deletes that note from disk.
    2018/04/13  Taught MarkNoteReadOnly() to also delete ref in NoteLister to a sync deleted note
    2018/05/12  Extensive changes - MainUnit is now just that. Only change here relates
                to naming of MainUnit and SearchUnit.
    2018/05/21  Show any sync errors as hints in the StringGrid.
    2018/06/02  Honor a cli --debug-sync
    2018/06/14  Update labels when transitioning from Testing Sync to Manual Sync
    2018/08/14  Added SDiff to replace clumbsy dialog when sync clash happens.
    2018/08/18  Improved test/reporting of file access during sync



}

{$mode objfpc}{$H+}




interface

uses
		Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
		StdCtrls, Grids, Syncutils;

type

		{ TFormSync }

  TFormSync = class(TForm)
				ButtonSave: TButton;
				ButtonCancel: TButton;
				ButtonClose: TButton;
				Label1: TLabel;
				Label2: TLabel;
				Memo1: TMemo;
				Panel1: TPanel;
				Panel2: TPanel;
				Panel3: TPanel;
				Splitter3: TSplitter;
				StringGridReport: TStringGrid;
				procedure ButtonCancelClick(Sender: TObject);
				procedure ButtonCloseClick(Sender: TObject);
    			procedure ButtonSaveClick(Sender: TObject);
				procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
                procedure FormHide(Sender: TObject);
                { At Show, depending on SetUpSync, we'll either go ahead and do it, any
                  error is fatal or, if True, walk user through process. }
				procedure FormShow(Sender: TObject);
                procedure StringGridReportGetCellHint(Sender: TObject; ACol, ARow: Integer;
                                  var HintText: String);
		private
                FormShown : boolean;
                LocalTimer : TTimer;
                procedure AfterShown(Sender : TObject);
                procedure DisplaySync();
                    { Called when user wants to join a (possibly uninitialised) Repo,
                      will handle some problems with user's help. }
                procedure JoinSync;
                    { Called to do a sync assuming its all setup. Any problem is fatal }
                procedure ManualSync;

    procedure ShowReport;
            	//procedure TestRepo();
        		//procedure DoSetUp();

		public

                Transport : TSyncTransPort;

                    // A string containg a URL to remote repo, just a dir for FileSync
              	RemoteRepo : String;
                LocalConfig, NoteDirectory : ANSIString;
                    { Indicates we are doing a setup User has already agreed to abandon any
                      existing Repo but we don't know if indicated spot already contains a
                      repo or, maybe we want to make one. }
              	SetupSync : boolean;
                    { we will pass address of this function to Sync }
                procedure MarkNoteReadOnly(const Filename : string; const WasDeleted : Boolean = False);
                    { we will pass address of this function to Sync }
                function Proceed(const ClashRec : TClashRecord) : TSyncAction;
		end;

var
		FormSync: TFormSync;

implementation

{ In SetupFileSync mode, does a superficial, non writing, test OnShow() user
  can then click 'OK' and we'd do a real sync, exit and settings saved in calling
  process.
}

uses LazLogger, SearchUnit, TB_SDiff, Sync;
{$R *.lfm}

var
        ASync : TSync;
{ TFormSync }


procedure TFormSync.MarkNoteReadOnly(const Filename : string; const WasDeleted : Boolean = False);
begin
    SearchForm.MarkNoteReadOnly(FileName, WasDeleted);
end;

function TFormSync.Proceed(const ClashRec : TClashRecord) : TSyncAction;
var
    SDiff : TFormSDiff;
    Res : integer;
begin
    SDiff := TFormSDiff.Create(self);
    SDiff.RemoteFilename := ClashRec.ServerFileName;
    SDiff.LocalFilename := ClashRec.LocalFileName;
    case SDiff.ShowModal of
            mrYes      : Result := SyDownLoad;
            mrNo       : Result := SyUpLoadEdit;
            mrNoToAll  : Result := SyAllLocal;
            mrYesToAll : Result := SyAllRemote;
            mrAll      : Result := SyAllNewest;
            mrClose    : Result := SyAllOldest;
    else
            Result := SyUnSet;      // Thats an ERROR !  What are you doing about it ?
    end;
    SDiff.Free;
    // Use Remote, Yellow is mrYes, File1
    // Use Local, Aqua is mrNo, File2
end;

procedure TFormSync.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
	FreeandNil(ASync);          // probably not necessary but ....
end;

procedure TFormSync.FormHide(Sender: TObject);
begin
    if LocalTimer = Nil then exit();
    LocalTimer.Free;
    LocalTimer := nil;
end;




procedure TFormSync.DisplaySync();
var
    UpNew, UpEdit, Down, DelLoc, DelRem, Clash, DoNothing : integer;
begin
    ASync.ReportMetaData(UpNew, UpEdit, Down, DelLoc, DelRem, Clash, DoNothing);
    Memo1.Append('New Uploads    ' + inttostr(UpNew));
    Memo1.Append('Edit Uploads   ' + inttostr(UpEdit));
    Memo1.Append('Downloads      ' + inttostr(Down));
    Memo1.Append('Local Deletes  ' + inttostr(DelLoc));
    Memo1.Append('Remote Deletes ' + inttostr(DelRem));
    Memo1.Append('Clashes        ' + inttostr(Clash));
    Memo1.Append('Do Nothing     ' + inttostr(DoNothing));
    debugln('Display Sync called, DoNothings is ' + inttostr(DoNothing));
end;

    // User is only allowed to press Cancel or Save when this is finished.
procedure TFormSync.JoinSync;
var
    SyncAvail : TSyncAvailable;
    // ASync : TSync;
    UpNew, UpEdit, Down, DelLoc, DelRem, Clash, DoNothing : integer;
begin
    freeandnil(ASync);
	ASync := TSync.Create;
    Label1.Caption:='Testing Repo ....';
    Application.ProcessMessages;
    ASync.ProceedFunction:= @Proceed;
    // ASync.MarkNoteReadOnlyProcedure := @MarkNoteReadOnly;      // This is a TODO !!
    ASync.DebugMode := Application.HasOption('s', 'debug-sync');
	ASync.NotesDir:= NoteDirectory;
	ASync.SyncAddress := RemoteRepo;        // This is 'some' URL
	ASync.ConfigDir := LocalConfig;
    ASync.RepoAction:=RepoJoin;
    Async.SetMode(TransPort);
    SyncAvail := ASync.TestConnection();
    if SyncAvail = SyncNoRemoteRepo then
        if mrYes = QuestionDlg('Advice', 'Create a new Repo ?', mtConfirmation, [mrYes, mrNo], 0) then begin
            ASync.RepoAction:=RepoNew;
            SyncAvail := ASync.TestConnection();
        end;
    if SyncAvail <> SyncReady then begin
        showmessage('Unable to proceed because ' + ASync.ErrorString);
        ModalResult := mrCancel;
    end;
    Label1.Caption:='Looking at notes ....';
    Application.ProcessMessages;
    ASync.TestRun := True;
    if ASync.StartSync() then begin
        DisplaySync();
        Label1.Caption:='Ready to proceed ....';
        Label2.Caption := 'Press Save and Sync if this looks OK';
        ButtonSave.Enabled := True;
    end  else
        Showmessage('A Sync Error occured ' + ASync.ErrorString);
    ButtonCancel.Enabled := True;
end;

procedure TFormSync.AfterShown(Sender : TObject);
begin
    LocalTimer.Enabled := False;             // Don't want to hear from you again
    if SetUpSync then begin
        JoinSync();
    end else
        ManualSync();
end;

procedure TFormSync.FormShow(Sender: TObject);
begin
    FormShown := False;
    Label2.Caption := 'Please wait a minute or two ...';
    Memo1.Clear;
    StringGridReport.Clear;
    ButtonSave.Enabled := False;
    ButtonClose.Enabled := False;
    ButtonCancel.Enabled := False;
    // We call a timer to get out of OnShow so ProcessMessages works as expected
    LocalTimer := TTimer.Create(Nil);
    LocalTimer.OnTimer:= @AfterShown;
    LocalTimer.Interval:=500;
    LocalTimer.Enabled := True;
end;

        // User is only allowed to press Close when this is finished.
procedure TFormSync.ManualSync();
begin
    Label1.Caption := 'Testing Sync';
    Application.ProcessMessages;
	ASync := TSync.Create;
    try
        ASync.ProceedFunction:= @Proceed;
        // ASync.MarkNoteReadOnlyProcedure := @MarkNoteReadOnly;      // This is a TODO !!
        ASync.DebugMode := Application.HasOption('s', 'debug-sync');
	    ASync.NotesDir:= NoteDirectory;
	    ASync.SyncAddress := RemoteRepo;        // This is 'some' URL
	    ASync.ConfigDir := LocalConfig;
        ASync.RepoAction:=RepoUse;
        Async.SetMode(TransPort);
        if Syncready <> ASync.TestConnection() then begin
            showmessage('Unable to sync because ' + ASync.ErrorString);
            ModalResult := mrCancel;
        end;
        Label1.Caption:= 'Running Sync';
        Application.ProcessMessages;
        ASync.TestRun := False;                    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ASync.StartSync();
        DisplaySync();
        Label1.Caption:='All Done';
        Label2.Caption := 'Press Close';
        ButtonClose.Enabled := True;
    finally
        FreeandNil(ASync);
    end;

{        if FileSync.GetLocalServerID then
            Memo1.Append('Found local manifest, good !')
        else begin
	     	DebugLn('Looks like we are not setup to sync - ', FileSync.ErrorMessage);
            Label2.Caption:= 'Sorry, Sync does not appear to be setup yet';
            exit();
        end;
		if not FileSync.CheckRemoteServerID() then begin
	        Memo1.Append('ERROR - ' + FileSync.ErrorMessage);
            DebugLn('ERROR - ' + FileSync.ErrorMessage);
		end else begin
            Application.ProcessMessages;
        	if FileSync.DoSync(True, True) then begin
                Memo1.Append('Sync completed.');
                SearchForm.IndexNotes();
            end else
	    		Memo1.Append('DoSync reported an error - ' + FileSync.ErrorMessage);
            ShowReport();
		end;
        ButtonClose.Enabled := True; }
end;

procedure TFormSync.ShowReport();
var
        Index : integer;
begin
{     	with FileSync.ReportList do begin
    		for Index := 0 to Count -1 do begin
                StringGridReport.InsertRowWithValues(Index,
                	[Items[Index]^.Action, Items[Index]^.Title, Items[Index]^.ID]);
    		end
    	end;
        StringGridReport.AutoSizeColumn(0);
        StringGridReport.AutoSizeColumn(1);
        if  FileSync.ReportList.Count = 0 then
            Memo1.Append('No notes needed syncing. You need to write more.')
        else Memo1.Append(inttostr(FileSync.ReportList.Count) + ' notes were dealt with.');    }
end;

procedure TFormSync.StringGridReportGetCellHint(Sender: TObject; ACol,
  ARow: Integer; var HintText: String);
begin
// HintText := FileSync.ReportList.Items[ARow]^.Message;
end;

{procedure TFormSync.TestRepo;	// called OnShow()
var
        ServerID : ANSIString;
        FatalError : boolean;
begin
    FileSync.TestMode := True;
    Memo1.Append('Remote Repo is ' + RemoteRepo);
    if FileSync.GetRemoteServerID(ServerID, FatalError) then begin
        // Joining an existing Repo, we'll do Dummy run and ask user to confirm ?
        Memo1.Append('There is a Repo there already, suggest we join it.');
        FileSync.DoSync(False, True);
    end else begin
        // No, if it turns out the remote manifest is not writeable or the directory is not ....
        if FatalError then begin
            showmessage('Error - that sync repo is unusable, check space, write permissions. '
                + FileSync.ErrorMessage);
            close();
            exit();
        end;
        if FileSync.MakeConnection() then memo1.Append('Make connection True')
            else memo1.append('make connection false');
    end;
    ShowReport();
end; }

{procedure TFormSync.DoSetUp;
begin
    Memo1.Append('Remote Repo is ' + RemoteRepo);
    FileSync.TestMode := False;
    if not FileSync.MakeConnection() then begin
        Memo1.Append('DoSync reported an error');
        Memo1.Append(FileSync.ErrorMessage);
    end else Memo1.Append('Wow, DoSync appears to be happy');
    StringGridReport.Clear;
    ShowReport();
    Label2.Caption:='OK, finished that';
end;  }



procedure TFormSync.ButtonCancelClick(Sender: TObject);
begin
    ModalResult := mrCancel;
end;

procedure TFormSync.ButtonCloseClick(Sender: TObject);
begin
	ModalResult := mrOK;
end;

    // This only ever happens during a Join .....
procedure TFormSync.ButtonSaveClick(Sender: TObject);
begin
    Label2.Caption:='OK, I''ll do it, please wait .....';
    Label1.Caption:='First Time Sync';
    Memo1.Clear;
    Application.ProcessMessages;
    ButtonCancel.Enabled := False;
    ButtonSave.Enabled := False;
    ASync.TestRun := False;
    if ASync.StartSync() then begin
        DisplaySync();
        Label1.Caption:='All Done';
        Label2.Caption := 'Press Close';
    end  else
        Showmessage('A Sync Error occured ' + ASync.ErrorString);
    ButtonClose.Enabled := True;
end;

end.

