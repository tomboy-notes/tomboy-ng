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
    2018/10/25  New sync model. Much testing, support for Tomdroid.
    2018/10/28  Much tweaking and bug fixing.
    2018/10/29  Tell TB_Sdiff about note title before showing it.
    2018/10/30  Don't show SyNothing in sync report
    2018/11/04  Added support to update in memory NoteList after a sync.
    2019/05/19  Display strings all (?) moved to resourcestrings
    2020/02/20  Added capability to sync without showing GUI.
    2020/06/18  Only show good sync notification for 3 seconds
    2020/08/07  Changed the stringGrid to a ListView 'cos it handles dark themes better.
    2020/08/10  ListView becomes type=vsReport
    2020/09/22  Added a progress indicator, could expand to file transfer count .....
    2020/09/22  Restructured panel changeover between file nextcloud
}

{$mode objfpc}{$H+}




interface

uses
		Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
		StdCtrls, Sync, ComCtrls,  Syncutils;

type

		{ TFormSync }

  TFormSync = class(TForm)
				ButtonSave: TButton;
				ButtonCancel: TButton;
				ButtonClose: TButton;
				EditNextHostAddress: TEdit;
				EditNextUser: TEdit;
				EditPass: TEdit;
				Label1: TLabel;
				Label2: TLabel;
				Label3: TLabel;
				Label4: TLabel;
				Label5: TLabel;
				LabelProgress: TLabel;
				LabelFileSyncInfo1: TLabel;
				LabelFileSyncInfo2: TLabel;
				LabelSlow: TLabel;
                ListViewReport: TListView;      // Viewstyle=vsReport, make columns in Object Inspector
				Memo1: TMemo;
				PanelFile: TPanel;
				Panel2: TPanel;
				Panel3: TPanel;
				PanelNextCloud: TPanel;
				PanelTop: TPanel;
				Splitter3: TSplitter;
                                        { Runs a sync without showing form. Ret False if error or its not setup.
                                          Caller must ensure that Sync is config and that the Sync dir is available.
                                          If clash, user will see dialog. }


				procedure EditPassKeyPress(Sender: TObject; var Key: char);
                procedure FormCreate(Sender: TObject);
                                        { Runs a File Sync with displaying the form. Likely only triggered
                                        by the auto sync timer in Settings. Caller is responsible for checking
                                        that we are not busy before calling. }
                function RunSyncHidden() : boolean;
				procedure ButtonCancelClick(Sender: TObject);
				procedure ButtonCloseClick(Sender: TObject);
    			procedure ButtonSaveClick(Sender: TObject);
				procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
                procedure FormHide(Sender: TObject);
			                            { At Show, depending on SetUpSync, we'll either go ahead and do it, any
			                            error is fatal or, if Setup is True, walk user through process. }
				procedure FormShow(Sender: TObject);
		private
                FormShown : boolean;
                LocalTimer : TTimer;
                procedure AddLVItem(Act, Title, ID: string);
                procedure AdjustNoteList(S: TSync);
                procedure AfterShown(Sender : TObject);
				procedure DisplayMode(Mode: TSyncTransport);
                    // Display a summary of sync actions to user.
                function DisplaySync(S: TSync): string;
                    { Called when user wants to join a (possibly uninitialised) Repo,
                      will handle some problems with user's help. }
                procedure JoinSync;
                    { Called to do a sync assuming its all setup. Any problem is fatal }
                function ManualSync: boolean;
				procedure NextCloudSync();
                                    { We respond here to RepoJoin, RepoUse, RepoTest.  A RepoNew
                                      is not appropriate becasue there maybe notes at either end.
                                      RepoJoin will force a new connection, overwriting any existing config
                                      so, make sure its required before calling it ! RepoUse will
                                      return an error if creds don't match. RepoTest will just
                                      look to see if Sync appears to exist, does not test viability.}
				function RunNextCloudSync(Mode: TRepoAction): TSyncAvailable;
                                    { Populates the string grid with details of notes to be actioned }
                procedure ShowReport(S: TSync);
            	//procedure TestRepo();
        		//procedure DoSetUp();

		public
                Busy : boolean; // indicates that there is some sort of sync in process now.
                Transport : TSyncTransPort;

                LocalConfig, NoteDirectory : ANSIString;
                    { Indicates we are doing a setup User has already agreed to abandon any
                      existing Repo but we don't know if indicated spot already contains a
                      repo or, maybe we want to make one. }
              	SetupSync : boolean;
                    { we will pass address of this function to Sync }
                function Proceed(const ClashRec : TClashRecord) : TSyncAction;

                    { we will pass address of this function to Sync to report on progress }
                procedure SyncProgress(const St : string);
		end;

var
		FormSync: TFormSync;

implementation

{ In SetupFileSync mode, does a superficial, non writing, test OnShow() user
  can then click 'OK' and we'd do a real sync, exit and settings saved in calling
  process.
  Important to note that this form can be active but not visible when its doing an
  auto sync.  Be careful to ensure nothing is actioned while Busy = true and Busy is
  always set when doing stuff. This unit is definitly not thread safe but the Sync
  unit could (and perhaps should) be ....
}

uses LazLogger, SearchUnit, TB_SDiff, {Sync,}  LCLType, SyncError, ResourceStr, notifier,
        settings;		// just for DarkTheme

{$R *.lfm}

var
        ASync : TSync;
{ TFormSync }


function TFormSync.Proceed(const ClashRec : TClashRecord) : TSyncAction;
var
    SDiff : TFormSDiff;
    //Res : integer;
begin
    SDiff := TFormSDiff.Create(self);
    SDiff.RemoteFilename := ClashRec.ServerFileName;
    SDiff.LocalFilename := ClashRec.LocalFileName;
    SDiff.NoteTitle := ClashRec.Title;
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
    Application.ProcessMessages;    // so dialog goes away while remainder are being processed.
    // Use Remote, Yellow is mrYes, File1
    // Use Local, Aqua is mrNo, File2
end;

procedure TFormSync.SyncProgress(const St: string);
begin
        LabelProgress.Caption := St;
        Application.ProcessMessages;
end;

procedure TFormSync.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
	FreeandNil(ASync);
    Busy := False;
    writeln('close');
end;

procedure TFormSync.FormHide(Sender: TObject);
begin
    if LocalTimer = Nil then exit();
    LocalTimer.Free;
    LocalTimer := nil;
    writeln('kill timer');
end;


// Following resourcestrings defined in syncUtils.pas

function TFormSync.DisplaySync(S : TSync): string;
var
    UpNew, UpEdit, Down, DelLoc, DelRem, Clash, DoNothing, Errors : integer;
begin
    S.ReportMetaData(UpNew, UpEdit, Down, DelLoc, DelRem, Clash, DoNothing, Errors);
    Memo1.Append(rsNewUploads + inttostr(UpNew));
    Memo1.Append(rsEditUploads + inttostr(UpEdit));
    Memo1.Append(rsDownloads + inttostr(Down));
    Memo1.Append(rsLocalDeletes + inttostr(DelLoc));
    Memo1.Append(rsRemoteDeletes + inttostr(DelRem));
    Memo1.Append(rsClashes + inttostr(Clash));
    Memo1.Append(rsDoNothing + inttostr(DoNothing));
    Memo1.Append(rsSyncERRORS + inttostr(Errors));
    result := 'Uploads=' + inttostr(UpNew+UpEdit) + ' downloads=' + inttostr(Down) + ' deletes=' + inttostr(DelLoc + DelRem);
    // debugln('Display Sync called, DoNothings is ' + inttostr(DoNothing));
end;

    // User is only allowed to press Cancel or Save when this is finished.
procedure TFormSync.JoinSync;
var
    SyncAvail : TSyncAvailable;
    // ASync : TSync;
    // UpNew, UpEdit, Down, DelLoc, DelRem, Clash, DoNothing : integer;
begin
    freeandnil(ASync);
    ASync := TSync.Create;                      // This is a regional Var
    Label1.Caption:= rsTestingRepo;
    Application.ProcessMessages;
    ASync.ProceedFunction := @Proceed;
    ASync.ProgressProcedure := @SyncProgress;
    ASync.DebugMode := Application.HasOption('s', 'debug-sync');
    ASync.NotesDir:= NoteDirectory;
    ASync.ConfigDir := LocalConfig;
    ASync.RepoAction:=RepoJoin;
    Async.SetTransport(TransPort);
    SyncAvail := ASync.TestConnection();
    if SyncAvail = SyncNoRemoteRepo then
        if mrYes = QuestionDlg('Advice', rsCreateNewRepo, mtConfirmation, [mrYes, mrNo], 0) then begin
            ASync.RepoAction:=RepoNew;
            SyncAvail := ASync.TestConnection();
        end;
    if SyncAvail <> SyncReady then begin
        showmessage(rsUnableToProceed + ' ' + ASync.ErrorString);
        ModalResult := mrCancel;
    end;
    Label1.Caption:=rsLookingatNotes;
    Application.ProcessMessages;
    ASync.TestRun := True;
    if ASync.StartSync() then begin
        DisplaySync(ASync);
        ShowReport(ASync);
        Label1.Caption:=rsLookingatNotes;
        Label2.Caption := rsSaveAndSync;
        ButtonSave.Enabled := True;
    end  else
        Showmessage(rsSyncError + ' ' + ASync.ErrorString);
    ButtonCancel.Enabled := True;                                   // now wait for user to press a button !
end;

procedure TFormSync.AfterShown(Sender : TObject);
begin
    LocalTimer.Enabled := False;              // Don't want to hear from you again
    if self.Transport = SyncFile then begin
        Caption := 'File Sync';
        Busy := True;
        try
            if SetUpSync then begin
                memo1.Append('---- Joining a file sync -----');
	            JoinSync();
                exit;                           // exit here because if it was a setup, user is probably finished.
	        end;
{            if (Sett.ValidFileSync <> '') and (not Sett.CheckFileSyncDisabled.checked) then begin
                memo1.Append('---- Using a file sync -----');
	            ManualSync();
			end; }
			if Sett.ValidNextCloudSync <> '' then begin
                DisplayMode(SyncNextCloud);
                Transport := SyncNextCloud;
                LabelProgress.Caption := '';
			    // We cannot just proceed with a NextCloud 'use' sync here, must wait until user enters a .
                // Username and password. But we do know its a 'Use' situation, not setup.
                // Hmm, if user already has username/passwrd entered, we could proceed ......
			end;
		finally
            Busy := False;
		end;
	end;
    if Transport = SyncNextCloud then
        DisplayMode(SyncNextCloud);
end;

//RESOURCESTRING
//  rsPleaseWait = 'Please wait a minute or two ...';

procedure TFormSync.FormShow(Sender: TObject);
begin
    Left := 55 + random(55);
    Top := 55 + random(55);
    FormShown := False;
{    if Transport = syncFile then PanelFile.Height := 90;
    if Transport = SyncNextCloud then PanelFile.Height := 150;}

	Label2.Caption := rsNextBitSlow;
    Memo1.Clear;
    ListViewReport.Clear;
    ButtonSave.Enabled := False;
    ButtonClose.Enabled := False;
    ButtonCancel.Enabled := False;
    LabelProgress.Caption := '';
    DisplayMode(Transport);
    {$ifdef windows}  // linux apps know how to do this themselves
    if Sett.DarkTheme then begin
         // Sett.BackGndColour;  Sett.TextColour;
         ListViewReport.Color :=       clnavy;
         ListViewReport.Font.Color :=  Sett.HiColour;
         splitter3.Color:= clnavy;
         PanelFile.color := Sett.BackGndColour;
         Panel2.color := Sett.BackGndColour;
         Panel3.color := Sett.BackGndColour;
         PanelNextCloud.color := Sett.BackGndColour;
         Label1.Font.Color:= Sett.TextColour;
         Label2.Font.Color := Sett.TextColour;
         Memo1.Color:= Sett.BackGndColour;
         Memo1.Font.Color := Sett.TextColour;
         ButtonCancel.Color := Sett.HiColour;
         ButtonClose.Color := Sett.HiColour;
         ButtonSave.Color := Sett.HiColour;
    end;
    {$endif}
    // We call a timer to get out of OnShow so ProcessMessages works as expected
    LocalTimer := TTimer.Create(Nil);
    LocalTimer.OnTimer:= @AfterShown;
    LocalTimer.Interval:=500;
    LocalTimer.Enabled := True;
end;

function TFormSync.RunSyncHidden(): boolean;
begin
    //debugln('In RunSyncHidden');
    if SetUpSync then exit(False);      // should never call this in setup mode but to be sure ...
    busy := true;
    ListViewReport.Clear;
    Result := ManualSync;
end;

procedure TFormSync.FormCreate(Sender: TObject);
begin
    ASync := Nil;
end;


procedure TFormSync.EditPassKeyPress(Sender: TObject; var Key: char);
begin
    if (ord(Key) = VK_Return) and (EditPass.Text <> '') and (EditNextUser.Text <> '') then
        ButtonSaveClick(sender);
end;

procedure TFormSync.DisplayMode(Mode : TSyncTransport);
begin
    LabelSlow.Caption := '';
    if Mode = SyncNextCloud then begin      // we setup here like we are waiting for user to enter host ?, username and password.
        Label1.Caption := 'NextCloud Sync';
        //PanelNextCloud.Visible := true;
        //PanelNextCloud.width := PanelFile.width;
        EditNextHostAddress.Text := Sett.ValidNextCloudSync;
        ButtonCancel.Enabled := True;
        ButtonSave.Enabled   := True;
        ButtonSave.Caption   := 'NextCloud Sync';
        PanelFile.Width := 0;
	end else begin
        PanelFile.Width := Width;
        //PanelNextCloud.Visible := false;
        ButtonSave.Caption   := 'Save and Sync';
        LabelFileSyncInfo1.Caption := rsFileSyncInfo1;
        LabelFileSyncInfo2.Caption := rsFileSyncInfo2;
    end;
end;

// We might get here in a Use situation, just after a manual FileSync, or in a Join
// where the user has clicked the NextCloud Setup button. We assume they have already agreed
// to discard any existing config in the latter case. But still check again because its
// serious.

procedure TFormSync.NextCloudSync();
begin
    if SetupSync then begin
        if RunNextCloudSync(RepoTest) = SyncReady then
            // SyncRead is an error when Joining, check user really wants to abandon existing config files.
            if IDNO = Application.MessageBox('Discard existing sync config ?'
                        , 'Might generate lots of duplicates !', MB_ICONQUESTION + MB_YESNO) then
                exit;
        memo1.Append('---- Joining a NextCloud sync -----');
        RunNextCloudSync(RepoJoin);
	end else begin
        if RunNextCloudSync(RepoTest) = SyncReady then begin
            memo1.Append('---- Using a NextCloud sync -----');
            RunNextCloudSync(RepoUse)
		end
		else
            showmessage('No sync setup yet');
	end;
end;

        { We can have three paths here -
          - Join, there is no existing connection, make end to end, sync, maybe notes at either end.
          - Use, an existing connection exists, lets use it.
         Join will always overwrite existing sync config if it exists, use Test to check
         first. Three components are required to say we have a connection, each should have
         same ServerID -
             * The local manifest, <config_dir>/nextcloud/manifest.xml
             * The remote  manifest, <config_dir>/nextcloud/manifest-remote.xml
             * The NextCloud Note, aka, the KEYNOTE, on remote system.
            If we do not have a three way match, we cannot proceed unless user says
            'force' it. Under those circumstances, we trash all three and do 'Join'.
            So, there is no 'New' here or any one to one sync.
          }


function TFormSync.RunNextCloudSync(Mode : TRepoAction) : TSyncAvailable;
	    { There are some structural problems here. First, we do not do a test run at initial connection
	    time like in FileSync. Thats because we have to write a new signiture file remotely, I am sure
	    we could work around that if we tried.
	    Secondly, we delete the local xml files before we determine if there is an existing sig file
	    remotely. It would be far better to test for that remote file, see if it matches the local xml
	    and try to talk user out of proceeding if possible. Can we move dealing with that remote sig
        file back to SetTransport ? }
var
    HaveSync : boolean;
    NSync : TSync;
begin
    NSync := TSync.Create;
    try
        NSync.ProceedFunction:= @Proceed;
        NSync.ProgressProcedure := @SyncProgress;
        //NSync.DebugMode  := True;
	    NSync.NotesDir   := NoteDirectory;
	    NSync.ConfigDir  := LocalConfig + 'nextcloud/';
        NSync.RepoAction := Mode;
        NSync.SyncAddress:= EditNextHostAddress.text;
        NSync.Password   := EditPass.text;
        NSync.UserName   := EditNextUser.text;
        // NSync.TestRun := True;                                          // NOTE THIS LINE    TEST RUN   !!!!
        if Mode = RepoTest then
            exit(Nsync.SetTransport(TransPort))                         // Ret either SyncReady or SyncNoLocal
        else HaveSync := (Nsync.SetTransport(TransPort) = SyncReady);   // which should be syncNextCloud
        // If to here, we must be either RepoNew or RepoUse.
        if (HaveSync and (Mode = RepoJoin)) then
            Nsync.SetTransport(TransPort, True);                        // That line really needs to be AFTER the TestConnection !
        HaveSync := False;
		case NSync.TestConnection() of
            SyncReady : haveSync := True;
            SyncCredentialError,
            SyncXMLError,
            SyncNoLocal,
            SyncNetworkError : Memo1.append('ERROR : ' + NSync.ErrorString);
            SyncNoRemoteRepo : Memo1.append('Failed to find a repo : ' + NSync.ErrorString);
            SyncMismatch :     Memo1.Append('Server ID does not match, maybe you should remove the NextCloud to tomboy-ng note.');
		else
                Memo1.append('TestConnection returned some other error : ' + NSync.ErrorString);
		end;

        //exit;

        if not HaveSync then exit;
        //Memo1.append('OK, looks like something worked, wow ! ');
        // if to here, we seem ready to do some sync stuff.
        LabelSlow.Caption := rsNextBitSlow;
        Application.ProcessMessages;
        if NSync.StartSync() then begin
            if Sett.ValidNextCloudSync <> EditNextHostAddress.Text then
                Sett.ValidNextCloudSync := EditNextHostAddress.Text;
            ShowReport(NSync);
            DisplaySync(NSync);
            AdjustNoteList(NSync);
		end else debugln('ERROR - TFormSync.RunNextCloudSync - StartSync returned False');
	finally
        freeandnil(NSync);
	end;
    LabelSlow.Caption := '';
end;

        // User is only allowed to press Close when this is finished.
function TFormSync.ManualSync : boolean;
var
    SyncState : TSyncAvailable = SyncNotYet;
    Notifier : TNotifier;
    SyncSummary : string;
    MSync : TSync;
begin
    Label1.Caption := rsTestingSync;
    Application.ProcessMessages;
	MSync := TSync.Create;
    try
        MSync.ProceedFunction:= @Proceed;
        MSync.ProgressProcedure := @SyncProgress;
        MSync.DebugMode := Application.HasOption('s', 'debug-sync');
	    MSync.NotesDir:= NoteDirectory;
	    MSync.ConfigDir := LocalConfig;
        MSync.RepoAction:=RepoUse;
        Msync.SetTransport(TransPort);
        SyncState := MSync.TestConnection();
        //MSync.SyncAddress := MSync.SyncAddress;         // ToDo : WTF ? is this another   grosjo-ism ?
	    while SyncState <> SyncReady do begin
            if MSync.DebugMode then debugln('Failed testConnection');
            FormSyncError.Label1.caption := rsUnableToSync + ':';
            FormSyncError.label3.caption := MSync.ErrorString;
            // in autosync mode, form is not visible, we just send a notify that cannot sync right now.
            if not Visible then begin
                SearchForm.UpdateStatusBar(rsAutoSyncNotPossible);
                Notifier := TNotifier.Create;                                           // does not require a 'free'.
                Notifier.ShowTheMessage('tomboy-ng', rsAutoSyncNotPossible, 12000);     // 12 seconds
                exit;
            end else begin
                showmessage('Unable to sync because ' + MSync.ErrorString);
                FormSync.ModalResult := mrAbort;
                exit(false);
            end;
            (*FormSyncError.ButtRetry.Visible := not Visible;                           // Dont show Retry if interactive
            ModalResult := FormSyncError.ShowModal;
            if ModalResult = mrCancel then begin                                        // else its Retry

                exit(false);
            end; *)
            SyncState := MSync.TestConnection();
        end;
        Label1.Caption:= rsRunningSync;
        Application.ProcessMessages;
        MSync.TestRun := False;
        MSync.StartSync();
        SyncSummary :=  DisplaySync(MSync);
        SearchForm.UpdateStatusBar(rsLastSync + ' ' + FormatDateTime('YYYY-MM-DD hh:mm', now)  + ' ' + SyncSummary);
        if not Visible then begin
            Notifier := TNotifier.Create;                                           // does not require a 'free'.
            Notifier.ShowTheMessage('tomboy-ng', rsLastSync  + ' ' + SyncSummary, 3000);
        end;
        ShowReport(MSync);
        AdjustNoteList(MSync);                              // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Label1.Caption:= 'File Sync ' + rsAllDone;
        Label2.Caption := rsPressClose;
        ButtonClose.Enabled := True;
        Result := True;
    finally
        FreeandNil(MSync);
        Busy := False;
    end;
end;

procedure TFormSync.AdjustNoteList(S : TSync);
var
    DeletedList, DownList : TStringList;
    Index : integer;
begin
    DeletedList := TStringList.Create;
    DownList := TStringList.Create;
 	with S.MainMetaData do begin
		for Index := 0 to Count -1 do begin
            if Items[Index]^.Action = SyDeleteLocal then
                DeletedList.Add(Items[Index]^.ID);
            if Items[Index]^.Action = SyDownload then
                DownList.Add(Items[Index]^.ID);
        end;
    end;
    if (DeletedList.Count > 0) or (DownList.Count > 0) then
        SearchForm.ProcessSyncUpdates(DeletedList, DownList);
   FreeandNil(DeletedList);
   FreeandNil(DownList);
end;

procedure TFormSync.AddLVItem(Act, Title, ID : string);
var
    TheItem : TListItem;
begin
   TheItem := ListViewReport.Items.Add;
   TheItem.Caption := Act;
   TheItem.SubItems.Add(copy(Title, 1, 25)+' ');
   TheItem.SubItems.Add(ID);
end;

procedure TFormSync.ShowReport(S : TSync);
var
        Index : integer;
        Rows : integer = 0;
begin
    with S.MainMetaData do begin
	    for Index := 0 to Count -1 do begin
	    if Items[Index]^.Action <> SyNothing then begin
	            AddLVItem(
	                S.MainMetaData.ActionName(Items[Index]^.Action)
	                , Items[Index]^.Title
	                , Items[Index]^.ID);
	            inc(Rows);
	    end;
	    end
	end;
	if  Rows = 0 then
	    Memo1.Append(rsNoNotesNeededSync)
	else Memo1.Append(inttostr(S.MainMetaData.Count) + rsNotesWereDealt);
    {$IFDEF DARWIN}     // Apparently ListView.columns[n].autosize does not work in Mac, this is rough but better then nothing.
    ListViewReport.Columns[0].Width := listviewReport.Canvas.Font.GetTextWidth('upload edit ');
    ListViewReport.Columns[1].Width := ListViewReport.Columns[0].Width *2;
    {$ENDIF}
end;


procedure TFormSync.ButtonCancelClick(Sender: TObject);
begin
    ModalResult := mrCancel;
end;

procedure TFormSync.ButtonCloseClick(Sender: TObject);
begin
	ModalResult := mrOK;
end;

    // This only ever happens during a Join or a NextCloud Sync
procedure TFormSync.ButtonSaveClick(Sender: TObject);
begin
    //ModalResult := mrCancel;

    //exit;


    Label2.Caption:=rsNextBitSlow;
    Label1.Caption:='First Time Sync';
    // Memo1.Clear;
    Memo1.append('');
    Application.ProcessMessages;
    ButtonCancel.Enabled := False;
    ButtonSave.Enabled := False;
    if Transport = SyncNextCloud then begin
        Label1.Caption:='Running NextCloud Sync';
        LabelSlow.Caption := rsNextBitSlow;
        Application.ProcessMessages;
        NextCloudSync();
        Label1.Caption:='NextCloud Sync Completed';
        LabelSlow.Caption := '';
        ButtonSave.Enabled := True;
        ButtonClose.Enabled:= True;
        exit;
	end;
    ASync.TestRun := False;
	if ASync.StartSync() then begin
        SearchForm.UpdateStatusBar(rsLastSync + ' ' + FormatDateTime('YYYY-MM-DD hh:mm', now)  + ' ' + DisplaySync(ASync));
        ShowReport(ASync);
        AdjustNoteList(ASync);
        Label1.Caption:=rsAllDone;
        Label2.Caption := rsPressClose;
    end  else
        Showmessage(rsSyncError + ASync.ErrorString);
    ButtonClose.Enabled := True;
end;

end.

