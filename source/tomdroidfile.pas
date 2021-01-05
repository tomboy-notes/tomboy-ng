unit tomdroidFile;

{$mode objfpc}{$H+}

{
This unit is opened by the user, it tries to find the gio/gvfs mtp directory that
contains $MTPDIR/Phone/tomdroid/tomboy.serverid. It may find -
1. No mtp directory - likely the device is not connected.
2. A $mtpdir/Phone - Its connected but tomdroid is not configured to dump sync files in /storage/emulated/0/tomdroid  - rsInstallTomdroid
3. A $mtpdir/Phone/tomdroid - Its ready to create a new repo.
4. A $mtpdir/Phone/tomboy.serverid - Its ready to sync as an existing repo,

We do not need to save repo details in config file, if we have local manifest that
matches the serverid we found, we are good to go.  If we don't have a manifest that
matches, we must not proceed, it might do very bad things, we should offer user
a chance to do a JOIN instead (with appropriate warnings).
}

{ HISTORY
    2018/12/06  Added AdjustNoteList() to call ProcessSyncUpdates at end of a sync
    2018/04/28  Ensure user does not save profile after a Test run, the ID will change.
    2019/05/14  Display strings all (?) moved to resourcestrings
    2020/07/09  New help notes location.
    2021/01/04  Refocused on to USB cable based transport.
}


interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
    StdCtrls, ExtCtrls, Grids, LCLIntf,
    SyncUtils;    // For TClashRec

type

    { TFormTomdroidFile }

    TFormTomdroidFile = class(TForm)
			ButtonOldSSH: TButton;
        ButtonClose: TButton;
        ButtonJoin: TButton;
        ButtonHelp: TButton;
        ButtonSync: TButton;
        CheckBoxTestRun: TCheckBox;
        Label1: TLabel;
		Label6: TLabel;
		LabelAdvice2: TLabel;
		LabelAdvice1: TLabel;
		LabelAdvice: TLabel;
        Memo1: TMemo;
        Panel1: TPanel;
        Panel2: TPanel;
		PanelAdvice: TPanel;
        Splitter1: TSplitter;
        StringGridReport: TStringGrid;
        procedure ButtonHelpClick(Sender: TObject);
        procedure ButtonJoinClick(Sender: TObject);
		procedure ButtonOldSSHClick(Sender: TObject);
        procedure ButtonSyncClick(Sender: TObject);
        procedure FormShow(Sender: TObject);
    private
        //ServerID : string;    // Keep copies to see if user changed after selection
        procedure AdjustNoteList();
		procedure CheckStatus();
        procedure DisplaySync();
        procedure DoNewSync();
		function GetLocalManifestDateS(ID: string): string;
        function Proceed(const ClashRec: TClashRecord): TSyncAction;
        procedure ShowReport();
        function DoSync(): boolean;
    public

    end;

var
    FormTomdroidFile: TFormTomdroidFile;

implementation

{$R *.lfm}

{ TFormTomdroidFile }


uses Settings, Sync, TB_SDiff, typInfo, LazLogger, LCLType, LazFileUtils, laz2_DOM, laz2_XMLRead,
    SearchUnit, Tomdroid; // we call ProcessSyncUpdates( and ShowHelpNote(

RESOURCESTRING

  // ToDo : once the ssh based tomdroid sync is removed, we could move the active rs to Resourcestr file ?
  rsSetUpNewSync ='Setting up a new sync ....';                                                             // file
  rsFailedToConnect = 'Failed to connect.';                                                                 // file
  rsTalking = 'OK, talking to device. Wait for it ....';
  rsNoTomdroid =  'Unable to find Tomdroid sync dir on that device.';                                       // file
  rsInstallTomdroid = 'Install Tomdroid, config filesync, and run a sync';                                  // file
  rsNoConnection = 'Failed to establish a connection. ';                                                    // file
  rsFixConnection = 'If you are sure its there, check settings.';                                           // file
  rsConnectionGood = 'Connection is looking Good.';                                                         // file

//  rsCheckingForExistingSync = 'Checking for an existing sync ....';
//  rsTalkingToDevice = 'OK, talking to device. Wait for it ....';
  rsNotExistingRepo = 'That''s not an existing Repo, maybe click "Join" ?';                                  // file
  rsNotCorrectProfile = 'This is not correct profile for that device';
//  rsFailedToFindConnection_1 = 'Failed to find an existing connection.';
  rsFailedToFindConnection_2 = 'If you are sure there should be an existing connection, check settings.';    // file
//  rsFailedToFindConnection_3 = 'Otherwise, try joining a new connection.';
  rsHaveValidSync = 'Looking Good. Last sync date ';                                                         // file

  // New items in this unit
  rsJoinAnyway = 'Forcing a Join may "recover" some notes you thought you have deleted.';                    // file

  
 // Maybe some resourcestrings are defined in syncUtils.pas ??

var
    ASync : TSync;

procedure TFormTomdroidFile.CheckStatus();
begin
	    ASync := TSync.Create({DebugMode});
	    ASync.NotesDir  := Sett.NoteDirectory;
	    ASync.ConfigDir := Sett.LocalConfig;
        ASync.DebugMode := Application.HasOption('s', 'debug-sync');
	    ASync.RepoAction:= RepoJoin;
        ButtonHelp.SetFocus;
	    case Async.SetTransport(SyncFileAndroid) of
	            SyncReady : // We found a server id, but does it match a manifest ?
                    if FileExistsUTF8(Sett.LocalConfig + 'android' + pathdelim
                            + copy(Async.LocalServerID, 1, 13) + 'manifest.xml') then begin
	                            ButtonClose.Enabled := True;
	                            ButtonSync.Enabled := True;
                                ButtonJoin.Enabled := False;
                                LabelAdvice1.Caption := rsHaveValidSync
                                    + GetLocalManifestDateS(Async.LocalServerID);
                                LabelAdvice2.Caption := Async.LocalServerID;
                                ButtonSync.SetFocus;
				            end else begin
	                            ButtonClose.Enabled := True;
	                            ButtonSync.Enabled := False;
                                ButtonJoin.Enabled := True;
                                LabelAdvice1.Caption := rsNotCorrectProfile;
                                LabelAdvice2.Caption := rsJoinAnyway;
							end;
	            SyncNoRemoteRepo : begin  // We found the necessary dir but it has not been used before.
	                        ButtonClose.Enabled := True;
	                        ButtonSync.Enabled := False;
	                        ButtonJoin.Enabled := True;
                            LabelAdvice1.Caption := rsNotExistingRepo;
                            LabelAdvice2.Caption := rsFailedToFindConnection_2;
                            ButtonJoin.SetFocus;
				            end;
	            SyncNoRemoteDir : begin   // Dir is not there, maybe no device connected ?
	                        ButtonClose.Enabled := True;
	                        ButtonSync.Enabled := False;
	                        ButtonJoin.Enabled := False;
                            LabelAdvice1.Caption := rsNoTomdroid;
                            LabelAdvice2.Caption := rsInstallTomdroid;
				            end;
		end;
	    ASync.Free;
end;

function TFormTomdroidFile.GetLocalManifestDateS(ID : string) : string;
var
    Doc : TXMLDocument;
    Node : TDOMNode;
begin
	ReadXMLFile(Doc, Sett.LocalConfig + 'android' + pathdelim + copy(ID, 1, 13) + 'manifest.xml');
    Node := Doc.DocumentElement.FindNode('last-sync-date');
    if assigned(Node) then begin
        Result := copy(Node.FirstChild.NodeValue, 1, 19);
    end else
        Result := 'Failed to get LSD';
    Doc.Free;
end;

procedure TFormTomdroidFile.FormShow(Sender: TObject);
begin
    //debugln('Tomdroid screen OnShow event');
    left := (screen.Width div 2) - (width div 2);
    top := (screen.Height div 2) - (height div 2);
    Memo1.Clear;
    StringGridReport.Clear;
    CheckStatus();
end;


procedure TFormTomdroidFile.ButtonHelpClick(Sender: TObject);
begin
    SearchForm.ShowHelpNote('tomdroid.note');
end;


{ --------------- S C R E E N   F U N C T I O N S    ---------------------------}

//RESOURCESTRING
//  rsSelectProfile = 'Select a profile';

{ --------------- S Y N C    R E L A T E D    F U N C T I O N S ----------------}

procedure TFormTomdroidFile.ButtonSyncClick(Sender: TObject);
begin
    ButtonClose.Enabled := False;
    DoSync();
    ButtonClose.Enabled := True;
end;


procedure TFormTomdroidFile.DoNewSync();
var
    Tick1, Tick2, Tick3, Tick4 : QWord;
begin
    Memo1.clear;
    StringGridReport.Clear;
    Memo1.append(rsSetUpNewSync);
    Application.ProcessMessages;
    try
        ASync := TSync.Create();
        ASync.DebugMode := Application.HasOption('s', 'debug-sync');
        ASync.TestRun := CheckBoxTestRun.Checked;
        ASync.ProceedFunction:=@Proceed;
        ASync.NotesDir:= Sett.NoteDirectory;
        ASync.ConfigDir := Sett.LocalConfig;
        ASync.SyncAddress := '';
        // ASync.LocalServerID := LabelServerID.Caption;       // Only do this for Tomdroid Use!
        ASync.RepoAction:= RepoJoin;
        ASync.Password:= '';
        Tick1 := GetTickCount64();
        if SyncNetworkError = Async.SetTransport(SyncFileAndroid) then begin      // will set sync.manprefix, is sync.localserverID set ?
            memo1.append(rsFailedToConnect + ' ' + ASync.ErrorString);
            exit();
        end;
        Memo1.Append(rsTalking);
        Application.ProcessMessages;
        Tick2 := GetTickCount64();
        case ASync.TestConnection() of
            SyncNoRemoteDir :
                begin
                    Memo1.append(rsNoTomdroid );
                    Memo1.append(rsInstallTomdroid);
                    Memo1.Append(ASync.Errorstring);
                    exit();
                end;
            SyncNetworkError :
                begin
                    Memo1.Append(rsNoConnection + ' ' + ASync.ErrorString);
                    memo1.append(rsFixConnection);
                    exit();
                end;
            SyncReady, SyncNoRemoteRepo : ;         // For TransFileAnd its SyncNoRemoteRepo
            else begin showmessage(ASync.ErrorString); exit(); end;
        end;
        // If to here, sync should be enabled and know about remote files it might need.
        Memo1.append(rsConnectionGood);
        Memo1.append(rsNextBitSlow);
        Application.ProcessMessages;
        Tick3 := GetTickCount64();
        ASync.StartSync();
        //LabelServerID.Caption := ASync.LocalServerID;
        Tick4 := GetTickCount64();
        DisplaySync();
        memo1.Append('Set=' + inttostr(Tick2 - Tick1) + 'mS Test=' + inttostr(Tick3 - Tick2) + 'mS Sync=' + inttostr(Tick4 - Tick3) + 'mS ');
        ShowReport();
        AdjustNoteList();
    finally
      ASync.Free;
    end;
    if not CheckBoxTestRun.Checked then             // don't write a config if its only a test run.
       ;                                            //  do something here ??
end;

procedure TFormTomdroidFile.AdjustNoteList();
var
    DeletedList, DownList : TStringList;
    Index : integer;
begin
    DeletedList := TStringList.Create;
    DownList := TStringList.Create;
 	with ASync.RemoteMetaData do begin
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

procedure TFormTomdroidFile.ButtonJoinClick(Sender: TObject);
begin
    DoNewSync();
    ButtonSync.Enabled := True;
end;

procedure TFormTomdroidFile.ButtonOldSSHClick(Sender: TObject);
var
    TomdroidForm : TFormTomdroid;
begin
    TomdroidForm := TFormTomdroid.Create(self);
    TomdroidForm.ShowModal;
    TomdroidForm.Free;
end;

procedure TFormTomdroidFile.DisplaySync();
var
    UpNew, UpEdit, Down, DelLoc, DelRem, Clash, DoNothing, Errors : integer;
begin
    ASync.ReportMetaData(UpNew, UpEdit, Down, DelLoc, DelRem, Clash, DoNothing, Errors);
    Memo1.Append(rsNewUploads + inttostr(UpNew));
    Memo1.Append(rsEditUploads + inttostr(UpEdit));
    Memo1.Append(rsDownloads + inttostr(Down));
    Memo1.Append(rsLocalDeletes + inttostr(DelLoc));
    Memo1.Append(rsRemoteDeletes + inttostr(DelRem));
    Memo1.Append(rsClashes + inttostr(Clash));
    Memo1.Append(rsDoNothing + inttostr(DoNothing));
end;

procedure TFormTomdroidFile.ShowReport();
var
        Index : integer;
        Rows : integer = 0;
begin
    StringGridReport.Clean;
 	with ASync.RemoteMetaData do begin
		for Index := 0 to Count -1 do begin
            if Items[Index]^.Action <> SyNothing then begin
                    StringGridReport.InsertRowWithValues(Rows
            	        , [ASync.RemoteMetaData.ActionName(Items[Index]^.Action)
                        , Items[Index]^.Title, Items[Index]^.ID]);
                    inc(Rows);
            end;
		end
	end;
    StringGridReport.AutoSizeColumn(0);
    StringGridReport.AutoSizeColumn(1);
    if  Rows = 0 then
        Memo1.Append(rsNoNotesNeededSync);
    Memo1.Append(inttostr(ASync.RemoteMetaData.Count) + rsNotesWereDealt);
end;

function TFormTomdroidFile.DoSync() : boolean;
var
    Tick1, Tick2, Tick3, Tick4 : DWord;
begin
    Memo1.clear;
    StringGridReport.Clear;
    //Memo1.append(rsCheckingForExistingSync);            // no point ....
    //Application.ProcessMessages;
    try
        ASync := TSync.Create();
        ASync.DebugMode := Application.HasOption('s', 'debug-sync');
        ASync.TestRun := CheckBoxTestRun.Checked;
        ASync.ProceedFunction:=@Proceed;
        ASync.NotesDir:= Sett.NoteDirectory;
        ASync.ConfigDir := Sett.LocalConfig;
        ASync.SyncAddress := '';
        ASync.RepoAction:= RepoUse;
        ASync.Password:= '';
        Tick1 := GetTickCount64();
        if SyncNetworkError = Async.SetTransport(SyncFileAndroid) then begin      // this just pings remote dev
            memo1.append(rsFailedToConnect + ASync.ErrorString);
            exit(false);
        end;
//        Memo1.Append(rsTalkingToDevice);
//        Application.ProcessMessages;
        Tick2 := GetTickCount64();
        case ASync.TestConnection() of
            // SyncXMLError, SyncNoRemoteWrite, SyncNoRemoteDir :
            SyncNoLocal :
                begin Memo1.Append(ASync.ErrorString); Memo1.Append('Sync is cancelled'); exit(False); end;
                // That may be caused by a previous failure to complete a Join or New, look for bad notes perhaps ?
            SyncNoRemoteRepo :
                begin Memo1.Append(rsNotExistingRepo); exit(False); end;
{            SyncMisMatch :
                begin Memo1.Append(rsNotCorrectProfile); exit(False); end;   }
{            SyncNetworkError :
                begin
                    Memo1.Append(rsFailedToFindConnection_1 + ASync.ErrorString);
                    memo1.append(rsFailedToFindConnection_2);
                    memo1.append(rsFailedToFindConnection_3);
                    exit(false);
                end;    }
            SyncReady : ;
        else begin showmessage(ASync.ErrorString); exit(False); end;
        end;
        // If to here, sync should be enabled and know about remote files it might need.
        Memo1.append(rsHaveValidSync + ASync.LocalLastSyncDateSt);
        Memo1.append(rsNextBitSlow);
        Application.ProcessMessages;
        Tick3 := GetTickCount64();
        ASync.StartSync();
        Tick4 := GetTickCount64();
        DisplaySync();
        memo1.Append('Set=' + inttostr(Tick2 - Tick1) + 'mS Test=' + inttostr(Tick3 - Tick2) + 'mS Sync=' + inttostr(Tick4 - Tick3) + 'mS ');
        ShowReport();
        AdjustNoteList();
    finally
      ASync.Free;
    end;
    result := True;
end;

function TFormTomdroidFile.Proceed(const ClashRec : TClashRecord) : TSyncAction;
var
    SDiff : TFormSDiff;
    Res : integer;
begin
    result := SyDownload;
    SDiff := TFormSDiff.Create(self);
    SDiff.RemoteFilename := ClashRec.ServerFileName;
    SDiff.LocalFilename := ClashRec.LocalFileName;
    Res := SDiff.ShowModal;
    case Res of
            mrYes      : Result := SyDownLoad;
            mrNo       : Result := SyUpLoadEdit;
            mrNoToAll  : Result := SyAllLocal;
            mrYesToAll : Result := SyAllRemote;
            mrAll      : Result := SyAllNewest;
            mrClose    : Result := SyAllOldest;
     end;
    SDiff.Free;
    Application.ProcessMessages;    // so dialog goes away while remainder are being processed.
end;

end.

