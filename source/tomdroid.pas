unit tomdroid;

{$mode objfpc}{$H+}


{ HISTORY
    2018/12/06  Added AdjustNoteList() to call ProcessSyncUpdates at end of a sync
    2018/04/28  Ensure user does not save profile after a Test run, the ID will change.
    2019/05/14  Display strings all (?) moved to resourcestrings
    2020/07/09  New help notes location.
}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
    StdCtrls, ExtCtrls, Grids, LCLIntf,
    SyncUtils;    // For TClashRec

type

    { TFormTomdroid }

    TFormTomdroid = class(TForm)
        ButtonClose: TButton;
        ButtonJoin: TButton;
        ButtonDelete: TButton;
        ButtonSaveProfile: TButton;
        ButtonHelp: TButton;
        ButtonSync: TButton;
        CheckBoxTestRun: TCheckBox;
        CheckBoxDebugMode: TCheckBox;
        CheckSavePassword: TCheckBox;
        ComboBox1: TComboBox;
        EditProfileName: TEdit;
        EditPassword: TEdit;
        EditIPAddress: TEdit;
        Label1: TLabel;
        Label2: TLabel;
        Label6: TLabel;
        LabelServerID: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Memo1: TMemo;
        Panel1: TPanel;
        Panel2: TPanel;
        Splitter1: TSplitter;
        StringGridReport: TStringGrid;
        procedure ButtonDeleteClick(Sender: TObject);
        procedure ButtonHelpClick(Sender: TObject);
        procedure ButtonJoinClick(Sender: TObject);
        procedure ButtonSaveProfileClick(Sender: TObject);
        procedure ButtonSyncClick(Sender: TObject);
        procedure ComboBox1Select(Sender: TObject);
        procedure EditProfileNameChange(Sender: TObject);
        procedure FormShow(Sender: TObject);
    private
        ProfileName, IPAddress, Password : string;    // Keep copies to see if user changed after selection
        procedure AdjustNoteList();
        procedure ClearFields();
        procedure DisplaySync();
        procedure DoNewSync();
        procedure EnableButtons(const Enable: boolean);
        procedure LoadProfile(Profile: string);
        function NeedToSave(): boolean;
        function Proceed(const ClashRec: TClashRecord): TSyncAction;
        function ReadConfig(): boolean;
        function SaveCurrentProfile(const JustDelete : boolean = false): boolean;
        procedure ShowReport();
        function DoSync(): boolean;
    public

    end;

var
    FormTomdroid: TFormTomdroid;

implementation

{$R *.lfm}

{ TFormTomdroid }


uses Settings, IniFiles, Sync, TB_SDiff, typInfo, LazLogger, LCLType,
    SearchUnit; // we call ProcessSyncUpdates( and ShowHelpNote(

var
    ASync : TSync;

function TFormTomdroid.ReadConfig() : boolean;
var
    ConfigFile : TINIFile;
begin
    if not FileExists(Sett.LocalConfig + 'android' + pathdelim + 'tomdroid.cfg') then begin
        // showmessage('did not find ' + Sett.LocalConfig + 'android' + pathdelim + 'tomdroid.cfg');
        exit(False);
    end;
    ConfigFile := TINIFile.Create(Sett.LocalConfig + 'android' + pathdelim + 'tomdroid.cfg');
    ConfigFile.ReadSections(ComboBox1.Items);
    FreeandNil(ConfigFile);
    Result := True;
end;

function TFormTomdroid.SaveCurrentProfile(const JustDelete : boolean = false) : boolean;
var
    ConfigFile : TINIFile;
    Profile : string;
begin
    Profile := EditProfileName.Text;
    ConfigFile := TINIFile.Create(Sett.LocalConfig + 'android' + pathdelim + 'tomdroid.cfg');
    if ConfigFile.SectionExists(Profile) then ConfigFile.EraseSection(Profile);
    if JustDelete then exit(True);
    ConfigFile.WriteString(Profile, 'IP', EditIPAddress.Text);
    if CheckSavePassword.Checked then begin
        ConfigFile.WriteString(Profile, 'Password', EditPassword.Text);
        ConfigFile.WriteString(Profile, 'SavePassword', 'true');
    end;
    ConfigFile.WriteString(Profile, 'ServerID', LabelServerID.Caption);
    freeandnil(ConfigFile);
    Result := True;
end;

procedure TFormTomdroid.FormShow(Sender: TObject);
begin
    //debugln('Tomdroid screen OnShow event');
    left := (screen.Width div 2) - (width div 2);
    top := (screen.Height div 2) - (height div 2);
    Memo1.Clear;
    StringGridReport.Clear;
    ClearFields();
    ReadConfig;
    ButtonSaveProfile.Enabled := False;
    ButtonDelete.Enabled := False;
    ButtonSync.Enabled := False;
    ButtonJoin.Enabled := False;
end;

procedure TFormTomdroid.ButtonSaveProfileClick(Sender: TObject);
begin
    SaveCurrentProfile();
    ButtonSaveProfile.Enabled := False;
    ReadConfig();
end;

procedure TFormTomdroid.ButtonHelpClick(Sender: TObject);
begin
    //MainUnit.MainForm.ShowHelpNote('tomdroid.note');         // change the bloody name ! Er, why ?
    SearchForm.ShowHelpNote('tomdroid.note');
end;

procedure TFormTomdroid.ButtonDeleteClick(Sender: TObject);
begin
    SaveCurrentProfile(True);
    ReadConfig();
    ClearFields();
    Memo1.Clear;
    StringGridReport.clear;
end;

procedure TFormTomdroid.LoadProfile(Profile : string);
var
    ConfigFile : TINIFile;
begin
    ConfigFile := TINIFile.Create(Sett.LocalConfig + 'android' + pathdelim + 'tomdroid.cfg');
    if ConfigFile.SectionExists(Profile) then begin
        EditProfileName.Text := Profile;
        ProfileName := Profile;
        EditIPAddress.Text := ConfigFile.readstring(Profile, 'IP', 'Oh, an Error occured');
        IPAddress := EditIPAddress.Text;
        EditPassword.Text := ConfigFile.readstring(Profile, 'Password', '');
        Password := EditPassword.Text;
        CheckSavePassword.Checked := ('true' = ConfigFile.readstring(Profile, 'SavePassword', ''));
        LabelServerID.Caption := ConfigFile.readstring(Profile, 'ServerID', 'ERROR - profile has no server ID');
        ButtonSync.Enabled := IDLooksOK(LabelServerID.Caption);
    end else showmessage('WTF ?  Cannot find profile in config ' + Profile);
    freeandnil(ConfigFile);
    ButtonSaveProfile.Enabled := False;
    ButtonJoin.Enabled := False;
    ButtonDelete.Enabled := True;
end;

procedure TFormTomdroid.ComboBox1Select(Sender: TObject);
begin
    LoadProfile(combobox1.items[combobox1.ItemIndex]);
end;

        // Responds to changes to any of the three EditBoxes
procedure TFormTomdroid.EditProfileNameChange(Sender: TObject);
begin
    if (EditProfileName.Text <> '') and (EditIPAddress.Name <> '')
                                        and (EditPassword.Text <> '')  then begin
        ButtonJoin.Enabled := True;
        ButtonDelete.Enabled := False;
        // A Join is OK if we have no existing config, a Sync is definitly not
        if FileExists(Sett.LocalConfig + 'android' + pathdelim + 'tomdroid.cfg') then
            ButtonSync.Enabled := True;
    end;
end;

{ --------------- S C R E E N   F U N C T I O N S    ---------------------------}

RESOURCESTRING
  rsSelectProfile = 'Select a profile';

procedure TFormTomdroid.ClearFields();
begin
    ComboBox1.Text := rsSelectProfile;
    EditProfileName.Text := '';
    EditIPAddress.Text := '';
    EditPassword.Text := '';
    CheckSavePassword.Checked := false;
    CheckBoxTestRun.Checked := false;
    LabelServerID.Caption := '';
end;

procedure TFormTomdroid.EnableButtons(const Enable : boolean);
begin
    ButtonClose.Enabled := Enable;
    // ButtonSaveProfile.Enabled := Enable;
    ButtonDelete.Enabled := Enable;
    ButtonSync.Enabled := Enable;
    ButtonHelp.Enabled := Enable;
    ButtonJoin.Enabled:= Enable;
end;


{ --------------- S Y N C    R E L A T E D    F U N C T I O N S ----------------}

procedure TFormTomdroid.ButtonSyncClick(Sender: TObject);
begin
    ButtonClose.Enabled := False;
    // Todo - prevent a sync proceeding if its a new name in the edit box, that is, does not match the config file.
    // Compare the profile in config file with one in edit boxes, any change, fail !!
    DoSync();
    ButtonClose.Enabled := True;
end;

RESOURCESTRING
  rsSetUpNewSync ='Setting up a new sync ....';
  rsFailedToConnect = 'Failed to connect.';
  rsTalking = 'OK, talking to device. Wait for it ....';
  rsNoTomdroid =  'Unable to find Tomdroid sync dir on that device.';
  rsInstallTomdroid = 'Install Tomdroid, config filesync, and run a sync';
  rsNoConnection = 'Failed to establish a connection. ';
  rsFixConnection = 'If you are sure its there, check settings.';
  rsConnectionGood = 'Connection is looking Good.';


procedure TFormTomdroid.DoNewSync();
var
    Tick1, Tick2, Tick3, Tick4 : QWord;
begin
    Memo1.clear;
    StringGridReport.Clear;
    EnableButtons(False);
    Memo1.append(rsSetUpNewSync);
    Application.ProcessMessages;
    try
        ASync := TSync.Create();
        ASync.DebugMode:=CheckBoxDebugMode.Checked;
        ASync.TestRun := CheckBoxTestRun.Checked;
        ASync.ProceedFunction:=@Proceed;
        ASync.NotesDir:= Sett.NoteDirectory;
        ASync.ConfigDir := Sett.LocalConfig;
        ASync.SyncAddress := EditIPAddress.Text;
        // ASync.LocalServerID := LabelServerID.Caption;       // Only do this for Tomdroid Use!
        ASync.RepoAction:= RepoJoin;
        ASync.Password:= EditPassword.Text;
        Tick1 := GetTickCount64();
        if SyncNetworkError = Async.SetTransport(SyncAndroid) then begin
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
            SyncReady : ;
        else begin showmessage(ASync.ErrorString); exit(); end;
        end;
        // If to here, sync should be enabled and know about remote files it might need.
        Memo1.append(rsConnectionGood);
        Memo1.append(rsNextBitSlow);
        Application.ProcessMessages;
        Tick3 := GetTickCount64();
        ASync.StartSync();
        LabelServerID.Caption := ASync.LocalServerID;
        Tick4 := GetTickCount64();
        DisplaySync();
        memo1.Append('Set=' + inttostr(Tick2 - Tick1) + 'mS Test=' + inttostr(Tick3 - Tick2) + 'mS Sync=' + inttostr(Tick4 - Tick3) + 'mS ');
        ShowReport();
        AdjustNoteList();
    finally
      ASync.Free;
      EnableButtons(True);
    end;
    if not CheckBoxTestRun.Checked then             // don't write a config if its only a test run.
        ButtonSaveProfile.Enabled := NeedToSave();
end;

procedure TFormTomdroid.AdjustNoteList();
var
    DeletedList, DownList : TStringList;
    Index : integer;
begin
    DeletedList := TStringList.Create;
    DownList := TStringList.Create;
 	with ASync.NoteMetaData do begin
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

procedure TFormTomdroid.ButtonJoinClick(Sender: TObject);
begin
    if ProfileName <> '' then begin
        if (ProfileName = EditProfileName.Text) then
            if IDYES <> Application.MessageBox(pchar(rsChangeExistingSync),
                    pchar(rsNotRecommend), MB_ICONQUESTION + MB_YESNO) then exit;
    end;
    EnableButtons(False);
    DoNewSync();
    ButtonSaveProfile.Enabled := True; // NeedToSave();
    ButtonDelete.Enabled := True;
    ButtonSync.Enabled := True;

end;

 // Following resourcestrings defined in syncUtils.pas

procedure TFormTomdroid.DisplaySync();
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

procedure TFormTomdroid.ShowReport();
var
        Index : integer;
        Rows : integer = 0;
begin
    StringGridReport.Clean;
 	with ASync.NoteMetaData do begin
		for Index := 0 to Count -1 do begin
            if Items[Index]^.Action <> SyNothing then begin
                    StringGridReport.InsertRowWithValues(Rows
            	        , [ASync.NoteMetaData.ActionName(Items[Index]^.Action)
                        , Items[Index]^.Title, Items[Index]^.ID]);
                    inc(Rows);
            end;
		end
	end;
    StringGridReport.AutoSizeColumn(0);
    StringGridReport.AutoSizeColumn(1);
    if  Rows = 0 then
        Memo1.Append(rsNoNotesNeededSync);
    Memo1.Append(inttostr(ASync.NoteMetaData.Count) + rsNotesWereDealt);
end;

function TFormTomdroid.NeedToSave() : boolean;
begin
    Result := (ProfileName <> EditProfileName.Text)
                    or (IPAddress <> EditIPAddress.Text)
                    or (Password <> EditPassword.Text);
end;

RESOURCESTRING
  rsCheckingForExistingSync = 'Checking for an existing sync ....';
  rsTalkingToDevice = 'OK, talking to device. Wait for it ....';
  rsNotExistingRepo = 'That''s not an existing Repo, maybe click "Join" ?';
  rsNotCorrectProfile = 'This is not correct profile for that device';
  rsFailedToFindConnection_1 = 'Failed to find an existing connection.';
  rsFailedToFindConnection_2 = 'If you are sure there should be an existing connection, check settings.';
  rsFailedToFindConnection_3 = 'Otherwise, try joining a new connection.';
  rsHaveValidSync = 'Looking Good. Last sync date ';


function TFormTomdroid.DoSync() : boolean;
var
    Tick1, Tick2, Tick3, Tick4 : DWord;
begin
    Memo1.clear;
    StringGridReport.Clear;
    EnableButtons(False);
    Memo1.append(rsCheckingForExistingSync);
    Application.ProcessMessages;
    try
        ASync := TSync.Create();
        ASync.DebugMode:=CheckBoxDebugMode.Checked;
        ASync.TestRun := CheckBoxTestRun.Checked;
        ASync.ProceedFunction:=@Proceed;
        ASync.NotesDir:= Sett.NoteDirectory;
        ASync.ConfigDir := Sett.LocalConfig;
        ASync.SyncAddress := EditIPAddress.Text;
        ASync.LocalServerID := LabelServerID.Caption;       // Only do this for Tomdroid !
        ASync.RepoAction:= RepoUse;
        ASync.Password:= EditPassword.Text;
        Tick1 := GetTickCount64();
        if SyncNetworkError = Async.SetTransport(SyncAndroid) then begin      // this just pings remote dev
            memo1.append(rsFailedToConnect + ASync.ErrorString);
            exit(false);
        end;
        Memo1.Append(rsTalkingToDevice);
        Application.ProcessMessages;
        Tick2 := GetTickCount64();
        case ASync.TestConnection() of
            // SyncXMLError, SyncNoRemoteWrite, SyncNoRemoteDir :
            SyncNoLocal :
                begin Memo1.Append(ASync.ErrorString); Memo1.Append('Sync is cancelled'); exit(False); end;
                // That may be caused by a previous failure to complete a Join or New, look for bad notes perhaps ?
            SyncNoRemoteRepo :
                begin Memo1.Append(rsNotExistingRepo); exit(False); end;
            SyncMisMatch :
                begin Memo1.Append(rsNotCorrectProfile); exit(False); end;
            SyncNetworkError :
                begin
                    Memo1.Append(rsFailedToFindConnection_1 + ASync.ErrorString);
                    memo1.append(rsFailedToFindConnection_2);
                    memo1.append(rsFailedToFindConnection_3);
                    exit(false);
                end;
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
      EnableButtons(True);
    end;
    ButtonSaveProfile.Enabled := NeedToSave();
    result := True;
end;



function TFormTomdroid.Proceed(const ClashRec : TClashRecord) : TSyncAction;
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

