unit tomdroid;

{$mode objfpc}{$H+}

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
    MainUnit;   // just for MainUnit.MainForm.ShowHelpNote(

var
    ASync : TSync;

function TFormTomdroid.ReadConfig() : boolean;
var
    ConfigFile : TINIFile;
begin
    if not FileExists(Sett.LocalConfig + 'android' + pathdelim + 'tomdroid.cfg') then begin
        showmessage('did not find ' + Sett.LocalConfig + 'android' + pathdelim + 'tomdroid.cfg');
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
    debugln('Tomdroid screen OnShow event');
    Memo1.Clear;
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
    MainUnit.MainForm.ShowHelpNote('recover.note');         // change the bloody name !
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
        ButtonSync.Enabled := True;
        ButtonDelete.Enabled := False;
    end;
end;

{ --------------- S C R E E N   F U N C T I O N S    ---------------------------}

procedure TFormTomdroid.ClearFields();
begin
    ComboBox1.Text := 'Select a profile';
    EditProfileName.Text := '';
    EditIPAddress.Text := '';
    EditPassword.Text := '';
    CheckSavePassword.Checked := false;
    CheckBoxTestRun.Checked := true;
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
    DoSync();
    ButtonClose.Enabled := True;
end;

procedure TFormTomdroid.DoNewSync();
var
    Tick1, Tick2, Tick3, Tick4 : DWord;
begin
    Memo1.clear;
    StringGridReport.Clear;
    EnableButtons(False);
    Memo1.append('Setting up a new sync ....');
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
            memo1.append('Failed to connect. ' + ASync.ErrorString);
            exit();
        end;
        Memo1.Append('OK, talking to device. Wait for it ....');
        Application.ProcessMessages;
        Tick2 := GetTickCount64();
        case ASync.TestConnection() of
            SyncNoRemoteDir :
                begin
                    Memo1.append('Unable to find Tomdroid sync dir on that device.');
                    Memo1.append('Install Tomdroid, config filesync, and run a sync');
                    Memo1.Append(ASync.Errorstring);
                    exit();
                end;
            SyncNetworkError :
                begin
                    Memo1.Append('Failed to establish a connection. ' + ASync.ErrorString);
                    memo1.append('If you are sure its there, check settings.');
                    exit();
                end;
            SyncReady : ;
        else begin showmessage(ASync.ErrorString); exit(); end;
        end;
        // If to here, sync should be enabled and know about remote files it might need.
        Memo1.append('Connection is looking Good.');
        Memo1.append('Next bit can be a bit slow, please wait');
        Application.ProcessMessages;
        Tick3 := GetTickCount64();
        ASync.StartSync();
        LabelServerID.Caption := ASync.LocalServerID;
        Tick4 := GetTickCount64();
        DisplaySync();
        memo1.Append('Set=' + inttostr(Tick2 - Tick1) + 'mS Test=' + inttostr(Tick3 - Tick2) + 'mS Sync=' + inttostr(Tick4 - Tick3) + 'mS ');
        ShowReport();
    finally
      ASync.Free;
      EnableButtons(True);
    end;
    ButtonSaveProfile.Enabled := NeedToSave();
end;

procedure TFormTomdroid.ButtonJoinClick(Sender: TObject);
begin
    if ProfileName <> '' then begin
        if (ProfileName = EditProfileName.Text) then
            if IDYES <> Application.MessageBox('Change an Existing Sync Connection ?',
                			'Generally not recommended.',
                   			MB_ICONQUESTION + MB_YESNO) then exit;
    end;
    EnableButtons(False);
    DoNewSync();
    ButtonSaveProfile.Enabled := True; // NeedToSave();
    ButtonDelete.Enabled := True;
    ButtonSync.Enabled := True;

end;

procedure TFormTomdroid.DisplaySync();
var
    UpNew, UpEdit, Down, DelLoc, DelRem, Clash, DoNothing, Errors : integer;
begin
    ASync.ReportMetaData(UpNew, UpEdit, Down, DelLoc, DelRem, Clash, DoNothing, Errors);
    Memo1.Append('New Uploads    ' + inttostr(UpNew));
    Memo1.Append('Edit Uploads   ' + inttostr(UpEdit));
    Memo1.Append('Downloads      ' + inttostr(Down));
    Memo1.Append('Local Deletes  ' + inttostr(DelLoc));
    Memo1.Append('Remote Deletes ' + inttostr(DelRem));
    Memo1.Append('Clashes        ' + inttostr(Clash));
    Memo1.Append('Do Nothing     ' + inttostr(DoNothing));
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
        Memo1.Append('No notes needed syncing. You need to write more.');
    Memo1.Append(inttostr(ASync.NoteMetaData.Count) + ' notes were dealt with.');
end;

function TFormTomdroid.NeedToSave() : boolean;
begin
    Result := (ProfileName <> EditProfileName.Text)
                    or (IPAddress <> EditIPAddress.Text)
                    or (Password <> EditPassword.Text);
end;

function TFormTomdroid.DoSync() : boolean;
var
    Tick1, Tick2, Tick3, Tick4 : DWord;
begin
    Memo1.clear;
    StringGridReport.Clear;
    EnableButtons(False);
    Memo1.append('Checking for an existing sync ....');
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
        if SyncNetworkError = Async.SetTransport(SyncAndroid) then begin
            memo1.append('Failed to connect. ' + ASync.ErrorString);
            exit(false);
        end;
        Memo1.Append('OK, talking to device. Wait for it ....');
        Application.ProcessMessages;
        Tick2 := GetTickCount64();
        case ASync.TestConnection() of
            // SyncXMLError, SyncNoRemoteWrite, SyncNoRemoteDir :
            SyncNoRemoteRepo :
                begin Memo1.Append('Thats not an existing Repo, maybe click "Join" ?'); exit(False); end;
            SyncMisMatch :
                begin Memo1.Append('This is not correct profile for that device'); exit(False); end;
            SyncNetworkError :
                begin
                    Memo1.Append('Failed to find an existing connection. ' + ASync.ErrorString);
                    memo1.append('If you are sure there should be an existing connection, check settings.');
                    memo1.append('Otherwise, try joining a new connection.');
                    exit(false);
                end;
            SyncReady : ;
        else begin showmessage(ASync.ErrorString); exit(False); end;
        end;
        // If to here, sync should be enabled and know about remote files it might need.
        Memo1.append('Looking Good. Last sync date ' + ASync.LocalLastSyncDateSt);
        Memo1.append('Next bit can be a bit slow, please wait');
        Application.ProcessMessages;
        Tick3 := GetTickCount64();
        ASync.StartSync();
        Tick4 := GetTickCount64();
        DisplaySync();
        memo1.Append('Set=' + inttostr(Tick2 - Tick1) + 'mS Test=' + inttostr(Tick3 - Tick2) + 'mS Sync=' + inttostr(Tick4 - Tick3) + 'mS ');
        ShowReport();
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
end;

end.

