unit tomdroid;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
    StdCtrls, ExtCtrls, Grids,
    SyncUtils;    // For TClashRec

type

    { TFormTomdroid }

    TFormTomdroid = class(TForm)
        ButtonClose: TButton;
        ButtonSaveProfile: TButton;
        ButtonHelp: TButton;
        ButtonSync: TButton;
        CheckSavePassword: TCheckBox;
        ComboBox1: TComboBox;
        EditProfileName: TEdit;
        EditPassword: TEdit;
        EditIPAddress: TEdit;
        Label1: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Memo1: TMemo;
        Panel1: TPanel;
        Panel2: TPanel;
        Splitter1: TSplitter;
        StringGridReport: TStringGrid;
        procedure ButtonHelpClick(Sender: TObject);
        procedure ButtonSaveProfileClick(Sender: TObject);
        procedure ButtonSyncClick(Sender: TObject);
        procedure ComboBox1Select(Sender: TObject);
        procedure FormShow(Sender: TObject);
    private
        procedure ClearFields();
        procedure DisplaySync();
        procedure LoadProfile(Profile: string);
        function Proceed(const ClashRec: TClashRecord): TSyncAction;
        function ReadConfig(): boolean;
        function SaveCurrentProfile(): boolean;
        procedure ShowReport();
        function TrySync(): boolean;
    public

    end;

var
    FormTomdroid: TFormTomdroid;

implementation

{$R *.lfm}

{ TFormTomdroid }


uses Settings, IniFiles, Sync, TB_SDiff, typInfo, LazLogger,
    MainUnit;   // just for MainUnit.MainForm.ShowHelpNote(

var
    ASync : TSync;

function TFormTomdroid.ReadConfig() : boolean;
var
  ConfigFile : TINIFile;
  //Profiles : TStringsList;
begin
  if not FileExists(Sett.LocalConfig + 'android' + pathdelim + 'tomdroid.cfg') then begin
    showmessage('did not find ' + Sett.LocalConfig + 'android' + pathdelim + 'tomdroid.cfg');
    exit(False);
  end;
  ConfigFile := TINIFile.Create(Sett.LocalConfig + 'android' + pathdelim + 'tomdroid.cfg');
  //Profiles := TStringList.Create;
  ConfigFile.ReadSections(ComboBox1.Items);
  FreeandNil(ConfigFile);
end;

function TFormTomdroid.SaveCurrentProfile() : boolean;
var
  ConfigFile : TINIFile;
  Profile : string;
begin
  Profile := EditProfileName.Text;
  ConfigFile := TINIFile.Create(Sett.LocalConfig + 'android' + pathdelim + 'tomdroid.cfg');
  if ConfigFile.SectionExists(Profile) then ConfigFile.EraseSection(Profile);
  ConfigFile.WriteString(Profile, 'IP', EditIPAddress.Text);
  if CheckSavePassword.Checked then begin
    ConfigFile.WriteString(Profile, 'Password', EditPassword.Text);
    ConfigFile.WriteString(Profile, 'SavePassword', 'true');
    ConfigFile.WriteString(Profile, 'guid', 'guid');        // ????????????????
  end;
  freeandnil(ConfigFile);
  Result := True;
end;

procedure TFormTomdroid.FormShow(Sender: TObject);
begin
    ClearFields();
    if not ReadConfig then begin
      ;
    end;
end;

procedure TFormTomdroid.ButtonSaveProfileClick(Sender: TObject);
begin
    SaveCurrentProfile();
end;


procedure TFormTomdroid.ButtonHelpClick(Sender: TObject);
begin
    MainUnit.MainForm.ShowHelpNote('recover.note');         // change the bloody name !
end;

procedure TFormTomdroid.ClearFields();
begin
  ComboBox1.Text := 'Select a profile';
  EditProfileName.Text := '';
  EditIPAddress.Text := '';
  EditPassword.Text := '';
  CheckSavePassword.Checked := false;
end;

procedure TFormTomdroid.LoadProfile(Profile : string);
var
  ConfigFile : TINIFile;
begin
  ConfigFile := TINIFile.Create(Sett.LocalConfig + 'android' + pathdelim + 'tomdroid.cfg');
  if ConfigFile.SectionExists(Profile) then begin
    EditProfileName.Text := Profile;
    EditIPAddress.Text := ConfigFile.readstring(Profile, 'IP', 'Oh, an Error occured');
    EditPassword.Text := ConfigFile.readstring(Profile, 'Password', '');
    CheckSavePassword.Checked := ('true' = ConfigFile.readstring(Profile, 'SavePassword', ''));
  end else showmessage('WTF ?  Cannot find profile in config ' + Profile);
  freeandnil(ConfigFile);
end;

procedure TFormTomdroid.ComboBox1Select(Sender: TObject);
begin
    LoadProfile(combobox1.items[combobox1.ItemIndex]);
end;


{ --------------- S Y N C    R E L A T E D    F U N C T I O N S ----------------}

procedure TFormTomdroid.ButtonSyncClick(Sender: TObject);
begin
    TrySync();
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

function TFormTomdroid.TrySync() : boolean;
var
    Tick1, Tick2, Tick3, Tick4 : DWord;
begin
  Memo1.clear;
  Memo1.append('This process can be a bit slow .....');
  Application.ProcessMessages;
  try
      ASync := TSync.Create();
      ASync.DebugMode:=True;
      ASync.TestRun := True;
      ASync.ProceedFunction:=@Proceed;
      ASync.NotesDir:= Sett.NoteDirectory;
      ASync.ConfigDir := Sett.LocalConfig;
      ASync.SyncAddress := EditIPAddress.Text;
      ASync.RepoAction:= RepoUse;
      ASync.Password:= EditPassword.Text;
      Tick1 := GetTickCount64();
      if SyncNetworkError = Async.SetTransport(SyncAndroid) then begin
          memo1.append('Failed to connect ' + ASync.ErrorString);
          memo1.append('Check ssh server is running on device, IP address and password');
          exit(false);
      end;
      Memo1.Append('OK, talking to it. More to come ....');
      Application.ProcessMessages;
      Tick2 := GetTickCount64();
      if SyncReady <> ASync.TestConnection() then begin
            Memo1.Append('Failed to find an existing connection. ' + ASync.ErrorString);
            memo1.append('If you are sure there should be an existing connection, check settings.');
            memo1.append('Otherwise, try making a new connection.');
      end else begin
            // If to here, sync should be enabled and know about remote files it might need.
            Memo1.append('Last sync date ' + ASync.LocalLastSyncDateSt);
            Memo1.append('Next bit can be a bit slow, please wait');
            Application.ProcessMessages;
            Tick3 := GetTickCount64();
            ASync.StartSync();
            Tick4 := GetTickCount64();
            DisplaySync();
            memo1.Append(inttostr(Tick4 - Tick3) + 'mS ' + inttostr(Tick3 - Tick2) + 'mS ' + inttostr(Tick2 - Tick1) + 'mS');
            ShowReport();
      end;
  finally
      ASync.Free;
  end;
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

