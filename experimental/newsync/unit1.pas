unit Unit1;

{
    A basic GUI to facilitate testing of the sync model.
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls, Graphics,
    Dialogs, StdCtrls, Menus, ExtCtrls, Grids, SyncUtils;

type

    { TForm1 }

    TForm1 = class(TForm)
            ButtonSyncDir: TButton;
            ButtonNotesDir: TButton;
            ButtonConfigDir: TButton;
        ButtSyncExisting: TButton;
		ButJoinNew: TButton;
        CheckDevMode: TCheckBox;
		CheckBoxTestRun: TCheckBox;
        ComboTransport: TComboBox;
        ComboSyncLoc: TComboBox;
        ComboConfigDir: TComboBox;
        ComboNotesDir: TComboBox;
        EditPW: TEdit;
		Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
		LabelNotes: TLabel;
		LabelConfig: TLabel;
        LabelSync: TLabel;
        Memo1: TMemo;
        Panel1: TPanel;
        RadioButtonReal: TRadioButton;
        RadioButtonSource: TRadioButton;
        RadioButtonTest: TRadioButton;
        RadioButtonRemTest: TRadioButton;
        RadioGroup1: TRadioGroup;
        SelectDirectoryDialog1: TSelectDirectoryDialog;
        Splitter1: TSplitter;
        StringGridReport: TStringGrid;
        procedure ButtonConfigDirClick(Sender: TObject);
        procedure ButtonNotesDirClick(Sender: TObject);
        procedure ButtonSyncDirClick(Sender: TObject);
        procedure ButtSyncExistingClick(Sender: TObject);
		procedure ButJoinNewClick(Sender: TObject);
        procedure CheckDevModeChange(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormShow(Sender: TObject);
        procedure RadioButtonRealChange(Sender: TObject);
        procedure RadioButtonRemTestChange(Sender: TObject);
        procedure RadioButtonSourceChange(Sender: TObject);
        procedure RadioButtonTestChange(Sender: TObject);
        procedure RadioGroup1Click(Sender: TObject);
    private
        procedure DevMode(const Switch: boolean);
	    procedure DisplaySync();
        procedure ShowReport();
    public
        function Proceed(const ClashRec : TClashRecord) : TSyncAction;
    end;


var
    Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }
uses Sync, TB_SDiff, typInfo, LazLogger;

var
    ASync : TSync;

procedure TForm1.DisplaySync();
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

procedure TForm1.ShowReport();
var
        Index : integer;
begin
    StringGridReport.Clean;
        with ASync.NoteMetaData do begin
                for Index := 0 to Count -1 do begin
                StringGridReport.InsertRowWithValues(Index
                        , [ASync.NoteMetaData.ActionName(Items[Index]^.Action)
                    , Items[Index]^.Title, Items[Index]^.ID]);
                end
        end;
        StringGridReport.AutoSizeColumn(0);
        StringGridReport.AutoSizeColumn(1);
        if  ASync.NoteMetaData.Count = 0 then
            Memo1.Append('No notes needed syncing. You need to write more.')
        else Memo1.Append(inttostr(ASync.NoteMetaData.Count) + ' notes were dealt with.');
end;

procedure TForm1.ButtSyncExistingClick(Sender: TObject);
var
    Tick1, Tick2, Tick3, Tick4 : DWord;
begin
    Memo1.clear;
  {if SelectDirectoryDialog1.Execute then begin
       LabelSync.Caption := TrimFilename(SelectDirectoryDialog1.FileName + PathDelim);    }
    try
	    //ASync := TSync.Create('/home/dbannon/.local/share/tomboy-ng/', '/home/dbannon/.config/', SpinEdit1.Value);
	    ASync := TSync.Create();
	    ASync.DebugMode:=True;
	    ASync.TestRun := CheckBoxTestRun.Checked;
	    ASync.ProceedFunction:=@Proceed;
	    ASync.NotesDir:= ComboNotesDir.Text;
	    ASync.ConfigDir := ComboConfigDir.Text;
	    ASync.SyncAddress := ComboSyncLoc.Text;
        ASync.RepoAction:= RepoUse;
        ASync.Password:= EditPW.Text;
	    //ASync.CurrRev:=SpinEdit1.Value;
        Tick1 := GetTickCount64();
	    if SyncNetworkError = Async.SetTransport(TSyncTransport(ComboTransport.ItemIndex)) then begin
            showmessage('Failed to connect ' + ASync.ErrorString);
            exit;
        end;
        Tick2 := GetTickCount64();
        if SyncReady <> ASync.TestConnection() then begin
	        Memo1.Append('Failed to find an existing connection. ' + ASync.ErrorString);
	    end else begin
	        // If to here, sync should be enabled and know about remote files it might need.
           Memo1.append('Last sync date ' + ASync.LocalLastSyncDateSt);
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
end;


procedure TForm1.ButtonConfigDirClick(Sender: TObject);
begin
    if selectdirectorydialog1.Execute then
        ComboConfigDir.ItemIndex :=
            ComboConfigDir.Items.Add(trim(appendpathdelim(selectdirectorydialog1.FileName)));
end;

procedure TForm1.ButtonNotesDirClick(Sender: TObject);
begin
    if selectdirectorydialog1.Execute then
        ComboNotesDir.ItemIndex :=
            ComboNotesDir.Items.Add(trim(appendpathdelim(selectdirectorydialog1.FileName)));
end;


procedure TForm1.ButtonSyncDirClick(Sender: TObject);
var
    Index : integer;
begin
    if selectdirectorydialog1.Execute then
        ComboSyncLoc.ItemIndex :=
            ComboSyncLoc.Items.Add(trim(appendpathdelim(selectdirectorydialog1.FileName)));
end;

// Join (possibly forcing a (new) join) connection or, if its not there
// initialise that connection.

procedure TForm1.ButJoinNewClick(Sender: TObject);
var
    SyncState : TSyncAvailable;
    MR : integer;
    //UseLocalManifest : boolean = True;
    Tick1, Tick2, Tick3, Tick4 : DWord;
begin
    Memo1.Clear;
    Application.ProcessMessages;
	try
	    ASync := TSync.Create();
	    ASync.DebugMode:=True;
	    ASync.TestRun := CheckBoxTestRun.Checked;
	    ASync.ProceedFunction:=@Proceed;
	    ASync.NotesDir:= ComboNotesDir.Text;
	    ASync.ConfigDir := ComboConfigDir.Text;
	    ASync.SyncAddress := ComboSyncLoc.Text;
        ASync.RepoAction := RepoJoin;
        ASync.Password:= EditPW.Text;
        Tick1 := GetTickCount64();
        if SyncNetworkError = Async.SetTransport(TSyncTransport(ComboTransport.Itemindex))  then begin
            showmessage('No net connection');
            exit;
        end;
	    SyncState := Async.TestConnection();
        Tick2 := GetTickCount64();
        while SyncState <> SyncReady do
	    case SyncState of
        // Ask user if we should proceed if we are SyncMisMatch, SyncNoRemoteRepo
        // quietlty proceed if NoLocal (but log to debug)
        // We cannot handle SyncXMLError, SyncBadRemote, SyncNoRemoteWrite
	    {    SyncNoLocal : begin
                showmessage('No Local Manifest, Thats OK');
                ASync.UseLocalManifest := False;
                SyncState := ASync.TestConnection();
                end;    }
	        SyncNoRemoteRepo : begin ;
                    MR := QuestionDlg('Warning', 'Create a new Reop ?', mtConfirmation, [mrYes, mrNo], 'Blar');
                    if MR = mrYes then begin
                        ASync.RepoAction:= RepoNew;
                        SyncState := ASync.TestConnection();    // will try and create repo.
                        if SyncState = SyncNoRemoteRepo then
                            showmessage('ERROR no remote repo ' + Async.ErrorString);
					end else exit;
                end;
            {SyncMisMatch : begin
                    MR := QuestionDlg('Warning', 'Force Join of this Repo ?', mtConfirmation, [mrYes, mrNo], 'Blar');
                    if MR = mrYes then begin
                        ASync.UseLocalManifest := False;
                        SyncState := ASync.TestConnection();
                    end;
				end; }
	        SyncXMLError, SyncNoRemoteDir, SyncBadRemote, SyncNoRemoteWrite, SyncBadError : begin       // NOTE New SyncBadERROR !
                        showmessage(ASync.ErrorString);
                        exit;
                    end;
	    end;    // end of case statement
	    if SyncState = SyncReady then begin
	        if not ASync.StartSync() then
                showmessage('StartSync ERROR ' + Async.ErrorString);
            Tick3 := GetTickCount64();
	        DisplaySync();
            ShowReport();
		end else showmessage('Cancelling operation');
    finally
        ASync.Free;
    end;
    memo1.append('Test = ' + inttostr(Tick2 - Tick1) + 'mS and StartSync = ' + inttostr(Tick3 - Tick2) + 'mS');
end;

procedure TForm1.CheckDevModeChange(Sender: TObject);
begin
    DevMode(CheckDevMode.Checked);
end;

procedure TForm1.DevMode(const Switch : boolean);
begin
    ComboConfigDir.Enabled := Switch;
    ComboNotesDir.Enabled := switch;
    // ComboSyncLoc.Enabled := switch;
    ComboTransport.Enabled:=switch;
    ButtonNotesDir.Enabled := switch;
    ButtonSyncDir.Enabled := switch;
    ButtonConfigDir.Enabled := switch;
    RadioGroup1.Enabled:= switch;

    ComboSyncLoc.Clear;
    if Switch then begin
        ComboSyncLoc.Items.Add('sync');
        ComboSyncLoc.Items.Add(GetEnvironmentVariable('HOME') + '/TB_Test_Sync/');
        ComboSyncLoc.Items.Add('/run/user/1000/gvfs/smb-share:server=192.168.1.1,share=usbdisk/SanDisk_FirebirdUSBFlashDrive_1_ca4c/PascalTrans/Sync_TestRig/');
        ComboSyncLoc.Items.Add('/run/user/1000/gvfs/smb-share:server=192.168.1.1,share=usbdisk/SanDisk_FirebirdUSBFlashDrive_1_ca4c/PascalTrans/TB_NG_Sync/');
    end;
    ComboSyncLoc.Items.Add('192.168.1.174');
    ComboSyncLoc.Items.Add('192.168.1.174');
    ComboSyncLoc.ItemIndex := 0;
end;

procedure TForm1.RadioButtonSourceChange(Sender: TObject);
begin
    if RadioButtonSource.Checked then begin
        ComboNotesDir.ItemIndex := 0;
        ComboConfigDir.ItemIndex := 0;
        ComboSyncLoc.ItemIndex := 0;
    end;
end;

procedure TForm1.RadioButtonTestChange(Sender: TObject);
begin
    if RadioButtonTest.Checked then begin
        ComboNotesDir.ItemIndex := 1;
        ComboConfigDir.ItemIndex := 1;
        ComboSyncLoc.ItemIndex := 1;

    end;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin

end;

procedure TForm1.RadioButtonRemTestChange(Sender: TObject);
begin
    if RadioButtonRemTest.Checked then begin
        ComboNotesDir.ItemIndex := 1;
        ComboConfigDir.ItemIndex := 1;
        ComboSyncLoc.ItemIndex := 2;
    end;
end;

procedure TForm1.RadioButtonRealChange(Sender: TObject);
begin
    if RadioButtonReal.Checked then begin
        ComboNotesDir.ItemIndex := 2;
        ComboConfigDir.ItemIndex := 2;
        ComboSyncLoc.ItemIndex := 3;
    end;
end;



{ 0 - src    0 0 0
  1 - test   1 1 1
  2 - Test (remote shared drive) 1,1,2
  3 - Real   2,2,3
}

procedure TForm1.FormCreate(Sender: TObject);
var
    I : integer;
    St : string;
begin
    ComboNotesDir.clear;
    ComboNotesDir.Items.Add('notes');
    ComboNotesDir.Items.Add(GetEnvironmentVariable('HOME') + '/.local/share/TB_Test/');
    ComboNotesDir.Items.Add(GetEnvironmentVariable('HOME') + '/.local/share/tomboy-ng/');
    ComboNotesDir.ItemIndex := 2;

    ComboConfigDir.Clear;
    ComboConfigDir.Items.Add('config');
    ComboConfigDir.Items.Add(GetEnvironmentVariable('HOME') + '/.config/TB_Test/');
    ComboConfigDir.Items.Add(GetEnvironmentVariable('HOME') + '/.config/tomboy-ng/');
    ComboConfigDir.ItemIndex := 2;


    ComboTransport.Items.clear;
      for i := Ord(Low(TSyncTransport)) to Ord(High(TSyncTransport)) do
      begin
         ComboTransport.Items.add(GetEnumName(TypeInfo(TSyncTransport), Ord(i)));
      end;
      ComboTransport.ItemIndex := 2;

      RadioButtonReal.Checked := True;
end;



procedure TForm1.FormShow(Sender: TObject);
begin
    CheckBoxtestRun.Checked := True;
    CheckdevMode.Checked := False;
    DevMode(false);
end;


function TForm1.Proceed(const ClashRec : TClashRecord) : TSyncAction;
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

