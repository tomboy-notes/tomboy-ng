unit Unit1;

{
    A basic GUI to facilitate testing of the sync model.
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls, Graphics,
    Dialogs, StdCtrls, Menus, SyncUtils;

type

    { TForm1 }

    TForm1 = class(TForm)
			Button1: TButton;
            ButtonAltNotes: TButton;
            ButtonSetIP: TButton;
            ButtonSyncDir: TButton;
            ButtonNotesDir: TButton;
            ButtonConfigDir: TButton;
            ButtonAltSync: TButton;
        ButtSyncExisting: TButton;
		ButJoinNew: TButton;
		CheckBoxTestRun: TCheckBox;
        ComboBox1: TComboBox;
		EditConfig: TEdit;
		EditNotes: TEdit;
		EditSync: TEdit;
		Label1: TLabel;
		LabelNotes: TLabel;
		LabelConfig: TLabel;
        LabelSync: TLabel;
        Memo1: TMemo;
        SelectDirectoryDialog1: TSelectDirectoryDialog;
		procedure Button1Click(Sender: TObject);
        procedure ButtonAltNotesClick(Sender: TObject);
        procedure ButtonAltSyncClick(Sender: TObject);
        procedure ButtonConfigDirClick(Sender: TObject);
        procedure ButtonNotesDirClick(Sender: TObject);
        procedure ButtonSetIPClick(Sender: TObject);
        procedure ButtonSyncDirClick(Sender: TObject);
        procedure ButtSyncExistingClick(Sender: TObject);
		procedure ButJoinNewClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormShow(Sender: TObject);
    private
	    procedure DisplaySync();
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
	    ASync.NotesDir:= EditNotes.Text;
	    ASync.ConfigDir := EditConfig.Text;
	    ASync.SyncAddress := EditSync.Text;
        ASync.RepoAction:= RepoUse;
	    //ASync.CurrRev:=SpinEdit1.Value;
        Tick1 := GetTickCount64();
	    if SyncNetworkError = Async.SetTransport(TSyncTransport(ComboBox1.ItemIndex)) then begin
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
	    end;
    finally
	    ASync.Free;
    end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
    MR : integer;
begin
    if selectdirectorydialog1.Execute then editSync.Text := selectdirectorydialog1.FileName;
    {
    MR := QuestionDlg('Window Title', 'My question', mtConfirmation, [mrYes, mrNo], 'Blar');
    if Mr = mrYes  then showmessage('YES')
    else if MR = mrNo then showmessage('no');}
end;

procedure TForm1.ButtonAltNotesClick(Sender: TObject);
begin
    EditNotes.Text := '/home/dbannon/.local/share/tomboy-ng/';
end;

procedure TForm1.ButtonAltSyncClick(Sender: TObject);
begin
    EditSync.text := '/run/user/1000/gvfs/smb-share:server=192.168.1.1,share=usbdisk/SanDisk_FirebirdUSBFlashDrive_1_ca4c/PascalTrans/Sync_TestRig/';
end;

procedure TForm1.ButtonConfigDirClick(Sender: TObject);
begin
    if selectdirectorydialog1.Execute then
        editConfig.Text := trim(appendpathdelim(selectdirectorydialog1.FileName));
end;

procedure TForm1.ButtonNotesDirClick(Sender: TObject);
begin
    if selectdirectorydialog1.Execute then
        editNotes.Text := trim(appendpathdelim(selectdirectorydialog1.FileName));
end;

procedure TForm1.ButtonSetIPClick(Sender: TObject);
begin
    EditSync.Text := '192.168.1.174';
end;

procedure TForm1.ButtonSyncDirClick(Sender: TObject);
begin
    if selectdirectorydialog1.Execute then
        editSync.Text := trim(appendpathdelim(selectdirectorydialog1.FileName));
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
	    ASync.NotesDir:= EditNotes.Text;
	    ASync.ConfigDir := EditConfig.Text;
	    ASync.SyncAddress := EditSync.Text;
        ASync.RepoAction := RepoJoin;
        Tick1 := GetTickCount64();
        if SyncNetworkError = Async.SetTransport(TSyncTransport(ComboBox1.Itemindex))  then begin
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
		end else showmessage('Cancelling operation');
    finally
        ASync.Free;
    end;
    memo1.append('Test = ' + inttostr(Tick2 - Tick1) + 'mS and StartSync = ' + inttostr(Tick3 - Tick2) + 'mS');
end;

procedure TForm1.FormCreate(Sender: TObject);
var
    I : integer;
    St : string;
begin
      ComboBox1.Items.clear;
      for i := Ord(Low(TSyncTransport)) to Ord(High(TSyncTransport)) do
      begin
         ComboBox1.Items.add(GetEnumName(TypeInfo(TSyncTransport), Ord(i)));
      end;
      ComboBox1.ItemIndex := 2;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
	    EditConfig.Text := 'config';
	    EditSync.Text := 'sync';
	    EditNotes.Text := 'notes';
	    CheckBoxtestRun.Checked := True;
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

