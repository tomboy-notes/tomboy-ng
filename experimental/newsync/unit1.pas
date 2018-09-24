unit Unit1;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls, Graphics,
    Dialogs, StdCtrls, Spin, Menus, SyncUtils;

type

    { TForm1 }

    TForm1 = class(TForm)
        ButJoinExist: TButton;
		ButJoinNew: TButton;
		LabelConfig: TLabel;
        LabelSync: TLabel;
        Memo1: TMemo;
        SelectDirectoryDialog1: TSelectDirectoryDialog;
        SpinEdit1: TSpinEdit;
        procedure ButJoinExistClick(Sender: TObject);
		procedure ButJoinNewClick(Sender: TObject);
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
uses Sync, TB_SDiff;

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

procedure TForm1.ButJoinExistClick(Sender: TObject);
var
    SyncState : TSyncAvailable;
begin
  {if SelectDirectoryDialog1.Execute then begin
       LabelSync.Caption := TrimFilename(SelectDirectoryDialog1.FileName + PathDelim);    }
       try
           //ASync := TSync.Create('/home/dbannon/.local/share/tomboy-ng/', '/home/dbannon/.config/', SpinEdit1.Value);
           ASync := TSync.Create();
           ASync.DebugMode:=True;
           ASync.TestRun := True;
           ASync.ProceedFunction:=@Proceed;
           ASync.NotesDir:='/home/dbannon/.local/share/tomboy-ng/';
           ASync.ConfigDir := LabelConfig.Caption;
           ASync.SyncAddress := LabelSync.Caption;
           ASync.CurrRev:=SpinEdit1.Value;
           Async.SetMode(True);
           SyncState := ASync.testConnection;
           {case SyncState of
                   Syncready : Async.StartSync();
                   SyncNoLocal : begin
                                    if ASync.JoinSync() then begin
                                        DisplaySync();
                                        ASync.StartSync()
									end
									else showmessage('Failed to join sync repo')
				                end;
		   end;  }
		   if SyncReady <> ASync.TestConnection() then begin
                Memo1.Append('Failed to find an existing connection. ' + ASync.ErrorString);
           end else begin
                // If to here, we sync should be enabled and know about remote files it might need.
                ASync.StartSync();
                DisplaySync();
           end;
       finally
            ASync.Free;
       end;

  // end;
end;

procedure TForm1.ButJoinNewClick(Sender: TObject);
var
    SyncState : TSyncAvailable;
begin
  {if SelectDirectoryDialog1.Execute then begin
       LabelSync.Caption := TrimFilename(SelectDirectoryDialog1.FileName + PathDelim);    }
       try
           //ASync := TSync.Create('/home/dbannon/.local/share/tomboy-ng/', '/home/dbannon/.config/', SpinEdit1.Value);
           ASync := TSync.Create();
           ASync.DebugMode:=True;
           ASync.TestRun := True;
           ASync.ProceedFunction:=@Proceed;
           ASync.NotesDir:='/home/dbannon/.local/share/tomboy-ng/';
           ASync.ConfigDir := LabelConfig.Caption;
           ASync.SyncAddress := LabelSync.Caption;
           ASync.CurrRev:=SpinEdit1.Value;
           Async.SetMode(True);
           if ASync.JoinSync() then begin
                DisplaySync();
		   end;
       finally
            ASync.Free;
       end;

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
            mrYes : Result := SyDownLoad;
            mrNo  : Result := SyUpLoadEdit;
    end;
    SDiff.Free;
end;

end.

