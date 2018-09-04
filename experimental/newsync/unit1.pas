unit Unit1;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls, Graphics,
    Dialogs, StdCtrls, Spin;

type

    { TForm1 }

    TForm1 = class(TForm)
        Button1: TButton;
        LabelDir: TLabel;
        Memo1: TMemo;
        SelectDirectoryDialog1: TSelectDirectoryDialog;
        SpinEdit1: TSpinEdit;
        procedure Button1Click(Sender: TObject);
    private

    public

    end;

var
    Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }
uses Sync;

procedure TForm1.Button1Click(Sender: TObject);
var
    ASync : TSync;
begin
  if SelectDirectoryDialog1.Execute then begin
       LabelDir.Caption := TrimFilename(SelectDirectoryDialog1.FileName + PathDelim);
       try
           //ASync := TSync.Create('/home/dbannon/.local/share/tomboy-ng/', '/home/dbannon/.config/', SpinEdit1.Value);
           ASync := TSync.Create();
           ASync.DebugMode:=True;
           ASync.NotesDir:='/home/dbannon/.local/share/tomboy-ng/';
           ASync.ConfigDir:='/home/dbannon/.config/';
           ASync.SyncAddress:= LabelDir.Caption;
           ASync.CurrRev:=SpinEdit1.Value;
           Async.SetMode(True);
           if not ASync.TestConnection() then begin
                Memo1.Append('Failed to find an existing connection. ' + ASync.ErrorString);
           end else begin
                // If to here, we sync should be enabled and know about remote files it might need.
                ASync.StartSync();
           end;
       finally
            ASync.Free;
       end;

  end;
end;

end.

