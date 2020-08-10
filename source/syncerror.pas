unit SyncError;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

    { TFormSyncError }

    TFormSyncError = class(TForm)
        ButtCancel: TBitBtn;
        ButtRetry: TBitBtn;
        Label1: TLabel;
        Label3: TLabel;
        procedure FormShow(Sender: TObject);
    private

    public

    end;

var
    FormSyncError: TFormSyncError;

implementation

{$R *.lfm}

{ TFormSyncError }

procedure TFormSyncError.FormShow(Sender: TObject);
begin
    left := (screen.Width div 2) - (width div 2);
    top := (screen.Height div 2) - (height div 2);
end;

end.

