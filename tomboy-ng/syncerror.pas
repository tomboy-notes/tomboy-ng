unit SyncError;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

    { TFormSyncError }

    TFormSyncError = class(TForm)
        BitBtn1: TBitBtn;
        BitBtn2: TBitBtn;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
    private

    public

    end;

var
    FormSyncError: TFormSyncError;

implementation

{$R *.lfm}

end.

