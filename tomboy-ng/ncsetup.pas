unit ncsetup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLIntf, StdCtrls;

type    { TFormNCSetup }
  TFormNCSetup = class(TForm)
    NCAuth: TButton;
    URL: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Reset: TToggleBox;
    NCDone: TToggleBox;
    procedure Label1Click(Sender: TObject);
    procedure NCDoneChange(Sender: TObject);
  private

  public

  end;

var
  FormNCSetup: TFormNCSetup;

implementation

{$R *.lfm}

{ TFormNCSetup }

procedure TFormNCSetup.Label1Click(Sender: TObject);
begin

end;

procedure TFormNCSetup.NCDoneChange(Sender: TObject);
begin

end;

end.

