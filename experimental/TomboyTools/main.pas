unit main;

{$mode objfpc}{$H+} {$assertions on}

interface

uses
        Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type

		{ TFormMain }

        TFormMain = class(TForm)
				procedure FormShow(Sender: TObject);
        private

        public

        end;

var
        FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormShow(Sender: TObject);
begin

end;

end.

