unit Backlinks;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;


type PString = ^String;

type

    { TFormBackLinks }

    TFormBackLinks = class(TForm)
        BitBtn1: TBitBtn;
        Label1: TLabel;
        ListBox1: TListBox;
        procedure FormShow(Sender: TObject);
        procedure ListBox1Click(Sender: TObject);
    private

    public
        BackList : TStringList; // dont free, belongs to EditBox
        BackTitle : PString;
    end;

var
    FormBackLinks: TFormBackLinks;


implementation

uses {SearchUnit,} LCLProc;

{$R *.lfm}

{ TFormBackLinks }

procedure TFormBackLinks.FormShow(Sender: TObject);
var
    i : integer;
begin
    //DebugLn('TFormBackLinks.FormShow 1 ItemIndex=' + inttostr(ListBox1.ItemIndex));
    ListBox1.ItemIndex := -1;
    Label1.Visible := BackList.Count = 0;
    ListBox1.ClickOnSelChange := False;
    // Above line to ensure we don't get unwanted click events on some linux systems.
    ListBox1.Clear;
    ListBox1.Items := BackList;
    ListBox1.ItemIndex := -1;
    //DebugLn('TFormBackLinks.FormShow 2 ItemIndex=' + inttostr(ListBox1.ItemIndex));
end;

procedure TFormBackLinks.ListBox1Click(Sender: TObject);
begin
    //DebugLn('TFormBackLinks.ListBox1Click 1 ItemIndex=' + inttostr(ListBox1.ItemIndex));
    if (ListBox1.ItemIndex >= 0) and (ListBox1.ItemIndex < ListBox1.Count) then begin
        BackTitle^ := ListBox1.Items[ListBox1.ItemIndex];
        close;
    end;
end;

end.

