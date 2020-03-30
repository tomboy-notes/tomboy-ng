unit colours;

{$mode objfpc}{$H+}

interface

uses
        Classes, SysUtils, kmemo, Forms, Controls, Graphics, Dialogs, StdCtrls,
		Buttons;

type    { TFormColours }
    TFormColours = class(TForm)
		BitBtn1: TBitBtn;
		BitBtn2: TBitBtn;
		BitBtnDefault: TBitBtn;
		ButtonHiBack: TButton;
		ButtonTitle: TButton;
		ButtonText: TButton;
		ButtonBack: TButton;
		ColorDialog1: TColorDialog;
		KMemo1: TKMemo;
		Label1: TLabel;
		Label2: TLabel;
        procedure ButtonBackClick(Sender: TObject);
        procedure ButtonHiBackClick(Sender: TObject);
		procedure ButtonTextClick(Sender: TObject);
		procedure ButtonTitleClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormShow(Sender: TObject);
    private
		procedure PopulateMemo;

    public
        CTitle, CBack, CText, CHiBack : TColor;
end;

var
        FormColours: TFormColours;

implementation

{$R *.lfm}

{ TFormColours }

procedure TFormColours.ButtonHiBackClick(Sender: TObject);
begin
    ColorDialog1.Color := CHiBack;
    if ColorDialog1.Execute then begin
        CHiBack := ColorDialog1.Color;
        PopulateMemo;
	end;
end;

procedure TFormColours.ButtonBackClick(Sender: TObject);
begin
    ColorDialog1.Color := CBack;
    if ColorDialog1.Execute then begin
        CBack := ColorDialog1.Color;
        PopulateMemo;
	end;
end;


procedure TFormColours.ButtonTextClick(Sender: TObject);
begin
    ColorDialog1.Color := CText;
    if ColorDialog1.Execute then begin
        CText := ColorDialog1.Color;
        PopulateMemo;
	end;
end;

procedure TFormColours.ButtonTitleClick(Sender: TObject);
begin
    ColorDialog1.Color := CTitle;
    if ColorDialog1.Execute then begin
        CTitle := ColorDialog1.Color;
        PopulateMemo;
	end;
end;

procedure TFormColours.PopulateMemo;
var
    TB : TKMemoTextBlock;
begin
    KMemo1.Clear(False);
    TB := KMemo1.Blocks.AddTextBlock('The Title');
    TB.TextStyle.Font.Size:= 16;
    TB.TextStyle.Font.Color:= CTitle;
    TB.TextStyle.Brush.Color:= CBack;
    TB.TextStyle.Font.Underline := true;
    KMemo1.blocks.AddParagraph();
    TB := KMemo1.Blocks.AddTextBlock('Normal Text');
    TB.TextStyle.Font.Size:= 11;
    TB.TextStyle.Font.Color:=CText;
    TB.TextStyle.Brush.Color:= CBack;
    KMemo1.blocks.AddParagraph();
    TB := KMemo1.Blocks.AddTextBlock('Some Highlight');
    TB.TextStyle.Font.Size:= 11;
    TB.TextStyle.Font.Color:= CText;
    TB.TextStyle.Brush.Color:= CHiBack;
    KMemo1.blocks.AddParagraph();
    TB := KMemo1.Blocks.AddTextBlock('More normal Text');
    TB.TextStyle.Font.Size:=11;
    TB.TextStyle.Font.Color:= CText;
    TB.TextStyle.Brush.Color:= CBack;
    KMemo1.blocks.AddParagraph();
end;

procedure TFormColours.FormCreate(Sender: TObject);
begin
{
    CBack := clCream;
    CHiBack := clYellow;
    CText := clBlack;
    CTitle := clBlue;          }

end;

procedure TFormColours.FormShow(Sender: TObject);
begin
    PopulateMemo;
end;

end.

