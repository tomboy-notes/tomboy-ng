unit colours;

{$mode objfpc}{$H+}

interface

uses
        Classes, SysUtils, kmemo, Forms, Controls, Graphics, Dialogs, StdCtrls,
		Buttons;

type    { TFormColours }
    TFormColours = class(TForm)
		ColorDialog1: TColorDialog;
		KMemo1: TKMemo;
		Label1: TLabel;
		Label2: TLabel;
		SpeedTitle: TSpeedButton;
		SpeedText: TSpeedButton;
		SpeedBackground: TSpeedButton;
		SpeedHighlight: TSpeedButton;
		SpeedDefault: TSpeedButton;
		SpeedCancel: TSpeedButton;
		SpeedOK: TSpeedButton;
		procedure FormCreate(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure SpeedBackgroundClick(Sender: TObject);
		procedure SpeedCancelClick(Sender: TObject);
		procedure SpeedDefaultClick(Sender: TObject);
		procedure SpeedHighlightClick(Sender: TObject);
		procedure SpeedOKClick(Sender: TObject);
		procedure SpeedTextClick(Sender: TObject);
		procedure SpeedTitleClick(Sender: TObject);
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

procedure TFormColours.PopulateMemo;
var
    TB : TKMemoTextBlock;
begin
    KMemo1.Clear(False);
    KMemo1.Colors.BkGnd:= CBack;
    TB := KMemo1.Blocks.AddTextBlock('The Title');
    TB.TextStyle.Font.Size:= 16;
    TB.TextStyle.Font.Color:= CTitle;
    //TB.TextStyle.Brush.Color:= CBack;
    TB.TextStyle.Font.Underline := true;
    KMemo1.blocks.AddParagraph();
    TB := KMemo1.Blocks.AddTextBlock('Normal Text');
    TB.TextStyle.Font.Size:= 11;
    TB.TextStyle.Font.Color:=CText;
    //TB.TextStyle.Brush.Color:= CBack;
    KMemo1.blocks.AddParagraph();
    TB := KMemo1.Blocks.AddTextBlock('Some Highlight');
    TB.TextStyle.Font.Size:= 11;
    TB.TextStyle.Font.Color:= CText;
    TB.TextStyle.Brush.Color:= CHiBack;
    KMemo1.blocks.AddParagraph();
    TB := KMemo1.Blocks.AddTextBlock('More normal Text');
    TB.TextStyle.Font.Size:=11;
    TB.TextStyle.Font.Color:= CText;
    //TB.TextStyle.Brush.Color:= CBack;
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
    left := (screen.Width div 2) - (width div 2);
    top := (screen.Height div 2) - (width div 2);
end;

procedure TFormColours.SpeedBackgroundClick(Sender: TObject);
begin
    ColorDialog1.Color := CBack;
    if ColorDialog1.Execute then begin
        CBack := ColorDialog1.Color;
        PopulateMemo;
	end;
end;

procedure TFormColours.SpeedCancelClick(Sender: TObject);
begin
        ModalResult := mrCancel;
end;

procedure TFormColours.SpeedDefaultClick(Sender: TObject);
begin
        ModalResult := mrRetry;
end;

procedure TFormColours.SpeedHighlightClick(Sender: TObject);
begin
    ColorDialog1.Color := CHiBack;
    if ColorDialog1.Execute then begin
        CHiBack := ColorDialog1.Color;
        PopulateMemo;
	end;
end;

procedure TFormColours.SpeedOKClick(Sender: TObject);
begin
        ModalResult:=mrOK;
end;

procedure TFormColours.SpeedTextClick(Sender: TObject);
begin
    ColorDialog1.Color := CText;
    if ColorDialog1.Execute then begin
        CText := ColorDialog1.Color;
        PopulateMemo;
	end;
end;

procedure TFormColours.SpeedTitleClick(Sender: TObject);
begin
    ColorDialog1.Color := CTitle;
    if ColorDialog1.Execute then begin
        CTitle := ColorDialog1.Color;
        PopulateMemo;
	end;
end;

end.

