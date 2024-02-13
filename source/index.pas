unit Index;
{   Copyright (C) 2017-2024 David Bannon

    License:
    This code is licensed under MIT License, see the file License.txt
    or https://spdx.org/licenses/MIT.html  SPDX short identifier: MIT

    ------------------

    This unit find and displays all the 'headings' in a note, returns with
    the block index of the one user clicked.
    A Heading is any complete line that is all either Large or Huge text.
    We indent Huge, Large Bold and just Large differently so easy to see
    relative heading importance.

HISTORY
    2019/05/14  Display strings all (?) moved to resourcestrings
    2020/06/14  Added lockout to deal with RH issue.
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ExtCtrls, KMemo;

type

    { TFormIndex }

    TFormIndex = class(TForm)
        Label1: TLabel;
        ListBox1: TListBox;
        Panel1: TPanel;
        procedure FormActivate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure ListBox1Click(Sender: TObject);
    private
        BlockClose : boolean;
        function IsHeading(const BlkNo: integer): integer;

    public
        TheKMemo : TKMemo;
        SelectedBlock : integer;
    end;

var
    FormIndex: TFormIndex;

implementation

{$R *.lfm}

{ TFormIndex }

uses Settings, lazlogger;

procedure TFormIndex.FormShow(Sender: TObject);
var
    Index : integer = 0;
begin
    BlockClose := True;
    ModalResult := mrNone;
    SelectedBlock := -1;
    ListBox1.Items.BeginUpdate;
    try
        while Index < TheKmemo.Blocks.Count do begin
            while TheKMemo.Blocks.Items[Index].ClassNameIs('TKMemoParagraph') do begin
                inc(Index);
                if Index >= TheKMemo.Blocks.Count then exit;
            end;
            Index := IsHeading(Index);
        end;
    finally
        ListBox1.Items.EndUpdate;
    end;
end;

procedure TFormIndex.FormActivate(Sender: TObject);
begin
    BlockClose := False;
end;

procedure TFormIndex.ListBox1Click(Sender: TObject);
begin
    if (Listbox1.ItemIndex = -1) or BlockClose then begin
        ListBox1.ItemIndex := -1;
        exit;
    end;
    SelectedBlock := PtrInt(Listbox1.Items.Objects[Listbox1.ItemIndex]);
    //ShowMessage('Attached value: ' + IntToStr(SelectedBlock));
    Modalresult := mrOK;
end;

function TFormIndex.IsHeading(const BlkNo : integer) : integer;
var
    Index : integer;
    St : string = '';
begin
    Index := BlkNo;
    Result := BlkNo;
    // write('[CALLED=' + inttostr(blkno) + '] ');
    while Result < TheKmemo.Blocks.Count do begin
        if TheKMemo.Blocks.Items[Result].ClassNameIs('TKMemoParagraph') then break;
        inc(result);
    end;
    // writeln('[PARA=' + inttostr(Result)+']');
    // Result is now pointing to first Para beyond BlkNo OR beyond kmemo content
    while Index < Result do begin
        // writeln('[' + inttostr(Index) + '] ' + TKmemoTextBlock(TheKMemo.Blocks.Items[Index]).Text);
        if (TheKMemo.Blocks.Items[Index].ClassNameIs('TKMemoTextBlock')
                    and (TKmemoTextBlock(TheKMemo.Blocks.Items[Index]).TextStyle.Font.Size
                        in [Sett.FontTitle, Sett.FontLarge, Sett.FontHuge]))
        then {begin writeln('================ Examined ' + TKmemoTextBlock(TheKMemo.Blocks.Items[Index]).Text);} inc(Index){; end }
        else                    // its not a heading
            exit(Result)        // Remember we may be beyond the content ....
    end;
    // OK, its a heading, all blocks are Large or Huge. Huge=NoSpaces; LargeBold=2 spaces; Large=4 spaces
    if TKmemoTextBlock(TheKMemo.Blocks.Items[BlkNo]).TextStyle.Font.Size = Sett.FontLarge then begin
        St := '.  ';
        if not (fsBold in TKmemoTextBlock(TheKMemo.Blocks.Items[BlkNo]).TextStyle.Font.style) then
            St := St + '   ';
    end;
    Index := BlkNo;
    while Index < Result do begin
        St := St + TKmemoTextBlock(TheKMemo.Blocks.Items[Index]).Text;
        inc(Index);
    end;
    ListBox1.AddItem(St, TObject(PtrInt(BlkNo)));
    inc(Result);
end;

end.

