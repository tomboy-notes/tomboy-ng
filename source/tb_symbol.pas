unit tb_symbol;
{$mode ObjFPC}{$H+}

{   Copyright (C) 2017-2024 David Bannon

    License:
    This code is licensed under MIT License, see the file License.txt
    or https://spdx.org/licenses/MIT.html  SPDX short identifier: MIT

    ------------------

    This is a unit that manages a list of extended characters, configurable by the user.

    a four byte char for testing $F09382BA

    History :
    2023-02-19 Initial release.
}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
    Buttons, LCLIntf, ResourceStr;

type Tutf8Char = string[4];

type TSymbolRec = record
    Sym : TUTF8Char;       // will hold a UTF8 1-4 byte string
    SymStr : string[10];   // will hold a string represation of the UTF8 char
end;

type
    { TFormSymbol }
    TFormSymbol = class(TForm)
        BitBtnCancel: TBitBtn;
        BitBtnOK: TBitBtn;
        BitBtnRevert: TBitBtn;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        StringGrid1: TStringGrid;
        procedure BitBtnOKClick(Sender: TObject);
        procedure BitBtnRevertClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure Label3Click(Sender: TObject);
        procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
            const Value: string);
    private
        IsChanged : boolean;
        procedure DefaultSymbolRecArray();
        function StrToUtf8(const St: string): TUTF8Char;
        procedure UpdateGrid;
        function ValidUTF8(UCh: TUTF8Char): boolean;
        procedure FChanged(NewState : boolean);
        property Changed : boolean read IsChanged write fChanged;
    public
        // Redraws the symbols in SymArray using SymArray's SymStr data.
        procedure UpdateSymbols();
    end;

var
    FormSymbol: TFormSymbol;
    SymArray : array[0..9] of TSymbolRec;

implementation

{$R *.lfm}
// Initial, default values for Symbol Array, user can edit.
procedure TFormSymbol.DefaultSymbolRecArray();
var
    i : integer;
begin
    SymArray[0].SymStr := 'CEA9';    // OMEGA
    SymArray[1].SymStr := 'CE94';    // Delta
    SymArray[2].SymStr := 'CF80';    // Pi
    SymArray[3].SymStr := 'CEBC';    // Micro
    SymArray[4].SymStr := 'C2B0';    // Degree
    SymArray[5].SymStr := 'C3A6';    // LATIN SMALL LETTER AE
    SymArray[6].SymStr := 'C3A4';    // LATIN SMALL LETTER A WITH DIAERESIS
    SymArray[7].SymStr := 'C3AB';    // LATIN SMALL LETTER E WITH DIAERESIS
    SymArray[8].SymStr := 'C3B6';    // LATIN SMALL LETTER O WITH DIAERESIS
    SymArray[9].SymStr := 'C3BC';    // LATIN SMALL LETTER U WITH DIAERESIS
    for i := 0 to high(SymArray) do begin
        SymArray[i].Sym := StrToUTF8(SymArray[i].SymStr);
    end;
end;

    // Converts a valid string with a UTF8 representation to a UTF8 string, returns
    // empty str if passed string is cannot be so converted. Errror tolerant.
function TFormSymbol.StrToUtf8(const St : string) : TUTF8Char;
// St should hold hex characters only, an even number, 2..8
    function CharOK() : boolean;
    var
        i : integer = 1;
    begin
        while i <= length(St) do begin
            if St[i] in ['0'..'9', 'A'..'F', 'a'..'f']  then
                inc(i)
            else exit(false);
        end;
        result := true;
    end;
var
    Buff : integer = 0;
    J : integer = 1;
begin
   result := '';
    if (byte(length(St)) in [2,4,6,8])
            and CharOK() then begin        // C3BC
        while J < length(St) do begin
            Buff := strtointdef('$' + St[j] + St[j+1], -1);
            Result := Result + char(Buff);
            inc(J, 2);
        end;
    end;
    if not ValidUTF8(Result) then
        Result := '';
end;

(* function TFormSymbol.LongToUtf8(const ANumb : longint) : TUTF8Char;   // Can handle ONLY 1..3 byte UTF8 char
var
    Numb : longint;
    AByte : byte = 0;
begin
    Result := '';
    Numb := ANumb;
    while true do begin
        AByte := Numb mod 256;
        Numb := Numb div 256;             // shr faster ...
        Result := char(AByte) + Result;
        if Numb < 256 then begin
            Result := char(Numb) + Result;
            break;
        end;
    end;
end;     *)

function TFormSymbol.ValidUTF8(UCh : TUTF8Char) : boolean;
    // returns true if the TheBit'th from right is set.    110xxxxx
    function BitSet(Value : char; TheBit : integer) : boolean;      // theBit 0-7
        begin
            Result := ((byte(Value) shr TheBit) and 1) = 1;
        end;
begin
    result := false;
    case length(Uch) of
        0 : exit(false);
        1 : exit(not BitSet(Uch[1], 7));                            //  0xxxxxxx
        2 : exit(BitSet(Uch[1], 7) and BitSet(Uch[1], 6)            //  110xxxxx 10xxxxxx
                and (not BitSet(Uch[1], 5)) and BitSet(Uch[2], 7)
                and (not BitSet(Uch[2], 6)));
        3 : exit(BitSet(Uch[1], 7) and BitSet(Uch[1], 6)            // 1110xxxx 10xxxxxx 10xxxxxx
                and BitSet(Uch[1], 5) and (not BitSet(Uch[1], 4))
                and BitSet(Uch[2], 7) and (not BitSet(Uch[2], 6))
                and BitSet(Uch[3], 7) and (not BitSet(Uch[3], 6)));
        4 : exit(BitSet(Uch[1], 7) and BitSet(Uch[1], 6)            // 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
                and BitSet(Uch[1], 5) and BitSet(Uch[1], 4)
                and (not BitSet(Uch[1], 3)) and BitSet(Uch[2], 7)
                and (not BitSet(Uch[2], 6)) and BitSet(Uch[3], 7)
                and (not BitSet(Uch[3], 6)) and BitSet(Uch[3], 7)
                and (not BitSet(Uch[3], 6)));
    end;
end;


procedure TFormSymbol.FChanged(NewState : boolean);
begin
    BitBtnOK.Enabled := NewState;
    BitBtnRevert.Enabled := NewState;
    IsChanged := NewState;
end;

procedure TFormSymbol.UpdateSymbols();
var
    I : integer;
begin
    for i := 0 to high(SymArray) do begin
        SymArray[i].Sym := StrToUtf8(SymArray[i].SymStr);
        //writeln('TFormSymbol.UpdateSymbols ' + SymArray[i].Sym);
    end;
    UpdateGrid();
end;

procedure TFormSymbol.UpdateGrid();
var
    i : integer;
begin
    for i := 0 to high(SymArray) do begin
        StringGrid1.Cells[0, i] := SymArray[i].Sym;
        StringGrid1.Cells[1, i] := SymArray[i].SymStr;
    end;
end;

{ TFormSymbol }

procedure TFormSymbol.FormCreate(Sender: TObject);

begin
    // in StringGrid, tick goAlwaysShowEditor, goEditing
    Label1.Caption := rsEnterHexValue;
    Label2.caption := rsHexCharRequired;
    Label3.Caption := 'Click here to browse to full list';
    DefaultSymbolRecArray();
    UpdateGrid();
    BitBtnOk.Enabled := False;
    BitBtnRevert.Enabled := False;
end;

procedure TFormSymbol.Label3Click(Sender: TObject);
begin
    // open the users browser at https://www.utf8-chartable.de/unicode-utf8-table.pl
    OpenURL('https://www.utf8-chartable.de/unicode-utf8-table.pl');
end;

procedure TFormSymbol.BitBtnRevertClick(Sender: TObject);
begin
    UpdateGrid();
    Changed := False;
end;

procedure TFormSymbol.BitBtnOKClick(Sender: TObject);
var
    i : integer;
    St : string;
begin
    if Changed then begin
        for i := 0 to high(SymArray) do begin
            St := StringGrid1.Cells[1, i];
            if (length(St) > 0) and (St[1] in ['x', 'X', '$']) then
                delete(St, 1, 1);               // silently forgive inclusion of a hex symbol
            SymArray[i].SymStr := St;
            SymArray[i].Sym := StrToUTF8(St);
        end;
    end;
end;

{ we expect 0..8 hex digits, will remove a leading $ or x. Must work as string,
not numerically because for hex bytes might be bigger than longint. }

procedure TFormSymbol.StringGrid1SetEditText(Sender: TObject; ACol,
    ARow: Integer; const Value: string);
var
    St : string;
begin
    St := StringGrid1.Cells[ACol, ARow];
    if (length(St) > 0) and (St[1] in ['x', 'X', '$']) then
        delete(St, 1, 1);                               // silently forgive inclusion of a hex symbol
    StringGrid1.Cells[0, ARow] := StrToUTF8(St);        // try $F09382BA
    if St <> SymArray[ARow].SymStr then                 // A change made
        Changed := True;
end;


end.

