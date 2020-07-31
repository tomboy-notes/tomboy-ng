unit Spelling;

{ A GUI unit that uses hunspell.pas to check spelling of the passed KMemo
  Note we start at the end of doc and scan back to beginning

 * Copyright (C) 2018 David Bannon
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 }

{  HISTORY -
    2018/03/03 Initial Commit
	2018/03/24	Win only bug, missed counting a #13 when replacing first word on line.
				Win only - allow for #13 in a selected block
	2018/04/07	Changes in ReplaceWord to really get it right for Windows UTF8 but
				some changes outside ifdef so must check on Linux too.
    2018/06/21  Hide an unnecessary debug line
    2018/11/29  Fixed bug when spell checking a selection of text
    2019/05/19  Display strings all (?) moved to resourcestrings
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    Buttons, kmemo;

type

    { TFormSpell }

    TFormSpell = class(TForm)
        BitBtn1: TBitBtn;
        ButtonSkip: TButton;
        ButtonIgnore: TButton;
        ButtonUseAndNextWord: TButton;
        //Label1: TLabel;
        LabelContext: TLabel;
        LabelPrompt: TLabel;
        Label4: TLabel;
        LabelStatus: TLabel;
        LabelSuspect: TLabel;
        ListBox1: TListBox;
        procedure ButtonIgnoreClick(Sender: TObject);
        procedure ButtonUseAndNextWordClick(Sender: TObject);
        procedure ButtonSkipClick(Sender: TObject);
        procedure FormHide(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure ListBox1Click(Sender: TObject);
    private
        function CleanContext(): AnsiString;
        	{ Returns the number of #13 before the indicated location - bloody windows ! }
		function NewLinesBefore(const Idx: integer): integer;
            { Returns True if it found another mis spelt word }
        procedure PreviousWord(var TheIndex: longint);
        procedure ReplaceWord(const NewWord: AnsiString);
        procedure ShowContents;
        procedure ShowSuggestions();
        procedure WeAreDone();
        function WordToCheck(): boolean;

    public
        TextToCheck : AnsiString;
        TheKMemo : TKmemo;

    end;

var
    FormSpell: TFormSpell;

implementation

uses hunspell, settings, LazUTF8, LazLogger;

const
  SetofDelims = [#10, #13, ' '..'@', '['..'`', '{'..'~'];   // all askii visible char ??
  // SetofDelims = [' ',#10,#13,'(', ')', ',', '[', ']', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '.', ';', '/', '-'];
  ContextSize = 20; // Thats plus and minus

var
    Spell : THunspell;
        { Index points to character before word we are checking, a deliminator or its 0
          its a count of UTF8 characters, not bytes }
    Index  : integer;
    FinishIndex : integer;
    TheWord : ANSIString;       // The word we think might be mis-spelt
    LeadContext, TrailContext : AnsiString;
    SaveSelStart, SaveSelEnd : integer;
    Str_Text : ANSIString;      // Will have a copy of KMemo's Text property, faster
{$R *.lfm}

{ TFormSpell }

function TFormSpell.NewLinesBefore(const Idx : integer) : integer;
var
    BlockNo, LocalOffset : integer;
begin
	Result := 0;
   BlockNo := TheKmemo.Blocks.IndexToBlockIndex(Idx, LocalOffset);
   while BlockNo > 0 do begin
       if TheKmemo.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then
           inc(Result);
       dec(BlockNo);
   end;
end;

RESOURCESTRING
  rsCheckingFull = 'Checking full document';
  rsCheckingSelection = 'Checking selection';
  rsSpellNotConfig = 'Spelling not configured';

procedure TFormSpell.FormShow(Sender: TObject);
begin
    // ShowContents();
    SaveSelStart := TheKmemo.RealSelStart;
    SaveSelEnd := TheKMemo.RealSelEnd;          // SelEnd points to first non-selected char
    if SaveSelEnd > (SaveSelStart+1) then begin     // Something was selected ...
        Index := SaveSelEnd;
        FinishIndex := SaveSelStart;
        {$ifdef WINDOWS}        // yet again, Windows silly line endings in .Text property
        Index := Index + NewLinesBefore(Index) + 1;
        FinishIndex := FinishIndex + NewLinesBefore(FinishIndex);
        {$endif}
        LabelStatus.Caption := rsCheckingSelection;
    end else begin								// note only one char selected, we treat as check whole doc
        FinishIndex := 0;
        Index := UTF8Length(TheKMemo.Blocks.Text);
        LabelStatus.Caption := rsCheckingFull;
    end;
    TheKMemo.SelEnd := TheKMemo.SelStart;   	// Now, nothing selected.
    LabelStatus.Caption := '';
    TheKMemo.Blocks.LockUpdate;
    Str_Text := TheKMemo.Blocks.Text;       	// Make a copy to work with, faster
    if Sett.SpellConfig then begin
        Spell :=  THunspell.Create(Application.HasOption('debug-spell'), Sett.LabelLibrary.Caption);
        if Spell.ErrorMessage = '' then begin
            if Spell.SetDictionary(Sett.LabelDic.Caption) then
                PreviousWord(Index);
        end;
     end else
        LabelStatus.Caption := rsSpellNotConfig;
end;

RESOURCESTRING
  rsREPLACE_with_1 = 'replace';
  rsReplace_WITH_2 = 'with';

procedure TFormSpell.ListBox1Click(Sender: TObject);
begin
    LabelStatus.Caption := rsREPLACE_with_1 + ' ' + TheWord + ' ' + rsReplace_WITH_2 + ' ' + ListBox1.Items[ListBox1.ItemIndex];
    ButtonUseAndNextWord.Enabled := True;
end;

procedure TFormSpell.ReplaceWord(const NewWord : AnsiString);
var
    BlockNo, TempIndex : integer;
    LocalIndex  {$ifdef WINDOWS}, I {$endif} : integer;
    TB: TKMemoTextBlock;
    TextSize : integer;
begin
    TempIndex := Index;
    {$ifdef WINDOWS}
	// Must allow for Windows extra CR in newline
    // need to know how many CRs between Index and 0
    I := length(UTF8copy(Str_Text, 1, Index+1));     // One based St, we need a byte count, not char
    while I > 0 do begin
        if Str_Text[I] = #13 then dec(TempIndex);
        dec(I);
	end;
    {$endif}
    //BlockNo := TheKmemo.Blocks.IndexToBlockIndex(TempIndex+1, LocalIndex);
    BlockNo := TheKmemo.Blocks.IndexToBlockIndex(TempIndex, LocalIndex);
	//debugln('Operating on BK=' + inttostr(BlockNo) + ' Loc=' + inttostr(LocalIndex) + ' TempIndex=' + inttostr(TempIndex) + ' Index=' + inttostr(Index));


    TextSize := TKMemoTextBlock(TheKmemo.Blocks.Items[BlockNo]).TextStyle.Font.Size;         // and Style ????
    TheKMemo.SelStart := TempIndex;					// zero based
    TheKMemo.SelEnd := TempIndex + UTF8Length(TheWord);
    //TheKMemo.Blocks.DeleteChar(0);                 // will delete selected text, maybe use ClearSelection() ??
    TheKMemo.Blocks.ClearSelection();
    if TheKmemo.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin
        TB := TheKMemo.Blocks.AddTextBlock(NewWord, BlockNo);
        TB.TextStyle.Font.Size := TextSize;
    end else
        //TheKmemo.Blocks.Items[BlockNo].InsertString(NewWord, LocalIndex-1);     // TKSelectionIndex is 0 based
        TheKmemo.Blocks.Items[BlockNo].InsertString(NewWord, LocalIndex);
    LabelStatus.Caption := '';
end;

procedure TFormSpell.ShowSuggestions();
begin
    Spell.Suggest(TheWord, ListBox1.Items);
    LabelPrompt.Visible:= True;
    ListBox1.Items.Add(TheWord);
end;

function TFormSpell.CleanContext() : AnsiString;   // Ah, what about bloody windows ?
var
    LineEndPos : integer;
begin
    // DebugLn('[[' + LeadContext + ' & ' + TrailContext + ']]');
    LineEndPos := UTF8Pos(LineEnding, LeadContext, 1);
    While LineEndPos > 0 do begin
        UTF8Delete(LeadContext, 1, LineEndPos);
        LineEndPos := UTF8Pos(LineEnding, LeadContext, 1);
    end;
    // DebugLn('[' + LeadContext + ']');
    LineEndPos := UTF8Pos(LineEnding, TrailContext);
    if LineEndPos > 0 then
        UTF8Delete(TrailContext, LineEndPos, ContextSize);
    // DebugLn('[' + TrailContext + ']');
    Result := LeadContext + ' ' + TrailContext;
end;

function TFormSpell.WordToCheck() : boolean;
begin
    Result := False;
    if UTF8Length(TheWord) > 1 then
        if not Spell.Spell(TheWord) then
            Result := True;
    if Result then begin
        ShowSuggestions();
        LabelSuspect.Caption := TheWord;
        LabelContext.Caption := CleanContext();
        // ShowContents();
    end else
        TheWord := ''
end;

RESOURCESTRING
    rsSpellComplete = 'Spell check complete';

procedure TFormSpell.WeAreDone();
begin
    LabelStatus.Caption := rsSpellComplete;
    LabelContext.Caption := '';
    ListBox1.Clear;
    LabelSuspect.Caption:='';
    ButtonSkip.Enabled := False;
    ButtonIgnore.Enabled := False;
end;


procedure TFormSpell.PreviousWord(var TheIndex : longint);
var
    //TS1, TS2, TS3, TS4 : TTimeStamp;           // Temp time stamping to test speed
    UTFCode : AnsiString;
begin
    LabelPrompt.Visible:= False;
    Dec(TheIndex);
    TheWord := '';
    ButtonUseAndNextWord.Enabled := False;
    //Str_Text := TheKMemo.Blocks.Text;     // much faster ! but tough on memory ?
    while TheIndex > FinishIndex do begin     // remember, first char is #1
        UTFCode := UTF8Copy(Str_Text, TheIndex, 1);
        if UTFCode[1] in SetOfDelims then begin
            LeadContext := UTF8Copy(Str_Text, TheIndex - ContextSize, ContextSize);
            TrailContext := UTF8Copy(Str_Text, TheIndex+1, ContextSize);
            if WordToCheck() then exit();
        end else
            TheWord := UTFCode + TheWord;
        dec(TheIndex);
    end;
    // if to here, TheIndex is 0
    if UTF8Length(TheWord) > 1 then
        if not WordToCheck() then
            WeAreDone();
    if TheIndex < FinishIndex then
        WeAreDone();
end;

procedure TFormSpell.ButtonUseAndNextWordClick(Sender: TObject);
begin
    ReplaceWord(ListBox1.Items[ListBox1.ItemIndex]);
    PreviousWord(Index);
end;

procedure TFormSpell.ButtonIgnoreClick(Sender: TObject);
begin
    Spell.Add(TheWord);
    PreviousWord(Index);
end;

procedure TFormSpell.ButtonSkipClick(Sender: TObject);
begin
    PreviousWord(Index);
end;

procedure TFormSpell.FormHide(Sender: TObject);
begin
    FreeandNil(Spell);
    TheKMemo.Blocks.UnLockUpdate;
    TheKMemo.SelEnd := SaveSelEnd;      // Restore selection as best we can
    TheKMemo.SelStart := SaveSelStart;  // but if spelling has changed size of intermediate text .....
end;

procedure TFormSpell.ShowContents;      // this method for debug only
// To see debug messages in ($#$@#!) windows, Lazarus->Run->Run Parameter and set (eg) --debug-log=logfile.txt
// Note that you need to exit the app to flush all content to file. And file is not zeroed on startup. Sigh.....
var
    Cnt : integer = 0;
begin
    debugln('------------');
    while Cnt < TheKMemo.Blocks.Count do begin
        debugln(TheKMemo.Blocks.Items[Cnt].ClassName + '=' + inttostr(Cnt) + ' [' + TheKMemo.Blocks.Items[Cnt].Text
        	+ '] starts at ' + inttostr(TheKMemo.Blocks.BlockToIndex(TheKMemo.Blocks.Items[Cnt])) );
        inc(Cnt);
    end;
    debugln('------------');
end;

end.

