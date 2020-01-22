unit markdown;
{
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

{$mode objfpc}{$H+}

{ This unit converts a note to markdown format.

HISTORY
    2018/12/05  This unit is pleased to serve.
    2018/12/06  Fixed a bug in Addtag, if Buff is only spaces.
    2019/05/14  Display strings all (?) moved to resourcestrings
    2019/09/27  Added SmallFont, actually subscript because markdown does not do a small font.
    2020/01/22  Enabled sending md to clipboard and saving to a file.
}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ExtCtrls, KMemo;

type

    { TFormMarkdown }

    TFormMarkdown = class(TForm)
        ButtonClose: TButton;
        ButtonCopyAll: TButton;
        ButtonSave: TButton;
        Label1: TLabel;
        Memo1: TMemo;
        Panel1: TPanel;
        SaveDialog1: TSaveDialog;
        procedure ButtonCloseClick(Sender: TObject);
        procedure ButtonCopyAllClick(Sender: TObject);
        procedure ButtonSaveClick(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure FormShow(Sender: TObject);
    private
        FSize : integer;
        SmallFont : boolean;
		Bold : boolean;
		Italics : boolean;
		HiLight : boolean;
        Underline : boolean;
        Strikeout : boolean;
        FixedWidth : boolean;
        PrevFSize : integer;
		PrevBold : boolean;
		PrevItalics : boolean;
		PrevHiLight : boolean;
        PrevUnderline : boolean;
        PrevStrikeout : boolean;
        PrevFixedWidth : boolean;
        PrevSmallFont : boolean;
		//InList : boolean;
        function AddCodeBlock(var BlkNo: integer): string;
        function AddHeading(BlkNo: integer): string;
        function AddTag(const FT: TKMemoTextBlock; var Buff: ANSIString;
            CloseOnly: boolean=False): ANSIString;
        procedure CopyLastFontAttr();
        procedure DisplayMarkDown();
        function GetNextTextBlock(const BlkNo: integer): integer;

    public
        TheKMemo : TKMemo;

    end;

var
    FormMarkdown: TFormMarkdown;

implementation

uses settings, Clipbrd;
{$R *.lfm}

{ TFormMarkdown }

procedure TFormMarkdown.FormShow(Sender: TObject);
begin
   {$ifdef DARWIN}
   Label1.Caption := 'Press Cmd-A, Cmd-C to copy';
   {$endif}
end;

procedure TFormMarkdown.ButtonCloseClick(Sender: TObject);
begin
    close;
end;

procedure TFormMarkdown.ButtonCopyAllClick(Sender: TObject);
begin
    Clipboard.astext := Memo1.text;
end;

procedure TFormMarkdown.ButtonSaveClick(Sender: TObject);
begin
    SaveDialog1.DefaultExt := 'md';
    {$ifdef UNIX}
    SaveDialog1.InitialDir :=  GetEnvironmentVariable('HOME');
    {$endif}
    {$ifdef WINDOWS}
    SaveDialog1.InitialDir :=  GetEnvironmentVariable('HOMEPATH');
    {$endif}
    SaveDialog1.Filename := StringReplace(Caption, #32, '', [rfReplaceAll]) + '.' + 'md';
    if SaveDialog1.Execute then
        Memo1.Lines.SaveToFile(SaveDialog1.Filename);
end;

procedure TFormMarkdown.FormActivate(Sender: TObject);
begin
   Memo1.Clear;
   DisplayMarkDown();
end;

procedure TFormMarkdown.CopyLastFontAttr();
begin
    PrevSmallFont := SmallFont;
  PrevFSize := FSize;
  PrevBold := Bold;
   PrevItalics := Italics;
   PrevHiLight := HiLight;
  PrevUnderline := Underline;
  PrevStrikeout := Strikeout;
  PrevFixedWidth := FixedWidth;
  PrevFSize := FSize;
end;

function TFormMarkdown.GetNextTextBlock(const BlkNo : integer) : integer;
begin
    Result := BlkNo;
    while Result < TheKmemo.Blocks.Count do begin
        if (TheKMemo.Blocks.Items[Result].ClassNameIs('TKMemoTextBlock')
                    or TheKMemo.Blocks.Items[Result].ClassNameIs('TKMemoHyperlink'))
                and (TKMemoTextBlock(TheKMemo.Blocks.Items[Result]).Text <> '') then
            exit(Result);
        inc(Result);
    end;
    Result := 0;
end;

procedure TFormMarkdown.DisplayMarkDown();
var
   Buff : ANSIstring = '';
   BlockNo : integer = 0;
   Block : TKMemoBlock;
   NextBlock : integer;
 begin
    SmallFont := false;
    Bold := false;
    Italics := False;
    HiLight := False;
    FixedWidth := False;
    try
        try
            repeat
                if BlockNo >= TheKMemo.Blocks.Count then break;
                Buff := AddHeading(BlockNo);
                if Buff <> '' then begin
                    Memo1.Append(Buff);
                    inc(BlockNo, 2);
                    continue;
                end;
                Buff := AddCodeBlock(BlockNo);       // carefull, we fiddle blockno in there ....
                if Buff <> '' then begin
                    Memo1.Append(Buff);
                    continue;
                end;

                CopyLastFontAttr();
                while  not TheKMemo.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') do begin
                    Block := TheKmemo.Blocks.Items[BlockNo];
                    if Block.ClassNameIs('TKMemoTextBlock') then begin
                         if Block.Text.Length > 0 then begin
                        	AddTag(TKMemoTextBlock(Block), Buff);
                            Buff := Buff + Block.Text;
						 end;
					end;
                    if Block.ClassNameIs('TKMemoHyperlink') then
                        Buff := Buff + Block.Text;
                    // debugln('Block=' + inttostr(BlockNo) + ' ' +BlockAttributes(Block));
                    inc(BlockNo);
                    if BlockNo >= TheKMemo.Blocks.Count then break;
				end;
                // At this stage, BlockNo points to either a Paragraph marker or beyond items
                if BlockNo < TheKMemo.Blocks.Count then
                    if TKMemoParagraph(TheKMemo.Blocks.Items[BlockNo]).Numbering = pnuBullets then
                        Buff := '* ' + Buff;
                NextBlock := GetNextTextBlock(BlockNo);
                if NextBlock > 0 then
                    AddTag(TKMemoTextBlock(TheKMemo.Blocks.Items[NextBlock]), Buff, True);
                Memo1.Append(Buff);
                inc(BlockNo);
                if BlockNo >= TheKMemo.Blocks.Count then break;
			until false;

            { At this point we may have unsaved content in Buff cos last block was not
              a Para. But it cannot be Bullet. If it was a Para, Buff is empty. But we
              could still have hanging xml tags. So either case, send it to add tag with
              an empty Font.
            }
            Buff := '';
            if SmallFont then Buff := '</sub>';
            if Bold then Buff := Buff + '**';
            if Italics then Buff := Buff + '_';
            //if HiLight then Buff := Buff + '</highlight>';
            //if Underline then Buff := Buff + '</underline>';
            if Strikeout then Buff := Buff + '~~';
            if FixedWidth then Buff := Buff + #10'```'#10;
//            if FSize <> Sett.FontNormal then
//                 Buff := Buff + SetFontXML(FSize, False);
            if length(Buff) > 0 then
                  Memo1.Append(Buff);
         Except     { TODO 1 : Must test this to see what happens with an empty
         				list of blocks. Probably makes sense to not save anything
                        that does not have at least one TKMemotextBlock  }
            on EListError do begin
            	Memo1.Append(Buff);
            end;
        end;
 	finally

    end;
end;

// Called on first block after a paragraph marker, deals with single block, whole para in monospace
// Must be one block followed by paragaraph marker. If it cannot help, returns empty string.
// Assumes an para that starts with Mono font is all code. No arguments !
// Sequential mono paras are kept together as long as nothing between them.
function TFormMarkDown.AddCodeBlock(var BlkNo : integer) : string;
var
   Found : integer = 0;
   Starting : integer;
begin
    Starting := BlkNo;
    Result := #10'```'#10;
    while TheKMemo.Blocks.Items[BlkNo].ClassNameIs('TKMemoTextBlock')
               and (TKMemoTextBlock(TheKMemo.Blocks.Items[BlkNo]).TextStyle.Font.Pitch = fpFixed)
        do begin
            inc(Found);
            while TheKMemo.Blocks.Items[BlkNo].ClassNameIs('TKMemoTextBlock') do begin
                Result := Result + TKmemoTextBlock(TheKMemo.Blocks.Items[BlkNo]).Text;
                inc(BlkNo);
                if BlkNo >= TheKmemo.Blocks.Count then break;
            end;
            if BlkNo >= TheKmemo.Blocks.Count then break;
            // OK, if to here, we must be on a para marker, end of that line.
            Result := Result + #10;
            inc(BlkNo);              // step over the para marker
    end;
    // BlkNo is now pointing to block after the para marker at end of a mono para
    // or, possibly its pointing beyond kmemo and maybe we did not find a Mono para ?
    if Found > 0 then
        Result := Result + #10'```'#10
    else begin
        Result := '';
        BlkNo := Starting;  // I didn't mess with it ....
    end;
end;

// Called on first block after a paragraph marker, deals with larger fonts that are headers
// Must be one block followed by paragaraph marker. If it cannot help, returns empty string.
function TFormMarkdown.AddHeading(BlkNo : integer) : string;
begin
    Result := '';
    if TheKMemo.Blocks.Items[BlkNo].ClassNameIs('TKMemoTextBlock')
                and ((BlkNo +1) < TheKMemo.Blocks.Count)
                and TheKMemo.Blocks.Items[BlkNo+1].ClassNameIs('TKMemoParagraph')
    then begin
        // OK, its a single block line. But is it a heading ?

        if TKmemoTextBlock(TheKMemo.Blocks.Items[BlkNo]).TextStyle.Font.Size = Sett.FontTitle then
              Result := '# ' + TKmemoTextBlock(TheKMemo.Blocks.Items[BlkNo]).Text;
        if TKmemoTextBlock(TheKMemo.Blocks.Items[BlkNo]).TextStyle.Font.Size = Sett.FontHuge then
              Result := '## ' + TKmemoTextBlock(TheKMemo.Blocks.Items[BlkNo]).Text;
        if TKmemoTextBlock(TheKMemo.Blocks.Items[BlkNo]).TextStyle.Font.Size = Sett.FontLarge then
              Result := '### ' + TKmemoTextBlock(TheKMemo.Blocks.Items[BlkNo]).Text;
    end;
end;

// AddTag deals with markup that has a pre and post componet, ie **Bold**
function TFormMarkdown.AddTag(const FT : TKMemoTextBlock; var Buff : ANSIString; CloseOnly : boolean = False) : ANSIString;
var
   BorrowedSpaces : integer = 0;
begin
    // Important that we keep the tag order consistent. Good xml requires no cross over
    // tags. If the note is to be readable by Tomboy, must comply. (EditBox does not care)
    // Tag order -
    // FontSize HiLite Ital Bold Bullet TEXT BulletOff BoldOff ItalOff HiLiteOff FontSize
	// Processing Order is the reverse -
    // ListOff BoldOff ItalicsOff HiLiteOff FontSize HiLite Ital Bold List

    //debugln(BlockAttributes(FT));

  // When Bold Turns OFF
   if Buff <> '' then           // remove, temporarly, any trailing spaces
       while Buff[length(Buff)] = ' ' do begin
            inc(BorrowedSpaces);
            delete(Buff, length(Buff), 1);
            if Buff = '' then break;
       end;

   if CloseOnly then begin      // In closeonly mode, we are just shuttig them all done prior to newline
       if Bold then begin
           Buff := Buff + '**';
           Bold := false;
       end;
       if Italics then begin
            Buff := Buff + '_';
            Italics := false;
       end;
       if Strikeout then begin
           Buff := Buff + '~~';
           Strikeout := false;
       end;
       if FixedWidth then begin
           Buff := Buff + '`';
           FixedWidth := False;
       end;
       if SmallFont then begin
           Buff := Buff + '</sub>';
           SmallFont := False;
       end;
   end;

    // Normal mode.
    // When smallfont turns off
    if (SmallFont and (FT.TextStyle.Font.Size <> Sett.FontSmall)) then begin
        Buff := Buff + '</sub>';
        SmallFont := False;
    end;
    if (Bold and (not (fsBold in FT.TextStyle.Font.Style))) then begin
        Buff := Buff + '**';
        Bold := false;
    end;
    // When Italic turns OFF
    if (Italics and (not (fsItalic in FT.TextStyle.Font.Style))) then begin
		if Bold then Buff := Buff + '**';
        Buff := Buff + '_';
        if Bold then Buff := Buff + '**';
        Italics := false;
    end;
    // When Strikeout turns OFF
    if (Strikeout and (not (fsStrikeout in FT.TextStyle.Font.Style))) then begin
                  if Bold then Buff := Buff + '**';
                  if Italics then Buff := Buff + '_';
                  Buff := Buff + '~~';
                  if Italics then Buff := Buff + '_';
                  if Bold then Buff := Buff + '**';
                  Strikeout := false;
    end;

    // Full para fixed with is already looked after, here we deal with bits in an a para
    // When FixedWidth turns OFF
    //if (FixedWidth <> (FT.TextStyle.Font.Pitch = fpFixed) or (FT.TextStyle.Font.Name = MonospaceFont)) then begin
    if (FixedWidth and ((FT.TextStyle.Font.Pitch <> fpFixed) {or (FT.TextStyle.Font.Name <> MonospaceFont)})) then begin
                  if Bold then Buff := Buff + '**';
                  if Italics then Buff := Buff + '_';
                  if Strikeout then Buff := Buff + '~~';
                  Buff := Buff + '`';
                  if Strikeout then Buff := Buff + '~~';
                  if Italics then Buff := Buff + '_';
                  if Bold then Buff := Buff + '**';
                  FixedWidth := false;
    end;

    while BorrowedSpaces > 0 do begin
        Buff := Buff + ' ';
        dec(BorrowedSpaces);
    end;
    if CloseOnly then exit(Buff);

    // FixedWidth turns ON
    if ((not FixedWidth) and ((FT.TextStyle.Font.Pitch = fpFixed))) then begin
        if Bold then Buff := Buff + '**';
        if Italics then Buff := Buff + '_';
        if Strikeout then Buff := Buff + '~~';
        Buff := Buff + '`';
        if Strikeout then Buff := Buff + '~~';
        if Italics then Buff := Buff + '_';
        if Bold then Buff := Buff + '**';
        FixedWidth := true;
    end;

    // Strikeout turns ON
    if ((not Strikeout) and (fsStrikeout in FT.TextStyle.Font.Style)) then begin
        if Bold then Buff := Buff + '**';
        if Italics then Buff := Buff + '_';
        Buff := Buff + '~~';
        if Italics then Buff := Buff + '_';
        if Bold then Buff := Buff + '**';
        Strikeout := true;
    end;

    // Italic turns On
    if ((not Italics) and (fsItalic in FT.TextStyle.Font.Style)) then begin
        if Bold then Buff := Buff + '**';
        Buff := Buff + '_';
        if Bold then Buff := Buff + '**';
        Italics := true;
    end;

    // Bold turns On
    if ((not Bold) and (fsBold in FT.TextStyle.Font.Style)) then begin
        Buff := Buff + '**';
        Bold := true;
    end;

    // SmallFont turns on
    if ((not SmallFont) and (FT.TextStyle.Font.Size = Sett.FontSmall)) then begin
        Buff := Buff + '<sub>';
        SmallFont := True;
    end;
    Result := Buff;
end;


end.

