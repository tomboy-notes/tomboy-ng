unit kmemo2pdf;

{    Copyright (C) 2017-2023 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------

	This form will conver an in memory KMemo note to a PDF. At this stage, it requires
    creating, geting told a few things (The Kmemo, a default font, etc) and call show.

    It still needs to offer some options to the user and manage sone file overwriting issues.

    I need to find out where Windows and Mac keep their fonts.

    Its probably needs an awful lot of testing ...

}





{$mode ObjFPC}{$H+}

interface

uses  {$ifdef unix}cwstring,{$endif}
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
    kmemo, k_prn, fpimage, fppdf, fpparsettf, fpttf, typinfo, tb_utils, KFunctions;

type
    PFontRecord=^TFontRecord;
    TFontRecord=record
      FamName:string;       // eg 'Ubuntu', 'FreeSans'
      Bold : boolean;
      Italic : boolean;
      FontIndex : integer;  // -1 if not set yet
    end;

type
    { TFontList }

    TFontList = class(TFPList)
        private
            function Get(Index : Integer) : PFontRecord;
        public
            destructor Destroy; Override;
            procedure Add(TheFont: ANSIString; Bold, Italic: boolean);
            procedure Dump();
            // Returns FontIndex of passed font record, -1 is not present;
            function Find(TheFont: ANSIString; Bold, Italic, FIndex: boolean): integer;
            property Items[Index : integer] : PFontRecord read Get; default;
    end;




type

    { TFormKMemo2pdf }

    TFormKMemo2pdf = class(TForm)
        BitBtn1: TBitBtn;
        BitBtn2: TBitBtn;
        procedure BitBtn2Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
    private
        BulletIndent : integer;      // Bullet Indent (not including bullet), zero means no bullet;
        FontList : TFontList;
        WordList : TWordList;
        WordIndex : integer;         // Points to a word in wordlist, must be less than WordList.count
        FPage: integer;
        FDoc: TPDFDocument;
        CurrentPage : integer ;      // Starts at zero ?
        function CheckHeight: integer;
        function GetWordDimentions(const WI: integer; out W: integer; var H: integer;
            AltText: string = ''): boolean;
        function KMemoRead(): boolean;
        function LoadFonts(): boolean;
        function MakePDF: boolean;
        procedure SaveDocument();
        function WriteLine(Page: TPDFPage; XLoc: integer; const Y: integer): boolean;
        function WritePage(): boolean;
    public
        TheKMemo : TKMemo;
        DefaultFont : string;        // name of the default font, used if cannot find indicated one
    end;

var
    FormKMemo2pdf: TFormKMemo2pdf;

implementation

{$R *.lfm}

{ TFormKMemo2pdf }


const   TopMargin = 15;
        BottomMargin = 10;
        SideMargin = 15;
        PageHeight = 297;
        PageWidth = 210;
        LineHeight = 6;

{ TFontList }

function TFontList.Get(Index: Integer): PFontRecord;
begin
    Result := PFontRecord(inherited get(Index));
end;

destructor TFontList.Destroy;
var
    I : integer;
begin
    for I := 0 to Count-1 do begin
        dispose(Items[I]);
    end;
    inherited Destroy;
end;

procedure TFontList.Add(TheFont: ANSIString; Bold, Italic: boolean);
var
    P : PFontRecord;
begin
    if Find(TheFont, Bold, Italic, False) = -1 then begin
        new(P);
        P^.FamName := TheFont;
        P^.Bold := Bold;
        P^.Italic := Italic;
        P^.FontIndex := -1;
        inherited Add(P);
    end;
end;

procedure TFontList.Dump();
var i : integer;
begin
  writeln('TFontList.Dump we have ' + count.tostring + ' items');
  for i := 0 to count -1 do
    writeln('FONT : ', Items[i]^.FamName, ' B=', booltostr(Items[i]^.Bold, True), ' I=', booltostr(Items[i]^.Italic, True));
end;

function TFontList.Find(TheFont: ANSIString; Bold, Italic, FIndex: boolean): integer;
var i : integer;
begin
    Result := -1;
    for i := 0 to count -1 do
        if (Items[i]^.FamName = TheFont) and (Items[i]^.Bold = Bold)
                    and (Items[i]^.Italic = Italic) then begin
            if FIndex then
                Result := Items[i]^.FontIndex
            else Result := i;
           break;
        end;
end;

// Returns with width and height in mm of the word in wordlist pointed to by WI
// If AltText contains anything, it will be used instead of the text in WordList
// but font and associated settings from WordList will still be used.
function TFormKMemo2PDF.GetWordDimentions(const WI : integer; out W : integer; var H : integer; AltText : string = '') : boolean;
var
    CachedFont : TFPFontCacheItem;
    SWidthF, DescenderH : single;
    LocH : integer;
begin
    if AltText = '' then
           AltText := WordList[WI]^.AWord;
    CachedFont := gTTFontCache.Find(WordList[WI]^.FName, WordList[WI]^.Bold, WordList[WI]^.Italic);
    if not Assigned(CachedFont) then  begin
        writeln('INFO - TFormKMemo2pdf.GetWordDimentions Cannot find Font in Cache : ', WordList[WI]^.FName, ' ', WordList[WI]^.Bold, ' ', WordList[WI]^.Italic);
        exit(False);
    end;
    SWidthF := CachedFont.TextWidth(AltText,  WordList[WI]^.Size);
    W := round((SWidthF * 25.4) / gTTFontCache.DPI);
    LocH := round(CachedFont.TextHeight(AltText, WordList[WI]^.Size, DescenderH) / 2.0);
    // writeln('TFormKMemo2pdf.GetWordDimentions LocH=', LocH);
    if LocH > H then H := LocH;
end;

(*const //Bullet = '• ';
      Bullet = '* ';      *)

// Trys to print out a line of words, word by word, at indicated starting point.
// returns false if there is no more words to print.
function TFormKMemo2pdf.WriteLine(Page : TPDFPage; XLoc : integer; const Y : integer) : boolean;
var
    W, H : integer; // word dimensions
    i : integer;
    Bullet : string;

    function ExtraIndent(Bull : TKMemoParaNumbering) : integer;
    begin
        case Bull of
            pnuNone       : result := 0;         // The bullet is at indicated indent
            BulletOne     : result := 1;         // and the text starts the width of bullet
            BulletTwo     : result := 6;         // and a couple of spaces further in.
            BulletThree   : result := 11;        // mm.
            BulletFour    : result := 16;
            BulletFive    : result := 21;
            BulletSix     : result := 26;
            BulletSeven   : result := 31;
            BulletEight   : result := 36;
        end;
    end;

begin
    H := 0;
    i := 0;
    if WordList[WordIndex]^.ABullet <> pnuNone then  begin // We arrive at the start of a line, is it a bullet line ?
        Bullet := UnicodeToNativeUTF(cRoundBullet) + '  ';
           if not GetWordDimentions(WordIndex, W, H, Bullet) then exit(False);    // font fault unlikely, we have already checked this block.
           BulletIndent := ExtraIndent(WordList[WordIndex]^.ABullet);
           Page.WriteText(BulletIndent + XLoc, Y, Bullet);                        // u2022
           inc(BulletIndent, W);
    end;
    while WordIndex < WordList.Count do begin
        inc(i);
        if i > 2000 then break;
        if WordList[WordIndex]^.NewLine then begin
            inc(WordIndex);
            BulletIndent := 0;
            exit(true);
        end;
        if not GetWordDimentions(WordIndex, W, H) then exit(False);             // be a font fault most likely
        Page.SetFont(   FontList.Find(WordList[WordIndex]^.FName, WordList[WordIndex]^.Bold, WordList[WordIndex]^.Italic, True), WordList[WordIndex]^.Size);
        if (XLoc+W+BulletIndent) > (PageWidth - SideMargin) then exit(true);    // no more on this line.
        writeln('TFormKMemo2pdf.WriteLine will WriteText T=', WordList[WordIndex]^.AWord, ' F=', WordList[WordIndex]^.FName, ' X=', BulletIndent + XLoc, ' W=', W);
        Page.WriteText(BulletIndent + XLoc, Y, WordList[WordIndex]^.AWord);
        XLoc := XLoc+W;
        inc(WordIndex);
    end;
    result := false;    // no more to do.
end;


                                // does a non write scan of next block of words returning the necessary Height
                                // to push Y down to to provide clearance for any larger text. Do We have to push down
                                // by less if first char of the line is higher ? test run is very strange !
                                // Return 0 for a blank line (just inc Y by line height, don't run WriteLine)
function TFormKMemo2pdf.CheckHeight() : integer;
var
    W, i, WI : integer; // word dimensions
    Xloc : integer = SideMargin;
    OldFont : string;
begin
    i := 0;
    WI := WordIndex;
    Result := 0;
    while WI < WordList.Count do begin
        inc(i);
        if i > 2000 then break;
        if WordList[WI]^.NewLine then begin
            //BulletIndent := 0;
            exit;
        end;
        if not GetWordDimentions(WI, W, Result) then begin
            OldFont := WordList[WI]^.FName;
            WordList[WI]^.FName := DefaultFont;                                // We will change it to default font and try again
            if not GetWordDimentions(WI, W, Result) then begin
                Writeln('ERROR - TFormKMemo2pdf.CheckHeight Failed to load the requested font : ', OldFont);
                exit;
            end;
        end;
//        writeln('TFormKMemo2pdf.WriteLine Setting FontIndex to ', FontList.Find(WordList[Wi]^.FName, WordList[WI]^.Bold, WordList[WI]^.Italic)
//            , ' and name is ', WordList[WI]^.FName);
        if (Xloc+W) > (PageWidth - SideMargin) then exit;
        if WI >= WordList.Count then exit;
        XLoc := XLoc+W;
        inc(WI);
    end;
end;

function TFormKMemo2pdf.LoadFonts() : boolean;
var
    I  : integer;
    CachedFont : TFPFontCacheItem;
    Suffix : string = '';
begin
    for i := 0 to FontList.Count -1 do begin

        //writeln('TFormKMemo2pdf.LoadFonts FontName=', FontList[i]^.FamName);
        Suffix := '';
        CachedFont := gTTFontCache.Find(FontList[i]^.FamName, FontList[i]^.Bold, FontList[i]^.Italic);
        if not Assigned(CachedFont) then begin
            writeln('INFO - TFormKMemo2pdf.LoadFonts could not find font in cache ', FontList[i]^.FamName);
            continue;                                                           // We will replace it with default
        end;
        if  FontList[i]^.Bold then Suffix := 'B';
        if  FontList[i]^.Italic then Suffix := Suffix + 'I';
        if Suffix <> '' then Suffix := '-' + Suffix;

        FontList[i]^.FontIndex := FDoc.AddFont(CachedFont.FileName, CachedFont.FamilyName + Suffix);

        // FontList[i]^.FontIndex := FDoc.AddFont(gTTFontCache.Items[i].FileName,  gTTFontCache.Items[i].FamilyName + Suffix);
        // writeln('TFormKMemo2pdf.LoadFonts Font=', CachedFont.FamilyName + Suffix, ' FontIndex=',FontList[i]^.FontIndex, ' Family=', FontList[i]^.FamName);
    end;
end;



// sends a page of text to the Document, returning True when its all done.
function TFormKMemo2pdf.WritePage() : boolean;
var
    X : integer = SideMargin;       // how far we are across the page, left to right, mm
    Y : integer = TopMargin;        // how far we are down the page, top to botton, mm
    Page : TPDFPage;
    i, Yt : integer;


begin
    Result := true;
    // writeln('TFormKMemo2pdf.WritePage starting page ===== ' + inttostr(CurrentPage));
    Page := FDoc.Pages[CurrentPage];
    // writeln('TFormKMemo2pdf.WritePage Fonts loaded for this page -');
//    for i := 0 to Page.Document.Fonts.Count-1 do begin
//        writeln('Found in Page ' + Page.Document.Fonts[i].Name + ' DisplayName=' + Page.Document.Fonts[i].DisplayName );
//    end;
    while Result do begin        // Returns false if it has run out of words
        Yt := CheckHeight();     // Does not inc WordIndex.
        inc(Y, Yt);
        if Y > (PageHeight - TopMargin - BottomMargin) then exit;
        if Yt <> 0 then
             Result := WriteLine(Page, X, Y)   // Does inc WordIndex
        else begin
           inc(Y, LineHeight);
           inc(WordIndex);
        end;
        if WordIndex >= WordList.Count then Result := false;
        // writeln('TFormKMemo2pdf.WritePage writing at ', X, ' ', Y);
    end;
end;

function TFormKMemo2pdf.MakePDF : boolean;
var
  P: TPDFPage;
  S: TPDFSection;
  Opts: TPDFOptions;
begin
    FDoc.Infos.Title := 'Get the note title please';
    FDoc.Infos.Author := 'tomboy-ng notes';
    FDoc.Infos.Producer := 'fpGUI Toolkit 1.4.1';
    //Result.Infos.ApplicationName := ApplicationName;
    FDoc.Infos.CreationDate := Now;
    Opts := [poPageOriginAtTop];
    Include(Opts, poSubsetFont);
    Include(Opts, poCompressFonts);
    Include(Opts,poCompressText);
    FDoc.Options := Opts;
    FDoc.StartDocument;
    S := FDoc.Sections.AddSection; // we always need at least one section
    LoadFonts();
    repeat
        if CurrentPage > 15 then break;

        P := FDoc.Pages.AddPage;
        P.PaperType := ptA4;
        P.UnitOfMeasure := uomMillimeters;
        S.AddPage(P);               // Add the Page to the Section
        inc(CurrentPage);
    until not WritePage();
    SaveDocument();
end;

procedure TFormKMemo2pdf.SaveDocument();
var
    F: TFileStream;
begin
  F := TFileStream.Create('test.pdf',fmCreate);
  try try
    FDoc.SaveToStream(F);
//    Writeln('Document used ',FDoc.ObjectCount,' PDF objects/commands');
  except on E: Exception do
    writeln('ERROR - Failed to save the PDF ' + E.Message);
  end;
  finally
    F.Free;
  end;
end;

procedure TFormKMemo2pdf.BitBtn2Click(Sender: TObject);
var
    i : integer;
begin
    WordList := nil;
    CurrentPage := -1;
    If WordList <> nil then
        FreeAndNil(WordList);
    FontList := TFontList.Create;
    WordList := TWordList.Create;
    KMemoRead();
    FontList.Dump();
    // WordList.Dump();
    // This is for later, it does no make fonts available to the PDF
    gTTFontCache.SearchPath.Add('/usr/share/fonts/');
    gTTFontCache.BuildFontCache;

    for i := 0 to gTTFontCache.Count -1 do
     if gTTFontCache.Items[i].FamilyName = 'Ubuntu' then
         writeln(gTTFontCache.Items[i].FamilyName
            + ' - ' + gTTFontCache.Items[i].FileName
            + ' - ' + gTTFontCache.Items[i].HumanFriendlyName
            + ' - ' + booltostr(gTTFontCache.Items[i].IsBold, true)
            + ' - ' + booltostr(gTTFontCache.Items[i].IsItalic, true)
            + ' - ' + booltostr(gTTFontCache.Items[i].IsFixedWidth, true)
            + ' - ' + booltostr(gTTFontCache.Items[i].IsRegular, true));

    FDoc := TPDFDocument.Create(Nil);
    FDoc.FontDirectory := '/usr/share/fonts';            // ToDo : this is NOT cross platform

    MakePDF();
    FDoc.Free;
    FreeAndNil(FontList);
    FreeAndNil(WordList);
end;

// Builds a WordList based on the KMemo. Each word will have a trailing space if applicable
// A newline has an empty list item and NewLine true.
function TFormKMemo2pdf.KMemoRead() : boolean;
var
    BlockNo : integer = 0;
    I : integer;
    ExFont : TFont;
    St : ANSIString = '';

        procedure CopyFont(FromFont : TFont);
        begin
            ExFont.Bold := FromFont.Bold;
            ExFont.Italic := FromFont.Italic;
            ExFont.Size := FromFont.Size;
            ExFont.Color := FromFont.Color;
            ExFont.Name := FromFont.Name;
        end;
begin
    ExFont := TFont.Create();
    for BlockNo := 0 to TheKMemo.Blocks.Count-1 do begin
        if not TheKMemo.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin
           CopyFont(TKMemoTextBlock(TheKmemo.Blocks.Items[BlockNo]).TextStyle.Font);        // copies to ExFont
           for I := 0 to TheKMemo.Blocks.Items[BlockNo].WordCount-1 do begin
               FontList.Add(ExFont.Name, ExFont.Bold, ExFont.Italic);
               St := TheKMemo.Blocks.Items[BlockNo].Words[I];
               WordList.Add(St, ExFont.Size, ExFont.Bold, ExFont.Italic, False, ExFont.Color, ExFont.Name);
           end;
        end else begin
            WordList.Add('', 0, False, False, True, clBlack);   // TKMemoParagraph, thats easy but is it a Bullet
            if (TKMemoParagraph(Thekmemo.blocks.Items[BlockNo]).Numbering <> pnuNone) then begin
               // We want to mark start of bullet, not KMemo's way of marking at the end.
               I := WordList.Count -2;                          // Thats just before the current one
               while I > -1 do begin
                       if WordList[i]^.NewLine then break;
                       dec(i);
               end;
               if i < 0 then                                    // under run, bullet must be first line ??
                      WordList[0]^.ABullet := TKMemoParagraph(Thekmemo.blocks.Items[BlockNo]).Numbering
               else WordList[i+1]^.ABullet := TKMemoParagraph(Thekmemo.blocks.Items[BlockNo]).Numbering;  // Must be the one we want. Mark first word after NL
            end;
        end;
    end;
    FreeandNil(ExFont);
    WordIndex := 0;
    result := (WordList.Count > 1);
    WordList.Dump();
end;

{   (TKMemoParagraph(Thekmemo.blocks.Items[BlockNo]).Numbering = pnuBullets)
    pnuNone, BulletOne .. BulletEight
}

procedure TFormKMemo2pdf.FormCreate(Sender: TObject);
begin
    WordList := nil;
    CurrentPage := -1;
end;

procedure TFormKMemo2pdf.FormShow(Sender: TObject);
begin

end;

end.

