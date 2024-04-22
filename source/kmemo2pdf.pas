unit kmemo2pdf;

{    Copyright (C) 2017-2024 David Bannon

    License:
    This code is licensed under MIT License, see the file License.txt
    or https://spdx.org/licenses/MIT.html  SPDX short identifier: MIT

    ------------------

	This form will convert an in memory KMemo note to a PDF. At this stage, it requires
    creating, getting told a few things (The Kmemo, a default font, etc) and call StartPDF.
    The form itself is not shown unless something has gone wrong, in which case StartPDF
    will return false. Some messages are shown in the memo ....

    So, we first populate the FontCache, then see if we can find a proportional and
    fixed spacing font suitable for our needs. Then we iterate over the KMemo, adding
    all words into a list along with font, size, color information. At the same time
    we add any extra font requirements to FontList (ie combinations of name, Bold, Italic).
    If thats all OK, we add the Fonts in FontList (with the font's full filename)
    the the doc, they become embeddded fonts.

    Then its just a case of iterating over that word list, checking size of each word
    to get wrapping and line spacing correct.

    Clean up.

    Note -
    -*- This model should work with any TTF fonts, but possibly ony ones that have seperate
        font files for each 'style' such as bold, italic.
    -*- I found I had to give each font loaded into doc a different name as well as its
        full file name. But that name is not interesting after that ?  Strange.
    -*- Using the standard formal Adobe fonts (and a local equivilent for sizing) is
        a bit simpler and makes for smaller files but cannot handle UTF8 chars.

    Need lots of testing.



    See https://gitlab.com/freepascal.org/fpc/source/-/issues/30116 for issues about fonts !
    https://forum.lazarus.freepascal.org/index.php/topic,62254.0.html - my question on the topic.

    Fonts that mention bold and italic in the Font selection dialog do not necessarily
    have seperate font files for bold and italic, they may generate them !  But fpPDF does not.



    History :
        2023-02-14 Initial Release of this unit.
        2024-03-12 Use force fonts to Helvetica or Courier, seems to work evn when not present.


    The standard 14 PDF fonts – that can be referenced by name in a PDF document – are:

        Times-Roman                  Microsoft have Times New Roman, is that the same ?
        Times-Bold
        Time-Italic
        Time-BoldItalic
        Courier                      Microsoft have Courier New, Linux Liberation Mono or FreeMono ??
        Courier
        Courier-Bold
        Courier-Oblique
        Helvetica                      // Helvetica is no longer shipped with Windows
        Helvetica-Bold
        Helvetica-Oblique
        Helvetica-BoldOblique
        Symbol
        ZapfDingbats

    Pretty useless however !
}



{$mode ObjFPC}{$H+}

interface

uses  {$ifdef unix}cwstring,{$endif}
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, fpttf,
    kmemo, k_prn, fpimage, fppdf, fpparsettf,  typinfo, tb_utils, KFunctions;



type                                 // TFontList not used but might be if I revisit, at present, Adobe fonts don't do UTF8
    PFontRecord=^TFontRecord;
    TFontRecord=record
      FamName:string;       // eg 'Ubuntu', 'FreeSans', 'Courier New', 'Liberation Sans'
      Bold : boolean;
      Italic : boolean;
      Fixed : Boolean;      // Fixed spacing or not
      FontIndex : integer;  // As assigned by PDF Doc, AddFont(), -1 if not set yet
      FileName : string;    // full path to this specific font.  Get from cache
    end;

 type
    { TFontList }

    TFontList = class(TFPList)
        private
            function Get(Index : Integer) : PFontRecord;
            function FindInFontCache(FamName : string; Bold, Italics : boolean) : boolean;
            function LoadFonts(FDoc : TPDFDocument) : boolean;
        public
            constructor Create();
            destructor Destroy; Override;
                            { Passed font family name and it returns false if its unusable. If
                              true, its added  along with extra data to fontlist.
                             }
            function Add(const TheFont: ANSIString; const Bold, Italic: boolean; Fixed: Boolean): boolean;
            procedure Dump();
                            // Returns FontIndex of passed font record, -1 is not present;
            function Find(TheFont: ANSIString; Bold, Italic, FIndex: boolean): integer;
            property Items[Index : integer] : PFontRecord read Get; default;
    end;

type
    { TFormKMemo2pdf }
    TFormKMemo2pdf = class(TForm)
        BitBtn1: TBitBtn;
        BitBtnProceed: TBitBtn;
        Memo1: TMemo;
        procedure BitBtnProceedClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
    private
        TestFontProp, TestFontMono : string; // Are the fonts we use to calculate layout, but don't really use.
        AllowNoBoldItalic : boolean;// Allow use of font that does not have its own Bold or Italic font file.
        BulletIndent : integer;     // Bullet Indent (not including bullet), zero means no bullet;
        WordList : TWordList;       // A list of all the words and their details such as font, size etc. Populated in KMemoRead().
        WordIndex : integer;        // Points to a word in wordlist, must be less than WordList.count
        FontList : TFontList;       // A list of the fonts we will use in this document
        //FPage: integer;
        FDoc: TPDFDocument;
        CurrentPage : integer ;     // Starts at zero ?

                                    // Checks height of line to be rendered, so that base line can accomodate.
        function CheckHeight: integer;
                                    // Returns true if we have named local font in our cache.
        function FontInCache(FName: string; Bold, Italic: boolean): boolean;
                                    // Returns with width and height in mm of the word in wordlist pointed to by WI
                                    // If AltText contains anything, it will be used instead of the text in WordList
                                    // but font and associated settings from WordList will still be used.
        function GetWordDimentions(const WI: integer; out W: integer; var H: integer; AltText: string = ''): boolean;
                                    // Builds a WordList based on the KMemo. Each word will have a trailing space if applicable
                                    // A newline has an empty list item and NewLine true. Tries to register a suitable
                                    // font for each word but it may get told by FontList.Add() to use another font.
        function KMemoRead(): boolean;

                                    // Generates the PDF, might return False if it finds the available fonts
                                    // do not include a needed bold or italic version, user invited to proceed
                                    // anyway (without font tricks). However, thats now unlikely as we will
                                    // have already changed to a "know good" font. Hopefully.
        function MakePDF: boolean;
        procedure SaveDocument();
                                    // Tries to print out a line of words, word by word, at indicated starting point.
                                    // returns false if there is no more words to print.
        function WriteLine(Page: TPDFPage; XLoc: integer; const Y: integer): boolean;
                                    // sends a page of text to the Document, returning True when its all done.
        function WritePage(): boolean;
    public

        TheKMemo : TKMemo;          // This is the KMemo, set it before showing the form.
        TheTitle : string;
        FFileName : string;         // Full filename including path,
        DefaultFont : string;        // New Text in a KMemo is saved with font='default', only after reload does it have true name
                            // Public function to initiate the PDF. If it returns False then
                            // show user the form with some error or advice messages. Its
                            // going to be a font problem most likely, see header.
        function StartPDF: boolean;
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

        // These two arrays contain fonts that I know are suitable for this use. We use
        // the first one found on the given system. Add more here but read notes above.
        FontsFixed : array of string = ('Liberation Mono', 'Courier New', 'Courier');
        FontsVariable : array of string = ('Liberation Sans', 'Lucida Grande', 'Arial', 'Helvetica');

//        FontsFixed : array of string = ('Monaco', 'Menlo','Courier New');      // Darwin
//        FontsVariable : array of string = ('Lucida Grande', 'Geneva', 'Arial');  // Darwin


{ ------------------------------ TFontList ------------------------------------}

function TFontList.Get(Index: Integer): PFontRecord;
begin
    Result := PFontRecord(inherited get(Index));
end;

function TFontList.FindInFontCache(FamName: string; Bold, Italics: boolean): boolean;
var
    CachedFont : TFPFontCacheItem;
begin
  CachedFont := gTTFontCache.Find(FamName, Bold, Italics);
  result := Assigned(CachedFont);
end;

function TFontList.LoadFonts(FDoc: TPDFDocument): boolean;
var
    i : integer;
    FakeName : string;          // Davos invention, FDoc wants different names for each here
begin                           // but it does not need to know those names anywhere else. ?
  result := true;
  for i := 0 to count -1 do begin
    FakeName := Items[i]^.FamName;
    if  Items[i]^.Bold or Items[i]^.Italic then
           FakeName := FakeName + '-';
    if  Items[i]^.Bold then
           FakeName := FakeName + 'B';
    if Items[i]^.Italic then
           FakeName := FakeName + 'I';
      Items[i]^.FontIndex := FDoc.AddFont(Items[i]^.FileName, FakeName);
  end;
end;

constructor TFontList.Create();
begin
    inherited Create;
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

function TFontList.Add(const TheFont: ANSIString; const Bold, Italic: boolean; Fixed : Boolean) : boolean;
var
    P : PFontRecord;
    CachedFont : TFPFontCacheItem;

    procedure InsertIntoList(SubName : string = '');
    begin
        new(P);
        if SubName <> '' then begin
            P^.FamName := SubName;
        end else begin
            P^.FamName := TheFont;
        end;
        P^.Bold := Bold;
        P^.Italic := Italic;
        P^.FontIndex := -1;         // we fill this in later, when assigning the font to PDF Doc
        P^.Fixed := Fixed;
        P^.FileName := CachedFont.FileName;
        inherited Add(P);
        // writeln( 'InsertIntoList count=', count);
    end;

begin
    Result := True;
    if Find(TheFont, Bold, Italic, False) > -1 then
        Exit;    // All good, its already here.
    CachedFont := gTTFontCache.Find(TheFont, Bold, Italic);
    if Assigned(CachedFont) then
        InsertIntoList()
    else Result := False;           // That is, this font is not locally available. Sorry.
end;

procedure TFontList.Dump();         // WARNING, uses writeln
var i : integer;
begin
  writeln('TFontList.Dump we have ' + count.tostring + ' items ========');
  for i := 0 to count -1 do
    writeln('FONT : ', Items[i]^.FamName, ' B=', booltostr(Items[i]^.Bold, True)
        , ' I=', booltostr(Items[i]^.Italic, True), ' Idx=', inttostr(Items[i]^.FontIndex), ' FileName=', Items[i]^.FileName);
end;

function TFontList.Find(TheFont: ANSIString; Bold, Italic, FIndex: boolean): integer;
var i : integer;
begin
    Result := -1;
    for i := 0 to count -1 do
        if (Items[i]^.FamName = TheFont) and (Items[i]^.Bold = Bold)
                    and (Items[i]^.Italic = Italic) then begin
            if FIndex then
                Result := Items[i]^.FontIndex       // thats data saved in the record.
            else Result := i;
           break;
        end;
end;



{ ----------------------------- TFormKMemo2PDF --------------------------------}

function TFormKMemo2PDF.GetWordDimentions(const WI : integer; out W : integer; var H : integer; AltText : string = '') : boolean;
var
    CachedFont : TFPFontCacheItem;
    SWidthF, DescenderH : single;
    LocH : integer;
    TestFontName : string;

begin
    if WordList[WI]^.Fixed then
         TestFontName := TestFontMono
    else TestFontName := TestFontProp;
    Result := true;
    //writeln('INFO - TFormKMemo2pdf.GetWordDimentions WI=' + inttostr(WI) + ' and Wordlist.Count=' + inttostr(WordList.count));
    if AltText = '' then
           AltText := WordList[WI]^.AWord;
    //writeln('INFO - TFormKMemo2pdf.GetWordDimentions looking at ' + WordList[WI]^.AWord + ' Font=' + WordList[WI]^.FName);
    CachedFont := gTTFontCache.Find(WordList[WI]^.FName, WordList[WI]^.Bold, WordList[WI]^.Italic);

    if not Assigned(CachedFont) then  begin                                     // Well, that should not happen !
        //writeln('INFO - TFormKMemo2pdf.GetWordDimentions Cannot find Font in Cache : ', WordList[WI]^.FName, ' ', WordList[WI]^.Bold, ' ', WordList[WI]^.Italic);
        // We will try a plain version of same font and if that works, change entry in WordList
        CachedFont := gTTFontCache.Find(TestFontName, False, False);
        if Assigned(CachedFont) then begin
            WordList[WI]^.Bold := False;                                        // ToDo : this is very, very ugly must understand fonts better
            WordList[WI]^.Italic := False;
        end else begin
            //writeln('ERROR - TFormKMemo2pdf.GetWordDimentions Cannot find Font in Cache : ', WordList[WI]^.FName, ' Plain Font');
            memo1.append('ERROR - TFormKMemo2pdf.GetWordDimentions Cannot find Font in Cache : ' + WordList[WI]^.FName + '/' + TestFontName + ' Plain Font');
            exit(False);
        end;
    end;
    SWidthF := CachedFont.TextWidth(AltText,  WordList[WI]^.Size);
    W := round((SWidthF * 25.4) / gTTFontCache.DPI);
    LocH := round(CachedFont.TextHeight(AltText, WordList[WI]^.Size, DescenderH) / 2.0);
    // writeln('TFormKMemo2pdf.GetWordDimentions LocH=', LocH);
    if LocH > H then H := LocH;
end;

function TFormKMemo2pdf.WriteLine(Page : TPDFPage; XLoc : integer; const Y : integer) : boolean;
var
    W, H : integer; // word dimensions
    Bullet : string;
    FontIndex : integer;

    function ExtraIndent(Bull : TKMemoParaNumbering) : integer;
    begin
        result := 0;
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
    //writeln('TFormKMemo2pdf.WriteLine arrived WordIndex is ', WordIndex, ' Word = ', WordList[WordIndex]^.AWord);
    if WordList[WordIndex]^.ABullet <> pnuNone then  begin // We arrive at the start of a line, is it a bullet line ?
        Bullet := UnicodeToNativeUTF(cRoundBullet) + '  ';
           if not GetWordDimentions(WordIndex, W, H, Bullet) then exit(False);    // font fault unlikely, we have already checked this block.
           BulletIndent := ExtraIndent(WordList[WordIndex]^.ABullet);

           FontIndex :=  FontList.Find(WordList[WordIndex]^.FName, WordList[WordIndex]^.Bold, WordList[WordIndex]^.Italic, True);

           Page.SetFont(FontIndex, WordList[WordIndex]^.Size);
           Page.WriteText(BulletIndent + XLoc, Y, Bullet);                        // u2022
           inc(BulletIndent, W);
    end;
    while WordIndex < WordList.Count do begin
        if WordList[WordIndex]^.NewLine then begin
            inc(WordIndex);
            BulletIndent := 0;
            exit(true);
        end;
        if not GetWordDimentions(WordIndex, W, H) then exit(False);             // font fault unlikely, we have already checked this block.
        //writeln('TFormKMemo2pdf.WriteLine Font is ', WordList[WordIndex]^.FName);

        FontIndex :=  FontList.Find(WordList[WordIndex]^.FName, WordList[WordIndex]^.Bold, WordList[WordIndex]^.Italic, True);
//        writeln('TFormKMemo2pdf.WriteLine Font is ', WordList[WordIndex]^.FName, ' Index=', inttostr(FontIndex));

//        Page.SetFont(AdobeIndex(WordList[WordIndex]^.FName, WordList[WordIndex]^.Bold, WordList[WordIndex]^.Italic), WordList[WordIndex]^.Size);
        Page.SetFont( FontIndex, WordList[WordIndex]^.Size);

        if (XLoc+W+BulletIndent) > (PageWidth - SideMargin) then exit(true);    // no more on this line.
        //writeln('TFormKMemo2pdf.WriteLine will write T=' + WordList[WordIndex]^.AWord + ' F=' + WordList[WordIndex]^.FName+ ' X=' {+ inttostr(BulletIndent)} + ' ' + inttostr(XLoc) + ' W='+ inttostr(W));
//        memo1.Append('TFormKMemo2pdf.WriteLine will WriteText T=' + WordList[WordIndex]^.AWord + ' F=' + WordList[WordIndex]^.FName+ ' X=' {+ inttostr(BulletIndent)} + ' ' + inttostr(XLoc) + ' W='+ inttostr(W));
        Page.WriteText(BulletIndent + XLoc, Y, WordList[WordIndex]^.AWord);
//        memo1.Append('TFormKMemo2pdf.WriteLine wrote word=' + WordList[WordIndex]^.AWord + ' Font=' + WordList[WordIndex]^.FName + ' ' + inttostr(W) + ' ' + inttostr(H));
        XLoc := XLoc+W;
        inc(WordIndex);
    end;
    result := false;    // no more to do.
end;


function TFormKMemo2pdf.CheckHeight() : integer;
var
    W, i, WI : integer;     // word dimensions
    Xloc : integer = SideMargin;
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
        if not GetWordDimentions(WI, W, Result) then begin      // this should never happen, we only use fonts we know are present
                memo1.Append('ERROR - TFormKMemo2pdf.CheckHeight Failed to load the font : ' + WordList[WI]^.FName);
                exit;
        end;
        if (Xloc+W) > (PageWidth - SideMargin) then exit;
        if WI >= WordList.Count then exit;
        XLoc := XLoc+W;
        inc(WI);
    end;
end;

function TFormKMemo2PDF.FontInCache(FName : string; Bold, Italic : boolean) : boolean;
    var CachedFont : TFPFontCacheItem;
begin
    CachedFont := gTTFontCache.Find(FName, Bold, Italic);
    Result := assigned(CachedFont);
end;

function TFormKMemo2pdf.WritePage() : boolean;
var
    X : integer = SideMargin;       // how far we are across the page, left to right, mm
    Y : integer = TopMargin;        // how far we are down the page, top to botton, mm
    Page : TPDFPage;
    {i,} Yt : integer;
begin
    Result := true;
    if WordList.Count < 1 then begin
       showmessage('WritePage called with empty word list');
       exit(false);
    end;
    //writeln('TFormKMemo2pdf.WritePage starting page ===== ' + inttostr(CurrentPage));
    Page := FDoc.Pages[CurrentPage];
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
        //writeln('TFormKMemo2pdf.WritePage writing  ' + WordList[i]^.AWord, X, ' ', Y);    // WARNING, crashes on a newline !!!!!!
    end;
end;

function TFormKMemo2pdf.StartPDF : boolean;     // return false and user is shown the memo with error messages
var
    i : integer;

begin
    Memo1.Clear;
    CurrentPage := -1;
    If WordList <> nil then
        FreeAndNil(WordList);
    if FontList <> Nil then
        FreeAndNil(FontList);
    FontList := TFontList.Create();

    TestFontMono := '';
    TestFontProp := '';

     for I := 0 to high(FontsFixed) do
         if FontList.Add(FontsFixed[i], false, False, True) then begin
            TestFontMono := FontsFixed[i];
            break;
         end;
     for I := 0 to high(FontsVariable) do
         if FontList.Add(FontsVariable[i], false, false, False) then begin
            TestFontProp := FontsVariable[i];
            break;
         end;
    if TestFontMono.IsEmpty or TestFontProp.IsEmpty then begin
        Memo1.Append('ERROR - cannot find suitable fonts to use. Please install');
        Memo1.Append('the Adobe Standard fonts, Courier and Helvetica or the open');
        Memo1.Append('source Liberation Sans and Liberation Mono or FreeType');
        showmessage('Unable to find suitable fonts.');
        exit(false);
    end;
    WordList := TWordList.Create;
    FDoc := TPDFDocument.Create(Nil);
    try
        KMemoRead();
        Result := MakePDF();            // False if we found an issue, probably font related !
    finally
        FDoc.Free;
        FreeAndNil(WordList);
        FreeAndNil(FontList);
    end;
end;


function TFormKMemo2pdf.MakePDF : boolean;
var
  P: TPDFPage;
  S: TPDFSection;
  Opts: TPDFOptions;
begin
    Result := True;
    FDoc.Infos.Title := TheTitle;
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
    if FontList.Count > 0 then
           FontList.LoadFonts(FDoc);
    // FontList.Dump;
    S := FDoc.Sections.AddSection; // we always need at least one section
    repeat
        P := FDoc.Pages.AddPage;
        P.PaperType := ptA4;
        P.UnitOfMeasure := uomMillimeters;
        S.AddPage(P);                       // Add the Page to the Section
        inc(CurrentPage);
    until not WritePage();                  // This is where page content is created.
    SaveDocument();
end;

procedure TFormKMemo2pdf.SaveDocument();
var
    F: TFileStream;
begin
  F := TFileStream.Create(FFileName, fmCreate);
  try try
    FDoc.SaveToStream(F);
  except on E: Exception do begin
        Memo1.Append('ERROR - Failed to save the PDF ' + E.Message);
        show;
        end;
  end;
  finally
    F.Free;
  end;
   Memo1.Append('INFO Wrote file to ' + FFileName);
end;

procedure TFormKMemo2pdf.BitBtnProceedClick(Sender: TObject);        // ToDo : replace all this with call to StartPDF
begin
    AllowNoBoldItalic := True;
    StartPDF();
    exit();
end;


function TFormKMemo2pdf.KMemoRead() : boolean;
var
    BlockNo : integer = 0;
    I : integer;
    ExFont : TFont;
    AWord : ANSIString = '';
{    AFontName : string;
    MyBold, MyItalic : boolean;  }

        procedure CopyFont(FromFont : TFont);
        begin
            ExFont.Bold := FromFont.Bold;
            ExFont.Italic := FromFont.Italic;
            ExFont.Size := FromFont.Size;
            ExFont.Color := FromFont.Color;
            ExFont.Name := FromFont.Name;
            ExFont.Pitch := FromFont.Pitch;     // fpFixed, fpVariable, fpDefault
        end;
begin
    ExFont := TFont.Create();
    for BlockNo := 0 to TheKMemo.Blocks.Count-1 do begin                                    // For every block
        if not TheKMemo.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin
           CopyFont(TKMemoTextBlock(TheKmemo.Blocks.Items[BlockNo]).TextStyle.Font);        // copies to ExFont
           if ExFont.Pitch = fpFixed then begin                                 // Then we add (if necessary) font to FontList
//              MyBold := ExFont.Bold;
//              MyItalic := ExFont.Italic;

              ExFont.Name := TestFontMono;
               if not FontList.Add(TestFontMono, ExFont.Bold, ExFont.Italic, True) then begin
                   showmessage('Font is missing Bold or Italic ' + TestFontMono);
                   exit(false);                                                 // here we assume if regular font exists, so to marked up ??
               end;
           end else begin
                ExFont.Name := TestFontProp;
                if not FontList.Add(TestFontProp, ExFont.Bold, ExFont.Italic, False) then begin
                   showmessage('Font is missing Bold or Italic ' + TestFontProp);
                   exit(false);
                end;
           end;
           for I := 0 to TheKMemo.Blocks.Items[BlockNo].WordCount-1 do begin                // For every word in this block
               AWord := TheKMemo.Blocks.Items[BlockNo].Words[I];
               WordList.Add(AWord, ExFont.Size, ExFont.Bold, ExFont.Italic, False, ExFont.Color, ExFont.Name, (ExFont.Pitch = fpFixed));
           end;
        end else begin
            WordList.Add('', 0, False, False, True, clBlack);   // TKMemoParagraph, thats easy but is it a Bullet ?
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
//    WordList.Dump();
end;


const HaveReadFonts : boolean = false;

procedure TFormKMemo2pdf.FormCreate(Sender: TObject);
begin
    WordList := nil;
//    FontList := nil;
    CurrentPage := -1;
    Memo1.Clear;
    BitBtnProceed.Visible := False;         // ToDo : I really wonder what ?
    if not HaveReadFonts then begin
       {$if defined(CPU32) and defined(LINUX)}
       gTTFontCache.SearchPath.Add('/usr/share/fonts/');  // Avoids a problem noted on 32bit linux where
       gTTFontCache.BuildFontCache;                       // libfontconfig returns a nil pointer to font.cfg file
       {$else}
       gTTFontCache.ReadStandardFonts;
       {$endif}
       HaveReadFonts := True;
    end;
end;

procedure TFormKMemo2pdf.FormShow(Sender: TObject);
begin
    AllowNoBoldItalic := False;
end;

end.

