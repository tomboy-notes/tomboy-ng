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
    -*- This model should work with any TTF fonts, but possibly only ones that have seperate
        font files for each 'style' such as bold, italic.
    -*- I found I had to give each font loaded into doc a different name as well as its
        full file name. But that name is not interesting after that ?  Strange.
    -*- Using the standard formal Adobe fonts (and a local equivilent for sizing) is
        a bit simpler and makes for smaller files but cannot handle UTF8 chars.

    * We cannot handle .otf, OpenType Fonts, the cache loads them but when wrapping up the
      doc, it will crash with an exception worried about unable to load font metrics.
    * We canno handle Chinese Characters
    * We cannot handle .ttc, collections of (.otf?) fonts.
    * I should allow selection of at least Letter size paper.
    * Should I allow user to select the font to use ? Check filename is .ttf ?
    * Note that if we end up using a font without Bold or Italics, the fpPDF can fake it.

    Need lots of testing.



    See https://gitlab.com/freepascal.org/fpc/source/-/issues/30116 for issues about fonts !
    https://forum.lazarus.freepascal.org/index.php/topic,62254.0.html - my question on the topic.

    Fonts that mention bold and italic in the Font selection dialog do not necessarily
    have seperate font files for bold and italic, they may generate them !  But fpPDF does not.



    History :
        2023-02-14 Initial Release of this unit.
        2024-03-12 Use force fonts to Helvetica or Courier, seems to work evn when not present.
        2024-09-26 Unproductive investigation into Chinese fonts, added a couple of Windows proportional.

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

    Pretty useless however, very poor UTF8 character support in particular.
}



{$mode ObjFPC}{$H+}

interface

uses  {$ifdef unix}cwstring,{$endif}
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
    LCLProc, fpttf, kmemo, k_prn, fpimage, fppdf, fpparsettf, typinfo, tb_utils,
    KFunctions;



type                                 // TFontList not used but might be if I revisit, at present, Adobe fonts don't do UTF8
    PFontRecord=^TFontRecord;
    TFontRecord=record
      FamName:string;       // eg 'Ubuntu', 'FreeSans', 'Courier New', 'Liberation Sans'
      Bold : boolean;
      Italic : boolean;
      Fixed : Boolean;      // Fixed spacing or not
      FontIndex : integer;  // As assigned by PDF Doc, AddFont(), -1 if not set yet
      FileName : string;    // full path to this specific font.  Get from cache
      Available : boolean;  // Often, the Bold or Italics version of a font is NOT !
    end;

 type
    { TFontList }

    TFontList = class(TFPList)
        private
            function Get(Index : Integer) : PFontRecord;
            function FindInFontCache(FamName : string; Bold, Italics : boolean) : boolean;
                            { While we try to load every font that we added to the local font list some may
                            have duplicate file names. These are ones we cannot find files for and will be
                            simulated (based on the 'regular' version, file name provided) when we hit WriteLine() }
            function LoadFonts(FDoc : TPDFDocument) : boolean;
        public
            constructor Create();
            destructor Destroy; Override;

                            { Passed a font family name and it returns false if its unusable. If
                            true, its added  along with extra data to fontlist. But if its a Bold
                            or Italics font is requested and that is not available, we will try for a
                            'regular' version and add that if possible. If that works, we also add
                            the requested font (bold, italics) but mark Available as false and supply
                            the regular font filename. This method Checks against
                            the font cache, does not, add the font to the document.  }
            function Add(TheFont : ANSIString; Bold, Italics : boolean; Fixed : Boolean) : boolean;
            procedure Dump();
                            // Returns FontIndex of passed font record, -1 is not present;
                            // If FIndex = false, then returns internal index, ie -1 if not present.
            function Find(TheFont: ANSIString; Bold, Italic, FIndex: boolean): integer;
            function IsAvailable(TheFont: ANSIString; const Bold, Italic : boolean) : boolean;
            property Items[Index : integer] : PFontRecord read Get; default;
    end;

type
    { TFormKMemo2pdf }
    TFormKMemo2pdf = class(TForm)
        BitBtn1: TBitBtn;
        BitBtnHelp : TBitBtn;
        BitBtnProceed: TBitBtn;
        ComboMono : TComboBox;
        ComboProp : TComboBox;
        GroupBox1 : TGroupBox;
        GroupBox2 : TGroupBox;
        GroupBox3 : TGroupBox;
        Label1 : TLabel;
        Label2 : TLabel;
        Label3 : TLabel;
        LabelPropNotFound : TLabel;
        LabelMonoNotFound : TLabel;
        Memo1: TMemo;
        RadioA4 : TRadioButton;
        RadioSimYes : TRadioButton;
        RadioSimNo : TRadioButton;
        RadioLetter : TRadioButton;
        RadioDefault : TRadioButton;
        RadioUserDefined : TRadioButton;
        procedure BitBtnHelpClick(Sender : TObject);
        procedure BitBtnProceedClick(Sender: TObject);
        procedure ComboPropEditingDone(Sender : TObject);
        procedure FormActivate(Sender : TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure RadioDefaultChange(Sender : TObject);
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
        procedure RefreshForm;
        procedure SaveDocument();
        procedure TestWordList();
                                    // Tries to print out a line of words, word by word, at indicated starting point.
                                    // returns false if there is no more words to print.
        function WriteLine(Page: TPDFPage; XLoc: integer; const Y: integer): boolean;
                                    // sends a page of text to the Document, returning True when its all done.
        function WritePage(): boolean;
    public
        ParentLeft, ParentTop : integer;
        TheKMemo : TKMemo;          // This is the KMemo, set it before showing the form.
        TheTitle : string;
        FFileName : string;         // Full filename including path,
        DefaultFont : string;        // New Text in a KMemo is saved with font='default', only after reload does it have true name
                            // Public function to initiate the PDF. If it returns False then we
                            // have failed to find necessary fonts. It tries fonts listed in arrays
                            // at top of file or, if user chose to provide font name, that font.
                            // Will accept a "Human Friendly Name" or the simple font filename.
                            // Will show user some error or advice messages.
                            // Note : here we test and add ONLY the regular font, not bold/italic yet
        function StartPDF: boolean;

    end;



var
    FormKMemo2pdf: TFormKMemo2pdf;

implementation

uses LCLIntf;

{$R *.lfm}

{$define USESIMFONTS}     // fppdf can 'generate' simulated bold and italics
{x$define DEBUG}

{ TFormKMemo2pdf }

const   TopMargin    =  15;
        BottomMargin =  10;
        SideMargin   =  15;
        PageHeight   = 297;                  // Hmm, we assume A4 page here don't we ?
        PageWidth    = 210;
        LineHeight   =   6;

        // These two arrays contain fonts that I know are suitable for this use. We use
        // the first one found on the given system. Add more here but read notes above.
        FontsFixed : array of string = ('Liberation Mono', 'Courier New', 'Courier');  // Linux, Windows/MacOS, Adobe
        FontsVariable : array of string = ( 'Liberation Sans', 'Lucida Grande',        // Linux, MacOS
                    'Bahnschrift', 'Lucinda Sans Unicode', 'Arial', 'Helvetica');      // Windows, Windows, Adobe, Adobe
//      Note: Names of other, potentially interesting fonts are specified in the
//      ObjectInspector->ComboProp, ComboMono->Items.
//        FontsFixed : array of string = ('Monaco', 'Menlo','Courier New');        // Darwin
//        FontsVariable : array of string = ('Lucida Grande', 'Geneva', 'Arial');  // Darwin
//        'Simsun-ExtB' is a SC Windows font that does show some Latin char but no Chinese one !

{ ------------------------------ TFontList ------------------------------------}

                // A utility that searches the FontCache for a passed filename, returning
                // the Font Name if found and empty string otherwise.
function GetCachedFontFromFile(FName : string) : string;
var
    i : integer = 0;
begin
    Result := '';
    if ((pos('.ttf', FName) = 0) and (pos('.otf', FName) = 0)) then exit();     // cannot be a font name
    while I < gTTFontCache.Count do begin
        if ExtractFileName(gTTFontCache.Items[i].FileName) = FName then
            exit(gTTFontCache.Items[i].HumanFriendlyName)
        else inc(i);
    end;
end;

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
  //      writeln('TFontList.LoadFonts 3 ', Items[i]^.FileName, ' B=', booltostr(Items[i]^.Bold, true), ' A=', booltostr(Items[i]^.Available, True));
        Items[i]^.FontIndex := FDoc.AddFont(Items[i]^.FileName, FakeName);
        {$ifdef DEBUG}
        if Items[i]^.FontIndex < 0 then
            debugln('TFontList.LoadFonts ERROR, failed to add ' + FakeName + ' A=', booltostr(Items[i]^.Available, True))
        else debugln('TFontList.LoadFonts font ' + inttostr(Items[i]^.FontIndex) + ' ' + FakeName + ' A=', booltostr(Items[i]^.Available, True));
        {$endif}
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


function TFontList.Add(TheFont: ANSIString; Bold, Italics: boolean; Fixed : Boolean) : boolean;
var
    P : PFontRecord;
    CachedFont : TFPFontCacheItem;

    procedure InsertIntoList(const Avail, ForceRegular : boolean);
    begin
        new(P);
        P^.FamName := TheFont;
        if ForceRegular then begin
            P^.Bold := False;
            P^.Italic := False;
        end else begin
            P^.Bold := Bold;
            P^.Italic := Italics;
        end;
        P^.FontIndex := -1;         // The fpPDF index, fill this in later, when assigning the font to PDF Doc
        P^.Fixed := Fixed;
//        if Avail then
            P^.FileName := CachedFont.FileName;     // This will be duplicated filename for Regular and Markup font
//        else  P^.FileName := '';
        P^.Available := Avail;     // Dont need that, could test for a missing filename ?
        inherited Add(P);
        // writeln( 'InsertIntoList count=', count);
    end;

begin
    Result := False;
    if Find(TheFont, Bold, Italics, False) > -1 then exit(True);                // ToDo : is that right ????
    CachedFont := gTTFontCache.Find(TheFont, Bold, Italics);
    if Assigned(CachedFont) and ((pos('.ttf', CachedFont.FileName) > 0)
                or (pos('.otf', CachedFont.FileName) > 0)) then                 // font filename must end with .ttf or .otf
        InsertIntoList(True, False)                                             // Available and as requested,
    else begin
       CachedFont := gTTFontCache.Find(TheFont, False, False);                  // Try for a regular version then
       if Assigned(CachedFont) and ((pos('.ttf', CachedFont.FileName) > 0)
                   or (pos('.otf', CachedFont.FileName) > 0)) then begin
           InsertIntoList(True, True);                                          // Available but a regular version
           {$ifdef USESIMFONTS}
           InsertIntoList(False, False);                                        // Not Available but what was requested.
           {$endif}
       end else exit(False);                                                    // cannot find font
    end;
    Result := True;
end;

procedure TFontList.Dump();         // WARNING, uses writeln
var i : integer;
begin
  writeln('TFontList.Dump we have ' + count.tostring + ' items ========');
  for i := 0 to count -1 do
    writeln('TFontList.Dump : ', Items[i]^.FamName, ' B=', booltostr(Items[i]^.Bold, True)
        , ' I=', booltostr(Items[i]^.Italic, True), ' Idx=', inttostr(Items[i]^.FontIndex)
        , ' FileName=', Items[i]^.FileName, ' Avail=', booltostr(Items[i]^.Available, true));
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

function TFontList.IsAvailable(TheFont : ANSIString; const Bold, Italic : boolean) : boolean;
var P : PFontRecord;
begin
    Result := False;
    for P in self do
      if (P^.FamName = TheFont)
            and (P^.Bold = Bold)
            and (P^.Italic = Italic) then begin
                result := P^.Available;
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
    // Note, this might pick up a cached font in unusable file, ie .ttc - thats OK, is a good guess.
    CachedFont := gTTFontCache.Find(WordList[WI]^.FName, WordList[WI]^.Bold, WordList[WI]^.Italic);
    if not Assigned(CachedFont) then  begin                                     // might happen if italics or bold requested but not available
        //writeln('INFO - TFormKMemo2pdf.GetWordDimentions Cannot find Font in Cache : ', WordList[WI]^.FName, ' ', WordList[WI]^.Bold, ' ', WordList[WI]^.Italic);
        // We will try a plain version of same font and if that works, change entry in WordList
        CachedFont := gTTFontCache.Find(TestFontName, False, False);
        {$ifndef USESIMFONTS}if Assigned(CachedFont) then begin
            WordList[WI]^.Bold := False;                                        // ToDo : this is very, very ugly must understand fonts better
            WordList[WI]^.Italic := False;
        end else begin    {$else}
        if not Assigned(CachedFont) then begin {$endif USESIMFONTS}
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
    FontIndex : integer = -1;
    SimBold : boolean = False;
    SimItalics : boolean = False;

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
        SimBold := False;
        SimItalics := False;
        if not GetWordDimentions(WordIndex, W, H) then exit(False);             // font fault unlikely, we have already checked this block.
        if FontList.IsAvailable(WordList[WordIndex]^.FName, WordList[WordIndex]^.Bold, WordList[WordIndex]^.Italic) then
            FontIndex :=  FontList.Find(WordList[WordIndex]^.FName, WordList[WordIndex]^.Bold, WordList[WordIndex]^.Italic, True)
        else begin
            if FontList.IsAvailable(WordList[WordIndex]^.FName, False, False) then begin
                FontIndex :=  FontList.Find(WordList[WordIndex]^.FName, False, False, True);     // Thats already been checked
                if RadioSimYes.Checked then begin
                    // what did we fail on ?
                    if WordList[WordIndex]^.Bold then SimBold := True;          // We try to simulate the missing fonts, causes crash with some fonts
                    if WordList[WordIndex]^.Italic then SimItalics := True;
                end else begin                                                  // We turn Markup off for this word because one failed
                    WordList[WordIndex]^.Bold := False;
                    WordList[WordIndex]^.Italic := False;
                end;
            end;
        end;
        if FontIndex = -1 then begin
            debugln('TFormKMemo2pdf.WriteLine ERROR cannot find font ' + WordList[WordIndex]^.FName);
            exit(False);
        end;
        {$ifdef DEBUG}
        if (wordIndex < 25) or (FontIndex <> 0) then    // first 25 lines OR any line not using 'std' font.
            debugln('TFormKMemo2pdf.WriteLine Font is ' + inttostr(FontIndex) + ' ' + WordList[WordIndex]^.FName
            + ' ', booltostr(WordList[WordIndex]^.Bold, True) + ' ' + booltostr(WordList[WordIndex]^.Italic, True)
            + ' [' + WordList[WordIndex]^.AWord + '] ' + booltostr(SimBold, True) + ' ' + booltostr(SimItalics, True));
        {$endif}
        Page.SetFont( FontIndex, WordList[WordIndex]^.Size, SimBold, SimItalics);
        if (XLoc+W+BulletIndent) > (PageWidth - SideMargin) then exit(true);    // no more on this line.
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

function TFormKMemo2PDF.FontInCache(FName : string; Bold, Italic : boolean) : boolean;  // not used !
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

// TestWordList();

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

procedure TFormKMemo2pdf.RefreshForm;
var TempName : string;
begin
    ComboMono.Text := trim(ComboMono.Text);
    ComboProp.Enabled := RadioUserDefined.Checked;
    ComboMono.Enabled := RadioUserDefined.Checked;
    ComboProp.Text := trim(ComboProp.Text);
    if ((pos('.ttf', lowercase(ComboProp.Text)) > 0) or (pos('.otf', lowercase(ComboProp.Text)) > 0)) then begin
        TempName := GetCachedFontFromFile(ComboProp.Text);
        if TempName <> '' then ComboProp.Text := TempName;                      // if it failed, we will pick up in StartPDF
    end;
    if ((pos('.ttf', lowercase(ComboMono.Text)) > 0) or (pos('.otf', lowercase(ComboMono.Text)) > 0)) then begin
        TempName := GetCachedFontFromFile(ComboMono.Text);
        if TempName <> '' then ComboMono.Text := TempName;
    end;
    StartPDF();
end;

function TFormKMemo2pdf.StartPDF : boolean;     // return false and user is shown the memo with error messages

    // in default font mode check for both fonts from the arrays. If User mode we check
    // the user provided font name unless its blank in which case, use appropriate array.
    // Checks for the basic font, we don't know, yet, if we have or need bold/italic
    procedure SetTheFont(IsProp : boolean);
    var i : integer;
    begin
        if IsProp then begin                                               // Prop or variable spaced fonts
            if RadioDefault.Checked or (ComboProp.Text = '') then begin   // use ones from array
                for I := 0 to high(FontsVariable) do
                    if FontList.Add(FontsVariable[i], false, False, False) then begin
                        TestFontProp := FontsVariable[i];
                        ComboProp.Text := FontsVariable[i];
                        break;
                    end;
            end else                                                            // Setting Prop font from user set ComboBox
                if FontList.Add(trim(ComboProp.Text), false, false, False) then
                    TestFontProp := trim(ComboProp.Text)
                else if FontList.Add(trim(ComboProp.Text), false, false, False) then   // See if Combo holds a file name instead
                    TestFontProp := GetCachedFontFromFile(trim(ComboProp.Text));
        end else begin                                                          // doing monspace
            if RadioDefault.Checked or (ComboMono.Text = '') then begin         // using arrays
                for I := 0 to high(FontsFixed) do
                    if FontList.Add(FontsFixed[i], false, False, True) then begin
                        TestFontMono := FontsFixed[i];
                        ComboMono.Text := FontsFixed[i];
                        break;
                    end;
            end else                                                            // Using user request
                if FontList.Add(ComboMono.Text, false, False, True) then
                    TestFontMono := ComboMono.Text
                else if FontList.Add(trim(ComboMono.Text), false, false, False) then   // See if Combo holds a file name instead
                    TestFontMono := GetCachedFontFromFile(trim(ComboMono.Text));
        end;
    end;

begin
    Result := False;

    BitBtnProceed.Enabled := False;
    CurrentPage := -1;
    If WordList <> nil then
        FreeAndNil(WordList);
    if FontList <> Nil then
        FreeAndNil(FontList);
    FontList := TFontList.Create();
    TestFontMono := '';
    TestFontProp := '';
    SetTheFont(True);            // Prop
    SetTheFont(False);           // Mono
    LabelMonoNotFound.Visible := TestFontMono = '';
    LabelPropNotFound.Visible := TestFontProp = '';
    if TestFontMono.IsEmpty or TestFontProp.IsEmpty then begin
        exit(false);
    end;
    BitBtnProceed.Enabled := True;
    Result := True;
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
//    Include(Opts, poSubsetFont);     // do not assert poSubsetFont, messes with Chinese fonts
    Include(Opts, poCompressFonts);
    Include(Opts,poCompressText);
    FDoc.Options := Opts;              // poNoEmbeddedFonts not asserted, embedded by default
    FDoc.StartDocument;
    {$ifdef DEBUG}FontList.Dump;{$endif}
    if FontList.Count > 0 then
           FontList.LoadFonts(FDoc);
    S := FDoc.Sections.AddSection; // we always need at least one section
    repeat
        P := FDoc.Pages.AddPage;
        if RadioA4.Checked then
            P.PaperType := ptA4
        else P.PaperType := ptLetter;
        P.UnitOfMeasure := uomMillimeters;
        S.AddPage(P);                       // Add the Page to the Section
        inc(CurrentPage);
    until not WritePage();                  // This is where page content is created.
    SaveDocument();
end;

procedure TFormKMemo2pdf.TestWordList();        // Just a debug method
var i : integer;
begin
    for i := 0 to WordList.Count -1 do
        if WordList.Items[i]^.Bold then writeln('TestWordList Bold [' + WordList.Items[i]^.AWord + ']');
end;

procedure TFormKMemo2pdf.SaveDocument();
var
    F: TFileStream;
begin
  F := TFileStream.Create(FFileName, fmCreate);
  try try
    FDoc.SaveToStream(F);
  except on E: Exception do
        ShowMessage('ERROR - Failed to save the PDF (font issue ?)' + E.Message);
  end;
  finally
    F.Free;
  end;
end;

procedure TFormKMemo2pdf.BitBtnProceedClick(Sender: TObject);
begin
    if not StartPDF() then exit;        // It is possible to jump here without triggering a StartPDF()
    WordList := TWordList.Create;       // regional
    FDoc := TPDFDocument.Create(Nil);   // regional
    try
        KMemoRead();
        if not MakePDF() then            // False if we found an issue, probably font related !
            ShowMessage('An error occured making the PDF');

    finally
        FDoc.Free;
        FreeAndNil(WordList);
        FreeAndNil(FontList);
    end;
end;

procedure TFormKMemo2pdf.BitBtnHelpClick(Sender : TObject);
begin
    OpenURL('https://github.com/tomboy-notes/tomboy-ng/wiki/PDF-Fonts');
end;

procedure TFormKMemo2pdf.ComboPropEditingDone(Sender : TObject);
begin
   RefreshForm();
end;

procedure TFormKMemo2pdf.FormActivate(Sender : TObject);
begin
    Left := ParentLeft div 2;   // This is apparently necessary in Qt with Modal forms ??
    Top := ParentTop div 2;
    AllowNoBoldItalic := True;
    RefreshForm();              // will call startPDF, test fonts
    Memo1.Clear;
    Memo1.Append('If characters in your PDF do not appear as expected you');
    Memo1.Append('may need to set a language specific font. tomboy-ng''s ');
    Memo1.Append('libraries cannot work with all fonts, you must supply a');
    Memo1.Append('font that is in a .ttf or .otf file. A .ttc font file will');
    Memo1.Append('not work. Press the Help Button.');
    Memo1.Append('');
end;

function TFormKMemo2pdf.KMemoRead() : boolean;
var
    BlockNo : integer = 0;
    I : integer;
    ExFont : TFont;
    AWord : ANSIString = '';

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
        if not TheKMemo.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin     // A text block of some sort
            CopyFont(TKMemoTextBlock(TheKmemo.Blocks.Items[BlockNo]).TextStyle.Font);       // copies to ExFont from KMemo
            if ExFont.Pitch = fpFixed then begin                                // Then we add (if necessary) font to FontList
                ExFont.Name := TestFontMono;
                // if FontList fails to find the exact combo,
                if not FontList.Add(TestFontMono, ExFont.Bold, ExFont.Italic, True) then begin    // try to add it to FontList
 //                   FailedBoldItalics := True;
                    DebugLn(TestFontMono + ' is missing Bold or Italic ');      // we keep 'printing' but without the markup
                end;
            end else begin
                ExFont.Name := TestFontProp;                                    // must be Prop font
                if not FontList.Add(TestFontProp, ExFont.Bold, ExFont.Italic, False) then begin
 //                   FailedBoldItalics := True;
                    DebugLn(TestFontProp + ' is missing Bold or Italic exfont.bold is', booltostr(ExFont.Bold, True));

                end;
            end;
            for I := 0 to TheKMemo.Blocks.Items[BlockNo].WordCount-1 do begin   // For every word in this block, all same font
                AWord := TheKMemo.Blocks.Items[BlockNo].Words[I];
                // We add every requested font to WordList, even if it is not available (and not in FontList)
                WordList.Add(AWord, ExFont.Size, ExFont.Bold, ExFont.Italic, False, ExFont.Color, ExFont.Name, (ExFont.Pitch = fpFixed));
            end;
            //if ExFont.Bold then debugln('TFormKMemo2pdf.KMemoRead ExFont True');
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

(*    if FailedBoldItalics then begin             // only happens if content requires one of them and it is unavailable
        Memo1.Append('Note : Font is missing Bold or Italic ');
        Memo1.Append('We will generate simulated fonts.');
        // Memo1.Append('Delaying a few seconds while you see this message ...');
        Application.ProcessMessages;
        sleep(4000);
    end;              *)
//    WordList.Dump();
end;

const HaveReadFonts : boolean = false;
        // Using this model, if a run finds the necessary fonts but they become
        // unavailable for a later run (but tomboy-ng is not restarted) then an
        // unhandled exception occurs.  Hmm ......

procedure TFormKMemo2pdf.FormCreate(Sender: TObject);
{$ifdef TestSize}var
    CachedFont : TFPFontCacheItem;
    DescenderH : single; {$endif}
begin
    WordList := nil;
//    FontList := nil;
    CurrentPage := -1;
    BitBtnProceed.Enabled := False;
    if not HaveReadFonts then begin
       {$if defined(CPU32) and defined(LINUX)}
       gTTFontCache.SearchPath.Add('/usr/share/fonts/');  // Avoids a problem noted on 32bit linux where
       gTTFontCache.BuildFontCache;                       // libfontconfig returns a nil pointer to font.cfg file
       {$else}
//       gTTFontCache.SearchPath.Add('./');               // Also look for fonts where binary lives ?
       gTTFontCache.ReadStandardFonts;
       {$endif}
       HaveReadFonts := True;
    end;
    {$ifdef TestSize}
    CachedFont := gTTFontCache.Find('Noto Sans CJK SC', False, False);  // Bold and Italic are booleans
    if Assigned(CachedFont) then begin
        writeln('Width of ABC in a PDF at 12pt is ', CachedFont.TextWidth('ABC',  24));
        writeln('Height of above is ', CachedFont.TextHeight('ABC', 24, DescenderH))
    end else writeln('Sorry, don''t seem to have that font.'); {$endif}
end;

procedure TFormKMemo2pdf.FormShow(Sender: TObject);
begin

end;

procedure TFormKMemo2pdf.RadioDefaultChange(Sender : TObject);
begin
    RefreshForm();
end;

end.

