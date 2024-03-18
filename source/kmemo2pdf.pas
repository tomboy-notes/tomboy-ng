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

    NOTE: - ABANDONED - above includes rtl fpttf but I have copied it to source dir, added it to this
       project and so guzumpted the one in FTP. In there, around line 396, I have commented
       out the code that loads a otf font, now gTTFontCache reads only TTF.
       Must look into what this is all about !

    New approach to fonts. I now, in ReadKMemo, force all fonts to be Helvetica or
    Courier, or thier marked up variations. These are some of the 14 internal Adobe
    fonts. That is, ones that don't have to be embedded. However, the FontCache quite rightly
    notes I don't have either installed and replaces them Liberation Sans and Liberation Mono.

    However, no sign of the "empty PDF" problem I experience when using .otf fonts. Maybe
    th Cache only offers TTF as substitutes ???

    Need lots of testing.



    See https://gitlab.com/freepascal.org/fpc/source/-/issues/30116 for issues about fonts !
    https://forum.lazarus.freepascal.org/index.php/topic,62254.0.html - my question on the topic.

    Fonts that mention bold and italic in the Font selection dialog do not necessarily
    have seperate font files for bold and italic, they may generate them !  But fpPDF does not.

    ======================= March 2024 ======================
    New Model. We only poke the Adobe standard fonts into the PDF, only Courier
    for mono, and Helvetica for a proportional sans font (Could do Times New Roman too).
    But on Linux (and FreeBSD etc ?) where there fonts may not be available in the
    font cache, I look up the numbers in the equivilent, Liberation Mono and
    Liberation Sans.

    PROBLEM - These basic Adobe fonts are not UTF8 capable.

  I build wordlist using the Adobe font names, not the ones KMemo uses.

  Further, I cannot call ~.ReadAllFonts on (at least) 32bit debian machines, so
  instead, some $ifdef that will add some font paths and call BuildCache without
  ReadAllFonts problematic calls to libfontconfig asking about the fong.conf file.
  (libfontconfig retuns a null pointer to a filename, code does not check and
  the result is a fall over).

  Still unsure how I manage the Mac equivilent Adobe fonts, maybe I will find
  them present ?  Fingers crossed.


    History :
        2023-02-14 Initial Release of this unit.
        2024-03-12 Use force fonts to Helvetica or Courier, seems to work evn when not present.


    The standard 14 PDF fonts – that can be referenced by name in a PDF document – are:

        Times-Roman
        Times-Bold
        Time-Italic
        Time-BoldItalic
        Courier
        Courier
        Courier-Bold
        Courier-Oblique
        Helvetica
        Helvetica-Bold
        Helvetica-Oblique
        Helvetica-BoldOblique
        Symbol
        ZapfDingbats

    I suspect my pdf font issues will go away if I use ONLY these fonts.
}



{$mode ObjFPC}{$H+}

interface

uses  {$ifdef unix}cwstring,{$endif}
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, fpttf,
    kmemo, k_prn, fpimage, fppdf, fpparsettf,  typinfo, tb_utils, KFunctions;



(* type                                 // TFontList not used but might be if I revisit, at present, Adobe fonts don't do UTF8
    PFontRecord=^TFontRecord;
    TFontRecord=record
      FamName:string;       // eg 'Ubuntu', 'FreeSans'
      OldName : string;     // the name of the font this record replaces.
      Bold : boolean;
      Italic : boolean;
      Pitch : TFontPitch;   // eg fpFixed or not.
      FontIndex : integer;  // -1 if not set yet
    end;

 type
    { TFontList }

    TFontList = class(TFPList)
        private

            function FindOld(TheFont: ANSIString; Bold, Italic : boolean; out NewFont: string): boolean;
            //DefaultFontName : string;
            function Get(Index : Integer) : PFontRecord;
            function FindInFontCache(FamName : string; Bold, Italics : boolean) : boolean;
            function GetSuitableFont(const Bold, Italic, Fixed: boolean): string;
        public
            constructor Create();
            destructor Destroy; Override;
                            { Might add the passed font family name to the Fontlist as either OldName or FamName.
                              If its already in FontList, as FamName, or is not yet there but will be now because
                              it can be found by gTTFFontCache, returns False.  If its already there as OldName
                              or not there but a substitute can be found in gttFFoneCache and added to list, it
                              returns True and with the new FamName in NewName (and calling process should use it
                              in WordList). Hmm, what to do if we cannot find a substitute ?    }
            function Add(const TheFont: ANSIString; const Bold, Italic: boolean;
                Pitch: TFontPitch; var NewName: string): boolean;
            procedure Dump();
                            // Returns FontIndex of passed font record, -1 is not present;
            function Find(TheFont: ANSIString; Bold, Italic, FIndex: boolean): integer;
            property Items[Index : integer] : PFontRecord read Get; default;
    end;    *)

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
        UseLocalFontInfo : boolean; // Says Adobe font info available in cache, no mapping required.
        AllowNoBoldItalic : boolean;// Allow use of font that does not have its own Bold or Italic font file.
        BulletIndent : integer;     // Bullet Indent (not including bullet), zero means no bullet;
//        FontList : TFontList;       // A list of all the fonts we will need to print/display the PDF, Initially populated in KMemoRead()
        WordList : TWordList;       // A list of all the words and their details such as font, size etc. Populated in KMemoRead().
        WordIndex : integer;        // Points to a word in wordlist, must be less than WordList.count
        //FPage: integer;
        FDoc: TPDFDocument;
        CurrentPage : integer ;     // Starts at zero ?
                                    // does a non write scan of next block of words returning the necessary Height
                                    // to push Y down to to provide clearance for any larger text. Do We have to push down
                                    // by less if first char of the line is higher ? test run is very strange !
                                    // Return 0 for a blank line (just inc Y by line height, don't run WriteLine)
                                    // This function is not being used !
//        function ChangedFont(): boolean;
                                    // Returns the index into Font array AND Doc font list of the indicated font.
                                    // Use only family name, ie Liberation Sans, Courier etc.
        function AdobeIndex(FName: string; Bold, Italic: boolean): integer;
                                    // Checks height of line to be rendered, so that base line can accomodate.
        function CheckHeight: integer;
                                    // Returns true if we have named font in our cache.
        function FontInCache(FName: string): boolean;
                                    // Returns with width and height in mm of the word in wordlist pointed to by WI
                                    // If AltText contains anything, it will be used instead of the text in WordList
                                    // but font and associated settings from WordList will still be used.
        function GetWordDimentions(const WI: integer; out W: integer; var H: integer; AltText: string = ''): boolean;
                                    // Builds a WordList based on the KMemo. Each word will have a trailing space if applicable
                                    // A newline has an empty list item and NewLine true. Tries to register a suitable
                                    // font for each word but it may get told by FontList.Add() to use another font.
        function KMemoRead(): boolean;
                                    { Checks all fonts in FontList for necessary bold and Italic varients and allocates
                                    a FontIndex that will be used during PDF writing. Returns False if no suitable
                                    font files exist (hopefully, issue was dealt with before we got here). }
        function LoadFonts(): boolean;
                                    // Generates the PDF, might return False if it finds the available fonts
                                    // do not include a needed bold or italic version, user invited to proceed
                                    // anyway (without font tricks). However, thats now unlikely as we will
                                    // have already changed to a "know good" font. Hopefully.
        function MakePDF: boolean;
        procedure SaveDocument();
                                    // Returns true if all the fonts in FontList can be found.
//        function TestForGoodFonts: string;
                                    // Returns true if a font with passed name can be found, regular, bold, italics, bold-italics
//        function TestForGoodFonts(FName: string): boolean;
//        procedure UpdateWordList(BadFName, GoodFName: string);

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
        AdobeFonts : array of string = ('Courier', 'Courier-Bold', 'Courier-Oblique'
                , 'Courier-BoldOblique', 'Helvetica', 'Helvetica-Bold'
                , 'Helvetica-Oblique', 'Helvetica-BoldOblique');
                //, 'Times-Roman'
                //, 'Times-Bold', 'Times-Italic', 'Times-BoldItalic', 'Symbol', 'ZapfDingbats');

(*        {$ifdef LINUX}
        FontsFixed : array of string = ('Liberation Mono', 'Ubuntu Mono');
        FontsVariable : array of string = ('Liberation Sans', 'Ubuntu Mono', 'Noto Sans', 'Liberation Serif');
        {$endif}
        {$ifdef WINDOWS}
        FontsFixed : array of string = ('Courier New');
        FontsVariable : array of string = ('Arial', 'Calibri', 'Segoe UI');
        {$endif}
        {$ifdef DARWIN}
        FontsFixed : array of string = ('Monaco', 'Menlo','Courier New');
        FontsVariable : array of string = ('Lucida Grande', 'Geneva', 'Arial');
        {$endif}   *)

{ TFontList }

(*
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



// Will return the name a (hopefully) suitable font to use.
function TFontList.GetSuitableFont(const Bold, Italic, Fixed : boolean) : string;
var
    i : integer;
begin
    if Fixed then begin
        for I := 0 to high(FontsFixed) do
            if FindInFontCache(FontsFixed[i], Bold, Italic) then
                exit(FontsFixed[i]);
    end else
        for I := 0 to high(FontsVariable) do
            if FindInFontCache(FontsVariable[i], Bold, Italic) then
                exit(FontsVariable[i]);
    result := '';
end;

function TFontList.Add(const TheFont: ANSIString; const Bold, Italic: boolean;                         // ToDo : remove ?
                                        Pitch : TFontPitch; var NewName : string) : boolean;
var
    P : PFontRecord;
    SubFont : string;

    procedure InsertIntoList(SubName : string = '');
    begin
        new(P);
        if SubName <> '' then begin
            P^.FamName := SubName;
            P^.OldName := TheFont;
        end else begin
            P^.FamName := TheFont;
            P^.OldName := '';
        end;
        P^.Bold := Bold;
        P^.Italic := Italic;
        P^.FontIndex := -1;         // Wots this for ???
        P^.Pitch := Pitch;
        inherited Add(P);
    end;

begin
    Result := False;
    if Find(TheFont, Bold, Italic, False) > -1 then
           Exit(False);    // All good, its already here.
    if FindOld(TheFont, Bold, Italic, NewName) then
           exit(True);    // We already have a substitute
    if FindInFontCache(TheFont, Bold, Italic) then begin
        InsertIntoList();
        NewName := TheFont;
//        writeln('TFontList.Add has added ', NewName, ' replacing ', TheFont);
        exit(False);
    end;

   SubFont := GetSuitableFont(Bold, Italic, (Pitch = fpFixed));
   if SubFont <> '' then begin
        InsertIntoList(SubFont);
        NewName := SubFont;
//        writeln('TFontList.Add has substituted ', SubFont, ' for ', TheFont);
        exit(True);
   end;
   showmessage('ERROR - Cannot find a sunbtitute font for ' + TheFont + Bold.ToString(True) + Italic.ToString(True));
end;

procedure TFontList.Dump();         // WARNING, uses writeln
var i : integer;
begin
  writeln('TFontList.Dump we have ' + count.tostring + ' items ========');
  for i := 0 to count -1 do
    writeln('FONT : ', Items[i]^.FamName, ' B=', booltostr(Items[i]^.Bold, True)
        , ' I=', booltostr(Items[i]^.Italic, True), ' replaces=',  Items[i]^.OldName);
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

function TFontList.FindOld(TheFont: ANSIString; Bold, Italic : boolean; out NewFont : string): boolean;
var i : integer;
begin
    for i := 0 to count -1 do
        if (Items[i]^.OldName = TheFont) and (Items[i]^.Bold = Bold)
                    and (Items[i]^.Italic = Italic) then begin
           NewFont := Items[i]^.FamName;
           exit(True);
    end;
    Result := False;
end;                               *)

function TFormKMemo2PDF.FontInCache(FName : string) : boolean;
var CachedFont : TFPFontCacheItem;
begin
    CachedFont := gTTFontCache.Find(FName, false, false);
    Result := assigned(CachedFont);
end;

function TFormKMemo2PDF.GetWordDimentions(const WI : integer; out W : integer; var H : integer; AltText : string = '') : boolean;
var
    CachedFont : TFPFontCacheItem;
    SWidthF, DescenderH : single;
    LocH : integer;
    TestFontName : string;

begin
    if UseLocalFontInfo then
        TestFontName := WordList[WI]^.FName
    else begin
        if WordList[WI]^.FName = 'Helvetica' then   // Assumes we ONLY have Helvetica and Courier in Word List!
            TestFontName := 'Liberation Sans'
        else TestFontName := 'Liberation Mono';
    end;
    Result := true;
    //writeln('INFO - TFormKMemo2pdf.GetWordDimentions WI=' + inttostr(WI) + ' and Wordlist.Count=' + inttostr(WordList.count));
    if AltText = '' then
           AltText := WordList[WI]^.AWord;
    //writeln('INFO - TFormKMemo2pdf.GetWordDimentions looking at ' + WordList[WI]^.AWord + ' Font=' + WordList[WI]^.FName);
    CachedFont := gTTFontCache.Find(TestFontName, WordList[WI]^.Bold, WordList[WI]^.Italic);

    if not Assigned(CachedFont) then  begin
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
           Page.SetFont(AdobeIndex(WordList[WordIndex]^.FName, WordList[WordIndex]^.Bold, WordList[WordIndex]^.Italic), WordList[WordIndex]^.Size);
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
        Page.SetFont(AdobeIndex(WordList[WordIndex]^.FName, WordList[WordIndex]^.Bold, WordList[WordIndex]^.Italic), WordList[WordIndex]^.Size);
        if (XLoc+W+BulletIndent) > (PageWidth - SideMargin) then exit(true);    // no more on this line.
        //writeln('TFormKMemo2pdf.WriteLine will write T=' + WordList[WordIndex]^.AWord + ' F=' + WordList[WordIndex]^.FName+ ' X=' {+ inttostr(BulletIndent)} + ' ' + inttostr(XLoc) + ' W='+ inttostr(W));
        memo1.Append('TFormKMemo2pdf.WriteLine will WriteText T=' + WordList[WordIndex]^.AWord + ' F=' + WordList[WordIndex]^.FName+ ' X=' {+ inttostr(BulletIndent)} + ' ' + inttostr(XLoc) + ' W='+ inttostr(W));
        Page.WriteText(BulletIndent + XLoc, Y, WordList[WordIndex]^.AWord);
        memo1.Append('TFormKMemo2pdf.WriteLine wrote word=' + WordList[WordIndex]^.AWord + ' Font=' + WordList[WordIndex]^.FName + ' ' + inttostr(W) + ' ' + inttostr(H));
        XLoc := XLoc+W;
        inc(WordIndex);
    end;
    result := false;    // no more to do.
end;


function TFormKMemo2pdf.CheckHeight() : integer;
var
    W, i, WI : integer;     // word dimensions
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
            memo1.Append('WARNING - TFormKMemo2pdf.CheckHeight Failed to initially load the requested font : ' + OldFont);
            WordList[WI]^.FName := DefaultFont;                                // We will change it to default font and try again
            if not GetWordDimentions(WI, W, Result) then begin
                memo1.Append('ERROR - TFormKMemo2pdf.CheckHeight Failed to load the substitute font : ' + WordList[WI]^.FName);
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


{ Adobe Fonts permitted without embedding. May be case sensitive.
  Courier, Courier-Bold, Courier-Oblique, Courier-BoldOblique
  Helvetica, Helvetica-Bold, Helvetica-Oblique, Helvetica-BoldOblique,
  Times-Roman, Times-Bold, Times-Italic, Times-BoldItalic,
  Symbol, ZapfDingbats
}

function TFormKMemo2pdf.AdobeIndex(FName : string; Bold, Italic : boolean) : integer;
begin
    if Bold or Italic then                 // Note this only works for Helvetica and Courier, others use "Italic"
        FName := FName + '-';
    if Bold then FName := FName + 'Bold';
    if Italic then FName := FName + 'Oblique';
    Result := 0;
    while Result <= high(AdobeFonts) do begin
        if AdobeFonts[Result] = FName then exit;
        inc(Result);
    end;
    if Result >= high(AdobeFonts) then result := -1;
//    if Result < 0 then writeln('TFormKMemo2pdf.AdobeIndex failed to find ', FName, ' in Adobe Font Array'); // ToDo : fix safety
end;

function TFormKMemo2pdf.LoadFonts() : boolean;
var
    I  : integer;
//    CachedFont : TFPFontCacheItem;
//    Suffix : string = '';
begin
    Result := True;
    for I := 0 to high(AdobeFonts) do
        FDoc.AddFont(AdobeFonts[i]);        // Add only the Adobe fonts we have selected


    exit;

    // Code below here may be no longer required.
(*


    for i := 0 to FontList.Count -1 do begin
        //writeln('TFormKMemo2pdf.LoadFonts FontName=', FontList[i]^.FamName);
        Suffix := '';
        CachedFont := gTTFontCache.Find(FontList[i]^.FamName, FontList[i]^.Bold, FontList[i]^.Italic);
        if not Assigned(CachedFont) then begin
           Memo1.Append('Font ' + FontList[i]^.FamName + ' does not have a bold or italic font file.');
           //writeln('WARNING - TFormKMemo2pdf.LoadFonts could not find font in cache ' + FontList[i]^.FamName + ' ' + booltostr(FontList[i]^.Bold, True) +' ' + booltostr(FontList[i]^.Italic, True));
           Result := False;
           continue;                            // will cause (managed) problems in GetWordDimensions()  !
        end;
        // writeln('INFO - TFormKMemo2pdf.LoadFonts DID find font in cache ' + FontList[i]^.FamName + ' ' + booltostr(FontList[i]^.Bold, True) +' ' + booltostr(FontList[i]^.Italic, True));
        if  FontList[i]^.Bold then Suffix := 'B';
        if  FontList[i]^.Italic then Suffix := Suffix + 'I';
        if Suffix <> '' then Suffix := '-' + Suffix;
        FontList[i]^.FontIndex := FDoc.AddFont(CachedFont.FileName, CachedFont.FamilyName + Suffix);       // is This  my naming scheme ??
        // FontList[i]^.FontIndex := FDoc.AddFont(gTTFontCache.Items[i].FileName,  gTTFontCache.Items[i].FamilyName + Suffix);
        // writeln('TFormKMemo2pdf.LoadFonts Font=', CachedFont.FamilyName + Suffix, ' FontIndex=',FontList[i]^.FontIndex, ' Family=', FontList[i]^.FamName);
    end;     *)
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
        //writeln('TFormKMemo2pdf.WritePage writing  ' + WordList[i]^.AWord, X, ' ', Y);    // WARNING, crashes on a newline !!!!!!
    end;
end;

(*
procedure TFormKMemo2pdf.UpdateWordList(BadFName, GoodFName : string);          // ToDo : remove
var
    I  : integer;
begin
  for i := 0 to FontList.Count -1 do
        if FontList[i]^.FamName = BadFName then
               FontList[i]^.FamName := GoodFName;
  for i := 0 to WordList.Count-1 do
        if WordList[i]^.FName = BadFName then
            WordList[i]^.FName := GoodFName;
end;                       *)


function TFormKMemo2pdf.StartPDF : boolean;     // return false and user is shown the memo with error messages
{var
    i : integer;  }
begin
    Memo1.Clear;
    CurrentPage := -1;
    If WordList <> nil then
        FreeAndNil(WordList);
(*    if FontList <> Nil then
        FreeAndNil(FontList);
    FontList := TFontList.Create();  *)
    WordList := TWordList.Create;
    FDoc := TPDFDocument.Create(Nil);
    UseLocalFontInfo := FontInCache('Helvetica');
    if (not UseLocalFontInfo) and (not FontInCache('Liberation Sans')) then begin         // ToDo : wot about Mac ?
        Memo1.Append('ERROR - cannot find suitable fonts to use. Please install');
        Memo1.Append('the Adobe Standard fonts, Courier and Helvetica or the open');
        Memo1.Append('source Liberation Sans and Liberation Mono');
        exit(false);                    // neither the formal Adobe fonts nor known substitutes found.
    end;
    try
        KMemoRead();
        Result := MakePDF();            // False if we found an issue, probably font related !
    finally
        FDoc.Free;
//        FreeAndNil(FontList);
        FreeAndNil(WordList);
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
    if not LoadFonts() then begin
        if not AllowNoBoldItalic then
             {memo1.Append('Warning, your selected fonts may be unsuitable')
        else} begin
           showmessage('Warning, your selected fonts may be unsuitable');
           memo1.Append('Warning, Press Retry to make a PDF without bold or italics');
           Memo1.Append('or close, select more appropriate fonts from Settings and start again.');
           exit(False);                     // check for memory leaks here please
        end;
    end;

    S := FDoc.Sections.AddSection; // we always need at least one section
    // ToDo : chase this up one way or another.
    // If the note has eg bold or italic markup and the user selected font does not
    // have seperate font files for bold and/or italic, the markup will be ignored
    // and the user will be shown the following message.
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
    AFontName : string;

        procedure ReMapFont();      // Force the use of only the Adobe inbuilt fonts.
        begin                       // Even if they are not available.
        if (ExFont.Pitch = fpFixed) then begin
            ExFont.Name := 'Courier';           // Note, just "Courier", bold and Italic stored elsewhere
(*            if ExFont.Bold then
                ExFont.Name := 'Courier-Bold'   // Note, no Courier bold and Italic.
            else if ExFont.Italic then
                ExFont.Name := 'Courier-Oblique';     *)
            exit;
        end;
        ExFont.Name := 'Helvetica';
(*        if not (ExFont.Bold or ExFont.Italic) then exit;
        if ExFont.Bold and ExFont.Italic then
            ExFont.Name := 'Helvetica-BoldOblique'
        else if ExFont.Bold then
                ExFont.Name := 'Helvetica-Bold'
             else
               ExFont.Name := 'Helvetica-Oblique'   // must be italic if we are here     *)
        end;

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
           if ExFont.Name = 'default' then begin
               //writeln('INFO TFormKMemo2pdf.KMemoRead() Reasigning [' + TheKmemo.Blocks.Items[BlockNo].Text + '] to font=' + DefaultFont);
               ExFont.Name := DefaultFont;
           end;
           // writeln('INFO TFormKMemo2pdf.KMemoRead() [' + TheKMemo.Blocks.Items[BlockNo].Text + '] ' + booltostr(ExFont.Pitch = fpFixed, true));

           RemapFont();
           AFontName := ExFont.Name; // we might need to change this in next line.                               // todo : dont need
           //writeln('INFO TFormKMemo2pdf.KMemoRead() fontname before=' + AFontName + ' [' + TheKMemo.Blocks.Items[BlockNo].Text + ']');

//            FontList.Add(ExFont.Name, ExFont.Bold, ExFont.Italic, ExFont.Pitch, AFontName);                    // new model does not use fontlist
           // writeln('INFO TFormKMemo2pdf.KMemoRead() fontname after=' + AFontName);
           for I := 0 to TheKMemo.Blocks.Items[BlockNo].WordCount-1 do begin                // For every word in this block
               AWord := TheKMemo.Blocks.Items[BlockNo].Words[I];
               WordList.Add(AWord, ExFont.Size, ExFont.Bold, ExFont.Italic, False, ExFont.Color, AFontName);
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
    {   (TKMemoParagraph(Thekmemo.blocks.Items[BlockNo]).Numbering = pnuBullets)
    pnuNone, BulletOne .. BulletEight }
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

