{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2015 by Graeme Geldenhuys

    Description:
      This is a homegrown font cache. The fpReport reports can reference
      a font by its name. The job of the font cache is to look through
      its cached fonts to match the font name, and which *.ttf file it
      relates too. The reporting code can then extract font details
      correctly (eg: font width, height etc).

    See the file COPYING.FPC, included in the FPC distribution,
    for details about the copyright.

    Library GNU General Public License (with exception).

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpTTF;

{$mode objfpc}{$H+}

{.$define ttfdebug}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  fpparsettf;

type

  TTrueTypeFontStyle = (fsRegular, fsItalic, fsBold, fsCondensed, fsExtraLight, fsLight, fsSemibold, fsMedium, fsBlack, fsFixedWidth);
  TTrueTypeFontStyles = set of TTrueTypeFontStyle;


  { Forward declaration }
  TFPFontCacheList = class;


  TFPFontCacheItem = class(TObject)
  private
    FFamilyName: String;
    FFileName: String;
    FStyleFlags: TTrueTypeFontStyles;
    FFileInfo: TTFFileInfo;
    FOwner: TFPFontCacheList; // reference to FontCacheList that owns this instance
    FPostScriptName: string;
    FHumanFriendlyName: string; // aka FullName
    procedure   DoLoadFileInfo;
    procedure   LoadFileInfo;
    procedure   BuildFontCacheItem;
    procedure   SetStyleIfExists(var AText: string; var AStyleFlags: TTrueTypeFontStyles; const AStyleName: String; const AStyle: TTrueTypeFontStyle);
    function    GetIsBold: boolean;
    function    GetIsFixedWidth: boolean;
    function    GetIsItalic: boolean;
    function    GetIsRegular: boolean;
    function    GetFamilyName: String;
    function    GetPostScriptName: string;
    function    GetHumanFriendlyName: string;
    function    GetFileInfo: TTFFileInfo;
  public
    constructor Create(const AFilename: String);
    destructor  Destroy; override;
    { Result is in pixels }
    function    TextWidth(const AStr: utf8string; const APointSize: single): single;
    { Result is in pixels }
    function    TextHeight(const AText: utf8string; const APointSize: single; out ADescender: single): single;
    property    FileName: String read FFileName;
    property    FamilyName: String read GetFamilyName;
    property    PostScriptName: string read GetPostScriptName;
    property    HumanFriendlyName: string read GetHumanFriendlyName;
    property    FontData: TTFFileInfo read GetFileInfo;
    { A bitmasked value describing the full font style }
    property    StyleFlags: TTrueTypeFontStyles read FStyleFlags;
    { IsXXX properties are convenience properties, internally querying StyleFlags. }
    property    IsFixedWidth: boolean read GetIsFixedWidth;
    property    IsRegular: boolean read GetIsRegular;
    property    IsItalic: boolean read GetIsItalic;
    property    IsBold: boolean read GetIsBold;
  end;
  TFPFontCacheItemArray = Array of TFPFontCacheItem;

  { TFPFontCacheList }
  EFontNotFound = Class(Exception);

  TFPFontCacheList = class(TObject)
  private
    FBuildFontCacheIgnoresErrors: Boolean;
    FList: TObjectList;
    FSearchPath: TStringList;
    FDPI: integer;
    procedure   SearchForFonts(const AFontPath: String);
    procedure   SetDPI(AValue: integer);
    { Set any / or \ path delimiters to the OS specific delimiter }
    procedure   FixPathDelimiters;
  protected
    function    DoFindPostScriptFontName(const AFontName: string; ABold: boolean; AItalic: boolean; Out aBaseFont : TFPFontCacheItem): String;
    function    GetCount: integer; virtual;
    function    GetItem(AIndex: Integer): TFPFontCacheItem; virtual;
    procedure   SetItem(AIndex: Integer; AValue: TFPFontCacheItem); virtual;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   BuildFontCache;
    function    Add(const AObject: TFPFontCacheItem): integer;
    procedure   AssignFontList(const AStrings: TStrings);
    procedure   Clear;
    procedure   LoadFromFile(const AFilename: string);
    procedure   ReadStandardFonts;
    property    Count: integer read GetCount;
    function    IndexOf(const AObject: TFPFontCacheItem): integer;
    // Find postscript font name based on fontname and attributes
    function    FindPostScriptFontName(const AFontName: string; ABold: boolean; AItalic: boolean): String;
    // Same as Find, but raise exception when not found.
    function    GetPostScriptFontName(const AFontName: string; ABold: boolean; AItalic: boolean): String;
    function    Find(const AFontCacheItem: TFPFontCacheItem): integer; overload;
    function    Find(const AFamilyName: string; ABold: boolean; AItalic: boolean): TFPFontCacheItem; overload;
    function    Find(const APostScriptName: string): TFPFontCacheItem; overload;
    function    FindHumanFriendly(const AName: string ): TFPFontCacheItem; overload;
    function    FindFamily(const AFamilyName: string ): TFPFontCacheItemArray; overload;
    function    FindFont(const AName: string): TFPFontCacheItem; overload;
    { not used: utility function doing a conversion for us. }
    function    PointSizeInPixels(const APointSize: single): single;
    property    Items[AIndex: Integer]: TFPFontCacheItem read GetItem write SetItem; default;
    property    SearchPath: TStringList read FSearchPath;
    property    DPI: integer read FDPI write SetDPI;
    Property    BuildFontCacheIgnoresErrors : Boolean Read FBuildFontCacheIgnoresErrors Write FBuildFontCacheIgnoresErrors;
  end;


function gTTFontCache: TFPFontCacheList;

implementation

uses
  DOM
  ,XMLRead
  {$ifdef mswindows}
  ,Windows,  // for SHGetFolderPath API call used by gTTFontCache.ReadStandardFonts() method
  Shlobj,activex
  {$endif}
  ;

resourcestring
  rsNoSearchPathDefined = 'No search path was defined';
  rsNoFontFileName = 'The FileName property is empty, so we can''t load font data.';
  rsMissingFontFile = 'The font file <%s> can''t be found.';
  SErrFontNotFound = 'The font <%s> can''t be found';

var
  uFontCacheList: TFPFontCacheList;

function gTTFontCache: TFPFontCacheList;
begin
 if not Assigned(uFontCacheList) then
 begin
   uFontCacheList := TFPFontCacheList.Create;
 end;
 Result := uFontCacheList;
end;

{ TFPFontCacheItem }

procedure TFPFontCacheItem.DoLoadFileInfo;
begin
  if not Assigned(FFileInfo) then
    LoadFileInfo;
end;

procedure TFPFontCacheItem.LoadFileInfo;
begin
  if FileExists(FFilename) then
  begin
    FFileInfo := TTFFileInfo.Create;
    FFileInfo.LoadFromFile(FFilename);
    BuildFontCacheItem;
  end
  else
    raise ETTF.CreateFmt(rsMissingFontFile, [FFilename]);
end;

function TFPFontCacheItem.GetIsBold: boolean;
begin
  DoLoadFileInfo;
  Result := fsBold in FStyleFlags;
end;

function TFPFontCacheItem.GetIsFixedWidth: boolean;
begin
  DoLoadFileInfo;
  Result := fsFixedWidth in FStyleFlags;
end;

function TFPFontCacheItem.GetIsItalic: boolean;
begin
  DoLoadFileInfo;
  Result := fsItalic in FStyleFlags;
end;

function TFPFontCacheItem.GetIsRegular: boolean;
begin
  DoLoadFileInfo;
  Result := fsRegular in FStyleFlags;
end;

function TFPFontCacheItem.GetFamilyName: String;
begin
  DoLoadFileInfo;
  Result := FFamilyName;
end;

function TFPFontCacheItem.GetPostScriptName: string;
begin
  DoLoadFileInfo;
  Result := FPostScriptName;
end;

function TFPFontCacheItem.GetHumanFriendlyName: string;
begin
  DoLoadFileInfo;
  Result := FHumanFriendlyName;
end;

function TFPFontCacheItem.GetFileInfo: TTFFileInfo;
begin
  DoLoadFileInfo;
  Result := FFileInfo;
end;

procedure TFPFontCacheItem.BuildFontCacheItem;
var
  s: string;
begin
  s := FFileInfo.PostScriptName;
  FPostScriptName := s;
  FFamilyName := FFileInfo.FamilyName;
  if Pos(s, FFamilyName) = 1 then
    Delete(s, 1, Length(FFamilyName));
  FHumanFriendlyName := FFileInfo.HumanFriendlyName;

  FStyleFlags := [fsRegular];

  // extract simple styles first
  if FFileInfo.PostScript.isFixedPitch > 0 then
    FStyleFlags := [fsFixedWidth]; // this should overwrite Regular style

  if FFileInfo.PostScript.ItalicAngle <> 0 then
    FStyleFlags := FStyleFlags + [fsItalic];

  // Now to more complex styles stored in StyleName field. eg: 'Condensed Medium'
  SetStyleIfExists(s, FStyleFlags, 'Bold', fsBold);
  SetStyleIfExists(s, FStyleFlags, 'Condensed', fsCondensed);
  SetStyleIfExists(s, FStyleFlags, 'ExtraLight', fsExtraLight);
  SetStyleIfExists(s, FStyleFlags, 'Light', fsLight);
  SetStyleIfExists(s, FStyleFlags, 'Semibold', fsSemibold);
  SetStyleIfExists(s, FStyleFlags, 'Medium', fsMedium);
  SetStyleIfExists(s, FStyleFlags, 'Black', fsBlack);
  SetStyleIfExists(s, FStyleFlags, 'Oblique', fsItalic);
end;

procedure TFPFontCacheItem.SetStyleIfExists(var AText: string; var AStyleFlags: TTrueTypeFontStyles;
  const AStyleName: String; const AStyle: TTrueTypeFontStyle);
var
  i: integer;
begin
  i := Pos(AStyleName, AText);
  if i > 0 then
  begin
    AStyleFlags := AStyleFlags + [AStyle];
    Delete(AText, i, Length(AStyleName));
  end;
end;

constructor TFPFontCacheItem.Create(const AFilename: String);
begin
  inherited Create;
  FFileName := AFilename;
  FStyleFlags := [fsRegular];

  if AFileName = '' then
    raise ETTF.Create(rsNoFontFileName);
end;

destructor TFPFontCacheItem.Destroy;
begin
  FFileInfo.Free;
  inherited Destroy;
end;

{ TextWidth returns with width of the text. If APointSize = 0.0, then it returns
  the text width in Font Units. If APointSize > 0 then it returns the text width
  in Pixels. }
function TFPFontCacheItem.TextWidth(const AStr: utf8string; const APointSize: single): single;
{
    From Microsoft's Typography website:
    Converting FUnits (font units) to pixels

    Values in the em square are converted to values in the pixel coordinate system
    by multiplying them by a scale. This scale is:

    pointSize * resolution / ( 72 points per inch * units_per_em )

    where pointSize is the size at which the glyph is to be displayed, and resolution
    is the resolution of the output device. The 72 in the denominator reflects the
    number of points per inch.

    For example, assume that a glyph feature is 550 FUnits in length on a 72 dpi
    screen at 18 point. There are 2048 units per em. The following calculation
    reveals that the feature is 4.83 pixels long.

    550 * 18 * 72 / ( 72 * 2048 ) = 4.83
}
var
  i: integer;
  lWidth: integer;
  lGIndex: integer;
  us: UnicodeString;
  {$IFDEF ttfdebug}
  sl: TStringList;
  s: string;
  {$ENDIF}
begin
  DoLoadFileInfo;
  Result := 0;
  if Length(AStr) = 0 then
    Exit;

  if not Assigned(FFileInfo) then
    Exit;

  {$IFDEF ttfdebug}
    sl := TStringList.Create;
    s := '';
    for i := 0 to 255 do
    begin
      lGIndex := FFileInfo.GetGlyphIndex(i);
      lWidth := FFileInfo.GetAdvanceWidth(lGIndex);
      s := s + ',' + IntToStr(lWidth);
    end;
    sl.Add(s);
    sl.Add('UnitsPerEm = ' + IntToStr(FFileInfo.Head.UnitsPerEm));
    sl.SaveToFile(GetTempDir(True) + FFileInfo.PostScriptName + '.txt');
    sl.Free;
  {$ENDIF}

  lWidth := 0;
  us := UTF8Decode(AStr);
  for i := 1 to Length(us) do
  begin
    lGIndex := FFileInfo.GetGlyphIndex(Word(us[i]));
    lWidth := lWidth + FFileInfo.GetAdvanceWidth(lGIndex);
  end;
  if APointSize = 0.0 then
    Result := lWidth
  else
  begin
    { Converting Font Units to Pixels. The formula is:
      pixels = glyph_units * pointSize * resolution / ( 72 points per inch * THead.UnitsPerEm )  }
    Result := lWidth * APointSize * FOwner.DPI / (72 * FFileInfo.Head.UnitsPerEm);
  end;
end;

function TFPFontCacheItem.TextHeight(const AText: utf8string; const APointSize: single; out ADescender: single): single;
begin
  DoLoadFileInfo;
  { Both lHeight and lDescenderHeight are in pixels }
  Result := FFileInfo.CapHeight * APointSize * gTTFontCache.DPI / (72 * FFileInfo.Head.UnitsPerEm);
  ADescender := Abs(FFileInfo.Descender) * APointSize * gTTFontCache.DPI / (72 * FFileInfo.Head.UnitsPerEm);
end;

{ TFPFontCacheList }

procedure TFPFontCacheList.SearchForFonts(const AFontPath: String);
var
  sr: TSearchRec;
  lFont: TFPFontCacheItem;
  s: String;
begin
  if SysUtils.FindFirst(AFontPath + AllFilesMask, faAnyFile, sr) = 0 then
  begin
    repeat
      // check if special files to skip
      if (sr.Name = '.') or (sr.Name = '..') or (sr.Name = '') then
        Continue;
      // We got something, so lets continue
      s := sr.Name;
      if (sr.Attr and faDirectory) <> 0 then // found a directory
        SearchForFonts(IncludeTrailingPathDelimiter(AFontPath + s))
      else
      begin // we have a file
        if (lowercase(ExtractFileExt(s)) = '.ttf') {or             // DRB
           (lowercase(ExtractFileExt(s)) = '.otf')} then
        begin
          try
            lFont := TFPFontCacheItem.Create(AFontPath + s);
            Add(lFont);
          except
            if not FBuildFontCacheIgnoresErrors then
              Raise;
          end;
        end;
      end;
    until SysUtils.FindNext(sr) <> 0;
  end;
  SysUtils.FindClose(sr);
end;

procedure TFPFontCacheList.SetDPI(AValue: integer);
begin
  if FDPI = AValue then Exit;
  FDPI := AValue;
end;

procedure TFPFontCacheList.FixPathDelimiters;
var
  i: integer;
begin
  for i := 0 to FSearchPath.Count-1 do
    FSearchPath[i] := SetDirSeparators(FSearchPath[i]);
end;

function TFPFontCacheList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TFPFontCacheList.GetItem(AIndex: Integer): TFPFontCacheItem;
begin
  Result := TFPFontCacheItem(FList.Items[AIndex]);
end;

procedure TFPFontCacheList.SetItem(AIndex: Integer; AValue: TFPFontCacheItem);
begin
  FList.Items[AIndex] := AValue;
end;

constructor TFPFontCacheList.Create;
begin
  inherited Create;
  FList := TObjectList.Create;
  FSearchPath := TStringList.Create;
  FDPI := 96; // The default is the most common dpi used
end;

destructor TFPFontCacheList.Destroy;
begin
  FList.Free;
  FSearchPath.Free;
  inherited Destroy;
end;

procedure TFPFontCacheList.BuildFontCache;
var
  lPath: String;
  i: integer;
begin
  if FSearchPath.Count < 1 then
    raise ETTF.Create(rsNoSearchPathDefined);

  FixPathDelimiters;
  for i := 0 to FSearchPath.Count-1 do
  begin
    lPath := FSearchPath[i];
    if DirectoryExists(lPath) then
      SearchForFonts(IncludeTrailingPathDelimiter(lPath));
  end;
end;

function TFPFontCacheList.Add(const AObject: TFPFontCacheItem): integer;
begin
  Result := FList.Add(AObject);
  AObject.FOwner := self;
end;

procedure TFPFontCacheList.AssignFontList(const AStrings: TStrings);
var
  i: integer;
begin
  if not Assigned(AStrings) then
    Exit;
  AStrings.Clear;
  for i := 0 to FList.Count-1 do
    AStrings.Add(TFPFontCacheItem(FList.Items[i]).PostScriptName);
end;

procedure TFPFontCacheList.Clear;
begin
  FList.Clear;
end;

procedure TFPFontCacheList.LoadFromFile(const AFilename: string);
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFilename);
    for i := 0 to sl.Count-1 do
      Add(TFPFontCacheItem.Create(sl[i]));
  finally
    sl.Free;
  end;
end;

{ This is operating system dependent. Our default implementation only supports
  Linux, FreeBSD, Windows and OSX. On other platforms, no fonts will be loaded,
  until a implementation is created.

  NOTE:
    This is definitely not a perfect solution, especially due to the inconsistent
    implementations and locations of files under various Linux distros. But it's
    the best we can do for now. }
procedure TFPFontCacheList.ReadStandardFonts;

  {$ifdef linux}
    {$define HasFontsConf}
    const
      cFontsConf = '/etc/fonts/fonts.conf';
  {$endif}

  {$ifdef freebsd}
    {$define HasFontsConf}
    const
      cFontsConf = '/usr/local/etc/fonts/fonts.conf';
  {$endif}

  {$ifdef mswindows}
  function GetWinFontsDir: string;
  var
    {$if FPC_FULLVERSION < 30400}
    w :  Array[0..MaxPathLen] of Char;
    {$ELSE}
    w : pwidechar;
    {$ENDIF}
  begin
    {$if FPC_FULLVERSION < 30400}
    SHGetSpecialFolderPath(0,w,CSIDL_FONTS,false);
    {$else}
    SHGetKnownFolderPath(FOLDERID_Fonts,0,0,w);
    {$endif}
    Result := w;
    {$if FPC_FULLVERSION > 30400}
    CoTaskMemFree(w);
    {$endif}
  end;
{$endif}

{$ifdef HasFontsConf}
var
  doc: TXMLDocument;
  lChild: TDOMNode;
  lDir: string;
{$endif}
begin
  {$ifdef HasFontsConf} // Linux & FreeBSD
  ReadXMLFile(doc, cFontsConf);
  try
    lChild := doc.DocumentElement.FirstChild;
    while Assigned(lChild) do
    begin
      if lChild.NodeName = 'dir' then
      begin
        if lChild.FirstChild.NodeValue = '~/.fonts' then
          lDir := ExpandFilename(lChild.FirstChild.NodeValue)
        else
          lDir := lChild.FirstChild.NodeValue;
        SearchPath.Add(lDir);
        // writeln(lDir);
      end;
      lChild := lChild.NextSibling;
    end;
  finally
    doc.Free;
  end;
  {$endif}

  {$ifdef mswindows}
  SearchPath.Add(GetWinFontsDir);
  {$endif}

  {$ifdef darwin} // OSX
  { As per Apple Support page: https://support.apple.com/en-us/HT201722 }
  SearchPath.Add('/System/Library/Fonts/');
  SearchPath.Add('/Library/Fonts/');
  SearchPath.Add(ExpandFilename('~/Library/Fonts/'));
  {$endif}

  BuildFontCache;
end;

function TFPFontCacheList.IndexOf(const AObject: TFPFontCacheItem): integer;
begin
  Result := FList.IndexOf(AObject);
end;

function TFPFontCacheList.GetPostScriptFontName(const AFontName: string; ABold: boolean; AItalic: boolean): String;

Var
  lFC : TFPFontCacheItem;
  lMissingFontName : String;

begin
  Result:=DoFindPostScriptFontName(aFontName,aBold,aItalic,lfc);
  if (Result=aFontName) and (aBold or aItalic) then
    begin
    if lFC<>Nil then
      lMissingFontName := lfc.FamilyName
    else
      lMissingFontName := aFontName;
    if (aBold and AItalic) then
      lMissingFontName := lMissingFontName + '-BoldItalic'
    else if aBold then
      lMissingFontName := lMissingFontName + '-Bold'
    else if aItalic then
      lMissingFontName := lMissingFontName + '-Italic';
    raise EFontNotFound.CreateFmt(SErrFontNotFound, [lMissingFontName]);
    end;
end;

function TFPFontCacheList.FindPostScriptFontName(const AFontName: string; ABold: boolean; AItalic: boolean): String;

Var
  lFC : TFPFontCacheItem;

begin
  Result:=DoFindPostScriptFontName(aFontName,aBold,aItalic,lfc);
end;

function  TFPFontCacheList.DoFindPostScriptFontName(const AFontName: string; ABold: boolean; AItalic: boolean; Out aBaseFont : TFPFontCacheItem): String;

Var
   lNewFC : TFPFontCacheItem;

begin
  Result:=aFontName;
  aBaseFont := FindFont(aFontName);
  if not Assigned(aBaseFont) then
    exit;
  // find corresponding font style (bold and/or italic)
  lNewFC := Find(aBaseFont.FamilyName, aBold, aItalic);
  if not Assigned(lNewFC) then
    exit;
  Result := lNewFC.PostScriptName;
end;

function TFPFontCacheList.Find(const AFontCacheItem: TFPFontCacheItem): integer;
var
  i: integer;
begin
  Result := -1; // nothing found
  for i := 0 to Count-1 do
  begin
    if (Items[i].FamilyName = AFontCacheItem.FamilyName) and
       (Items[i].StyleFlags = AFontCacheItem.StyleFlags) then
    begin
      Result := i;
      exit;
    end;
  end;
end;

function TFPFontCacheList.Find(const AFamilyName: string; ABold: boolean; AItalic: boolean): TFPFontCacheItem;

var
  i: integer;

begin
  for i := 0 to Count-1 do
  begin
    Result := Items[i];
    if SameText(Result.FamilyName,AFamilyName) and (Result.IsItalic = AItalic)
        and (Result.IsBold = ABold)
    then
      exit;
  end;
  Result := nil;
end;

function TFPFontCacheList.Find(const APostScriptName: string): TFPFontCacheItem;
var
  i: integer;
begin
  for i := 0 to Count-1 do
  begin
    Result := Items[i];
    if SameText(Result.PostScriptName,APostScriptName) then
      Exit;
  end;
  Result := nil;
end;

function TFPFontCacheList.FindHumanFriendly(const AName: string): TFPFontCacheItem;
var
  i: integer;
begin
  for i := 0 to Count-1 do
  begin
    Result := Items[i];
    if SameText(Result.HumanFriendlyName,AName) then
      Exit;
  end;
  Result := nil;
end;

function TFPFontCacheList.FindFamily(const AFamilyName: string): TFPFontCacheItemArray;

var
  i,aLen: integer;
  f : TFPFontCacheItem;

begin
  aLen:=0;
  SetLength(Result,Count);
  for i := 0 to Count-1 do
    begin
    f:=Items[i];
    if SameText(F.FamilyName,AFamilyName) then
      begin
      Result[aLen]:=F;
      inc(Alen);
      end;
    end;
  SetLength(Result,aLen);
end;

function TFPFontCacheList.FindFont(const AName: string): TFPFontCacheItem;

Var
  aFamily : TFPFontCacheItemArray;

begin
  Result:=Find(AName);
  if (Result=Nil) then
    Result:=Find(aName,False,False);
  if (Result=Nil) then
    Result:=FindHumanFriendly(aName);
  if (Result=Nil) then
    begin
    aFamily:=FindFamily(aName);
    if Length(aFamily)>0 then
      Result:=aFamily[0];
    end;
end;

function TFPFontCacheList.PointSizeInPixels(const APointSize: single): single;
begin
  Result := APointSize * DPI / 72;
end;


initialization
  uFontCacheList := nil;

finalization
  uFontCacheList.Free;

end.


