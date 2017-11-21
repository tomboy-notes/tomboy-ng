unit EditBox;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
    Menus, StdCtrls, Buttons, RichMemo;

type

    { TEditBoxForm }

    TEditBoxForm = class(TForm)
        ButtLink: TBitBtn;
        ButtText: TBitBtn;
        ButtTools: TBitBtn;
        ButtDelete: TBitBtn;
        ButtNotebook: TBitBtn;
        ButtSearch: TBitBtn;
        MenuBold: TMenuItem;
        MenuItalic: TMenuItem;
        MenuHighLight: TMenuItem;
        MenuItemWrite: TMenuItem;
        MenuItemSync: TMenuItem;
        MenuItemExportHTML: TMenuItem;
        MenuSmall: TMenuItem;
        MenuItem2: TMenuItem;
        MenuNormal: TMenuItem;
        MenuLarge: TMenuItem;
        MenuFixedWidth: TMenuItem;
        MenuHuge: TMenuItem;
        PopupMenuTools: TPopupMenu;
        PopupMenuText: TPopupMenu;
        RichMemo1: TRichMemo;
        procedure ButtSearchClick(Sender: TObject);
        procedure ButtTextClick(Sender: TObject);
        procedure ButtToolsClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
        procedure MenuBoldClick(Sender: TObject);
        procedure MenuHighLightClick(Sender: TObject);
        procedure MenuHugeClick(Sender: TObject);
        procedure MenuItalicClick(Sender: TObject);
        procedure MenuItemWriteClick(Sender: TObject);
        procedure MenuLargeClick(Sender: TObject);
        procedure MenuNormalClick(Sender: TObject);
        procedure MenuSmallClick(Sender: TObject);
        procedure RichMemo1Change(Sender: TObject);
        procedure RichMemo1LinkAction(Sender: TObject; ALinkAction: TLinkAction;
            	const info: TLinkMouseInfo; LinkStart, LinkLen: Integer);
    private
        Str : ANSIString;
        InContent : boolean;
        Bold : boolean;
        Italic : boolean;
        HighLight : boolean;
        FontSize : integer;
        DoneFirst : boolean;
        CreateDate : string;
        Dirty : boolean;
        Ready : boolean;
        TheFont : string;	// Set in OnShow, const after that
        FontNormal : integer; // as above
        function GetAFilename() : ANSIString;
        procedure CheckForLinks();
        Procedure ReadTag(fs : TFileStream);
        procedure AddText();
        procedure ImportNote(FileName : string);
        procedure MarkTitle();
        Function Footer() : ANSIstring;
        Function Header(var Index : longint) : ANSIstring;
        Function GetLocalTime():string;
        procedure SaveNote();
        function SetFontXML(Size : integer; TurnOn : boolean) : string;
    public
        NoteFileName, NoteTitle : string;
    end;

var
    EditBoxForm: TEditBoxForm;

const
  // Font sizes, no text should be other than these sizes. Note TheFont and FontNormal are vars
  FontSmall = 8;
  FontLarge = 15;
  FontHuge = 18;
  FontTitle = 16;	// Dont set this to one of the other sizes !

                        {    On each OS there is a "default" or "Application" font, if you make a
                            call to SetLink() and keep typing, you will get that font.
                            However, unless set explicitly, prior to that, its different,
                            apparently undefined. So far I have determined that this "default" is
                            Windows - Tahoma 11
                            Ubuntu et el - Ubuntu 11
                            Debian Gnome - Cantarell 11
                            Debian xfce - Sans 10
                            }

implementation

{$R *.lfm}

{ TEditBoxForm }
uses RichMemoUtils, Unit1 {$ifdef LINUX}, Unix {$endif} ;


{  ---- E D I T I N G   F U N C T I O N S ----- }


procedure TEditBoxForm.ButtTextClick(Sender: TObject);
begin
    PopupMenuText.PopUp;
end;

procedure TEditBoxForm.ButtSearchClick(Sender: TObject);
begin
    SearchForm.Show;
end;



procedure TEditBoxForm.ButtToolsClick(Sender: TObject);
begin
    PopupMenuTools.PopUp;
end;

procedure TEditBoxForm.MenuBoldClick(Sender: TObject);
var
     TP : TFontParams;
begin
       if RichMemo1.GetTextAttributes(RichMemo1.SelStart, TP) then begin
          if fsBold in TP.Style then
             RichMemo1.SetRangeParams( RichMemo1.SelStart,RichMemo1.SelLength,
                 [tmm_Styles], TP, [], [fsBold])
          else
              RichMemo1.SetRangeParams( RichMemo1.SelStart,RichMemo1.SelLength,
                 [tmm_Styles], TP, [fsBold], []);
          Dirty := true;
       end;

end;

procedure TEditBoxForm.MenuHighLightClick(Sender: TObject);
var
     TP : TFontParams;
begin
       if RichMemo1.GetTextAttributes(RichMemo1.SelStart, TP) then begin
          TP.HasBkClr := true;
          if  TP.BkColor = clYellow then
              TP.BkColor := $FFFFFF				// A linux thing, turning it 'off' does not work
          else  TP.BkColor := clYellow;
          RichMemo1.SetRangeParams( RichMemo1.SelStart,RichMemo1.SelLength,
                 [tmm_BackColor], TP, [], []);
          Dirty := true;
       end;
end;

procedure TEditBoxForm.MenuHugeClick(Sender: TObject);
var
     TP : TFontParams;
begin
    if RichMemo1.GetTextAttributes(RichMemo1.SelStart, TP) then begin
       if TP.Size = FontHuge then
          TP.Size := FontNormal
       else
          TP.Size := FontHuge;
       RichMemo1.SetRangeParams( RichMemo1.SelStart,RichMemo1.SelLength,
              [tmm_Size], TP, [], []);
       Dirty := true;
    end;
end;

procedure TEditBoxForm.MenuItalicClick(Sender: TObject);
var
     TP : TFontParams;
begin
    if RichMemo1.GetTextAttributes(RichMemo1.SelStart, TP) then begin
       if fsItalic in TP.Style then
          RichMemo1.SetRangeParams( RichMemo1.SelStart,RichMemo1.SelLength,
                 [tmm_Styles], TP, [], [fsItalic])
       else
          RichMemo1.SetRangeParams( RichMemo1.SelStart,RichMemo1.SelLength,
                 [tmm_Styles], TP, [fsItalic], []);
       Dirty := true;
    end;
end;



procedure TEditBoxForm.MenuLargeClick(Sender: TObject);
var
     TP : TFontParams;
begin
    if RichMemo1.GetTextAttributes(RichMemo1.SelStart, TP) then begin
       if TP.Size = FontLarge then
          TP.Size := FontNormal
       else
          TP.Size := FontLarge;
       RichMemo1.SetRangeParams( RichMemo1.SelStart,RichMemo1.SelLength,
              [tmm_Size], TP, [], []);
       Dirty := true;
    end;
  end;

procedure TEditBoxForm.MenuNormalClick(Sender: TObject);
var
     TP : TFontParams;
begin
       RichMemo1.GetTextAttributes(RichMemo1.SelStart, TP);
       TP.Size := FontNormal;
       RichMemo1.SetRangeParams( RichMemo1.SelStart,RichMemo1.SelLength,
                 [tmm_Size], TP, [], []);
       Dirty := true;
end;

procedure TEditBoxForm.MenuSmallClick(Sender: TObject);
var
     TP : TFontParams;
  begin
       if RichMemo1.GetTextAttributes(RichMemo1.SelStart, TP) then begin
          if TP.Size = FontSmall then
             TP.Size := FontNormal
          else
             TP.Size := FontSmall;
          RichMemo1.SetRangeParams( RichMemo1.SelStart,RichMemo1.SelLength,
                 [tmm_Size], TP, [], []);
          Dirty := true;
       end;
end;


{ - - - H O U S E   K E E P I N G   F U C T I O N S ----- }

procedure TEditBoxForm.RichMemo1LinkAction(Sender: TObject;
    ALinkAction: TLinkAction; const info: TLinkMouseInfo; LinkStart,
    LinkLen: Integer);
	// Note as of August 2017, this funtion is incomplete in Win10
	// Cannot rely on LinkStart or LinkLen so workaround.
	// http://forum.lazarus.freepascal.org/index.php/topic,37909.0.html
var
     Index, Len : longint;
begin
    Index := RichMemo1.SelStart;
    Len := 0;
    while RichMemo1.IsLink(Index) do dec(Index);
    inc(Index);
    While RichMemo1.isLink(Index + Len) do inc(Len);

    // Ahh, but what if user has changed the note's title ??
    if RichMemo1.GetText(LinkStart, LinkLen) <> NoteTitle then
    	SearchForm.OpenNote(RichMemo1.GetText(Index, Len));
    // SearchForm.OpenNote(RichMemo1.GetText(LinkStart, LinkLen));
end;


procedure TEditBoxForm.FormShow(Sender: TObject);
var
  FP :  TFontParams;
begin
    RichMemo1.Clear;                     // Yes, I know this is horrible.
    RichMemo1.Append('a line of text');  // but it works around an incomplete
    Richmemo1.SetLink(2, 4, true);       // setlink(). I accept all blame DRB
    RichMemo1.GetTextAttributes(4, FP);
    SearchForm.Edit1.Text := 'Size - ' + inttostr(FP.Size) + ' ' + FP.Name;
    TheFont := FP.Name;
    FontNormal := FP.Size;
    if length(NoteTitle) > 0 then begin
       Caption := NoteTitle;
       ImportNote(NoteFileName);
    end else begin
        RichMemo1.Clear;
        Ready := true;
    end;
end;

procedure TEditBoxForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    Release;
end;

procedure TEditBoxForm.MarkTitle();  // First line must always be title.
var
    i : longint = 0;
    Start, Len : longint;
    FP :  TFontParams;
begin
    i := length(RichMemo1.Lines[0]);
    FP.Size := FontTitle;
    FP.Color := clBlue;
    FP.Style := [fsUnderline];
    FP.HasBkClr := false;
    FP.Name := TheFont;
    FP.VScriptPos := vpNormal;
    RichMemo1.SetTextAttributes(0, i, FP);
    if RichMemo1.GetTextAttributes(i+1, FP) then   // Has user 'smeared' the title ?
        if FP.Size = FontTitle then begin
                if RichMemo1.GetStyleRange(i+1, Start, Len) then begin
                FP.Size := FontNormal;
                FP.Color := clBlack;
                RichMemo1.SetRangeParams(i, Len, [tmm_Styles, tmm_Size, tmm_Color], FP, [], [fsUnderline] );
            end;
     end;   { TODO 1 : Things here a user can do I don't detect. Like insert new line. }
end;

procedure TEditBoxForm.CheckForLinks();
var
      Searchterm : ANSIstring;
      Start, len, LinkLen, SearchStart : longint;
      TitleLen : integer;
begin
    Titlelen := length(RichMemo1.Lines[0]);
      SearchForm.StartSearch();
      Len := RichMemo1.GetTextLen();
      while SearchForm.NextNoteTitle(SearchTerm) do begin
          SearchStart := 0;
          while RichMemo1.Search(Searchterm, SearchStart, Len, [], Start, Linklen) do begin
                if Start <= TitleLen then begin
                   MarkTitle();
                end else RichMemo1.SetLink(Start, LinkLen, true, 'local:link');
                // Last line depends on http://forum.lazarus.freepascal.org/index.php/topic,37850.0.html
                SearchStart := SearchStart + Start + 1;
          end;
      end;
end;

procedure TEditBoxForm.RichMemo1Change(Sender: TObject);
var
     Point : TPoint;
     Start, Len : longint;
begin
    if not Ready then exit();           // don't do any of this while starting up.
    Start := RichMemo1.SelStart -1;
    Point := Richmemo1.CaretPos;
    if Point.Y < 2 then             // Only check title if Caret is up near title
        MarkTitle();
    if RichMemo1.isLink(Start) then begin
        // We know we are in a link, but how far does it extend, left and right ?
        while RichMemo1.IsLink(Start) do dec(Start);
        inc(Start);
        Len := 0;
        while Richmemo1.IsLink(Start + Len) do inc(Len);
        // Memo1.Append('In link right now! ' + inttostr(Start) + ' ' + inttostr(Len));
        RichMemo1.SetLink(Start, Len, False);  // then clear that link
    end;
    // But what if we just made a link ?
    CheckForLinks();{ TODO 1 : Thats very inefficent, better to just check that line +/- 1 }
    Dirty := true;
    PopUpMenuTools.Items.Items[2].Enabled := true;
end;



{ --- I M P O R T I N G        F U N C T I O N S    ----  }


procedure TEditBoxForm.AddText();
var
  FP :  TFontParams; // Name (ie font name), Size, Color, Style,  HasBkClr, BkColor and others
begin
  if (Str = '') or (not InContent) then exit;
  FP.Style := [];
  FP.Color := clBlack;
  FP.Name := TheFont;			{ TODO 2 : Going to have to solve font across platforms issue }
  FP.VScriptPos := vpNormal;
  if Bold then FP.Style := FP.Style + [fsBold]
  else  FP.Style := FP.Style - [fsBold];
  if Italic then FP.Style := FP.Style + [fsItalic]
  else  FP.Style := FP.Style - [fsItalic];
  if HighLight then begin
        FP.HasBkClr := true;
        FP.BkColor:= clYellow;
  end else FP.HasBkClr := false;
  FP.Size := FontSize;
  InsertFontText(RichMemo1, Str, FP, -1);
end;


Procedure TEditBoxForm.ReadTag(fs : TFileStream);    // we are here because '<'
var
    Buff : String;
    Ch : char = ' ';
begin
  Addtext();    // Write the text we have so far with existing params
  Buff := '';   // now, lets set new params or get other data
  while fs.Position < fs.Size do begin
      fs.read(Ch, 1);
      if Ch = '>'then exit;
      Buff := Buff + Ch;
      case Buff of
            'note-content version="0.1"' : InContent := true;
            'note-content version="0.3"' : InContent := true;
            '/note-content' : InContent := false;
            'bold' : Bold := True;
            '/bold' : Bold := False;
            'italic' : Italic := True;
            '/italic' : Italic := false;
            'highlight' : HighLight := true;
            '/highlight' : HighLight := false;
            'size:small' : FontSize := FontSmall;
            '/size:small' : FontSize := FontNormal;
            'size:large' : FontSize := FontLarge;
            '/size:large' : FontSize := FontNormal;
            'size:huge' : FontSize := FontHuge;
            '/size:huge' : FontSize := FontNormal;
            '/create-date' : CreateDate := Str;
      end;
  end;
end;


procedure TEditBoxForm.ImportNote(FileName : ANSIstring);
var
 fs : TFileStream;
 ch : char = ' ';
 // TS : TTimeStamp;           // Temp time stamping
begin
  { TS:=DateTimeToTimeStamp(Now);
  Memo1.Clear;
  Memo1.Append(inttostr(TS.Time));
  memo1.Append('Opening ' + FileName);       }
  FontSize := FontNormal;
  RichMemo1.Clear;
  fs := TFileStream.Create(Utf8ToAnsi(FileName), fmOpenRead or fmShareDenyNone);
    try
       while fs.Position < fs.Size do begin
         fs.read(ch, 1);
          if Ch = '<' then begin
             ReadTag(fs);
             Str := '';
          end else
                Str := Str + ch;
        end;
    finally
        FreeAndNil(fs);
    end;
    // TS:=DateTimeToTimeStamp(Now);
    // Memo1.Append(inttostr(TS.Time));
    MarkTitle();
    CheckForLinks();
    // TS:=DateTimeToTimeStamp(Now);
    // Memo1.Append(inttostr(TS.Time));
    PopUpMenuTools.Items.Items[2].Enabled := false;  { TODO 1 : This is for manual saving, we'll do auto in real thing }
    Ready := true;
end;


{ --- S A V I N G    F U N C T I O N S --- }

{
procedure TEditBoxForm.ButtonTestWriteClick(Sender: TObject);
begin
    SaveNote;
end;
 }

{ TODO 1 : If user has altered the title and then closes the note (or manually saves)
we need to tell the search screen to reload the notes list. Or, in case a large list,
should we just update that record ? }

function TEditBoxForm.GetAFilename() : ANSIString;
var
  Ch, Index : integer;
  FileName : string = '';   // A:65, Z:90, a:97, z:122   91-96 (6) unusable
begin
   	Index := 0;
    randomize();				// comment out to test name clash handling.
	while (Index < 36) do begin
		Ch := random(ord('z') - ord('A') +1) + ord('A');
		if (Ch > ord('Z')) and (Ch < ord('a')) then Ch := Ch + 6;
		FileName := FileName + char(Ch);
       	inc(Index);
    end;
    Result := SearchForm.NoteDirectory + FileName + '.note';
end;


procedure TEditBoxForm.MenuItemWriteClick(Sender: TObject);
begin
  	if length(NoteFileName) = 0 then
        NoteFileName := GetAFilename();
    SaveNote();
    Dirty := false;
end;

function TEditBoxForm.SetFontXML(Size : integer; TurnOn : boolean) : string;
begin
	Result := '';
    case Size of
    	FontHuge : if TurnOn then Result  := '<size:huge>' else Result  := '</size:huge>';
        FontLarge : if TurnOn then Result  := '<size:large>' else Result  := '</size:large>';
        FontSmall :  if TurnOn then Result  := '<size:small>' else Result  := '</size:small>';
    end;
end;

procedure TEditBoxForm.SaveNote;
var
   Start : integer = 0;
   Len  : integer = 0;
   Index  : longint = 0;
   FP :  TFontParams;
   Buff : ANSIstring = '';
   OutStream:TFilestream;
   FSize : integer = 0;
   Bld : boolean = false;
   Italics : boolean = False;
   HiLight : boolean = False;
 begin
    // FSize := FontNormal;
    try
        outstream :=TFilestream.Create(NoteFileName, fmCreate);
        // Write and WriteBuffer accept a buffer, not a string !  Need to start at pos 1
        // when sending string or ANSIstring otherwise it uses first byte which makes it look like a binary file.
        // http://free-pascal-general.1045716.n5.nabble.com/Creating-text-files-with-TFileStream-td2824859.html
        Buff := Header(Index);
        OutStream.Write(Buff[1], length(Buff));
        Buff := '';
        while RichMemo1.GetStyleRange(Index, Start, Len) do begin
        	if RichMemo1.GetTextAttributes(Start, FP) then begin
                // Sadely, we need to do it this way, Tomboy cannot handle overlapping tags.
                // When Bold Turns OFF
                if (Bld and (not (fsBold in FP.Style))) then begin
                    if FSize <> FontNormal then
                        Buff := Buff + SetFontXML(FSize, false);
                    if Hilight then Buff := Buff + '</highlight>';
                    if Italics then Buff := Buff + '</italic>';
                    Buff := Buff + '</bold>';
                    if Italics then Buff := Buff + '<italic>';
                    if Hilight then Buff := Buff + '<highlight>';
                    if FSize <> FontNormal then
                        Buff := Buff + SetFontXML(FSize, true);
                    Bld := false;
                end;
                // When Italic turns OFF
                if (Italics and (not (fsItalic in FP.Style))) then begin
                    if FSize <> FontNormal then
                         Buff := Buff + SetFontXML(FSize, false);
                    if Hilight then Buff := Buff + '</highlight>';
                 	Buff := Buff + '</italic>';
                    if Hilight then Buff := Buff + '<highlight>';
                    if FSize <> FontNormal then
                        Buff := Buff + SetFontXML(FSize, true);
                    Italics := false;
                end;
                // When Highlight turns OFF
                if (HiLight and (not (FP.BkColor = clYellow))) then begin
                    if FSize <> FontNormal then
                        Buff := Buff + SetFontXML(FSize, false);
                    Buff := Buff + '</highlight>';
                    if FSize <> FontNormal then
                        Buff := Buff + SetFontXML(FSize, true);
                    HiLight := false;
                end;
                // When Font size changes
                if FSize <> FP.Size then begin
                    Buff := Buff + SetFontXML(FSize, false);
                    FSize := FP.Size;
                    Buff := Buff + SetFontXML(FSize, true);
                end;
                // Highlight
                if ((not HiLight) and (FP.BkColor = clYellow)) then begin
                    if FSize <> FontNormal then
                         Buff := Buff + SetFontXML(FSize, false);
                    Buff := Buff + '<highlight>';
                    if FSize <> FontNormal then
                         Buff := Buff + SetFontXML(FSize, true);
                    HiLight := true;
                end;
                // Italic turns On
                if ((not Italics) and (fsItalic in FP.Style)) then begin
                    if Hilight then Buff := Buff + '</highlight>';
                    if FSize <> FontNormal then
                    	Buff := Buff + SetFontXML(FSize, false);
                    Buff := Buff + '<italic>';
                    if FSize <> FontNormal then
                         Buff := Buff + SetFontXML(FSize, true);
                    if Hilight then Buff := Buff + '<highlight>';
                    Italics := true;
                end;
                // Bold turns On
                if ((not Bld) and (fsBold in FP.Style)) then begin
                    if Italics then Buff := Buff + '</italic>';
                    if Hilight then Buff := Buff + '</highlight>';
                    if FSize <> FontNormal then
                         Buff := Buff + SetFontXML(FSize, false);
                    Buff := Buff + '<bold>';
                    if FSize <> FontNormal then
                         Buff := Buff + SetFontXML(FSize, true);
                     if Hilight then Buff := Buff + '<highlight>';
                     if Italics then Buff := Buff + '<italic>';
                    Bld := true;
                end;
             	Buff := Buff + RichMemo1.GetText(Start, Len);
             	OutStream.Write(Buff[1], length(Buff));
             	// Memo1.Append(Buff);
             	Buff := '';
         	end;
         	Index := Index + Len;
         end;
         { TODO 1 : Before writing footer, need to check that there are no open tags.  }
         if Bld then Buff := Buff + '</bold>';
         if Italic then Buff := Buff + '</italic>';
         if HighLight then Buff := Buff + '</highlight>';
         OutStream.Write(Buff[1], length(Buff));
         Buff := Footer();
         OutStream.Write(Buff[1], length(Buff));
      finally
        OutStream.Free;
      end;
end;


Function TEditBoxForm.GetLocalTime():string;
var
   ThisMoment : TDateTime;
begin
    {$ifdef LINUX}
    ReReadLocalTime();    // in case we are near daylight saving time changeover
    {$endif}
    ThisMoment:=Now;
    Result := FormatDateTime('YYYY-MM-DD',ThisMoment) + 'T'
                   + FormatDateTime('hh:mm:ss.z',ThisMoment) + '0000+'
                   + inttostr(GetLocalTimeOffset() div -60);
end;

Function TEditBoxForm.Header(var Index : longint) : ANSIstring;
var
   S1, S2, S3, S4 : ANSIString;
begin
  S1 := '<?xml version="1.0" encoding="utf-8"?>'#10'<note version="0.3" xmlns:link="';
  S2 := 'http://beatniksoftware.com/tomboy/link" xmlns:size="http://beatniksoftware.com/tomboy/size"';
  S3 := ' xmlns="http://beatniksoftware.com/tomboy">'#10'  <title>';
  S4 := '</title>'#10'  <text xml:space="preserve"><note-content version="0.1">';
  Result := S1 + S2 + S3 + RichMemo1.Lines[0] + S4 + RichMemo1.Lines[0];
  Index := Index + length(RichMemo1.Lines[0]);
end;

Function TEditBoxForm.Footer() : ANSIstring;
var
   S1, S2, S3, S4, S5 : string;
   TimeStamp : string;
begin
  TimeStamp := GetLocalTime();   // get actual time date in format like Tomboy's
  S1 := '</note-content></text>'#10'  <last-change-date>';
  S2 := '</last-change-date>'#10'  <last-metadata-change-date>';
  S3 := '</last-metadata-change-date>'#10'  <create-date>';
  S4 := '</create-date>'#10'  <cursor-position>1</cursor-position>'#10'  <selection-bound-position>1</selection-bound-position>'#10;
  S5 := '  <width>1000</width>'#10'  <height>626</height>'#10'  <x>0</x>'#10'  <y>0</y>'#10'  <open-on-startup>False</open-on-startup>'#10'</note>';
  Result := S1 + TimeStamp + S2 + TimeStamp + S3 + CreateDate + S4 + S5;
end;

end.

