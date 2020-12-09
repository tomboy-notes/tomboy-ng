unit TB_SDiff;
{    Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------

    A unit that can display differences between two similar notes.

    User can choose to use First (Remote) or Second (Local)

    // Use Remote, Yellow is mrYes, File1
    // Use Local, Aqua is mrNo, File2
    // Always Use Local is mrNoToAll
    // Always Use Remote is mrYesToAll
    // Always use newest mrAll
    // Always use oldest mrClose
    // Anything else is DoNothing - no, do not permit donothing
}

{ History
    2018/08/14  Added to project
    2018/09/17  Changes to work with new sync model. We now just use the two
                file names in TClashRec and we get the last-change-dates our
                selves. Should be compatible with old sync model ....
    2018/10/16  Options to apply choice to all notes.
    2019/10/17  Trap exception if, for some reason, we cannot load one of the note files.
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ExtCtrls, ComCtrls, Buttons, kmemo, menus;
type

    { TFormSDiff }

    TFormSDiff = class(TForm)
        BitBtnUseRemote: TBitBtn;
        BitBtnUseLocal: TBitBtn;
        ButtAllOldest: TButton;
        ButtAllNewest: TButton;
        ButtAllLocal: TButton;
        ButtAllRemote: TButton;
        KMemo1: TKMemo;
        Label1: TLabel;
        LabelRemote: TLabel;
        LabelLocal: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Panel1: TPanel;
        RadioLong: TRadioButton;
        RadioShort: TRadioButton;
        procedure FormShow(Sender: TObject);
		procedure KMemo1Change(Sender: TObject);
        procedure RadioLongChange(Sender: TObject);
    private
        procedure AddDiffText(DiffText : string; NoteNo : integer = 0);
        procedure AddHeader(Title: string);
        {Returns 0 if no further sync works, 1 or 2 depending on which does sync
         eg 1 means you should go incrementing 1 until it looks like 2
         in other words, '1' means 1 can be matched. 2 must have been an insert. }
        function CanResync(const SL1, SL2: TStringList; const Spot1, Spot2, End1, End2: integer
            ): integer;
        procedure CheckFiles();
		function GetDateFromStr(const DateStr: ANSIString): TDateTime;
		function GetNoteChangeGMT(const FullFileName: ANSIString; out
				LastChange: ANSIString): TDateTime;
        procedure GotoEnd(const NoteNo : integer; const SL: TStringList; const Spot, TheEnd: integer);
        function RemoveXml(const St: AnsiString): AnsiString;
        // Returns a new (synced) Pos, showing intermediate lines.
        function Resync(const Target : string; NoteNo : integer; const SL : TStringList; var Spot : integer) : integer;
    public
        RemoteFileName : string;   // #1
        LocalFileName  : string;   // #2
        NoteTitle : string;

    end;

//var
//    FormSDiff: TFormSDiff;

implementation

{$R *.lfm}

uses LazLogger, laz2_DOM, laz2_XMLRead, LazFileUtils, DateUtils{, syncutils};

{ TFormSDiff }

function TFormSDiff.RemoveXml(const St : AnsiString) : AnsiString;
var
    X, Y : integer;
    FoundOne : boolean = false;
begin
    Result := St;
    repeat
        FoundOne := False;
        X := Pos('<', Result);      // don't use UTF8Pos for byte operations
        if X > 0 then begin
            Y := Pos('>', Result);
            if Y > 0 then begin
                Delete(Result, X, Y-X+1);
                FoundOne := True;
            end;
        end;
    until not FoundOne;
    Result := trim(Result);
end;

procedure TFormSDiff.AddDiffText(DiffText : string; NoteNo : integer = 0);
var
    TB, TBPre: TKMemoTextBlock;
begin
    // TB.TextStyle.Font.Size := 16;
    if NoteNo > 0 then begin
        TBpre := KMemo1.Blocks.AddTextBlock(inttostr(NoteNo) + '> ');
        //TBpre.TextStyle.Font.Style := TBpre.TextStyle.Font.Style + [fsBold];
        TBpre.TextStyle.Font.Style := [fsBold];
    end;
    if RadioShort.Checked then
        TB := KMemo1.Blocks.AddTextBlock(Copy(DiffText, 1, 50))
    else TB := KMemo1.Blocks.AddTextBlock(DiffText);
    if NoteNo = 1 then
        TB.TextStyle.Brush.Color := BitBtnUseRemote.Color;
    if NoteNo = 2 then
        TB.TextStyle.Brush.Color := BitBtnUseLocal.Color;
    KMemo1.blocks.AddParagraph();
end;

procedure TFormSDiff.AddHeader(Title : string);
var
    TB: TKMemoTextBlock;
begin
    KMemo1.Clear(False);
    TB := KMemo1.Blocks.AddTextBlock(Title);
    TB.TextStyle.Font.Size := 16;
    TB.TextStyle.Font.Style := [fsBold];
    KMemo1.blocks.AddParagraph();
end;

function TFormSDiff.CanResync(const SL1, SL2 : TStringList; const Spot1, Spot2, End1, End2 : integer) : integer;
var
    Offset : integer;
begin
    Result := 0;
    for Offset := Spot1 to End1 do
        if RemoveXML(SL1[Offset]) = RemoveXML(SL2[Spot2]) then exit(1);
    for Offset := Spot2 to End2 do
        if RemoveXML(SL2[Offset]) = RemoveXML(SL1[Spot1]) then exit(2);
end;

// Returns a new (synced) Pos, showing intermediate lines.
function TFormSDiff.Resync(const Target : string; NoteNo : integer; const SL : TStringList; var Spot : integer) : integer;
begin
    Result := Spot;
    while RemoveXML(Target) <> RemoveXML(SL[Result]) do begin
        AddDiffText(RemoveXML(SL[Result]), NoteNo);
        inc(Result);
    end;
end;

procedure TFormSDiff.GotoEnd(const NoteNo : integer; const SL : TStringList; const Spot, TheEnd : integer);
var
    I : integer;
begin
    for I := Spot to TheEnd - 1 do
        AddDiffText(RemoveXML(SL[I]), NoteNo);
end;

procedure TFormSDiff.FormShow(Sender: TObject);
var
    TestDate: TDateTime;
    LastChange : string;
begin
    //{$ifdef LINUX}      // Linux has whole button coloured  - not all !
    BitBtnUseRemote.GlyphShowMode := gsmNever;    // these are 20x36 coloured glyphs
    BitBtnUseRemote.GlyphShowMode := gsmNever;    // defined in menus
    //{$endif}
    // Go and get Title and last-change-date from both versions of note
    TestDate := GetNoteChangeGMT(LocalFileName, LastChange);
    if (TestDate > now()) or (TestDate < (Now() - 36500))  then
        // TDateTime has integer part no. of days, fraction part is fraction of day.
        // we have here in the future or more than 100years ago - Fail !

        // +++++++++++++++++++++++++++++++++++++++++++++++++
        // ToDo : this is wrong, see how to do it in sync
        // +++++++++++++++++++++++++++++++++++++++++++++++++

        Showmessage('Invalid last sync date in local version of note')
    else  LabelLocal.Caption := LastChange;
    GetNoteChangeGMT(RemoteFileName, LastChange);
    if (TestDate > now()) or (TestDate < (Now() - 36500))  then
        Showmessage('Invalid last sync date in remote version of note')
    else LabelRemote.Caption := LastChange;
    CheckFiles();
end;

procedure TFormSDiff.KMemo1Change(Sender: TObject);
begin

end;

function TFormSDiff.GetNoteChangeGMT(const FullFileName : ANSIString; out LastChange : ANSIString) : TDateTime;
var
        Doc : TXMLDocument;
        Node : TDOMNode;
begin
    if not FileExistsUTF8(FullFileName) then begin
        DebugLn('ERROR - File not found, cant read note change date for ',  FullFileName);
        Result := 0.0;
        exit();
    end;
    try
        ReadXMLFile(Doc, FullFileName);
        Node := Doc.DocumentElement.FindNode('last-change-date');
        LastChange := Node.FirstChild.NodeValue;
        finally
            Doc.free;               // xml errors are caught in calling process
        end;
    Result := GetDateFromStr(LastChange);
end;

function TFormSDiff.GetDateFromStr(const DateStr: ANSIString): TDateTime;
var
    TimeZone : TDateTime;
begin
    try
        if not TryEncodeTimeInterval(strtoint(copy(DateStr, 29, 2)),    // Hour
                 strtoint(copy(DateStr, 32, 2)),                        // Minutes
                 0,                                                     // Seconds
                 0,                                                     // mSeconds
                 TimeZone)  then DebugLn('Fail on interval encode ');
    except on EConvertError do begin
            DebugLn('FAIL on converting time interval ' + DateStr);
            DebugLn('Hour ', copy(DateStr, 29, 2), ' minutes ', copy(DateStr, 32, 2));
        end;
    end;
    try
        if not TryEncodeDateTime(strtoint(copy(DateStr, 1, 4)),         // Year
                strtoint(copy(DateStr, 6, 2)),                          // Month
                strtoint(copy(DateStr, 9, 2)),                          // Day
                strtoint(copy(DateStr, 12, 2)),                         // Hour
                strtoint(copy(DateStr, 15, 2)),                         // Minutes
                strtoint(copy(DateStr, 18, 2)),                         // Seconds
                strtoint(copy(DateStr, 21, 3)),                         // mSeconds
                Result)  then DebugLn('Fail on date time encode ');
    except on EConvertError do begin
            DebugLn('FAIL on converting date time ' + DateStr);
        end;
    end;
    try
        if DateStr[28] = '+' then Result := Result - TimeZone
        else if DateStr[28] = '-' then Result := Result + TimeZone
        else debugLn('******* Bugger, we are not parsing DATE String - Please Report ********');
     except on EConvertError do begin
            DebugLn('FAIL on calculating GMT ' + DateStr);
        end;
     end;
     { debugln('Date is ', DatetoStr(Result), ' ', TimetoStr(Result));  }
end;

procedure TFormSDiff.RadioLongChange(Sender: TObject);
begin
    CheckFiles();
end;

const LinesXML = 16;     // bit arbitary, seems notes have about 16 lines of XML header and footer

procedure TFormSDiff.CheckFiles();
var
    SL1, SL2 : TStringList;
    Pos1, Pos2, Offset1, Offset2, End1, End2 : integer;
    Sync : Integer;
begin
    Pos1 := 0; Pos2 := 0; Offset2 := 0; Offset1 := 0;
    SL1 := TStringList.create;                                 // This may generate a EFOpenError !!
    SL2 := TStringList.create;                                 // This may generate a EFOpenError !!
    try
        // open the two files and find their beginnings and ends of content
        try
            SL1.LoadFromFile(RemoteFileName);
        except on E: EFOpenError do debugln('Unable to find remote file in repo ' + RemoteFileName);
        end;
        try
            SL2.LoadFromFile(LocalFileName);
        except on E: EFOpenError do debugln('Unable to find local file ' + LocalFileName);
        end;
        AddHeader(NoteTitle);
        AddDiffText('Remote File ' + inttostr(SL1.Count-LinesXML) + ' lines ', 1);
        AddDiffText('Local File ' + inttostr(SL2.Count-LinesXML) + ' lines ', 2);
        while (Pos1 < SL1.Count) and (0 = pos('<note-content version', SL1[Pos1])) do
            inc(Pos1);
        while (Pos2 < SL2.Count) and (0 = pos('<note-content version', SL2[Pos2])) do
            inc(Pos2);
        //if Pos1 = SL1.Count then exit;    // no content found ?
        End1 := Pos1;
        End2 := Pos2;
        while (End1 < SL1.Count) and (0 = pos('</note-content', SL1[End1])) do
            inc(End1);
        while (End2 < SL2.Count) and (0 = pos('</note-content', SL2[End2])) do
            inc(End2);

        while ((Pos1+Offset1 < End1) and (Pos2+Offset2 < End2)) do begin
            if RemoveXML(SL1[Pos1+Offset1]) = RemoveXML(SL2[Pos2+Offset2]) then begin
                inc(Pos1); inc(pos2);
            end else begin
                AddDiffText('---- Out of Sync ----');
                AddDiffText(RemoveXML(SL2[Pos2+Offset2]), 2);
                AddDiffText(RemoveXML(SL1[Pos1+Offset1]), 1);
                inc(Pos1); inc(pos2);
                Sync := CanReSync(SL1, Sl2, Pos1, Pos2, End1, End2);
                case Sync of
                    0:  begin
                            GotoEnd(1, SL1, Pos1, End1);
                            Gotoend(2, SL2, Pos2, End2);
                            break;
                        end;
                    1:  begin
                            Pos1 := Resync(SL2[Pos2], 1, SL1, Pos1);
                        end;
                    2:  begin
                            Pos2 := Resync(SL1[Pos1], 2, SL2, Pos2);
                        end;
                end;
            end;
        end;
    finally
      Sl1.Free;
      Sl2.Free;
    end;
end;

end.

