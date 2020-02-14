unit recover;
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

{   History
    2018/08/27  Now take config and local (sync) manifest with a snpshot, restore
                on the main 'Restore' tab.
    2018/10/28  Much changes, now working reasonably well.
    2018/10/29  Set attributes of Unzipped files on the Mac, it apparently leaves then 000
    2018/11/05  Altered name of safety zip file
    2019/05/19  Display strings all (?) moved to resourcestrings
    2019/12/18  Allow user to move stringgrid colums and pin its bottom to form.
}


{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ComCtrls, ExtCtrls, Grids, zipper, Types;

type

    { TFormRecover }

    TFormRecover = class(TForm)
        ButtonRecoverSnap: TButton;
        ButtonSnapHelp: TButton;
        ButtonDeleteBadNotes: TButton;
        Button4: TButton;
        ButtonMakeSafetySnap: TButton;
        Label1: TLabel;
        Label10: TLabel;
        Label11: TLabel;
        Label12: TLabel;
        Label14: TLabel;
        Label15: TLabel;
        Label16: TLabel;
        LabelExistingAdvice2: TLabel;
        LabelExistingAdvice: TLabel;
        LabelNoteErrors: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        Label7: TLabel;
        Label8: TLabel;
        Label9: TLabel;
        ListBoxSnapshots: TListBox;
        PageControl1: TPageControl;
        Panel1: TPanel;
        PanelSnapshots: TPanel;
        PanelNoteList: TPanel;
        StringGrid1: TStringGrid;
        TabSheetMergeSnapshot: TTabSheet;
        TabSheetRecoverSnapshot: TTabSheet;
        TabSheetSnapshots: TTabSheet;
        TabSheetIntro: TTabSheet;
        TabSheetExisting: TTabSheet;
        procedure Button4Click(Sender: TObject);
        procedure ButtonMakeSafetySnapClick(Sender: TObject);
        procedure ButtonDeleteBadNotesClick(Sender: TObject);
        procedure ButtonRecoverSnapClick(Sender: TObject);
        procedure ButtonSnapHelpClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure ListBoxSnapshotsDblClick(Sender: TObject);
        procedure StringGrid1DblClick(Sender: TObject);
        procedure TabSheetExistingShow(Sender: TObject);
        procedure TabSheetIntroContextPopup(Sender: TObject; MousePos: TPoint;
            var Handled: Boolean);
        procedure TabSheetIntroShow(Sender: TObject);
        procedure TabSheetMergeSnapshotShow(Sender: TObject);
        procedure TabSheetRecoverSnapshotShow(Sender: TObject);
        procedure TabSheetSnapshotsShow(Sender: TObject);
    private
        procedure CleanAndUnzip(const FullDestDir, FullZipName: string);
        function ExpandZipName(AFileName: string): string;
        function FindSnapFiles(): integer;
        procedure RestoreSnapshot(const Snapshot: string);
        procedure ShowNotes(const FullSnapName: string);
        function ZipDate(WithDay : Boolean): string;

    public
            // Note that, at present, this debugmode is not set automatically anywhere.
        DebugMode : boolean;
        RequiresIndex : boolean;
        SnapDir, NoteDir, ConfigDir : string;
        procedure CreateSnapshot(const FullSourceDir, FullZipName: string);
        // Creates a snapshot, returning its full name.
        function CreateSnapshot(const Manual, Monthly : boolean) : string;
    end;

var
    FormRecover: TFormRecover;

implementation

{$R *.lfm}

{ TFormRecover }

uses LazFileUtils, Note_Lister, SearchUnit, process, LazLogger,
    {$ifdef DARWIN}baseunix,{$endif}            // for fpChmod
    MainUnit;   // just for MainUnit.MainForm.ShowHelpNote(

var
    SnapNoteLister : TNoteLister;

RESOURCESTRING
  rsWeHaveSnapShots_1 = 'We have';
  rsWeHaveSnapShots_2 = 'snapshots';

procedure TFormRecover.FormShow(Sender: TObject);
begin
    RequiresIndex := False;
    StringGrid1.ColCount:=4;
    StringGrid1.FixedCols:=0;
    //stringgrid1.Options := [stringgrid1.options] + [goThumbTracking];
    Label1.Caption := rsWeHaveSnapShots_1 + ' ' + inttostr(FindSnapFiles()) + ' ' + rsWeHaveSnapShots_2;
end;

procedure TFormRecover.FormCreate(Sender: TObject);
begin
    PageControl1.ActivePageIndex:=0;
    ButtonRecoverSnap.Enabled := False;
end;

procedure TFormRecover.FormDestroy(Sender: TObject);
begin
    //If SnapNoteLister <> Nil then
        FreeAndNil(SnapNoteLister);
end;

RESOURCESTRING
  rsDeletedDamaged_1 = 'OK, deleted';
  rsDeletedDamaged_2 = 'damaged notes';

procedure TFormRecover.ButtonDeleteBadNotesClick(Sender: TObject);
var I : integer;
begin
    for I := 1 to StringGrid1.RowCount-1 do begin     // includes header
        showmessage('Delete ' + StringGrid1.Cells[0, I]);
        DeleteFile(NoteDir + StringGrid1.Cells[0, I] + '.note');
    end;
    showmessage(rsDeletedDamaged_1 + ' ' + inttostr(I) + ' ' + rsDeletedDamaged_2 );
end;

procedure TFormRecover.ButtonRecoverSnapClick(Sender: TObject);
{var
    ZName : string; }
begin
    if (ListBoxSnapshots.ItemIndex >= 0)
                and (ListBoxSnapshots.ItemIndex < ListBoxSnapshots.Count) then begin
        RestoreSnapshot(ListBoxSnapshots.Items[ListBoxSnapshots.ItemIndex]);
        //showmessage('I''d use [' + ZName + '] and put it all in [' + NoteDir);
    end;
end;

procedure TFormRecover.ButtonSnapHelpClick(Sender: TObject);
{var
    DocsDir : string;   // this probably belongs in Settings. }
begin
    MainUnit.MainForm.ShowHelpNote('recover.note');
    (*
    DocsDir := AppendPathDelim(ExtractFileDir(Application.ExeName));                     // UNTESTED
    {$ifdef LINUX}DocsDir := '/usr/share/doc/tomboy-ng/'; {$endif}
    {$ifdef DARWIN}
    DocsDir := ExtractFileDir(ExtractFileDir(Application.ExeName))+'/Resources/';
    //DocsDir := '/Applications/tomboy-ng.app/Contents/SharedSupport/';
    {$endif}  // untested
    // showmessage('About to open ' + DocsDir + 'recover.note');
    MainUnit.MainForm.SingleNoteMode(DocsDir + 'recover.note', False, True);
    *)
end;


procedure TFormRecover.ButtonMakeSafetySnapClick(Sender: TObject);
begin
    CreateSnapShot(NoteDir, SnapDir + 'Safety.zip');
    Label1.Caption := rsWeHaveSnapShots_1 + ' ' + inttostr(FindSnapFiles()) + ' ' + rsWeHaveSnapShots_2;
end;

RESOURCESTRING
  rsDeleteAndReplace_1 = 'Notes at risk !';
  rsDeleteAndReplace_2 = 'Delete all notes in';
  rsDeleteAndReplace_3 = 'and replace with snapshot dated';
  rsAllRestored = 'Notes and config files Restored, restart suggested.';

procedure TFormRecover.RestoreSnapshot(const Snapshot : string);
begin
    if mrYes <> QuestionDlg(rsDeleteAndReplace_1, rsDeleteAndReplace_2 + ' ' + NoteDir
            + ' '  + rsDeleteAndReplace_3 + ' '
            + FormatDateTime( 'yyyy-mm-dd hh:mm', FileDateToDateTime(FileAge(SnapDir + Snapshot))) + ' ?'
            // + Snapshot + ' ' + DateTimeToStr(FileDateToDateTime(FileAge(SnapDir + Snapshot))) + ' ?'
            , mtConfirmation, [mrYes, mrNo], 0) then exit;
    CleanAndUnzip(NoteDir, SnapDir + Snapshot);
    if FileExists(NoteDir + 'config' + PathDelim + 'tomboy-ng.cfg') then begin
        CopyFile(NoteDir + 'config' + PathDelim + 'tomboy-ng.cfg', ConfigDir + 'tomboy-ng.cfg');
        DeleteFile(NoteDir + 'config' + PathDelim + 'tomboy-ng.cfg');
        if FileExists(NoteDir + 'config' + PathDelim + 'manifest.xml') then begin
            CopyFile(NoteDir + 'config' + PathDelim + 'manifest.xml', ConfigDir + 'manifest.xml');
            DeleteFile(NoteDir + 'config' + PathDelim + 'manifest.xml');
        end;
        DeleteDirectory(NoteDir + 'config', False);
    end;
    showmessage(rsAllRestored);
    RequiresIndex := true;
end;

RESOURCESTRING
  rsNoSafetySnapshot = 'A Safety snapshot not found. Try setting Snapsot Dir to where you may have one.';

procedure TFormRecover.Button4Click(Sender: TObject);
begin
    if fileexists(SnapDir + 'Safety.zip') then
        RestoreSnapshot('Safety.zip')
    else showmessage(rsNoSafetySnapshot);
end;

procedure TFormRecover.StringGrid1DblClick(Sender: TObject);
var
    NName : string;
begin
    case PageControl1.ActivePageIndex of
        {0,} 1 :  begin
                    try
                        NName := StringGrid1.Cells[0, StringGrid1.Row];
                    except on EGridException do exit;
                    end;
                    if length(NName) <  9 then exit;            // empty returns ID.note from col(0) title
                    // showmessage('We will open [' + NName + ']');
                    MainUnit.MainForm.SingleNoteMode(NoteDir + NName, False, False);
                end;
        2, 3, 4 : begin
                    try
                        NName := StringGrid1.Cells[3, StringGrid1.Row];
                    except on EGridException do exit;                   // clicked outside valid area
                    end;
                    if length(NName) < 9 then exit;
                    // showmessage('We will open ' + SnapDir + 'temp' + PathDelim + NName);
                    MainUnit.MainForm.SingleNoteMode(SnapDir + 'temp' + PathDelim + NName, False, True);
                end;
    end;
end;

procedure TFormRecover.CleanAndUnzip(const FullDestDir, FullZipName : string);
var
  ZipFile: TUnZipper;
  Info : TSearchRec;
begin
    ForceDirectory(FullDestDir);
    if FindFirst(FullDestDir + '*.note', faAnyFile, Info)=0 then begin
        repeat
            if debugmode then Debugln('Deleting [' + FullDestDir + Info.Name + ']');
            DeleteFileUTF8(FullDestDir + Info.Name);                // should we test return value ?
	    until FindNext(Info) <> 0;
	end;
    FindClose(Info);
    if FileExists(FullDestDir + 'config' + PathDelim + 'manifest.xml') then
        DeleteFile(FullDestDir + 'config' + PathDelim + 'manifest.xml');
    if FileExists(FullDestDir + 'config' + PathDelim + 'tomboy-ng.cfg') then
        DeleteFile(FullDestDir + 'config' + PathDelim + 'tomboy-ng.cfg');
    ZipFile := TUnZipper.Create;
    try
        ZipFile.FileName := FullZipName;
        ZipFile.OutputPath := FullDestDir;
        ZipFile.Examine;
        ZipFile.UnZipAllFiles;
    finally
        ZipFile.Free;
    end;
    {$ifdef Darwin}                     // paszlib, on mac, leaves files with no permissions !
    if FindFirst(FullDestDir + '*.note', faAnyFile, Info)=0 then begin
        repeat
            fpChmod(FullDestDir + Info.Name, &644);    // uses baseunix, should we test return value ?
	    until FindNext(Info) <> 0;
	end;
    FindClose(Info);
    if FileExists(FullDestDir + 'config' + PathDelim + 'manifest.xml') then
        fpchmod(FullDestDir + 'config' + PathDelim + 'manifest.xml', &644);
    if FileExists(FullDestDir + 'config' + PathDelim + 'tomboy-ng.cfg') then
        fpchmod(FullDestDir + 'config' + PathDelim + 'tomboy-ng.cfg', &644);
    {$endif}
end;

function TFormRecover.ExpandZipName(AFileName : string) : string;
var
    FName : string;
begin
    // gets eg /somepath/20180826_2135_Sun.zip, 20180826_2135_Sun_Man.zip, 20180826_2135_Sun_Month.zip
    FName := ExtractFileName(AFileName);
    if FName = 'Safety.zip' then
        Result := 'from Intro Tab'
    else begin
        Result := copy(FName, 1, 4) + '-' + copy(FName, 5, 2) + '-' + copy(FName, 7, 2);
        Result := Result + ' ' + copy(FName, 10, 2) + ':' + copy(FName, 12, 2);
        Result := Result + ' ' + copy(FName, 15, 3);
    end;
end;

RESOURCESTRING
  rsNotesInSnap = 'Notes in Snapshot';

procedure TFormRecover.ShowNotes(const FullSnapName : string);
begin
    PanelNoteList.Caption:=rsNotesInSnap +' ' + ExpandZipName(FullSnapName);
    ForceDirectory(SnapDir + 'temp');
    CleanAndUnZip(SnapDir + 'temp' + PathDelim, FullSnapName);
    if SnapNoteLister <> Nil then
        FreeAndNil(SnapNoteLister);
    SnapNoteLister := TNoteLister.Create;
    SnapNoteLister.Debugmode := DebugMode;
    SnapNoteLister.WorkingDir:= SnapDir + 'temp' + PathDelim;
    {Result := }SnapNoteLister.GetNotes();
	SnapNoteLister.LoadStGrid(StringGrid1, 4);
	Stringgrid1.SortOrder := soDescending;    // Sort with most recent at top
	StringGrid1.SortColRow(True, 1);
    ButtonRecoverSnap.Enabled := True;
end;

function TFormRecover.ZipDate(WithDay : Boolean) : string;
var
   ThisMoment : TDateTime;
begin
    ThisMoment:=Now;
    Result := FormatDateTime('YYYYMMDD',ThisMoment) + '_'
        + FormatDateTime('hhmm',ThisMoment);
    if WithDay then Result := Result + '_' + FormatDateTime('ddd', ThisMoment);
end;

function TFormRecover.CreateSnapshot(const Manual, Monthly : boolean) : string;
var
   ZipName : string;
begin
    if not (Manual or Monthly) then ZipName := ZipDate(True)
    else ZipName := ZipDate(True) + '_';
    if Manual then ZipName := ZipName + 'Man';
    if Monthly then ZipName := ZipName + 'Month';       // both true is silly, assert !
    CreateSnapshot(NoteDir, SnapDir + ZipName + '.zip');
    result := SnapDir + ZipName + '.zip';
end;

procedure TFormRecover.CreateSnapshot(const FullSourceDir, FullZipName: string);
var
    Zip : TZipper;
    Info : TSearchRec;
    // Tick, Tock : QWord;
begin
    //debugln('--------- Config = ' + ConfigDir);
    Zip := TZipper.Create;
    try
        Zip.FileName := FullZipName;
        // Tick := GetTickCount64();
      	if FindFirst(FullSourceDir + '*.note', faAnyFile, Info)=0 then begin
      		repeat
                // debugln('Zipping note [' + FullSourceDir + Info.Name + ']');
                Zip.Entries.AddFileEntry(FullSourceDir + Info.Name, Info.Name);
      	    until FindNext(Info) <> 0;
            if FileExists(ConfigDir + 'tomboy-ng.cfg')
                then Zip.Entries.AddFileEntry(ConfigDir + 'tomboy-ng.cfg', 'config' + PathDelim + 'tomboy-ng.cfg')
            else Debugln('ERROR - cannot locate ' + ConfigDir + 'tomboy-ng.cfg');
            if FileExists(ConfigDir + 'manifest.xml')
                then Zip.Entries.AddFileEntry(ConfigDir + 'manifest.xml', 'config' + PathDelim + 'manifest.xml')
            else if DebugMode then debugln('NOTE : Local Manifest not found ' + ConfigDir + 'manifest.xml');
            Zip.ZipAllFiles;
      	end;
        //Tock := GetTickCount64(); // 150mS, 120 notes on lowend laptop
    finally
        FindClose(Info);
        Zip.Free;
    end;
    // debugln('All notes in ' + FullSourceDir + ' to ' + FullZipName + ' took ' + inttostr(Tock - Tick) + 'ms');
end;

procedure TFormRecover.ListBoxSnapshotsDblClick(Sender: TObject);
begin
    if (ListBoxSnapshots.ItemIndex >= 0) and (ListBoxSnapshots.ItemIndex < ListBoxSnapshots.Count) then begin
        ShowNotes(SnapDir + ListBoxSnapshots.Items[ListBoxSnapshots.ItemIndex]);
    end;
end;

RESOURCESTRING
  rsBadNotes_1 = 'You have';
  rsBadNotes_2 = ' bad notes in Notes Directory';
  rsClickBadNote = 'Double click on any Bad Notes';
  rsNoBadNotes = 'No errors, perhaps you should proceed to Snapshots';
  rsTryRecover_1 = 'Proceed to Snapshots or try to recover by double clicking below,';
  rsTryrecover_2 = 'if and only IF, you see useful content, make a small change, exit';

procedure TFormRecover.TabSheetExistingShow(Sender: TObject);
var
  I, Comma : integer;
  Msg : string;
begin
    //showmessage('Existing Show');
    ButtonDeleteBadNotes.Enabled := False;
    ListBoxSnapShots.Enabled:=False;
    PanelNoteList.Caption:=rsClickBadNote ;
    LabelNoteErrors.Caption := rsBadNotes_1 + ' ' + inttostr(SearchForm.NoteLister.ErrorNotes.Count)
        + ' ' + rsBadNotes_2;
  	StringGrid1.Clear;
    LabelExistingAdvice2.Caption := '';
    if SearchForm.NoteLister.ErrorNotes.Count = 0 then
        LabelExistingAdvice.Caption := rsNoBadNotes
    else begin
        LabelExistingAdvice.Caption := rsTryRecover_1;
        LabelExistingAdvice2.Caption := rsTryrecover_2;
    end;
    StringGrid1.FixedRows := 0;
    StringGrid1.InsertRowWithValues(0, ['ID', 'ErrorMessage']);
    StringGrid1.FixedRows := 1;
    for I := 0 to SearchForm.NoteLister.ErrorNotes.Count -1 do begin
        Msg := SearchForm.NoteLister.ErrorNotes.Strings[I];
        Comma := pos(',', Msg);
        StringGrid1.InsertRowWithValues(I + 1, [copy(Msg, 1, Comma-1), copy(Msg, Comma+1, 200)]);
    end;
    StringGrid1.AutoSizeColumns;
    if I > 0 then ButtonDeleteBadNotes.Enabled:= True;
end;

procedure TFormRecover.TabSheetIntroContextPopup(Sender: TObject;
    MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TFormRecover.TabSheetIntroShow(Sender: TObject);
begin
    ListBoxSnapShots.Enabled:=False;
end;

procedure TFormRecover.TabSheetMergeSnapshotShow(Sender: TObject);
begin
    ListBoxSnapShots.Enabled:=True;
end;

procedure TFormRecover.TabSheetRecoverSnapshotShow(Sender: TObject);
begin
    ListBoxSnapShots.Enabled:=True;
    ButtonRecoverSnap.Enabled := (ListBoxSnapshots.ItemIndex >= 0)
                and (ListBoxSnapshots.ItemIndex < ListBoxSnapshots.Count);
end;

RESOURCESTRING
  rsClickSnapShot = 'Double Click an Available Snapshot';

procedure TFormRecover.TabSheetSnapshotsShow(Sender: TObject);
begin
    PanelNoteList.Caption:=rsClickSnapShot;
    ListBoxSnapShots.Enabled:=True;
end;

function TFormRecover.FindSnapFiles() : integer;
var
    Info : TSearchRec;
begin
    ListBoxSnapshots.Clear;
    Result := 0;
	if FindFirst(SnapDir + '*.zip', faAnyFile and faDirectory, Info)=0 then begin
		repeat
          ListBoxSnapshots.AddItem(Info.Name, nil);
          inc(Result);
	    until FindNext(Info) <> 0;
	end;
    FindClose(Info);
end;

end.

