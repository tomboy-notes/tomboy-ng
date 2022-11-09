unit recover;
{    Copyright (C) 2017-2022 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------

}

{   History
    2018/08/27  Now take config and local (sync) manifest with a snpshot, restore
                on the main 'Restore' tab.
    2018/10/28  Much changes, now working reasonably well.
    2018/10/29  Set attributes of Unzipped files on the Mac, it apparently leaves then 000
    2018/11/05  Altered name of safety zip file
    2019/05/19  Display strings all (?) moved to resourcestrings
    2019/12/18  Allow user to move stringgrid colums and pin its bottom to form.
    2020/05/19  Avoid var out of for loop problem in ButtonDeleteBadNotesClick() and TabSheetExistingShow()
    2020/06/11  Ensure snapshot dir exists .....
    2020/06/11  Really Ensure snapshot dir exists, rename SnapDir FullSnapDir
    2020/07/16  Extensive work to improve 'UI' sanity.
    2020/07/16  cleanup unused constants
    2020/08/21  Improve windows dark theme colours.
    2022/11/09  Fixed a fail to delete Bad Note, day one bug ?
}


{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ComCtrls, ExtCtrls, Grids, zipper{, Types};

type

    { TFormRecover }

    TFormRecover = class(TForm)
        ButtonRecoverSnap: TButton;
        ButtonSnapHelp: TButton;
        ButtonDeleteBadNotes: TButton;
        ButtonMakeSafetySnap: TButton;
        Label1: TLabel;
        Label10: TLabel;
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
        Label9: TLabel;
        ListBoxSnapshots: TListBox;
        PageControl1: TPageControl;
        Panel1: TPanel;
        PanelSnapshots: TPanel;
        PanelNoteList: TPanel;
        StringGridNotes: TStringGrid;
        TabSheetMergeSnapshot: TTabSheet;
        TabSheetRecoverSnapshot: TTabSheet;
        TabSheetRecoverNotes: TTabSheet;
        TabSheetIntro: TTabSheet;
        TabSheetBadNotes: TTabSheet;
        //procedure Button4Click(Sender: TObject);
        procedure ButtonMakeSafetySnapClick(Sender: TObject);
        procedure ButtonDeleteBadNotesClick(Sender: TObject);
        procedure ButtonRecoverSnapClick(Sender: TObject);
        procedure ButtonSnapHelpClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure ListBoxSnapshotsClick(Sender: TObject);
        procedure ListBoxSnapshotsDblClick(Sender: TObject);
        procedure StringGridNotesDblClick(Sender: TObject);
        procedure TabSheetBadNotesShow(Sender: TObject);
        procedure TabSheetIntroShow(Sender: TObject);
        procedure TabSheetMergeSnapshotShow(Sender: TObject);
        procedure TabSheetRecoverSnapshotShow(Sender: TObject);
        procedure TabSheetRecoverNotesShow(Sender: TObject);
    private
        procedure CleanAndUnzip(const FullDestDir, FullZipName: string);
        function ExpandZipName(AFileName: string): string;
        function FindSnapFiles(): integer;
        procedure RestoreSnapshot(const Snapshot: string);
        //procedure ScaleGridNotes();
        procedure ShowNotes(const FullSnapName: string);
        function ZipDate(): string;
        procedure CreateSnapshot(const FullSourceDir, FullZipName: string);
    public
            // Note that, at present, this debugmode is not set automatically anywhere.
        DebugMode : boolean;
        RequiresIndex : boolean;
        FullSnapDir, NoteDir, ConfigDir : string;
        // Creates a snapshot, returning its full name.
        function CreateSnapshot(const Manual: boolean): string;
        procedure CleanUpSnapshots(const MaxSnaps : integer);
    end;

var
    FormRecover: TFormRecover;

implementation

{$R *.lfm}

{ TFormRecover }

uses LazFileUtils, Note_Lister, SearchUnit, process, LazLogger,
    {$ifdef DARWIN}baseunix,{$endif}            // for fpChmod
    MainUnit,                                   // just for MainUnit.MainForm.ShowHelpNote(
    ResourceStr;

var
    SnapNoteLister : TNoteLister;



procedure TFormRecover.FormShow(Sender: TObject);
begin
    RequiresIndex := False;
    StringGridNotes.ColCount:=4;
    StringGridNotes.FixedCols:=0;
    //StringGridNotes.Options := [StringGridNotes.options] + [goThumbTracking];
    // Label1.Caption := rsWeHaveSnapShots_1 + ' ' + inttostr(FindSnapFiles()) + ' ' + rsWeHaveSnapShots_2;
    Label1.Caption := format(rsWeHaveSnapShots, [FindSnapFiles()]);
end;

procedure TFormRecover.FormCreate(Sender: TObject);
begin
    PageControl1.ActivePageIndex:=0;
    ButtonRecoverSnap.Enabled := False;
    Left := (Screen.Width  - Width)  div 2;
    Top  := (Screen.Height - height) div 2;
end;

procedure TFormRecover.FormDestroy(Sender: TObject);
begin
    //If SnapNoteLister <> Nil then
        FreeAndNil(SnapNoteLister);
end;

procedure TFormRecover.ButtonDeleteBadNotesClick(Sender: TObject);
var
    I : integer = 1;
    Cnt : integer = 0;
begin
    for I := 1 to StringGridNotes.RowCount-1 do begin     // includes header
        showmessage('Delete ' + StringGridNotes.Cells[0, I]);
        DeleteFile(NoteDir + StringGridNotes.Cells[0, I]);
        Debugln('TFormRecover.ButtonDeleteBadNotesClick  - ' + NoteDir + StringGridNotes.Cells[0, I]);
        inc(Cnt);
    end;
    //showmessage(rsDeletedDamaged_1 + ' ' + inttostr(CNT) + ' ' + rsDeletedDamaged_2 );
    showmessage(format(rsDeletedDamaged, [CNT]));
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
begin
    // MainUnit.MainForm.ShowHelpNote('recover.note');
    SearchForm.ShowHelpNote('recover.note');
end;


procedure TFormRecover.ButtonMakeSafetySnapClick(Sender: TObject);
begin
    CreateSnapshot(True);
    //CreateSnapShot(NoteDir, FullSnapDir + 'Safety.zip');          // abandonded idea of safety snapshot, too complicated
    //Label1.Caption := rsWeHaveSnapShots_1 + ' ' + inttostr(FindSnapFiles()) + ' ' + rsWeHaveSnapShots_2;
    Label1.Caption := format(rsWeHaveSnapShots, [FindSnapFiles()]);
end;

procedure TFormRecover.RestoreSnapshot(const Snapshot : string);
begin
    {if mrYes <> QuestionDlg(rsDeleteAndReplace_1, rsDeleteAndReplace_2 + ' ' + NoteDir
            + ' '  + rsDeleteAndReplace_3 + ' '
            + FormatDateTime( 'yyyy-mm-dd hh:mm', FileDateToDateTime(FileAge(FullSnapDir + Snapshot))) + ' ?'
            // + Snapshot + ' ' + DateTimeToStr(FileDateToDateTime(FileAge(FullSnapDir + Snapshot))) + ' ?'
            , mtConfirmation, [mrYes, mrNo], 0) then exit;  }

    if mrYes <> QuestionDlg(rsDeleteAndReplace_1, format(rsDeleteAndReplace_2
            , [NoteDir, FormatDateTime( 'yyyy-mm-dd hh:mm', FileDateToDateTime(FileAge(FullSnapDir + Snapshot)))])
            ,  mtConfirmation, [mrYes, mrNo], 0) then exit;

    CleanAndUnzip(NoteDir, FullSnapDir + Snapshot);
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

//RESOURCESTRING
//  rsNoSafetySnapshot = 'A Safety snapshot not found. Try setting Snapshot Dir to where you may have one.';

{procedure TFormRecover.Button4Click(Sender: TObject);
begin
    if fileexists(FullSnapDir + 'Safety.zip') then
        RestoreSnapshot('Safety.zip')
    else showmessage(rsNoSafetySnapshot);
end;}

procedure TFormRecover.StringGridNotesDblClick(Sender: TObject);
var
    NName : string;
begin
    case PageControl1.ActivePageIndex of
        {0,} 1 :  begin
                    try
                        NName := StringGridNotes.Cells[0, StringGridNotes.Row];
                    except on EGridException do exit;
                    end;
                    if length(NName) <  9 then exit;            // empty returns ID.note from col(0) title
                    // showmessage('We will open [' + NName + ']');
                    MainUnit.MainForm.SingleNoteMode(NoteDir + NName, False, False);
                end;
        2, 3, 4 : begin
                    try
                        NName := StringGridNotes.Cells[3, StringGridNotes.Row];
                    except on EGridException do exit;                   // clicked outside valid area
                    end;
                    if length(NName) < 9 then exit;
                    // showmessage('We will open ' + FullSnapDir + 'temp' + PathDelim + NName);
                    MainUnit.MainForm.SingleNoteMode(FullSnapDir + 'temp' + PathDelim + NName, False, True);
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
    // 20200714_2004_Auto.zip
    FName := ExtractFileName(AFileName);
    if FName = 'Safety.zip' then
        Result := 'from Intro Tab'
    else begin
        Result := copy(FName, 1, 4) + '-' + copy(FName, 5, 2) + '-' + copy(FName, 7, 2);   // year Month day
        Result := Result + ' ' + copy(FName, 10, 2) + ':' + copy(FName, 12, 2);             // hour minutes
        Result := Result + ' ' + copy(FName, 15, 4);                                        //
    end;
end;

// Unzips indicated snapshot, indexes its files and lists them in the StringGridNotes
procedure TFormRecover.ShowNotes(const FullSnapName : string);
begin
    PanelNoteList.Caption:=rsNotesInSnap +' ' + ExpandZipName(FullSnapName);
    ForceDirectory(FullSnapDir + 'temp');
    CleanAndUnZip(FullSnapDir + 'temp' + PathDelim, FullSnapName);
    if SnapNoteLister <> Nil then
        FreeAndNil(SnapNoteLister);
    SnapNoteLister := TNoteLister.Create;
    SnapNoteLister.Debugmode := DebugMode;
    SnapNoteLister.WorkingDir:= FullSnapDir + 'temp' + PathDelim;
    {Result := }SnapNoteLister.IndexNotes();
	SnapNoteLister.LoadStGrid(StringGridNotes, 4);          // this must be a TStringGrid 'cos it can show very long lines such as xml errors

    StringGridNotes.Cells[0, 0] := 'Title';
    StringGridNotes.Cells[1, 0] := 'Date';
    StringGridNotes.Cells[2, 0] := 'Create';
    StringGridNotes.Cells[3, 0] := 'Filename';

    //StringGridNotes.Row[0] := ['Title', 'Date', 'Create', 'File'];
    // StringGridNotes.InsertRowWithValues(0, ['Title', 'Date', 'Create', 'File']);
    //StringGridNotes.SortOrder := soDescending;              // Sort with most recent at top
	//StringGridNotes.SortColRow(True, 1);

    stringGridNotes.AutoSizeColumns;
    ButtonRecoverSnap.Enabled := True;
end;

function TFormRecover.ZipDate({WithDay : Boolean}) : string;
var
   ThisMoment : TDateTime;
begin
    ThisMoment:=Now;
    Result := FormatDateTime('YYYYMMDD',ThisMoment) + '_'
        + FormatDateTime('hhmm',ThisMoment);
    //if WithDay then Result := Result + '_' + FormatDateTime('ddd', ThisMoment);
end;

function TFormRecover.CreateSnapshot(const Manual : boolean) : string;
var
   ZipName : string;
begin
    if Manual then ZipName := ZipDate() + '_Man'
    else ZipName := ZipDate() + '_Auto';
    if not DirectoryExists(FullSnapDir) then begin
        createDir(FullSnapDir);
        if not DirectoryExists(FullSnapDir) then begin
            Showmessage('Cannot create ' + FullSnapDir);
            exit('');
        end;
    end;
    CreateSnapshot(NoteDir, FullSnapDir + ZipName + '.zip');
    result := FullSnapDir + ZipName + '.zip';
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

                                { Removes any more than MaxSnaps Auto generated from the
                                  snaps dir. Does not play with Manually generated ones. }
procedure TFormRecover.CleanUpSnapshots(const MaxSnaps: integer);
var
     Snaps : TStringList;
     ToRemoveFromList : integer;
     St : string;
begin
    Snaps := FindAllFiles(FullSnapDir, '*_Auto.zip', false); // list contains full file names !
    try
        // debugln('RECOVER - CleanUpSnapshots() we have numb snapshots = ' + dbgs(Snaps.Count));
        Snaps.Sort;
        ToRemoveFromList := MaxSnaps;
        while (ToRemoveFromList > 0) and (Snaps.Count <> 0) do begin
            Snaps.Delete(Snaps.Count-1);
            dec(ToRemoveFromList);
        end;
        for St in Snaps do begin
            // debugln('Deleting snap item ' + St);
            DeleteFile(St);
        end;
    finally
        freeandnil(Snaps);
    end;
end;

procedure TFormRecover.ListBoxSnapshotsDblClick(Sender: TObject);
begin
    if (ListBoxSnapshots.ItemIndex >= 0) and (ListBoxSnapshots.ItemIndex < ListBoxSnapshots.Count) then begin
        ShowNotes(FullSnapDir + ListBoxSnapshots.Items[ListBoxSnapshots.ItemIndex]);
    end;
end;

procedure TFormRecover.ListBoxSnapshotsClick(Sender: TObject);
begin
    if (ListBoxSnapshots.ItemIndex >= 0) and (ListBoxSnapshots.ItemIndex < ListBoxSnapshots.Count) then begin
        ShowNotes(FullSnapDir + ListBoxSnapshots.Items[ListBoxSnapshots.ItemIndex]);
    end;
end;


procedure TFormRecover.TabSheetBadNotesShow(Sender: TObject);
var
  I, Comma : integer;
  Msg : string;
begin
    //showmessage('Existing Show');
    StringGridNotes.Visible := True;
    StringGridNotes.Enabled := True;
    with StringGridNotes do while RowCount > 1 do DeleteRow(RowCount-1);
    ButtonDeleteBadNotes.Enabled := False;
    ListBoxSnapshots.ItemIndex:= -1;
    ListBoxSnapShots.Enabled:=False;
    PanelNoteList.Caption:=rsClickBadNote;

    // LabelNoteErrors.Caption := rsBadNotes_1 + ' ' + inttostr(SearchForm.NoteLister.ErrorNotes.Count) + ' ' + rsBadNotes_2;

    LabelNoteErrors.Caption := format(rsBadNotes, [TheMainNoteLister.ErrorNotes.Count]);

    LabelExistingAdvice2.Caption := '';
    LabelExistingAdvice.Caption := '';
    if TheMainNoteLister.ErrorNotes.Count <> 0  then
      begin
        LabelExistingAdvice.Caption := rsTryRecover_1;
        LabelExistingAdvice2.Caption := rsTryrecover_2;
    end;
    StringGridNotes.Clear;
    StringGridNotes.FixedRows := 0;
    StringGridNotes.InsertRowWithValues(0, ['ID', 'ErrorMessage']);
    StringGridNotes.FixedRows := 1;
    for I := 0 to TheMainNoteLister.ErrorNotes.Count -1 do begin
        Msg := TheMainNoteLister.ErrorNotes.Strings[I];
        Comma := pos(',', Msg);
        StringGridNotes.InsertRowWithValues(I + 1, [copy(Msg, 1, Comma-1), copy(Msg, Comma+1, 200)]);
        //   copy(Msg, 1, Comma-1) = simple file name
        //   copy(Msg, Comma+1, 200) = error messages, may be quite long.
    end;
    StringGridNotes.AutoSizeColumns;
    if {I} TheMainNoteLister.ErrorNotes.Count > 0 then ButtonDeleteBadNotes.Enabled:= True;
end;

procedure TFormRecover.TabSheetIntroShow(Sender: TObject);
begin
    ListBoxSnapShots.Enabled := False;
    StringGridNotes.Visible := false;
    ListBoxSnapshots.ItemIndex:= -1;
end;

procedure TFormRecover.TabSheetMergeSnapshotShow(Sender: TObject);
begin
    ListBoxSnapShots.Enabled:=True;
    // this tab is not used, probably will not ever be. But it has a count so be careful removing it
end;

procedure TFormRecover.TabSheetRecoverSnapshotShow(Sender: TObject);
begin
    ListBoxSnapShots.Enabled:=True;
    ListBoxSnapshots.ItemIndex:= -1;
    with StringGridNotes do while RowCount > 1 do DeleteRow(RowCount-1);
    PanelNoteList.Caption:= rsClickSnapShot;
    StringGridNotes.Visible := True;
    StringGridNotes.Enabled := True;
    ButtonRecoverSnap.Enabled := (ListBoxSnapshots.ItemIndex >= 0)
                and (ListBoxSnapshots.ItemIndex < ListBoxSnapshots.Count);
end;


procedure TFormRecover.TabSheetRecoverNotesShow(Sender: TObject);
begin
    with StringGridNotes do while RowCount > 1 do DeleteRow(RowCount-1);
    StringGridNotes.Visible := True;
    StringGridNotes.Enabled := True;
    PanelNoteList.Caption:=rsClickSnapShot;
    ListBoxSnapShots.Enabled:=True;
    ListBoxSnapshots.ItemIndex:= -1;
end;

function TFormRecover.FindSnapFiles() : integer;
var
    Info : TSearchRec;
begin
    ListBoxSnapshots.Clear;
    Result := 0;
	if FindFirst(FullSnapDir + '*.zip', faAnyFile and faDirectory, Info)=0 then begin
		repeat
          ListBoxSnapshots.AddItem(Info.Name, nil);
          inc(Result);
	    until FindNext(Info) <> 0;
	end;
    FindClose(Info);
end;

end.

