unit recover;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ComCtrls, ExtCtrls, Grids, zipper, Types;

type

    { TFormRecover }

    TFormRecover = class(TForm)
        ButtonDeleteBadNotes: TButton;
        Button4: TButton;
        ButtonMakeIntroSnap: TButton;
        Label1: TLabel;
        Label10: TLabel;
        Label11: TLabel;
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
        TabSheet1: TTabSheet;
        TabSheet2: TTabSheet;
        TabSheetSnapshots: TTabSheet;
        TabSheetIntro: TTabSheet;
        TabSheetExisting: TTabSheet;
        procedure Button4Click(Sender: TObject);
        procedure ButtonMakeIntroSnapClick(Sender: TObject);
        procedure ButtonDeleteBadNotesClick(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure ListBoxSnapshotsDblClick(Sender: TObject);
        procedure StringGrid1DblClick(Sender: TObject);
        procedure TabSheetExistingShow(Sender: TObject);
    private
        procedure CleanAndUnzip(const FullDestDir, FullZipName: string);
        function FindSnapFiles(): integer;
        procedure ShowNotes(const FullSnapName: string);

    public
        SnapDir, NoteDir : string;
    end;

var
    FormRecover: TFormRecover;

implementation

{$R *.lfm}

{ TFormRecover }

uses LazFileUtils, Note_Lister, SearchUnit, process, LazLogger;

var
    SnapNoteLister : TNoteLister;

procedure TFormRecover.FormShow(Sender: TObject);
begin
    PageControl1.ActivePageIndex:=0;
    StringGrid1.ColCount:=4;
    StringGrid1.FixedCols:=0;
    //stringgrid1.Options := [stringgrid1.options] + [goThumbTracking];
    Label1.Caption := 'We have ' + inttostr(FindSnapFiles()) + ' snapshots';
end;

procedure TFormRecover.FormDestroy(Sender: TObject);
begin
    //If SnapNoteLister <> Nil then
        FreeAndNil(SnapNoteLister);
end;

procedure TFormRecover.ButtonDeleteBadNotesClick(Sender: TObject);
var I : integer;
begin
        //showmessage(inttostr(StringGrid1.RowCount));
        //exit();
    for I := 1 to StringGrid1.RowCount-1 do begin     // includes header
        showmessage('Delete ' + StringGrid1.Cells[0, I]);
        // delete NotesDir + StringGrid1.Cells[0, I] + '.note'
    end;
end;

procedure TFormRecover.ButtonMakeIntroSnapClick(Sender: TObject);
var
    Zip : TZipper;
    Info : TSearchRec;
    Tick, Tock : DWord;
begin
    Zip := TZipper.Create;
    try
        Zip.FileName :=  SnapDir + 'Exist.zip';
        Tick := GetTickCount64();
      	if FindFirst(NoteDir + '*.note', faAnyFile and faDirectory, Info)=0 then begin
      		repeat
                //debugln('Zipping note [' + Info.Name + ']');
                Zip.Entries.AddFileEntry(NoteDir + Info.Name, Info.Name);
      	    until FindNext(Info) <> 0;
            Zip.ZipAllFiles;
      	end;
        Tock := GetTickCount64(); // 150mS, 120 notes on lowend laptop
    finally
        FindClose(Info);
        Zip.Free;
    end;
    showmessage( SnapDir + 'Exist.zip created');
    debugln('That all took ' + inttostr(Tock - Tick) + 'ms');
end;

procedure TFormRecover.Button4Click(Sender: TObject);
begin
    CleanAndUnzip(NoteDir, SnapDir + 'Exist.zip');
    showmessage('Notes Restored');
end;

procedure TFormRecover.StringGrid1DblClick(Sender: TObject);
var
    Dump : string;
begin
    //if RunCommand('/bin/bash',['-c','ldconfig -p | grep hunspell'], FullName)
    showmessage('will run ' + NoteDir + StringGrid1.Cells[0, StringGrid1.Row] + '.note');
    {$ifdef LINUX}
    if not RunCommand('tomboy-ng -o ' + NoteDir + StringGrid1.Cells[0, StringGrid1.Row] + '.note', Dump) then
        showmessage('cannot run ' + NoteDir + StringGrid1.Cells[0, StringGrid1.Row] + '.note');
    {$endif}
    {$ifdef WINDOWS}
    showmessage('sorry, not working yet on Windows');
    {$endif}
    {$ifdef DARWIN}
    showmessage('sorry, not working yet on Windows');
    {$endif}
end;

procedure TFormRecover.CleanAndUnzip(const FullDestDir, FullZipName : string);
var
  ZipFile: TUnZipper;
  Info : TSearchRec;
begin
    ForceDirectory(FullDestDir);
    if FindFirst(FullDestDir + '*.note', faAnyFile, Info)=0 then begin
        repeat
            Debugln('Deleting [' + FullDestDir + Info.Name + ']');
            DeleteFileUTF8(FullDestDir + Info.Name);
	    until FindNext(Info) <> 0;
	end;
    FindClose(Info);
    //exit();
    ZipFile := TUnZipper.Create;
    try
          ZipFile.FileName := FullZipName;
          ZipFile.OutputPath := FullDestDir;
          ZipFile.Examine;
          ZipFile.UnZipAllFiles;
    finally
        ZipFile.Free;
    end;
end;

procedure TFormRecover.ShowNotes(const FullSnapName : string);
var
  ZipFile: TUnZipper;
  I : integer;
begin
    PanelNoteList.Caption:='Notes in Snapshot ' + FullSnapName;
    ForceDirectory(SnapDir + 'temp');
    ZipFile := TUnZipper.Create;
    try
      ZipFile.FileName := FullSnapName;
      ZipFile.OutputPath := SnapDir  + 'temp';
      ZipFile.Examine;
      ZipFile.UnZipAllFiles;
    finally
      ZipFile.Free;
    end;
    if SnapNoteLister <> Nil then
        FreeAndNil(SnapNoteLister);
    SnapNoteLister := TNoteLister.Create;
    SnapNoteLister.Debugmode := True;
    SnapNoteLister.WorkingDir:= SnapDir + 'temp' + PathDelim;
    {Result := }SnapNoteLister.GetNotes();
	SnapNoteLister.LoadStGrid(StringGrid1);
	Stringgrid1.SortOrder := soDescending;    // Sort with most recent at top
	StringGrid1.SortColRow(True, 1);
end;

procedure TFormRecover.ListBoxSnapshotsDblClick(Sender: TObject);
begin
    if (ListBoxSnapshots.ItemIndex >= 0) and (ListBoxSnapshots.ItemIndex < ListBoxSnapshots.Count) then begin
        ShowNotes(SnapDir + ListBoxSnapshots.Items[ListBoxSnapshots.ItemIndex]);
    end;
end;

procedure TFormRecover.TabSheetExistingShow(Sender: TObject);
var
  I : integer;
  Msg : string;
begin
    ListBoxSnapShots.Enabled:=False;
    PanelNoteList.Caption:='Bad Notes in Notes Directory';
    LabelNoteErrors.Caption := 'You have ' + inttostr(SearchForm.NoteLister.ErrorNotes.Count)
        + ' bad notes in Notes Directory';
  	StringGrid1.Clear;
    LabelExistingAdvice2.Caption := '';
    if SearchForm.NoteLister.ErrorNotes.Count = 0 then
        LabelExistingAdvice.Caption := 'No errors, perhaps you should proceed to Snapshots'
    else begin
        LabelExistingAdvice.Caption := 'Proceed to Snapshots or try to recover by double clicking below,';
        LabelExistingAdvice2.Caption := 'if and only IF, you see useful content, make a small change, exit';
    end;
    StringGrid1.FixedRows := 0;
    StringGrid1.InsertRowWithValues(0, ['ID', 'ErrorMessage']);
    StringGrid1.FixedRows := 1;
    for I := 0 to SearchForm.NoteLister.ErrorNotes.Count -1 do begin
        Msg := SearchForm.NoteLister.ErrorNotes.Strings[I];
        StringGrid1.InsertRowWithValues(I + 1, [copy(Msg, 1, 36), copy(Msg, 44, 200)]);
    end;
    StringGrid1.AutoSizeColumns;
end;

function TFormRecover.FindSnapFiles() : integer;
var
    Info : TSearchRec;
begin
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

