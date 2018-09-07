unit Unit1;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ExtCtrls, Spin;

type

    { TForm1 }

    TForm1 = class(TForm)
        ButtonUse: TButton;
        ButtonTestRepo: TButton;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        LabelFileSyncRepo: TLabel;
        Memo1: TMemo;
        MemoStatus: TMemo;
        SelectDirectoryDialog1: TSelectDirectoryDialog;
        SpinEdit1: TSpinEdit;
        procedure ButtonTestRepoClick(Sender: TObject);
        procedure ButtonUseClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure LabelFileSyncRepoClick(Sender: TObject);
        procedure Memo1DblClick(Sender: TObject);
        procedure SpinEdit1Change(Sender: TObject);

    private
        MaxDirRev, CurrRev : integer;     // Max rev according to dir and manifest. -1 says undetermined.
        procedure FindMaxDir();
        function GetListofNotes(const DirNo: integer; TheList: TStringList
            ): boolean;
        function ReadRManifest(const Dir: string): boolean;
        function ShowRevDir(): boolean;
        procedure ShowStatus();
        function TestRepoDir(const Dir: string; out Proceed: boolean): string;

    public

    end;

var
    Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }
uses  laz2_DOM, laz2_XMLRead, LazFileUtils, LazLogger;

function TForm1.ReadRManifest(const Dir : string) : boolean;
var
    Doc : TXMLDocument;
    NodeList : TDOMNodeList;
    Node : TDOMNode;
    j : integer;
    TestSt : string;
    TestInt : integer;
begin
    Result := true;
    try
        try
            ReadXMLFile(Doc, Dir + 'manifest.xml');
            TestSt := Doc.DocumentElement.GetAttribute('server-id');
            Memo1.Append('ServerID=' + TestSt);
            TestSt :=  Doc.DocumentElement.GetAttribute('revision');
            Memo1.Append('Revision=' + TestSt);
            CurrRev := strtoint(TestSt);                  // that might ERROR ! eConvert ?
            NodeList := Doc.DocumentElement.ChildNodes;
            if assigned(NodeList) then begin
                for j := 0 to NodeList.Count-1 do begin
                    Node := NodeList.Item[j].Attributes.GetNamedItem('id');
                    if assigned(Node) then begin
                        TestSt := Node.NodeValue;                             // ID does not contain '.note';
                        if length(testSt) <> 36 then
                            memo1.Append('Invalid ID listed in manifest')
                    end else memo1.Append('Listing does not contain an ID');
                    Node := NodeList.Item[j].Attributes.GetNamedItem('rev');
                    if assigned(Node) then begin
                        TestInt := strtoint(Node.NodeValue);                  // this might ERROR, happens if its empty ?
                        if TestInt < 0 then Memo1.Append('Listing Rev number below zero');
                    end else Memo1.append('Listing does not contain a RevNo');
                 end;
            end;
        except
            on E: EFOpenError do begin
                Memo1.Append(E.Message);
                exit(False);
          	    end;
            on E: EXMLReadError do begin
                Memo1.Append(E.Message);
                exit(False);
      			end;
            on E: EAccessViolation do begin
                Memo1.Append(E.Message);
                exit(False);
                end;
      	end;
    finally
        Doc.Free;
    end;
    Memo1.Append('Manifest contains ' + inttostr(J) + ' listings.');
end;

function TForm1.GetListofNotes(const DirNo : integer; TheList : TStringList) : boolean;
var
    //I : integer;
    Info : TSearchRec;
begin
    if FindFirst(LabelFileSyncRepo.Caption + '0' + PathDelim
                    + inttostr(DirNo) + Pathdelim + '*.note', faAnyFile {and faDirectory}, Info)=0 then repeat
            TheList.Append(copy(Info.Name, 1,36));
            // Now, update the good manifest with this note and I+1 revision number.
     until FindNext(Info) <> 0;
    FindClose(Info);
end;

function TForm1.ShowRevDir() : boolean;
var
    I : integer;
    Info : TSearchRec;
begin
    // is spinedit is 1, an MaxDirRev is 290, this should show notes in 0/290
    // is spinedit is 2, an MaxDirRev is 290, this should show notes in 0/289 and 0/290
    I := MaxDirRev - SpinEdit1.value;
    while I < MaxDirRev do begin
        Memo1.Append('------ Notes changed from revision ' + inttostr(I) + ' to ' + inttostr(I+1) + ' -------');
        if FindFirst(LabelFileSyncRepo.Caption + '0' + PathDelim
                        + inttostr(I+1) + Pathdelim + '*.note', faAnyFile {and faDirectory}, Info)=0 then repeat
                Memo1.Append(Info.Name);
                // Now, update the good manifest with this note and I+1 revision number.
         until FindNext(Info) <> 0;
        FindClose(Info);
        inc(I);
    end;
    // List notes in dir and their status
end;

    // Possibiltes include -
    // both '0' dir and manifest.xml exist and no other file.       OK
    // '0' dir but no manifest                                      Maybe
    // manifest AND zero or more notes.                             OK but not top
    // none of above                                                sad ...
    // notes and 'Backup' dir -                                     user has pointed to wrong dir.
function TForm1.TestRepoDir(const Dir : string; out Proceed : boolean) : string;
begin
    Proceed := False;
    if FileExists(Dir + 'manifest.xml') and DirectoryExists(Dir + '0') then begin
        if DirectoryExists(Dir + '1') then exit('tomboy-ng does not do rev dir > 0');
        Proceed := Self.ReadRManifest(Dir);
        exit('appear to be at top of a repo');
    end;

end;

procedure TForm1.FindMaxDir();
var
        Info : TSearchRec;
        MaxDir : integer = 0;   // thats a longint, ~ 2 billion
begin
    if FindFirst(LabelFileSyncRepo.Caption + '0' + PathDelim + '*', faDirectory, Info)=0 then repeat
        //debugln('In Findfirst [' + Info.Name + ']');
        if (Info.Name = '.') or (Info.Name = '..') then continue;
        try
            if strtoint(Info.Name) > MaxDir then MaxDir := strtoint(Info.Name);
        except on E: EConvertError do showmessage('warning - non numeric dir name in 0 ' + info.name);
        end;
    until FindNext(Info) <> 0;
    FindClose(Info);
    // search through Dir + '0' finding highest number; report on non numeric dir
    MaxDirRev := MaxDir;
end;

procedure TForm1.ShowStatus();
var
    WorkDir : string;
begin
    MemoStatus.Clear;
    Memo1.Clear;
    MemoStatus.Append('Max Revision according to directories is ' + inttostr(MaxDirRev));
    if MaxDirRev = 0 then
        Memo1.Append('This does not look like a FileSync Repo to me.');
    WorkDir := LabelFileSyncRepo.Caption + '0' + PathDelim + inttostr(MaxDirRev-Spinedit1.Value +1) + PathDelim;
    if SpinEdit1.Value = 0 then
        WorkDir := LabelFileSyncRepo.Caption;
    Memo1.Append('Status for ' + WorkDir);
    if Self.ReadRManifest(WorkDir) then begin
        MemoStatus.Append('Current Rev according to Manifest is ' + inttostr(CurrRev) + ' looks OK');
        ButtonUse.Enabled := True;
    end
    else begin
        MemoStatus.Append('Manifest is BROKEN');
        ButtonUse.Enabled := False;
    end;
    ShowRevDir();
end;

procedure TForm1.ButtonTestRepoClick(Sender: TObject);
var
    GoodToGo : boolean;
begin
    spinedit1.Value := 0;
    FindMaxDir();
    ShowStatus();

end;

procedure TForm1.ButtonUseClick(Sender: TObject);
var
    RevNo, LIndex : integer;
    Backup : boolean = true;
    InFile, OutFile: TextFile;
    InString : string;
    FoundOne : boolean = false;
    NoteList : TStringList;     // A list of notes needing to be updated to achieve next revision
begin
    RevNo := MaxDirRev - spinedit1.value;
    if fileexists(LabelFileSyncRepo.Caption + 'manifest.xml') then
        copyfile(LabelFileSyncRepo.Caption + 'manifest.xml',
                        LabelFileSyncRepo.Caption + 'manifest.xml-Backup');
    copyfile(LabelFileSyncRepo.Caption + '0' + pathdelim + inttostr(RevNo+1) + PathDelim + 'manifest.xml',
                        LabelFileSyncRepo.Caption + 'manifest.xml-' + inttostr(RevNo));

    NoteList := TStringList.Create;
    { Rewrite manifest file. How brave are you ? We'll do it rev by rev until
      we hit the latest rev dir - lets hope.
      eg MaxDirRev is 290, its manifest is buggered, but the one in 0/290 is OK
      thats actually rev 289 and it says so. Go and have a look if you must !
    RevNo := MaxDirRev - spinedit1.value  (eg 290 - 1 = 289)
    Copy 0/RevNo+1/manifest.xml to root. (eg cp 0/290/manifest.xml ./manifest.xml-289)
    while RevNo < MaxDirRev begin
        record note IDs in 0/RevNo+1/*.notes into List
        Copy, line by line manifest.xml-RevNo to manifest.xml-RevNo+1 (see below)
        inc(I)
    end
    The Copy Line by Line must check for several 'events' -
        * The manifest revision number - <sync revision="289" server-id="BDD8BBC7-F907-471F-B3FA-C1919FD11487"> becomes 290
        * A listing with a note number from List, replace line, RevNo + 1
        * the last line of manifest, delay it until any remaining entries from list deal with
    }
    try
        while RevNo < MaxDirRev do begin
            NoteList.Clear;
            getListofNotes(RevNo+1, NoteList);
            AssignFile(InFile, LabelFileSyncRepo.Caption + 'manifest.xml-' + inttostr(RevNo));
            AssignFile(OutFile, LabelFileSyncRepo.Caption + 'manifest.xml-' + inttostr(RevNo+1));
            try
                try
                    Reset(InFile);
                    Rewrite(OutFile);
                    while not eof(InFile) do begin
                        readln(InFile, InString);
                        if Pos('sync revision="', InString) > 0 then begin
                            write(OutFile, '<sync revision="');
                            write(OutFile, inttostr(RevNo+1));
                            writeln(OutFile, copy(InString, pos('" server-id=', InString), length(InString)));
                            continue;
                        end;
                        if Pos('</sync>', InString) > 0 then begin
                            // deal with any notes not seen yet.
                            LIndex := 0;
                            while LIndex < NoteList.Count do begin
                                writeln(OutFile, '<note id="' + copy(NoteList.Strings[LIndex], 1, 36)
                                            + '" rev="' + inttostr(RevNo+1) + '" />');
                                inc(LIndex);
                            end;
                            writeln(OutFile, InString);
                            break;
                        end;
                        LIndex := 0;
                        FoundOne := False;
                        while LIndex < NoteList.Count do begin      // match one of our changed notes ?
                            if Pos(NoteList.Strings[LIndex], InString) > 0 then begin
                                writeln(OutFile, '<note id="' + copy(NoteList.Strings[LIndex], 1, 36)
                                            + '" rev="' + inttostr(RevNo+1) + '" />');
                                NoteList.Delete(LIndex);
                                FoundOne := True;
                                break;
                            end;
                            inc(LIndex);
                        end;
                        if not FoundOne then      // nope, just copy what we read from file
                            writeln(OutFile, InString);
                    end;
                finally
                    CloseFile(OutFile);
                    CloseFile(InFile);
                end;
             except
                on E: EInOutError do
                    showmessage('File handling error occurred. Details: ' + E.Message);
            end;
            inc(revNo);
        end;
    finally
    FreeandNil(NoteList);
    end;
    copyfile(LabelFileSyncRepo.Caption + 'manifest.xml-'+ inttostr(RevNo),
                        LabelFileSyncRepo.Caption + 'manifest.xml');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    MaxDirRev := -1;
    CurrRev := -1;
    ButtonUse.Enabled := False;
end;

procedure TForm1.LabelFileSyncRepoClick(Sender: TObject);
begin
    if SelectDirectoryDialog1.Execute then
         LabelFileSyncRepo.Caption := TrimFilename(SelectDirectoryDialog1.FileName + PathDelim);
end;

procedure TForm1.Memo1DblClick(Sender: TObject);
var
    ClickedLine : string;
begin
    ClickedLine := Memo1.Lines[memo1.CaretPos.y];
    // OK, does it look like a note ?
    if length(ClickedLine) <> 41 then exit;
    if copy(ClickedLine, 37, 5) = '.note' then
        MemoStatus.Append(ClickedLine);  // show the poor user this note's title or something !
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
    ShowStatus();
end;

end.

