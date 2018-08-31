unit Unit1;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    Grids, kmemo;
type
    TNoteRecord = record
        Title : ANSIString;
        NoteID : ANSIString;
        ServerLastChange : ANSIString;
        LastChange : ANSIString;
        Created : ANSIstring;
        Template : Boolean;
        NoteBook : boolean;
    end;

type

    { TForm1 }

    TForm1 = class(TForm)
        ButtonIndex: TButton;
        ButtonImport: TButton;
        KMemo1: TKMemo;
        Label1: TLabel;
        LabelDoubleClick: TLabel;
        LabelTiming: TLabel;
        LabelNoNotes: TLabel;
        SelectDirectoryDialog1: TSelectDirectoryDialog;
        StringGrid1: TStringGrid;
        procedure ButtonDisplayClick(Sender: TObject);
        procedure ButtonImportClick(Sender: TObject);
        procedure ButtonIndexClick(Sender: TObject);
        procedure CheckNote(FullFileName: string);
        procedure FormShow(Sender: TObject);
        procedure StringGrid1DblClick(Sender: TObject);
    private
        procedure CreateDB(DBName: string);
        procedure DisplayNote(NoteID: string);
        procedure ExtractData(FullFileName: string; var Rec : TNoteRecord);
        procedure ImportNotes(const NoteDir, DBFile: string);

    public
        DebugMode : boolean;
    end;

var
    Form1: TForm1;

implementation

{$R *.lfm}


{ TForm1 }
uses Lazfileutils, sqldb, sqlite3conn, LazLogger, LazUTF8, laz2_DOM,
    laz2_XMLRead, settings,
    loadNote;

var
    Conn : TSQLite3Connection;
    Tran : TSQLTransaction;

const DBFileName = 'TB_DBase.db';


procedure TForm1.FormShow(Sender: TObject);
var
  AConnection : TSQLite3Connection;
  ATransaction : TSQLTransaction;
  Schema : string;
begin
    if FileExists(DBFileName) then
           Label1.Caption := 'dbase present';
    StringGrid1.ColCount:=4;
    StringGrid1.FixedCols:=0;
    StringGrid1.Enabled := false;
    LabelDoubleClick.Visible:= false;
end;

procedure TForm1.StringGrid1DblClick(Sender: TObject);
var
  Tick, Tock : DWord;
begin
    Tick := GetTickCount64();
    DisplayNote(StringGrid1.Cells[2, StringGrid1.Row]);
    Tock := GetTickCount64();
    LabelTiming.Caption := 'Display took note ' + inttostr(Tock - Tick) + 'mS';
end;

procedure TForm1.ExtractData(FullFileName : string; var Rec : TNoteRecord);
var
    Doc : TXMLDocument;
    Node : TDOMNode;
    J : integer;
begin
    //Debugln('Extracting from ' + FullFileName);
    if FileExists(FullFileName) then begin

        try
            Rec.Template := False;
            ReadXMLFile(Doc, FullFileName);
            Node := Doc.DocumentElement.FindNode('title');
            Rec.Title := Node.FirstChild.NodeValue;
            if DebugMode then Debugln('Title is [' + Node.FirstChild.NodeValue + ']');
            Node := Doc.DocumentElement.FindNode('last-change-date');
            Rec.LastChange := Node.FirstChild.NodeValue;
            Node := Doc.DocumentElement.FindNode('create-date');
            Rec.Created := Node.FirstChild.NodeValue;
            Node := Doc.DocumentElement.FindNode('tags');
                if Assigned(Node) then begin
                    for J := 0 to Node.ChildNodes.Count-1 do
                    if UTF8pos('system:template', Node.ChildNodes.Item[J].TextContent) > 0 then
                        Rec.Template := True;
                    for J := 0 to Node.ChildNodes.Count-1 do
                        if UTF8pos('system:notebook', Node.ChildNodes.Item[J].TextContent) > 0 then
                                Rec.NoteBook := True;
                                //NoteBookList.Add(Filename, UTF8Copy(Node.ChildNodes.Item[J].TextContent, 17, 1000), NoteP^.IsTemplate);
                        // Node.ChildNodes.Item[J].TextContent) may be something like -
                        // * system:notebook:DavosNotebook - this note belongs to DavosNotebook
                        // * system:template - this note is a template, if does not also have a
                        // Notebook tag its the StartHere note, otherwise its the Template for
                        // for the mentioned Notebook.
                end;
        except on E: EXMLReadError do begin
                    DebugLn(E.Message);
                    exit();
               end;
            on EAccessViolation do DebugLn('Access Violation ' + FullFileName);
        end;
    end;
end;

procedure TForm1.CheckNote(FullFileName : string);
var
    Loader : TBLoadNote;
    Stm : TMemoryStream;
    I : integer = 0;
    Rec : TNoteRecord;
    Content, DataString, ExecString : AnsIstring;
    Ch : Char;
begin
    ExtractData(FullFileName, Rec);
    Rec.NoteID:= copy(extractFilename(FullFileName), 1, 36);
    Sett := TSett.Create();
    KMemo1.Blocks.Clear;
    KMemo1.Blocks.LockUpdate;
    Loader := TBLoadNote.Create;
    Loader.LoadFile(FullFileName, KMemo1);
    KMemo1.Blocks.UnLockUpdate;
    Loader.Free;
    Sett.Free;
    Stm := TMemoryStream.Create;
    KMemo1.SaveToRTFStream(Stm);
    //Stm.SaveToFile('My.rtf');
    Stm.Position:=0;
    Content := '';
    for I := 0 to Stm.Size-1 do begin
        Ch := char(Stm.ReadByte);
        Content := Content + Ch;
        if Ch = '''' then Content := Content + '''';
    end;
    Stm.Free;
    DataString := '''' + Rec.NoteID + '''' + ',' + '''' + Rec.Title + '''' + ','
            + '''' + Rec.LastChange + '''' + ',' + '''' + Rec.Created + '''' + ','
            + '''' + Content + '''';
    ExecString := 'insert into TBLNOTES (ID,Title,LastChange,Created,Content) values (' + DataString +');';
    // Debugln(ExecString);
    Conn.ExecuteDirect(ExecString);
end;


procedure TForm1.CreateDB(DBName : string);
var
    Schema : string;
begin
    if FileExists(DBFileName) then
           DeleteFile(DBFileName);
    Schema :=
    'ID TEXT,' +          // The GUID used by Tomboy
    'Title TEXT,' +       // Note title
    'LastChange TEXT,' +  // maybe a string using Tomboy prefered format or Unix Int ?
    'Created TEXT,' +     // ..
    'SyncRev1 integer,' + // One of two sync references
    'SyncRev2 integer,' + // ..
    'Status TEXT,' +      // [Current; Backup; SyncOverWrite; Deleted....]
    'Content TEXT';       // Note content in RFT format
    Conn := TSQLite3Connection.Create(nil);
    Conn.DatabaseName := DBFileName;
    Tran := TSQLTransaction.Create(Conn);
    Conn.Transaction := Tran;
    Conn.Open;
    Tran.StartTransaction;
    Conn.ExecuteDirect('create table TBLNOTES (' + Schema + ');');
    Tran.Commit;
    Conn.Close;
    Tran.Free;
    Conn.Free;
    Label1.Caption := 'dbase created';
end;

procedure TForm1.ButtonImportClick(Sender: TObject);
var
    Repo : string;
begin
    if SelectDirectoryDialog1.Execute then begin
       Repo := TrimFilename(SelectDirectoryDialog1.FileName) + PathDelim;
       CreateDB(DBFileName);
       ImportNotes(Repo, DBFileName);
    end;
end;

procedure TForm1.ButtonIndexClick(Sender: TObject);
var
    Query : TSQLQuery;
    Index : integer =0;
    Tick, Tock : DWord;
begin
    if not fileexists(DBFileName) then begin
       showmessage('You need import some notes first.');
       exit;
    end;
    StringGrid1.Enabled := true;
    LabelDoubleClick.Visible:= True;
    Tick := GetTickCount64();
    Conn := TSQLite3Connection.Create(nil);
    Conn.DatabaseName := DBFileName;
    Tran := TSQLTransaction.Create(Conn);
    Conn.Transaction := Tran;
    Query := TSQLQuery.Create(nil);
    Query.SQL.Text := 'select ID,TITLE,LASTCHANGE from TBLNOTES';
    Query.Database := Conn;
    Query.Open;
    StringGrid1.InsertRowWithValues(0, ['Title', 'Last Change', 'ID']);
    StringGrid1.FixedRows := 1;

    while not Query.Eof do begin
        StringGrid1.InsertRowWithValues(Index+1, [ Query.FieldByName('TITLE').AsString,
                Query.FieldByName('LASTCHANGE').AsString, Query.FieldByName('ID').AsString ]);
        inc(Index);
        Query.Next;
    end;

    Query.Close;
    Conn.Close;
    Query.Free;
    Tran.Free;
    Conn.Free;
    Tock := GetTickCount64();
    LabelTiming.Caption := 'Indexing took ' + inttostr(Tock - Tick) + 'mS';
    StringGrid1.AutoSizeColumns;
end;

procedure TForm1.ImportNotes(const NoteDir, DBFile : string);
var
  Info : TSearchRec;
  I : integer = 0;
  Tick, Tock : DWord;
begin
    Tick := GetTickCount64();
    Conn := TSQLite3Connection.Create(nil);
    Conn.DatabaseName := DBFileName;
    Tran := TSQLTransaction.Create(Conn);
    Conn.Transaction := Tran;
    Conn.Open;
    Tran.StartTransaction;
    if FindFirst(NoteDir + '*.note', faAnyFile, Info)=0 then begin
        repeat
            if I mod 10 = 0 then begin
                LabelNoNotes.Caption := 'Notes Processed ' + inttostr(I);
                Application.ProcessMessages;
            end;
            inc(I);
            //debugln('Looking at ' + Info.Name);
            CheckNote(NoteDir + Info.Name);
        until FindNext(Info) <> 0;
    end;
    LabelNoNotes.Caption := 'Completed - Notes Processed ' + inttostr(I);
    FindClose(Info);
    Tran.Commit;
    Conn.Close;
    Tran.Free;
    Conn.Free;
    Tock := GetTickCount64();
    LabelTiming.Caption := 'Import took ' + inttostr(Tock - Tick) + 'mS';
end;

procedure TForm1.ButtonDisplayClick(Sender: TObject);
var
  Tick, Tock : DWord;
begin
    Tick := GetTickCount64();
    DisplayNote('5b1eff25-c5a2-4d5e-9642-b9deabff71da');
    Tock := GetTickCount64();
    LabelTiming.Caption := 'Display took ' + inttostr(Tock - Tick) + 'mS';
end;

procedure TForm1.DisplayNote(NoteID : string);
var
  Content : string;
  Query   : TSQLQuery;
  Stm : TMemoryStream;
begin
    KMemo1.clear();
    Conn := TSQLite3Connection.Create(nil);
    Conn.DatabaseName := DBFileName;
    Tran := TSQLTransaction.Create(Conn);
    Conn.Transaction := Tran;
    Query := TSQLQuery.Create(nil);
    Query.SQL.Text := 'select CONTENT from TBLNOTES where ID = ''' + NoteID + ''';';
    Query.Database := Conn;
    Query.Open;
    Stm := TMemoryStream.Create;
    Stm.WriteAnsiString(Query.FieldByName('CONTENT').AsString);
    //stm.SaveToFile('blar.rtf');
    stm.Position:=0;
    KMemo1.Blocks.LockUpdate;
    KMemo1.LoadFromRTFStream(Stm);
    Kmemo1.Blocks.UnLockUpdate;
    Stm.Free;
    Query.Close;
    Conn.Close;
    Query.Free;
    Tran.Free;
    Conn.Free;

end;

end.

