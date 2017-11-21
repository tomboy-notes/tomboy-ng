unit Note_Lister;
{
 * Copyright (C) 2017 David Bannon
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

{	A class that knows how to read a directory full of notes. It keeps that list
	internally, unsorted (easier to sort in the display grid). Note details (
    Title, LastChange) can be updated (eg when a note is saved).

	It keeps a second list if user has done a search.
}

{$mode objfpc}

INTERFACE

uses
		Classes, SysUtils, Grids;

type
  	PNote=^TNote;
  	TNote = record
        		{ will have 36 char GUI plus '.note' }
		ID : ANSIString;
        Title : ANSIString;
        		{ a 19 char date time string }
    	CreateDate : ANSIString;
                { a 19 char date time string, updateable }
    	LastChange : ANSIString;
	end;

type                                 { ---------- TNoteInfoList ---------}

   { TNoteList }

   TNoteList = class(TList)   // TFPList
   private
    	function Get(Index: integer): PNote;
    public
        destructor Destroy; override;
        function Add(ANote : PNote) : integer;
        function FindID(const ID : ANSIString) : PNote;
        property Items[Index: integer]: PNote read Get; default;
    end;




type

   { NoteLister }

   { TNoteLister }

   TNoteLister = class
   private
   	NoteList : TNoteList;
   	SearchNoteList : TNoteList;
   	procedure CleanupList(const Lst : TNoteList);
   	procedure GetNoteDetails(const Dir, FileName: ANSIString; const SearchTerm : ANSIString = '');
    		{ Returns True if indicated note contains term in its content }
   	function NoteContains(const Term, FileName : ANSIString) : boolean;
   public
    		{ The directory, with trailing seperator, that the notes are in }
   	WorkingDir : ANSIString;
   	SearchIndex : integer;
    		{ Read the metadata from all the notes in internal data structure }
   	function GetNotes(const Term : ANSIstring = '') : longint;
    		{ Copy the internal data to the passed TStringGrid, empting it first }
   	procedure LoadStGrid(const Grid : TStringGrid);
    		{ Returns True if its updated the internal record as indicated,
              will accept either an ID or a filename. }
    function AlterNote(ID, Change : ANSIString; Title : ANSIString = '') : boolean;
    		{ Destroy, destroy .... }
    function IsThisATitle(const Title : ANSIString) : boolean;
    		{ Returns true if it can find a FileName to Match this Title }
    function FileNameForTitle(const Title: ANSIString; out FileName : ANSIstring): boolean;
    procedure StartSearch();
    function NextNoteTitle(out SearchTerm : ANSIString) : boolean;
    		{ removes note from int data, accepting either an ID or Filename }
    function DeleteNote(const ID : ANSIString) : boolean;
			{ Copy the internal data to the passed TStringGrid, empting it first }
	procedure LoadSearchGrid(const Grid : TStringGrid);
    destructor Destroy; override;
   end;


{ ----------------------- IMPLEMENTATION --------------- }

implementation

uses  laz2_DOM, laz2_XMLRead, LazFileUtils, LazUTF8, LazLogger;

{ Projectinspector, double click Required Packages and add LCL }

{ NoteLister }

procedure TNoteLister.CleanupList(const Lst : TNoteList);
var
    Index : integer;
begin
    { Try and make sure these dates work, even if messed up }
    for Index := 0 to Lst.count -1 do begin;
        while UTF8length(Lst.Items[Index]^.CreateDate) < 20 do
        	Lst.Items[Index]^.CreateDate :=  Lst.Items[Index]^.CreateDate + ' ';
	  Lst.Items[Index]^.CreateDate := copy(Lst.Items[Index]^.CreateDate, 1, 19);
      Lst.Items[Index]^.CreateDate[11] := ' ';
      while UTF8length(Lst.Items[Index]^.LastChange) < 20 do
          Lst.Items[Index]^.LastChange := Lst.Items[Index]^.LastChange + ' ';
      Lst.Items[Index]^.LastChange := copy(Lst.Items[Index]^.LastChange, 1, 19);
      Lst.Items[Index]^.LastChange[11] := ' ';
	end;
end;

procedure TNoteLister.GetNoteDetails(const Dir, FileName : ANSIString; const SearchTerm : AnSIString = '');
			// This is how we search for XML elements, attributes are different.
var
    NoteP : PNote;
    Doc : TXMLDocument;
	Node : TDOMNode;
begin
    // writeln('Checking note ', FileName);
  	if FileExistsUTF8(Dir + FileName) then begin
        if SearchTerm <> '' then
        	if not NoteContains(SearchTerm, FileName) then exit();
        new(NoteP);
  	    try
			try
                NoteP^.ID:=FileName;
  			    ReadXMLFile(Doc, Dir + FileName);
  			    Node := Doc.DocumentElement.FindNode('title');
      		    NoteP^.Title := Node.FirstChild.NodeValue;
                Node := Doc.DocumentElement.FindNode('last-change-date');
                NoteP^.LastChange := Node.FirstChild.NodeValue;
                Node := Doc.DocumentElement.FindNode('create-date');
                NoteP^.CreateDate := Node.FirstChild.NodeValue;
            except 		on EXMLReadError do DebugLn('Note has no Title');
            		    on EAccessViolation do DebugLn('Access Violation');
  		    end;
            if SearchTerm = '' then
            	NoteList.Add(NoteP)
            else SearchNoteList.Add(NoteP);
  	    finally
      	    Doc.free;
  	    end;
  end else DebugLn('Error, found a note and lost it !');
end;

function TNoteLister.NoteContains(const Term, FileName: ANSIString): boolean;
var
	Doc : TXMLDocument;
	Node : TDOMNode;
    Content : ANSIString;
begin
  	Result := False;
	if FileExistsUTF8(WorkingDir + FileName) then begin
      	try
    		try
	  			ReadXMLFile(Doc, WorkingDir + FileName);
	  			Node := Doc.DocumentElement.FindNode('text');
	      		Content := Node.FirstChild.FirstChild.NodeValue;
            except	on EXMLReadError do DebugLn('Note has bad xml', WorkingDir + Filename);
                	on EAccessViolation do DebugLn('Access Violation on Note Content ', WorkingDir + Filename);
      		end;
      	finally
        	Doc.free;
      	end;
        if UTF8Pos(Term, Content) > 0 then Result := True;
	end else DebugLn('Error, found a note and lost it !', WorkingDir + Filename);
end;

function TNoteLister.GetNotes(const Term : ANSIString = '') : longint;
var
    Info : TSearchRec;
begin
	if Term = '' then begin
        NoteList.Free;
    	NoteList := TNoteList.Create;
	end else begin
        SearchNoteList.Free;
    	SearchNoteList := TNoteList.Create;
    end;
    if WorkingDir = '' then
    	DebugLn('In GetNotes with a blank working dir, thats going to lead to tears....');

  	if FindFirst(WorkingDir + '*.note', faAnyFile and faDirectory, Info)=0 then begin
  		repeat
  			GetNoteDetails(WorkingDir, Info.Name, Term);
  		until FindNext(Info) <> 0;
  	end;
  	FindClose(Info);
    if Term = '' then begin
        CleanUpList(NoteList);
        Result := NoteList.Count;
	end else begin
    	CleanUpList(SearchNoteList);
		result := NoteList.Count;
	end;
end;

procedure TNoteLister.LoadStGrid(const Grid : TStringGrid);
var
    Index : integer;
begin
  	Grid.Clear;
    Grid.InsertRowWithValues(0, ['Title', 'Last Change', 'Create Date', 'File Name']);
	for Index := 0 to NoteList.Count -1 do begin
        Grid.InsertRowWithValues(Index+1, [NoteList.Items[Index]^.Title,
        	NoteList.Items[Index]^.LastChange, NoteList.Items[Index]^.CreateDate,
            NoteList.Items[Index]^.ID]);
        { writeln(NoteList.Items[Index]^.ID, ' - ', NoteList.Items[Index]^.LastChange, ' - ',
        NoteList.Items[Index]^.Title, ' - ', NoteList.Items[Index]^.CreateDate ); }
	end;
    Grid.AutoSizeColumns;
end;

function TNoteLister.AlterNote(ID, Change: ANSIString; Title: ANSIString): boolean;
var
    Index : integer;
    TestID : ANSIString;
begin
  	if length(ID) = 36 then
        TestID := ID + '.note'
    else
        TestID := ID;
	result := False;
    for Index := 0 to NoteList.Count -1 do begin
        if TestID = NoteList.Items[Index]^.ID then begin
        	if Title <> '' then
            	NoteList.Items[Index]^.Title := Title;
        	if Change <> '' then
            	NoteList.Items[Index]^.LastChange := Change;
            	NoteList.Items[Index]^.LastChange := copy(NoteList.Items[Index]^.LastChange, 1, 19);
            	NoteList.Items[Index]^.LastChange[11] := ' ';
            Result := True;
            exit();
		end;
	end;
    // DebugLn('Trying to match ', TestID);
end;

function TNoteLister.IsThisATitle(const Title: ANSIString): boolean;
var
    Index : integer;
begin
  	Result := False;
	for Index := 0 to NoteList.Count -1 do begin
        if Title = NoteList.Items[Index]^.Title then begin
        	Result := True;
            break;
		end;
	end;
end;

function TNoteLister.FileNameForTitle(const Title: ANSIString; out FileName : ANSIstring): boolean;
var
    Index : integer;
begin
  	Result := False;
	for Index := 0 to NoteList.Count -1 do begin
        if Title = NoteList.Items[Index]^.Title then begin
            FileName := NoteList.Items[Index]^.ID;
        	Result := True;
            break;
		end;
	end;
end;

procedure TNoteLister.StartSearch;
begin
	SearchIndex := 0;
end;

function TNoteLister.NextNoteTitle(out SearchTerm: ANSIString): boolean;
begin
  	Result := False;
	if SearchIndex < NoteList.Count then begin
    	SearchTerm := NoteList.Items[SearchIndex]^.Title;
    	inc(SearchIndex);
        Result := True;
	end;
end;

function TNoteLister.DeleteNote(const ID: ANSIString): boolean;
var
    Index : integer;
    TestID : ANSIString;
begin
    if length(ID) = 36 then
        TestID := ID + '.note'
    else
    	TestID := ID;
	result := False;
    for Index := 0 to NoteList.Count -1 do begin
        if TestID = NoteList.Items[Index]^.ID then begin
          NoteList.Delete(Index);
          Result := True;
          break;
		end;
	end;
    if Result = false then
        DebugLn('Failed to remove ref to note in NoteLister ', TestID);
end;

procedure TNoteLister.LoadSearchGrid(const Grid: TStringGrid);
var
    Index : integer;
begin
  	Grid.Clear;
    Grid.InsertRowWithValues(0, ['Title', 'Last Change', 'Create Date', 'File Name']);
	for Index := 0 to SearchNoteList.Count -1 do begin
        Grid.InsertRowWithValues(Index+1, [SearchNoteList.Items[Index]^.Title,
        	SearchNoteList.Items[Index]^.LastChange, SearchNoteList.Items[Index]^.CreateDate,
            SearchNoteList.Items[Index]^.ID]);
        { writeln(NoteList.Items[Index]^.ID, ' - ', NoteList.Items[Index]^.LastChange, ' - ',
        NoteList.Items[Index]^.Title, ' - ', NoteList.Items[Index]^.CreateDate ); }
	end;
    Grid.AutoSizeColumns;
end;

destructor TNoteLister.Destroy;
begin
    SearchNoteList.Free;
    NoteList.Free;
	inherited Destroy;
end;

{ TNoteList }


destructor TNoteList.Destroy;
var
  I : integer;
begin
	for I := 0 to Count-1 do
    	dispose(Items[I]);
	inherited Destroy;
end;

function TNoteList.Add(ANote: PNote): integer;
begin
    result := inherited Add(ANote);
end;

function TNoteList.FindID(const ID: ANSIString): PNote;
var
    Index : longint;
begin
    Result := Nil;
    for Index := 0 to Count-1 do begin
        if Items[Index]^.ID = ID then begin
            Result := Items[Index];
            exit()
		end;
	end;
end;

function TNoteList.Get(Index: integer): PNote;
begin
    Result := PNote(inherited get(Index));
end;


end.

