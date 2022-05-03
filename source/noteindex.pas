unit NoteIndex;
{   Copyright (C) 2017-2021 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------

    This unit will load all notes in a directory into a memory structure and
    provides a means to filter it down incrementally (ie while user types a
    search term.  Each subsquent seach term is only searched for in notes that
    are still 'active' so, gets faster as search progresses.

    But uses quite a bit of memory so make sure you release it as soon as possible.

    Some users, with very large note collections, very large notes or slow hardware
    may find using this model tiresome.

    HISTORY : released on 2021/12/03

}

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, ComCtrls;

type
      PNote=^TNote;
      TNote = record
            Content : string;
            ID    : string;
            Active : boolean;
      end;

type

    { TNoteList }

    { TNoteIndex }

    TNoteIndex = class(TFPList)
        private
            function Get(Index: integer): PNote;
            function GetNotecontent(const FFName: string; out St: string): boolean;
            function ImportAll(const NDir: string; const CSensitive: boolean): boolean;
                              // Adds a new item to List, Notes and files in Meta require prefix added before being passed here
            procedure AddNewItem(const ID, Content : string);
                            // Returns True if the passed FName exists (as an ID) in the list
            function Find(const FName : string): boolean;
            property Items[Index: integer]: PNote read Get; default;

        public
            Busy : boolean;
            constructor Create(NoteDir: string; CaseSensitive: boolean);
            destructor Destroy; override;
            // Searches content of each note for St, if not present, mark that
            // note as Active=False.
            function SearchList(St: string): integer;
            // Files out the created STL with IDs of notes that are 'Active',
            // True if one or more notes passed back.
            function GetActive(STL : TStringList) : boolean;
            procedure ResetActive();
            procedure UpDateLview(LV : TListView);
        end;



implementation

function TNoteIndex.Get(Index: integer): PNote;
begin
    Result := PNote(inherited get(Index));
end;

constructor TNoteIndex.Create(NoteDir : string; CaseSensitive : boolean);
begin
    inherited Create;
    ImportAll(NoteDir, CaseSensitive);
end;

destructor TNoteIndex.Destroy;
var
    i : integer;
begin
    for I := 0 to Count-1 do begin
        dispose(Items[I]);
    end;
    inherited Destroy;
end;

procedure TNoteIndex.AddNewItem(const ID, Content : string);
var
    NoteP : PNote;
begin
    new(NoteP);
    NoteP^.Active:= True;
    NoteP^.Content := Content;
    NoteP^.ID := ID;
    inherited Add(NoteP);
end;


// iterates over list looking for St in the Content of all Required notes. Any that
// don't have St are set to Required False. Returns the number of remaining trues.
function TNoteIndex.SearchList(St : string) : integer;
var
    NoteP : PNote;
begin
    busy := true;
    Result := 0;
    // iterate over structure. I tried moving known false entries down the
    //  list so they could be skipped but it did not help. Unnecessary complication.
    for NoteP in self do
        if NoteP^.Active then begin
            //writeln(NoteP^.ID);
            NoteP^.Active := pos(St, NoteP^.Content) > 0;
            if NoteP^.Active then
                inc(Result);
        end;
    Busy := False;
end;

function TNoteIndex.GetActive(STL: TStringList): boolean;      // do not need this
var
    NoteP : PNote;
begin
    for NoteP in self do
        if NoteP^.Active then
            STL.Add(NoteP^.ID);
    result := STL.Count > 0;
end;

procedure TNoteIndex.ResetActive;
var
    NoteP : PNote;
begin
    busy := true;
    for NoteP in self do
        NoteP^.Active := true;
    busy := false;
end;

function TNoteIndex.Find(const FName: string): boolean;
var
    NoteP : PNote;
begin
    for NoteP in self do
        if NoteP^.ID + '.note' = FName then
            exit(NoteP^.Active);
    result := false;
end;

procedure TNoteIndex.UpDateLview(LV: TListView);
var
    Cnt : integer;
begin
    busy := true;
    Cnt := LV.items.count -1;
    while Cnt > -1 do begin
        //writeln('TNoteIndex.UpDateLview - checking ' + LV.Items[Cnt].SubItems[1]);
        if not find(lv.Items[Cnt].SubItems[1]) then begin    // subitem[1] being note name
            LV.Items[Cnt].Delete;
            //writeln('TNoteIndex.UpDateLview - deleted line ' + cnt.tostring);
        end;
        dec(Cnt)
    end;
    busy := false;
end;

function TNoteIndex.ImportAll(const NDir : string; const CSensitive : boolean): boolean;
var
    Info : TSearchRec;
    St : string;
begin
    busy := true;
    if FindFirst(NDir + '*.note', faAnyFile, Info) = 0 then
        try
            repeat
                GetNotecontent(NDir + Info.Name, ST);
                if CSensitive then
                    AddNewItem(copy(Info.Name, 1, length(Info.name) - 5), ST)
                else AddNewItem(copy(Info.Name, 1, length(Info.name) - 5), lowercase(ST));
            until FindNext(Info) <> 0;
        finally
            FindClose(Info);
        end
    else begin
        //ErrorMessage := 'ERROR : No notes were found in ' + NotesDir;
        //DebugLn(ErrorMessage);
        exit(false);
    end;
    result := true;
    busy := False;
end;

// Reads one note, putting all its content into the passed string, in the event of an error,
// returns .F. and with an empty string.
function TNoteIndex.GetNotecontent(const FFName : string; out St : string): boolean;
var
    FileStream: TFileStream;
    Sz : integer;
begin
    FileStream := TFileStream.Create(FFName, fmOpenRead);
    St := '';
    try
        try
        Sz := Filestream.Size;
        setlength(St, Sz);
        FileStream.Read(St[1], Sz);           // interesting, BufReadStream is slightly worse here ....
        except on E: Exception do begin
                //writeln('Bugger, ' + E.Message);
                St := '';
                end;
        end;
    finally
        Filestream.free;
    end;
    result := (st.length > 100);
end;

end.

