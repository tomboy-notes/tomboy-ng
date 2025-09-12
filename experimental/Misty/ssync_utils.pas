unit ssync_utils;

{   Part of the tomboy-ng Misty project.

    The Server Sync Utils unit
    A number of data handling types and functions directly taken from tomboy-ng
    source. In a seperate unit to make keeping them current easier ? Perhaps.

    Copyright (C) 2017-2025 David Bannon

    License:
    This code is licensed under MIT License, see the file License.txt
    or https://spdx.org/licenses/MIT.html  SPDX short identifier: MIT

    Really intended to be built using the Lazarus IDE it can also be built
    from the command ine with -
    $> ~/bin/Lazarus/lazarus-fixes_4/lazbuild ./webserver.lpr
    But there is, sadly, unavoidable dependenies on laz2_dom* so on LCL so
    you need a Lazarus install anyway !

}

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, laz2_DOM, laz2_XMLRead {, LCLProc}, LazLogger;

                // not needed here but mentioned in TNoteInfoList, maybe a variant someday ?
type TSyncAction=(SyUnset,      // initial state, should not be like this at end.
                SyNothing,      // This note, previously sync'ed has not changed.
                SyUploadNew,    // This a new local note, upload it.
                SyUploadEdit,   // A previously synced note, edited locally, upload.
                SyDownload,     // A new or edited note from elsewhere, download.
                SyDeleteLocal,  // Synced previously but no longer present on server, delete locally
                SyDeleteRemote, // Marked as having been deleted locally, so remove from server.
                SyClash,        // Edited both locally and remotly, policy or user must decide.
                SyError,
                SyNetError,     // And upload failed, probably a transit network problem
                SyAllRemote,    // Clash Decision - Use remote note for all subsquent clashes
                SyAllLocal,     // Clash Decision - Use local note for all subsquent clashes
                SyAllNewest,    // Clash Decision - Use newest note for all subsquent clashes
                SyAllOldest);   // Clash Decision - Use oldest note for all subsquent clashes

type                                // from syncutils
  	PNoteInfo=^TNoteInfo;
  	TNoteInfo = record
        ID : ANSIString;            // The 36 char ID
        LastChangeGMT : TDateTime;  // Compare less or greater than but not Equal !
        CreateDate : ANSIString;
        LastChange : ANSIString;    // leave as strings, need to compare and TDateTime uses real
        Rev : Integer;              // Not used for uploads, Trans knows how to inc its own.
        Deleted: Boolean;
        SID : longint;              // Short ID, clumbsy alt to the GUID/UUID we should use
        Action : TSyncAction;
        ForceUpload : boolean;      // this item, when true, should be marked force-upload in local manifest, transit net failure
        Title : ANSIString;
        Sha : ANSIString;           // The sha of an uploaded note. Stored in local manifest, Github mode.
    end;

 type                                 { ---------- TNoteInfoList ---------}

   { TNoteInfoList }

   TNoteInfoList = class(TList)
    private
     	function Get(Index: integer): PNoteInfo;
    public
        ServerID : string;              // Partially implemented, don't rely yet ....
        LastSyncDateSt : string;        // Partially implemented, don't rely yet ....
        LastSyncDate : TDateTime;       // Partially implemented, don't rely yet ....
        LastRev : integer;              // used only to initialise Revision at startup
        destructor Destroy; override;
        function Add(ANote : PNoteInfo) : integer;
        // returns a TNoteInfo pointer to note entry with passed ID or ID.note. Nil if not found.
        function FindID(ID: ANSIString): PNoteInfo;
        function ActionName(Act : TSyncAction) : string;
        procedure DumpList(const Wherefrom : string);
        property Items[Index: integer]: PNoteInfo read Get; default;
    end;

                    // Returns True if it loaded the manifest into data structure
                    // Returns False if Manifest not present.
                    // Returns False and sets EString if manifest present but contains error.
                    // this is based on transmisty unit;
function ReadManifest(var RMetaData : TNoteInfoList; const FFName : string; var EString : string) : boolean;

                    // Returns a string being FFName of a note, accept ID as empty (for creating dir),
                    // as an ID or as ID.note.  If anything in ID, always returns with the .note at end
function GetRevisionDirPath(ServerPath: string; Rev: integer; NoteID : string = ''): string;

function GetTitleFromFFN(ServerPath: string; Rev : integer; ID: string): string;

function MyAppendPathDelim(APath : string) : string;

var
    MetaData : TNoteInfoList;     // GLOBAL, list, built from manifest with details of notes.

implementation


function MyAppendPathDelim(APath : string) : string;    // here to avoid use lazfileutils
begin
    if APath[length(APath)] <> PathDelim then
        Result := APath + PathDelim
    else Result := APath;
end;


{ ----------------  TNoteInfoList ---------------- }

function TNoteInfoList.Add(ANote : PNoteInfo) : integer;
begin
    result := inherited Add(ANote);
end;

{ This will be quite slow with a big list notes, consider an AVLTree ? }
function TNoteInfoList.FindID(ID: ANSIString): PNoteInfo;
var
    Index : longint;
begin
    Result := Nil;
    if ID.EndsWith('.note') then ID := copy(ID, 1, 36);
    for Index := 0 to Count-1 do begin
        if Items[Index]^.ID = ID then begin
            Result := Items[Index];
            exit()
        end;
    end;
end;

function TNoteInfoList.ActionName(Act: TSyncAction): string;
begin
    Result := ' Unknown ';
    case Act of
        SyUnset : Result := ' Unset ';
        SyNothing : Result := ' Nothing ';
        SyUploadNew  : Result := ' UploadNew ';   // we differentiate in case of a write to remote fail.
        SyUpLoadEdit : Result := ' UpLoadEdit ';
        SyDownload: Result := ' Download ';
        SyDeleteLocal  : Result := ' DeleteLocal ';
        SyDeleteRemote : Result := ' DeleteRemote ';
        SyError : Result := ' ** ERROR **';
        SyClash : Result := ' Clash ';
        SyAllLocal : Result := ' AllLocal ';
        SyAllRemote : Result := ' AllRemote ';
        SyAllNewest : Result := ' AllNewest ';
        SyAllOldest : Result := ' AllOldest ';
    end;
    while length(result) < 15 do Result := Result + ' ';
end;

procedure TNoteInfoList.DumpList(const Wherefrom: string);
var
    P : PNoteInfo;
    St : string;
begin
    DebugLn('');
    debugln('----------- List MetaData ' + Wherefrom + ' -------------');
    for P in self do begin
        St := ' ' + inttostr(P^.Rev);
        while length(St) < 5 do St := St + ' ';
        debugln('ID=' + P^.ID  + St + ActionName(P^.Action)
                + '   ' + P^.Title + '  Rev=' + inttostr(P^.Rev));
        debugln('          CDate=' + P^.CreateDate + ' LCDate=' + P^.LastChange);
    end;
   debugln('-------------------------------------------------------');
(*    for I := 0 to Count -1 do begin
        St := ' ' + inttostr(Items[i]^.Rev);
        while length(St) < 5 do St := St + ' ';
        // St := Meta.ActionName(Meta.Items[i]^.Action);
        debugln('ID=' + copy(Items[I]^.ID, 1, 9)  + St + ActionName(Items[i]^.Action)
                + '   ' + Items[I]^.Title + '  sha=' + copy(Items[I]^.Sha, 1, 9));
        debugln('          CDate=' + Items[i]^.CreateDate + ' LCDate=' + Items[i]^.LastChange);
    end;    *)
end;

destructor TNoteInfoList.Destroy;
var
I : integer;
begin
    for I := 0 to Count-1 do
        dispose(Items[I]);
    inherited;
end;

function TNoteInfoList.Get(Index: integer): PNoteInfo;
begin
    Result := PNoteInfo(inherited get(Index));
end;

{ ---------------- End of TNoteInfoList ---------------- }



function GetTitleFromFFN(ServerPath: string; Rev : integer; ID: string): string;
var
     Doc : TXMLDocument;
     Node : TDOMNode;
     FFName : string;
begin
     FFName := GetRevisionDirPath(ServerPath, Rev, ID);
     if not FileExists(FFName) then
         exit('');
     ReadXMLFile(Doc, FFName);
     try
         Node := Doc.DocumentElement.FindNode('title');
         result := Node.FirstChild.NodeValue;
     finally
         Doc.free;
     end;
end;


function GetRevisionDirPath(ServerPath: string; Rev: integer; NoteID : string = ''): string;
begin
    result := ServerPath
        + inttostr(Rev div 100) + pathDelim + inttostr(rev) + pathDelim;
    if NoteID <> '' then begin
        result := result + NoteID;
        if not result.EndsWith('note') then
            result := result + '.note';
    end;
end;

function UsingRightRevisionPath(ServerPath: string; Rev: integer): boolean;
var
    FullDirName : string;
begin
    FullDirname := GetRevisionDirPath(ServerPath, Rev);
    // debugln('Right sync Dir is ' + FullDirName);
    Result :=  DirectoryExists(FullDirName);   // we hope its in 'wrong' place ....
    // Just to be carefull ...
    {FullDirname := appendpathdelim(serverPath) + '0' + pathDelim + inttostr(rev);
    if DirectoryExists(FullDirName) then begin
        debugln('ERROR, Sync Repo has two sync directories for rev no ' + inttostr(rev));
        debugln('We will use ' + GetRevisionDirPath(ServerPath, Rev));
    end; }
    //result := true;
end;


function ReadManifest(var RMetaData : TNoteInfoList; const FFName : string; var EString : string) : boolean;
var
    Doc : TXMLDocument;
    NodeList : TDOMNodeList;
    Node : TDOMNode;
    NoteInfo : PNoteInfo;
    j : integer;
begin
    EString := '';
    if not FileExists(FFNAme) then exit(false);
    if RMetaData <> Nil then FreeAndNil(RMetaData);
    RMetaData := TNoteInfoList.Create;
    Result := true;
    try
    	try
            ReadXMLFile(Doc, FFName);
            // 2nd line of server looks like this <sync revision="498" server-id="C20866A3-9D6D-415C-964A-2485E031D1A4">
            RMetaData.ServerID := Doc.DocumentElement.GetAttribute('server-id');
            RMetaData.LastRev := strtoint(Doc.DocumentElement.GetAttribute('revision'));  // used only to initialise Revision at startup
    		NodeList := Doc.DocumentElement.ChildNodes;
    		if assigned(NodeList) then begin
        		for j := 0 to NodeList.Count-1 do begin
                    new(NoteInfo);
                    NoteInfo^.ForceUpload := false;
                    NoteInfo^.Action:=SyUnset;
                    Node := NodeList.Item[j].Attributes.GetNamedItem('id');
                    NoteInfo^.ID := Node.NodeValue;                             // ID does not contain '.note';
					Node := NodeList.Item[j].Attributes.GetNamedItem('rev');
                    NoteInfo^.Rev := strtoint(Node.NodeValue);                      // what happens if its empty ?
                    Node := NodeList.Item[j].Attributes.GetNamedItem('last-change-date');
                    if assigned(node) then                                   // We can assume all manifest entries must have a LCD here.
                    NoteInfo^.LastChange:=Node.NodeValue;
                    RMetaData.Add(NoteInfo);
                end;
		    end;
        except
              on E: EAccessViolation do begin
                  Result := false;	// probably means we did not find an expected attribute
                  EString := 'Remote Manifest Error ' + E.Message;
              end;
              on E: EConvertError do begin
                  EString := 'Remote Manifest Error ' + E.Message;
                  Result := false;	// probably means we did not find an expected attribute
              end;
    	end;
	finally
        Doc.Free;
	end;
end;



end.

