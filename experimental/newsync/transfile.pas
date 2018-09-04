unit transfile;

{ A unit that does the file transfer side of a FileSync operation

This is one implementation of layer 4 from here -
https://github.com/tomboy-notes/tomboy-ng/wiki/Another-Sync-Model#implementation

}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, trans, SyncUtils;

type

  { TFileSync }

  TFileSync = Class(TTomboyTrans)
    private
        function GetNoteLastChange(const FullFileName: string): string;
            // Reads the (filesync) remote Manifest for synced note details. It gets ID, RevNo
            // and, if its there the LastChangeDate. If its not there, looks in File.
        function ReadRemoteManifest(NoteMeta: TNoteInfoList; LocRev : integer): boolean;

    public
        //RemoteDir : string; // where the remote filesync repo lives.

        function GetNewNotes(NoteMeta : TNoteInfoList; LocRev : integer) : boolean; override;
        function DownloadNotes(const DownLoads : TNoteInfoList) : boolean; override;
        function DeleteNote(const ID : string{; var NewRev : integer}) : boolean; override;
        function UploadNotes(const Uploads : TStringList; var NewRev : integer) : boolean; override;
  end;

implementation

uses laz2_DOM, laz2_XMLRead, LazFileUtils, FileUtil, LazLogger;

{ TFileSync }

function TFileSync.GetNewNotes(NoteMeta: TNoteInfoList; LocRev : integer): boolean;
begin
    // Read the remote mainfest and fill in as much as we can
    // pretty much just copy from TTomboyFileSync.ReadRemoteManifest()
    // but only return with entries whose rev number is greater than LocRev
  if NoteMeta = Nil then begin
      ErrorString := 'Passed an uncreated list to GetNewNotes()';
      exit(False);
  end;
  //    NoteMeta.Free;
  //NoteMeta := TNoteInfoList.Create;

  // OK, as a FileSync, I expect RemoteAddress to be an accessible dir containing a mainfest.
  if not DirectoryExists(RemoteAddress) then begin
        ErrorString := 'Remote Dir does not exist ' + RemoteAddress;
        exit(False);
      end;
  if not FileExists(RemoteAddress + 'manifest.xml') then begin
        ErrorString := 'Remote manifest does not exist ' + RemoteAddress + 'manifest.xml';
        exit(False);
  end;
  ReadRemoteManifest(NoteMeta, LocRev);
  result := True;
end;


function TFileSync.ReadRemoteManifest(NoteMeta: TNoteInfoList; LocRev : integer) : boolean;
var
    Doc : TXMLDocument;
    NodeList : TDOMNodeList;
    Node : TDOMNode;
    j : integer;
    NoteInfo : PNoteInfo;
begin
    Result := true;
    try
    	try
    		ReadXMLFile(Doc, RemoteAddress + 'manifest.xml');
    		NodeList := Doc.DocumentElement.ChildNodes;
    		if assigned(NodeList) then begin
        		for j := 0 to NodeList.Count-1 do begin
                    new(NoteInfo);
                    Node := NodeList.Item[j].Attributes.GetNamedItem('id');
                    NoteInfo^.ID := Node.NodeValue;									// ID does not contain '.note';
					Node := NodeList.Item[j].Attributes.GetNamedItem('rev');
                    NoteInfo^.Rev := strtoint(Node.NodeValue);                      // what happens if its empty ?
                    {if NoteInfo^.Rev <= LocRev then begin
                        dispose(NoteInfo);
                        continue;
                    end;}
                    Node := NodeList.Item[j].Attributes.GetNamedItem('last-change-date');
                    if assigned(node) then
                            NoteInfo^.LastChange:=Node.NodeValue
                    else if NoteInfo^.Rev > LocRev then                             // Only bother to get it if fresh note
                        NoteInfo^.LastChange := GetNoteLastChange(RemoteAddress
                                + '0' + pathdelim + inttostr(NoteInfo^.Rev)
                                + pathdelim + NoteInfo^.ID + '.note');
                    if NoteInfo^.LastChange <> '' then
                        NoteInfo^.LastChangeGMT := GetGMTFromStr(NoteInfo^.LastChange);
                    NoteMeta.Add(NoteInfo);
                end;
		end;
		finally
            Doc.Free;
		end;
	except
      on E: EAccessViolation do Result := false;	// probably means we did not find an expected attribute
      on E: EFOpenError do Result := False;		// File is not present.
	end;
    if Result = True then
    	Debugln('Seems we read the manifest OK')
    else
        DebugLn('We failed to read the remote manifest file ', RemoteAddress + 'manifest.xml');
end;

function TFileSync.GetNoteLastChange(const FullFileName : string) : string;
var
	Doc : TXMLDocument;
	Node : TDOMNode;
    LastChange : string;
begin
    if not FileExistsUTF8(FullFileName) then begin
        DebugLn('ERROR - File not found, cant read note change date for remote ',  FullFileName);
        exit('');
	end;
	try
		ReadXMLFile(Doc, FullFileName);
		Node := Doc.DocumentElement.FindNode('last-change-date');
        Result := Node.FirstChild.NodeValue;
	finally
        Doc.free;		// xml errors are caught in calling process
	end;
end;

function TFileSync.DownloadNotes(const DownLoads: TNoteInfoList): boolean;
var
    I : integer;
begin
    if not DirectoryExists(NotesDir + 'Backup') then
        if not ForceDirectory(NotesDir + 'Backup') then begin
            ErrorString := 'Failed to create Backup directory.';
            exit(False);
        end;
    for I := 0 to DownLoads.Count do begin
        if FileExists(NotesDir + Downloads.Items[I]^.ID + '.note') then
            // First make a Backup copy
            if not CopyFile(NotesDir + Downloads.Items[I]^.ID + '.note',
                        NotesDir + 'Backup' + PathDelim + Downloads.Items[I]^.ID + '.note') then begin
                ErrorString := 'Failed to copy file to Backup ' + NotesDir + Downloads.Items[I]^.ID + '.note';
                exit(False);
            end;
        // OK, now copy the file.
        if not CopyFile(RemoteAddress + '0' + pathdelim + inttostr(DownLoads.Items[i]^.Rev)
                + pathdelim + Downloads.Items[I]^.ID + '.note',
                NotesDir + Downloads.Items[I]^.ID + '.note') then begin
                    ErrorString := 'Failed to copy ' + Downloads.Items[I]^.ID + '.note';
                    exit(False);
                end;
    end;
    result := True;
end;

function TFileSync.DeleteNote(const ID: string): boolean;
begin
  // need a beter understaning here, not ready to implement yet !!!!!!!!!!!!
  result := True;
end;

function TFileSync.UploadNotes(const Uploads: TStringList; var NewRev : integer): boolean;
begin
  // Create a new server revision and upload the list of notes we have to it.
  result := True;
end;



end.

