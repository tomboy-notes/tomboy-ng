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
        function ReadRemoteManifest(const NoteMeta: TNoteInfoList; const LocRev : integer): boolean;

    public
        //RemoteDir : string; // where the remote filesync repo lives.
        function TestTransport(out ServerID : string) : TSyncAvailable; override;
        function GetNewNotes(const NoteMeta : TNoteInfoList; const LocRev : integer) : boolean; override;
        function DownloadNotes(const DownLoads : TNoteInfoList) : boolean; override;
        function DeleteNote(const ID : string; const ExistRev : integer) : boolean; override;
        function UploadNotes(const Uploads : TStringList) : boolean; override;
        function DoRemoteManifest(const RemoteManifest : string) : boolean; override;
        function DownLoadNote(const ID : string; const RevNo : Integer) : string; Override;
  end;

implementation

uses laz2_DOM, laz2_XMLRead, LazFileUtils, FileUtil, LazLogger;

{ TFileSync }

function TFileSync.TestTransport(out ServerID : string): TSyncAvailable;
var
    Doc : TXMLDocument;
begin
    if not DirectoryExists(RemoteAddress) then begin
	    ErrorString := 'Remote Dir does not exist ' + RemoteAddress;
	    exit(SyncNoRemoteDir);
    end;
    if not DirectoryExists(RemoteAddress + '0') then begin
	    ErrorString := 'Remote Revision Dir "0" does not exist ' + RemoteAddress;
	    exit(SyncNoRemoteDir);
    end;

    if not FileExists(RemoteAddress + 'manifest.xml') then begin
	    ErrorString := 'Remote manifest does not exist ' + RemoteAddress + 'manifest.xml';
	    exit(SyncNoRemoteMan);
    end;
    if not FileIsWritable(RemoteAddress + 'manifest.xml') then begin
	    ErrorString := 'Remote manifest NOT writable ' + RemoteAddress + 'manifest.xml';
	    exit(SyncNoRemoteWrite);
    end;
    try
	        try
	            ReadXMLFile(Doc, RemoteAddress + 'manifest.xml');
	            ServerID := Doc.DocumentElement.GetAttribute('server-id');
                RemoteServerID := ServerID;
                { ToDo - must check for error on next line }
                RemoteServerRev := strtoint(Doc.DocumentElement.GetAttribute('revision'));
		    finally
	            Doc.Free;
		    end;
	except
      on E: EAccessViolation do begin
          ErrorString := E.Message;
          exit(SyncXMLERROR);	// probably means we did not find an expected attribute
	  end;
	  on E: EFOpenError do begin
            ErrorString := E.Message;
            exit(SyncNoRemoteMan);		// File is not present.
	  end;
	end;
    if 36 <> length(ServerID) then begin
        ErrorString := 'Invalid ServerID';
        exit(SyncXMLError);
    end;
    Result := SyncReady;
end;

function TFileSync.GetNewNotes(const NoteMeta: TNoteInfoList; const LocRev : integer): boolean;
begin
    // Read the remote mainfest and fill in as much as we can
    // pretty much just copy from TTomboyFileSync.ReadRemoteManifest()
    // but only return with last-change-date as well.
  if NoteMeta = Nil then begin
      ErrorString := 'Passed an uncreated list to GetNewNotes()';
      exit(False);
  end;
  ReadRemoteManifest(NoteMeta, LocRev);
  result := True;
end;


function TFileSync.ReadRemoteManifest(const NoteMeta: TNoteInfoList; const LocRev : integer) : boolean;
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
                    NoteInfo^.Action:=SyUnset;
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
    // LastChange : string;
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

function TFileSync.DeleteNote(const ID: string; const ExistRev : integer ): boolean;
begin
  // need a beter understaning here, not ready to implement yet !!!!!!!!!!!!
  result := True;
end;

function TFileSync.UploadNotes(const Uploads: TStringList): boolean;
var
    Index : integer;
begin
  for Index := 0 to Uploads.Count -1 do begin
      if not copyFile(NotesDir + Uploads.Strings[Index] + '.note',
                self.RemoteAddress + inttostr(RemoteServerRev + 1) + PathDelim + Uploads.Strings[Index] + '.note')
      then begin
          ErrorString := 'ERROR copying ' + NotesDir + Uploads.Strings[Index] + '.note to '
            + RemoteAddress + inttostr(RemoteServerRev + 1) + PathDelim + Uploads.Strings[Index] + '.note';
          debugln(ErrorString);
          exit(False);
	  end;
  end;
  result := True;
end;

function TFileSync.DoRemoteManifest(const RemoteManifest: string): boolean;
begin
  if not ForceDirectoriesUTF8(RemoteAddress + inttostr(self.RemoteServerRev + 1)) then begin
      ErrorString := 'Failed to create new remote revision dir '
                + inttostr(self.RemoteServerRev + 1);
      debugln(ErrorString);
      exit(False);
  end;
  if not CopyFile(RemoteAddress + 'manifest.xml', RemoteAddress + inttostr(RemoteServerRev + 1)
                + PathDelim + ('manifest.xml')) then begin
      ErrorString := 'Failed to move root remote manifest to dir '
                + RemoteAddress + inttostr(RemoteServerRev + 1);
      debugln(ErrorString);
      exit(False);
  end;
  if not CopyFile(RemoteManifest, RemoteAddress + 'manifest.xml') then begin
      ErrorString := 'Failed to move new root remote manifest file ' + RemoteManifest;
      debugln(ErrorString);
      exit(False);
  end;
  Result := True;
end;

function TFileSync.DownLoadNote(const ID: string; const RevNo: Integer): string;
begin
    Result := RemoteAddress + inttostr(RevNo) + PathDelim + ID + '.note';
end;



end.

