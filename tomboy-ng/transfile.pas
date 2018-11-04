unit transfile;

{ A unit that does the file transfer side of a FileSync operation
  *  Copyright (C) 2018 David Bannon
  *  See attached licence file.

  HISTORY
  2018/10/25  Much testing, support for Tomdroid.
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
            // and, if its there the LastChangeDate. If LCD is not in manifest and GetLCD
            // is True, gets it from the file.
        function ReadRemoteManifest(const NoteMeta: TNoteInfoList; const GetLCD : boolean): boolean;


    public
        //RemoteDir : string; // where the remote filesync repo lives.
        function TestTransportEarly(out ManPrefix : string): TSyncAvailable; override;
        function TestTransport() : TSyncAvailable; override;
        function GetNewNotes(const NoteMeta : TNoteInfoList; const GetLCD : boolean) : boolean; override;
        function DownloadNotes(const DownLoads : TNoteInfoList) : boolean; override;
        function DeleteNote(const ID : string; const ExistRev : integer) : boolean; override;
        function UploadNotes(const Uploads : TStringList) : boolean; override;
        function DoRemoteManifest(const RemoteManifest : string) : boolean; override;
        function DownLoadNote(const ID : string; const RevNo : Integer) : string; Override;
        // function SetRemoteRepo(ManFile : string = '') : boolean; override;
  end;


implementation

uses laz2_DOM, laz2_XMLRead, LazFileUtils, FileUtil, LazLogger;

{ TFileSync }

function TFileSync.TestTransportEarly(out ManPrefix : string): TSyncAvailable;
begin
    ManPrefix := '';
    Result := SyncReady;
end;

function TFileSync.TestTransport(): TSyncAvailable;
var
    Doc : TXMLDocument;
    GUID : TGUID;
    ManExists, ZeroExists : boolean; // for readability of code only
begin
    RemoteAddress := AppendPathDelim(RemoteAddress);
    if not DirectoryExists(RemoteAddress) then begin
	    ErrorString := 'Remote Dir does not exist ' + RemoteAddress;
	    exit(SyncNoRemoteDir);
    end;
    if not DirectoryIsWritable(RemoteAddress) then begin
      ErrorString := 'Remote directory NOT writable ' + RemoteAddress;
      exit(SyncNoRemoteWrite);
    end;
    if ANewRepo then begin
        CreateGUID(GUID);
        ServerID := copy(GUIDToString(GUID), 2, 36);      // it arrives here wrapped in {}
        RemoteServerRev := -1;
        exit(SyncReady);
    end;
    ManExists := FileExists(RemoteAddress + 'manifest.xml');
    ZeroExists := DirectoryExists(RemoteAddress + '0');
    if (not ManExists) and (not ZeroExists) then begin
        ErrorString := 'Remote dir does not contain a Repo ' + RemoteAddress;
    	exit(SyncNoRemoteRepo);
	end;
    if (ManExists) and (not ZeroExists) then begin
        ErrorString := 'Apparently damaged repo, missing 0 dir at ' + RemoteAddress;
    	exit(SyncBadRemote);
    end;
	if (not ManExists) and (ZeroExists) then begin
        ErrorString := 'Apparently damaged repo, missing manifest at ' + RemoteAddress;
    	exit(SyncBadRemote);
    end;
    // If to here, looks and feels like a repo, lets see what it can tell !
    try
	        try
	            ReadXMLFile(Doc, RemoteAddress + 'manifest.xml');

	            ServerID := Doc.DocumentElement.GetAttribute('server-id');
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

function TFileSync.GetNewNotes(const NoteMeta: TNoteInfoList; const GetLCD : boolean): boolean;
begin
    if NoteMeta = Nil then begin
        ErrorString := 'Passed an uncreated list to GetNewNotes()';
        exit(False);
    end;
    if FileExists(RemoteAddress + 'manifest.xml') then
        ReadRemoteManifest(NoteMeta, GetLCD);           // No remote manifest is aceptable here, new repo
    result := True;
end;


function TFileSync.ReadRemoteManifest(const NoteMeta: TNoteInfoList; const GetLCD : boolean) : boolean;
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
                    Node := NodeList.Item[j].Attributes.GetNamedItem('last-change-date');
                    if assigned(node) then
                            NoteInfo^.LastChange:=Node.NodeValue
                    else if GetLCD then                // Only bother to get it if we really need it
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
    if Result = True then begin
    	if debugmode then Debugln('Transfile.ReadRemoteManifest - read OK');
    end else
        DebugLn('We failed to read the remote manifest file ', RemoteAddress + 'manifest.xml');
end;


function TFileSync.GetNoteLastChange(const FullFileName : string) : string;
begin
    Result := GetNoteLastChangeSt(FullFileName, ErrorString);   // syncutils function
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
    for I := 0 to DownLoads.Count-1 do begin
        if DownLoads.Items[I]^.Action = SyDownLoad then begin
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
    end;
    result := True;
end;

function TFileSync.DeleteNote(const ID: string; const ExistRev : integer ): boolean;
begin
    // I _THINK_ all that happens is deleted note is not listed in remote manifest
    // and that is done. But other thansport modes might need to do something here ?
  result := True;
end;

function TFileSync.UploadNotes(const Uploads: TStringList): boolean;
var
    Index : integer;
begin
  for Index := 0 to Uploads.Count -1 do begin
      if DebugMode then debugln('Uploading ' + Uploads.Strings[Index] + '.note');
      if not copyFile(NotesDir + Uploads.Strings[Index] + '.note',
                RemoteAddress + '0' + PathDelim + inttostr(RemoteServerRev + 1) + PathDelim + Uploads.Strings[Index] + '.note')
      then begin
          ErrorString := 'ERROR copying ' + NotesDir + Uploads.Strings[Index] + '.note to '
            + RemoteAddress + '0' + PathDelim + inttostr(RemoteServerRev + 1) + PathDelim + Uploads.Strings[Index] + '.note';
          debugln(ErrorString);
          exit(False);
	  end;
  end;
  result := True;
end;

function TFileSync.DoRemoteManifest(const RemoteManifest: string): boolean;
begin
    // I think that ForceDir will make intermediate dir too ......
    if not ForceDirectoriesUTF8(RemoteAddress + '0' + PathDelim + inttostr(self.RemoteServerRev + 1)) then begin
        ErrorString := 'Failed to create new remote revision dir '
                + inttostr(self.RemoteServerRev + 1);
        debugln(ErrorString);
        exit(False);
    end;
  if debugmode then  debugln('Remote Manifest is ' + RemoteManifest);
  if not CopyFile(RemoteManifest, RemoteAddress + 'manifest.xml') then begin
      ErrorString := 'Failed to move new root remote manifest file ' + RemoteManifest;
      debugln(ErrorString);
      exit(False);
  end;
  if not CopyFile(RemoteManifest, RemoteAddress + '0' + PathDelim + inttostr(RemoteServerRev + 1)
        + PathDelim + 'manifest.xml') then begin
      ErrorString := 'Failed to move new remote manifest file to revision dir';
      debugln(ErrorString);
      exit(False);
  end;

  Result := True;
end;

function TFileSync.DownLoadNote(const ID: string; const RevNo: Integer): string;
begin
    Result := RemoteAddress + '0' + PathDelim + inttostr(RevNo) + PathDelim + ID + '.note';
end;


// WARNING -I don't think we need this function any more
{
function TFileSync.SetRemoteRepo(ManFile: string=''): boolean;
begin
    if ManFile = '' then begin
        RemoteServerRev := -1;      // because Sync will add 1 when it decides a new rev happens.
        AnewRepo := True;
	end else begin
        Result := ForceDirectory(RemoteAddress + '0' + pathDelim + inttostr(self.RemoteServerRev + 1));
        if not Result then begin
            self.ErrorString := 'Cannot create remote rev dir ' + inttostr(self.RemoteServerRev + 1);
            exit(False);
		end;
        result := copyfile(ManFile, RemoteAddress + 'manifest.xml');
            if not Result then begin
                self.ErrorString := 'Cannot copy new remote manifest to ' + RemoteAddress;
                exit(False);
    		end;
	end;
end;
}


end.

