unit transfile;

{ A unit that does the file transfer side of a FileSync operation
  *  Copyright (C) 2018 David Bannon
  *  See attached licence file.

  HISTORY
  2018/10/25  Much testing, support for Tomdroid.
  2018/06/05  Change to doing Tomboy's sync dir names, rev 431 is in ~/4/341
  2019/10/17  Ensure DownloadFile returns true remote dir name, irrespective of above.
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, trans, SyncUtils, LazLogger;

type

  { TFileSync }

  TFileSync = Class(TTomboyTrans)
    private
        function GetRemoteNotePath(Rev: integer; NoteID : string = ''): string;
        function GetRemoteNoteLastChange(const ID : string; rev : Integer; out Error : string) : string;
    public
        function SetTransport(): TSyncAvailable; override;
        function TestTransport() : TSyncAvailable; override;
        function GetNotes(const NoteMeta : TNoteInfoList) : boolean; override;
        function PushChanges(notes : TNoteInfoList): boolean; override;
        function DoRemoteManifest(const RemoteManifest : string) : boolean; override;
        function IDLooksOK() : boolean; Override;
        function getPrefix(): string; Override;
    end;


implementation

uses laz2_DOM, laz2_XMLRead, LazFileUtils, FileUtil;

{ TFileSync }

function TFileSync.getPrefix(): string;
begin
  Result := 'file';
end;

function TFileSync.SetTransport(): TSyncAvailable;
begin
    Result := SyncReady;
end;

function TFileSync.TestTransport(): TSyncAvailable;
var
    Doc : TXMLDocument;
    GUID : TGUID;
    repo : String;
    ManExists, ZeroExists : boolean; // for readability of code only
begin

    setParam('RemoteAddess',AppendPathDelim(ChompPathDelim(getParam('RemoteAddress'))));
    repo := getParam('RemoteAddress');

    if not DirectoryExists(repo) then
        if not DirectoryExists(repo) then begin    // try again because it might be just remounted.
           ErrorString := 'Remote Dir does not exist : ' + repo;
	   exit(SyncNoRemoteDir);
        end;

    if not DirectoryIsWritable(repo) then begin
      ErrorString := 'Remote directory NOT writable ' + repo;
      exit(SyncNoRemoteWrite);
    end;

    ManExists := FileExists(repo + 'manifest.xml');
    ZeroExists := DirectoryExists(repo + '0');

    if (ManExists) and (not ZeroExists) then
    begin
        ErrorString := 'Apparently damaged repo, missing 0 dir at ' + repo;
    	exit(SyncBadRemote);
    end;
    if (not ManExists) and (ZeroExists) then
    begin
        ErrorString := 'Apparently damaged repo, missing manifest at ' + repo;
    	exit(SyncBadRemote);
    end;

    if (not ManExists) and (not ZeroExists) then
    begin
        CreateGUID(GUID);
        ServerID := copy(GUIDToString(GUID), 2, 36);      // it arrives here wrapped in {}
        ServerRev := -1;
        ForceDirectoriesUTF8(GetRemoteNotePath(0));
        exit(SyncReady);
    end;

    try
       ReadXMLFile(Doc, repo + 'manifest.xml');
    except on E:Exception do
       begin
           ErrorString := E.message;
           exit(SyncXMLERROR);
       end;
    end;

    try
       ServerID := Doc.DocumentElement.GetAttribute('server-id');
       ServerRev := strtoint(Doc.DocumentElement.GetAttribute('revision'));
    except on E:Exception do
       begin
            ErrorString := E.message;
            Doc.Free;
            exit(SyncXMLERROR);
       end;
    end;
    Doc.Free;

    if not IDLooksOK()
    then begin
        ErrorString := 'Invalid ServerID '+ServerID;
        exit(SyncXMLError);
    end;

    Result := SyncReady;
end;

function TFileSync.GetNotes(const NoteMeta: TNoteInfoList): boolean;
var
    Doc : TXMLDocument;
    NodeList : TDOMNodeList;
    Node : TDOMNode;
    j : integer;
    NoteInfo : PNoteInfo;
    manifest,note : String;
begin
    if NoteMeta = Nil then begin
        ErrorString := 'Passed an uncreated list to GetNotes()';
        exit(False);
    end;

    manifest:= getParam('RemoteAddress') + 'manifest.xml';
    if not FileExists(manifest) then exit(true);

    try
         ReadXMLFile(Doc, manifest);
    except on E:Exception do begin debugln(E.message); exit(false); end;
    end;

    NodeList := Doc.DocumentElement.ChildNodes;

    if not assigned(NodeList) then
    begin
         Doc.Free;
         debugln('We failed to read XML children in the remote manifest file '+manifest);
         exit(false);
    end;

    for j := 0 to NodeList.Count-1 do
    begin
         new(NoteInfo);

         NoteInfo^.Action:=SynUnset;
         Node := NodeList.Item[j].Attributes.GetNamedItem('guid');
         NoteInfo^.ID := Node.NodeValue;
         If(not NoteIdLooksOk(NoteInfo^.ID)) then begin FreeAndNil(NoteInfo); continue; end;

         Node := NodeList.Item[j].Attributes.GetNamedItem('latest-revision');
         NoteInfo^.Rev := strtoint(Node.NodeValue);

         note := GetRemoteNotePath(NoteInfo^.Rev, NoteInfo^.ID);
         if(FileToNote(note, NoteInfo ))
              then NoteMeta.Add(NoteInfo)
              else FreeAndNil(NoteInfo);
    end;

    Doc.Free;

    Debugln('Transfile.ReadRemoteManifest - read OK');
    Result := true;

end;



function TFileSync.PushChanges(notes : TNoteInfoList): boolean;
var
    i : integer;
    d,n : string;
    f : TextFile;
    note : PNoteInfo;
begin
   d := GetRemoteNotePath(ServerRev + 1);
   ForceDirectoriesUTF8(d);


   for i := 0 to notes.Count -1 do
   begin
       note := notes.Items[i];

       if(not (note^.Action in [SynUploadEdit, SynUploadNew])) then continue;

       debugln('Uploading ' + note^.ID );
       n := GetRemoteNotePath(ServerRev + 1,note^.ID);

       try
            AssignFile(f,n);
            WriteLn(f,'<?xml version="1.0" encoding="utf-8"?>');
            WriteLn(f,'<note version="' + note^.Version + '" xmlns:link="http://beatniksoftware.com/tomboy/link" xmlns:size="http://beatniksoftware.com/tomboy/size" xmlns="http://beatniksoftware.com/tomboy">');
            WriteLn(f,'<title>' + note^.Title + '</title>');
            WriteLn(f,'<create-date>' + note^.CreateDate + '</create-date>');
            WriteLn(f,'<last-change-date>' + note^.LastChange + '</last-change-date>');
            WriteLn(f,'<last-metadata-change-date>' + note^.LastMetaChange + '</last-metadata-change-date>');
            WriteLn(f,'<width>' + IntToStr(note^.Width) + '</width>');
            WriteLn(f,'<height>' + IntToStr(note^.Height) + '</height>');
            WriteLn(f,'<x>' + IntToStr(note^.X) + '</x>');
            WriteLn(f,'<y>' + IntToStr(note^.Y) + '</y>');
            WriteLn(f,'<selection-bound-position>' + IntToStr(note^.SelectBoundPosition) + '</selection-bound-position>');
            WriteLn(f,'<cursor-position>' + IntToStr(note^.CursorPosition) + '</cursor-position>');
            WriteLn(f,'<pinned>' + BoolToStr(note^.Pinned) + '</pinned>');
            WriteLn(f,'<open-on-startup>' + BoolToStr(note^.OpenOnStartup) + '</open-on-startup>');
            WriteLn(f,'<text xml:space="preserve"><note-content version=">' + note^.Version + '">' + note^.Content + '</note-content></text> ');
            CloseFile(f);
       except on E:Exception do
           begin
              ErrorString := E.message;
              debugln(ErrorString);
              exit(false);
           end;
       end;
   end;
   result := True;
end;

function TFileSync.DoRemoteManifest(const RemoteManifest: string): boolean;
var
    d : String;
    f : TextFile;
begin
    debugln('DoRemote Manifest ' + RemoteManifest);

    try
       AssignFile(f,getParam('RemoteAddress') + 'manifest.xml');
       Rewrite(f);
       Write(f,RemoteManifest);
       Close(f);

       d := GetRemoteNotePath(ServerRev + 1);
       ForceDirectoriesUTF8(d);

       AssignFile(f,d + 'manifest.xml');
       Rewrite(f);
       Write(f,RemoteManifest);
       Close(f)
    except on E:Exception do begin
       ErrorString := E.message;
       debugln(ErrorString);
       exit(false);
       end;
    end;

    Result := True;
end;


function TFileSync.IDLooksOK() : boolean;
begin
    if length(ServerID) <> 36 then exit(false);
    if pos('-', ServerID) <> 9 then exit(false);
    result := True;
end;

function TFileSync.GetRemoteNotePath(Rev: integer; NoteID : string = ''): string;
var
   s,path : String;
   SearchResult : TSearchRec;
begin

    path := getParam('RemoteAddress');

    if ((Rev<0) or (FindFirst(path + '*.note',faAnyFile,SearchResult) = 0))
    then s := path
    else s := appendpathDelim(path
        + inttostr(Rev div 100) + pathDelim + inttostr(rev));

    if NoteID <> '' then
        s := s + NoteID + '.note';

    Result := s;
end;

function TFileSync.GetRemoteNoteLastChange(const ID : string; rev : Integer; out Error : string) : string;
var
   Doc : TXMLDocument;
   Node : TDOMNode;
   filename : string;
begin
   filename := GetRemoteNotePath(rev,ID);

   if not FileExists(filename) then
   begin
        Error := 'ERROR - File not found, cant read note change date for remote ' +  filename;
        exit('');
    end;

    try
       ReadXMLFile(Doc, filename);
       Node := Doc.DocumentElement.FindNode('last-change-date');
       Result := Node.FirstChild.NodeValue;
    except on E:Exception do begin
       Error := E.message;
       debugln(Error);
       Result := '';
       end;
    end;
end;

end.

