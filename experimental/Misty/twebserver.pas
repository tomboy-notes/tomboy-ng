unit TWebserver;

{   Part of the tomboy-ng Misty project.

    Provides a set of web services that allows tomboy-ng to sync remotely
    and allows end user to access synced notes from  browser.
    Very much under development.


    Copyright (C) 2017-2025 David Bannon

    License:
    This code is licensed under MIT License, see the file License.txt
    or https://spdx.org/licenses/MIT.html  SPDX short identifier: MIT
}

{$mode objfpc}{$H+}   {$ifdef windows}{$apptype console}{$endif}
{$define UseCThreads}

{   Revision - we the current Revision number in memory at all time. At startup we read
    it from the manifestif preset, set to -1 otherwise.  When a Sync sends us a new or
    updated note, we place it in Revision + 1 and do not update Revision until a new
    manifest is sent. That new manifest will provide us with new current revision, which
    should always be Revision + 1, log error otherwise ?

    Similarly, theclient will start with -1, all notes uploaded will assume that
    +1 until it posts the new manifest. The manifest is always written in the client.

    A note is saved in ServerHome + (Rev+1) + $ID.note

    We don't delete notes, just don't list them in the updated manifest.

    When a note is "quill edited", the save process will same following above rule
    and then immediatly increment Revision.


}
interface

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  ssync_utils,    // data sync utils based on tomboy-ng code
  sysutils, Classes, fphttpserver, fpmimetypes, {wmecho,} exporthtml, httpdefs,

  html2note;
  // LazLogger, LazFileUtils;      // does that introduce a LCL dependency ?


type TFileStatus = (fsOK, fsNoNote, fsNoEntry, fsNoManifest);

Type

  { TTestHTTPServer }

  { TMistyHTTPServer }

  TMistyHTTPServer = Class(TFPHTTPServer)
  private
    FBaseDir : String;
//    FCount : Integer;
    FMimeLoaded : Boolean;
    FMimeTypesFile: String;
    Revision : integer;           // The last used Revision number, new sync +1. Inc when savng new manifest.

                        // Sync - Upload a file from the users PC tomboy-ng process. Not from a browser.
                        // This is invoked if AReq.Files.count > 0. Means a note or mainfest has been sent.
                        // Note will be saved in ServerHome using revision based directory structure
                        // Note commented out test code to trigger uploadfrom browser.
                        // Will return to client 200 on success or 422 on fail to save (permission, disk space).
    procedure DoCommandUpload(AReq : TFPHTTPConnectionRequest; AResp : TFPHTTPConnectionResponse);
                        // Sync - This might be called to send a note - /DOWNLOAD/$ID.note
                        // or, still an xml file, the manifest - /MANIFEST back to client -ng
    procedure DoCommandDownload(AReq : TFPHTTPConnectionRequest; AResp : TFPHTTPConnectionResponse);

                        // Interactive, user has requested to see a list of clickable notes.
    procedure DoCommandListNotes(AReq : TFPHTTPConnectionRequest; AResp : TFPHTTPConnectionResponse);
                        // Interactive. When a web user has clicked on a note, bundles up the note in HTML
                        // format adds code to involve quill and applies the three Editor_? templates.
                        // This start of Edit process.
    procedure DoCommandEdit(AReq : TFPHTTPConnectionRequest; AResp : TFPHTTPConnectionResponse);
                        // Interactve. A web user has sent back an edited Quill note. Convert back to
                        // note format and save it. Everything that arrives here does need
                        // to be saved ?  Need more validation.
    procedure DoCommandAcceptEdit(AReq : TFPHTTPConnectionRequest; AResp : TFPHTTPConnectionResponse);

    //procedure GenerateFileList(FFName: string; AResp: TFPHTTPConnectionResponse);
    function GetFullFileName(FName: string; var FFName: string): TFileStatus;

    procedure SetBaseDir(const AValue: String);
                        // Writes a new manifest when a user initiated web edit of a note is returned
                        // to the server. Most data derived from  MetaData but not Revision. Normally
                        // written into the curret Rev directory and then copied to serverhome. Only
                        // changes would be Revision, note rev and note LCD. Second and Third line of manifest -
                        // <sync revision="1" server-id="53576166-6C50-4FB6-9AAB-CDC289D084CC">
                        // <note id="88299c65-8858-44d0-9fa2-e78ca77749ba" rev="0" last-change-date="2020-02-19T02:41:48.0510000+00:00" />
    function WriteNewManifest(FFName : string) : boolean;
  Protected
    Procedure DoIdle(Sender : TObject);
    procedure CheckMimeLoaded;

    Property MimeLoaded : Boolean Read FMimeLoaded;
  public
    procedure Startup;
    procedure HandleRequest(Var ARequest: TFPHTTPConnectionRequest;
                            Var AResponse : TFPHTTPConnectionResponse); override;
    Property BaseDir : String Read FBaseDir Write SetBaseDir;
    Property MimeTypesFile : String Read FMimeTypesFile Write FMimeTypesFile;

  end;


var
    Serv : TMistyHTTPServer;
    ExitNow : boolean;           // A semaphore set when ctrl-C received
    DebugMode : boolean;

implementation

uses FileUtil, LazFileUtils;


Var


  ErrorSt : string = '';       // Check this before exiting with an error
  ServerHome : string;
  // DebugMode : boolean = True;

const

    Editor_1 : string = '<html>';    // placeholders, load real content at run time
    Editor_2 : string = '';
    Editor_3 : string = '</html>';


    //Just a debug tool, probably delete at some stage
procedure SaveString(InString, OutFilePath: string);
var
    F: TextFile;
begin
    AssignFile(F, OutFilePath);
    try
        ReWrite(F);
        Write(F, InString);
    finally
      CloseFile(F);
    end;
end;

function LoadFromFile(const FFName : string; var TheSt : string) : boolean;
var
    InFile : TextFile;
    S : string;
begin
    result := True;
    if not FileExists(FFname) then begin
        ErrorSt := 'File does not exist : ' + FFName;
        exit(False);
    end;
    TheSt := '';
    AssignFile(InFile, FFName);
    try
        reset(InFile);
        while not eof(InFile) do begin
            readln(InFile, S);
            TheSt := TheSt + S + #10;
        end;
        CloseFile(InFile);
    except on E: EInOutError do begin
            ErrorSt := 'File Error ' + E.Message;
            result := False;
        end;
    end;
end;

{ TTestHTTPServer }

procedure TMistyHTTPServer.SetBaseDir(const AValue: String);
begin
  if FBaseDir=AValue then exit;
  FBaseDir:=AValue;
  If (FBaseDir<>'') then
    FBaseDir:=IncludeTrailingPathDelimiter(FBaseDir);
end;

function TMistyHTTPServer.WriteNewManifest(FFName: string): boolean;
var
    OutFile: TextFile;
    FPath : string;
    PNote : PNoteInfo;
begin
    FPath := ExtractFileDir(FFName);
    if not (DirectoryExists(FPath) or ForceDirectory(FPath)) then exit(False);
    // if not (DirectoryExists(FPath) or ForceDirectory(FPath)) then exit(False);
    AssignFile(OutFile, FFName);
    try
	    try
		    Rewrite(OutFile);
            writeln(OutFile, '<?xml version="1.0" encoding="utf-8"?>');
            writeln(OutFile, '<sync revision="' + Revision.ToString + '" server-id="' + MetaData.ServerID + '">');
            for PNote in MetaData do
                writeln(OutFile, '  <note id="' + PNote^.ID + '" rev="' + PNote^.Rev.ToString + '" last-change-date="' + PNote^.LastChange + '" />');
            writeln(OutFile, '</sync>');
		finally
	        CloseFile(OutFile);
		end;
    except
      on E: EInOutError do begin
          writeln('File handling error occurred. Details: ' + E.Message);
          exit(false);
	  end;
	end;
    // if to here, copy the file over top of existing main local manifest
    if fileexists(FFName) then                                                  // ToDo : and if it does not exist ?
        CopyFile(FFName, ServerHome + 'manifest.xml');                          // Note : Requires LazUtils/FileUtils !
    if debugmode then
       writeln('Have written server manifest to [' + FFName  + '] [' + ServerHome + 'manifest.xml' + ']');
    Result := True;
end;


procedure TMistyHTTPServer.DoIdle(Sender: TObject);
begin
  // Writeln('Idle, waiting for connections');
    if ExitNow then Serv.Active := False;      // Shutdown by a signal
end;

procedure TMistyHTTPServer.CheckMimeLoaded;
begin
  If (Not MimeLoaded) and (MimeTypesFile<>'') then
    begin
    MimeTypes.LoadFromFile(MimeTypesFile);
    FMimeLoaded:=true;
    end;
end;





(*
procedure TMistyHTTPServer.GenerateFileList(FFName : string; AResp : TFPHTTPConnectionResponse);   // doppy, this should be a stream ....
var
    STL : tstringlist;
    Info : TSearchRec;
begin
    STL := TStringList.Create();
    STL.Insert(0, '<html><body><h1>File List</h1>');
    Stl.Add('<table>');
    If FindFirst (FFName + '*',faAnyFile, Info)=0 then begin
        Stl.Add('<tr>');
        repeat
            if (Info.Name = '.') or (Info.Name = '..') then continue;
            if (Info.Attr and faDirectory) = faDirectory then
                Stl.add('<td>Directory</td><td><a href="' + Info.Name + '/">' + Info.Name + '</td>')
            else begin
                Stl.add('<td>' + inttostr(Info.size) + '</td><td><a href="'+ Info.Name + '">' + Info.Name + '</a></td>');
            end;
            Stl.add('</tr>');
        until FindNext(info) <> 0;
    end;
    Stl.add('</table>');
    FindClose(Info);
    Stl.Add('</body></html>');
    AResp.Content := StL.Text;
    STL.Free;
    AResp.SendContent;
end; *)


{ we must first check file looks like a note, then determine a suitable Rev based
dir to save it too. Update the mainfest with a) the new rev number and LCD for that note
and b) the new rev number. Then, c) inc the server rev number. Second and third lines
<sync revision="1" server-id="53576166-6C50-4FB6-9AAB-CDC289D084CC">
  <note id="88299c65-8858-44d0-9fa2-e78ca77749ba" rev="0" last-change-date="2020-02-19T02:41:48.0510000+00:00" />


So, change the rev number and LCD of the note in MetaData
}

procedure TMistyHTTPServer.DoCommandAcceptEdit(AReq  : TFPHTTPConnectionRequest ;
                                             AResp : TFPHTTPConnectionResponse);
var
  St : string;
  Convert : THTML2Note;
  PNote : PNoteInfo;
begin
    // Resp.Code - 298 : cannot find orig note;  296 : convert html2note error;  295 : file i/o error;  200 : OK
    // 200 is OK, anything betwen 201-299 is, between you and me, an error reported to user on the Edit page.
    AResp.Code := 200;       // this appears to be response.status that the JS sendData() gets, must be 200 - 299
    if DebugMode then writeLn('TMistyHTTPServer.DoCommandAcceptEdit()');
    AReq.HandleGetOnPost := True;
    PNote := MetaData.FindID(ExtractFileName(AReq.ContentFields.Values['noteFName']));
    if PNote = nil then begin                                                   // note, we use it further down
        writeln('TMistyHTTPServer.DoCommandAcceptEdit - ERROR note not present : ' + AReq.ContentFields.Values['noteFName']);
        exit;
    end;
    Convert := THTML2Note.Create;
    Convert.NoteFFName := GetRevisionDirPath(ServerHome, PNote^.Rev, PNote^.ID);// That is the existing, unedited note
    Convert.InStr := AReq.ContentFields.Values['noteData'];                     // thats an edited note in html format
    if not Convert.Convert() then begin                                         // Convert back to TB note format
        AResp.Code := 296;
        St := '<p>Failed to convert note</p>';
        writeln('TMistyHTTPServer.DoCommandAcceptEdit - ERROR - Failed to convert html to note' + Convert.NoteFFName);
        SaveString(AReq.ContentFields.Values['noteData'], ServerHome + 'failed2convert.html');
    end else begin                                                              // Converted back to .note OK

        if not Convert.SaveNote(ServerHome, Convert.NoteFFName, Revision) then begin             // $ID.note
            writeln('TMistyHTTPServer.DoCommandAcceptEdit - ERROR dealing with ' + Convert.NoteFFName);
            AResp.Code := 295;
            St := '<p>Failed to save note</p>';
        end else begin
            if DebugMode then writeln('TMistyHTTPServer.DoCommandAcceptEdit - writing ' + Convert.NoteFFName);
            St := '<p>All Good</p>';                                            // OK, now Rev Revision and save manifest
            PNote^.LastChange := Convert.DateSt;
            inc(Revision);                                                      // note : we are not using MetaData.LastRev (except at startup)
            PNote^.Rev := Revision;
            if not WriteNewManifest(GetRevisionDirPath(ServerHome, Revision) + 'manifest.xml') then // writes both manifests
                writeln('TMistyHTTPServer.DoCommandAcceptEdit - ERROR - Failed to write manifest')
            else
               if DebugMode then
                    writeln('TMistyHTTPServer.DoCommandAcceptEdit - wrote new mainfest');
        end;                                                                    // Unlike Note Upload, we do not re-read the mainfest.
    end;
    Convert.Free;
    CheckMimeLoaded();
    AResp.ContentType:=MimeTypes.GetMimeType('html');
    AResp.ContentLength := length(St);
    AResp.content := St;
    // SaveString(St, Serv.BaseDir + 'temp' + PathDelim + 'LastResp.html');
    AResp.SendContent;
end;



procedure TMistyHTTPServer.DoCommandUpload(AReq  : TFPHTTPConnectionRequest ;
                                           AResp : TFPHTTPConnectionResponse);   // this is also a POST
var
    f: TUploadedFile;
    Sts : TstringList;
    SaveFileName : string = '/tmp/';
begin
    if DebugMode then writeln('DoCommandUpload ' + AReq.URL);
    (*
    if AReq.Files.count = 0 then        // then we are not trying to upload, just get an upload prompt. Not needed in Misty
        with AResp.Contents do begin
            Add('<form id="form" action="' + AReq.URI + '" method="POST" enctype="multipart/form-data">');
            Add('<label for="name">Drag n drop or click to add file:</label>');
            Add('<input type="file" name="input" />');
            Add('<input type="submit" value="Send" />');
        Add('</form>');
    end else begin             *)         // the secret here is that AReq.Files.count > 0 ! So, must be a file coming.
        f := AReq.Files[0];               // here we assume only one file has been sent. Hope thats safe.
        Sts := TStringList.Create;
        try                                                                     // Note  manifest arrives BEFORE uploaded notes.
            Sts.LoadFromStream(F.Stream);
            if F.FileName.Endswith('-remote') then begin                        // manifest
                SaveFileName := GetRevisionDirPath(ServerHome, Revision + 1);   // +1 because we have spotted a new change sync starting !
                if ForceDirectory(SaveFileName) then begin
                    SaveFileName := SaveFileName + 'manifest.xml';
                    Sts.SaveToFile(SaveFileName);                               // thats the copy of manifest
                    Sts.SaveToFile(ServerHome + 'manifest.xml');                // thats the main one, assume here we can always save it ??
                    if ReadManifest(MetaData, ServerHome + 'manifest.xml', ErrorSt) then
                        Revision := MetaData.LastRev
                    else begin
                        writeln('TMistyHTTPServer.DoCommandUpload - ERROR cannot read manifest : ' {+ ErrorSt});
                        ErrorSt := '';
                        exit;                                                   // ToDo : Unlikely but extreamly bad, is this a Quit situation ?
                    end;
                    // inc(Revision);                                           // no - update it by reading the manifest !
                end;
            end else begin                                                      // Its a note, save to current Rev dir
                SaveFileName := GetRevisionDirPath(ServerHome, Revision) + F.FileName;
                Sts.SaveToFile(SaveFileName);                                   // Save note or copy of manifest in Rev dir
            end;
            if FileExists(SaveFileName) then begin
                if DebugMode then writeln('DoCommandUpload just saved ' + SaveFileName);
                AResp.Contents.Add('<html><body><h2>OK</h2></p>' + f.FileName +  ' was uploaded.' + '</p></body></html>');
            end else begin
                writeln('DoCommandUpload ERROR - failed to create or save ' + SaveFileName);
                AResp.Contents.Add('<html><body><h2>Nope</h2></p>' + 'file was not uploaded.' + '</p></body></html>');
                writeln('DoCommandUpload ERROR [' + SaveFileName + '] NOT saved.');
                AResp.Code := 422;
            end;
            AResp.SendResponse;
        finally
            Sts.Free;
        end;
end;



{ Passed note name and returns with the full local pathname to that note. Returns
fsOK if note found or other TFileStatus errors, takes ID or FName (inc .note) }
function TMistyHTTPServer.GetFullFileName(FName: string; var FFName: string): TFileStatus;
var
    PNote : PNoteInfo = nil;
begin
    if FileExists(ServerHome + 'manifest.xml') then begin        // can only help here if we have a manifest.
       PNote := MetaData.FindID(copy(FName, 1, 36));
       if PNote <> Nil then                                      // if nil, FileExists test below will get and report it
           FFName := GetRevisionDirPath(ServerHome, PNote^.Rev, FName)
       else
           exit(fsNoEntry);
       if FileExists(FFName) then
            exit(fsOK)
       else
            exit(fsNoNote);
    end else
        Result := fsNoManifest;
end;


procedure TMistyHTTPServer.DoCommandDownload(AReq: TFPHTTPConnectionRequest;
                                             AResp: TFPHTTPConnectionResponse);
var STL : TStringList;
//    St  : string;
    FName : string;
    PNote : PNoteInfo = nil;
begin                                                                           // ToDo : this should use GetFullFileName()

    // MetaData.DumpList('in DoCommandDownload');
    if DebugMode then writeln('TMistyHTTPServer.DoCommandDownload ' + AReq.URL);
    AResp.ContentType := 'application/text';                     // in case we have to return an error message
    STL := TStringList.Create;
    try
        if FileExists(ServerHome + 'manifest.xml') then begin        // can only help here if we have a manifest.
            if pos('/MANIFEST', AReq.URL) > 0 then                   // client wants the manifest.
                FName := ServerHome + 'manifest.xml'
            else begin
                // A note filename should always be exactly 41 char long, 36 in the ID plus '.note'

                FName := copy(AReq.URL, AReq.URL.LastIndexOf('/')+2, 100);      // should be just $ID.note It arrives here as /DOWNLOAD/$ID.note
                // code relating to file version number as obtained from manifest
                PNote := MetaData.FindID(copy(FName, 1, 36));
                if PNote <> Nil then                                // if nil, FileExists test below will get and report it
                    FName := GetRevisionDirPath(ServerHome, PNote^.Rev, FName)
                else writeln('TMistyHTTPServer.DoCommandDownload, ERROR failed to find ' + FName + ' in MetaData');
            end;
            // writeln('TMistyHTTPServer.DoCommandDownload FName is ' + FName);
            if FileExists(FName) then begin
                STL.LoadFromFile(FName);
                AResp.ContentType := 'application/xml';
                AResp.code := 200;
            end else begin
                AResp.ContentType := 'application/text';                // hmm, client will be expecting XML
                AResp.code := 404;
                Stl.Add('Cannot find that file ' + FName);
                writeln('TMistyHTTPServer.DoCommandDownload ' + Stl.Text);
                writeln('That file has revision number ' + inttostr(PNote^.Rev) + ' using ' + ServerHome + 'manifest.xml');
            end;
        end else begin
            AResp.code := 404;
            Stl.Add('Cannot serve files without manifest');
            writeln('TMistyHTTPServer.DoCommandDownload - ' + Stl.Text);
        end;
        AResp.ContentLength := length(STL.Text);
        AResp.Content := STL.Text;
    finally
        Stl.free;
    end;
    AResp.SendContent;
end;


procedure TMistyHTTPServer.DoCommandListNotes(AReq: TFPHTTPConnectionRequest;
                                              AResp: TFPHTTPConnectionResponse);
var
    PNote : PNoteInfo = nil;
    STL : tstringlist;
begin
    if DebugMode then writeln('TMistyHTTPServer.DoCommandListNotes');
    STL := TStringList.Create();
    STL.Insert(0, '<!DOCTYPE html><body><h1>File List</h1>');
    Stl.Add('<table>');
    for PNote in MetaData do begin
            Stl.Add('<tr>');
            Stl.add('<td><a href="'+ PNote^.ID + '.note' + '">'
                    + GetTitleFromFFN(ServerHome, PNote^.Rev, PNote^.ID) + '</a></td>');
            Stl.add('</tr>');
    end;
    Stl.add('</table>');
    Stl.Add('</body></html>');
    AResp.Content := StL.Text;
    STL.Free;                                                                   // ToDo : html ?
    AResp.SendContent;
end;


procedure TMistyHTTPServer.DoCommandEdit(AReq: TFPHTTPConnectionRequest;
    AResp: TFPHTTPConnectionResponse);
var
    FName : string;
    HTML : TExportHTML = Nil;
    STL : TStringList = Nil;
    St : string;
    PNote : PNoteInfo = nil;
begin
    FName := copy(AReq.URL, 2, 100);                                            // ToDo : better checking that file is available and is a note
    if FName.EndsWith('.note') then FName := FName.Remove(36, 5);               // check I have those numbers correct. Just ID now
    PNote := MetaData.FindID(copy(FName, 1, 36));
    if PNote <> Nil then                                // if nil, FileExists test below will get and report it
        FName := GetRevisionDirPath(ServerHome, PNote^.Rev, FName)
    else writeln('TMistyHTTPServer.DoCommandEdit, ERROR failed to find ' + FName + ' in MetaData');     // ToDo : better error handling
    if DebugMode then writeln('MistyHTTPServer.DoCommandEdit wants to edit ' + FName);
    HTML := TExportHTML.Create();
    HTML.NotesDir:= ServerHome;
//    HTML.OutDir := AppendPathDelim(DestDir);
//    HTML.FollowLinks := False;
    STL := TStringList.Create();
    try
        HTML.ExportContent(FName, STL);                                         // ToDo : some error checking please.
        if Stl.Count < 2 then begin
            writeln('TMistyHTTPServer.DoCommandEdit ERROR failed to convert note to HTML : ' + FName);
            AResp.Content := '<html><body><h2>ERROR</h2></p>' + 'empty string list returned' + '</p></body></html>';
            exit;
        end;

//        SaveString(Editor_1 + Editor_2 + STL.Text + Editor_3, ServerHome + copy(FName, 1, 36) + '.html');

        St := Editor_1 + '  formData.append("noteFName", "' + FName + '");' + #10
                                  + '  formData.append("noteTitle", "unknown");'
                       + Editor_2.Replace('!!INSERT NOTE TITLE!!', HTML.Title) + STL.Text
                       + Editor_3
                       + ' <table style="width:100%"><tr>'
                       + '<th style="width:10%"><button type="button" onclick="sendData()">Save</button></th>'
                       + '<th style="width:10%"><p id="IDstatus">_____</p></th>'
                       + '<th style="width:80%"><p><a href="LISTNOTES">Main Note List</a></p></th>'
                       + '</tr></table> '
                       + '</body></html>';

        CheckMimeLoaded;
        AResp.ContentType:=MimeTypes.GetMimeType('html');
        AResp.ContentLength := length(St);
        AResp.Content := St;
        // SaveString(St, Serv.BaseDir + 'temp' + PathDelim + 'LastEdit.html');
        if DebugMode then writeln('TMistyHTTPServer.DoCommandEdit - response prepared');
        AResp.SendContent;
    finally
        if HTML <> Nil then HTML.Free;
        if STL <> Nil then STL.Free;
    end;
end;

{  Content in URL after the server:8080/ bit
    (nothing) a short hml error message and 'error' code 200
    (nothing but ARequest.ContentFields.Count > 0) - We are sending back an edit from Quill
    (nothing but ARequest.Files.Count > 0) - its a file upload.
    /LISTNOTES             - dislays a html formatted list of notes, click to edit
    /DOWNLOAD/$NOTEID.note - client wants to download ndicated note
    /MANIFEST              - client wants to download the manifest,
    /$NOTEID.note          - User has clicked a LISTNOTES entry, send it back wrapped up in Quill
    /



}
                // override the fphttpserver version, called for every request.
                // we will ty and help if we can. If a dir, list dir contents,
                // if its a filename, display it, if a command, we'll obey !
procedure TMistyHTTPServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
                                        var AResponse: TFPHTTPConnectionResponse);
Var
  F : TFileStream;
  FN : String;                      // ToDo : must refactor this method, its a dogs breakfast !

begin
    AResponse.Code := 200;          // Default, we return a HTML text error message and 200 if we cannot work out what to do.

    // Here we decide just what this particular call is. We pass ctrl to a procedure
    // that handles each type of call. In  all cases, we assume that procedure all that is necessary.
    ARequest.HandleGetOnPost := True;                                           // ?
    if DebugMode then writeln('TMistyHTTPServer.HandleRequest URL=' + ARequest.URL + ' FileCount='
            + ARequest.Files.Count.ToString + ' FieldCount=' + ARequest.ContentFields.Count.ToString);

    if ARequest.Files.Count > 0 then                                            // Fileupload ?
        DoCommandUpLoad(ARequest, AResponse)
    else                                                  // Content Update, data back from Quill webpage, an edited note.
    if ARequest.ContentFields.Count > 1 then              // > 0 would work too, but better checks necessary
        DoCommandAcceptEdit(ARequest, AResponse)
    else
    if ARequest.URL = '/LISTNOTES' then                   // List notes, user wants to browse list of notes
        DoCommandListNotes(ARequest, AResponse)
    else
    if pos('/DOWNLOAD', ARequest.URL) > 0 then
    // if copy(ARequest.URL, 1, 9) = '/DOWNLOAD' then        // TB client wants to download a note from repo
        DoCommandDownLoad(ARequest, AResponse)
    else
    if pos('/MANIFEST', ARequest.URL) > 0 then
    // if copy(ARequest.URL, 1, 9) = '/MANIFEST' then        // TB client wants the manifest, same as above
        DoCommandDownLoad(ARequest, AResponse)
    else

    if copy(ARequest.URL, length(ARequest.URL)-4, 5) = '.note' then  // Send a note to quill editor
        DoCommandEdit(ARequest, AResponse)                           // just note name (and .note"), no path required
    else
        if ARequest.URL = '/quill.html' then begin                   // ToDo : remove this block ?
            writeln('TMistyHTTPServer.HandleRequest In TEST mode');
            FN := Serv.BaseDir + 'quill.html';
            F:=TFileStream.Create(FN, fmOpenRead);
            try
                   CheckMimeLoaded;
                   AResponse.ContentType:=MimeTypes.GetMimeType(ExtractFileExt(FN));
                   Writeln('Serving file: "',Fn,'". Reported Mime type: ',AResponse.ContentType);
                   AResponse.ContentLength:=F.Size;
                   AResponse.ContentStream:=F;
                   AResponse.SendContent;
                   AResponse.ContentStream:=Nil;
           finally
                   F.Free;
           end;
            writeln('TMistyHTTPServer.HandleRequest - we are done here');       // ToDo : remove
        end
    else begin                                                                  // this is a special case, no command, instruction, data in URL, its working but non-functional
        writeln(' ARequest.URL = ', ARequest.URL);
        writeln(copy(ARequest.URL, length(ARequest.URL)-4, 5));
        AResponse.Content := '<html><body><h2>ERROR</h2></p>' + 'I have no idea what you mean.' + '</p></body></html>';
    end;
end;



procedure TMistyHTTPServer.Startup;
begin
    MetaData := nil;        // to be sure, to be sure
    LoadFromFile('editor_1.template', Editor_1);
    LoadFromFile('editor_2.template', Editor_2);
    LoadFromFile('editor_3.template', Editor_3);
    try
        {$ifdef unix}
        Serv.MimeTypesFile:='/etc/mime.types';
        {$endif}
        Serv.Threaded:=False;
        Serv.AcceptIdleTimeout:=1000;
        Serv.OnAcceptIdle:=@Serv.DoIdle;
        ServerHome := Serv.BaseDir + 'home' + PathDelim;
        if ReadManifest(MetaData, ServerHome + 'manifest.xml', ErrorSt) then
            Serv.Revision := MetaData.LastRev
        else
            Serv.Revision := -1;                    // Thats OK, its a new install.
        writeln(ParamStr(0), ' starting, serving from ', Serv.BaseDir, ' on port ', Serv.Port.tostring);
        writeln('Revision=', Serv.Revision.tostring);
        if MetaData <> nil then writeln('ServerID=', MetaData.ServerID        // only shown after first manifest.
                + ' Found notes = ', MetaData.Count.ToString);
        Serv.Active:=True;                                                      // does not return until Serv is terminated
        // from time to time, an exception relating to POST is raised and apparently dealt with
        // internally in httpserver. But if running under the debugger in the IDE you get told about it.
    finally
        if MetaData <> nil then MetaData.free;
        Serv.Free;
    end;
end;

end.
