program webserver;

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

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  ssync_utils,    // data sync utils based on tomboy-ng code
  sysutils, Classes, fphttpserver, fpmimetypes, wmecho, exporthtml, httpdefs,
  BaseUnix,       // for Signal Names  Hmm, windows ?  No idea !
  html2note,
  LazLogger, LazFileUtils;      // does that introduce a LCL dependency ?



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

    procedure GenerateFileList(FFName: string; AResp: TFPHTTPConnectionResponse);
    function MyAppendPathDelim(APath: string): string;
    procedure SetBaseDir(const AValue: String);
  Protected
    Procedure DoIdle(Sender : TObject);
    procedure CheckMimeLoaded;

    Property MimeLoaded : Boolean Read FMimeLoaded;
  public
    procedure HandleRequest(Var ARequest: TFPHTTPConnectionRequest;
                            Var AResponse : TFPHTTPConnectionResponse); override;
    Property BaseDir : String Read FBaseDir Write SetBaseDir;
    Property MimeTypesFile : String Read FMimeTypesFile Write FMimeTypesFile;

  end;


Var
  Serv : TMistyHTTPServer;
  ExitNow : boolean;           // A semaphore set when ctrl-C received
  ErrorSt : string = '';       // Check this before exiting with an error
  ServerHome : string;
  DebugMode : boolean = false;

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

function TMistyHTTPServer.MyAppendPathDelim(APath : string) : string;    // here to avoid use lazfileutils
begin
    if APath[length(APath)] <> PathDelim then
        Result := APath + PathDelim
    else Result := APath;
end;

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
end;


procedure TMistyHTTPServer.DoCommandAcceptEdit(AReq  : TFPHTTPConnectionRequest ;
                                             AResp : TFPHTTPConnectionResponse);
var
  St : string;
  Convert : THTML2Note;
begin
    // Resp.Code - 298 : cannot find orig note;  296 : convert html2note error;  295 : file i/o error;  200 : OK
    // 200 is OK, anything betwen 201-299 is, between you and me, an error reported to user on the Edit page.
    AResp.Code := 200;       // this appears to be response.status that the JS sendData() gets, must be 200 - 299
    AReq.HandleGetOnPost := True;
    Convert := THTML2Note.Create;
    Convert.NoteFName := AReq.ContentFields.Values['noteFName'];                  // that should be an existing note in ~/home
    Convert.InStr := AReq.ContentFields.Values['noteData'];                       // thats an edited note in html format
    if not Convert.Convert() then begin                                           // Convert back to TB note format
        AResp.Code := 296;
        St := '<p>Failed to convert note</p>';
    end else
        if not Convert.SaveNote(Serv.BaseDir + 'temp' + PathDelim + Convert.NoteFName) then begin
            debugln('TMistyHTTPServer.DoCommandPost - ERROR dealing with ' + Convert.NoteFName);
            AResp.Code := 295;
            St := '<p>Failed to save note</p>';
        end else begin
            if DebugMode then debugln('TMistyHTTPServer.DoCommandPost - write ' + Serv.BaseDir + 'temp' + PathDelim + Convert.NoteFName);
            St := '<p>All Good</p>';
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
    if DebugMode then debugln('DoCommandUpload ' + AReq.URL);
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
        SaveFileName := GetRevisionDirPath(ServerHome, Revision + 1);
        Sts := TStringList.Create;
        try
            Sts.LoadFromStream(F.Stream);
            if DirectoryExists(SaveFileName) or ForceDirectoriesUTF8(SaveFileName) then begin
                if F.FileName.Endswith('-remote') then
                    SaveFileName := SaveFileName + 'manifest.xml'
                else SaveFileName := SaveFileName + F.FileName;
                Sts.SaveToFile(SaveFileName);                  // Save notes and a copy of manifest in Rev dir
                if F.FileName.Endswith('-remote') then         // Save main manifest in home
                     Sts.SaveToFile(ServerHome + 'manifest.xml');
            end;
            if FileExists(SaveFileName) then begin
                AResp.Contents.Add('<html><body><h2>OK</h2></p>' + f.FileName +  ' was uploaded.' + '</p></body></html>');
            end else begin
                AResp.Contents.Add('<html><body><h2>Nope</h2></p>' + 'file was not uploaded.' + '</p></body></html>');
                debugln('DoCommandUpload ERROR [' + SaveFileName + '] NOT saved.');
                AResp.Code := 422;
            end;
            AResp.SendResponse;
        finally
            Sts.Free;
        end;
end;


procedure TMistyHTTPServer.DoCommandDownload(AReq: TFPHTTPConnectionRequest;
                                             AResp: TFPHTTPConnectionResponse);
var STL : TStringList;
//    St  : string;
    FName : string;
    PNote : PNoteInfo = nil;
begin

    // MetaData.DumpList('in DoCommandDownload');
    if DebugMode then debugln('TMistyHTTPServer.DoCommandDownload ' + AReq.URL);
    AResp.ContentType := 'application/text';                     // in case we have to return an error message
    STL := TStringList.Create;
    try
        if FileExists(ServerHome + 'manifest.xml') then begin        // can only help here if we have a manifest.
            if pos('MANIFEST', AReq.URL) > 0 then
                FName := ServerHome + 'manifest.xml'
            else begin
                FName := copy(AReq.URL, 11, 100);                   // should be just $ID.note It arrives here as /DOWNLOAD/$ID.note
                // Insert here code relating to file version number as obtained from manifest
                PNote := MetaData.FindID(copy(FName, 1, 36));
                if PNote <> Nil then                                // if nil, FileExists test below will get and report it
                    FName := GetRevisionDirPath(ServerHome, PNote^.Rev, FName)
                else debugln('TMistyHTTPServer.DoCommandDownload, ERROR failed to find ' + FName + ' in MetaData');
            end;
            // debugln('TMistyHTTPServer.DoCommandDownload FName is ' + FName);
            if FileExists(FName) then begin
                STL.LoadFromFile(FName);
                AResp.ContentType := 'application/xml';
                AResp.code := 200;
            end else begin
                AResp.ContentType := 'application/text';                // hmm, client will be expecting XML
                AResp.code := 404;
                Stl.Add('Cannot find that file ' + FName);
                debugln('TMistyHTTPServer.DoCommandDownload ' + Stl.Text);
            end;
        end else begin
            AResp.code := 404;
            Stl.Add('Cannot serve files without manifest');
            debugln('TMistyHTTPServer.DoCommandDownload - ' + Stl.Text);
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
    STL : tstringlist;
    Info : TSearchRec;
begin
    if DebugMode then debugln('TMistyHTTPServer.DoCommandListNotes');
    STL := TStringList.Create();
    STL.Insert(0, '<!DOCTYPE html><body><h1>File List</h1>');
    Stl.Add('<table>');
    If FindFirst (ServerHome + '*.note',faAnyFile, Info)=0 then begin
        repeat
            Stl.Add('<tr>');
            if (Info.Name = '.') or (Info.Name = '..') then continue;
            if (Info.Attr and faDirectory) = faDirectory then continue;
            Stl.add('<td>' + inttostr(Info.size) + '</td><td><a href="'+ Info.Name + '">' + Info.Name + '</a></td>');
            Stl.add('</tr>');
        until FindNext(info) <> 0;
    end;
    Stl.add('</table>');
    FindClose(Info);
    Stl.Add('</body></html>');
    AResp.Content := StL.Text;
    STL.Free;
    AResp.SendContent;
end;


procedure TMistyHTTPServer.DoCommandEdit(AReq: TFPHTTPConnectionRequest;
    AResp: TFPHTTPConnectionResponse);
var
    FName : string;
    HTML : TExportHTML = Nil;
    STL : TStringList = Nil;
    St : string;
begin
    FName := copy(AReq.URL, 2, 100);                                            // ToDo : better checking that file is available and is a note
    if DebugMode then debugln('MistyHTTPServer.DoCommandEdit wants to edit ' + FName);
    HTML := TExportHTML.Create();
    HTML.NotesDir:= ServerHome;
//    HTML.OutDir := AppendPathDelim(DestDir);
//    HTML.FollowLinks := False;
    STL := TStringList.Create();
    try
        HTML.ExportContent(ServerHome + FName, STL);                                         // ToDo : some error checking please.
        if Stl.Count < 2 then begin
            debugln('TMistyHTTPServer.DoCommandEdit ERROR failed to convert note to HTML : ' + FName);
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
        if DebugMode then debugln('TMistyHTTPServer.DoCommandEdit - response prepared');
        AResp.SendContent;
    finally
        if HTML <> Nil then HTML.Free;
        if STL <> Nil then STL.Free;
    end;
end;

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
    writeln('TMistyHTTPServer.HandleRequest URL=', ARequest.URL, ' FileCount=', ARequest.Files.Count, ' FieldCount=', ARequest.ContentFields.Count);

    if ARequest.Files.Count > 0 then                                            // Fileupload ?
        DoCommandUpLoad(ARequest, AResponse)
    else                                                  // Content Update, data back from Quill webpage, an edited note.
    if ARequest.ContentFields.Count > 1 then              // > 0 would work too, but better checks necessary
        DoCommandAcceptEdit(ARequest, AResponse)
    else
    if ARequest.URL = '/LISTNOTES' then                   // List notes, user wants to browse list of notes
        DoCommandListNotes(ARequest, AResponse)
    else
    if copy(ARequest.URL, 1, 9) = '/DOWNLOAD' then        // TB client wants to download a note from repo
        DoCommandDownLoad(ARequest, AResponse)
    else
    if copy(ARequest.URL, 1, 9) = '/MANIFEST' then        // TB client wants the manifest, same as above
        DoCommandDownLoad(ARequest, AResponse)
    else

    if copy(ARequest.URL, length(ARequest.URL)-4, 5) = '.note' then  // Send a note to quill editor
        DoCommandEdit(ARequest, AResponse)                           // just note name (and .note"), no path required
    else
        if ARequest.URL = '/quill.html' then begin
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
    else begin                                                                  // his is a special case, no command, instruction, data in URL, its working but non-functional
        writeln(' ARequest.URL = ', ARequest.URL);
        writeln(copy(ARequest.URL, length(ARequest.URL)-4, 5));
        AResponse.Content := '<html><body><h2>ERROR</h2></p>' + 'I have no idea what you mean.' + '</p></body></html>';

    end;
end;



procedure HandleSigInt(aSignal: LongInt); cdecl;
begin
    case aSignal of
        SigInt : Writeln('Ctrl + C used, will clean up and shutdown.');
        SigTerm : writeln('TERM signal, will clean up and shutdown.');
    else
        begin writeln('Some signal received ??'); exit; end;
    end;
    ExitNow := True;            // Watched by the Idle method
end;

                                // Refactor to accept proper command line parameters.
begin
    MetaData := nil;        // to be sure, to be sure
    if FpSignal(SigInt, @HandleSigInt) = signalhandler(SIG_ERR) then begin
        Writeln('Failed to install signal error: ', fpGetErrno);
        exit;
    end;
    if FpSignal(SigTerm, @HandleSigInt) = signalhandler(SIG_ERR) then begin
        Writeln('Failed to install signal error: ', fpGetErrno);
        exit;
    end;
    Serv:=TMistyHTTPServer.Create(Nil);
    try
        Serv.BaseDir:=ExtractFilePath(ParamStr(0));
        if not LoadFromFile(Serv.BaseDir + 'editor_1.template', Editor_1) then begin
            writeln('Failed to load editor_1.template : ' + ErrorSt);
            exit;
        end;
        if not LoadFromFile(Serv.BaseDir + 'editor_2.template', Editor_2) then begin
            writeln('Failed to load editor_2.template : ' + ErrorSt);
            exit;
        end;
        if not LoadFromFile(Serv.BaseDir + 'editor_3.template', Editor_3) then begin
            writeln('Failed to load editor_3.template : ' + ErrorSt);
            exit;
        end;
        {$ifdef unix}
        Serv.MimeTypesFile:='/etc/mime.types';
        {$endif}
        Serv.Port:=8080;
        if ParamCount > 0 then begin
            if (ParamStr(1) = '-h') or (ParamStr(1) = '--help') or (ParamStr(1) = 'help') then begin
                writeln('Usage ', ExtractFileName(ParamStr(0)), ' [BaseDir] [Port]');
                exit;
            end;
            Serv.BaseDir:=ParamStr(1);
        end;
        if ParamCount > 1 then
            Serv.Port:=StrToIntDef(ParamStr(2),8080);
        Serv.Threaded:=False;
        Serv.AcceptIdleTimeout:=1000;
        Serv.OnAcceptIdle:=@Serv.DoIdle;
        ServerHome := Serv.BaseDir + '/home/';
        if ReadManifest(MetaData, ServerHome + 'manifest.xml', ErrorSt) then
            Serv.Revision := MetaData.LastRev
        else
            Serv.Revision := -1;                    // Thats OK, its a new install.

        debugln(ParamStr(0), ' starting, serving from ', Serv.BaseDir, ' on port ', Serv.Port.tostring);
        debugln('Revision=', Serv.Revision.tostring);
        if MetaData <> nil then debugln('ServerID=', MetaData.ServerID);        // only shown after first manifest.
        Serv.Active:=True;
        // from time to time, a exception relating to POST is raised and apparently dealt with
        // internally in httpserver. But if running under the debugger in the IDE you get told about it.
    finally
        if MetaData <> nil then MetaData.free;
        Serv.Free;
    end;
end.

