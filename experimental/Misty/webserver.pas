program webserver;

{ License

}

{$mode objfpc}{$H+}
{$define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  sysutils, Classes, fphttpserver, fpmimetypes, wmecho, exporthtml, httpdefs,
  BaseUnix,       // for Signal Names  Hmm, windows ?  No idea !
  html2note;



Type

  { TTestHTTPServer }

  { TMistyHTTPServer }

  TMistyHTTPServer = Class(TFPHTTPServer)
  private
    FBaseDir : String;
//    FCount : Integer;
    FMimeLoaded : Boolean;
    FMimeTypesFile: String;
    procedure DoCommandPost(AReq : TFPHTTPConnectionRequest; AResp : TFPHTTPConnectionResponse);

    procedure DoCommandUpload(AReq : TFPHTTPConnectionRequest; AResp : TFPHTTPConnectionResponse);
    procedure DoCommandDownload(AReq : TFPHTTPConnectionRequest; AResp : TFPHTTPConnectionResponse);
    procedure DoCommandListNotes(AReq : TFPHTTPConnectionRequest; AResp : TFPHTTPConnectionResponse);
    procedure DoCommandEdit(AReq : TFPHTTPConnectionRequest; AResp : TFPHTTPConnectionResponse);
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

const
    ServerHome = '/home/dbannon/Pascal/tb-web/Misty/home/';
    Editor_1 : string = '<html>';
    Editor_2 : string = '';
    Editor_3 : string = '</html>';


    //Just a debug tool, probably delete at some stage
procedure SaveString(InString, OutFilePath: string);
var
    F: TextFile;
begin
//    writeln('SaveString InS=', InString);
//    writeln('SaveString Path=', OutFilePath);
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

        // A web user has sent back a edited Quill note. Convert back to note
        // format and save it. We that everything that arrives here does need
        // to be saved.
procedure TMistyHTTPServer.DoCommandPost(AReq  : TFPHTTPConnectionRequest ;
                                             AResp : TFPHTTPConnectionResponse);
var
  LName: String;
  V, St : string;
  Convert : THTML2Note;
begin
  writeln('DoCommandPost');
  AReq.HandleGetOnPost := True;
  //for V in AReq.ContentFields do
  //      writeln('DoCommandPost Value=', V);

  //AResp.Content := 'thanks';
  // SaveString(AReq.ContentFields.Values['noteData'], Serv.BaseDir + 'temp' + PathDelim + 'something.html');

  Convert := THTML2Note.Create;
  Convert.NoteFName := AReq.ContentFields.Values['noteFName'];                  // that should be an existing note in ~/home
  // Convert.LoadHTML('26D6EDFD-B518-472A-985B-AEF0B266D00F.note.html');        // its in working dir, just testing
  Convert.InStr := AReq.ContentFields.Values['noteData'];                       // thats an edited note in html format
  Convert.Convert();                                                            // Convert back to TB note format
  // writeln('DoCommandPost - about to save '  + Convert.NoteFName);
  Convert.SaveNote(Serv.BaseDir + 'temp' + PathDelim + Convert.NoteFName);
  Convert.Free;
//  SaveString(AReq.ContentFields.Values['noteData'], Serv.BaseDir + 'temp' + PathDelim
//        + AReq.ContentFields.Values['noteFName'] + '.html');
  // Above needs to convert html to xml and overwrite note, update manifest NOT just save it to a file.
  // DoCommandListNotes(AReq, Aresp); // ToDo : this does not work


  St := '<html><body><h2>OK</h2></p>' + 'this is something back' + '</p></body></html>' + #10;
  CheckMimeLoaded();
  AResp.ContentType:=MimeTypes.GetMimeType('html');
  AResp.ContentLength := length(St);
  AResp.Content := St;
  SaveString(St, Serv.BaseDir + 'temp' + PathDelim + 'LastResp.html');
  AResp.SendContent;
  // writeln('DoCommandPost finished');
  exit;




  LName := AReq.ContentFields.Values['name'];            // match name="Aname" below, not ID
  writeln('We are in DoCommandPost and LName=', LNAme);   // Not because method is POST, because we put it in URL
  if LName = '' then
    with AResp.Contents do begin
      Add('<form action="' + AReq.URI + '" method="POST"');
      Add('<label for="Aname">Please tell me your name:</label>');
      Add('<input type="text" name="name" id="myname" />');         //
      Add('<input type="submit" value="Send" />');
      Add('</form>');
    end
  else
    // we don't want this for our normal POST (when it works) but must set return message somehow
    AResp.Content := 'Hello, ' + LName + '!';   // OK, something in AReq.ContentFields.Values['Name'] ?
end;


        // Upload a file from the users PC, ie during a tomboy-ng sync process.
procedure TMistyHTTPServer.DoCommandUpload(AReq  : TFPHTTPConnectionRequest ;
                                         AResp : TFPHTTPConnectionResponse);   // this is also a POST
var
    f: TUploadedFile;
    Sts : TstringList;
    SaveFileName : string = '/tmp/';
begin
    if AReq.Files.count = 0 then
        with AResp.Contents do begin
            Add('<form id="form" action="' + AReq.URI + '" method="POST" enctype="multipart/form-data">');
            Add('<label for="name">Drag n drop or click to add file:</label>');
            Add('<input type="file" name="input" />');
            Add('<input type="submit" value="Send" />');
        Add('</form>');
    end else begin                      // the secret here is that AReq.Files.count > 0 ! So, must be a file coming.
        f := AReq.Files[0];
        SaveFileName := SaveFileName + F.FileName;
        Sts := TStringList.Create;
        Sts.LoadFromStream(F.Stream);
        Sts.SaveToFile(SaveFileName);
        if FileExists(SaveFileName) then
            AResp.Contents.Add('<html><body><h2>OK</h2></p>' + f.FileName +  ' was uploaded (I think).' + '</p></body></html>')
        else
            AResp.Contents.Add('<html><body><h2>Nope</h2></p>' + 'file was not uploaded.' + '</p></body></html>');
        Sts.Free;
    end;
end;


procedure TMistyHTTPServer.DoCommandDownload(AReq: TFPHTTPConnectionRequest;
                                             AResp: TFPHTTPConnectionResponse);
begin

end;

procedure TMistyHTTPServer.DoCommandListNotes(AReq: TFPHTTPConnectionRequest;
                                              AResp: TFPHTTPConnectionResponse);
var
    STL : tstringlist;
    Info : TSearchRec;
begin
    writeln('TMistyHTTPServer.DoCommandListNotes');
    STL := TStringList.Create();
    STL.Insert(0, '<html><body><h1>File List</h1>');
    Stl.Add('<table>');
    If FindFirst (ServerHome + '*.note',faAnyFile, Info)=0 then begin
        Stl.Add('<tr>');
        repeat
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

        // Called when a web user has clicked on a note, bundles up the note in HTML format
        // adds code to involve quill and applies the three Editor_? templates.
procedure TMistyHTTPServer.DoCommandEdit(AReq: TFPHTTPConnectionRequest;
    AResp: TFPHTTPConnectionResponse);
var
    FName : string;
    HTML : TExportHTML = Nil;
    STL : TStringList = Nil;
    St : string;
begin
    FName := copy(AReq.URL, 2, 100);                                            // ToDo : better checking that file is available and is a note
    writeln('MistyHTTPServer.DoCommandEdit looking ' + FName);
    HTML := TExportHTML.Create();
    HTML.NotesDir:= ServerHome;
//    HTML.OutDir := AppendPathDelim(DestDir);
//    HTML.FollowLinks := False;
    STL := TStringList.Create();
    try
        HTML.ExportContent(ServerHome + FName, STL);                                         // ToDo : some error checking please.
        if Stl.Count < 2 then begin
            writeln('TMistyHTTPServer.DoCommandEdit failed to convert note to HTML : ' + FName);
            AResp.Content := '<html><body><h2>ERROR</h2></p>' + 'empty string list returned' + '</p></body></html>';
            exit;
        end;

{        writeln('-----------------------------');
        writeln('EditorTop = ' + EditorTop);
        writeln('-----------------------------');
        writeln('EditBottom = ' + EditorBottom);
        writeln('-----------------------------');
        writeln('TMistyHTTPServer.DoCommandEdit Content=' +  STL.Text );
        writeln('-----------------------------');
        writeln('TMistyHTTPServer.DoCommandEdit File=' + ServerHome + copy(FName, 1, 36) + 'html');   }
//        SaveString(Editor_1 + Editor_2 + STL.Text + Editor_3, ServerHome + copy(FName, 1, 36) + '.html');

        St := Editor_1 + '  formData.append("noteFName", "' + FName + '");' + #10
                                  + '  formData.append("noteTitle", "unknown");'
                       + Editor_2.Replace('!!INSERT NOTE TITLE!!', HTML.Title) + STL.Text + Editor_3;

CheckMimeLoaded;
AResp.ContentType:=MimeTypes.GetMimeType('html');
AResp.ContentLength := length(St);
AResp.Content := St;

{        AResp.Content := Editor_1 + '  formData.append("noteFName", "' + FName + '");' + #10
                                  + '  formData.append("noteTitle", "unknown");'
                       + Editor_2 + STL.Text + Editor_3;     }

        SaveString(St, Serv.BaseDir + 'temp' + PathDelim + 'LastEdit.html');
        writeln('TMistyHTTPServer.DoCommandEdit - response prepared');
        AResp.SendContent;

//         AResp.Content := Editor_1 + Editor_2 + STL.Text + Editor_3;
//        writeln('MistyHTTPServer.DoCommandEdit +++++++++++++++++++++++++++');
//        writeln( AResp.Content);
//        writeln('MistyHTTPServer.DoCommandEdit ---------------------------');
        finally
        if HTML <> Nil then HTML.Free;
        if STL <> Nil then STL.Free;
    end;



    // AResp.Content := '<html><body><h2>OK</h2></p>' + 'Looking at ' + FName + '</p></body></html>';
    // OK, now we have to convert that note to HTML, clean off the header and footer
    // insert into into quill html template.
end;

                // override the fphttpserver version, called for every request.
                // we will ty and help if we can. If a dir, list dir contents,
                // if its a filename, display it, if a command, we'll obey !
procedure TMistyHTTPServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
                                        var AResponse: TFPHTTPConnectionResponse);
Var
  F : TFileStream;
  FN : String;
  Cmd : string;

    // Looks at URL, if it can make it work by adding an index.html or a '/' (ie dir) does so,
    // returns True. If it returns false, nothing we can do, missing file or dir.
 {   function CheckURL() : boolean;
    begin
            Result := True;
            if (length(FN)>0) and (FN[1]='/') then
                Delete(FN,1,1);
            DoDirSeparators(FN);       // Make a path contain appropiate seperators.
            if FileExists(BaseDir + FN) then begin
                FN:=BaseDir+FN;
                exit(true);            // user has entered a usable filename
            end;
            if FileExists(BaseDir + FN + 'index.html') then begin
                FN:=BaseDir+FN + 'index.html';
                exit(true);            // a dir that does contain an index.html
            end;
            if DirectoryExists(BaseDir+FN) then begin
                FN := MyAppendPathDelim(BaseDir+FN);
                if FileExists(FN + 'index.html') then
                    FN := FN + 'index.html';
                exit(True);
            end;
            // If to here, user must have put an invalid file or dir in URL
            result := False;
    end;    }

begin
    // Here we decide just what this particular call is. We pass ctrl to a procedure
    // that handles each type of call. I  all cases, we assume that procedure all that is necessary.
    ARequest.HandleGetOnPost := True;
    writeln('TMistyHTTPServer.HandleRequest URL=', ARequest.URL);

    if ARequest.Files.Count > 0 then                                            // Fileupload ?
        DoCommandUpLoad(ARequest, AResponse)
    else                                     // Content Update, data back from Quill webpage, an edited note.
    if ARequest.ContentFields.Count > 1 then // > 0 would work too !
        DoCommandPost(ARequest, AResponse)
    else
    if ARequest.URL = '/LISTNOTES' then       // List notes, user wants to browse list of notes
        DoCommandListNotes(ARequest, AResponse)
    else
    if copy(ARequest.URL, 1, 8) = '/DOWNLOAD' then // TB client wants to download a note from repo        "8" ???
        DoCommandDownLoad(ARequest, AResponse)
    else
    if copy(ARequest.URL, length(ARequest.URL)-4, 5) = '.note' then  // Send a note in quill editor
        DoCommandEdit(ARequest, AResponse)
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


            (* if LoadFromFile(Serv.BaseDir + 'quill.html', Cmd) then begin
                CheckMimeLoaded;
                AResponse.ContentType:=MimeTypes.GetMimeType('html');
                Writeln('Serving file: Reported Mime type: ',AResponse.ContentType);
                AResponse.Content := Cmd;
                AResponse.SendContent;
                SaveString(AResponse.Content, Serv.BaseDir + 'LastTest.html');
            end else writeln('TMistyHTTPServer.HandleRequest cannot load quill.html');   *)
            writeln('TMistyHTTPServer.HandleRequest - we are done here');
        end
    else begin
        writeln(' ARequest.URL = ', ARequest.URL);
        writeln(copy(ARequest.URL, length(ARequest.URL)-4, 5));
        AResponse.Content := '<html><body><h2>ERROR</h2></p>' + 'I have no idea what you mean.' + '</p></body></html>';
        AResponse.Code := 404;
    end;

{    ARequest.HandleGetOnPost := False;          // ????
    if pos('POST', ARequest.HeaderLine) > 0 then begin
        writeln('POST received, count=', ARequest.ContentFields.count);
    end;
    writeln('Command=' + ARequest.CommandLine + ' and HeaderLine=' + ARequest.HeaderLine + ' and CF.Count=', ARequest.ContentFields.Count);

    // in our case here, its possibly a POST, if so, Count > 0. Check Name
    if ARequest.Files.Count > 0 then writeln('Someone sent us a file.');        // ? not used, its a data post
    if ARequest.ContentFields.count > 0 then begin                              // A POST with data ?
        DoCommandPost(ARequest, AResponse);
        exit;
    end;


    FN:=ARequest.Url;
    if DoCommands(ARequest, AResponse) then exit;
//    writeln('TTestHTTPServer.HandleRequest : User requested ', FN);
    if not CheckURL() then begin
        AResponse.Code:=404;                       // ToDo : this does not work
        writeln('TTestHTTPServer.HandleRequest : File or Dir not found [', FN, ']');
        AResponse.Content := '<html><body><h2>ERROR 404, File or Dir not found</h2></p>' + FN + '</p></body></html>';
        AResponse.SendContent;
        exit;            ;
    end;
    // Here, we believe we can help the user, either with a file or a dir list.
    if (length(FN) = 0) or (FN[length(FN)] = PathDelim) then begin            // trailing Sep says is just a dir.
//        Writeln('No file to serve, will do file list, FN=[', FN, ']');
        GenerateFileList(FN, AResponse);
        exit;
    end;
    // If to here, we seem to have a file to serve, do so !
    // writeln(' TTestHTTPServer.HandleRequest and file exists.');
    F:=TFileStream.Create(FN,fmOpenRead);
    try
        CheckMimeLoaded;
        AResponse.ContentType:=MimeTypes.GetMimeType(ExtractFileExt(FN));
//        Writeln('Serving file: "',Fn,'". Reported Mime type: ',AResponse.ContentType);
        AResponse.ContentLength:=F.Size;
        AResponse.ContentStream:=F;
        AResponse.SendContent;
        AResponse.ContentStream:=Nil;
    finally
        F.Free;
    end;    }
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


procedure TestConvert2HTML(FFName : string);       // This is just a test, remove this block some time.
var HTML : TExportHTML = Nil;
    STL : TStringList = Nil;
begin
    writeln('Off line test mode');
    if FileExists(FFName) then writeln('Have found ' + FFName);
    HTML := TExportHTML.Create();
    HTML.NotesDir:= ServerHome;
    STL := TStringList.Create();
    try
        HTML.ExportContent(FFName, STL);
        STL.SaveToFile('Converted.html');
    finally
        STL.Free;
        HTML.Free;
    end;
end;



begin
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
            if (ParamStr(1) = '-t') then begin           // This is just a test, remove this block some time.
               TestConvert2HTML(ServerHome + ParamStr(2));
               exit;
            end;
            Serv.BaseDir:=ParamStr(1);
        end;
        if ParamCount > 1 then
            Serv.Port:=StrToIntDef(ParamStr(2),8080);
        Serv.Threaded:=False;
        Serv.AcceptIdleTimeout:=1000;
        Serv.OnAcceptIdle:=@Serv.DoIdle;
        writeln(ParamStr(0), ' starting, serving from ', Serv.BaseDir, ' on port ', Serv.Port);
        Serv.Active:=True;
        // from time to time, a exception relating to POST is raised and apparently dealt with
        // internally. But if running under the debugger in the IDE you get told about it.
    finally
        Serv.Free;
    end;
end.

