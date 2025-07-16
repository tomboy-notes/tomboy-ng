program webserver;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}
    cthreads,
    {$ENDIF}
    Classes, SysUtils, CustApp,
    BaseUnix,                     // for Signal Names  Hmm, windows ?  No idea !
    TWebserver, LazFileUtils
    { you can add units after this };

type

    { TMyApplication }

    TMyApplication = class(TCustomApplication)
    private
        function BuildServer: boolean;
    protected
        procedure DoRun; override;
    public
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;
        procedure WriteHelp; virtual;
    end;



var ReqFiles : array of string = ('editor_1.template', 'editor_2.template', 'editor_3.template');


{ TMyApplication }

function TMyApplication.BuildServer : boolean;
var i : integer;
    HomeDir : string;
begin
    if HasOption('H', 'Home') then begin
        HomeDir := AppendPathDelim(GetOptionValue('H', 'home'));
        for i := 0 to high(ReqFiles) do
            if not FileExists(HomeDir + ReqFiles[i]) then begin
                writeln('ERROR, cnnot see ' + HomeDir + ReqFiles[i]);           // ToDo : windows ?
                exit;
            end;
    end else begin
        writeln('ERROR, must specify path to web server home dir, -H dir');
        exit;
    end;

    Serv:=TMistyHTTPServer.Create(Nil);
    Serv.BaseDir := HomeDir;
    // We must hav either -p port OR -s
    if HasOption('s', 'ssl') then
        Serv.Port := 443
    else if HasOption('p', 'port') then
        Serv.Port := strtoint(GetOptionValue('p', 'port'))
    else begin
        writeln('ERROR, must either -s (ie use SSL) or -p Port (port number, eg 8080');
        Serv.Free;
        exit;
    end;
    Serv.Startup;
    Result := True;
end;

procedure TMyApplication.DoRun;
var
    ErrorMsg: String;
begin
    // quick check parameters
    ErrorMsg := CheckOptions('hp:H:sd', 'help port home ssl');
    if ErrorMsg <> '' then begin
        ShowException(Exception.Create(ErrorMsg));
        Terminate;
        Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then begin
        WriteHelp;
        Terminate;
        Exit;
    end;
    if HasOption('d', 'debug') then
        DebugMode := True;

    BuildServer();



    // stop program loop
    Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
    inherited Create(TheOwner);
    StopOnException := True;
end;

destructor TMyApplication.Destroy;
begin
    inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
    { add your help code here }
    writeln('tomboy-ng Sync Webserver - Options :');
    writeln('  -H Dir | --dir=Dir    Dir is home directory for web server, REQUIRED');
    writeln('  -p PortNumber         If running in insecure mode, eg 8080');
    writeln('  -s                    Run with SSL, implies port 443');
    writeln('                        Either -p or -s Required');
    writeln('  -d                    Debug mode');
    writeln('  eg  webserver -H /home/dbannon/Pascal/tomboy-ng/experimental/Misty -p 8080');

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

var
    Application: TMyApplication;
begin
    if FpSignal(SigInt, @HandleSigInt) = signalhandler(SIG_ERR) then begin
        Writeln('Failed to install signal error: ', fpGetErrno);
        exit;
    end;
    if FpSignal(SigTerm, @HandleSigInt) = signalhandler(SIG_ERR) then begin
        Writeln('Failed to install signal error: ', fpGetErrno);
        exit;
    end;
    DebugMode := False;
    Application := TMyApplication.Create(nil);
    Application.Title := 'My Application';
    Application.Run;
    Application.Free;
end.

