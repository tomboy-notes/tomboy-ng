program webserver;
{$ifdef WINDOWS} {$apptype console} {$endif}
{$mode objfpc}{$H+}

{
/home/dbannon/bin/FPC/fpc-3.2.3/bin/fpc -MObjFPC -Scaghi -Cg -CirotR -O1 -gw3 -gl -gh -gt -l -vewnhibq -Fu/home/dbannon/Pascal/tomboy-ng/experimental/Misty-Small/  -omisty-server -dLCL -dLCLgtk2 webserver.lpr
}

{ This is a simple version of Misty, it does not offer any way to view or edit
  notes online. It will run in SSL secure mode if given a certificate and password
  or in insecure mode, (not recommended) without the above.

  Absolutly must NOT be used on the internet in insecure mode !

  To see the options, try this -

  $> ./misty-server -h

  Insecure Mode eg (using built in defaults for repo location and port)
  $> ./misty-server

  Secure Mode (recommended) eg -
  $> ./misty-server -p 8080 -r ~/Misty -c domain.crt -k domain.key -w TrustMe

  (please don't use 'TrustMe' as your password !)

  check its running by browsing to the url printed at startup if you use an official
  certificate or are willing to se a browser security exception.

  Config tomboy-ng's Misty Sync with Repo set to that URL and, of course the password
  if running secure.

  Do not use on the internet in insecure mode ! Local, secure home networks should be OK, maybe.

  Every care but absolutly no responsability !
  David Bannon, May 6, 2026
}

{$DEFINE MISTY-SMALL}
{$define USE-SSL}
// eg openssl req -newkey rsa:2048 -x509 -days 365 -keyout domain.key -out domain.crt -nodes

uses
    {$IFDEF UNIX}
    cthreads, Unix, BaseUnix,     // for Signal Names
    {$ENDIF}
    {$ifdef USE-SSL}
    opensslsockets,
    {$endif}
    Classes, SysUtils, CustApp, IniFiles,
    TWebserver,
    ssync_utils, LazUTF8, LazFileUtils;

type

    { TMyApplication }

    TMyApplication = class(TCustomApplication)
    private
        Certificate, CertKey, HomeDir : string;
        Port : integer;
        UseSSL : boolean;
        function BuildServer: boolean;
        function CommandLineOK(): boolean;
        // function ReadConfig(): boolean;
    protected
        procedure DoRun; override;
    public
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;
        procedure WriteHelp; virtual;
    end;


{$ifndef MISTY-SMALL}
var ReqFiles : array of string = ('editor_1.template', 'editor_2.template', 'editor_3.template');
{$endif}

{ TMyApplication }

var
    Application: TMyApplication;



function GetDefaultRepoDir() : string;
begin
    {$IFDEF UNIX}
    Result := GetUserDir() + 'Misty/';
    {$ENDIF}
    {$IFDEF DARWIN}
    Result := GetUserDir() + 'Library/Application Support/Misty/';
    {$ENDIF}
    {$IFDEF WINDOWS}
    Result := GetUserDir() + 'Misty\';            // ???
    // %APPDATA%\Tomboy\notes\
    {$ENDIF}
end;

{ function TMyApplication.ReadConfig() : boolean;
var
    ConfigFile : TINIFile;
begin
    // Record Repo dir, full path to two cert files (if '' then insecure), port, pw.
    // eg [BasicSettings]
    //    NotesPath=/home/dbannon/.local/share/tomboy-ng-test/
end;    }


function TMyApplication.BuildServer : boolean;
begin
    if UseSSL then
    writeln('NOTICE, secure mode but please be aware of security issues!')
    else writeln('WARNING, insecure, be aware of issues if not on a secure home network!');
    Serv:=TMistyHTTPServer.Create(Nil);
    Serv.BaseDir := HomeDir;
    Serv.Port := Port;                 // defaults to 8088
    if UseSSL then begin
        Serv.UseSSL := True;
        Serv.CertificateData.PrivateKey.FileName := CertKey;
        Serv.CertificateData.Certificate.FileName := Certificate;
    end;
    Serv.Startup;
    Result := True;
end;

function TMyApplication.CommandLineOK() : boolean;    // false if error .....
var
    ErrorMsg: String;
begin
    ErrorMsg := CheckOptions('hdr:p:k:c:w::', 'help ssl debug port: repo: key: cert:');
    if ErrorMsg <> '' then begin
        writeln('ERROR - ' + ErrorMsg);
        //ShowException(Exception.Create(ErrorMsg));  // Leaks
        Exit(false);
    end;
    Port := 8088;       // default, works for ssl or not
    if HasOption('h', 'help') then begin
        WriteHelp;
        Exit(false);
    end;
    if HasOption('d', 'debug') then
        DebugMode := True;
    if HasOption('c', 'cert') then
        Certificate := GetOptionValue('c', 'cert');
    if HasOption('k', 'key') then
        CertKey := GetOptionValue('k', 'key');
    if HasOption('p', 'port') then
        Port := strtoint(GetOptionValue('p', 'port'))
    else Port := 8088;
    if HasOption('w', 'password') then
        PW := GetOptionValue('w', 'password');
    // ---------- Check repo location.
    if HasOption('r', 'repo') then
        HomeDir := GetOptionValue('r', 'repo')
    else HomeDir := GetDefaultRepoDir();
    // ToDo : below is only (?) dependency on LazFileUtils, paste it in locally ?
    if not DirectoryIsWritable(HomeDir) then begin               // Lazutils
        if DebugMode then writeln('TMyApplication.CommandLineOK - repo directory needs checking');
        if DirectoryExists(HomeDir) then begin                   // sysutils, must be unwritable
            writeln('ERROR Dir [' + HomeDir + '] cannot be written to.');
            WriteHelp;
            exit(false);
        end else begin
            ForceDirectoriesUTF8(HomeDir);       // systils, does recurse
            if DebugMode then writeln('TMyApplication.CommandLineOK - trying to create ', HomeDir);
            if not DirectoryExists(HomeDir) then begin
                writeln('ERROR Dir [' + HomeDir + '] cannot be created');
                WriteHelp;
                exit(false);
            end;
        end;
    end;                         // if to here, repo is OK
    {$ifndef MISTY-SMALL}
    for i := 0 to high(ReqFiles) do
        if not FileExists(HomeDir + ReqFiles[i]) then begin
            writeln('ERROR, cannot see ' + HomeDir + ReqFiles[i]);           // ToDo : windows ?
            Exit(false);
        end;
    {$endif}
    // ---------- Check of a correct request to use SSL
    if ((Certificate <> '') or (CertKey <> '') or (PW <> '')) then begin                     // user wants SSL
        if ((Certificate = '') or (CertKey = '') or (PW = '')) then begin                    // but at least one is missing
            writeln('ERROR, must provide all 3, a Certificate, Key and Password to use SSL');
            WriteHelp;
            Exit(false);
        end;
        UseSSL := True;
    end else writeln('----- Running in insecure mode ! -----');
    if DebugMode then writeln('TMyApplication.CommandLineOK - Repo dir OK');
    Result := True;
end;

procedure TMyApplication.DoRun;      // reads and checks options first
begin
    if CommandLineOK() then
        try
            BuildServer()
        except on EControlC do begin             // I suspect this is not used in Unix, the FPSignal works first ??
                    ExitNow := True;
                    Terminate;                   // maybe redundant ?
                end;
        end
    else
        Terminate;  // stop program loop, DoRun is called repeatably !
    // but we only get here when app has been terminated.
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
    writeln('tomboy-ng Sync Webserver - Options :');
    writeln('  -r Dir | --repo=Dir   Repo Dir to store sync files. Default ~/Misty');
    writeln('  -p PortNumber         Port to run on, default is 8088');
    writeln('  -c certificate        A valid SSL certificate (maybe self signed)');
    writeln('  -k key                A valid SSL key file that matches above');
    writeln('  -d                    Debug mode');
    writeln('  -w                    Set a new Pass Word, no spaces');
    writeln('  eg  misty-server --repo=/home/dbannon/Misty');
    writeln('Do NOT use ports below 1024, requires root, not necessary, not safe.');
    writeln('If you provide one, must provide all of Cert, Key, Password');
    writeln('Maybe you want a self signed certificate for SSL ? try this -');
    writeln('openssl req -newkey rsa:2048 -x509 -days 365 -keyout domain.key -out domain.crt -nodes');
    writeln('');
end;

{$ifdef Linux}     // not in Windows so an exception, https://fpc-pascal.freepascal.narkive.com/TBXENFF1/econtrolc-exception
procedure HandleSigInt(aSignal: LongInt); cdecl;
begin
    if DebugMode then begin
        case aSignal of
            SigInt : Writeln('Ctrl + C used, will clean up and shutdown.');
            SigTerm : writeln('TERM signal, will clean up and shutdown.');
        else
            begin writeln('Some signal received ??'); exit; end;
        end;
    end;
    ExitNow := True;            // Watched by the Idle method
    Application.Terminate;
end;
{$endif}

begin
//    writeln(unix.GetHostName(), '.', GetDomainName());
    {$ifdef LINUX}
    if FpSignal(SigInt, @HandleSigInt) = signalhandler(SIG_ERR) then begin
        Writeln('Failed to install signal error: ', fpGetErrno);
        exit;
    end;
    if FpSignal(SigTerm, @HandleSigInt) = signalhandler(SIG_ERR) then begin
        Writeln('Failed to install signal error: ', fpGetErrno);
        exit;
    end;
    {$endif}
    DebugMode := False;
    Application := TMyApplication.Create(nil);
    Application.Title := 'My Application';
    Application.Run;
    Application.Free;
end.

