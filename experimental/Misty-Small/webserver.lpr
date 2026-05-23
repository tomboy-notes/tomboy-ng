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
    Classes, SysUtils, CustApp, IniFiles, base64,
    TWebserver,
    ssync_utils, LazUTF8, LazFileUtils
    {$ifdef UNIX}, termIO{$endif};                              // use in the GetPassword function



type

    { TMyApplication }

    TMyApplication = class(TCustomApplication)
    private
        // UseSSL : boolean;
        function BuildServer: boolean;
        function CommandLineOK(): boolean;
        function GetPassword() : string;
        procedure WriteConfig;
        function ReadConfig() : boolean;
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
    ConfigFileName : string = 'misty.cfg';

function GetDefaultRepoDir() : string;
begin
    {$IFDEF UNIX}
    Result := GetUserDir() + 'Misty/';          // in SysUtils
    {$ENDIF}
    {$IFDEF DARWIN}
    Result := GetUserDir() + 'Library/Application Support/Misty/';
    {$ENDIF}
    {$IFDEF WINDOWS}
    Result := GetUserDir() + 'Misty\';            // ???
    // %APPDATA%\Tomboy\notes\
    {$ENDIF}
end;



{ -------------------- TMyApplication ---------------- }

function TMyApplication.BuildServer : boolean;
begin
    if Sett.UsingSSL() then
        writeln('NOTICE, secure mode but please be aware of security issues!')
    else writeln('WARNING, insecure, be aware of issues if not on a secure home network!');
    Serv:=TMistyHTTPServer.Create(Nil);
    Serv.BaseDir := Sett.Repo;
    Serv.Port := Sett.Port;                 // defaults to 8088
    if Sett.UsingSSL then begin
        Serv.UseSSL := True;
        Serv.CertificateData.PrivateKey.FileName := Sett.Key;
        Serv.CertificateData.Certificate.FileName := Sett.Cert;
        Serv.PW := Sett.PW;
    end;
    Serv.Startup;
    Result := True;
end;

function TMyApplication.CommandLineOK() : boolean;    // false if error .....
var
    ErrorMsg: String;
begin
    ErrorMsg := CheckOptions('hdrs:p:k::c::w::D:', 'help ssl debug port: repo: key: cert: save-settings');
    if ErrorMsg <> '' then begin
        writeln('ERROR - ' + ErrorMsg);
        //ShowException(Exception.Create(ErrorMsg));  // Leaks
        Exit(false);
    end;
    if HasOption('d', 'debug') then
        DebugMode := True;
    ReadConfig();
    if HasOption('h', 'help') then begin
        WriteHelp;
        Exit(false);
    end;
    if HasOption('D', 'domain') then begin
        Sett.Domain := GetOptionValue('D', 'domain');
        if Sett.Domain[1] <> '.' then
            Sett.Domain := '.' + Sett.Domain;
    end;
    if HasOption('c', 'cert') then
        Sett.Cert := GetOptionValue('c', 'cert');
    if HasOption('k', 'key') then
        Sett.Key := GetOptionValue('k', 'key');
    if HasOption('p', 'port') then
        Sett.Port := strtoint(GetOptionValue('p', 'port'));
    if HasOption('w', 'password') then begin
        Sett.PW := GetOptionValue('w', 'password');
        if (Sett.PW = '') and (Sett.Cert <> '') and (Sett.Key <> '') then
            Sett.PW := GetPassword();                // prompt user
    end;

    // ---------- Check repo location.
    if HasOption('r', 'repo') then
        Sett.Repo := GetOptionValue('r', 'repo');    // Danger, the '~' is not expanded !
    if Sett.Repo[1] = '~' then begin                 // Unix only, ~ expands to user home dir
        delete(Sett.Repo,1,1);                       // ... and if Repo = '' ?
        Sett.Repo := GetUserDir() + Sett.Repo;
    end;
    if not DirectoryIsWritable(Sett.Repo) then begin               // ToDo : can I use FPAccess() ?
        if DebugMode then writeln('TMyApplication.CommandLineOK - repo directory needs checking');
        if DirectoryExists(Sett.Repo) then begin                   // sysutils, must be unwritable
            writeln('ERROR Dir [' + Sett.Repo + '] exists but cannot be written to.');
            WriteHelp;
            exit(false);
        end else begin                                           // it does not exist
            if DebugMode then writeln('TMyApplication.CommandLineOK - trying to create ', Sett.Repo);
            ForceDirectoriesUTF8(Sett.Repo);       // systils does not recurse // ToDo : might generate EInOutError
            if DebugMode then writeln('TMyApplication.CommandLineOK - tried to create ', Sett.Repo);
            if not DirectoryExists(Sett.Repo) then begin
                writeln('ERROR Dir [' + Sett.Repo + '] cannot be created');
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
    // ---------- Check for a correct request to use SSL

    if Sett.WantsSSL() then begin                            // user wants SSL
        if not Sett.UsingSSL() then begin                    // but at least one is missing
            writeln('ERROR, must provide all 3, a Certificate, Key and Password to use SSL');
            WriteHelp;
            Exit(false);
        end;
        // UseSSL := True;
    end else writeln('----- Running in insecure mode ! -----');
    // At this point, our settings in the variables are as user wants them. Maybe different from ini file.
    if HasOption('s', 'save-settings') then
        WriteConfig();
    if DebugMode then writeln('TMyApplication.CommandLineOK - Repo dir OK');
    Result := True;
end;

function TMyApplication.GetPassword(): string;
{$ifdef UNIX}
var
     oldSettings, newSettings : termios;                   // uses termIO unit;
begin
    tcgetattr(0, OldSettings{%H-});                               // Get current terminal settings
    NewSettings := OldSettings;
    newSettings.c_lflag := newSettings.c_lflag and not ECHO; // Disable echo
    tcsetattr(0, TCSANOW, newSettings);
    Write('Enter password (does not echo) : ');
    {$else}
 begin
    Write('Enter password (does echo) : ');
    {$endif}
    ReadLn(result);
    {$ifdef UNIX}
    tcsetattr(0, TCSANOW, oldSettings);                      // Restore old settings
    {$endif}
end;

procedure TMyApplication.WriteConfig();
var
    SettF : TIniFile;
begin
    SettF := TIniFile.Create(ConfigFileName);
    SettF.WriteString('basic', 'repo', Sett.Repo);
    SettF.WriteString('basic', 'certificate', Sett.Cert);
    SettF.WriteString('basic', 'key', Sett.Key);
    SettF.WriteInteger('basic', 'port', Sett.Port);
    SettF.WriteString('basic', 'pw', EncodeStringBase64(Sett.PW));
    SettF.WriteString('basic', 'domain', Sett.Domain);
    {$ifdef UNIX}                                                               // check what permissions Win can do, fpsetattrUF8()
    fpChmod(ConfigFileName,&600);        // 600, user read/write only
    {$endif}
    SettF.Free;
end;

function TMyApplication.ReadConfig(): boolean;
var
    SettF : TIniFile;
begin
    Result := FileExists(ConfigFileName);
    if DebugMode then
        writeln('TMyApplication.ReadConfig Config File is ', ConfigFileName, ' exists=', booltostr(Result, true));
    SettF := TIniFile.Create(ConfigFileName);
    Sett.Repo   := SettF.ReadString('basic', 'repo', GetDefaultRepoDir());
    Sett.Cert   := SettF.ReadString('basic', 'certificate', '');
    Sett.Key    := SettF.ReadString('basic', 'key', '');
    Sett.Port   := SettF.ReadInteger('basic', 'port', 8088);
    Sett.PW     := DecodeStringBase64(SettF.ReadString('basic', 'pw', encodeStringBase64('')));
    Sett.Domain := SettF.ReadString('basic', 'domain', '.local');    // default works for mDNS
    SettF.Free;
//    if DebugMode then
//        writeln('TMyApplication.ReadConfig Repo=', Sett.Repo, ' Cert=', Sett.Cert, ' Key=', Sett.Key, ' Port=', Sett.Port, ' PW=', Sett.PW);
end;

procedure TMyApplication.DoRun;      // reads and checks options first
begin

    if CommandLineOK() then
        try
            BuildServer()
        except
            on EControlC do begin             // this is not used in Unix, the FPSignal works first
                writeln('TMyApplication.DoRun - EControlC');
                ExitNow := True;
                Terminate;                   // maybe redundant ?
            end;
            on E: Exception do begin
                writeln('Exception reported, beats me ! ', E.message);
                ExitNow := True;
                Terminate;
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
    writeln('  -c certificate        A valid SSL certificate (maybe self signed), or blank.');
    writeln('  -k key                A valid SSL key file that matches above, or blank.');
    writeln('  -d                    Debug mode');
    writeln('  -D Domain             A network facing domain, eg example.com, box uses.');
    writeln('  -w or --passWord      Set a passWord, if password is not present, will prompt');
    writeln('  -s or --save-settings Save current settings (inc password)');
    writeln('  eg  misty-server --repo=/home/dbannon/Misty');
    writeln('Do not set repo to your tomboy-ng notes directory, they are different things');
    writeln('Do NOT use ports below 1024, requires root, not necessary, not safe.');
    writeln('If you provide one, must provide all of Cert, Key, Password');
    writeln('Maybe you want a self signed certificate for SSL ? try this -');
    writeln('openssl req -newkey rsa:2048 -x509 -days 365 -keyout domain.key -out domain.crt -nodes');
    writeln('');
end;

{$ifdef Linux}     // not in Windows so an exception, https://fpc-pascal.freepascal.narkive.com/TBXENFF1/econtrolc-exception
procedure HandleSigInt(aSignal: LongInt); cdecl;
begin
        case aSignal of
            SigInt : Writeln('Ctrl + C used, will clean up and shutdown.');
            SigTerm : writeln('TERM signal, will clean up and shutdown.');
        else
            begin writeln('Some signal received ??'); exit; end;
        end;
    ExitNow := True;            // Watched by the Idle method
    Application.Terminate;
end;
{$endif}

begin
    //writeln(GetHostName(), '.', GetDomainName());
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
    ConfigFileName := GetAppConfigDir(False) + 'misty.cfg';
    DebugMode := False;
    Application := TMyApplication.Create(nil);
    Application.Title := 'My Application';
    Application.Run;
    Application.Free;
end.

