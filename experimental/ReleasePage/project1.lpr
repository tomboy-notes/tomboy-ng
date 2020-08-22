program project1;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Classes, SysUtils, CustApp
    { you can add units after this };

type

    { TMyApplication }

    TMyApplication = class(TCustomApplication)
    protected
        procedure DoRun; override;
    public
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;
        procedure WriteHelp; virtual;
    end;


Type
  TStringArray = Array of string;
  TStringArrayArray = Array of TStringArray;

{ TMyApplication }

const
  //VER = '0.26';
  WEB = 'https://github.com/tomboy-notes/tomboy-ng/releases/download/v';


var
  Packs : TStringArrayArray;
  ErrorCount : integer;
  Version : string;

function MatchMarker(const Marker : string) : string;
var
  I : integer = 0;
begin
    Result := 'ERROR, failed match';
    //writeln('Processing marker [' + Marker +']');
    if Marker = '$$GPGKEY' then
        exit('https://raw.githubusercontent.com/tomboy-notes/tomboy-ng/master/package/tomboy-ng-GPG-KEY');
    while I < Length(Packs) do begin
        if (('$$' + Packs[i][0]) = Marker) then begin
            exit(WEB + Version + '/' + Packs[i][1] + Version + Packs[i][2]);
        end;
        inc(i);
    end;
    inc(ErrorCount);
    writeln('ERROR - cannot match ' + Marker);
end;

procedure ReplaceMarker(var Str : string);
var
  MPos, MLength : integer;
  Marker : string;
begin
    MPos := Pos('$$', Str);
    if MPos > 0 then begin
        MLength := 2;
        while Str[MPos + MLength] in [ '0'..'9', 'A'..'Z' ] do begin
            inc(MLength);
            if length(Str) < (MPos+MLength) then break;
        end;
        Marker := copy(Str, MPos, MLength);
        //writeln('Length of [' + Marker + '] = ' + inttostr(MLength) + ' Replace with ' + MatchMarker(Marker));
        Delete(Str, MPos, MLength);
        Insert(MatchMarker(Marker), Str, MPos);
    end;
end;

procedure ScanFile(const InFileName, OutFileName : string);
var
  InFile, OutFile: TextFile;
  InString : string;
begin
    AssignFile(InFile, InFileName);
    if OutFileName <> '' then
        AssignFile(OutFile, OutFileName);
    try
        try
            Reset(InFile);
            if OutFileName <> '' then
                Rewrite(OutFile)
            else
                writeln('Writing to console');
            while not eof(InFile) do begin
                readln(InFile, InString);
                while Pos('$$', InString) > 0  do
                    ReplaceMarker(InString);
                if OutFileName <> '' then
                    writeln(OutFile, InString)
                else writeln(InString);
            end;
        finally
            if OutFileName <> '' then
                CloseFile(OutFile);
            CloseFile(InFile);
        end;
    except
        on E: EInOutError do
            writeln('File handling error occurred. Details: ' + E.Message);
    end;
end;

procedure TMyApplication.DoRun;
var
    ErrorMsg: String;
    InFileName : string = '';
    OutFileName : string = '';
begin
    // quick check parameters
    ErrorMsg:=CheckOptions('h', 'help infile: outfile: verstr:');
    if ErrorMsg<>'' then begin
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
    if HasOption('verstr') then
        Version := GetOptionValue('verstr')
    else begin
        WriteHelp();
        Terminate;
        exit;
    end;

    if HasOption('infile') then
        InFileName := GetOptionValue('infile')
    else begin
        WriteHelp();
        Terminate;
        exit;
    end;
    if HasOption('outfile') then
        OutFileName := GetOptionValue('outfile');

    ErrorCount := 0;

    Packs := TStringArrayArray.Create(
               TStringArray.Create('DEB64',    'tomboy-ng_',       '-0_amd64.deb'),
               TStringArray.Create('DEB32',    'tomboy-ng_',       '-0_i386.deb'),
               TStringArray.Create('DEB64QT',  'tomboy-ng_',       '-0_amd64Qt.deb'),
               TStringArray.Create('DEB32ARM', 'tomboy-ng_',       '-0_arm.deb'),
               TStringArray.Create('RPM64',    'tomboy-ng-',       '-2.x86_64.rpm'),
               TStringArray.Create('RPM32',    'tomboy-ng-',       '-2.x86.rpm'),
               TStringArray.Create('RPM64QT',  'tomboy-ngQt-',     '-2.x86_64.rpm'),
               TStringArray.Create('TGZ64',    'tomboy-ng-',       '.tgz'),
               TStringArray.Create('TGZ32',    'tomboy-ng32-',     '.tgz'),
               TStringArray.Create('DMG64',    'tomboy-ng64_',     '.dmg'),
               TStringArray.Create('DMG32',    'tomboy-ng32_',     '.dmg'),
               TStringArray.Create('EXE',      'tomboy-ng-setup-', '.exe')
               );

    scanfile(InFileName, OutFileName);
    if ErrorCount > 0 then
        writeln('ERRORS were recorded, in fact ', inttostr(ErrorCount));

    // stop program loop
    Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
    inherited Create(TheOwner);
    StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
    inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
var
    I : integer =0;
begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
    writeln('       --verstr=Ver.Str             Required eg 0.26');
    writeln('       --infile=Template            Required eg Releases.template');
    writeln('       --outfile=OutFile            eg Releases.md');
    while I < Length(Packs) do begin
        writeln('   $$' + Packs[i][0]);
        inc(i);
    end;
end;

var
    Application: TMyApplication;
begin
    Application:=TMyApplication.Create(nil);
    Application.Title:='My Application';
    Application.Run;
    Application.Free;
end.

