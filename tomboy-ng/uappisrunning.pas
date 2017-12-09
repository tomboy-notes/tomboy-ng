unit uappisrunning;

// Based almost completely on http://wiki.freepascal.org/AppIsRunning
// Know to work Under Linux, Windows10 and MacOSX as of Dec 2017, DRB
// Very minor changes to Linux/Unix function, now count process found,
// extra code for Mac's ps line, don't use deprecated TProcess.CommandLine
// Tidied up IFDEF, now refer to 'UNIX' but some others will require
// alternative options to 'ps'.
// Added function to easily detect if another copy of me is already running.

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils
  {$IFDEF WINDOWS}, Windows, JwaTlHelp32{$ELSE}, process{$ENDIF};
// JwaTlHelp32 is in fpc\packages\winunits-jedi\src\jwatlhelp32.pas

// Returns TRUE if EXEName is running under Windows or Linux
// Don't pass an .exe extension to Linux!

function AlreadyRunning():Boolean;
function AppIsRunning(const ExeName: string):Boolean;

implementation

uses Forms, LazUTF8;
	// Need 'Forms' so we can get Application.ExeName

// These functions return Zero if app is NOT running
// Override them if you have a better implementation
{$IFDEF WINDOWS}
function AppsRunning(const ExeName: string): integer;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  Result := 0;
  while integer(ContinueLoop) <> 0 do
    begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
      UpperCase(ExeName)) or (UpperCase(FProcessEntry32.szExeFile) =
      UpperCase(ExeName))) then
      begin
      Inc(Result);
      // SendMessage(Exit-Message) possible?
      end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
  CloseHandle(FSnapshotHandle);
end;
{$ELSE} 		// All Unixes but some might need more fiddling with ps options
function AppsRunning(const ExeName: string): integer;
var
  t: TProcess;
  s: TStringList;
  Index : Integer = 0;
begin
  Result := 0;
  t := tprocess.Create(nil);
  // t.CommandLine := 'ps -C ' + ExeName;
  t.Executable := 'ps';
  {$IFDEF DARWIN}
  t.Parameters.Add('x');
  t.Parameters.Add('-c');
  t.Parameters.Add('-ocommand');
  {$ELSE}
  t.Parameters.Add('-C');
  t.Parameters.Add(ExeName);
  {$ENDIF}
  t.Options := [poUsePipes, poWaitonexit];
    try
    t.Execute;
    s := TStringList.Create;
      try
      	s.LoadFromStream(t.Output);
      	repeat
      		Index := UTF8Pos(ExeName, s.Text, Index+1);
        	if Index <> 0 then inc(Result);
      	until Index = 0;
      finally
      s.Free;
      end;
    finally
    t.Free;
    end;
end;
{$ENDIF}

function AlreadyRunning() : Boolean;
begin
	Result := (AppsRunning(ExtractFileName(Application.ExeName)) > 1);
end;


function AppIsRunning(const ExeName: string):Boolean;
begin
  Result := (AppsRunning(ExtractFileName(ExeName)) > 0)
end;

end.


