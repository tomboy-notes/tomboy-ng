unit autostart;
{
    2019/05/24  Display strings all (?) moved to resourcestrings
}
{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type

  { TAutoStartCtrl }

  TAutoStartCtrl = class
    Private
        procedure AutoOff(AppName : string);            // don't need appname Linux or windows, maybe MacOS ?
        procedure AutoOn(AppName: string);
        {$ifdef WINDOWS}
        function WindowsDirectory(CSIDL : integer): string;
        {$endif}
    Public
        ErrorMessage : string;  // If set, trouble Will Roberinson !
        TargetName : string;    // Whatever we need copy, link or what ever to
        LinkName : string;      // What we put in destination
        LinkDestination : string; // Directory we put the above.
        constructor Create(AppName : string; StartIt : boolean);
        destructor Destroy;  override;
  end;

implementation

uses LazLogger, LazFileUtils, FileUtil, LazUTF8
    {$ifdef WINDOWS}, Windows, ShlObj, ActiveX, ComObj, ExtCtrls{$endif};

{ TAutoStartCtrl }

{$ifdef WINDOWS}
function TAutoStartCtrl.WindowsDirectory(CSIDL : integer) : string;
var
   DirArray : array[0..MAX_PATH] of Char;
   PIDL : PItemIDList;
begin
    SHGetSpecialFolderLocation(0, CSIDL, PIDL) ;
    SHGetPathFromIDList(PIDL, DirArray) ;
    Result := DirArray;
    // CSIDL_PROGRAM_FILES -> Where the tomboy-ng directory containg exe is.
    // CSIDL_STARTUP  ->  Where we put our shortcut (or remove it from)
    // CSIDL_APPDATA  ->  top of ~/AppData\Roaming ...... (not needed here)
    // CSIDL_DESKTOPDIRECTORY  -> User's desktop.
end;
{$endif}

procedure TAutoStartCtrl.AutoOn(AppName : string);
{$ifdef WINDOWS}
Var
   IObject : IUnknown;
   ISLink : IShellLink;
   IPFile : IPersistFile;
{$endif}
begin
    {$ifdef LINUX}
        // Just copy the desktop file, too easy.
        if not DirPathExists(LinkDestination) then
            ForceDirectory(LinkDestination);
        if  FileExistsUTF8(TargetName) then
            CopyFile(TargetName, LinkDestination + LinkName)
        else ErrorMessage := 'Cannot find ' + TargetName;
    {$endif}
    {$ifdef WINDOWS}
        // Typically makes a link to c:\Programe Files\tomboy-ng\tomboy-ng64.exe  in
        // C:\Users\dbann\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup\.
        if not FileExistsUTF8(TargetName) then begin
            ErrorMessage := 'Cannot find ' + TargetName;
            exit;
        end;
        IObject := CreateComObject(CLSID_ShellLink) ;
        ISLink := IObject as IShellLink;
        IPFile := IObject as IPersistFile;
        ISLink.SetPath(pChar(TargetName));
        IsLink.SetWorkingDirectory(pChar(ExtractFilePath(TargetName)));
        IPFile.Save(PWChar(WideString(LinkDestination + LinkName)), false);
        // ErrorMessage := TargetName + ' --- ' + LinkDestination + LinkName;
    {$endif}
end;

procedure TAutoStartCtrl.AutoOff(AppName : string);
begin
    if FileExistsUTF8(LinkDestination + LinkName) then
        DeleteFileUTF8(LinkDestination + LinkName);
end;

constructor TAutoStartCtrl.Create(AppName: string; StartIt: boolean);
{$ifdef WINDOWS}var CPU : string;{$endif}
begin
    inherited create;
    ErrorMessage := '';
    {$ifdef LINUX}
    TargetName := '/usr/share/applications/' + AppName + '.desktop';
    LinkDestination := AppendPathDelim(GetEnvironmentVariableUTF8('HOME')) + '.config/autostart';
        LinkName := '/' + AppName + '.desktop';
    {$endif}
    {$ifdef WINDOWS}
    CPU := {$i %FPCTARGETCPU%};
    if CPU = 'i386' then
        CPU := '32'
    else CPU := '64';
    TargetName := WindowsDirectory(CSIDL_PROGRAM_FILES) + '\' + AppName + '\' + AppName + CPU + '.exe';
    LinkDestination := WindowsDirectory(CSIDL_STARTUP);
    LinkName := '\' + AppName + '.lnk';
    {$endif}
    if StartIt then AutoOn(AppName)
    else AutoOff(AppName);
end;

destructor TAutoStartCtrl.Destroy;
begin
    inherited Destroy;
end;

end.
