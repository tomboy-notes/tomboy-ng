unit QtWorkaround;
{$mode objfpc}{$H+}

// does nothing except in Qt inserts an env var QT_QPA_PLATFORM=xcb into
// the app's perceived env.
// That is not a quarentee that xcb is being used or even available.
// https://forum.lazarus.freepascal.org/index.php/topic,74009.15.html

interface

implementation

{$if defined(LCLQt5) or defined(LCLQt6)}
uses
      InitC, BaseUnix;
     
function setenv(const name, value: pchar; overwrite: cint): cint; cdecl; external clib;

initialization
    setenv('QT_QPA_PLATFORM', 'xcb', 1);
    // writeln('Session type is ', fpGetEnv('XDG_SESSION_TYPE')); // note, this NOT changed
    {$ifdef LINUX}
    writeln('Using xcb to get around ugly Wayland bugs');
    {$endif}
    // ToDo : should check that xcb is installed
{$endif  qt5 or qt6}
end.
