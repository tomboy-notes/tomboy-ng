unit mvxwindow;

{ A small unit that wil move a window between Linux Workspaces using X11 }


{$mode ObjFPC}{$H+}

interface

uses
  x, xlib, xatom, unix, ctypes, sysutils;

type TXWinInfo = record
    XWinID : qword;
    WorkS : integer;         // not implemented yet, needs a XGetWindowProperty() call
    CurrentWorkS : integer;
    ErrorMsg : string;
end;

function XWindowInformation(Caption : string; out Info : TXWinInfo) : boolean;

function MvXWinWorkSpace(Caption : string; WorkS : integer = -1) : boolean;

var
    MvXWinError : string = '';    // contains an error msg if above returns false.

const MvXWinVerNumb = '1.01';     //  release Sept 9, 2024

implementation

{ Notes :

    Copyright (C) 2024 David Bannon <dbannon@internode.on.net>

    License:
    This code is licensed under MIT License, see
    https://spdx.org/licenses/MIT.html  SPDX short identifier: MIT

    Alternativly, your choice, it is released under the GNU General Public License,
    either version 2 of the License, or (at your option) any later version.

    Uses, in some part, the C code for wmctrl as a template. See refs below.
    wmctrl was authored by Tomas Styblo <tripie@cpan.org> and released under GPL 2

    A small unit to move a Window between Linux Workspaces (or "Virtual Desktops").
    You pass the windows Title or Caption and it will be moved to the current
    workspace or include an option integer to move to a specific one. User friendly
    Desptops ( innamon, Mate, XFCe etc) usually have four such workspaces. Others
    have the workspaces hidden or set to just one and you need to look at your
    Desktop's settings.


    Gnome : (Wayland) This works as expected for GTK2. It works for Qt5 and Qt6 if (and
            only if) you use the QT_QPA_PLATFORM=xcb or the similar command line switch.
            It does not work with Wayland, maybe, when Wayland works as it should,
            maybe it will ?

    KDE :   All good, well, almost. When moving a window AWAY from current workspace
            along with the target window, KDE takes the focus to the new workspace too.
            Demonstrated this in both Debian and Mageia KDE. No where else.
            So, not a problem in the "bring a window here" model but ....

    Cinnamon : All good.

    XFCe :  All Good.

    Mate :  All Good.

    Enlightenment : Fails. Generates a AV (caught in this unit) when readining the
            xwindows list trying to match Caption to a WindowID. On an old Enlightenment
            running om an equally old Mageia. Does not crash app but does not behave
            as expected.


    Timing - On my laptop, the recursive scan through all known windows takes some 40mS
            with a largish number of windows open. Additionally, wmctrl suggests a 100mS
            sleep after posting a X11 msg to move a window. My tests indicated flawless
            performance with only 1mS. wmctrl has been around for some time ...
            I have set source below to use 20mS, alter the sleep(2) in MvXWinWorkSpace().

    (XLib) functions like XQueryTree() may fail if given bad data. It will drop a message
    to the console, eg "X Error of failed request:  BadWindow (invalid Window parameter)"
    and terminate. Does not seem to be a way to protect against this happening. So,
    don't mess with the data.

    The Client_Msg() function will post a message, if it turns out you gave it an invalid
    window ID, the message is silently ignored.

    If you send a window to a non-existing workspace, its (usually ?) ignored.
    While this code does set an error message, it was more about debugging than
    production use.

    Because we identify the Window to move by its Caption there is a risk, if you
    have multiple windows with the same caption, that the one 'chosen' is undetermined.
    Reversing the direction, in GetXWinID() of the for loop ?

    Workspaces are usually numbered 0-3 inclusivly but are presented in user
    menus as being 1-4. Requests for an invalid workspace are ignored.
}


            // Recursive function to drill down to window with matching caption.
            // To initialise, pass root window and set FoundWinID to 0.
function FindWindowCaption(Caption : string; Disp : PDisplay; WinID : TWindow;
                                var FoundWinID : TWindow) : boolean;
var
    NumbWindows, i : integer;
    RootWin, ParentRet : TWindow;                       // just dummy place holders
    WinName : pchar;
    ChildWinArray : PPWindow;
begin
    result := False;
    if (XFetchName(Disp, WinID, @WinName) = 1) and (Caption = WinName) then begin
        if FoundWinID = 0 then
            FoundWinID := WinID;            // only set it in successful recurse
        exit(True);
    end;
    XQueryTree(Disp, WinID, @RootWin, @ParentRet, @ChildWinArray, @NumbWindows);  // ret 1 on success.
    for i := 0 to NumbWindows - 1 do begin
        if FindWindowCaption(Caption, Disp, TWindow(ChildWinArray[i]), FoundWinID) then
            exit(True);
    end;
end;

        // Posts a message to X11 that might alter the charactistics of an XWindow
function Client_Msg(Disp : PDisplay; Win : TWindow; Msg {*msg : char} : string; Data0 : qword) : boolean;
var
    event : TXEvent;         // will be 'cased' to event.xclient... as a TXClientMessageEvent event (line #756 xlib.pp)
    mask : longint;
begin
    mask := SubstructureRedirectMask or SubstructureNotifyMask;
    event.xclient._type := ClientMessage;
    event.xclient.serial := 0;
    event.xclient.send_event := 1;
    event.xclient.message_type := XInternAtom(disp, pchar(msg), False);   // '_NET_WM_ACTION_CHANGE_DESKTOP' ? nope.
    if event.xclient.message_type = 0 then exit(false);
    event.xclient.window := win;                                   // the window we want to move
    event.xclient.format := 32;
    event.xclient.data.l[0] := clong(Data0);                       // the desktop we want to move to.
    event.xclient.data.l[1] := clong(0);
    event.xclient.data.l[2] := clong(0);
    event.xclient.data.l[3] := clong(0);
    event.xclient.data.l[4] := clong(0);
    // returns 1 on success.
    Result := (XSendEvent(Disp, DefaultRootWindow(Disp), False, Mask, @event) <> 0);
end;

        // Returns the Window ID of the current Workspace.
function CurrentWorkSpace(Disp : PDisplay; RootWin : TWindow) : integer;                           // this seems to work in KDE
var
    PropInst, type_ret : TAtom;
    Status: integer;
    format_ret: integer;
    nitems_ret, bytes_after_ret: culong;
    prop: Pcuchar;
begin
    PropInst := XInternAtom(Disp, '_NET_CURRENT_DESKTOP', True);
    Status := XGetWindowProperty(Disp, RootWin, PropInst, 0, 1, False,
            XA_CARDINAL, @type_ret,  @format_ret, @nitems_ret, @bytes_after_ret, @prop);
    if Status = 0 then
        Result :=  PCardinal(prop)^         // we know its a Cardinal, deref to get the value.
    else Result := -1;
end;

function XWindowInformation(Caption : string; out Info : TXWinInfo) : boolean;
var
    RootWin : TWindow;
    FoundWinID : TWindow = 0;
    Disp  : PDisplay;
begin
    Disp := xlib.XOpenDisplay(nil);
    try try
        RootWin := RootWindow(Disp, DefaultScreen(Disp));
        Info.CurrentWorkS := CurrentWorkSpace(Disp, RootWin);
        if FindWindowCaption(Caption, Disp, RootWin, FoundWinID) then begin
            Info.XWinID := qword(FoundWinID);
            Info.WorkS := 0;                 // not implemented yet
            Info.ErrorMsg := '';
        end else
            Info.ErrorMsg := 'ERROR, failed to get Window ID';
        result := Info.ErrorMsg = '';
    except
            on E: EAccessViolation do                                           // see note re Enlightenment above
                    Info.ErrorMsg := 'Access Violation when reading XWin Info. Desktop Related.';
    end;
    finally
       XCloseDisplay(Disp);
    end;
end;

        // Sole entry point for this using, tries to move a window with the passed
        // caption to indicated workspace. -1 means move to current active workspace.
function MvXWinWorkSpace(Caption : string; WorkS : integer) : boolean;
var
    Disp: PDisplay;
    WinID, RootWin : TWindow;
begin
    MvXWinError := '';
    Disp := xlib.XOpenDisplay(nil);
    try try
            RootWin := RootWindow(Disp, DefaultScreen(Disp));
            if WorkS = -1 then
                WorkS := CurrentWorkSpace(Disp, RootWin);
            WinID := 0;
            if not FindWindowCaption(Caption, Disp, RootWin, WinID) or (WinID = 0) then begin
                MvXWinError := 'ERROR - Failed to find that Window !';
                writeln('Failed to find that Window !');
                XCloseDisplay(Disp);
                exit(false);
            end;
            // writeln('MvXWinWorkSpace : WinID is 0x', WinID.ToHexString(8));
            if not Client_Msg(Disp, WinID, '_NET_WM_DESKTOP', qword(WorkS)) then begin   // note, no guaranteee mmsg works !
                MvXWinError := 'ERROR - Failed to send Window Move Message !';
                writeln('Failed to Window Move Message');
            end else begin
                sleep(20);                   // wmctrl uses 100mS, on my system, works fine with just 1mS ??
                if not client_msg(Disp, WinID, '_NET_ACTIVE_WINDOW', 0) then begin
                    MvXWinError := 'ERROR - Failed to send Window Activate Message';
                    writeln('Failed to send Window Activate Message');
                end  else
                    XMapRaised(Disp, winID);
            end;
            Result := MvXWinError = '';
        except
            on E: EAccessViolation do                                           // see note re Enlightenment above
                    MvXWinError := 'Access Violation when reading XWin Info. Desktop Related.';
        end;
    finally
        Result := MvXWinError = '';
        if Result then XCloseDisplay(Disp);

    end;

end;

end.

(*      ----------- E X A M P L E   U S A G E -------------

    procedure TForm1.DisplayWindow(Frm : TForm2);
    var
      Nothing : string = '';
    begin
        {$ifdef Linux}             // On Linux, the target form may be in another workspace
        if not CheckUseMvXWin.checked then
            Nothing := 'something';               // just so MvXWinWorkspace fails.
        if MvXWinWorkSpace(Frm.Caption+Nothing) then
             Memo1.Append('Seems to have worked.')
        else begin                 // Normally, this seems highly unlikely ! Maybe Enlightenment DE ?
            Memo1.Append(MvXWinError);
            Memo1.Append('Trying the Hide/Show trick');
            Frm.Hide;
            Frm.show;              // This produces a noticable flicker if the form is already visible
        end;                       // but maybe thats a good idea because it draws the user's attention ?
        {$else}
        Frm.EnsureVisible(True);   // All thats necessary in a non-workspace system
        {$endif}
    end;
*)


{ Refs -
  * https://tronche.com/gui/x/xlib/window-information/XQueryTree.html
  * https://tronche.com/gui/x/xlib/window-information/XGetWindowProperty.html
  * https://specifications.freedesktop.org/wm-spec/latest/ar01s05.html
  * https://tronche.com/gui/x/xlib/event-handling/XSendEvent.html
  * https://stackoverflow.com/questions/67318357/how-to-set-the-position-of-a-wayland-window-on-the-screen
  * https://github.com/Conservatory/wmctrl

  https://forum.lazarus.freepascal.org/index.php/topic,68389.0.html
  https://www.x.org/releases/X11R7.5/doc/man/man3/XChangeProperty.3.html

}
