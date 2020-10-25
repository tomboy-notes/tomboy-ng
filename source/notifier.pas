unit notifier;

{$mode objfpc}{$H+}

{   Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------

  A trivial unit that uses libnotify on Linux and a timed out TPopupNotify on other platforms.

  Does very little, on linux it just calls libnotify,  https://github.com/ik5/libnotify-fpc
  On Non-Linux, it calls TPopupNotifier, sets a timer and removes the Popup after the set
  time (in mS).  In both cases, it then destroys itself !

  Yes, you call create but YOU DO NOT FREE it !

  To use it, grab libnotify.pas, add it and this unit to your Lazarus project and do
  something like this -

  procedure TForm1.Button2Click(Sender: TObject);
  var
    Notifier : TNotifier;
  begin
    Notifier := TNotifier.Create;
    Notifier.ShowTheMessage('This is Title', 'and this is the message', 15000);   // 15 seconds
    // Note, don't free it, it frees itself.
  end;

  On Linux, multitheading must be enabled, in the project (lpr) file, add
  {$DEFINE UseCThreads} immediatly above the first "uses" line.

  On Linux, to compile you need the dev version of libnotify but at run time all
  that is needed is libnotify and it appears to be installed on most if not all distros.

  On tomboy-ng, my model is that "oh, you might like to know" type notifications take the common
  6 seconds. Things the end user might really need to know, such as sync drive not being available
  I give 12 seconds. Something, really urgent (I don't have any) might get longer. Its incorrect
  to leave messages that really don't need user action there for very long.

}

interface

uses
    Classes, SysUtils,  fpTimer{$ifdef Linux}, libnotify{$else}, PopupNotifier {$endif} ;

Type

{ TNotifier }

 TNotifier = class

    private
        {$ifdef LINUX}
        LNotifier : PNotifyNotification;
        {$else}
        LocalTimer : TFPTimer;
        PopupNotifier: TPopupNotifier;
        procedure TimerFinished( Sender : TObject );
        {$endif}
    public
        procedure ShowTheMessage(const Title, Message : string; ShowTime : integer = 6000);
        destructor Destroy; Override;
        constructor Create();
end;

implementation



procedure TNotifier.ShowTheMessage(const Title, Message : string; ShowTime : integer);
begin
    {$ifdef LINUX}
    notify_init(argv[0]);
    LNotifier := notify_notification_new (pchar(Title), pchar(Message), pchar('dialog-information'));
    notify_notification_set_timeout(LNotifier, ShowTime);                // figure is mS
    notify_notification_show (LNotifier, nil);
    notify_uninit;
    Destroy;
    // Should also check for errors and use TPopupNotifier if Notify won't work
    // But that will have to wait until I find a Linux where it does not work .....
    {$else}
    // Non Linux must use TPopupNotifier
    PopupNotifier := TPopupNotifier.Create(nil);
    PopupNotifier.Text  := Message;
    PopupNotifier.Title := Title;
    PopupNotifier.show;
    LocalTimer := TFPTimer.create(nil);
    LocalTimer.Interval := ShowTime;
    LocalTimer.OnTimer:= @TimerFinished;
    LocalTimer.Enabled := True;
    {$endif}
end;

{$ifndef LINUX}
procedure TNotifier.TimerFinished( Sender : TObject );
begin
    //  writeln('Timer finished');
    LocalTimer.Enabled := false;
    PopupNotifier.hide;
    Destroy;
end;
{$endif}

destructor TNotifier.Destroy;
begin
    {$ifndef LINUX}
    freeandnil(PopupNotifier);
    freeandnil(LocalTimer);
    {$endif}
    inherited Destroy;
end;

constructor TNotifier.Create();
begin
    inherited Create();
end;

end.

