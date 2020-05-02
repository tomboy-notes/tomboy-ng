program netsyncTest;

{$mode objfpc}{$H+}

uses
        {$IFDEF UNIX}{$IFDEF UseCThreads}
        cthreads,
        {$ENDIF}{$ENDIF}
        Classes, SysUtils, CustApp
        ,transnet, syncutils, oauth
        { you can add units after this };

type

        { TMyApplication }

		{ TNetTester }

        TNetTester = class(TCustomApplication)
		private
				procedure GetRemoteNotesList;
        protected
                NetSync : TNetSync;
                procedure DoRun; override;
        public
                constructor Create(TheOwner: TComponent); override;
                destructor Destroy; override;
                procedure WriteHelp; virtual;
        end;

{ TMyApplication }

//uses syncutils;

procedure TNetTester.GetRemoteNotesList;
var
    NoteInfoList : TNoteInfoList;
    P : pointer;
begin
    try
      NoteInfoList := TNoteInfoList.Create;
      NetSync.GetNewNotes(NoteInfoList, false);
      for P in NoteInfoList do begin                // NOTE - TList iterator !
          writeln( PNoteInfo(P)^.ID, '  -  ' + inttostr(PNoteInfo(P)^.Rev));
      end;
	finally
      NoteInfoList.Free;
	end;

end;

procedure TNetTester.DoRun;
var
        ErrorMsg: String;

begin
        // quick check parameters
        ErrorMsg:=CheckOptions('h', 'help');
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
        NetSync := TNetSync.Create;
        GetRemoteNotesList;
        { add your program here }

        // stop program loop
        Terminate;
end;

constructor TNetTester.Create(TheOwner: TComponent);
begin
        inherited Create(TheOwner);
        StopOnException:=True;
end;

destructor TNetTester.Destroy;
begin
        freeandnil(NetSync);
        inherited Destroy;
end;

procedure TNetTester.WriteHelp;
begin
        { add your help code here }
        writeln('Usage: ', ExeName, ' -h');
end;

var
        Application: TNetTester;
begin
        Application:=TNetTester.Create(nil);
        Application.Title:='Net Tester';
        Application.Run;
        Application.Free;
end.

