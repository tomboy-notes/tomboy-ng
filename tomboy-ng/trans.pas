unit trans;

{ Contains parent, abstract class that does Transport part of tomboy-ng sync.
  It moves files around, one way or another, determined by its children.
  *  Copyright (C) 2018 David Bannon
  *  See attached licence file.

  HISTORY
    2018/10/25  Much testing, support for Tomdroid.
    2018/10/28  Added password
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, SyncUtils;


type

  { TTomboyTrans }

  TTomboyTrans = class      // An abstract class, parent of eg FileTrans and NetTrans
    private
          // Hold any parameters
        names : TStringList;
        values : TStringList;

    public

        { Set to '' is no errors. }
        ErrorString : string;
        { Local notes directory }
        NotesDir, ConfigDir : string;
        { The current server ID. Is set with a successful TestTransport call. }
        ServerID : string;
        { The current Server Rev, before we upload. Is set with a successful
              TestTransport call. }
        ServerRev : integer;

        constructor create;
        destructor Destroy; override;

        { Tests availability of remote part of connection. For file sync (eg) thats
            existance of remote manifest and 0 dir, write access. Sets its own ServerID.
            This would be a good place to put lock or authenticate as  necessary}
        function TestTransport() : TSyncAvailable;     virtual; abstract;

        { May (or may not) do some early transport tests, ie, in Tomdroid sync
              it pings the remote device. Should return SyncReady or an error value
              if something failed.}
        function SetTransport() : TSyncAvailable; virtual; abstract;

        {Request a list of all notes the server knows about. Returns with Last Change
            Date (LCD) if easily available and always if GetLCD is true. We don't use all
            fields in TInfoList, the list must have been created.}
        function GetNotes(const NoteMeta : TNoteInfoList) : boolean; virtual; abstract;

        { Execute changes om the server }
        function PushChanges(notes : TNoteInfoList) : boolean; virtual; abstract;

        { Tells Trans to deal with with remote mainfest. This is the trigger
              for a new revision on the server, the server must now do whatever
              it needs to accomodate the new new revision, some new or update
              notes will be sent to it a bit later. New RevNo will be RemoteServerRev plus 1 }
        function DoRemoteManifest(const RemoteManifest : string) : boolean; virtual; abstract;

        function getPrefix(): string; virtual; abstract;

        function IDLooksOK() : boolean; virtual; abstract;

        procedure setParam(p : String; v : String);
        function getParam(p : String) : String;


  end;


implementation

{ TTomboyTrans }

constructor TTomboyTrans.Create;
begin
  names := TStringList.Create();
  values := TStringList.Create();
end;

destructor TTomboyTrans.Destroy;
begin
  writeln('Destroying trans' + getPrefix());
  FreeAndNil(names);
  FreeAndNil(values);
  inherited Destroy;

end;


procedure TTomboyTrans.setParam(p : String; v : String);
var
  i : LongInt;
begin
  writeln('SetParams( '+p+' , '+v+' )');

  i := names.IndexOf(p);
  if(i<0) then begin
     names.add(p);
     values.add(v);
  end else values.Strings[i] :=v;
  i := names.IndexOf(p);
end;

function TTomboyTrans.getParam(p : String) : String ;
var
  i : LongInt;
  v : String;
begin
  i := names.IndexOf(p);
  if(i<0) then v := ''
  else v := values.strings[i];
  writeln('GetParams( '+p+' ) => '+v);
  Result := v;
end;

end.





