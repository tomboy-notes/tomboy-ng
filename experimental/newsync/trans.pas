unit trans;

{ Contains parent, abstract class that does Transport part of sync.
  It moves files around, one way or another.
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
  	PNoteInfo=^TNoteInfo;
  	TNoteInfo = record
        // Value : integer;
		ID : ANSIString;
        GMTDate : TDateTime;
    	CreateDate : ANSIString;
    	LastChange : ANSIString;
        Rev : ANSIString;
        Deleted: Boolean;
        Title : ANSIString;
	end;

 type                                 { ---------- TNoteInfoList ---------}
   TNoteInfoList = class(TList)
    private
     	function Get(Index: integer): PNoteInfo;
    public
        destructor Destroy; override;
        function Add(ANote : PNoteInfo) : integer;
        function FindID(const ID : ANSIString) : PNoteInfo;
        property Items[Index: integer]: PNoteInfo read Get; default;
    end;


type

  { TTomboyTrans }

  TTomboyTrans = class      // An abstract class, parent of eg FileTrans and NetTrans
    private

    public
        DebugMode : boolean;
            // Set to '' is no errors.
        ErrorString : string;
            // Local directories
        NotesDir, ConfigDir : string;

            {Request a list of new notes since nominated revision with, ideally,
            last-change-date for each. We don't use all fields in TInfoList }
        function GetNewNotes(NoteMeta : TNoteInfoList; LocRev : integer) : boolean; virtual; abstract;

            {Request that all the notes mentioned in the simple list be downloaded and,
            if necessary, any existing note be moved to Backup.  }
        function DownloadNotes(const DownLoads : TStringList) : boolean; virtual; abstract;

            { Advise server that a note has been deleted (does this trigger a rev ??).
              Can it be a list of notes ? Does this trigger a new revision. }
        function DeleteNote(const ID : string{; var NewRev : integer}) : boolean; virtual; abstract;

           { Push a note, or ideally, a set of notes up to server (triggering a new release). }
        function UploadNotes(const Uploads : TStringList; var NewRev : integer) : boolean; virtual; abstract;
  end;


implementation

{ TTomboyTrans }

{ ----------  TNoteInfoList ------------- }

function TNoteInfoList.Add(ANote : PNoteInfo) : integer;
begin
	result := inherited Add(ANote);
end;

	{ This will be quite slow with a big list notes, consider an AVLTree ? }
function TNoteInfoList.FindID(const ID: ANSIString): PNoteInfo;
var
    Index : longint;
begin
    Result := Nil;
    for Index := 0 to Count-1 do begin
        if Items[Index]^.ID = ID then begin
            Result := Items[Index];
            exit()
		end;
	end;
end;

destructor TNoteInfoList.Destroy;
var
    I : integer;
begin
    for I := 0 to Count-1 do
    	dispose(Items[I]);
    inherited;
end;

function TNoteInfoList.Get(Index: integer): PNoteInfo;
begin
    Result := PNoteInfo(inherited get(Index));
end;

end.

