unit import_notes;

{ License - see tomboy-ng license information }

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type

    { TImportNotes }

    TImportNotes = class
    private
        function ImportFile(FullFileName: string): boolean;
        function ProcessPlain(Cont: TStringList; const Title: string): boolean;

    public
        ErrorMsg : string;              // '' if everything OK, content means something bad happened
        DestinationDir : string;        // Required, dir to save notes to
        Mode : string;                  // ie plaintext, markdown ....
        ImportNames : TStringList;      // A list of full file names to import, default filename will become title
        FirstLineTitle : boolean;       // if true, first line of note becomes title
        function Execute(): integer;    // you know all you need, go do it.
        constructor Create;
        destructor Destroy; override;
end;



implementation

{ TImportNotes }

uses ttutils, LazFileUtils;


{
<?xml version="1.0" encoding="utf-8"?>
<note version="0.3" xmlns:link="http://beatniksoftware.com/tomboy/link" xmlns:size="http://beatniksoftware.com/tomboy/size" xmlns="http://beatniksoftware.com/tomboy">
	<title>North 2018</title>
	<text xml:space="preserve"><note-content version="0.1">North 2018

    .....

    </note-content></text>
      <last-change-date>2020-05-19T18:58:37.9513193+10:00</last-change-date>
      <last-metadata-change-date>2020-05-19T18:58:37.9513243+10:00</last-metadata-change-date>
    	<create-date>2000-01-01T10:00:00.0000000+11:00</create-date>
    	<cursor-position>1</cursor-position>
    	<width>1000</width>
    	<height>626</height>
    	<x>20</x>
    	<y>30</y>
      <tags>
        <tag>system:notebook:travel</tag>
      </tags>

    	<tags>
    		<tag>system:notebook:travel</tag>
    	</tags>
    	<open-on-startup>False</open-on-startup>
    </note>

}

function TImportNotes.ProcessPlain(Cont : TStringList; const Title : string) : boolean;
var
    DateSt : string = '2020-05-19T18:58:37.9513193+10:00';
begin
    Cont.Insert(0, '	<text xml:space="preserve"><note-content version="0.1">' + Title);
    Cont.Insert(0, '	<title>' + Title + '</title>');
    Cont.Insert(0, '<note version="0.3" xmlns:link="http://beatniksoftware.com/tomboy/link" xmlns:size="http://beatniksoftware.com/tomboy/size" xmlns="http://beatniksoftware.com/tomboy">');
    Cont.Insert(0, '<?xml version="1.0" encoding="utf-8"?>');
    Cont.Add('    </note-content></text>');
    Cont.Add('      <last-change-date>' + DateSt + '</last-change-date>');
    Cont.Add('      <last-metadata-change-date>' + DateSt + '</last-metadata-change-date>');
    Cont.Add('      <create-date>' + DateSt + '</create-date>');
    Cont.Add('      <cursor-position>1</cursor-position>');
    Cont.Add('      <width>1000</width>');
    Cont.Add('      <height>626</height>');
    Cont.Add('    	<x>20</x>');
    Cont.Add('    	<y>30</y>');
    Cont.Add('      <tags>');
    Cont.Add('      </tags>');
    Cont.Add('    	<open-on-startup>False</open-on-startup>');
    Cont.Add('</note>');
    result := True;
end;

function TImportNotes.ImportFile(FullFileName: string): boolean;
var
  Content : TStringList;
  GUID : TGUID;
  Title : string;
  Index : integer = 0;
begin
    Result := True;
    if FileExists(FullFileName) then begin
        try
            Content := TStringList.Create;
            Content.LoadFromFile(FullFileName);
            while Index < Content.Count do begin
                Content.Strings[Index] := RemoveBadXMLCharacters(Content.Strings[Index]);
                inc(Index);
            end;
            if FirstLineTitle then begin
                Title := Content.Strings[0];
                Content.Delete(0);
            end else Title := ExtractFileNameOnly(FullFileName);
            ProcessPlain(Content, Title);
            CreateGUID(GUID);
            Content.SaveToFile(AppendPathDelim(DestinationDir) + copy(GUIDToString(GUID), 2, 36) + '.note');
        finally
            freeandnil(Content);
        end;
    end else begin
        ErrorMsg := 'Failed to open import file' + #10  + FullFileName;
        Result := False;
    end;
end;

function TImportNotes.Execute(): integer;
var
  St : string;
begin
    Result := 0;
    if ImportNames = nil then
        exit;
    for St in ImportNames do begin
        if not ImportFile(St) then
            exit;
        inc(Result);
    end;
end;

constructor TImportNotes.Create;
begin

end;

destructor TImportNotes.Destroy;
begin
    inherited Destroy;
end;

end.

