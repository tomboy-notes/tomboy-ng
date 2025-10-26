unit html2note;

{   Part of the tomboy-ng Misty project.

   Converts HTML versions of a note back to the tomboy-ng XML format.

    Copyright (C) 2017-2025 David Bannon

    License:
    This code is licensed under MIT License, see the file License.txt
    or https://spdx.org/licenses/MIT.html  SPDX short identifier: MIT

https://quilljs.com/docs/formats
multilevel bullets are implemented as an indented list, needs testing.
Still need to address highlight (any color, we will disable that.
we must tune up tool bar to limit formats to ones we agree on.

Expects to find the original note in ~/home, needs to get header and footer.

Refs -
https://quilljs.com/
https://developer.mozilla.org/en-US/docs/Learn_web_development/Extensions/Forms/Sending_forms_through_JavaScript
https://dev.to/davidking/a-guide-to-http-post-requests-in-javascript-no7
https://www.w3schools.com/js
https://www.w3schools.com/jsref/tryit.asp?filename=tryjsref_api_fetch_easy
}

{$mode ObjFPC}{$H+}

interface
uses
    Classes, SysUtils;

type

{ THTML2Note }

 THTML2Note = class
    public
      InStr : string;
      ErrorString : string;
                          // {UID}.note, supplied from calling method (except in test mode)
                          // needed to get header an footer information.
      NoteFFName : string;
      DateSt : string;
      constructor Create();
      destructor Destroy; override;
      function Convert() : boolean;
                                // Returns true if it has created a new directory and saved the file in the Rev Dir
                                // False if dir already exists, cannot be created or file cannot be saved
      function SaveNote(const ServHome, FName: string; Rev: integer): boolean;
      function LoadHTML(FFName : string) : boolean;   // This is used in test mode where we load content from a file
    private

      procedure RemoveATag();
      procedure FixLists();
      procedure AddHeaderFooter();
      function GetHeaderFooter(var Header, Footer: string): boolean;
end;

implementation

uses Unix, ssync_utils, LazFileUtils;
    //LazFileUtils;       // For ForceDirectory

function FindInStringList(const StL : TStringList; const FindMe : string) : integer;
var
    I : integer = 0;
begin
    if Stl = nil then exit(-1);
    while i < StL.Count {-1} do begin
        if pos(FindMe, StL.strings[i]) > 0 then
            exit(i);
        inc(i);
	end;
	result := -1;
end;

function TB_GetLocalTime: ANSIstring;
var
   ThisMoment : TDateTime;
   Res : ANSIString;
   Off : longint;
   PicoSeconds : string;
begin
    {$ifdef LINUX}
    ReReadLocalTime();    // in case we are near daylight saving time changeover
    {$endif}
    ThisMoment:=Now;                          // ToDo : should seed the random generator, really does not matter but ....
    PicoSeconds := inttostr(random(9999));    // add some certainty when using LCD as key to sync
    while length(PicoSeconds) < 4 do PicoSeconds := '0' + PicoSeconds;
    Result := FormatDateTime('YYYY-MM-DD',ThisMoment) + 'T'
                    + FormatDateTime('hh:mm:ss.zzz',ThisMoment) + PicoSeconds;
    Off := GetLocalTimeOffset();
    if (Off div -60) >= 0 then Res := '+'
	else Res := '-';
	if abs(Off div -60) < 10 then Res := Res + '0';
	Res := Res + inttostr(abs(Off div -60)) + ':';
       	if (Off mod 60) = 0 then
		Res := res + '00'
	else Res := Res + inttostr(abs(Off mod 60));
    Result := Result + res;
end;

{ THTML2Note }

constructor THTML2Note.Create();
begin
    inherited Create;

end;

destructor THTML2Note.Destroy;
begin

    inherited Destroy;
end;

function THTML2Note.Convert(): boolean;    // ToDo : my test setup has a note title at top of document, maybe from note->html ?
begin
    InStr := InStr.Replace('&nbsp;', ' ', [rfReplaceAll]);    // space
    InStr := InStr.Replace('&quot;', '"', [rfReplaceAll]);    // double inverted comma
    InStr := InStr.Replace('&#39;', '''', [rfReplaceAll]);    // single inverted comma - let encoded angle brackets go through ....
    InStr := InStr.Replace('<p>', '', [rfReplaceAll]);
    InStr := InStr.Replace('</p>', #10, [rfReplaceAll]);
    InStr := InStr.Replace('<s>',  '<strikeout>', [rfReplaceAll]);
    InStr := InStr.Replace('</s>', '</strikeout>', [rfReplaceAll]);
    InStr := InStr.Replace('<u>',  '<underline>', [rfReplaceAll]);
    InStr := InStr.Replace('</u>', '</underline>', [rfReplaceAll]);
    RemoveATag();
    FixLists();
                                                                          // ToDo : still need to deal with highlight .....
    InStr := InStr.Replace('<strong>', '<bold>', [rfReplaceAll]);
    InStr := InStr.Replace('</strong>', '</bold>', [rfReplaceAll]);
    InStr := InStr.Replace('<em>', '<italic>', [rfReplaceAll]);
    InStr := InStr.Replace('</em>', '</italic>', [rfReplaceAll]);
    InStr := InStr.Replace('<h3>', '<size:large>', [rfReplaceAll]);      // hmm, h1 is title ?
    InStr := InStr.Replace('</h3>', '</size:large>', [rfReplaceAll]);
    InStr := InStr.Replace('<h2>', '<size:huge>', [rfReplaceAll]);       // wot about h4, h5 ?
    InStr := InStr.Replace('</h2>', '</size:huge', [rfReplaceAll]);      // ToDo : must check what <h?> note->html generates.
    AddHeaderFooter();
    result := (ErrorString = '');
end;



function THTML2Note.SaveNote(const ServHome, FName: string; Rev : integer): boolean;     // FName is a Full File Name, with PATH !
var
    OutFile: TextFile;
    DirName : string;
begin
    // writeln('THTML2Note.SaveNote - saving ' + FFName);
    DirName := GetRevisionDirPath(ServHome, Rev+1);
    if DirectoryExists(DirName) then exit(False);                               // Must be new dir !
    if ForceDirectory(DirName) and DirectoryExists(DirName) then begin
        DirName := MyAppendPathDelim(DirName) + extractFileName(FName);
        AssignFile(OutFile, DirName);                                           // We must create a new revision !
        rewrite(OutFile);
        write(OutFile, InStr);
        CloseFile(OutFile);
    end;
    Result := FileExists(DirName);
end;

function THTML2Note.LoadHTML(FFName: string): boolean;    // maybe just for testing ??
var InFile: TextFile;
begin
    AssignFile(InFile, FFName);
    Reset(InFile);
    readln(InFile, InStr);      // assumes no newlines ?
    CloseFile(InFile);
    NoteFFName := copy(FFName, 1, 41);
    result := true;
end;

procedure THTML2Note.RemoveATag();
var index, EndTag : integer;
  x : integer = 0;
begin
    index := InStr.IndexOf('<a ', 0);
    while (index > -1) do begin
        // at this point, i points to start of an <a tag. We remove all meta data.
        EndTag := InStr.IndexOf('>', index);
        if EndTag = -1 then begin
            ErrorString := 'Error ! anchor tag without closing >';
            exit;
        end;
        // writeln(index, '   ', EndTag);
        InStr := InStr.Remove(index, EndTag - index +1);   // +1 ???
        inc(x);
        index := InStr.IndexOf('<a ', index);
        if x > 100 then exit;
    end;
    InStr := InStr.Replace('</a>', '', [rfReplaceAll]);
end;

// To my absolute amazement this seems to handle quill's multilevel lists. Will need
// a lot of testing. A rough start at doing it more deliberatly at bottom of this file.
procedure THTML2Note.FixLists();
begin
    InStr := InStr.Replace('<ul>', '', [rfReplaceAll]);
    InStr := InStr.Replace('</ul>', '', [rfReplaceAll]);
    InStr := InStr.Replace('<li>', '<list><list-item dir="ltr">', [rfReplaceAll]);
    InStr := InStr.Replace('</li>', '</list-item></list>', [rfReplaceAll]);
    InStr := InStr.Replace('</list>', '</list>'+#10, [rfReplaceAll]);  // ????
//    InStr := InStr.Replace('<list>', #10+'<list>', [rfReplaceAll]);             // always start a list on newline
                                                                                // ToDo : maybe not the first item in a block, ? extra blank line sometimes
end;                                                                            // OK, some times this is necessary !

procedure THTML2Note.AddHeaderFooter();
var Header : string = '';
    Footer : string = '</note-content></text>';
begin
    if GetHeaderFooter(Header, Footer) then
        InStr := Header + InStr + Footer
    else
        writeln('ERROR, THTML2Note.AddHeaderFooter : ' + ERRORSTRING);

//    writeln(header);
//    writeln('---------');
//    writeln(footer);
end;

function THTML2Note.GetHeaderFooter(var Header, Footer: string): boolean;
var STL : TStringList;
    Index, i : integer;
    // DateSt : string;
begin
    STL := TStringList.Create;
    try
        // here we get the header and footer from the existing, unedited note and
        // change (Title, ??) and LCD.
        STL.LoadFromFile(NoteFFName);
        Index := FindInStringList(STL, '<text xml:space="preserve">');
        if Index < 0 then begin
            ErrorString := 'Failed to find header in ' + NoteFFName;
            writeln('THTML2Note.GetHeaderFooter - ERROR - ', ErrorString);
            exit(false);
        end;
        for i := 0 to Index do                                                  // ToDo : assumes note has been normalised, won't work, eg, for GNote
            if i = 0 then
                Header := STL[i]
            else
                Header := Header + #10 + STL[i];
        Header := Header + #10;
        DateSt := TB_GetLocalTime;                                              // A Regional because calling process will want it too !
        Footer := Footer + #10 + '  <last-change-date>' + DateSt + '</last-change-date>';
        Footer := Footer + #10 + '  <last-metadata-change-date>' + DateSt + '</last-metadata-change-date>';
        Index := FindInStringList(STL, '<create-date>');
        if Index < 0 then begin
            ErrorString := 'Failed to find footer in ' + NoteFFName;
            exit(false);
        end;
        for i := Index to STL.Count-1 do                                        // Copy remainder of the footer.
            Footer := Footer + #10 + STL[i];
        Footer := Footer + #10
    finally
        STL.free;
    end;


end;



{ ========================= L I S T   S T U F F   ========================
<ul>
	<li>Get quill to allow four preset font sizes.
		<ul>
			<li>Level 2 bullet
				<ul>
					<li>Level 3 bullet</li>
				</ul>
			</li>
		</ul>
	</li>
	<li>Level 1 bullet</li>
</ul>

<s>Strikeout text</s>, ok ? and, just to be fancy, <u>some underline</u> text.

<ul><li><ul><li><ul><li><ul><li>Level four bullets</li></ul></li></ul></li></ul></li></ul>
}


// Called in a loop that finds the next <ul>. The funcion updates Index as it goes
// so its just a case of looking for next <ul>

{
procedure THTML2Note.FixLists();
var Index : integer = 0;   // Points to location in InStr we are processing.
    ListString : string;   // Where we acrue a list block, 1 or more bullets.
    StartUL : integer;
begin
    Index := 0;
    ListString := '';
    while Index < length(InStr) do begin
	    Index := InStr.Find(Index,'<ul>', InStr);   // -1 if not found
	    if Index = -1 then break;
	    StartUL := Index;
	    while BulletLevel(Index, ListString) do
		    Result := Result + ListString;         // Get all bullets in this block
	    ReplaceListContent(StartUL, Index, StringList);
	    Index := StartUL;
    end;
end;           }


// Finds list content starting at Index for List content. Keeps its own record of
// list depth. Initially called with Index pointing to a top level <ul> it keeps
// returning next bullet (with markup) until it encounters a </ul> that corresponds to
// its starting one.  It returns false on the last (and possibly first) item.
// LContent will possibly be valid (ie, not empty) ands need to be used.

// Index points to a <ul> we will return true and load LContent up with TB markup
// for that bullet item. It keeps scanning until it finds, firstly some text and then
// either a </li> or <ul>. Do not return until it has that text. After we have some
// text, a </li> or <ul> indicates end of that bullet line.

{function BulletLevel(var Index : integer; var LContent : string) : boolean;
const BulletLevel : integer = 0;
      InAList : boolean = false;
begin    }

end.

