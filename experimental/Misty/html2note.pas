unit html2note;

{
https://quilljs.com/docs/formats
multilevel bullets are implemented as an indented list, needs testing.
Still need to address highlight (any color, we will disable that.
we must tune up tool bar to limit formats to ones we agree on.

Expects to find the original note in ~/home, needs to get header and footer.
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
      NoteFName : string;             // {UID}.note, supplied from calling method (except in test mode)
      function Convert() : boolean;
      function SaveNote(FFName : string) : boolean;
      function LoadHTML(FFName : string) : boolean;   // This is used in test mode where we load content from a file
    private

      procedure RemoveATag();
      procedure FixLists();
      procedure AddHeaderFooter();
      function GetHeaderFooter(var Header, Footer: string): boolean;
end;

implementation

uses Unix;

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

function THTML2Note.Convert(): boolean;    // ToDo : my test setup has a note title at top of document, maybe from note->html ?
begin

    InStr := InStr.Replace('&nbsp;', ' ', [rfReplaceAll]);    // space
    InStr := InStr.Replace('&quot;', '"', [rfReplaceAll]);    // double inverted comma
    InStr := InStr.Replace('&#39;', '''', [rfReplaceAll]);    // single inverted comma - let encoded angle brackets go through ....
    InStr := InStr.Replace('<p>', '', [rfReplaceAll]);
    InStr := InStr.Replace('</p>', #10, [rfReplaceAll]);

    SaveNote('Content.html');                                                    // ToDo : remove this !

    InStr := InStr.Replace('<s>',  '<strikeout>', [rfReplaceAll]);
    InStr := InStr.Replace('</s>', '</strikeout>', [rfReplaceAll]);
    InStr := InStr.Replace('<u>',  '<underline>', [rfReplaceAll]);
    InStr := InStr.Replace('</u>', '</underline>', [rfReplaceAll]);
//    SaveNote('st1.txt');
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
    //writeln('THTML2Note.Convert - finished conversion, ErrorString is ' + ErrorString);
    SaveNote('Content.note');                                                   // ToDo : remove this !
end;

function THTML2Note.SaveNote(FFName: string): boolean;
var OutFile: TextFile;
begin
    // writeln('THTML2Note.SaveNote - saving ' + FFName);
    AssignFile(OutFile, FFName);
    rewrite(OutFile);
    write(OutFile, InStr);
    CloseFile(OutFile);
    result := true;
end;

function THTML2Note.LoadHTML(FFName: string): boolean;    // maybe just for testing ??
var InFile: TextFile;
begin
    AssignFile(InFile, FFName);
    Reset(InFile);
    readln(InFile, InStr);      // assumes no newlines ?
    CloseFile(InFile);
    NoteFName := copy(FFName, 1, 41);
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
end;

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
    DateSt : string;
begin
    STL := TStringList.Create;
    try
        STL.LoadFromFile('home/' + NoteFName);
        Index := FindInStringList(STL, '<text xml:space="preserve">');
        if Index < 0 then begin
            ErrorString := 'Failed to find header in ' + NoteFName;
            exit(false);
        end;
        for i := 0 to Index do
            if i = 0 then
                Header := STL[i]
            else
                Header := Header + #10 + STL[i];
        Header := Header + #10;
        DateSt := TB_GetLocalTime;
        Footer := Footer + #10 + '  <last-change-date>' + DateSt + '</last-change-date>';
        Footer := Footer + #10 + '  <last-metadata-change-date>' + DateSt + '</last-metadata-change-date>';
        Index := FindInStringList(STL, '<create-date>');
        if Index < 0 then begin
            ErrorString := 'Failed to find footer in ' + NoteFName;
            exit(false);
        end;
        for i := Index to STL.Count-1 do
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

