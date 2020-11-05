unit import_notes;

{   *  Copyright (C) 2020 David Bannon
    *  See attached licence file.
}

{  This unit will convert Mark Down to a Tomboy Note.  It implements some of Common Mark,
   https://spec.commonmark.org but mainly focuses on the way Next Cloud handles markdown.
   At presented, it supports larger fonts only when they are a full paragraph, that is a
   heading.  We can use the ## Heading, ### Heading syntax or Headings underlined with
   ---- and  ====.  In line, we do bold, italic, strikeout, monospaced, bullets.

   We do not attempt to do Highlight, Underline. We use <sub>Small Font</sub> which works
   on Github and the Common Mark demo pages but not on Nextcloud.  If you leave it alone
   it will be preserved in a tomboy-ng -> NextCloud -> tomboy-ng cycle.

  To use this unit, create it, poke DestinationDir in, set Mode, eg markdown, Dates if you have them.
  Then, use in one of two modes -
  1 - Use MDtoNote providing a StringList with md content, a title and ID, it will create a note.
  2 - Poke a stringlist full md files and it will batch process them.
}


{ HISTORY
    2020/09/23 Escape nasty char from note title before saving as TB format.
    2020/10/13 As above for Category/Notebook name. Sigh ....

}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type

    { TImportNotes }

    TImportNotes = class
    private
        GStr : string;

        function ChangeSmallFont(): boolean;
        function ChangeTag(const Tag, ChangeToLead, ChangeToTail: string): boolean;
		function HandleTriple(Tag: string; StartAt: integer): integer;
        function ImportFile(FullFileName: string): boolean;

				                { Will confirm or not that the substring at TStart in GStr is a Tag, TStart
				                is a 1 based index into GStr. If not Leading, its Trailing. A Leading Tag
				                starts at begining of line or after a space, does not have a space or eol
				                immediatly after. A Trailing tag cannot start at beginning of line, must
				                not have a space to its left. A Tag can have 1, 2 or 3 char, no more.
				                As the file may include xml tags, we have to also allow a > before a
				                leading tag or after a trailing one, a < }
		function IsThisATag(const Leading: boolean; const Tag: string; const TStart: integer): boolean;
		function ManageTriples(): boolean;
        function MarkUpMarkDown(Cont: TStringList): boolean;
		function NotSpecialTag(const Index: integer; out TagSize: integer): boolean;

                                { Returns the 1 based pos of the passed Tag, Leading says its a leading tag
                                must have whitespace of newline to left and an alpha mumeric to the right.
                                Reverse if Leading is false.  Ret 0 if a suitable tag is not found. }
        // function PosMDTag(const St, Tag: string; const leading: boolean): integer;

                                { Will wrap the provided content with Tomboy style headers and footers }
        function ProcessPlain(Cont: TStringList; const Title: string): boolean;
		function RemoveSelectedXMLChars(DoQuotes: boolean=false) : boolean;

    public
        ErrorMsg       : string;        // '' if everything OK, content means something bad happened
        DestinationDir : string;        // Required, dir to save notes to
                                { if we know the dates, poke them in here. Else they will be '' and we will
                                fill them in down below with now() They are the string Tomboy date format
                                eg '2020-05-19T18:58:37.9513193+10:00', ISO something or another. }
        CrDate, LCDate : string;
        Notebook       : string;        // We only support one notebook in this mode. A Nextcloud limitation. Needs escaping ....
        Mode           : string;        // ie plaintext, markdown ....
        ImportNames : TStringList;      // A list of full file names to import, default filename will become title
        FirstLineTitle : boolean;       // if true, first line of note becomes title
        function Execute(): integer;    // you know all you need, go do it.

                                { Public function, call passing a stringlist with MD content and appropriate dates
                                or blank dates to stamp now, note will be saved in Destination Dir.
                                We assume content arrives here with Title in seperate var, not in content. }
        function MDtoNote(var STL: TStringList; const Title, ID: string): boolean;

end;



implementation


{$DEFINE TOMBOY_NG}
                                      // ToDo : needs serious retro work to make usable in TT again.
 { TImportNotes }

uses {$ifdef TOMBOY_NG} SyncUtils, Settings, {$ELSE} ttutils, {$ENDIF} LazFileUtils, LazUTF8, LCLProc;   // added syncutils for tomboy-ng


// ------------------  E N T R Y      P O I N T S -------------------------

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

function TImportNotes.MDtoNote(var STL: TStringList; const Title, ID: string): boolean;
var
  Index : integer = 0;
begin
    (*          // Some debug tools......
    //GStr := '*** **Bold** and **Some Bold Text** ** *** *';
    //GStr :=  ' ***BoldItalic** Italic* ***BoldItalic* Bold** ';
    GSTR := '< a pair of angles <> <, & && <sub>sub</sub>';
    debugln('BEFORE' + GStr);
    // ChangeTag('**', '<bold>', '</bold>');
    // ManageTriples();
    RemoveSelectedXMLChars();
    debugln('AFTER' + GStr);
    exit(false); *)

(*    while Index < Stl.Count do begin
        Stl.Strings[Index] := RemoveBadXMLCharacters(Stl.Strings[Index]);
        inc(Index);
    end; *)

    while Index < STL.Count do begin
        GStr := STL[Index];                         // GStr is a 'regional' var
        if RemoveSelectedXMLChars() then begin
            Stl.Insert(Index, GStr);
            Stl.Delete(Index+1);
        end;
        inc(Index);
	end;

    MarkUpMarkDown(Stl);
    ProcessPlain(Stl, RemoveBadXMLCharacters(Title));               // Hmm, what about " and '  ?
    STL.SaveToFile(AppendPathDelim(DestinationDir) + ID + '.note');
    Result := FileExistsUTF8(DestinationDir + ID + '.note');
    //Result := True;
end;


// -------------------  I N T E R N A L    M E T H O D S -------------------

function TImportNotes.ProcessPlain(Cont : TStringList; const Title : string) : boolean;
{var
    CrDate, LCDate : string;    }    // eg '2020-05-19T18:58:37.9513193+10:00';
begin
    { In tomboy-ng, we are reconstructing a note that came from NextCloud. That means
      we may be able to define its CrDate and LCDate, lets make them regional Vars.
      When we revert back to TT, if they are empty, got to make it up ourselves. }
    if CrDate = '' then
    {$ifdef TOMBOY_NG}
    CrDate := Sett.GetLocalTime();
    {$ELSE}
    CrDate := GetTBLocalTime();
    {$endif}
    if LCDate = '' then
        LCDate := CrDate;
    Cont.Insert(0, '	<text xml:space="preserve"><note-content version="0.1">' + Title);
    Cont.Insert(0, '	<title>' + Title + '</title>');
    Cont.Insert(0, '<note version="0.3" xmlns:link="http://beatniksoftware.com/tomboy/link" xmlns:size="http://beatniksoftware.com/tomboy/size" xmlns="http://beatniksoftware.com/tomboy">');
    Cont.Insert(0, '<?xml version="1.0" encoding="utf-8"?>');
    Cont.Add('    </note-content></text>');
    Cont.Add('      <last-change-date>' + LCDate + '</last-change-date>');
    Cont.Add('      <last-metadata-change-date>' + LCDate + '</last-metadata-change-date>');
    Cont.Add('      <create-date>' + CrDate + '</create-date>');
    Cont.Add('      <cursor-position>1</cursor-position>');
    Cont.Add('      <width>1000</width>');
    Cont.Add('      <height>626</height>');
    Cont.Add('    	<x>20</x>');
    Cont.Add('    	<y>30</y>');
    Cont.Add('      <tags>');
    if Notebook <> '' then
        Cont.Add('        <tag>system:notebook:' + RemoveBadXMLCharacters(Notebook) + '</tag>');
    Cont.Add('      </tags>');
    Cont.Add('    	<open-on-startup>False</open-on-startup>');
    Cont.Add('</note>');
    result := True;
end;

{     Markdown Rules

A line starting with a asterik and a space is a bullet.
Bold text is wrapped in ** at either end. Or __ (that is two underscores) at either end.
Italics is wrapped in * at either end. Again, the underscore at either end will work as well.
Strikeout is wrapped in ~~ at either end, note this is NOT official Common Mark, it does not mention strikeout
backticks, ` at either end will make code, use for monospace text.
Stikeout is not supported.
a line starting with ###space  is a  bold, large line
a line starting with ##space is a bold, huge line
we ignore #space, have other ways of finding title.

plain ***bold italic* bold** plain

plain ***bold italic*** **bold** plain

plain ***bold italic** italic* plain

`monospace with grey background`

We cannot express here, from NextCloud the following because NextCloud does not do them -
	    * Underline
	    * Highlight
	    * Small Font
}







{ Ugly way to deal with ugly situation, we may encounter *** (or ___) and then may get
  any combination of *, **, *** closing one or both markups. It cannot be dealt with
  in the ChangeTag() model because of unpredictable followup tag or tags.
  Should only be passed *** or ___ as the tag. Must be 3 char long.
  It returns the 1 based index of where it found, and replaced a triple tag. 0 indicates
  there are no more there to work on.  This only does LEADING TRIPLES !!!!
}

function TImportNotes.HandleTriple({var St : string; }Tag : string; StartAt : integer) : integer;
var
    TagStart, TagLength, OwedTagPoints : integer;

    procedure ReplaceTag(TStart, TLen : integer; NextTag : string);                   // using a 1 based index
    begin
        delete(GStr, TStart, TLen);
        insert(NextTag, GStr, TStart);
    end;

begin
    if GStr.Indexof(Tag, StartAt) = -1 then exit(-1);                       // good, no such markdown present
    TagLength := length(Tag);
    TagStart := GStr.Indexof(Tag, StartAt) + 1;                             // Remember, IndexOf returns a zero based index.
    Result := TagStart;                                                   // but we now use a 1 based index from here on
    if IsThisATag(True, Tag, TagStart) then begin
        ReplaceTag(TagStart, TagLength, '<italic><bold>');
        OwedTagPoints := 3;
        // Now we have to find either a trailing *, ** or ***.  First one we find wins.
        while TagStart < GStr.Length do begin
            inc(TagStart);
            if Gstr[TagStart] = tag[1] then begin
                if IsThisATag(False, Tag, TagStart) then begin
                    ReplaceTag(TagStart, 3, '</bold></italic>');
                    OwedTagPoints := 0;
                    break;
				end;
                // depending on the order they happen, the next two blocks need to respond differently.
                // OwedTagPoints may be 3, 2 or 1 at this stage.
                if IsThisATag(False, copy(Tag, 2, 2), TagStart) then begin      // This is about closing Bold
                    if OwedTagPoints > 1 then begin
                        ReplaceTag(TagStart, 2, '</bold>');
                        dec(OwedTagPoints, 2);
                        continue;
					end;
				end;
                if IsThisATag(False, tag[1], TagStart) then begin      // This is closing italic, but (3) bold may be on
                    case OwedTagPoints of
                        3 : begin ReplaceTag(TagStart, 1, '</bold></italic><bold>'); dec(OwedTagPoints); continue; end;
                        1 : begin ReplaceTag(TagStart, 1, '</italic>'); dec(OwedTagPoints); break; end;
					end;
				end;
			end;
		end;
        case OwedTagPoints of                                           // Must have been bad MD
            3 : GStr := GStr + '</bold></italic>';
            2 : GStr := GStr + '</bold>';
            1 : GStr := GStr + '</italic>';
		end;
	end;
end;

function TImportNotes.ManageTriples(): boolean;
var
    StartPos : integer = 0;
begin
    while StartPos > -1 do
        StartPos := HandleTriple('***', StartPos);
    StartPos := 0;
    while StartPos > -1 do
        StartPos := HandleTriple('___', StartPos);
    result := true;
end;

function TImportNotes.IsThisATag(const Leading : boolean; const Tag : string; const TStart : integer): boolean;      // using a 1 based index
var
  TLen : integer;
begin
    result := false;
    //debugln('ACTING on ' + copy(GStr, PosStart, 20));
    TLen := length(Tag);
    if TStart + TLen -1 > GStr.Length then exit(False);                     // not enough room for the tag
    if (TLen > 1) and (GStr[TStart] <> GStr[TStart+1]) then exit(False);    // First two char are not the same
    if (TLen > 2) and (GStr[TStart] <> GStr[TStart+2]) then exit(False);    // First three char are not the same
    if Leading then begin
        if (TStart <> 1) and (GStr[TStart-1] <> ' ') and                    // Must start with the line or have a space to left
                    (GStr[TStart-1] <> '>') then exit(false);               // We also allow a > before a leading tag

        if TStart + TLen = length(GStr) then exit(false);                  // Nothing after it. Thats not a tag.
        // ToDo : next line generates an exception sometimes, try
        if (GStr[TStart + TLen] = ' ') or                                   // A space after a leading tag, I don't think so.
            (GStr[TStart + TLen] = tag[1]) then exit(False);                // nor is another char thats in the tag.
        exit(True);
    end;
    // OK, so its Trailing, lets see ....
    if TStart = 1 then exit(False);                                         // cannot start at beginning of line
    if GStr[TStart-1] = ' ' then exit(False);                               // Nor have a have a leading space.
    if (TStart+TLen > length(GStr)) or (GStr[TStart+TLen] = ' ') or         // Ends at EOL or with a space
                    (GStr[TStart+TLen] = '<') then Result := true;          // A < is allowed, part of xml tag
end;

function TImportNotes.ChangeTag(const Tag, ChangeToLead, ChangeToTail :  string)  : boolean;
var
    PosStart, TLen : integer;
    Leading : boolean = True;
begin
    PosStart := 1;
    TLen := Tag.length;
    while PosStart <= GStr.length do begin
        if (GStr[PosStart] =  Tag[1]) and IsThisATag(Leading, Tag, PosStart) then begin
            delete(GStr, PosStart, TLen);
            if Leading then
                insert(ChangeToLead, GStr, PosStart)
            else insert(ChangeToTail, GStr, PosStart);
            Leading := not Leading;
		end;
        inc(PosStart);
	end;
    result := true;
end;

function IsHeadingUnderline(St : String; IsDouble : boolean ) : boolean;
var
    Ch : char;
    I : integer;
begin
    if St.length < 3 then exit(False);
    if IsDouble then Ch := '=' else ch := '-';
    for I := 1 to St.Length do
        if St[i] <> Ch then
            exit(False);
    result := True;
end;


function TImportNotes.NotSpecialTag(const Index : integer; out TagSize : integer) : boolean;
var
    T1 : string = '<sub>';      // If this list gets any longer, make an array.
    T2 : string = '</sub>';
begin
    if T1 = copy(GStr, Index, length(T1)) then begin
        TagSize := length(T1);
        exit(false);
	end;
    if T2 = copy(GStr, Index, length(T2)) then begin
        TagSize := length(T2);
        exit(false);
	end;
    TagSize := 0;
    result := true;
end;

            { Similar to method in SyncUtils but does not act on certain, predefined XML Tag.
              These tags need be passed through unchanged. Does <>& plus ' " if requested for
              use in, eg, Title ?  Operates on global GStr, probably faster than orig. }
function TImportNotes.RemoveSelectedXMLChars(DoQuotes : boolean = false) : boolean;
var
   Index : longint = 1;
   //Start : longint = 1;
   STagSize : integer;
begin
   Result := false;
   while Index <= length(GStr) do begin
   		if GStr[Index] = '<' then begin             // Only this one can start a special xml tag
            if NotSpecialTag(Index, STagSize) then begin
                delete(GStr, Index, 1);
                insert('&lt;', GStr, Index);
                inc(Index, 4);                      // inc by extra length
                result := True;
            end else
                inc(Index, STagSize+1);             // jump over special tag, don't mess with '>' at end
            continue;                               // continue if we found char, cannot be another one
        end;
   		if GStr[Index] = '>' then begin
            delete(GStr, Index, 1);
            insert('&gt;', GStr, Index);
            inc(Index, 4);
            result := True;
            continue;
        end;
		if GStr[Index] = '&' then begin
            delete(GStr, Index, 1);
            insert('&amp;', GStr, Index);
            inc(Index, 5);
            result := True;
            continue;
        end;
        if DoQuotes then begin
       		if GStr[Index] = '''' then begin
                delete(GStr, Index, 1);
                insert('&apos;', GStr, Index);
                inc(Index, 6);
                result := True;
                continue;
            end;
       		if GStr[Index] = '"' then begin
                delete(GStr, Index, 1);
                insert('&quot;', GStr, Index);
                inc(Index, 6);
                result := True;
                continue;
            end;
        end;
        inc(Index);
    end;
end;


function TImportNotes.MarkUpMarkDown(Cont : TStringList) : boolean;
{  <size:huge><bold>huge heading</bold></size:huge><size:small> }
var
    Index : integer = 0;
    //St : string;
    DropNewLine : boolean = True;
begin
    Result := True;
    while Index < Cont.Count do begin
        GStr := Cont.Strings[Index];
        if Index > 0 then begin                     // setext heading, text underlined by  ---- or ====
            if IsHeadingUnderline(GStr, True) then begin
                // mark the line above as heading, delete this line.
                Gstr := '<size:huge>' + Cont.Strings[Index-1] + '</size:large>';
                Cont.Delete(Index-1);
                Cont.Delete(Index-1);
                Cont.Insert(Index-1, GStr);
                GStr := Cont.Strings[Index];
            end else
                if IsHeadingUnderline(GStr, False) then begin
                    //debugln('LARGE:' + GStr);
                    GStr := '<size:large>' + Cont.Strings[Index-1] + '</size:large>';
                    Cont.Delete(Index-1);
                    Cont.Delete(Index-1);
                    Cont.Insert(Index-1, GStr);
                    GStr := Cont.Strings[Index];
                end;
		end;
        GStr := Cont.Strings[Index];
        if (GStr = '') then begin
            if DropNewLine then begin
                Cont.Delete(Index);
                DropNewLine := False;
                continue;
            end else DropNewLine := True;
        end else DropNewLine := True;
        ManageTriples();                          // does ONLY leading triple so far !
        if copy(GStr, 1, 4) = '### ' then begin       // ATX heading, 1 to 6 # and a space in front of text
            delete(GStr, 1, 4);
            GStr := '<size:large><bold>' + GStr + '</bold></size:large>'
        end else
            if copy(GStr, 1, 3) = '## ' then begin
                delete(GStr, 1, 3);
                GStr := '<size:huge><bold>' + GStr + '</bold></size:huge>'
            end else if copy(GStr, 1, 2) = '* ' then begin
                delete(GStr, 1, 2);
                GStr := '<list><list-item dir="ltr">' + GStr + '</list-item></list>';
            end;
        ChangeTag('**', '<bold>',      '</bold>');
        ChangeTag('__', '<bold>',      '</bold>');
        ChangeTag('*',  '<italic>',    '</italic>');
        ChangeTag('_',  '<italic>',    '</italic>');
        ChangeTag('`',  '<monospace>', '</monospace>');
        ChangeTag('~~', '<strikeout>', '</strikeout>');
        while ChangeSmallFont() do;
        //DebugLn('POST [' + GStr + ']');
        Cont.Strings[Index] := GStr;
        inc(Index);
    end;
end;

function TImportNotes.ChangeSmallFont()  : boolean;
var
    PosStart : integer;
begin
    // The MD we use looks like this <sub>small font</sub> but by time it gets here,
    // the angle brackets have been munged by a call to removebadxml().
    // This particular md does work in the CommonMark demo and on Github but not in NextCloud
    Result := False;
    PosStart := GStr.IndexOf('<sub>', 0);
    // PosStart := GStr.IndexOf('&lt;sub&gt;', 0);
        if (PosStart > -1) then begin
        inc(PosStart);                                              // IndexOf is zero based
        delete(GStr, PosStart, 5);
        insert('<size:small>', GStr, PosStart);
        PosStart := GStr.IndexOf('</sub>', PosStart);
        //PosStart := GStr.IndexOf('&lt;/sub&gt;', PosStart);         // and thats </sub>
        if (PosStart > -1) then begin
            inc(PosStart);                                          // convert 1 based from zero based
            delete(GStr, PosStart, 6);
            insert('</size:small>', GStr, PosStart);
        end else GStr := GStr + '</size:small>';                    // if its not closed (bad!) we just add at end of line.
        result := True;
    end else Result := false;                                       // false says we
end;



// ToDo : Monospaced becomes code, wrap with backtick. MD:  `Mono or code`  Tomboy:  <monospace>Mono or code</monospace>

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
            if Mode = 'markdown' then
                MarkUpMarkDown(Content);
            if FirstLineTitle then begin
                Title := Content.Strings[0];
                Content.Delete(0);
            end else Title := ExtractFileNameOnly(FullFileName);
            if copy(Title, 1, 2) = '# ' then delete(Title, 1, 2);
            if copy(Title, 1, 2) = '## ' then delete(Title, 1, 3);
            if copy(Title, 1, 2) = '### ' then delete(Title, 1, 4);
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




end.

// Legacy methods !!
  (*
function  TImportNotes.PosMDTag(const St, Tag : string; const leading : boolean) : integer;
{ we will be asked about the following symetrical tags -
        **Bold**   _bold_            `mono`
        *Italic*   _Italic_          ~~strikeout~~

        Leading either starts a line (char[1]) or has a space before it. Visible char immediatly to right.
        Trailing either has a space or eol immediatly after, visible char immediatly to left.

        Note that three * or _ starts a BoldItalis block, it can be terminated with one, two or three matching
        char and remaining markup continues on. Same at end ! Important if you want to start (or end) both bold
        and italics at the same spot. Yuck !

        Note that Visible char means any char excluding potential markup, ascii, unicode etc. Cannt tell what, will
        have to just test what it is not, ie space, beginning or end of line.
        FontSize HiLite Ital Bold Bullet TEXT BulletOff BoldOff ItalOff HiLiteOff FontSize
}
var
    Stage : integer = 0;
begin
    {  Tag = '**'
       Leading ->  [sol]**Text   or .. **Text
       Trailing ->   ..Text** ...   or   ...Text**[eol]  }
    if Leading then begin
        while Stage < length(St) do begin
            Result :=  St.IndexOf(Tag, Stage);                          // zero based !
            if Result = -1 then exit;                                   // no more candidates
            if (Result = 0) or (St[Result] = ' ') then
            //if (Result = 0) or (St[Result] in [' '..'/']) then          // start of line or has space before tag
                if St[Result+length(Tag)+1] in  ['A'..'z', '0'..'9'] then
                    exit(Result+1);
            Stage := Result+1;
        end;
    end else begin
        debugln('St=[' + St + '] Stage=' + inttostr(Stage) + ' Leading=' + booltostr(Leading, true));
        while Stage < length(St) do begin
            Result :=  St.IndexOf(Tag, Stage);  // zero based !
            if Result = -1 then exit;
            debugln('Testing, R=' + inttostr(Result) + ' L=' + inttostr(Length(St)) + ' ' + Tag);
            //if ((Result+length(tag)) >= Length(St)) or (St[Result+length(Tag)+1] in [' '..'/']) then begin   // at end of line or a space after
            if ((Result+length(tag)) >= Length(St)) or (St[Result+length(Tag)+1] = ' ') then begin   // at end of line or a space after

                debugln('Tickle ! Sum=' + inttostr(Result+length(Tag)+1));

                if St[Result] in  ['A'..'z', '0'..'9'] then                       // thats before tag

                    exit(Result+1);
			end;
			Stage := Result+1;
        end;
    end;
    Result := -1;
end; *)
(*
function TImportNotes.ChangeBold(var St : string)  : boolean;    // ToDo - this is not being used.
var
    PosStart, PosEnd : integer;
begin
    // like this **bold** or __bold__
    Result := False;
    PosStart := PosMDTag(St, '**', True);
    if (PosStart > 0) then begin
        PosEnd := PosMDTag(St, '**', False);
        if (PosEnd > 0) then begin
            delete(St, PosEnd, 2);
            insert('</bold>', St, PosEnd);
            delete(St, PosStart, 2);
            insert('<bold>', St, PosStart);
            result := True;
        end;
    end;
    PosStart := PosMDTag(St, '__', True);
    if (PosStart > 0) then begin
        PosEnd := PosMDTag(St, '__', False);
        if (PosEnd > 0) then begin
            delete(St, PosEnd, 2);
            insert('</bold>', St, PosEnd);
            delete(St, PosStart, 2);
            insert('<bold>', St, PosStart);
            result := True;
        end;
    end;
end; *)

(*
function TImportNotes.ChangeItalic(var St : string)  : boolean;     // ToDo - this is not being used.
var
    PosStart, PosEnd : integer;
begin
    // like this *italic* or, confusingly, might be like _this_
    Result := False;
    PosStart := PosMDTag(St, '*', True);
    if (PosStart > 0) then begin
        PosEnd := PosMDTag(St, '*', False);
        if (PosEnd > 0) then begin
            delete(St, PosEnd, 1);
            insert('</italic>', St, PosEnd);
            delete(St, PosStart, 1);
            insert('<italic>', St, PosStart);
            result := True;
        end;
    end;
    PosStart := PosMDTag(St, '_', True);
    if (PosStart > 0) then begin
        PosEnd := PosMDTag(St, '_', False);
        if (PosEnd > 0) then begin
            delete(St, PosEnd, 1);
            insert('</italic>', St, PosEnd);
            delete(St, PosStart, 1);
            insert('<italic>', St, PosStart);
            result := True;
        end;
    end;
end;  *)

(*                        // This is not useful for an NextCloud import
function TImportNotes.ChangeSmallFont(var St : string)  : boolean;
var
    PosStart, PosEnd : integer;
begin
    // MD looks like this <sub>small font</sub> but by time we get here, the angle brackets have been munged.
    Result := False;
    PosStart := PosMDTag(St, '&lt;sub&gt;', True);              // thats <sub>
    if (PosStart > 0) then begin
        PosEnd := PosMDTag(St, '&lt;/sub&gt;', False);          // and thats </sub>
        if (PosEnd > 0) then begin
            delete(St, PosEnd, 12);
            insert('</size:small>', St, PosEnd);
            delete(St, PosStart, 11);
            insert('<size:small>', St, PosStart);
            result := True;
        end;
    end;
end; *)

