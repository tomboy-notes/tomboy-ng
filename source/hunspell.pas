{$MODE objfpc}{$H+}
unit hunspell;

{	Hunspell interface.
	Based on code that seems to appear in lots of places in the Lazarus Forum
    and elsewhere.

    With additions and corrections by dbannon to make it a little easier to use.

    As such, its assumed to be free to use by anyone for any purpose.
}

{	A Unit to connect to the hunspell library and check some spelling.
	First, create the class, it will try and find a library to load.
    Check ErrorMessage.
    Then call SetDictionary(), with a full filename of the dictionary to use.
    If GoodToGo is true, you can call Spell() and Suggests()
    otherwise, look in ErrorString for what went wrong.

    Look in FindLibrary() for default locations of Library.
    2018/10/31  Changed to TLibHandle to accomadate Mac 64bit
    2018/11/01  Added /usr/local/Cellar/hunspell/1.6.2/lib/ as place to look
                for hunspell library on Mac. Need to make that more flexible.
    2018/11/29  Better debug messages
}


interface
uses Classes, dynlibs;


type
  THunspell_create = function(aff_file: PChar; dict_file: PChar): Pointer; cdecl;
  THunspell_destroy = procedure(spell: Pointer); cdecl;
  THunspell_spell = function(spell: Pointer; word: PChar): Boolean; cdecl;
  THunspell_suggest = function(spell: Pointer; out slst: PPChar; word: PChar): Integer; cdecl;
  THunspell_analyze = function(spell: Pointer; var slst: PPChar; word: PChar): Integer; cdecl;
  THunspell_stem = function(spell: Pointer; var slst: PPChar; word: PChar): Integer; cdecl;
  THunspell_free_list = procedure(spell: Pointer; var slst: PPChar; n: integer); cdecl;
  THunspell_get_dic_encoding = function(spell: Pointer): PChar; cdecl;
  THunspell_add = function(spell: Pointer; word: PChar): Integer; cdecl;
  THunspell_remove = function(spell: Pointer; word: PChar): Integer; cdecl;

   { THunspell }

  THunspell = class
  private
    Speller: Pointer;
        { Loads indicated library, returns False and sets ErrorMessage if something wrong }
    function LoadHunspellLibrary(LibraryName: AnsiString): Boolean;
  public
    	    { set to True if speller is ready to accept requests }
    GoodToGo : boolean;
    	    { empty if OK, contains an error message if something goes wrong }
    ErrorMessage : ANSIString;
            { Will have a full name to library if correctly loaded at create }
    LibraryFullName : string;
            { if set t, typically by caller, prints a lot of whats happening }
    DebugMode : boolean;
            { Will have a "first guess" as to where dictionaries are, poke another name in
            and call FindDictionary() if default did not work }
    constructor Create(const Debug : boolean; const FullLibName : ANSIString = '');
    destructor Destroy; override;
            { Returns True if word spelt correctly }
    function Spell(Word: string): boolean;
            { Returns with List full of suggestions how to spell Word }
    procedure Suggest(Word: string; List: TStrings);
            { untested }
    procedure Add(Word: string);
            { untested }
    procedure Remove(Word: string);
            { returns a full library name or '' if it cannot find anything suitable }
    function FindLibrary(out FullName : AnsiString) : boolean;
            { returns true if it successfully set the indicated dictionary }
    function SetDictionary(const FullDictName: string) : boolean;
    function SetNewLibrary(const LibName : string) : boolean;
  end;

var Hunspell_create: THunspell_create;
var Hunspell_destroy: THunspell_destroy;
var Hunspell_spell: Thunspell_spell;
var Hunspell_suggest: Thunspell_suggest;
var Hunspell_analyze: Thunspell_analyze;
var Hunspell_stem: Thunspell_stem;
var Hunspell_get_dic_encoding: Thunspell_get_dic_encoding;
var Hunspell_add: THunspell_add;
var Hunspell_free_list: THunspell_free_list;
var Hunspell_remove: THunspell_remove;

var HunLibLoaded: Boolean = False;
var HunLibHandle: {THandle;} TLibHandle;     // 64bit requires use of TLibHandle
    // see https://forum.lazarus.freepascal.org/index.php/topic,34352.msg225157.html

implementation

uses LazUTF8, SysUtils, {$ifdef linux}Process,{$endif} LazFileUtils, Forms, lazlogger;
// Forms needed so we can call Application.~
// lazlogger for the debug lines.

{ THunspell }

function THunspell.LoadHunspellLibrary(libraryName: Ansistring): Boolean;
begin
    Result := false;
    HunLibHandle := LoadLibrary(PAnsiChar(libraryName));
    if HunLibHandle = NilHandle then begin
        if Debugmode then debugln('Failed to load library ' + libraryName);
        ErrorMessage := 'Failed to load library ' + libraryName;
    end else begin
        Result := True;
        Hunspell_create := THunspell_create(GetProcAddress(HunLibHandle, 'Hunspell_create'));
        if not Assigned(Hunspell_create) then Result := False; 
    	Hunspell_destroy := Thunspell_destroy(GetProcAddress(HunLibHandle, 'Hunspell_destroy'));
        if not Assigned(Hunspell_destroy) then Result := False;
        Hunspell_spell := THunspell_spell(GetProcAddress(HunLibHandle, 'Hunspell_spell'));
        if not Assigned(Hunspell_spell) then Result := False;
        Hunspell_suggest := THunspell_suggest(GetProcAddress(HunLibHandle, 'Hunspell_suggest'));
        if not Assigned(Hunspell_suggest) then Result := False;
        Hunspell_analyze := THunspell_analyze(GetProcAddress(HunLibHandle, 'Hunspell_analyze'));  // not used here
        if not Assigned(Hunspell_analyze) then Result := False;
        Hunspell_stem := THunspell_stem(GetProcAddress(HunLibHandle, 'Hunspell_stem'));           // not used here
        if not Assigned(Hunspell_stem) then Result := False;
        Hunspell_get_dic_encoding := THunspell_get_dic_encoding(GetProcAddress(HunLibHandle, 'Hunspell_get_dic_encoding'));   // not used here
        if not Assigned(Hunspell_get_dic_encoding) then Result := False;
        Hunspell_free_list := THunspell_free_list(GetProcAddress(HunLibHandle, 'Hunspell_free_list'));
        if not Assigned(Hunspell_free_list) then Result := False;
        Hunspell_add := THunspell_add(GetProcAddress(HunLibHandle, 'Hunspell_add'));
        if not Assigned(Hunspell_add) then Result := False;
        Hunspell_remove := THunspell_remove(GetProcAddress(HunLibHandle, 'Hunspell_remove'));
        if not Assigned(Hunspell_remove) then Result := False;
        HunLibLoaded := Result;
    end;
    if ErrorMessage = '' then
        if not Result then begin
            ErrorMessage := 'Failed to find functions in ' + LibraryName;
            if debugmode then debugln('Hunspell Failed to find functions in ' + LibraryName);
        end;
    if Result and debugmode then  debugln('Loaded library OK ' + LibraryName);
end;

constructor THunspell.Create(const Debug : boolean; const FullLibName : ANSIString = '');
begin
    DebugMode := Debug;
    ErrorMessage := '';
    LibraryFullName := FullLibName;
    if LibraryFullName = '' then
        if Not FindLibrary(LibraryFullName) then begin
            if debugmode then debugln('Cannot find Hunspell library');
            ErrorMessage := 'Cannot find Hunspell library';
            exit();
        end;
    if debugmode then debugln('Creating Hunspell with library = ' + LibraryFullName);
    LoadHunspellLibrary(LibraryFullName);    // will flag any errors it finds
    Speller := nil;           // we are not GoodToGo yet, need a dictionary ....
end;

destructor THunspell.Destroy;
begin
    if DebugMode then debugln('About to destry Hunspell');
    if (HunLibHandle <> 0) and HunLibLoaded then begin
        if Speller<>nil then hunspell_destroy(Speller);
        Speller:=nil;
        if HunLibHandle <> 0 then FreeLibrary(HunLibHandle);
        HunLibLoaded := false;
    end;
    inherited Destroy;
end;

function THunspell.Spell(Word: string): boolean;
begin
    Result := hunspell_spell(Speller, PChar(Word))
end;

procedure THunspell.Suggest(Word: string; List: TStrings);
var i, len: Integer;
	SugList, Words: PPChar;
    //Blar : AnsiString;
begin
    List.clear;
    try
        len := hunspell_suggest(Speller, SugList, PChar(Word));
        Words := SugList;
        for i := 1 to len do begin
            List.Add(Words^);
            //Blar := Words^;
            Inc(PtrInt(Words), sizeOf(Pointer));
        end;
    finally
        Hunspell_free_list(Speller, SugList, len);
    end;
end;

procedure THunspell.Add(Word: string);
begin
    Hunspell_add(Speller, Pchar(Word));
end;

procedure THunspell.Remove(Word: string);
begin
    Hunspell_remove(Speller, Pchar(Word));
end;

function THunspell.FindLibrary(out FullName : ANSIString):boolean;
var
    {$ifdef LINUX} I : integer = 1; {$endif}
    {$ifndef LINUX}
    Info : TSearchRec;
    Mask : ANSIString;
    {$endif}
begin
    Result := False;
    {$IFDEF LINUX}
    // Assumes ldconfig always returns same format, better than searching several dirs
    if RunCommand('/bin/bash',['-c','ldconfig -p | grep hunspell'], FullName) then begin
        while UTF8Pos(' ', FullName, I) <> 0 do inc(I);
        if I=1 then exit();
        UTF8Delete(FullName, 1, I-1);
        UTF8Delete(FullName, UTF8Pos(#10, FullName, 1), 1);
        Result := True;
    end else
        if RunCommand('/bin/bash',['-c','/sbin/ldconfig -p | grep hunspell'], FullName) then begin
            while UTF8Pos(' ', FullName, I) <> 0 do inc(I);
            if I=1 then exit();
            UTF8Delete(FullName, 1, I-1);
            UTF8Delete(FullName, UTF8Pos(#10, FullName, 1), 1);
            Result := True;
        end;
    {$ENDIF}
    {$ifdef DARWIN}
    Mask := 'libhunspell*';
    FullName := '/usr/local/Cellar/hunspell/1.6.2/lib/';
    //     /usr/local/Cellar/hunspell/1.6.2/lib/libhunspell-1.6.0.dylib
    if FindFirst(FullName + Mask, faAnyFile and faDirectory, Info)=0 then begin
        FullName := FullName + Info.name;
        Result := True;
    end;
    if not result then begin
        FullName := '/usr/lib/';
        if FindFirst(FullName + Mask, faAnyFile and faDirectory, Info)=0 then begin
            FullName := FullName + Info.name;
            Result := True;
        end;
    end;
    FindClose(Info);
    {$endif}
    {$ifdef WINDOWS}
    // Now, only Windows left. Look for a dll in application home dir.
    Mask := '*hunspell*.dll';
    FullName := ExtractFilePath(Application.ExeName);
    if FindFirst(FullName + Mask, faAnyFile and faDirectory, Info)=0 then begin
        FullName := FullName + Info.name;
        Result := True;
    end;
    FindClose(Info);
    {$endif}
    if Result then begin
        if DebugMode then debugln('FindLibrary looks promising [', FullName, ']');
    end else
        if DebugMode then debugln('FindLibrary Failed to find a Hunspell Library', FullName, ']');
end;

function THunspell.SetDictionary(const FullDictName: string) : boolean;
var
    FullAff : string;
begin
    if debugmode then debugln('about to try to set dictionary');
    Result := False;
    if not FileExistsUTF8(FullDictName) then exit();
    FullAff := FullDictName;
    UTF8Delete(FullAff, UTF8Length(FullAff) - 2, 3);
    FullAff := FullAff + 'aff';
    if not FileExistsutf8(FullAFF) then exit();
    {
    I have seen an access violaton occasionally in the next l
    reproducable. I think it happens when a new startup and,
    ShowLinks on and then go to Spell tab and choose a dictio
    OK, Nov 2018, think I got it ....
    }
    try
        if assigned(Speller) then begin
                hunspell_destroy(Speller);
                if debugmode then debugln('Speller destroyed');
        end;
        Speller := hunspell_create(PChar(FullAff), PChar(FullDictName));
                        // Create does not test the dictionaries !
    except
        on E: Exception do debugln('Hunspell ' + E.Message);
    else
        debugln('Hunspell has lost it !');
    end;
    Result := false;
    GoodToGo := assigned(Speller);
    if not GoodToGo then
        ErrorMessage := 'Failed to set Dictionary ' + FullDictName;
    Result := GoodToGo;
end;

function THunspell.SetNewLibrary(const LibName: string): boolean;
begin
    LibraryFullName := LibName;
    Result := LoadHunspellLibrary(LibraryFullName);
end;

end.
