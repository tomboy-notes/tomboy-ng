program project1;

{$mode objfpc}{$H+}

{   A test program that puts notenormal through so tests. Input file as defined
    below contains typical stuff from a block by block read of KMemo as xml but
    not pretty. Many tomboy-ng processes require xml to be presented with both tags
    on same line as content, no cross over etc.

    Any problem normalising or convertibg to XML causes an exception and crash.

    This only really checks if its valid XML, it could still be not what we
    expect to look over it carefully.
}

uses
    {$IFDEF UNIX}
    cthreads,
    {$ENDIF}
    Classes, sysutils,
    notenormal,
    laz2_DOM, laz2_XMLRead;

const
    TestFile = 'notenormal-test.txt';      // input file to exercise notenormal
    XMLFile  = 'xmlfile.xml';              // file we generate containing (hopefully) good xml


function ReadXML(xmlfile : string) : boolean;
var
    VersionSt : string;
    Doc : TXMLDocument;
    //NodeList : TDOMNodeList;
    //Node : TDOMNode;
    //j : integer;
begin
    ReadXMLFile(Doc, xmlfile);
    VersionSt := Doc.DocumentElement.GetAttribute('version');
    writeln('Version=', VersionSt);
    //RemoteServerRev := strtoint(Doc.DocumentElement.GetAttribute('revision'));
    Doc.Free;
    result := true;
end;


function Normalise(FN : String) : boolean;
var
    SL : TStringList;
    Normal : TNoteNormaliser;
begin
    SL := TStringList.Create;
       SL.LoadFromFile(FN);
       Normal := TNoteNormaliser.Create;
       Normal.NormaliseList(SL);
       // write(SL.Text);
       writeln(SL.Count, ' lines');      // expect 42 lines with existing input file
       // I expect it to have same number of lines as input file.
       Normal.Free;
       SL.Add('</note-content></text></note>');      // in tomboy-ng, this is done elsewhere
       Sl.SaveToFile('xmlfile.xml');                 // disk

 writeln(SL.Text);    // console

       Sl.Free;
       result := true;
end;

begin
    if FileExists(XMLFile) then DeleteFile(XMLFile);   // so no mistakes !
    Normalise('notenormal-test.txt');
    if ReadXML('xmlfile.xml') then writeln('OK, that went well !');
end.

