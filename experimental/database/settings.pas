unit settings;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Graphics;

type
    { TSett }
    TSett = class(Tobject)

        constructor Create();
    public
      FontSmall  : Integer;
      FontLarge  : Integer;
      FontHuge   : Integer;
      FontTitle  : Integer;                   // Dont set this to one of the other sizes !
      FontNormal : Integer;
      HiColor    : Integer;
end;

var
    Sett : TSett;

implementation

const HiColor = clYellow;

constructor TSett.Create();
begin
  FontSmall  :=  8;
  FontLarge  := 14;
  FontHuge   := 18;
  FontTitle  := 16;                   // Dont set this to one of the other sizes !
  FontNormal := 11;

end;

end.

