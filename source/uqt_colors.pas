unit uQt_Colors;
{   Copyright (C) 2023 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------

    This is a unit, only invoked if using the Qt5 widget set (some mods needed for Qt6)
    that will alter how the app manages colours if (and only IF) the user has the
    QT_QPA_PLATFORMTHEME=qt5ct in the env. It does NOT respect the -platformtheme switch
    because one dash means one char switch in POSIX, so, we don't consider
    -platformtheme qt5ct is a switch. (TApplication does make the commandline
    available Davo ....)
    This unit reads the colours that qt5ct wants us to use and passes a subset
    of them back for KMemo to use.

    Note : Unix only, makes some assumptions about paths, easy fix ....

    History :
    2023-03-13 Initial release.
}
{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, graphics;

type TQt_Colors_Rec = record
    QColorLink : TColor;
    QColorBright : TColor;
    QColorLessBright : TColor;
    QColorBackground : TColor;
    QColorAltBackGround : TColor;
    QColorText : TColor;
    QColorHighLight : Tcolor;
    end;

{ TQt_Colors }
type
 TQt_Colors = class
  private
    function GetActiveColors : string;
    function GetIndexedColor(CSt : string; const Index : integer) : TColor;

  public
    FoundColors : boolean;
    constructor Create();
end;

var
    Qt_Colors_Rec : TQt_Colors_Rec;

implementation

uses IniFiles, Forms;

const
    CONF = '/.config/qt5ct/qt5ct.conf';     // the conf file has an entry that points to the selected color_scheme

{ TQt_Colors }

function TQt_Colors.GetActiveColors: string;
var
   ConfigFile : TINIFile;
begin
    result := GetEnvironmentVariable('HOME') + CONF;
    if FileExists(result) then begin
        ConfigFile :=  TINIFile.Create(result);
        try
            result := ConfigFile.readstring('Appearance', 'color_scheme_path', '');
        finally
            ConfigFile.free;
        end;
    end;
    if Result = '' then exit;
    if FileExists(Result) then begin
        ConfigFile :=  TINIFile.Create(Result);
        try
            result := ConfigFile.readstring('ColorScheme', 'active_colors', '');
        finally
            ConfigFile.free;
        end;
    end;
end;

function TQt_Colors.GetIndexedColor(CSt: string; const Index: integer): TColor;
var
    StL : TStringList;
    St : String;
    CValue : Cardinal;
    R, G, B : byte;                 // thats the order in Qt's view
begin
    StL := TStringList.Create;      // probably more efficent to use CSt.Split .....
    StL.Delimiter := ' ';
    Stl.DelimitedText := CSt;
    St := StL[Index];
    Stl.Free;
    if St.length < 1 then exit(clBlack);       // ToDo : That is an uncaught error
    St[1] := '$';
    St := St.Replace(',', '');
    CValue := strtoInt(St) and $ffffff;
    B := CValue and $ff;
    CValue := CValue shr 8;
    G := CValue and $ff;
    CValue := CValue shr 8;
    R := CValue and $ff;
    result := TColor((B shl 16) + (G shl 8) + R);   // Thats TColor order.
end;

constructor TQt_Colors.Create();
var
    St : String;
begin
    if (GetEnvironmentVariable('QT_QPA_PLATFORMTHEME') <> 'qt5ct') then exit;   // Note : not responding to -platformtheme switch
    St := GetActiveColors();
    if St = '' then exit;
    Qt_Colors_Rec.QColorText  := GetIndexedColor(St, 0);               // 0 is Text - used for usual text
    Qt_Colors_Rec.QColorBright := GetIndexedColor(St, 2);              // 2 is Bright
    Qt_Colors_Rec.QColorLessBright := GetIndexedColor(St, 3);          // 3 is LessBright
    Qt_Colors_Rec.QColorBackground  := GetIndexedColor(St, 9);         // 9 is background
    Qt_Colors_Rec.QColorLink  := GetIndexedColor(St, 14);              // 14 is Link index 0..19
    Qt_Colors_Rec.QColorHighLight  := GetIndexedColor(St, 12);
    Qt_Colors_rec.QColorAltBackGround := GetIndexedColor(St, 16);
    FoundColors := True;
    //writeln('We have found suitable qt5ct colors');
end;

end.

{
WindowText = 0;
NormalBackGround = 1;
Link = 14;

if we have a qt env setting, QT_QPA_PLATFORMTHEME=qt5ct we can
look in /$HOME/.config/qt5ct/qt5ct.conf  Its an ini file that we will find, in
section [Appearance], color_scheme_path, value being eg /usr/share/qt5ct/colors/darker.conf

We open this file, also ini, the value of [ColorScheme], has a line that looks like this -

active_colors=#ffffff, #424245, #979797, #5e5c5b, #302f2e, #4a4947, #ffffff, #ffffff, #ffffff, #3d3d3d, #222020, #e7e4e0, #12608a, #f9f9f9, #0986d3, #a70b06, #5c5b5a, #ffffff, #3f3f36, #ffffff

each subvalue may be 6 or 8 hex digits, we loose the first two if its 8.

White = #FFFFFF (careful, apparently Qt can be funny about ffffff)
Six Digits, Red, Green, Blue, 2 each. Higher value means lighter.
Black is  #222222 - I guess it gets blacker than this but not in the themes I see.

Red   = #ff0000
Green = #00FF00
Blue  = #0000FF

Digits to left of 6 rightmost characters are Opacity. Strip them off.

TColor -   Blue-Green-Red
=========
clBlue    = TColor($FF0000);
clRed     = TColor($0000FF);
clGreen   = TColor($008000);
clBlack   = TColor($000000);
clWhite   = TColor($FFFFFF);

}
