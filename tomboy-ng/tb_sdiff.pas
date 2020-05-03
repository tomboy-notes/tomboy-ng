unit TB_SDiff;
{
    A unit that can display differences between two similar notes.
      *  Copyright (C) 2018 David Bannon
      *  See attached licence file.


    User can choose to DoNothing, use First (Remote) or Second (Local)

    // Use Remote, Yellow is mrYes, File1
    // Use Local, Aqua is mrNo, File2
    // Always Use Local is mrNoToAll
    // Always Use Remote is mrYesToAll
    // Always use newest mrAll
    // Always use oldest mrClose
    // Anything else is DoNothing - no, do not permit donothing
}

{ History
    2018/08/14  Added to project
    2018/09/17  Changes to work with new sync model. We now just use the two
                file names in TClashRec and we get the last-change-dates our
                selves. Should be compatible with old sync model ....
    2018/10/16  Options to apply choice to all notes.
    2019/10/17  Trap exception if, for some reason, we cannot load one of the note files.
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ExtCtrls, ComCtrls, Buttons, kmemo;
type

    { TFormSDiff }

    TFormSDiff = class(TForm)
        BitBtnUseRemote: TBitBtn;
        BitBtnUseLocal: TBitBtn;
        ButtAllOldest: TButton;
        ButtAllNewest: TButton;
        ButtAllLocal: TButton;
        ButtAllRemote: TButton;
        Button1: TButton;
        MemoRemote: TMemo;
        MemoLocal: TMemo;
        TitleLocal: TLabel;
        ChangeLocal: TLabel;
        TitleRemote: TLabel;
        ChangeRemote: TLabel;
        Label1: TLabel;
        NoteID: TLabel;
        function RemoveXml(const St: AnsiString): AnsiString;

    end;

implementation

{$R *.lfm}

uses LazLogger, laz2_DOM, laz2_XMLRead, LazFileUtils, DateUtils{, syncutils};

{ TFormSDiff }

function TFormSDiff.RemoveXml(const St : AnsiString) : AnsiString;
var
    X, Y : integer;
    FoundOne : boolean = false;
begin
    Result := St;
    repeat
        FoundOne := False;
        X := Pos('<', Result);      // don't use UTF8Pos for byte operations
        if X > 0 then begin
            Y := Pos('>', Result);
            if Y > 0 then begin
                Delete(Result, X, Y-X+1);
                FoundOne := True;
            end;
        end;
    until not FoundOne;
    Result := trim(Result);
end;


end.

