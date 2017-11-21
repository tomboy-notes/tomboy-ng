unit SaveNote;

{
 * Copyright (C) 2017 David Bannon
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}

{  This unit is responsible for saving a note in the Tomboy XML format.
    After creation, the class needs to be told the current FontNormal
	size and the CreatDate if any. If the supplied CreatDate is '', it will
	stamp it Now().
    All the work is done in the Save(..) function, it needs to be passed
	the name of a file (that may or may not exist) and the RichMemo its
	getting its content from.
}

{	HISTORY
	20170927 - added Hyperlink to blocks to be saved.
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, KMemo, Graphics;

type

    { TBSaveNote }

 TBSaveNote = class

       private
            FSize : integer;
			Bold : boolean;
			Italics : boolean;
			HiLight : boolean;
			InList : boolean;
            KM : TKMemo;
            function AddTag(const FT : TFont; var Buff : ANSIString) : ANSIString;
            function SetFontXML(Size : integer; TurnOn : boolean) : string;
          	function Header() : ANSIstring;
         	Function Footer() : ANSIstring;
            function GetLocalTime():ANSIstring;
       public
            Title : ANSIString;
      //      FontNormal : integer;		// Needs to be set after class created (could get from settings??)
            CreateDate : ANSIString;
         	procedure Save(FileName : ANSIString; KM1 : TKMemo);
    end;


implementation

uses FileUtil               // Graphics needed for font style defines
    ,Settings				// User settings and some defines across units.
    {$ifdef LINUX}, Unix {$endif} ;              // We call a ReReadLocalTime()


{ TODO 1 : If user has altered the title and then closes the note (or manually saves)
we need to tell the search screen to reload the notes list. Or, in case a large list,
should we just update that record ? }

{ TODO 1 : Must make the saving module capable of recognising and acting on bullets. <list><list-item dir="ltr">bullet 1
</list-item><list-item dir="ltr">
</list-item><list-item dir="ltr">Empty Bullet above.
</list-item><list-item dir="ltr">bullet 3</list-item></list> }

{ function TBSaveNote.FPisEqual(const FP, FPRef : TFontParams) : boolean;
begin
  Result := false;
  if FP.name <> FPRef.name then exit();
  if FP.size <> FPRef.size then exit();
  if FP.bkColor <> FPRef.bkcolor then exit();
  if FP.style <> FPRef.style then exit();
  if FP.HasBkClr <> FPRef.HasBkClr then exit();
  Result := true
end;  }

{ function TBSaveNote.GetStyleRange(const Index : longint; out Start, Len : longint) : boolean;
var
	FP, FPr :  TFontParams;
    ParaMetric: TParaMetric;
    Indent : integer;
begin
  	Result := false;
    Start := Index;
    if not RichMemo.GetTextAttributes(Index, FPr) then exit();
    if not RichMemo.GetParaMetric(Index, ParaMetric) then exit();
    Indent := round(ParaMetric.HeadIndent);
    repeat
        	dec(Start);
        	if not RichMemo.GetTextAttributes(Start, FP) then break;
            if not RichMemo.GetParaMetric(Start, ParaMetric) then break;
    until ((not FPisEqual(FP, FPr)) or (Indent <> round(ParaMetric.HeadIndent)));
    inc(Start);
    Len := 0;
    repeat
    	inc(Len);
        if not RichMemo.GetTextAttributes(Start + Len, FP) then break;
        if not RichMemo.GetParaMetric(Start + Len, ParaMetric) then break;
    until ((not FPisEqual(FP, FPr)) or (Indent <> round(ParaMetric.HeadIndent)));
    Result := True;
end;   }


function TBSaveNote.SetFontXML(Size : integer; TurnOn : boolean) : string;
begin
	Result := '';
    case Size of
    	FontHuge : if TurnOn then Result  := '<size:huge>' else Result  := '</size:huge>';
        FontLarge : if TurnOn then Result  := '<size:large>' else Result  := '</size:large>';
        FontSmall :  if TurnOn then Result  := '<size:small>' else Result  := '</size:small>';
    end;
end;


function TBSaveNote.AddTag(const FT : TFont; var Buff : ANSIString) : ANSIString;
begin
    // Important that we keep the tag order consistent. Good xml requires no cross over
    // tags. If the note is to be readable by Tomboy, must comply. (RTomboy does not care)
    // FontSize, HiLite, Ital, Bold, Bullet, BulletOff, BoldOff, ItalOff, HiLiteOff

    // When Bullets turn off
 {   if (InList and (PM.HeadIndent < IsBullet)) then begin
        if FSize <> FontNormal then
            Buff := Buff + SetFontXML(FSize, false);
        if Hilight then Buff := Buff + '</highlight>';
        if Italics then Buff := Buff + '</italic>';
        if Bold then Buff := Buff + '</bold>';
        if ItemOnly then Buff := Buff + '</list-item>'
        else Buff := Buff + '</list-item></list>';
        if Bold then Buff := Buff + '<bold>';
        if Italics then Buff := Buff + '<italic>';
        if Hilight then Buff := Buff + '<highlight>';
        if FSize <> FontNormal then
            Buff := Buff + SetFontXML(FSize, true);
        InList := False;
    end;                                    }
    // When Bold Turns OFF
    if (Bold and (not (fsBold in FT.Style))) then begin
        if FSize <> FontNormal then
            Buff := Buff + SetFontXML(FSize, false);
        if Hilight then Buff := Buff + '</highlight>';
        if Italics then Buff := Buff + '</italic>';
        Buff := Buff + '</bold>';
        if Italics then Buff := Buff + '<italic>';
        if Hilight then Buff := Buff + '<highlight>';
        if FSize <> FontNormal then
            Buff := Buff + SetFontXML(FSize, true);
        Bold := false;
    end;
    // When Italic turns OFF
    if (Italics and (not (fsItalic in FT.Style))) then begin
        if FSize <> FontNormal then
             Buff := Buff + SetFontXML(FSize, false);
        if Hilight then Buff := Buff + '</highlight>';
     	Buff := Buff + '</italic>';
        if Hilight then Buff := Buff + '<highlight>';
        if FSize <> FontNormal then
            Buff := Buff + SetFontXML(FSize, true);
        Italics := false;
    end;
    // When Highlight turns OFF
    if (HiLight and (not (FT.Color = HiColor))) then begin
        if FSize <> FontNormal then
            Buff := Buff + SetFontXML(FSize, false);
        Buff := Buff + '</highlight>';
        if FSize <> FontNormal then
            Buff := Buff + SetFontXML(FSize, true);
        HiLight := false;
    end;
    // When Font size changes
    if FSize <> FT.Size then begin
        Buff := Buff + SetFontXML(FSize, false);
        FSize := FT.Size;
        Buff := Buff + SetFontXML(FSize, true);
    end;
    // Highlight turns ON
    if ((not HiLight) and (FT.Color = HiColor)) then begin
        if FSize <> FontNormal then
             Buff := Buff + SetFontXML(FSize, false);
        Buff := Buff + '<highlight>';
        if FSize <> FontNormal then
             Buff := Buff + SetFontXML(FSize, true);
        HiLight := true;
    end;
    // Italic turns On
    if ((not Italics) and (fsItalic in FT.Style)) then begin
        if Hilight then Buff := Buff + '</highlight>';
        if FSize <> FontNormal then
        	Buff := Buff + SetFontXML(FSize, false);
        Buff := Buff + '<italic>';
        if FSize <> FontNormal then
             Buff := Buff + SetFontXML(FSize, true);
        if Hilight then Buff := Buff + '<highlight>';
        Italics := true;
    end;
    // Bold turns On
    if ((not Bold) and (fsBold in FT.Style)) then begin
        if Italics then Buff := Buff + '</italic>';
        if Hilight then Buff := Buff + '</highlight>';
        if FSize <> FontNormal then
             Buff := Buff + SetFontXML(FSize, false);
        Buff := Buff + '<bold>';
        if FSize <> FontNormal then
             Buff := Buff + SetFontXML(FSize, true);
         if Hilight then Buff := Buff + '<highlight>';
         if Italics then Buff := Buff + '<italic>';
        Bold := true;
    end;
    // when List turns on
    {if ((not InList) and (PM.HeadIndent > IsBullet)) then begin
        if Bold then Buff := Buff + '</bold>';
    	if Italics then Buff := Buff + '</italic>';
    	if Hilight then Buff := Buff + '</highlight>';
    	if FSize <> FontNormal then
         	Buff := Buff + SetFontXML(FSize, false);
    	if ItemOnly then Buff := Buff + '<list-item dir="ltr">'
        else Buff := Buff + '<list><list-item dir="ltr">';
    	if FSize <> FontNormal then
         	Buff := Buff + SetFontXML(FSize, true);
     	if Hilight then Buff := Buff + '<highlight>';
     	if Italics then Buff := Buff + '<italic>';
        if Bold then Buff := Buff + '<bold>';
        InList := true;
    end;   }
    Result := Buff;
end;


 {function TBSaveNote.SetListItems(const FP:TFontParams; PM:TParaMetric; const Buff:ANSIString) : ANSIString;
// This function returns a version of the text range that has -
// ListItem tags inserted after any newline other than the last char of the text.
// Any leading white space and '*' removed from start of para.
var
    Start : longint = 1;
    Index : longint = 1;
    LocalBuff : ANSIString;
    ResBuff : ANSIString = '';
    // Temp : ANSIString;
begin
    while Index < length(Buff) do begin	// we don't care about last char.
    	if Buff[Index] < ' ' then begin
            PM.HeadIndent := 0.0;
            LocalBuff := copy(Buff, Start, Index - Start);
            RemoveBulletString(LocalBuff);
         {   if Pos(BulletString, LocalBuff) = 1 then
                		delete(LocalBuff, 1, length(BulletString));   }  // Wrong, it may get an incomplete BulletString is another tag intrudes.
            ResBuff := ResBuff + LocalBuff + LineEnding;
            AddTag(FP, PM, true, ResBuff);
            PM.HeadIndent := BulletWidth;
            AddTag(FP, PM, true, ResBuff);
            Start := Index + 1;                  // Skip over the newline
        end;
        inc(Index);
    end;
    LocalBuff := copy(Buff, Start, Index - Start +1);
    if Pos(BulletString, LocalBuff) = 1 then
                		delete(LocalBuff, 1, length(BulletString));
	Result := ResBuff + LocalBuff;
end;   }

procedure TBSaveNote.Save(FileName : ANSIString; KM1 : TKMemo);
var
   Buff : ANSIstring = '';
   OutStream:TFilestream;
   BlockNo : integer = 0;
   Block : TKMemoBlock;
   BlankFont : TFont;
 begin
    KM := KM1;
    FSize := 0;
	Bold := false;
 	Italics := False;
 	HiLight := False;
 	InList := false;
    try
        outstream :=TFilestream.Create(FileName, fmCreate);
        // Write and WriteBuffer accept a buffer, not a string !  Need to start at pos 1
        // when sending string or ANSIstring otherwise it uses first byte which makes it look like a binary file.
        // http://free-pascal-general.1045716.n5.nabble.com/Creating-text-files-with-TFileStream-td2824859.html
        Buff := Header();
        OutStream.Write(Buff[1], length(Buff));
        Buff := '';
        {
        OK, an issue, if we are to write out note with Bullets this has got to change.
        Kmemo stores its knowledge about Bullets in the trailing Paragraph marker.
        So, an intermediate store between Buff and the stream is only option.
        That store must be flushed (or modified) only when we get to the next Paragraph
        marker and know what needs to be done.

        Or, alter structure so loop is -

        process header

        loop
        	loop
            	Build up Buff content
                inc BlockNo
                if BlockNo >= blocks.count break
            until TKMemoParagraph
            if BlockNo >= blocks.count break
            adjust Buff depending on bullet info
            write out and clear buff
        end loop

        process any data in Buff (assuming no para mark at end)

        process footer

        }




        try
            // Must deal with an empty list !
            repeat 									// writes out block by block.
         		Block := KM1.Blocks.Items[BlockNo];
                // temp := Block.ClassName;
                if (Block.ClassName = 'TKMemoTextBlock') or
                				(Block.ClassName = 'TKMemoHyperlink') then begin
                	AddTag(TKMemoTextBlock(Block).TextStyle.Font, Buff);
                	Buff := Buff + Block.Text;
                end else
                	Buff := Buff + LineEnding;
         		OutStream.Write(Buff[1], length(Buff));
             	Buff := '';
                inc(BlockNo);
   	 		until KM1.Blocks.LastBlock.Equals(KM1.Blocks.Items[BlockNo]);

            if KM1.Blocks.LastBlock.ClassNameIs('TKMemoTextBlock') then begin
                 // If we don't finish with a Para block, then we need to
                 // add it in as well.
                 BlankFont := TFont.Create();
                 BlankFont.Size := FontNormal;
                 BlankFont.Color := NormalColor;
                 BlankFont.Style := [];
                 AddTag(BlankFont, Buff);
                 Buff := Buff + KM1.Blocks.LastBlock.Text;
                 OutStream.Write(Buff[1], length(Buff));
                 BlankFont.Free;
            end;
            Buff := Footer();
            OutStream.Write(Buff[1], length(Buff));

         Except     { TODO 1 : Must test this to see what happens with an empty
         				list of blocks. Probably makes sense to not save anything
                        that does not have at least one TKMemotextBlock  }
            on EListError do begin
            	Buff := Footer();
            	OutStream.Write(Buff[1], length(Buff));
            end;
        end;
 	finally
        OutStream.Free;
    end;
end;


Function TBSaveNote.GetLocalTime():ANSIstring;
var
   ThisMoment : TDateTime;
begin
    {$ifdef LINUX}
    ReReadLocalTime();    // in case we are near daylight saving time changeover
    {$endif}
    ThisMoment:=Now;
    Result := FormatDateTime('YYYY-MM-DD',ThisMoment) + 'T'
                   + FormatDateTime('hh:mm:ss.z',ThisMoment) + '0000+'
                   + inttostr(GetLocalTimeOffset() div -60);
end;

Function TBSaveNote.Header() : ANSIstring;
var
   S1, S2, S3, S4 : ANSIString;
begin
  S1 := '<?xml version="1.0" encoding="utf-8"?>'#10'<note version="0.3" xmlns:link="';
  S2 := 'http://beatniksoftware.com/tomboy/link" xmlns:size="http://beatniksoftware.com/tomboy/size"';
  S3 := ' xmlns="http://beatniksoftware.com/tomboy">'#10'  <title>';
  S4 := '</title>'#10'  <text xml:space="preserve"><note-content version="0.1">';
  Result := S1 + S2 + S3 + Title + S4;
end;


Function TBSaveNote.Footer() : ANSIstring;
var
   S1, S2, S3, S4, S5 : string;
   TimeStamp : string;
begin
  TimeStamp := GetLocalTime();   // get actual time date in format like Tomboy's
  S1 := '</note-content></text>'#10'  <last-change-date>';
  S2 := '</last-change-date>'#10'  <last-metadata-change-date>';
  S3 := '</last-metadata-change-date>'#10'  <create-date>';
  S4 := '</create-date>'#10'  <cursor-position>1</cursor-position>'#10'  <selection-bound-position>1</selection-bound-position>'#10;
  S5 := '  <width>1000</width>'#10'  <height>626</height>'#10'  <x>0</x>'#10'  <y>0</y>'#10'  <open-on-startup>False</open-on-startup>'#10'</note>';
  if CreateDate = '' then CreateDate := TimeStamp;
  Result := S1 + TimeStamp + S2 + TimeStamp + S3 + CreateDate + S4 + S5;
end;

end.

