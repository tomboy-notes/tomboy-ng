unit Notebook;

{   Copyright (C) 2017-2024 David Bannon

    License:
    This code is licensed under MIT License, see the file License.txt
    or https://spdx.org/licenses/MIT.html  SPDX short identifier: MIT

    ------------------

    This GUI based unit has a form to allow user to see and select what notebooks
    the current note is a member of. It looks at settings to see if we are allowing
    a particular note to be a member of more than one notebook. If not, will cancel
    a previous choice if a user selects a new notebook.

    This form is created dynamically and shown modal, the user can only open one
    at a time. If shown non-modal, there is a danger form will get lost ....

    See the doc for Modes below for details

    History -
    2018/01/30 -replaced the function that cancels previous Notebook selection when
                a new one is made (if settings so demand). This one works on Macs
                and is a better job on the other platforms too.
    2018/04/13  Now call NotebookPick Form dynamically and ShowModal to ensure two notes don't share.
    2018/05/12  Extensive changes - MainUnit is now just that. Only change here relates
                to naming of MainUnit and SearchUnit.
    2019/05/18  Corrected alignment Label1 and 3
    2019/05/19  Display strings all (?) moved to resourcestrings
    2020/02/19  Do not escape new notebook title as sent to notelister.
    2020/05/19  Do not go through ButtonOKOnClick if ModalResult is already set to mrOK
    2020/08/10  In Windows, SetFocus was setting ModalRes to 1, so, would immediatly close ??
    2021/11/04  Extensive changes to support new Notebook management model from SearchForm
    2021/12/26  MakeNewNoteBook may have returned random bool.
    2025/02/25  Removed LCLProc to stop it handling debugln()
}


{$mode objfpc}{$H+}

interface

uses
		Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst,
		ExtCtrls, StdCtrls, Buttons, ComCtrls;


type NotebookMode = (
                            // Changing name of a notebook, need Name in ....
        nbChangeName,       // Open in TabChangeName, hide all others. Call from Search

                            // Set the Notebooks a note is in, need FullFileName, Title. Call from
                            // the note itself. Open in TabExisting, also show TabNewNoteBook, hide others
                            // This unit does not update file directly in this mode but does update
        nbSetNoteBooks,     // the NoteLister. So, if called from elsewhere, must take steps to update file.

                            // Make a new NoteBook,
        nbMakeNewNoteBook,  // Open in TabNewNoteBook, also show but disable TabSetNotes, hide others

                               // Set the notes that are a member of this notebook, need NBName
        nbSetNotesInNoteBook   // Open in TabSetNotes, hide all others
        );

type

		{ TNoteBookPick }

  TNoteBookPick = class(TForm)
				Button1: TButton;
				ButtonOK: TButton;
				CheckListBox1: TCheckListBox;
                CheckListAddNotes: TCheckListBox;
                EditNewNotebookName: TEdit;
				EditNewNotebook: TEdit;
				Label1: TLabel;
				Label2: TLabel;
				Label3: TLabel;
				Label4: TLabel;
				Label5: TLabel;
                Label6: TLabel;
                Label7: TLabel;
                Label8: TLabel;
                Label9: TLabel;
				PageControl1: TPageControl;
				Panel1: TPanel;
				TabExisting: TTabSheet;
				TabNewNoteBook: TTabSheet;
                TabChangeName: TTabSheet;
                TabSetNotes: TTabSheet;
				procedure ButtonOKClick(Sender: TObject);
                procedure CheckListBox1ItemClick(Sender: TObject; Index: integer);
                procedure EditNewNotebookKeyDown(Sender: TObject;
                    var Key: Word; {%H-}Shift: TShiftState);
                procedure EditNewNotebookNameEditingDone(Sender: TObject);
                procedure FormShow(Sender: TObject);
                procedure SetupForAddNotes();
                procedure TabNewNoteBookShow(Sender: TObject);
		private
                                      { A list pointer that will point to list of notes that are
                                        members of the notebook who's name we are about to change }
                NBIDList : TStringList;
                                    { Actually do all the stuff necessary when we change a notebook name }
                procedure AdjustNBookNotes();
                function ChangeNoteBookName(NewName: string): boolean;
                procedure InsertNoteBookTag(const FullFileName, NB: string);
                function MakeNewNoteBook: boolean;
                function RewriteTempate(const FileName, NewName: string
                    ): boolean;
                function RewriteWithNewNotebookName(FileName: string): boolean;
                procedure SetNoteBooks;
                                    { User wants to change the name of a Notebook, Title is name of Notebook }
                procedure SetupForChange();
                                    { Allow user to select an existing Notebook or make a new one }
                procedure SetupForNewSelect();
		public
                TheMode : NoteBookMode;         // Just what are we doing here ?
        	    FullFileName : ANSIString;      // The filename of the Note that invoked self. So, apply to this note oonly.
                Title  : ANSIString;            // Title of note that invoked self.
                NBName : string;                // Notebook Name, means we are working with just this notebook.
                ChangeMode : boolean;           // Indicates we wish to rename  existing notebook.
		end;

{var
		NoteBookPick: TNoteBookPick;   }

implementation

{$R *.lfm}

{ TNoteBookPick }

uses LazLogger, SearchUnit, LazFileUtils, {LCLProc,} LCLType, Settings, SaveNote, EditBox,
    resourcestr, tb_utils, note_lister
        {$ifdef WINDOWS}, SyncUtils{$endif};    // SafeWindowsDelete

procedure TNoteBookPick.SetupForNewSelect();
var
    //SL : TStringList;
    NBArray : TStringArray;
    Index, I : Integer;
begin
    PageControl1.ActivePage := TabExisting;
    TabExisting.TabVisible := True;
    TabNewNotebook.TabVisible := True;
    TabSetNotes.TabVisible := False;
    TabChangeName.TabVisible := False;
    Label1.Caption := Title;
    Label3.Caption := rsSetTheNotebooks;
    TheMainNoteLister.GetNotebooks(NBArray, '');
    for i := 0 to High(NBArray) do
        CheckListBox1.Items.Add(NBArray[i]);
    TheMainNoteLister.GetNotebooks(NBArray, ExtractFileNameOnly(FullFileName) + '.note');
    for I := 0 to CheckListBox1.Count-1 do
        CheckListBox1.Checked[I] := False;
    for Index := 0 to High(NBArray) do
    	for I := 0 to CheckListBox1.Count-1 do
			if NBArray[Index] = CheckListBox1.Items[I] then
            	CheckListBox1.Checked[I] := True;
end;

procedure TNoteBookPick.SetupForChange();
{var
    NoteID : String;}
begin
    //  Note : NBIDList does not need to be created or freed. Just a pointer.
    PageControl1.ActivePage := TabChangeName;
    TabNewNotebook.TabVisible := False;
    TabSetNotes.TabVisible := False;
    TabChangeName.TabVisible := True;
    TabExisting.TabVisible := False;
    Label3.Caption := rsChangeNameofNotebook;
    Label7.Caption := Title;
    if not TheMainNoteLister.GetNotesInNoteBook(NBIDList, Title) then
        {DebugLogger.}debugln('TNoteBookPick.SetupForChange ERROR - Notebook.pas #152 No member notes found');
    Label1.Caption := format(rsNumbNotesAffected, [NBIDList.Count]);
    EditNewNotebookName.SetFocus;
end;

procedure TNoteBookPick.SetupForAddNotes();
var
    STL : TStringList=Nil;  // does not require create/free
    Index, I : integer;
begin
    PageControl1.ActivePage := TabSetNotes;
    TabSetNotes.Enabled := True;
    TabExisting.TabVisible := False;
    TabNewNotebook.TabVisible := False;
    TabChangeName.TabVisible := False;
    Label1.Caption := NBName;
    Label3.Caption := rsAddNotesToNotebook;
    TheMainNoteLister.LoadStrings(CheckListAddNotes.Items);

    TheMainNoteLister.GetNotesInNoteBook(STL, NBName);               // Might set STL to nil
    if (STL <> Nil) and (STL.Count > 0) then begin
        for Index := 0 to CheckListAddNotes.Count -1 do begin
            for i := 0 to STL.Count-1 do begin
                if CheckListAddNotes.Items[Index] = TheMainNoteLister.GetTitle(STL[i]) then begin
                    CheckListAddNotes.Checked[Index] := True;
                    continue;
                end;
            end;
        end;
    end;
end;

procedure TNoteBookPick.TabNewNoteBookShow(Sender: TObject);
begin
    EditNewNotebook.SetFocus;
end;


{
If ChangeMode we are changing the name of a notebook. Messy. Else -
If FullFileName has something in it, then we are managing the NoteBooks that note is in.
If its neither ChangeMode nor FullFileName then its one of
If NBName has something, we are managing the notes that are in that NoteBook.

}
procedure TNoteBookPick.FormShow(Sender: TObject);
begin
    if Sett.CheckManyNotebooks.Checked then
        Label2.Caption := rsMultipleNoteBooks
    else Label2.Caption := rsOneNoteBook;

    case TheMode of

        nbSetNoteBooks : SetupForNewSelect();

        nbMakeNewNoteBook : begin
                                Label5.Caption := rsEnterNewNotebook;
                                PageControl1.ActivePage := TabNewNotebook;
                                TabExisting.TabVisible := False;
                                TabNewNotebook.TabVisible := True;
                                TabSetNotes.Visible := True;
                                TabChangeName.TabVisible := False;
                                TabSetNotes.Enabled := False;
                            end;
        nbSetNotesInNoteBook : SetupForAddNotes();

        nbChangeName : SetUpForChange();
    end;
    ModalResult := 0;           // On windows, 'something' in setfocus sets this to 1 !
end;

procedure TNoteBookPick.CheckListBox1ItemClick(Sender: TObject; Index: integer);
var
	I : integer;
begin
    if Sett.CheckManyNotebooks.Checked then exit;
    // ensure only one clicked.
    if (Sender as TCheckListBox).Checked[Index] then begin
        for I := 0 to CheckListBox1.Count -1 do
            CheckListBox1.Checked[I] := False;
        CheckListBox1.Checked[Index] := True;
    end;
end;

procedure TNoteBookPick.EditNewNotebookKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    if Key = VK_RETURN then begin
        key := 0;
        ButtonOK.Click;
    end;
end;

procedure TNoteBookPick.EditNewNotebookNameEditingDone(Sender: TObject);        // this one for change
begin
    ButtonOK.Click;
end;

function TNoteBookPick.RewriteWithNewNotebookName(FileName : string) : boolean; // ToDo : replace this fun with tb_utils.ReplaceNoteBookTags()
var
    InFile, OutFile: TextFile;
    {NoteDateSt, }InString, TempName, NextSeekString : string;
begin
  if not fileexists(Sett.NoteDirectory + FileName) then exit(false);            // if its not there, the note has just been deleted
  TempName := AppendPathDelim(Sett.NoteDirectory) + 'tmp';
  if not DirectoryExists(TempName) then
      CreateDir(AppendPathDelim(tempname));
  TempName := tempName + pathDelim + FileName;
  AssignFile(InFile, Sett.NoteDirectory + FileName);
  AssignFile(OutFile, TempName);
  try
      try
          Reset(InFile);
          Rewrite(OutFile);
          NextSeekString := '<last-change-date>';
          while not eof(InFile) do begin
              readln(InFile, InString);
              if (Pos(NextSeekString, InString) > 0) then begin
                    case NextSeekString of
                        '<last-change-date>' : begin
                                                    writeln(outFile, '  <last-change-date>'
                                                        + TB_GetLocalTime() + '</last-change-date>');
                                                    NextSeekString := '<last-metadata-change-date>';
                                                end;
                        '<last-metadata-change-date>' : begin
                                                    writeln(outFile, '  <last-metadata-change-date>'
                                                        + TB_GetLocalTime() + '</last-metadata-change-date>');
                                                    NextSeekString := '<y>';
                                                end;
                        '<y>' :     begin
                                        writeln(OutFile, InString);
                                        write(OutFile, TheMainNoteLister.NoteBookTags(Filename));
                                        NextSeekString := '<tags>';
                                    end;
                        '<tags>' :  begin
                                        readln(InFile, InString);                   // Danger, wot if we hit EOF ?
                                        while pos('<tag>', Instring) > 0 do
                                            readln(InFile, InString);               // now we have the </tags> line.
                                        NextSeekString := '321-blar-blar-blar-blar-123';
                                    end;
                    end;
              end else
                    writeln(OutFile, InString);
          end;                                  // end of while loop.
          //writeln(OutFile, '</note>');
      finally
          CloseFile(OutFile);
          CloseFile(InFile);
      end;
  except
    on E: EInOutError do begin
        debugln('File handling error occurred updating clean note location. Details: ' + E.Message);
        exit(False);
    end;
  end;
  {$ifdef WINDOWS}
  if not SafeWindowsDelete(Sett.NoteDirectory + FileName, NextSeekstring) then begin
      showmessage(NextSeekString);
      exit(false);
  end;
  {$endif}
  result := CopyFile(TempName, Sett.NoteDirectory + FileName);
end;

function TNoteBookPick.RewriteTempate(const FileName, NewName : string) : boolean;
var
    InFile, OutFile: TextFile;
    InString, TempName, NextSeekString : string;
begin
  if not fileexists(Sett.NoteDirectory + FileName) then exit(false);     // if its not there, the note has just been deleted
  TempName := AppendPathDelim(Sett.NoteDirectory) + 'tmp';
  if not DirectoryExists(TempName) then
      CreateDir(AppendPathDelim(tempname));
  TempName := tempName + pathDelim + FileName;
  AssignFile(InFile, Sett.NoteDirectory + FileName);
  AssignFile(OutFile, TempName);
  try
      try
          Reset(InFile);
          Rewrite(OutFile);
          NextSeekString := '<title>';
          while not eof(InFile) do begin
              readln(InFile, InString);
              if (Pos(NextSeekString, InString) > 0) then begin
                    case NextSeekString of
                        '<title>' : begin
                                        writeln(outFile, '<title>' + NewName + ' Template</title>');
                                        NextSeekString := '<note-content version=';
                                    end;
                        '<note-content version=' :
                                    begin
                                        writeln(outFile, '<text xml:space="preserve"><note-content version="0.1">'
                                                + NewName + ' Template');
                                        NextSeekString := '<last-change-date>';
                                    end;
                        '<last-change-date>' : begin
                                                    writeln(outFile, '  <last-change-date>'
                                                        + TB_GetLocalTime() + '</last-change-date>');
                                                    NextSeekString := '<last-metadata-change-date>';
                                                end;
                        '<last-metadata-change-date>' : begin
                                                    writeln(outFile, '  <last-metadata-change-date>'
                                                        + TB_GetLocalTime() + '</last-metadata-change-date>');
                                                    NextSeekString := '<y>';
                                                end;
                        '<y>' :     begin
                                        writeln(OutFile, InString);
                                        writeln(OutFile, '  <tags>');
                                        writeln(OutFile, '    <tag>system:template</tag>');
                                        writeln(OutFile, '    <tag>system:notebook:' + NewName + '</tag>');
                                        writeln(OutFile, '  </tags>');
                                        NextSeekString := '<tags>';
                                    end;
                        '<tags>' :  begin                                           // just drop on floor.
                                        readln(InFile, InString);                   // Danger, wot if we hit EOF ?
                                        while pos('<tag>', Instring) > 0 do
                                            readln(InFile, InString);               // now we have the </tags> line.
                                        NextSeekString := '321-blar-blar-blar-blar-123';  // wow, if we find that ???
                                    end;
                    end;
              end else
                    writeln(OutFile, InString);
          end;                                  // end of while loop.
      finally
          CloseFile(OutFile);
          CloseFile(InFile);
      end;
  except
    on E: EInOutError do begin
        debugln('File handling error occurred updating template. Details: ' + E.Message);
        exit(False);
    end;
  end;
  {$ifdef WINDOWS}
  if not SafeWindowsDelete(Sett.NoteDirectory + FileName, NextSeekstring) then begin
      showmessage(NextSeekString);
      exit(false);
  end;
  {$endif}
  result := CopyFile(TempName, Sett.NoteDirectory + FileName);
end;




function TNoteBookPick.ChangeNoteBookName(NewName : string) : boolean;
            { 1. We have a list of all the notes that are members of this notebook.
              2. Change the Notebook name stored in the Notebook data structure.
              3. For notes that are open, just force a write, reach in and mark dirty....
              4. For notes that are not open, we rewrite them, setting a new list of notebook tags
                 according to the data structure, a new last-change-date and a last-metadata-change-date.
              5. Rewrite Template, give it a new Title (which is new Notebook Name plus ' Template')
                 which needs to be written twice, updated last-change-date and last-metadata-change-date,
                 finally remove its one Notebook name and replace it with new notebook name.
              VERY IMPORTANT that end user has fully sync'ed before doing this. Else we
              might leave notes on remote machine that believe they belong to a missing notebook. }
var
    IDstr, TemplateID : string;
    OpenForm : TForm; //TEditBoxForm;
begin
    result := true;
    TemplateID := TheMainNoteLister.NotebookTemplateID(Title);
    if TemplateID = '' then begin
        showmessage('Failed to ID Template [' + Title + '] (' + NewName + ')');
        exit(false);
    end;
    TheMainNoteLister.AlterNotebook(Title, NewName);
    for IDstr in NBIDList do begin
        if TheMainNoteLister.IsThisNoteOpen(IDStr, OpenForm) then
            TEditBoxForm(OpenForm).MarkDirty
        else RewriteWithNewNotebookName(IDstr);
    end;
    // OK, now change template ......
    // debugln('template is ' + SearchForm.notelister.NotebookTemplateID(Title));
    RewriteTempate(TemplateID, RemoveBadXMLCharacters(NewName));
    ModalResult := mrOK;
    close;
end;

{ After use presses OK on AddNotes tab, we need to make the file/notelister notebook
  status agree with the displayed CheckListBox.  Its for one particular Notebook, so
  I ask notelister for a list of notes it believes belong to this NoteBook
}




procedure TNoteBookPick.InsertNoteBookTag(const FullFileName, NB : string);
var
    InFile, OutFile: TextFile;
    InString : string;
begin
    AssignFile(InFile, FullFileName);
    AssignFile(OutFile, FullFileName + '-temp');
    try
        try
            Reset(InFile);
            Rewrite(OutFile);
            while not eof(InFile) do begin
                readln(InFile, InString);
                if (Pos('</y>', InString) > 0) then begin                       // OK, next line may already have <tags>....
                    writeln(OutFile, InString);
                    readln(InFile, InString);
                    if (Pos('<tags>', InString) > 0) then begin                 // Already has <tags>, we add ours
                        writeln(outFile, InString);                             // Thats <tags>
                        writeln(outFile, '    <tag>system:notebook:' + NB + '</tag>');
                    end else  begin                                             // we need to add the lot
                        writeln(outFile, '  <tags>'#10'    <tag>system:notebook:' + NB + '  </tag>'#10'</tags>');
                        writeln(outFile, InString);                             // whatever it is
                    end;
                end else writeln(OutFile, InString);
            end;
        finally
            CloseFile(OutFile);
            CloseFile(InFile);
        end;
    except
        on E: EInOutError do
            debugln('File handling error occurred. Details: ' + E.Message);
    end;
    if not TB_ReplaceFile(FullFileName + '-temp', FullFileName) then
        debugln('ERROR, TNoteBookPick.InsertNoteBookTag failed to mv ' + FullFileName + '-temp to ' + FullFileName);
end;

{ Will adjust on disk note file and NoteLister to agree with NoteBook content
shown in CheckListAddNotes. }

procedure TNoteBookPick.AdjustNBookNotes();
var
    STL : TStringList;       // gets set to point to a list of FNames of notes in NBName
    Index, i : integer;
    InNoteList, InCheckList : boolean;   // if true, Note is a member of NBName (in relevent view)
    FName : string;
    Dummy : TForm;
begin
    TheMainNoteLister.GetNotesInNoteBook(STL, NBName);               // Might set STL to nil
    for Index := 0 to CheckListAddNotes.Count -1 do begin            // A list of note Titles
        // So, of the IDs in STL, does one of them have a title to match the one in CheckListAddNotes[Index] ?
        InNoteList := False;
        i := 0;
        while i <  STL.count do begin
        //for i := 0 to STL.count - 1 do
            if TheMainNoteLister.GetTitle(STL[i]) = CheckListAddNotes.Items[Index] then begin
                InNoteList := True;
                break;
            end;
            inc(i);
        end;
        InCheckList := CheckListAddNotes.Checked[Index];
        if InNoteList and InCheckList  then continue;
        if not (InNoteList or InCheckList) then continue;
        // OK, some action is required
        FName := string(CheckListAddNotes.Items.Objects[Index]);                // Thats the short file name, ID.note
        if InNoteList and (not InCheckList) then begin                          // remove tag from note and notelister
            STL.Delete(i);                                                      // Remove the NoteLister entry
            RemoveNoteBookTag(Sett.NoteDirectory+FName, NBName);                // Remove NB tag from note file
        end;
        if (not InNoteList) and InCheckList then begin                          // add tag to note and notelister
            TheMainNoteLister.AddNoteBook(FName, NBName, false);                // Update internal data view
            if not TheMainNoteLister.IsThisNoteOpen(FName, Dummy) then          // update on disk files
                InsertNoteBookTag(Sett.NoteDirectory+FName, NBName);
        end;
    end;
end;

// Makes a new NoteBook from TabNewNoteBook
function TNoteBookPick.MakeNewNoteBook : boolean;
begin
    Result := True;
    if TheMainNoteLister.IsANotebookTitle(EditNewNotebook.Text) then begin
        showmessage('That notebook already exists.');
        exit(false);
    end;
    if EditNewNotebook.Text = '' then begin
        showmessage(rsEnterNewNotebook);
        exit(false);
    end;
    SaveNewTemplate(EditNewNotebook.Text);      // that will also add the notebook to Note_Lister
    NBName := EditNewNotebook.Text;
    if TheMode = nbMakeNewNoteBook then
        SetupForAddNotes()
    else begin
        TheMainNoteLister.AddNoteBook(ExtractFileNameOnly(FullFileName) + '.note', EditNewNotebook.Text, False);
        // that adds the current note to the newly created notebook.
        SearchForm.RefreshNotebooks();
    end;
end;

// Sets one note to be a member of the Notebooks checked in TabExisting.
procedure TNotebookPick.SetNoteBooks;
var
    SL : TStringList;
    Index : Integer;
begin
    SL := TStringList.Create;                                           // That is TabExisting
    try
        for Index := 0 to CheckListBox1.Count -1 do
            if CheckListBox1.Checked[Index] then SL.Add(CheckListBox1.Items[Index]);
        TheMainNoteLister.SetNotebookMembership(ExtractFileNameOnly(FullFileName) + '.note', SL);
    finally
        Sl.Free;
    end;
end;

procedure TNoteBookPick.ButtonOKClick(Sender: TObject);
begin
    if ModalResult = mrOK then
        exit;
    case TheMode of

        nbSetNoteBooks :    if PageControl1.ActivePage = TabExisting then
                                SetNoteBooks
                            else  MakeNewNoteBook;

        nbMakeNewNoteBook : if not MakeNewNoteBook then exit;                   // Exit if invalid NB name

        nbSetNotesInNoteBook : AdjustNBookNotes;                                // Make file/notelister agree with user selections

        nbChangeName : if EditNewNotebookName.Text <> '' then
                            if not ChangeNoteBookName(EditNewNotebookName.Text) then exit;
    end;
    if TheMode = nbMakeNewNoteBook then begin                                   // don't close, mv to SetNotes mode
        TheMode := nbSetNotesInNoteBook;
        exit;
    end;
    ModalResult := mrOK;
end;

end.

