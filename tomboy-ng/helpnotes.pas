unit helpnotes;


{ We allow user to download (from github) an alternate set of help notes.
  These notes are dropped into mainunit->HelpNotesPath/alt-help/*, on Windows and Mac
  but on Linux ('cos HelpNotesPath is readonly) we put them [config-dir]/alt-help/*
  The help engine will always prefer alt-help files if they exist, if user wants to revert
  back to English, we just delete the files in alt-help.

  Windows users will need to have some ssl lib installed, what happens if they dont ....

  Much thanks to GetMem from the forum !
  https://forum.lazarus.freepascal.org/index.php/topic,46560.0.html
  PS: You need the openssl libraries on windows(ssleay32.dll and  libeay32.dll)

  HISTORY
  2019/09/08 Clean up of management logic.
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ssockets;

type

    { TFormHelpNotes }

    TFormHelpNotes = class(TForm)
        ButtonClose: TButton;
        ButtonRestore: TButton;
        Label1: TLabel;
        Label2: TLabel;
        LabelProgress: TLabel;
        ListBox1: TListBox;
        procedure ButtonCloseClick(Sender: TObject);
        procedure ButtonRestoreClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure ListBox1Click(Sender: TObject);
        procedure ListBox1DblClick(Sender: TObject);
    private
        procedure DataReceived(Sender: TObject; const ContentLength,
            CurrentPos: Int64);
        function DownloadFile(URL, FullFileName: string; out ErrorMsg: string): boolean;
        function FormatSize(Size: Int64): String;
        // This proc replaces the RTL version to enable v1.1 of ssl instead of v23
        // note we set it in DownLoadFile() after the object is created.
        procedure HttpClientGetSocketHandler(Sender: TObject;
            const UseSSL: Boolean; out AHandler: TSocketHandler);

    public

    end;

var
    FormHelpNotes: TFormHelpNotes;

implementation

{$R *.lfm}

{ TFormHelpNotes }
uses
   mainunit, // HelpNotesPath
   LazFileUtils, zipper,
  fphttpclient,
  fpopenssl,
  openssl,
  sslsockets;      // for TSSLSocketHandler etc

const
    DownLoadPath = 'https://github.com/tomboy-notes/tomboy-ng/raw/master/doc/';

resourcestring
    RS_Spanish = 'Spanish';
    RS_Installed = 'Installed';
    RS_Restored = 'Installed';
    RS_Downloading = 'Downloading please wait...';
    RS_Downloaded = 'Downloaded so far: ';
    RS_NoSSL = 'You do not appear to have the OpenSSL Library installed';

procedure TFormHelpNotes.HttpClientGetSocketHandler(Sender: TObject;
  const UseSSL: Boolean; out AHandler: TSocketHandler);
begin
  If UseSSL then begin
    AHandler := TSSLSocketHandler.Create;
    TSSLSocketHandler(AHandler).SSLType:=stTLSv1_2;  // <--
  end else
      AHandler := TSocketHandler.Create;
end;

function TFormHelpNotes.DownloadFile(URL, FullFileName : string; out ErrorMsg : string) : boolean;
var
    Client: TFPHttpClient;
begin
    result := false;
    InitSSLInterface;
    Client := TFPHttpClient.Create(nil);
    Client.OnGetSocketHandler := @HttpClientGetSocketHandler;
    Client.OnDataReceived := @DataReceived;
    Client.AllowRedirect := true;
    ErrorMsg := 'Created';
    try
        try
            Client.Get(URL, FullFileName);
        except
            on E: EInOutError do begin
                ShowMessage(RS_NOSSL);
                ErrorMsg := E.Message;
                exit;
                end;
            on E: ESSL do begin ErrorMsg := E.Message; exit; end;    // does not catch it !
            on E: Exception do begin
                ErrorMsg := E.Message;
                exit;
            end;
        end;
    finally
        //FS.Free;
        Client.Free;
    end;
    ErrorMsg := '';
    result := true;
end;

procedure TFormHelpNotes.ListBox1Click(Sender: TObject);
begin

end;

procedure TFormHelpNotes.FormCreate(Sender: TObject);
begin
    Top := 120;
    Left := 280;
    ListBox1.AddItem('es ' + RS_Spanish, Nil);
    if DirectoryExistsUTF8(MainForm.AltHelpPath) then begin
        ButtonRestore.Enabled := True;
        LabelProgress.Caption := RS_Installed;
    end else ButtonRestore.Enabled := False;
end;

procedure TFormHelpNotes.ButtonCloseClick(Sender: TObject);
begin
    close;
end;

procedure TFormHelpNotes.ButtonRestoreClick(Sender: TObject);
var
    Info : TSearchRec;
begin
    if FindFirst(MainForm.AltHelpPath + '*.*', faAnyFile, Info)=0 then begin
        repeat
            DeleteFileUTF8(MainForm.AltHelpPath() + Info.Name);                // should we test return value ?
	    until FindNext(Info) <> 0;
	end;
    FindClose(Info);
    if RemoveDirUTF8(MainForm.AltHelpPath) then begin
        LabelProgress.Caption := RS_Restored;
        ButtonRestore.Enabled := False;
        showmessage('Restored');        // remove this !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
    end else showmessage('Remove Dir Error in HelpNotes Unit');
end;

procedure TFormHelpNotes.ListBox1DblClick(Sender: TObject);
var
    EMsg : string;   ZipFile:
    TUnZipper;
    FileName : string = '';
    // DownLoadPath : string = 'http://bannons.id.au/downloads/';
begin
    if not DirectoryExistsUTF8(MainForm.AltHelpPath) then
        CreateDirUTF8(MainForm.AltHelpPath);
    // showmessage(ListBox1.Items[ListBox1.ItemIndex]);
    LabelProgress.Caption := RS_DownLoading;
    Application.ProcessMessages;
    case LeftStr(ListBox1.Items[ListBox1.ItemIndex], 2) of
            'es' : FileName := 'es_notes.zip';
    end;
    if FileName <> '' then begin
        if DownLoadFile(DownLoadPath + FileName, MainForm.AltHelpPath + FileName, EMsg) then begin
            ZipFile := TUnZipper.Create;
            try
                ZipFile.FileName := MainForm.AltHelpPath + FileName;
                ZipFile.OutputPath := MainForm.AltHelpPath;
                ZipFile.Examine;
                ZipFile.UnZipAllFiles;
                LabelProgress.Caption := RS_Installed;
            finally
                ZipFile.Free;
            end;
        end else
            showmessage('ERROR - ' + EMsg);
        ButtonRestore.Enabled := True;
    end;
end;

function TFormHelpNotes.FormatSize(Size: Int64): String;
const
  KB = 1024;
  MB = 1024 * KB;
  GB = 1024 * MB;
begin
  if Size < KB then
    Result := FormatFloat('#,##0 Bytes', Size)
  else if Size < MB then
    Result := FormatFloat('#,##0.0 KB', Size / KB)
  else if Size < GB then
    Result := FormatFloat('#,##0.0 MB', Size / MB)
  else
    Result := FormatFloat('#,##0.0 GB', Size / GB);
end;

procedure TFormHelpNotes.DataReceived(Sender: TObject; const ContentLength,
  CurrentPos: Int64);
begin
  if ContentLength > 0 then
    LabelProgress.Caption := RS_Downloaded + FormatSize(CurrentPos) + '/' + FormatSize(ContentLength)
  else
    LabelProgress.Caption := RS_Downloaded + FormatSize(CurrentPos);
  Application.ProcessMessages;
end;

end.
