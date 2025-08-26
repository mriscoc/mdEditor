unit mainformu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, SynEdit, SynHighlighterHTML, SynExportHTML,
  SynHighlighterPas, SynHighlighterCpp, SynHighlighterMulti,
  SynHighlighterJScript, SynHighlighterJava, SynHighlighterXML,
  synhighlighterunixshellscript, SynPopupMenu, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, Clipbrd, MarkdownProcessor, MarkdownUtils,
  LCLIntf, ComCtrls, Buttons, StrUtils, HtmlView, HtmlGlobals, HTMLUn2,
  SynHighlighterVHDL, SynHighlighterJSON, SynHighlighterSmali,
  SynHighlighterMarkdown, ssl_openssl, httpsend, BGRABitmap, BGRASvg,
  IniPropStorage, Menus;

type

  { TMainForm }

  TMainForm = class(TForm)
    B_Convert: TBitBtn;
    B_Copy: TBitBtn;
    B_OpenFile: TBitBtn;
    B_Paste: TBitBtn;
    B_Save: TBitBtn;
    B_ViewBrowser: TBitBtn;
    ChkB_DownloadfromWeb: TCheckBox;
    HtmlViewer: THtmlViewer;
    ImageList1: TImageList;
    IniPropStorage1: TIniPropStorage;
    CopyHTLMViewer: TMenuItem;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SE_MarkDown: TSynEdit;
    SE_HTML: TSynEdit;
    Splitter1: TSplitter;
    SynCppSyn1: TSynCppSyn;
    SynExporterHTML1: TSynExporterHTML;
    SynFreePascalSyn1: TSynFreePascalSyn;
    SynHTMLSyn1: TSynHTMLSyn;
    SynJavaSyn1: TSynJavaSyn;
    SynJScriptSyn1: TSynJScriptSyn;
    SynJSONSyn1: TSynJSONSyn;
    SynPopupMenu1: TSynPopupMenu;
    SynSmaliSyn1: TSynSmaliSyn;
    SynUNIXShellScriptSyn1: TSynUNIXShellScriptSyn;
    SynVHDLSyn1: TSynVHDLSyn;
    SynXMLSyn1: TSynXMLSyn;
    TS_MarkDown: TTabSheet;
    TS_HTML: TTabSheet;
    procedure B_CopyClick(Sender: TObject);
    procedure B_PasteClick(Sender: TObject);
    procedure B_ViewBrowserClick(Sender: TObject);
    procedure B_OpenFileClick(Sender: TObject);
    procedure B_ConvertClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HtmlViewerHotSpotClick(Sender: TObject; const SRC: ThtString;
      var Handled: Boolean);
    procedure HtmlViewerHotSpotTargetClick(Sender: TObject; const Target,
      URL: ThtString; var Handled: boolean);
    procedure HtmlViewerImageRequest(Sender: TObject; const SRC: ThtString;
      var Stream: TStream);
    procedure CopyHTLMViewerClick(Sender: TObject);
    procedure SE_HTMLChange(Sender: TObject);
  private
    procedure CheckParams;
    function getStreamData(FileName: String): TStream;
    function OpenFile(FileName: string): boolean;
    procedure OpenInBrowser;
    procedure SetPreview;
  end;

  { TCodeEmiter }

  TCodeEmiter = class(TBlockEmitter)
  public
    procedure emitBlock(out_: TStringBuilder; lines: TStringList; meta: String); override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

var
  RootPath,f:ThtString;
  md:TMarkdownProcessor=nil;
  MStream:TMemoryStream=nil;

const
  CSSDecoration = '<style type="text/css">'#10+
                  'code{'#10+
                  '  color: #A00;'#10+
                  '}'#10+
                  'pre{'#10+
                  '  background: #f4f4f4;'#10+
                  '  border: 1px solid #ddd;'#10+
                  '  border-left: 3px solid #f36d33;'#10+
                  '  color: #555;'#10+
                  '  overflow: auto;'#10+
                  '  padding: 1em 1.5em;'#10+
                  '  display: block;'#10+
                  '}'#10+
                  'pre code{'#10+
                  '  color: inherit;'#10+
                  '}'#10+
                  'Blockquote{'#10+
                  '  border-left: 3px solid #d0d0d0;'#10+
                  '  padding-left: 0.5em;'#10+
                  '  margin-left:1em;'#10+
                  '}'#10+
                  'Blockquote p{'#10+
                  '  margin: 0;'#10+
                  '}'#10+
                  'table{'#10+
                  '  border:1px solid;'#10+
                  '  border-collapse:collapse;'#10+
                  '}'#10+
                  'th{'+
                  '  padding:5px;'#10+
                  '  background: #e0e0e0;'#10+
                  '  border:1px solid;'#10+
                  '}'#10+
                  'td{'#10+
                  '  padding:5px;'#10+
                  '  border:1px solid;'#10+
                  '}'#10+
                  '</style>'#10;

{ TCodeEmiter }

procedure TCodeEmiter.emitBlock(out_: TStringBuilder; lines: TStringList;
  meta: String);
var
  s:string;

  procedure exportlines;
  var
    sstream: TStringStream;
  begin
    MainForm.SynExporterHTML1.ExportAll(lines);
    try
      sstream:=TStringStream.Create('');
      MainForm.SynExporterHTML1.SaveToStream(sstream);
      out_.Append(sstream.DataString);
    finally
      if assigned(sstream) then freeandnil(sstream);
    end;
  end;

begin
  case meta of
    'vhdl':
      begin
        MainForm.SynExporterHTML1.Highlighter:=MainForm.SynVHDLSyn1;
        exportlines;
      end;
    'html':
      begin
        MainForm.SynExporterHTML1.Highlighter:=MainForm.SynHTMLSyn1;
        exportlines;
      end;
    'js','jscript','javascript':
      begin
        MainForm.SynExporterHTML1.Highlighter:=MainForm.SynJScriptSyn1;
        exportlines;
      end;
    'java':
      begin
        MainForm.SynExporterHTML1.Highlighter:=MainForm.SynJavaSyn1;
        exportlines;
      end;
    'json':
      begin
        MainForm.SynExporterHTML1.Highlighter:=MainForm.SynJSONSyn1;
        exportlines;
      end;
    'fpc','pas','pascal':
      begin
        MainForm.SynExporterHTML1.Highlighter:=MainForm.SynFreePascalSyn1;
        exportlines;
      end;
    'cpp','c++','c':
      begin
        MainForm.SynExporterHTML1.Highlighter:=MainForm.SynCppSyn1;
        exportlines;
      end;
    'cmd','shell','bash':
      begin
        MainForm.SynExporterHTML1.Highlighter:=MainForm.SynUNIXShellScriptSyn1;
        exportlines;
      end;
    'xml':
      begin
        MainForm.SynExporterHTML1.Highlighter:=MainForm.SynXMLSyn1;
        exportlines;
      end;
    'smali':
      begin
        MainForm.SynExporterHTML1.Highlighter:=MainForm.SynSmaliSyn1;
        exportlines;
      end
    else
      begin
        if meta='' then   out_.append('<pre><code>')
        else out_.append('<pre><code class="'+meta+'">');
        for s in lines do
        begin
          TUtils.appendValue(out_,s,0,Length(s));
          out_.append(#10);
        end;
        out_.append('</code></pre>'#10);
      end;
  end;
end;


{ TMainForm }

procedure TMainForm.SetPreview;
begin
  if SE_HTML.Modified then
  begin
    HtmlViewer.LoadFromString(SE_HTML.Text);
    SE_HTML.Modified:=false;
  end;
end;

procedure TMainForm.B_ConvertClick(Sender: TObject);
begin
  SE_HTML.Text:=CSSDecoration+md.process(SE_MarkDown.Text);
  SE_HTML.Modified:=true;
  SetPreview;
end;

procedure TMainForm.B_SaveClick(Sender: TObject);
begin
  PageControl1.ActivePageIndex:=0;
  if savedialog1.Execute then
  begin
    SE_MarkDown.Lines.SaveToFile(savedialog1.FileName);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i:integer;
begin
  MStream := TMemoryStream.Create;
  md := TMarkdownProcessor.createDialect(mdCommonMark);
  md.UnSafe := false;
  md.config.codeBlockEmitter:=TCodeEmiter.Create;
  RootPath:=GetTempDir;
  I:=0;
  Repeat
    f:=Format('%s%.3d.html',['markdown',I]);
    Inc(I);
  Until not FileExists(RootPath+f);
  PageControl1.ActivePageIndex:=0;
  HtmlViewer.DefBackground:=clWhite;
  HtmlViewer.DefFontColor:=clBlack;
  HtmlViewer.DefFontName:='Helvetica';
  HtmlViewer.DefFontSize:=10;
  HtmlViewer.DefPreFontName:='Courier';
  HtmlViewer.ServerRoot:=RootPath;
//  HtmlViewer.OnHotSpotTargetClick:=@HtmlViewerHotSpotTargetClick;
  HtmlViewer.OnHotSpotClick:=@HtmlViewerHotSpotClick;
  HtmlViewer.OnImageRequest:=@HtmlViewerImageRequest;
  HtmlViewer.LoadFromString(CSSDecoration);
  CheckParams;
end;

procedure TMainForm.CheckParams;
begin
  If (ParamCount=1) and FileExists(ParamStr(1)) then
  begin
    OpenFile(ParamStr(1));
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if FileExists(RootPath+f) then DeleteFile(RootPath+f);
  if assigned(md) then md.Free;
  if Assigned(MStream) then freeandnil(MStream);
end;

procedure TMainForm.HtmlViewerHotSpotClick(Sender: TObject;
  const SRC: ThtString; var Handled: Boolean);
begin
  Handled:=OpenUrl(SRC);
end;

procedure TMainForm.HtmlViewerHotSpotTargetClick(Sender: TObject;
  const Target, URL: ThtString; var Handled: boolean);
begin
  Handled:=OpenUrl(URL);
end;

procedure ConvertSVG(FileName:string);
var
  bmp: TBGRABitmap;
  svg: TBGRASVG;
begin
  if (pos('.svg',lowercase(FileName))>0) then
  begin
    try
      MStream.Position:=0;
      svg:= TBGRASVG.Create(MStream);
      try
        bmp:=TBGRABitmap.Create(trunc(svg.Width.value),trunc(svg.Height.value));
        svg.Draw(bmp.Canvas2D,0,0);
        MStream.Clear;
        bmp.Bitmap.SaveToStream(MStream);
      finally
        if assigned(bmp) then FreeAndNil(bmp);
      end;
    finally
      if assigned(svg) then FreeAndNil(svg);
    end;
  end;
end;

function TMainForm.getStreamData(FileName : String):TStream;
var
  sl        : TStringList;
  bFail     : Boolean;
  bTryAgain : Boolean;
Begin
  Result:=nil;
  MStream.Clear;
  with THTTPSend.Create do
  Begin
    sl := TStringList.Create;
    if HTTPMethod('GET',FileName) then
    Begin
      MStream.CopyFrom(Document, 0);
      // Need To check For Failed Retrieval...
      MStream.Position:= 0;
      sl.LoadFromStream(MStream);
      bTryAgain := False;
      bFail     := False;
      if Length(sl.Text) = 0 then bFail:= True;
      if MStream.Size = 0 then bFail:= True;

      if MStream.Size < 1024 then
      Begin
        if Pos('Not Found', sl.Text) > 0 then bFail:= True;
        if (Pos(LowerCase('<title>301 Moved Permanently</title>'), LowerCase(sl.Text)) > 0) or
           (Pos(LowerCase('<html><body>'), LowerCase(sl.Text)) > 0) then
        Begin
          if Pos(LowerCase('<a href="'), LowerCase(sl.Text)) > 0 then
          Begin
            FileName := Copy(sl.Text, Pos('<a href="', sl.Text) + 9, Length(sl.Text));
            FileName := Copy(FileName, 1, Pos('"', FileName) -1);
            bTryAgain:= True;
          End;
        end;
      end;

      if bTryAgain then
        // Call Function Again...
        Result:= getStreamData(FileName);

      if not bTryAgain And not bFail then
      begin
        ConvertSVG(FileName);
        Result:= MStream;
      end;

    end; // If HTTPMethod
    Free;
    if Assigned(sl) then freeandnil(sl);
  end; // With HTTPSend
end;

procedure TMainForm.HtmlViewerImageRequest(Sender: TObject;
  const SRC: ThtString; var Stream: TStream);
var
  fullName  : ThtString;

Begin

  // HTMLViewer needs to be nil'ed
  Stream:= nil;

  // is "fullName" a local file, if not aquire file from internet
  fullName:=IfThen(FileExists(SRC),SRC,IfThen(FileExists(RootPath+SRC),RootPath+SRC,SRC));
  fullName := HtmlViewer.HTMLExpandFilename(fullName);

  if FileExists(fullName) then  // if local file, load it..
  Begin
    MStream.LoadFromFile(fullName);
    ConvertSVG(fullName);
    Stream:=MStream;
  end else if ChkB_DownloadfromWeb.Checked then  // if not local file, download it..
  Begin
    getStreamData(fullName);
    Stream:=MStream;
  End;
End;

procedure TMainForm.CopyHTLMViewerClick(Sender: TObject);
begin
  HtmlViewer.CopyToClipboard;
end;

procedure TMainForm.SE_HTMLChange(Sender: TObject);
begin
  SetPreview;
end;

procedure TMainForm.B_CopyClick(Sender: TObject);
begin
  Clipboard.AsText:=SE_MarkDown.text;
end;

procedure TMainForm.B_PasteClick(Sender: TObject);
begin
  SE_MarkDown.PasteFromClipboard;
  PageControl1.ActivePageIndex:=0;
end;

procedure TMainForm.B_ViewBrowserClick(Sender: TObject);
begin
  OpenInBrowser;
end;

function TMainForm.OpenFile(FileName:string):boolean;
var NewPath:ThtString;
begin
  try
    SE_MarkDown.Lines.LoadFromFile(FileName);
    NewPath:=ExtractFilePath(FileName);
    if NewPath<>RootPath then
    begin
      SE_HTML.Clear;
      HtmlViewer.Clear;
      if FileExists(RootPath+f) then DeleteFile(RootPath+f);
      HtmlViewer.ServerRoot:=NewPath;
      RootPath:=NewPath;
    end;
    SaveDialog1.InitialDir:=NewPath;
    SaveDialog1.FileName:=ExtractFileName(FileName);
    PageControl1.ActivePageIndex:=0;
    Result:=true;
  except
    Result:=false;
  end;
end;

procedure TMainForm.B_OpenFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then OpenFile(OpenDialog1.FileName);
end;

procedure TMainForm.OpenInBrowser;
var p:string;
begin
  p:=IfThen(DirectoryIsWritable(RootPath),RootPath+f,GetTempDir+f);
  try
    SE_HTML.Lines.SaveToFile(p);
    OpenURL(p);
  except
    ShowMessage('Can not create and open the temp file');
  end;
end;


end.

