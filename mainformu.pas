unit mainformu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, SynEdit, SynHighlighterHTML, SynExportHTML,
  SynHighlighterPas, SynHighlighterCpp, SynHighlighterMulti, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls, Clipbrd, MarkdownProcessor,
  MarkdownUtils, LCLIntf, ComCtrls, Buttons, StrUtils, HtmlView, HtmlGlobals,
  HTMLUn2, SynHighlighterSBA;

type

  { TMainForm }

  TMainForm = class(TForm)
    B_Save: TBitBtn;
    B_Copy: TButton;
    B_Paste: TButton;
    B_ViewBrowser: TButton;
    B_OpenFile: TButton;
    B_Convert: TButton;
    HtmlViewer: THtmlViewer;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    SE_MarkDown: TSynEdit;
    SE_HTML: TSynEdit;
    Splitter1: TSplitter;
    SynCppSyn1: TSynCppSyn;
    SynExporterHTML1: TSynExporterHTML;
    SynFreePascalSyn1: TSynFreePascalSyn;
    SynHTMLSyn1: TSynHTMLSyn;
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
    procedure SE_HTMLChange(Sender: TObject);
  private
    procedure OpenInBrowser;
    procedure SetPreview;
  end;

  { CodeEmiter }

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
  RootPath,f:string;
  md:TMarkdownProcessor=nil;
  MStream: TMemoryStream = nil;

const
  CSSDecoration = '<style type="text/css">'+
                  'code{'+
                  '  color: #A00;'+
                  '}'+
                  'pre{'+
                  '  background: #f4f4f4;'+
                  '  border: 1px solid #ddd;'+
                  '  border-left: 3px solid #f36d33;'+
                  '  color: #555;'+
                  '  overflow: auto;'+
                  '  padding: 1em 1.5em;'+
                  '  display: block;'+
                  '}'+
                  'pre code{'+
                  '  color: inherit;'+
                  '}'+
                  'Blockquote{'+
                  'border-left: 3px solid #d0d0d0;'+
                  'padding-left: 0.5em;'+
                  'margin-left:1em;'+
                  '}'+
                  'Blockquote p{'+
                  'margin: 0;'+
                  '}'+
                  '</style>';

{ TCodeEmiter }

procedure TCodeEmiter.emitBlock(out_: TStringBuilder; lines: TStringList;
  meta: String);
var
  s:string;
  i:integer;
  c:char;

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
        MainForm.SynExporterHTML1.Highlighter:=TSynSBASyn.Create(MainForm);
        exportlines;
      end;
    'html':
      begin
        MainForm.SynExporterHTML1.Highlighter:=MainForm.SynHTMLSyn1;
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
      end
    else
      begin
        if meta='' then   out_.append('<pre><code>')
        else out_.append('<pre><code class="'+meta+'">');
        for s in lines do
        begin
          for i := 1 to Length(s) do
          begin
            c := s[i];
            case c of
              '&':
                out_.append('&amp;');
              '<':
                out_.append('&lt;');
              '>':
                out_.append('&gt;');
            else
              out_.append(c);
            end;
          end;
          out_.append(#10);
        end;
        out_.append('</code></pre>'#10);
      end;
  end;
end;


(*
More decoration:

<style type="text/css">
pre {
  background-color: #eee;
  border: 1px solid #999;
  display: block;
  padding: 10px;
}

Blockquote{
  border-left: 3px solid #d0d0d0;
  padding-left: 0.5em;
  margin-left:1em;
}
Blockquote p{
  margin: 0;
}
</style>
*)


{ TMainForm }

procedure TMainForm.SetPreview;
begin
  if SE_HTML.Modified then
  begin
    HtmlViewer.LoadFromString(CSSDecoration+SE_HTML.Text);
    SE_HTML.Modified:=false;
  end;
end;

procedure TMainForm.B_ConvertClick(Sender: TObject);
begin
  SE_HTML.Text:=md.process(SE_MarkDown.Text);
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
//  HtmlViewer.DefPreFontName:='Lucida Console';
  HtmlViewer.DefPreFontName:='Courier';
  HtmlViewer.ServerRoot:=RootPath;
//  HtmlViewer.OnHotSpotTargetClick:=@HtmlViewerHotSpotTargetClick;
  HtmlViewer.OnHotSpotClick:=@HtmlViewerHotSpotClick;
  HtmlViewer.OnImageRequest:=@HtmlViewerImageRequest;
  MStream := TMemoryStream.Create;
  B_ConvertClick(Self);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if FileExists(RootPath+f) then DeleteFile(RootPath+f);
  if Assigned(MStream) then freeandnil(MStream);
  if assigned(md) then md.Free;
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

procedure TMainForm.HtmlViewerImageRequest(Sender: TObject;
  const SRC: ThtString; var Stream: TStream);

var
  Filename: string;
begin
  Stream:=nil;
  FileName:=IfThen(FileExists(SRC),SRC,RootPath+SRC);
  if FileExists(FileName) then
  begin
    MStream.LoadFromFile(FileName);
    Stream := MStream;
  end;
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
  SE_MarkDown.Clear;
  SE_MarkDown.PasteFromClipboard;
  PageControl1.ActivePageIndex:=0;
end;

procedure TMainForm.B_ViewBrowserClick(Sender: TObject);
begin
  OpenInBrowser;
end;

procedure TMainForm.B_OpenFileClick(Sender: TObject);
var NewPath:string;
begin
  if OpenDialog1.Execute then
  begin
    SE_MarkDown.Lines.LoadFromFile(OpenDialog1.FileName);
    NewPath:=ExtractFilePath(OpenDialog1.FileName);
    if NewPath<>RootPath then
    begin
      SE_HTML.Clear;
      HtmlViewer.Clear;
      if FileExists(RootPath+f) then DeleteFile(RootPath+f);
      HtmlViewer.ServerRoot:=NewPath;
      RootPath:=NewPath;
    end;
    SaveDialog1.FileName:=OpenDialog1.FileName;
    PageControl1.ActivePageIndex:=0;
  end;
end;

procedure TMainForm.OpenInBrowser;
var p:string;
begin
  p:=IfThen(DirectoryIsWritable(RootPath),RootPath+f,GetTempDir+f);
  try
    SE_HTML.Lines.SaveToFile(p);
    OpenURL('file://'+p);
  except
    ShowMessage('Can not create and open the temp file');
  end;
end;

end.

