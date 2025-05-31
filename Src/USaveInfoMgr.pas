{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2025, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Saves information about a snippet to disk in various, user specifed, formats.
 * Only routine snippet kinds are supported.
}


unit USaveInfoMgr;

interface

uses
  // Project
  UBaseObjects,
  UEncodings,
  UHTMLSnippetDoc,
  USaveSourceDlg,
  USnippetDoc,
  USourceFileInfo,
  UView;


type
  ///  <summary>Class that saves information about a snippet to file a user
  ///  specified format. The snippet is obtained from a view. Only snippet views
  ///  are supported.</summary>
  TSaveInfoMgr = class(TNoPublicConstructObject)
  strict private
    var
      fView: IView;
      fSaveDlg: TSaveSourceDlg;
      fSourceFileInfo: TSourceFileInfo;

    ///  <summary>Displays a warning message about data loss if
    ///  <c>ExpectedStr</c> doesn't match <c>EncodedStr</c>.</summary>
    class procedure WarnIfDataLoss(const ExpectedStr, EncodedStr: string);

    ///  <summary>Returns type of file selected in the associated save dialogue
    ///  box.</summary>
    function SelectedFileType: TSourceFileType;

    ///  <summary>Handles the custom save dialogue's <c>OnPreview</c> event.
    ///  Displays the required snippet information, appropriately formatted, in
    ///  a preview dialogues box.</summary>
    ///  <param name="Sender"><c>TObject</c> [in] Reference to the object that
    ///  triggered the event.</param>
    procedure PreviewHandler(Sender: TObject);

    ///  <summary>Handles the custom save dialogue's <c>OnHiliteQuery</c> event.
    ///  Determines whether syntax highlighting is supported for the source code
    ///  section of the required snippet information..</summary>
    ///  <param name="Sender"><c>TObject</c> [in] Reference to the object that
    ///  triggered the event.</param>
    ///  <param name="CanHilite"><c>Boolean</c> [in/out] Set to <c>False</c>
    ///  when called. Should be set to <c>True</c> iff highlighting is
    ///  supported.</param>
    procedure HighlightQueryHandler(Sender: TObject; var CanHilite: Boolean);

    ///  <summary>Handles the custom save dialogue's <c>OnEncodingQuery</c>
    ///  event.</summary>
    ///  <param name="Sender"><c>TObject</c> [in] Reference to the object that
    ///  triggered the event.</param>
    ///  <param name="Encodings"><c>TSourceFileEncodings</c> [in/out] Called
    ///  with an empty array which the event handler must be set to contain the
    ///  encodings supported by the currently selected file type.</param>
    procedure EncodingQueryHandler(Sender: TObject;
      var Encodings: TSourceFileEncodings);

    ///  <summary>Returns an instance of the document generator object for the
    ///  desired file type.</summary>
    ///  <param name="FileType"><c>TSourceFileType</c> [in] The type of file to
    ///  be generated.</param>
    ///  <returns><c>TSnippetDoc</c>. The required document generator object.
    ///  The caller MUST free this object.</returns>
    function GetDocGenerator(const FileType: TSourceFileType): TSnippetDoc;

    ///  <summary>Generates the required snippet information in the requested
    ///  format.</summary>
    ///  <param name="FileType"><c>TSourceFileType</c> [in] Type of file to be
    ///  generated.</param>
    ///  <returns><c>TEncodedData</c>. The formatted snippet information, syntax
    ///  highlighted if required.</returns>
    function GenerateOutput(const FileType: TSourceFileType): TEncodedData;

    ///  <summary>Displays the save dialogue box and creates required type of
    ///  snippet information file if the user OKs.</summary>
    procedure DoExecute;

  strict protected

    ///  <summary>Internal constructor. Initialises managed save source dialogue
    ///  box and records information about supported file types.</summary>
    constructor InternalCreate(AView: IView);

  public

    ///  <summary>Object descructor. Tears down object.</summary>
    destructor Destroy; override;

    ///  <summary>Saves information about the snippet referenced by the a given
    ///  view to file.</summary>
    ///  <remarks>The view must be a snippet view.</remarks>
    class procedure Execute(View: IView); static;

    ///  <summary>Checks if the given view can be saved to file. Returns
    ///  <c>True</c> if the view represents a snippet.</summary>
    class function CanHandleView(View: IView): Boolean; static;

  end;

implementation

uses
  // Delphi
  SysUtils,
  Dialogs,
  // Project
  DB.SnippetKind,
  FmPreviewDlg,
  Hiliter.UAttrs,
  Hiliter.UFileHiliter,
  Hiliter.UGlobals,
  UExceptions,
  UIOUtils,
  UMarkdownSnippetDoc,
  UMessageBox,
  UOpenDialogHelper,
  UPreferences,
  URTFSnippetDoc,
  URTFUtils,
  USourceGen,
  UTextSnippetDoc;

{ TSaveInfoMgr }

class function TSaveInfoMgr.CanHandleView(View: IView): Boolean;
begin
  Result := Supports(View, ISnippetView);
end;

destructor TSaveInfoMgr.Destroy;
begin
  fSourceFileInfo.Free;
  fSaveDlg.Free;
  inherited;
end;

procedure TSaveInfoMgr.DoExecute;
resourcestring
  sDlgCaption = 'Save Snippet Information for %s';
var
  Encoding: TEncoding;        // encoding to use for output file
  FileContent: string;        // output file content before encoding
  FileType: TSourceFileType;  // type of source file
begin
  // Set up dialog box
  fSaveDlg.Filter := fSourceFileInfo.FilterString;
  fSaveDlg.FilterIndex := FilterDescToIndex(
    fSaveDlg.Filter,
    fSourceFileInfo.FileTypeInfo[Preferences.SourceDefaultFileType].DisplayName,
    1
  );
  fSaveDlg.FileName := fSourceFileInfo.DefaultFileName;
  fSaveDlg.Title := Format(sDlgCaption, [
    (fView as ISnippetView).Snippet.DisplayName]
  );
  // Display dialog box and save file if user OKs
  if fSaveDlg.Execute then
  begin
    FileType := SelectedFileType;
    Encoding := TEncodingHelper.GetEncoding(fSaveDlg.SelectedEncoding);
    try
      FileContent := GenerateOutput(FileType).ToString;
      TFileIO.WriteAllText(fSaveDlg.FileName, FileContent, Encoding, True);
    finally
      TEncodingHelper.FreeEncoding(Encoding);
    end;
  end;
end;

procedure TSaveInfoMgr.EncodingQueryHandler(Sender: TObject;
  var Encodings: TSourceFileEncodings);
begin
  Encodings := fSourceFileInfo.FileTypeInfo[SelectedFileType].Encodings;
end;

class procedure TSaveInfoMgr.Execute(View: IView);
var
  Instance: TSaveInfoMgr;
begin
  Assert(Assigned(View), 'TSaveInfoMgr.Execute: View is nil');
  Assert(CanHandleView(View), 'TSaveInfoMgr.Execute: View not supported');

  Instance := TSaveInfoMgr.InternalCreate(View);
  try
    Instance.DoExecute;
  finally
    Instance.Free;
  end;
end;

function TSaveInfoMgr.GenerateOutput(const FileType: TSourceFileType):
  TEncodedData;
var
  Doc: TSnippetDoc;
  DocData: TEncodedData;
  ExpectedText: string;
begin
  // Create required type of document generator
  Doc := GetDocGenerator(FileType);
  try
    Assert(Assigned(Doc), ClassName + '.GenerateOutput: unknown file type');
    // Generate text
    DocData := Doc.Generate((fView as ISnippetView).Snippet);
    if DocData.EncodingType <> fSaveDlg.SelectedEncoding then
    begin
      // Required encoding is different to that used to generate document, so
      // we need to convert to the desired encoding
      ExpectedText := DocData.ToString;
      // Convert encoding to that selected in save dialogue box
      Result := TEncodedData.Create(
        ExpectedText, fSaveDlg.SelectedEncoding
      );
      // Check for data loss in desired encoding
      WarnIfDataLoss(ExpectedText, Result.ToString);
    end
    else
      // Required encoding is same as that used to generate the document
      Result := DocData;
  finally
    Doc.Free;
  end;
end;

function TSaveInfoMgr.GetDocGenerator(const FileType: TSourceFileType):
  TSnippetDoc;
var
  UseHiliting: Boolean;
  IsPascalSnippet: Boolean;
  HiliteAttrs: IHiliteAttrs;  // syntax highlighter formatting attributes
begin
  IsPascalSnippet := (fView as ISnippetView).Snippet.Kind <> skFreeform;
  UseHiliting := fSaveDlg.UseSyntaxHiliting
    and TFileHiliter.IsHilitingSupported(FileType)
    and (fView as ISnippetView).Snippet.HiliteSource;
  if UseHiliting then
    HiliteAttrs := THiliteAttrsFactory.CreateUserAttrs
  else
    HiliteAttrs := THiliteAttrsFactory.CreateNulAttrs;
  // Create required type of document generator
  case FileType of
    sfRTF: Result := TRTFSnippetDoc.Create(HiliteAttrs);
    sfText: Result := TTextSnippetDoc.Create;
    sfHTML5: Result := THTML5SnippetDoc.Create(HiliteAttrs);
    sfXHTML: Result := TXHTMLSnippetDoc.Create(HiliteAttrs);
    sfMarkdown: Result := TMarkdownSnippetDoc.Create(IsPascalSnippet);
    else Result := nil;
  end;
end;

procedure TSaveInfoMgr.HighlightQueryHandler(Sender: TObject;
  var CanHilite: Boolean);
begin
  CanHilite := TFileHiliter.IsHilitingSupported(SelectedFileType);
end;

constructor TSaveInfoMgr.InternalCreate(AView: IView);
const
  DlgHelpKeyword = 'SnippetInfoFileDlg';
resourcestring
  // descriptions of supported file filter strings
  sRTFDesc = 'Rich text file';
  sTextDesc = 'Plain text file';
  sHTML5Desc = 'HTML 5 file';
  sXHTMLDesc = 'XHTML file';
  sMarkdownDesc = 'Markdown file';

begin
  inherited InternalCreate;
  fView := AView;
  fSourceFileInfo := TSourceFileInfo.Create;
  // RTF and plain text files supported at present
  fSourceFileInfo.FileTypeInfo[sfRTF] := TSourceFileTypeInfo.Create(
    '.rtf',
    sRTFDesc,
    [etASCII]
  );
  fSourceFileInfo.FileTypeInfo[sfText] := TSourceFileTypeInfo.Create(
    '.txt',
    sTextDesc,
    [etUTF8, etUTF16LE, etUTF16BE, etSysDefault]
  );
  fSourceFileInfo.FileTypeInfo[sfHTML5] := TSourceFileTypeInfo.Create(
    '.html',
    sHTML5Desc,
    [etUTF8]
  );
  fSourceFileInfo.FileTypeInfo[sfXHTML] := TSourceFileTypeInfo.Create(
    '.html',
    sXHTMLDesc,
    [etUTF8]
  );
  fSourceFileInfo.FileTypeInfo[sfMarkdown] := TSourceFileTypeInfo.Create(
    '.md',
    sMarkdownDesc,
    [etUTF8, etUTF16LE, etUTF16BE, etSysDefault]
  );

  // set default file name without converting to valid Pascal identifier
  fSourceFileInfo.RequirePascalDefFileName := False;
  fSourceFileInfo.DefaultFileName := fView.Description;

  fSaveDlg := TSaveSourceDlg.Create(nil);
  fSaveDlg.HelpKeyword := DlgHelpKeyword;
  fSaveDlg.CommentStyle := TCommentStyle.csNone;
  fSaveDlg.EnableCommentStyles := False;
  fSaveDlg.TruncateComments := Preferences.TruncateSourceComments;
  fSaveDlg.UseSyntaxHiliting := Preferences.SourceSyntaxHilited;
  fSaveDlg.OnPreview := PreviewHandler;
  fSaveDlg.OnHiliteQuery := HighlightQueryHandler;
  fSaveDlg.OnEncodingQuery := EncodingQueryHandler;
end;

procedure TSaveInfoMgr.PreviewHandler(Sender: TObject);
resourcestring
  sDocTitle = '"%0:s" snippet';
var
  // Type of snippet information document to preview: this is not always the
  // same as the selected file type, because preview dialogue box doesn't
  // support some types & we have to use an alternate.
  PreviewFileType: TSourceFileType;
  // Type of preview document supported by preview dialogue box
  PreviewDocType: TPreviewDocType;
begin
  case SelectedFileType of
    sfRTF:
    begin
      // RTF is previewed as is
      PreviewDocType := dtRTF;
      PreviewFileType := sfRTF;
    end;
    sfText:
    begin
      // Plain text us previewed as is
      PreviewDocType := dtPlainText;
      PreviewFileType := sfText;
    end;
    sfHTML5, sfXHTML:
    begin
      // Both HTML 5 and XHTML are previewed as XHTML
      PreviewDocType := dtHTML;
      PreviewFileType := sfXHTML;
    end;
    sfMarkdown:
    begin
      // Markdown is previewed as plain text
      PreviewDocType := dtPlainText;
      PreviewFileType := sfMarkdown;
    end;
    else
      raise Exception.Create(
        ClassName + '.PreviewHandler: unsupported file type'
      );
  end;
  // Display preview dialogue box aligned over the save dialogue
  TPreviewDlg.Execute(
    fSaveDlg,
    GenerateOutput(PreviewFileType),
    PreviewDocType,
    Format(sDocTitle, [fView.Description])
  );
end;

function TSaveInfoMgr.SelectedFileType: TSourceFileType;
begin
  Result := fSourceFileInfo.FileTypeFromFilterIdx(fSaveDlg.FilterIndex);
end;

class procedure TSaveInfoMgr.WarnIfDataLoss(const ExpectedStr,
  EncodedStr: string);
resourcestring
  sEncodingError = 'The selected snippet contains characters that can''t be '
    + 'represented in the chosen file encoding.' + sLineBreak + sLineBreak
    + 'Please compare the output to the snippet displayed in the Details pane.';
begin
  if ExpectedStr <> EncodedStr then
    TMessageBox.Warning(nil, sEncodingError);
end;

end.
