{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements abstract base class for classes that manage generation, previewing
 * and saving to disk of a source code files in various formats and encodings.
}


unit USaveSourceMgr;


interface


uses
  // Project
  CS.SourceCode.Pascal.SourceGen,
  UBaseObjects,
  UEncodings,
  USaveSourceDlg,
  USourceFileInfo;


type
  ///  <summary>
  ///  Abstract base class for classes that manage generation, previewing and
  ///  saving to disk of a source code files in various formats and encodings.
  ///  </summary>
  TSaveSourceMgr = class abstract(TNoPublicConstructObject)
  strict private
    var
      ///  <summary>Records information about supported source file types.
      ///  </summary>
      fSourceFileInfo: TSourceFileInfo;
      ///  <summary>Dialogue box used to get information from user about type of
      ///  source file to be saved.</summary>
      fSaveDlg: TSaveSourceDlg;
    ///  <summary>Handles custom save dialogue box's OnHiliteQuery event.
    ///  Determines whether syntax highlighting is supported for a given file
    ///  extension.</summary>
    ///  <param name="Sender">TObject [in] Reference to object that triggered
    ///  event.</param>
    ///  <param name="Ext">string [in] Name of extension to check.</param>
    ///  <param name="CanHilite">Boolean [in/out] Set to True if highlighting
    ///  supported for extension or False if not.</param>
    procedure HiliteQueryHandler(Sender: TObject; const Ext: string;
      var CanHilite: Boolean);
    ///  <summary>Handles custom save dialogue box's OnEncodingQuery event.
    ///  Provides array of encodings supported for a file extension.</summary>
    ///  <param name="Sender">TObject [in] Reference to object that triggered
    ///  event.</param>
    ///  <param name="Ext">string [in] Name of extension to check.</param>
    ///  <param name="Encodings">TSourceFileEncodings [in/out] Receives array of
    ///  supported encodings.</param>
    procedure EncodingQueryHandler(Sender: TObject; const Ext: string;
      var Encodings: TSourceFileEncodings);
    ///  <summary>Handles custom save dialogue's OnPreview event. Displays
    ///  source code appropriately formatted in preview dialogue box.</summary>
    ///  <param name="Sender">TObject [in] Reference to object that triggered
    ///  event.</param>
    procedure PreviewHandler(Sender: TObject);
    ///  <summary>Handles custom save dialogue's OnCanClose event. Permits
    ///  dialogue to close if filename entered in dialogue box is acceptable.
    ///  </summary>
    ///  <param name="Sender">TObject [in] Reference to object that triggered
    ///  event.</param>
    ///  <param name="CanClose">Boolean [in/out] Set to True to allow dialogue
    ///  to close or false to inhibit.</param>
    procedure CanCloseHandler(Sender: TObject; var CanClose: Boolean);
    ///  <summary>Checks if file name entered in save dialogue box is
    ///  acceptable.</summary>
    ///  <returns>True if file name is acceptable, False if not.</returns>
    function CheckEnteredFileName: Boolean;
    ///  <summary>Generates source code in desired format.</summary>
    ///  <param name="FileType">TSourceFileType [in] Type of file. Determines
    ///  output file format.</param>
    ///  <returns>TEncodedData - Formatted source code, syntax highlighted if
    ///  required.</returns>
    function GenerateOutput(const FileType: TSourceOutputFileType):
      TEncodedData;
  strict protected
    ///  <summary>Internal constructor. Initialises managed save source dialogue
    ///  box and records information about supported file types.</summary>
    constructor InternalCreate;
    ///  <summary>Displays save dialogue box and creates required type of source
    ///  code file if user OKs.</summary>
    procedure DoExecute;
    ///  <summary>Gets description of given source code file type.</summary>
    function GetFileTypeDesc(const FileType: TSourceOutputFileType): string;
      virtual; abstract;
    ///  <summary>Gets default file name to display in dialogue box.</summary>
    function GetDefaultFileName: string; virtual; abstract;
    ///  <summary>Gets dialogue box title.</summary>
    function GetDlgTitle: string; virtual; abstract;
    ///  <summary>Get dialogue box's help keyword.</summary>
    function GetDlgHelpKeyword: string; virtual; abstract;
    ///  <summary>Gets title to be used for source document.</summary>
    function GetDocTitle: string; virtual; abstract;
    ///  <summary>Generates raw, un-highlighted, source code.</summary>
    ///  <param name="CommentStyle">TPascalCommentStyle [in] Style of commenting
    ///  to be used in source code.</param>
    ///  <param name="TruncateComments">Boolean [in] Whether comments are to be
    ///  truncated to just first line of multi line snippet descriptions.
    ///  </param>
    ///  <returns>String containing generated source code.</returns>
    function GenerateSource(const CommentStyle: TPascalCommentStyle;
      const TruncateComments: Boolean): string;
      virtual; abstract;
    ///  <summary>Checks if a file name is valid for the kind of file being
    ///  saved.</summary>
    ///  <param name="FileName">string [in] Name of file to check.</param>
    ///  <param name="NameOK">Boolean [out] Set True if file name OK, False if
    ///  not.</param>
    ///  <param name="ErrorMessage">string [out] Error message describing
    ///  problem with invalid file name. Undefined if NameOK is True.</param>
    procedure CheckFileName(const FileName: string; out NameOK: Boolean;
      out ErrorMessage: string); virtual; abstract;
  public
    ///  <summary>Object descructor. Tears down object.</summary>
    destructor Destroy; override;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  CS.Config,
  CS.SourceCode.Hiliter.Brushes,
  CS.SourceCode.Hiliter.Renderers,
  FmPreviewDlg,
  UIOUtils,
  UMessageBox,
  UOpenDialogHelper,
  UPreferences;


{ TSaveSourceMgr }

procedure TSaveSourceMgr.CanCloseHandler(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := CheckEnteredFileName;
end;

function TSaveSourceMgr.CheckEnteredFileName: Boolean;
var
  ErrMsg: string;   // error message displayed if name not OK
resourcestring
  sDefaultErrMsg = 'Invalid file name'; // default error message
begin
  // Assume file name is OK
  Result := True;
  ErrMsg := '';
  // Ask caller about file name via event
  CheckFileName(fSaveDlg.FileName, Result, ErrMsg);
  if not Result then
  begin
    // File name not acceptable: display message aligned over dialogue box
    if ErrMsg = '' then
      ErrMsg := sDefaultErrMsg;
    TMessageBox.Error(fSaveDlg, ErrMsg);
  end;
end;

destructor TSaveSourceMgr.Destroy;
begin
  fSaveDlg.Free;
  fSourceFileInfo.Free;
  inherited;
end;

procedure TSaveSourceMgr.DoExecute;
var
  Encoding: TEncoding;              // encoding to use for output file
  FileContent: string;              // output file content before encoding
  FileType: TSourceOutputFileType;  // type of source file
begin
  // Set up dialogue box
  fSaveDlg.Filter := fSourceFileInfo.FilterString;
  fSaveDlg.FilterIndex := ExtToFilterIndex(
    fSaveDlg.Filter,
    fSourceFileInfo.FileTypeInfo[Preferences.SourceDefaultFileType].Extension,
    1
  );
  fSaveDlg.FileName := fSourceFileInfo.DefaultFileName;
  // Display dialogue box and save file if user OKs
  if fSaveDlg.Execute then
  begin
    FileType := fSourceFileInfo.FileTypeFromExt(
      ExtractFileExt(fSaveDlg.FileName)
    );
    FileContent := GenerateOutput(FileType).ToString;
    Encoding := TEncodingHelper.GetEncoding(fSaveDlg.SelectedEncoding);
    try
      TFileIO.WriteAllText(fSaveDlg.FileName, FileContent, Encoding, True);
    finally
      TEncodingHelper.FreeEncoding(Encoding);
    end;
  end;
end;

procedure TSaveSourceMgr.EncodingQueryHandler(Sender: TObject;
  const Ext: string; var Encodings: TSourceFileEncodings);
var
  FileType: TSourceOutputFileType;  // type of file that has given extension
begin
  FileType := fSourceFileInfo.FileTypeFromExt(Ext);
  Encodings := fSourceFileInfo.FileTypeInfo[FileType].Encodings;
end;

function TSaveSourceMgr.GenerateOutput(const FileType: TSourceOutputFileType):
  TEncodedData;
var
  HiliterCls: TDocumentHiliterClass;  // class used to create hilited document
  Brush: TSyntaxHiliterBrush;         // highlights source code
  RawSource: string;                  // raw source code
begin
  RawSource := GenerateSource(fSaveDlg.CommentStyle, fSaveDlg.TruncateComments);
  HiliterCls := TDocumentHiliterHelper.GetHiliterClass(FileType);
  { TODO: revise to allow brush to be specified based on source code language.
          This will probably need extra Language ID parameter to be added.
  }
  Brush := TSyntaxHiliterBrushes.CreateBrush(
    TSyntaxHiliterBrushes.PascalBrushID
  );
  try
    Result := HiliterCls.Hilite(
      RawSource,
      Brush,
      TConfig.Instance.HiliterThemes[
        Preferences.CurrentHiliteThemeIds[htkExport]
      ],
      GetDocTitle
    );
  finally
    Brush.Free;
  end;
end;


procedure TSaveSourceMgr.HiliteQueryHandler(Sender: TObject; const Ext: string;
  var CanHilite: Boolean);
begin
  CanHilite := TDocumentHiliterHelper.IsHilitingSupported(
    fSourceFileInfo.FileTypeFromExt(Ext)
  );
end;

constructor TSaveSourceMgr.InternalCreate;
resourcestring
  // descriptions of supported encodings
  sANSIDefaultEncoding = 'ANSI (Default)';
  sUTF8Encoding = 'UTF-8';
  sUTF16LEEncoding = 'Unicode (Little Endian)';
  sUTF16BEEncoding = 'Unicode (Big Endian)';
begin
  inherited InternalCreate;
  fSourceFileInfo := TSourceFileInfo.Create;
  with fSourceFileInfo do
  begin
    FileTypeInfo[sfText] := TSourceFileTypeInfo.Create(
      '.txt',
      GetFileTypeDesc(sfText),
      [
        TSourceFileEncoding.Create(etSysDefault, sANSIDefaultEncoding),
        TSourceFileEncoding.Create(etUTF8, sUTF8Encoding),
        TSourceFileEncoding.Create(etUTF16LE, sUTF16LEEncoding),
        TSourceFileEncoding.Create(etUTF16BE, sUTF16BEEncoding)
      ]
    );
    FileTypeInfo[sfPascal] := TSourceFileTypeInfo.Create(
      '.pas',
      GetFileTypeDesc(sfPascal),
      [
        TSourceFileEncoding.Create(etSysDefault, sANSIDefaultEncoding),
        TSourceFileEncoding.Create(etUTF8, sUTF8Encoding)
      ]
    );
    FileTypeInfo[sfHTML] := TSourceFileTypeInfo.Create(
      '.html',
      GetFileTypeDesc(sfHTML),
      [
        TSourceFileEncoding.Create(etUTF8, sUTF8Encoding)
      ]
    );
    FileTypeInfo[sfRTF] := TSourceFileTypeInfo.Create(
      '.rtf',
      GetFileTypeDesc(sfRTF),
      [
        TSourceFileEncoding.Create(etSysDefault, sANSIDefaultEncoding)
      ]
   );
    DefaultFileName := GetDefaultFileName;
  end;

  fSaveDlg := TSaveSourceDlg.Create(nil);
  fSaveDlg.Title := GetDlgTitle;
  fSaveDlg.HelpKeyword := GetDlgHelpKeyword;
  fSaveDlg.CommentStyle := Preferences.SourceCommentStyle;
  fSaveDlg.TruncateComments := Preferences.TruncateSourceComments;
  fSaveDlg.UseSyntaxHiliting := Preferences.SourceSyntaxHilited;
  fSaveDlg.OnPreview := PreviewHandler;
  fSaveDlg.OnHiliteQuery := HiliteQueryHandler;
  fSaveDlg.OnEncodingQuery := EncodingQueryHandler;
  fSaveDlg.OnCanClose := CanCloseHandler;
end;

procedure TSaveSourceMgr.PreviewHandler(Sender: TObject);
const
  // Map of source file type to preview document types
  PreviewDocTypeMap: array[TSourceOutputFileType] of TPreviewDocType = (
    dtPlainText, dtPlainText, dtHTML, dtRTF
  );
var
  FileType: TSourceOutputFileType;  // type of source file to preview
begin
  FileType := fSourceFileInfo.FileTypeFromExt(fSaveDlg.SelectedExt);
  // Display preview dialogue box. We use save dialogue as owner to ensure
  // preview dialogue box is aligned over save dialogue box
  TPreviewDlg.Execute(
    fSaveDlg,
    GenerateOutput(FileType),
    PreviewDocTypeMap[FileType],
    GetDocTitle
  );
end;

end.

