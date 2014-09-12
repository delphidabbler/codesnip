{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2012, Peter Johnson (www.delphidabbler.com).
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
  UBaseObjects, UEncodings, USaveSourceDlg, USourceFileInfo, USourceGen;


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
      ///  <summary>Dialog box used to get information from user about type of
      ///  source file to be saved.</summary>
      fSaveDlg: TSaveSourceDlg;
    ///  <summary>Handles custom save dialog box's OnHiliteQuery event.
    ///  Determines whether syntax highlighting is supported for a given file
    ///  extension.</summary>
    ///  <param name="Sender">TObject [in] Reference to object that triggered
    ///  event.</param>
    ///  <param name="Ext">string [in] Name of extension to check.</param>
    ///  <param name="CanHilite">Boolean [in/out] Set to True if highlighting
    ///  supported for extension or False if not.</param>
    procedure HiliteQueryHandler(Sender: TObject; const Ext: string;
      var CanHilite: Boolean);
    ///  <summary>Handles custom save dialog box's OnEncodingQuery event.
    ///  Provides array of encodings supported for a file extension.</summary>
    ///  <param name="Sender">TObject [in] Reference to object that triggered
    ///  event.</param>
    ///  <param name="Ext">string [in] Name of extension to check.</param>
    ///  <param name="Encodings">TSourceFileEncodings [in/out] Receives array of
    ///  supported encodings.</param>
    procedure EncodingQueryHandler(Sender: TObject; const Ext: string;
      var Encodings: TSourceFileEncodings);
    ///  <summary>Handles custom save dialog's OnPreview event. Displays source
    ///  code appropriately formatted in preview dialog box.</summary>
    ///  <param name="Sender">TObject [in] Reference to object that triggered
    ///  event.</param>
    procedure PreviewHandler(Sender: TObject);
    ///  <summary>Handles custom save dialog's OnCanClose event. Permits dialog
    ///  to close if filename entered in dialog box is acceptable.</summary>
    ///  <param name="Sender">TObject [in] Reference to object that triggered
    ///  event.</param>
    ///  <param name="CanClose">Boolean [in/out] Set to True to allow dialog to
    ///  close or false to inhibit.</param>
    procedure CanCloseHandler(Sender: TObject; var CanClose: Boolean);
    ///  <summary>Checks if file name entered in save dialog box is acceptable.
    ///  </summary>
    ///  <returns>True if file name is acceptable, False if not.</returns>
    function CheckEnteredFileName: Boolean;
    ///  <summary>Checks if highlighting is supported for a file type.</summary>
    ///  <param name="FileType">TSourceFileType [in] File type to check.</param>
    ///  <returns>True if file type supports highlighting, False if not.
    ///  </returns>
    function IsHilitingSupported(const FileType: TSourceFileType): Boolean;
    ///  <summary>Generates source code in desired format.</summary>
    ///  <param name="FileType">TSourceFileType [in] Type of file. Determines
    ///  output file format.</param>
    ///  <returns>TEncodedData - Formatted source code, syntax highlighted if
    ///  required.</returns>
    function GenerateOutput(const FileType: TSourceFileType): TEncodedData;
  strict protected
    ///  <summary>Internal constructor. Initialises managed save source dialog
    ///  box and records information about supported file types.</summary>
    constructor InternalCreate;
    ///  <summary>Displays save dialog box and creates required type of source
    ///  code file if user OKs.</summary>
    procedure DoExecute;
    ///  <summary>Gets description of given source code file type.</summary>
    function GetFileTypeDesc(const FileType: TSourceFileType): string;
      virtual; abstract;
    ///  <summary>Gets default file name to display in dialog box.</summary>
    function GetDefaultFileName: string; virtual; abstract;
    ///  <summary>Gets dialog box title.</summary>
    function GetDlgTitle: string; virtual; abstract;
    ///  <summary>Get dialog box's help keyword.</summary>
    function GetDlgHelpKeyword: string; virtual; abstract;
    ///  <summary>Gets title to be used for source document.</summary>
    function GetDocTitle: string; virtual; abstract;
    ///  <summary>Generates raw, un-highlighted, source code.</summary>
    ///  <param name="CommentStyle">TCommentStyle [in] Style of commenting to be
    ///  used in source code.</param>
    ///  <param name="TruncateComments">Boolean [in] Whether comments are to be
    ///  truncated to just first line of multi line snippet descriptions.
    ///  </param>
    ///  <returns>String containing generated source code.</returns>
    function GenerateSource(const CommentStyle: TCommentStyle;
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
  FmPreviewDlg, Hiliter.UFileHiliter, UIOUtils, UMessageBox, UOpenDialogHelper,
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
    // File name not acceptable: display message aligned over dialog box
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
  Encoding: TEncoding;        // encoding to use for output file
  FileContent: string;        // output file content before encoding
  FileType: TSourceFileType;  // type of source file
begin
  // Set up dialog box
  fSaveDlg.Filter := fSourceFileInfo.FilterString;
  fSaveDlg.FilterIndex := ExtToFilterIndex(
    fSaveDlg.Filter,
    fSourceFileInfo.FileTypeInfo[Preferences.SourceDefaultFileType].Extension,
    1
  );
  fSaveDlg.FileName := fSourceFileInfo.DefaultFileName;
  // Display dialog box and save file if user OKs
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
  FileType: TSourceFileType;  // type of file that has given extension
begin
  FileType := fSourceFileInfo.FileTypeFromExt(Ext);
  Encodings := fSourceFileInfo.FileTypeInfo[FileType].Encodings;
end;

function TSaveSourceMgr.GenerateOutput(const FileType: TSourceFileType):
  TEncodedData;
var
  RawSource: string;      // raw source code
  Hiliter: TFileHiliter;  // object used to highlight source code
begin
  RawSource := GenerateSource(fSaveDlg.CommentStyle, fSaveDlg.TruncateComments);
  // Highlight the raw source as required
  Hiliter := TFileHiliter.Create(
    fSaveDlg.UseSyntaxHiliting and IsHilitingSupported(FileType),
    FileType
  );
  try
    Result := Hiliter.Hilite(RawSource, GetDocTitle);
  finally
    Hiliter.Free;
  end;
end;

procedure TSaveSourceMgr.HiliteQueryHandler(Sender: TObject; const Ext: string;
  var CanHilite: Boolean);
begin
  CanHilite := IsHilitingSupported(fSourceFileInfo.FileTypeFromExt(Ext));
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

function TSaveSourceMgr.IsHilitingSupported(
  const FileType: TSourceFileType): Boolean;
begin
  Result := TFileHiliter.IsHilitingSupported(FileType);
end;

procedure TSaveSourceMgr.PreviewHandler(Sender: TObject);
const
  // Map of source file type to preview document types
  PreviewDocTypeMap: array[TSourceFileType] of TPreviewDocType = (
    dtPlainText, dtPlainText, dtHTML, dtRTF
  );
var
  FileType: TSourceFileType;  // type of source file to preview
begin
  FileType := fSourceFileInfo.FileTypeFromExt(fSaveDlg.SelectedExt);
  // Display preview dialog box. We use save dialog as owner to ensure preview
  // dialog box is aligned over save dialog box
  TPreviewDlg.Execute(
    fSaveDlg,
    GenerateOutput(FileType),
    PreviewDocTypeMap[FileType],
    GetDocTitle
  );
end;

end.

