{
 * USourceFileOutputMgr.pas
 *
 * Implements class that manages customisation and output of source files.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is USourceFileOutputMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit USourceFileOutputMgr;


interface


uses
  // Project
  UBaseObjects, USaveSourceDlg, USourceFileInfo, USourceGen;


type

  {
  TSourceCodeGenEvent:
    Type of TSourceFileOutputMgr's OnGenerateOutput event used to get source
    code to be output.
      @param Sender [in] TSourceFileOutputMgr instance triggering event.
      @param CommentStyle [in] Style of commenting required in generated source.
      @param RawSourceCode [out] Set to raw source code in event handler.
      @param DocTitle [out] Set to the document's title in event handler.
  }
  TSourceCodeGenEvent = procedure(Sender: TObject;
    const CommentStyle: TCommentStyle;
    out RawSourceCode, DocTitle: string) of object;

  {
  TSourceFileNameCheckEvent:
    Type of TSourceFileOutputMgr's OnCheckFileName event used to check if a
    file name is acceptable to caller.
      @param Sender [in] TSourceFileOutputMgr instance triggering event.
      @param FileName [in] File name to be checked.
      @param NameOK [in/out] Defaults to true. Handler should set false if file
        name not acceptable.
      @param ErrorMessage [in/out] Default to ''. Handler should set to error
        message if NameOK is set false. Ignored if NameOK is true. Default
        message used if left as empty string.
  }
  TSourceFileNameCheckEvent = procedure(Sender: TObject; const FileName: string;
    var NameOK: Boolean; var ErrorMessage: string) of object;

  {
  TSourceFileOutputMgr:
    Class that manages output of source files. Gets information about file to
    be output from user, validates file, enables it to be previewed, and writes
    it to file with optional syntax highlighting. Owner of object must generate
    the source code and provide information about supported file types.
  }
  TSourceFileOutputMgr = class(TObject)
  private
    fSaveDlg: TSaveSourceDlg;
      {Customised save dialog box used to get name of source file, commenting
      style and whether to use syntax highlighting from user}
    fSourceFileInfo: TSourceFileInfo;
      {Object providing information about supported source file types}
    fOnGenerateOutput: TSourceCodeGenEvent;
      {Handler for OnGenerateOutput event}
    fOnCheckFileName: TSourceFileNameCheckEvent;
      {Handler for OnCheckFileName event}
    function GetDlgTile: string;
      {Read accessor for DlgTitle property.
        @return Dialog box title.
      }
    procedure SetDlgTitle(const Value: string);
      {Write accessor for DlgTitle property.
        @param Value [in] New dialog box title.
      }
    function GetDlgHelpKeyword: string;
      {Read accessor for DlgHelpKeyword property.
        @return Help keyword.
      }
    procedure SetDlgHelpKeyword(const Value: string);
      {Write accessor for DlgHelpKeyword property.
        @param Value [in] New help keyword.
      }
    function IsHilitingSupported(const FileType: TSourceFileType): Boolean;
      {Checks if highlighting is supported for a file type.
        @param FileType [in] File type to be checked.
        @return True if file type supports highlighting, False if not.
      }
    procedure HiliteQueryHandler(Sender: TObject; const Ext: string;
      var CanHilite: Boolean);
      {Handles custom save dialog box's OnHiliteQuery event. Determines whether
      syntax highlighting is supported for a file extension.
        @param Sender [in] Not used.
        @param Ext [in] Extension of selected file type.
        @param CanHilite [in/out] Set true if syntax highlighting is enabled and
          false (default) if not.
      }
    procedure EncodingQueryHandler(Sender: TObject; const Ext: string;
      var Encodings: TSourceFileEncodings);
      {Handles custom save dialog box's OnEncodingQuery event. Provides array of
      encoding supported for a file extension.
        @param Sender [in] Not used.
        @param Ext [in] Extension of selected file type.
        @param Encodings [in/out] Receives array of supported encodings.
      }
    procedure PreviewHandler(Sender: TObject);
      {Handles custom save dialog's OnPreview event. Displays source code
      appropriately formatted in preview dialog box.
        @param Sender [in] Not used.
      }
    procedure CanCloseHandler(Sender: TObject; var CanClose: Boolean);
      {Handles custom save dialog's OnCanClose event. Permits dialog to close if
      filename entered in dialog box is acceptable.
        @param Sender [in] Not used.
        @param CanClose [in/out] Set to true to allow dialog to close or false
          to inhibit.
      }
    function GenerateOutput(const FileType: TSourceFileType): string;
      {Generates source code in desired format.
        @param FileType [in] Type of file for which source code is required.
          Determines file format.
        @return Formatted source code.
      }
    function CheckEnteredFileName: Boolean;
      {Checks if file name entered in save dialog box is acceptable. Hands off
      decision about file name to caller by triggering OnCheckFileName event.
      Displays and error message if event handler notifies file name is not
      acceptable.
        @return True if filename is OK, false otherwise.
      }
  public
    constructor Create(const SourceFileInfo: TSourceFileInfo);
      {Class constructor. Sets up object.
        @param SourceFileInfo [in] Information about types of source file that
          may be saved.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure Execute;
      {Gets information about source file to be generated from user then outputs
      the source file.
      }
    property DlgTitle: string
      read GetDlgTile write SetDlgTitle;
      {Title of save dialog box}
    property DlgHelpKeyword: string
      read GetDlgHelpKeyword write SetDlgHelpKeyword;
      {Help keyword identifying topic accessed when user clicks help button in
      save dialog box}
    property OnGenerateOutput: TSourceCodeGenEvent
      read fOnGenerateOutput write fOnGenerateOutput;
      {Event triggered when this object needs to output source code. Caller must
      handle this event to generate the source code}
    property OnCheckFileName: TSourceFileNameCheckEvent
      read fOnCheckFileName write fOnCheckFileName;
      {Event used to enable caller to check if entered file name is valid}
  end;

  TSaveSourceMgr = class abstract(TNoPublicConstructObject)
  strict private
    var
      fSourceFileInfo: TSourceFileInfo;
      fOutputMgr: TSourceFileOutputMgr;
    procedure SourceGenHandler(Sender: TObject;
      const CommentStyle: TCommentStyle; out RawSourceCode, DocTitle: string);
      {Handles output manager's OnGenerateOutput event by generating source code
      of unit in required comment style.
      @param Sender [in] Not used.
      @param CommentStyle [in] Style of commenting to be used in source code.
      @param SourceCode [out] Receives generated source code.
      @param DocTitle [out] Receives document title.
    }
    procedure CheckFileNameHandler(Sender: TObject; const FileName: string;
      var NameOK: Boolean; var ErrorMessage: string);
      {Handler of output manager's OnCheckFileName event. Checks if file name is
      suitable for use as basis of a unit name. If so unit name is recorded.
        @param Sender [in] Not used.
        @param FileName [in] File name to be checked.
        @param NameOK [in/out] Defaults to true. Set to false if file name fails
          check, i.e. is not valid as a unit name.
        @param ErrorMessage [in/out] Default to ''. Set to error message if
          NameOK is set false.
      }
  strict protected
    constructor InternalCreate;
    procedure DoExecute;
    function GetFileTypeDesc(const FileType: TSourceFileType): string;
      virtual; abstract;
    function GetDefaultFileName: string; virtual; abstract;
    function GetDlgTitle: string; virtual; abstract;
    function GetDlgHelpKeyword: string; virtual; abstract;
    procedure GenerateSource(const CommentStyle: TCommentStyle;
      out RawSourceCode, DocTitle: string); virtual; abstract;
    procedure CheckFileName(const FileName: string; out NameOK: Boolean;
      out ErrorMessage: string); virtual; abstract;
  public
    destructor Destroy; override;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  FmPreviewDlg, Hiliter.UFileHiliter, UEncodings, UIOUtils, UMessageBox,
  UOpenDialogHelper, UPreferences, UUtils;


{ TSourceFileOutputMgr }

procedure TSourceFileOutputMgr.CanCloseHandler(Sender: TObject;
  var CanClose: Boolean);
  {Handles custom save dialog's OnCanClose event. Permits dialog to close if
  filename entered in dialog box is acceptable.
    @param Sender [in] Not used.
    @param CanClose [in/out] Set to true to allow dialog to close or false to
      inhibit.
  }
begin
  CanClose := CheckEnteredFileName;
end;

function TSourceFileOutputMgr.CheckEnteredFileName: Boolean;
  {Checks if file name entered in save dialog box is acceptable. Hands off
  decision about file name to caller by triggering OnCheckFileName event.
  Displays and error message if event handler notifies file name is not
  acceptable.
    @return True if filename is OK, false otherwise.
  }
var
  ErrMsg: string;   // error message displayed if name not OK
resourcestring
  sDefaultErrMsg = 'Invalid file name'; // default error message
begin
  // Assume file name is OK
  Result := True;
  ErrMsg := '';
  // Ask caller about file name via event
  if Assigned(fOnCheckFileName) then
    fOnCheckFileName(Self, fSaveDlg.FileName, Result, ErrMsg);
  if not Result then
  begin
    // File name not acceptable: display message aligned over dialog box
    if ErrMsg = '' then
      ErrMsg := sDefaultErrMsg;
    TMessageBox.Error(fSaveDlg, ErrMsg);
  end;
end;

constructor TSourceFileOutputMgr.Create(const SourceFileInfo: TSourceFileInfo);
  {Class constructor. Sets up object.
    @param SourceFileInfo [in] Information about types of source file that may
      be saved.
  }
begin
  Assert(Assigned(SourceFileInfo),
    ClassName + '.Create: SourceFileInfo is nil');
  inherited Create;
  // Create locally owned source file info object
  fSourceFileInfo := SourceFileInfo;
  // Create and initialize custom save dialog box
  fSaveDlg := TSaveSourceDlg.Create(nil);
  fSaveDlg.CommentStyle := Preferences.SourceCommentStyle;
  fSaveDlg.UseSyntaxHiliting := Preferences.SourceSyntaxHilited;
  fSaveDlg.OnPreview := PreviewHandler;
  fSaveDlg.OnHiliteQuery := HiliteQueryHandler;
  fSaveDlg.OnEncodingQuery := EncodingQueryHandler;
  fSaveDlg.OnCanClose := CanCloseHandler;
end;

destructor TSourceFileOutputMgr.Destroy;
  {Class destructor. Tears down object.
  }
begin
  fSaveDlg.Free;
  inherited;
end;

procedure TSourceFileOutputMgr.EncodingQueryHandler(Sender: TObject;
  const Ext: string; var Encodings: TSourceFileEncodings);
  {Handles custom save dialog box's OnEncodingQuery event. Provides array of
  encoding supported for a file extension.
    @param Sender [in] Not used.
    @param Ext [in] Extension of selected file type.
    @param Encodings [in/out] Receives array of supported encodings.
  }
var
  FileType: TSourceFileType;  // type of file that has given extension
begin
  FileType := fSourceFileInfo.FileTypeFromExt(Ext);
  Encodings := fSourceFileInfo.FileTypeInfo[FileType].Encodings;
end;

procedure TSourceFileOutputMgr.Execute;
  {Gets information about source file to be generated from user then outputs
  the source file.
  }
var
  Encoding: TEncoding;
  FileContent: string;
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
    FileContent := GenerateOutput(
      fSourceFileInfo.FileTypeFromExt(ExtractFileExt(fSaveDlg.FileName))
    );
    Encoding := TEncodingHelper.GetEncoding(fSaveDlg.SelectedEncoding);
    try
      TFileIO.WriteAllText(fSaveDlg.FileName, FileContent, Encoding, True);
    finally
      TEncodingHelper.FreeEncoding(Encoding);
    end;
  end;
end;

function TSourceFileOutputMgr.GenerateOutput(
  const FileType: TSourceFileType): string;
  {Generates optionally syntax highlighted source code. Requires that caller
  generates the raw source code by handling the OnGenerateOutput event.
    @param FileType [in] Type of file for which source code is required.
      Determines file format.
    @return Optionally highlighted source code.
  }
var
  RawSource: string;      // raw source code
  DocTitle: string;       // document title
  Hiliter: TFileHiliter;  // object used to highlight source code
  Ext: string;            // extension of selected file
begin
  if Assigned(fOnGenerateOutput) then
  begin
    // Get raw source code from caller
    fOnGenerateOutput(Self, fSaveDlg.CommentStyle, RawSource, DocTitle);
    // Highlight the raw source as required
    Ext := ExtractFileExt(fSaveDlg.FileName);
    Hiliter := TFileHiliter.Create(
      fSaveDlg.UseSyntaxHiliting and IsHilitingSupported(FileType),
      FileType
    );
    try
      Result := Hiliter.Hilite(RawSource, DocTitle);
    finally
      FreeAndNil(Hiliter);
    end;
  end;
end;

function TSourceFileOutputMgr.GetDlgHelpKeyword: string;
  {Read accessor for DlgHelpKeyword property.
    @return Help keyword.
  }
begin
  Result := fSaveDlg.HelpKeyword;
end;

function TSourceFileOutputMgr.GetDlgTile: string;
  {Read accessor for DlgTitle property.
    @return Dialog box title.
  }
begin
  Result := fSaveDlg.Title;
end;

procedure TSourceFileOutputMgr.HiliteQueryHandler(Sender: TObject;
  const Ext: string; var CanHilite: Boolean);
  {Handles custom save dialog box's OnHiliteQuery event. Determines whether
  syntax highlighting is supported for a file extension.
    @param Sender [in] Not used.
    @param Ext [in] Extension of selected file type.
    @param CanHilite [in/out] Set true if syntax highlighting is enabled and
      false (default) if not.
  }
begin
  CanHilite := IsHilitingSupported(fSourceFileInfo.FileTypeFromExt(Ext));
end;

function TSourceFileOutputMgr.IsHilitingSupported(
  const FileType: TSourceFileType): Boolean;
  {Checks if highlighting is supported for a file type.
    @param FileType [in] File type to be checked.
    @return True if file type supports highlighting, False if not.
  }
begin
  Result := TFileHiliter.IsHilitingSupported(FileType);
end;

procedure TSourceFileOutputMgr.PreviewHandler(Sender: TObject);
  {Handles custom save dialog's OnPreview event. Displays source code
  appropriately formatted in preview dialog box.
    @param Sender [in] Not used.
  }
begin
  // Display preview dialog box. We use save dialog as owner to ensure preview
  // dialog box is aligned over save dialog box
  TPreviewDlg.Execute(
    fSaveDlg,
    GenerateOutput(fSourceFileInfo.FileTypeFromExt(fSaveDlg.SelectedExt))
  );
end;

procedure TSourceFileOutputMgr.SetDlgHelpKeyword(const Value: string);
  {Write accessor for DlgHelpKeyword property.
    @param Value [in] New help keyword.
  }
begin
  fSaveDlg.HelpKeyword := Value;
end;

procedure TSourceFileOutputMgr.SetDlgTitle(const Value: string);
  {Write accessor for DlgTitle property.
    @param Value [in] New dialog box title.
  }
begin
  fSaveDlg.Title := Value;
end;

{ TSaveSourceMgr }

procedure TSaveSourceMgr.CheckFileNameHandler(Sender: TObject;
  const FileName: string; var NameOK: Boolean; var ErrorMessage: string);
begin
  CheckFileName(FileName, NameOK, ErrorMessage);
end;

destructor TSaveSourceMgr.Destroy;
begin
  fOutputMgr.Free;
  fSourceFileInfo.Free;
  inherited;
end;

procedure TSaveSourceMgr.DoExecute;
begin
  fOutputMgr.Execute;
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

  fOutputMgr := TSourceFileOutputMgr.Create(fSourceFileInfo);
  fOutputMgr.OnGenerateOutput := SourceGenHandler;
  fOutputMgr.OnCheckFileName := CheckFileNameHandler;
  fOutputMgr.DlgTitle := GetDlgTitle;
  fOutputMgr.DlgHelpKeyword := GetDlgHelpKeyword;
end;

procedure TSaveSourceMgr.SourceGenHandler(Sender: TObject;
  const CommentStyle: TCommentStyle; out RawSourceCode, DocTitle: string);
begin
  GenerateSource(CommentStyle, RawSourceCode, DocTitle);
end;

end.

