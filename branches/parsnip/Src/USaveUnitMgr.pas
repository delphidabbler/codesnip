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
 * Defines a class that manages generation, previewing and saving of a pascal
 * unit.
}


unit USaveUnitMgr;


interface


uses
  // Project
  CS.Database.Types,
  CS.SourceCode.Pascal.SourceGen,
  UIStringList,
  USourceFileInfo,
  USaveSourceMgr;


type
  ///  <summary>
  ///  Manages generation, previewing and saving of a Pascal unit to disk.
  ///  </summary>
  ///  <remarks>
  ///  Generated file can be a valid Pascal unit, a plain text file, an HTML
  ///  file or a RTF file. The last two file types can optionally be syntax
  ///  highlighted.
  ///  </remarks>
  TSaveUnitMgr = class(TSaveSourceMgr)
  strict private
    var
      ///  <summary>Used to generate source code unit.</summary>
      fSourceGen: TPascalSourceGen;
      ///  <summary>Name of generated unit.</summary>
      ///  <remarks>If empty string a default name is used.</remarks>
      fUnitName: string;
    ///  <summary>Gets name of unit to be used in generated code.</summary>
    function UnitName: string;
    ///  <summary>Creates a string list containing comments to be written to
    ///  head of unit.</summary>
    function CreateHeaderComments: IStringList;
  strict protected
    ///  <summary>Object constuctor. Sets up object to save a unit containing
    ///  all snippets in given list.</summary>
    constructor InternalCreate(Snips: ISnippetIDList);
    ///  <summary>Gets description of given source code file type.</summary>
    function GetFileTypeDesc(const FileType: TSourceOutputFileType): string;
      override;
    ///  <summary>Gets default file name to display in dialog box.</summary>
    function GetDefaultFileName: string; override;
    ///  <summary>Gets dialog box title.</summary>
    function GetDlgTitle: string; override;
    ///  <summary>Get dialog box's help keyword.</summary>
    function GetDlgHelpKeyword: string; override;
    ///  <summary>Gets title to be used for source document.</summary>
    function GetDocTitle: string; override;
    ///  <summary>Generates raw, un-highlighted, source code.</summary>
    ///  <param name="CommentStyle">TPascalCommentStyle [in] Style of commenting
    ///  to be used in source code.</param>
    ///  <param name="TruncateComments">Boolean [in] Indicates whether multi
    ///  paragraph comments are to be truncated to first paragraph.</param>
    ///  <returns>String containing generated source code.</returns>
    function GenerateSource(const CommentStyle: TPascalCommentStyle;
      const TruncateComments: Boolean): string;
      override;
    ///  <summary>Checks if a file name is valid for the kind of file being
    ///  saved.</summary>
    ///  <param name="FileName">string [in] Name of file to check.</param>
    ///  <param name="NameOK">Boolean [out] Set True if file name OK, False if
    ///  not.</param>
    ///  <param name="ErrorMessage">string [out] Error message describing
    ///  problem with invalid file name. Undefined if NameOK is True.</param>
    procedure CheckFileName(const FileName: string; out NameOK: Boolean;
      out ErrorMessage: string); override;
  public
    ///  <summary>Object destructor. Tears down object.</summary>
    destructor Destroy; override;
    ///  <summary>Creates and outputs a Pascal unit file containing specified
    ///  snippets with name and format speficied by user.</summary>
    ///  <param name="Snips">ISnippetIDList [in] IDs of snippets to include in
    ///  unit.</param>
    class procedure Execute(Snips: ISnippetIDList);
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  CS.Utils.Dates,
  UAppInfo,
  Web.UInfo;


resourcestring
  // Dialog box title
  sSaveDlgTitle = 'Save Unit';
  // File filter strings
  sHTMLDesc = 'HTML file';
  sRTFDesc = 'Rich text file';
  sPascalDesc = 'Pascal unit';
  sTextDesc = 'Plain text file';
  // Default file / unit name
  sDefUnitName = 'Snippets';
  // Error message
  sErrorMsg = 'Filename is not valid for a Pascal unit';
  // Unit header comments
  sLicense = 'This unit is copyright � %0:s by the Code Snippets Database '
    + 'Contributors (http://snippets.delphidabbler.com/contributors) and '
    + 'is licensed under the MIT license '
    + '(http://opensource.org/licenses/MIT).';
  sMainDescription = 'The unit was generated automatically and incorporates a '
    + 'selection of source code taken from the Code Snippets Database at %0:s.';
  sGenerated = 'Generated on : %0:s.';
  sGenerator = 'Generated by : %0:s %1:s.';
  sAdvert = 'The latest version of %0:s is available from the %0:s website '
    + 'at %1:s.';
  sUserDescription = 'This unit was generated automatically.';
  // Output document title
  sDocTitle = 'Pascal unit generated by %s';


{ TSaveUnitMgr }

procedure TSaveUnitMgr.CheckFileName(const FileName: string;
  out NameOK: Boolean; out ErrorMessage: string);
begin
  NameOK := TPascalSourceGen.IsFileNameValidUnitName(FileName);
  if NameOK then
    fUnitName := TPascalSourceGen.UnitNameFromFileName(FileName)
  else
  begin
    fUnitName := '';
    ErrorMessage := sErrorMsg
  end;
end;

function TSaveUnitMgr.CreateHeaderComments: IStringList;
begin
  Result := TIStringList.Create;
  Result.Add(sUserDescription);
  Result.Add('');
  Result.Add(Format(sGenerated, [TUTCDateTime.Now.ToRFC1123String]));
  Result.Add(
    Format(
      sGenerator, [TAppInfo.FullProgramName, TAppInfo.ProgramReleaseInfo]
    )
  );
end;

destructor TSaveUnitMgr.Destroy;
begin
  fSourceGen.Free;
  inherited;
end;

class procedure TSaveUnitMgr.Execute(Snips: ISnippetIDList);
begin
  with InternalCreate(Snips) do
    try
      DoExecute;
    finally
      Free;
    end;
end;

function TSaveUnitMgr.GenerateSource(const CommentStyle: TPascalCommentStyle;
  const TruncateComments: Boolean): string;
begin
  Result := fSourceGen.UnitAsString(
    UnitName, CommentStyle, TruncateComments, CreateHeaderComments
  );
end;

function TSaveUnitMgr.GetDefaultFileName: string;
begin
  Result := sDefUnitName;
end;

function TSaveUnitMgr.GetDlgHelpKeyword: string;
begin
  Result := 'SaveUnitDlg';
end;

function TSaveUnitMgr.GetDlgTitle: string;
begin
  Result := sSaveDlgTitle;
end;

function TSaveUnitMgr.GetDocTitle: string;
begin
  Result := Format(sDocTitle, [TAppInfo.ProgramName]);
end;

function TSaveUnitMgr.GetFileTypeDesc(const FileType: TSourceOutputFileType):
  string;
const
  Descriptions: array[TSourceOutputFileType] of string = (
    sTextDesc, sPascalDesc, sHTMLDesc, sRTFDesc
  );
begin
  Result := Descriptions[FileType];
end;

constructor TSaveUnitMgr.InternalCreate(Snips: ISnippetIDList);
begin
  Assert(Assigned(Snips), ClassName + '.InternalCreate: Snips is nil');
  inherited InternalCreate;
  // Create source generator and initialize it with required snippets
  fSourceGen := TPascalSourceGen.Create;
  fSourceGen.IncludeSnippets(Snips);
end;

function TSaveUnitMgr.UnitName: string;
begin
  // If we have valid unit name based on file, use it, otherwise use default
  if fUnitName <> '' then
    Result := fUnitName
  else
    Result := sDefUnitName;
end;

end.

