{* USaveUnitMgr.pas
 *
 * Defines a class that manages generation, previewing and saving of a pascal
 * unit.
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
 * The Original Code is USaveUnitMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit USaveUnitMgr;


interface


uses
  // Project
  DB.USnippet, UIStringList, USourceFileInfo, USaveSourceMgr, USourceGen;


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
      fSourceGen: TSourceGen;
      ///  <summary>Name of generated unit.</summary>
      ///  <remarks>If empty string a default name is used.</remarks>
      fUnitName: string;
      ///  <summary>Flag true if unit contains at least one snippet from main
      ///  database, False only if unit is completely user defined.</summary>
      fContainsMainDBSnippets: Boolean;
    ///  <summary>Gets name of unit to be used in generated code.</summary>
    function UnitName: string;
    ///  <summary>Creates a string list containing comments to be written to
    ///  head of unit.</summary>
    function CreateHeaderComments: IStringList;
  strict protected
    ///  <summary>Object constuctor. Sets up object to save a unit containing
    ///  all snippets in given list.</summary>
    constructor InternalCreate(const Snips: TSnippetList);
    ///  <summary>Gets description of given source code file type.</summary>
    function GetFileTypeDesc(const FileType: TSourceFileType): string; override;
    ///  <summary>Gets default file name to display in dialog box.</summary>
    function GetDefaultFileName: string; override;
    ///  <summary>Gets dialog box title.</summary>
    function GetDlgTitle: string; override;
    ///  <summary>Get dialog box's help keyword.</summary>
    function GetDlgHelpKeyword: string; override;
    ///  <summary>Gets title to be used for source document.</summary>
    function GetDocTitle: string; override;
    ///  <summary>Generates raw, un-highlighted, source code.</summary>
    ///  <param name="CommentStyle">TCommentStyle [in] Style of commenting to be
    ///  used in source code.</param>
    ///  <returns>String containing generated source code.</returns>
    function GenerateSource(const CommentStyle: TCommentStyle): string;
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
    ///  <param name="Snips">TSnippetList [in] List of snippets to include in
    ///  unit.</param>
    class procedure Execute(const Snips: TSnippetList);
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UAppInfo, UUtils, Web.UInfo;


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
  sLicense = 'This unit may be freely distributed and used on the condition '
    + 'that this comment is not removed from the unit.';
  sMainDescription = 'The unit was generated automatically from a '
    + 'selection of source code taken from the Code Snippets Database at %0:s.';
  sDisclaimer = 'The source code contained in this unit is made available on '
    + 'an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or '
    + 'implied. The code is used entirely at your own risk.';
  sGenerated = 'Generated on : %0:s.';
  sGenerator = 'Generated by : %0:s %1:s.';
  sAdvert = 'The latest version of %0:s is available from the %1:s website '
    + 'at %2:s.';
  sUserDescription = 'This unit was generated automatically.';
  // Output document title
  sDocTitle = 'Pascal unit generated by %s';


{ TSaveUnitMgr }

procedure TSaveUnitMgr.CheckFileName(const FileName: string;
  out NameOK: Boolean; out ErrorMessage: string);
begin
  NameOK := TSourceGen.IsFileNameValidUnitName(FileName);
  if NameOK then
    fUnitName := TSourceGen.UnitNameFromFileName(FileName)
  else
  begin
    fUnitName := '';
    ErrorMessage := sErrorMsg
  end;
end;

function TSaveUnitMgr.CreateHeaderComments: IStringList;
begin
  Result := TIStringList.Create;
  if fContainsMainDBSnippets then
  begin
    // Result used for units that contain at snippet(s) from main database
    Result.Add(sLicense);
    Result.Add('');
    Result.Add(Format(sMainDescription, [TWebInfo.DatabaseURL]));
    Result.Add('');
    Result.Add(sDisclaimer);
    Result.Add('');
    Result.Add(Format(sGenerated, [DateStamp]));
    Result.Add(
      Format(
        sGenerator, [TAppInfo.FullProgramName, TAppInfo.ProgramReleaseInfo]
      )
    );
    Result.Add('');
    Result.Add(
      Format(
        sAdvert,
        [TAppInfo.ProgramName, TAppInfo.CompanyName, TWebInfo.ProgramHomeURL]
      )
    );
  end
  else
  begin
    // Result used for units that contain only user defined snippets
    Result.Add(sUserDescription);
    Result.Add('');
    Result.Add(Format(sGenerated, [DateStamp]));
    Result.Add(
      Format(
        sGenerator, [TAppInfo.FullProgramName, TAppInfo.ProgramReleaseInfo]
      )
    );
  end;
end;

destructor TSaveUnitMgr.Destroy;
begin
  fSourceGen.Free;
  inherited;
end;

class procedure TSaveUnitMgr.Execute(const Snips: TSnippetList);
begin
  with InternalCreate(Snips) do
    try
      DoExecute;
    finally
      Free;
    end;
end;

function TSaveUnitMgr.GenerateSource(const CommentStyle: TCommentStyle): string;
begin
  Result := fSourceGen.UnitAsString(
    UnitName, CommentStyle, CreateHeaderComments
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

function TSaveUnitMgr.GetFileTypeDesc(const FileType: TSourceFileType): string;
const
  Descriptions: array[TSourceFileType] of string = (
    sTextDesc, sPascalDesc, sHTMLDesc, sRTFDesc
  );
begin
  Result := Descriptions[FileType];
end;

constructor TSaveUnitMgr.InternalCreate(const Snips: TSnippetList);
var
  Snippet: TSnippet;  // references each snippet in list
begin
  Assert(Assigned(Snips), ClassName + '.InternalCreate: Snips is nil');
  inherited InternalCreate;

  // Create source generator and initialize it with required snippets
  fSourceGen := TSourceGen.Create;
  fSourceGen.IncludeSnippets(Snips);

  // Determine if snippet list contains at least one snippet from main database
  fContainsMainDBSnippets := False;
  for Snippet in Snips do
  begin
    if not Snippet.UserDefined then
    begin
      fContainsMainDBSnippets := True;
      Break;
    end;
  end;
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

