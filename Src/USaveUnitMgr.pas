﻿{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2025, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines a class that manages generation, previewing and saving of a pascal
 * unit.
}


unit USaveUnitMgr;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  DB.Snippets,
  DB.Vaults,
  UIStringList,
  USourceFileInfo,
  USaveSourceMgr,
  USourceGen;


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
      ///  <summary>List of vaults that have contributed snippets to the source
      ///  code being generated.</summary>
      fVaults: TList<TVault>;
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
    ///  <param name="TruncateComments">Boolean [in] Indicates whether multi
    ///  paragraph comments are to be truncated to first paragraph.</param>
    ///  <returns>String containing generated source code.</returns>
    function GenerateSource(const CommentStyle: TCommentStyle;
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
    ///  <param name="Snips">TSnippetList [in] List of snippets to include in
    ///  unit.</param>
    class procedure Execute(const Snips: TSnippetList);
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  DB.DataFormats,
  DB.MetaData,
  UAppInfo,
  UStrUtils,
  UUtils;


resourcestring
  // Dialog box title
  sSaveDlgTitle = 'Save Unit';
  // File filter strings
  sHTML5Desc = 'HTML 5 file';
  sXHTMLDesc = 'XHTML file';
  sRTFDesc = 'Rich text file';
  sPascalDesc = 'Pascal unit';
  sTextDesc = 'Plain text file';
  // Default file / unit name
  sDefUnitName = 'Snippets';
  // Error message
  sErrorMsg = 'Filename is not valid for a Pascal unit';
  // Unit header comments
  sMainGenerator = 'This unit snippet was generated by %0:s %1:s on %2:s.';
  sVault = 'The code was sourced from the %s vault.';
  sVaultList = 'The code was sourced from the following vaults:';
  sVaultCredit = 'Vault "%0:s" is licensed under the %1:s';
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

  {TODO -cRefactoring: This code has a lot in common with header comment
          generator code in USnippetSourceGen and in TSnippetDoc.VaultInfo
          - extract common code.}

  function CreditsLine(const AVault: TVault): string;
  var
    MetaData: TMetaData;
  begin
    MetaData := AVault.MetaData;
    Result := '';
    if TMetaDataCap.License in MetaData.Capabilities then
    begin
      Result := Result + StrMakeSentence(MetaData.LicenseInfo.NameWithURL);
    end;
    if TMetaDataCap.Copyright in MetaData.Capabilities then
    begin
      if not StrIsEmpty(Result) then
        Result := Result + ' ';
      Result := Result + StrMakeSentence(MetaData.CopyrightInfo.ToString);
    end;
  end;

var
  Vault: TVault;
  Credits: string;
begin
  Result := TIStringList.Create;

  Result.Add(
    Format(
      sMainGenerator,
      [TAppInfo.FullProgramName, TAppInfo.ProgramReleaseInfo, RFC1123DateStamp]
    )
  );

  Result.Add('');
  if fVaults.Count = 1 then
    Result.Add(Format(sVault, [fVaults[0].Name]))
  else
  begin
    Result.Add(sVaultList);
    for Vault in fVaults do
    begin
      Result.Add('  - ' + Vault.Name);
    end;
  end;

  for Vault in fVaults do
  begin
    Credits := CreditsLine(Vault);
    if Credits <> '' then
    begin
      Result.Add('');
      Result.Add(Format(sVaultCredit, [Vault.Name, Credits]));
    end;
  end;

end;

destructor TSaveUnitMgr.Destroy;
begin
  fVaults.Free;
  fSourceGen.Free;
  inherited;
end;

class procedure TSaveUnitMgr.Execute(const Snips: TSnippetList);
var
  Instance: TSaveUnitMgr;
begin
  Instance := InternalCreate(Snips);
  try
    Instance.DoExecute;
  finally
    Instance.Free;
  end;
end;

function TSaveUnitMgr.GenerateSource(const CommentStyle: TCommentStyle;
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

function TSaveUnitMgr.GetFileTypeDesc(const FileType: TSourceFileType): string;
const
  Descriptions: array[TSourceFileType] of string = (
    sTextDesc, sPascalDesc, sHTML5Desc, sXHTMLDesc, sRTFDesc
  );
begin
  Result := Descriptions[FileType];
end;

constructor TSaveUnitMgr.InternalCreate(const Snips: TSnippetList);
var
  Snippet: TSnippet;  // references each snippet in list
  Vault: TVault;
begin
  Assert(Assigned(Snips), ClassName + '.InternalCreate: Snips is nil');
  inherited InternalCreate;

  fVaults := TList<TVault>.Create(TVault.TComparer.Create);

  // Create source generator and initialize it with required snippets
  fSourceGen := TSourceGen.Create;
  fSourceGen.IncludeSnippets(Snips);

  // Count the number of vaults containing snippet in the list
  for Snippet in Snips do
  begin
    Vault := TVaults.Instance.GetVault(Snippet.VaultID);
    if not fVaults.Contains(Vault) then
      fVaults.Add(Vault);
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

