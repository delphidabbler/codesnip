﻿{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2023, Peter Johnson (gravatar.com/delphidabbler).
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
  DB.UCollections,
  DB.USnippet,
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
      ///  <summary>List of collections that have contributed snippets to the
      ///  source code being generated.</summary>
      fCollections: TList<TCollection>;
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
  DB.UMetaData,
  UAppInfo,
  UUtils;


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
  sMainGenerator = 'This unit snippet was generated by %0:s %1:s on %2:s.';
  sCollection = 'The code was sourced from the %s collection.';
  sCollectionList = 'The code was sourced from the following collections:';
  sCollectionCredit = 'Collection "%0:s" is licensed under the %1:s.';
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
          generator code in USnippetSourceGen - extract common code.}

  function CreditsLine(const ACollection: TCollection): string;
  var
    DBMetaData: IDBMetaData;
  begin
    DBMetaData := TMetaDataFactory.CreateInstance(ACollection.Storage);
    Result := '';
    if mdcLicense in DBMetaData.GetCapabilities then
    begin
      Result := Result + DBMetaData.GetLicenseInfo.NameWithURL + '.';
      if (mdcCopyright in DBMetaData.GetCapabilities) then
        Result := Result + ' ' + DBMetaData.GetCopyrightInfo.ToString + '.';
    end;
  end;

var
  Collection: TCollection;
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
  if fCollections.Count = 1 then
    Result.Add(Format(sCollection, [fCollections[0].Name]))
  else
  begin
    Result.Add(sCollectionList);
    for Collection in fCollections do
    begin
      Result.Add('  - ' + Collection.Name);
    end;
  end;

  for Collection in fCollections do
  begin
    Credits := CreditsLine(Collection);
    if Credits <> '' then
    begin
      Result.Add('');
      Result.Add(Format(sCollectionCredit, [Collection.Name, Credits]));
    end;
  end;

end;

destructor TSaveUnitMgr.Destroy;
begin
  fCollections.Free;
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
    sTextDesc, sPascalDesc, sHTMLDesc, sRTFDesc
  );
begin
  Result := Descriptions[FileType];
end;

constructor TSaveUnitMgr.InternalCreate(const Snips: TSnippetList);
var
  Snippet: TSnippet;  // references each snippet in list
  Collection: TCollection;
begin
  Assert(Assigned(Snips), ClassName + '.InternalCreate: Snips is nil');
  inherited InternalCreate;

  fCollections := TList<TCollection>.Create(TCollection.TComparer.Create);

  // Create source generator and initialize it with required snippets
  fSourceGen := TSourceGen.Create;
  fSourceGen.IncludeSnippets(Snips);

  // Count the number of collections containing snippet in the list
  for Snippet in Snips do
  begin
    Collection := TCollections.Instance.GetCollection(Snippet.CollectionID);
    if not fCollections.Contains(Collection) then
      fCollections.Add(Collection);
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

