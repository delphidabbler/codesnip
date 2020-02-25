{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that converts snippets from the SWAG database into
 * CodeSnip format and adds them to the database.
}

// TODO -cDatabase: check BSD3URI const has valid license info

unit SWAG.UImporter;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  ActiveText.UMain,
  DB.USnippet,
  SWAG.UCommon;


type
  ///  <summary>Class that converts snippets from the SWAG database into
  ///  CodeSnip database format and imports them into the user defined
  ///  database.</summary>
  ///  <remarks>This class creates new user-defined snippets from records that
  ///  define a SWAF snippet. It is for the caller to acquire the required SWAG
  ///  snippets from the SWAG database.</remarks>
  TSWAGImporter = class(TObject)
  strict private
    type
      ///  <summary>Type of callback function used to notify the caller that a
      ///  snippet has been imported.</summary>
      ///  <param name="SWAGSnippet">TSWAGSnippet [in] Data of imported snippet.
      ///  </param>
      TProgressCallback = reference to procedure (
        const SWAGSnippet: TSWAGSnippet);
    var
      ///  <summary>List of SWAG snippets to be imported.</summary>
      fImportList: TList<TSWAGSnippet>;
      ///  <summary>Records the common active text that is included in the Extra
      ///  property of each imported snippet.</summary>
      fExtraBoilerplate: IActiveText;
    ///  <summary>Returns the common active text that will be included in the
    ///  Extra property of each imported snippet.</summary>
    function ExtraBoilerplate: IActiveText;
    ///  <summary>Records the data from the given SWAG snippet into a data
    ///  structure suitable for adding to CodeSnip's user database.</summary>
    function BuildSnippetInfo(const SWAGSnippet: TSWAGSnippet):
      TSnippetEditData;
    ///  <summary>Imports (i.e. adds) the given SWAG snippet into the user
    ///  database.</summary>
    procedure ImportSnippet(const SWAGSnippet: TSWAGSnippet);
  public
    ///  <summary>Constructs new object instance.</summary>
    constructor Create;
    ///  <summary>Destroys object instance.</summary>
    destructor Destroy; override;
    ///  <summary>Resets list of snippets to import.</summary>
    ///  <remarks>After calling this method no snippets are recorded for
    ///  inclusion in the import.</remarks>
    procedure Reset;
    ///  <summary>Records the given SWAG snippet ready for import into the user
    ///  database.</summary>
    procedure IncludeSnippet(const SWAGSnippet: TSWAGSnippet);
    ///  <summary>Imports all the required SWAG snippets into the user database.
    ///  </summary>
    ///  <param name="Callback">TProgressCallback [in] Optional callback to be
    ///  called after each SWAG snippet is imported.</param>
    ///  <remarks>The snippets that are imported are those that have been
    ///  recorded by calling IncludeSnippet.</remarks>
    procedure Import(const Callback: TProgressCallback = nil);
    ///  <summary>Creates and returns a valid snippet name, based on the given
    ///  SWAG snippet ID, that is unique in the user database.</summary>
    class function MakeValidSnippetName(SWAGSnippetID: Cardinal): string;
    ///  <summary>Description of the category in the user database used for all
    ///  imported SWAG snippets.</summary>
    class function SWAGCategoryDesc: string;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  DB.UCategory,
  DB.UMain,
  DB.USnippetKind,
  UReservedCategories,
  USnippetValidator;


{ TSWAGImporter }

function TSWAGImporter.BuildSnippetInfo(const SWAGSnippet: TSWAGSnippet):
  TSnippetEditData;

  // Constructs and returns the new snippet's description as active text.
  function BuildDescription: IActiveText;
  begin
    Result := TActiveTextFactory.CreateActiveText;
    Result.AddElem(
      TActiveTextFactory.CreateActionElem(ekPara, fsOpen)
    );
    Result.AddElem(TActiveTextFactory.CreateTextElem(SWAGSnippet.Title));
    Result.AddElem(
      TActiveTextFactory.CreateActionElem(ekPara, fsClose)
    );
  end;

  // Constructs and returns the active text to be stored in the new snippet's
  // Extra field.
  function BuildExtra: IActiveText;
  resourcestring
    sAuthor = 'Author(s): %s';
  begin
    Result := TActiveTextFactory.CloneActiveText(ExtraBoilerplate);
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsOpen));
    Result.AddElem(
      TActiveTextFactory.CreateTextElem(Format(sAuthor, [SWAGSnippet.Author]))
    );
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsClose));
  end;

begin
  Result.Init;
  Result.Props.Kind := skFreeform;
  Result.Props.Cat := TReservedCategories.SWAGCatID;
  Result.Props.Desc := BuildDescription;
  Result.Props.SourceCode := SWAGSnippet.SourceCode;
  Result.Props.HiliteSource := not SWAGSnippet.IsDocument;
  Result.Props.DisplayName := SWAGSnippet.Title;
  Result.Props.Extra := BuildExtra;
  // TSnippetEditData.Refs properties can keep default values
end;

constructor TSWAGImporter.Create;
begin
  inherited Create;
  fImportList := TList<TSWAGSnippet>.Create;
end;

destructor TSWAGImporter.Destroy;
begin
  fImportList.Free;
  inherited;
end;

function TSWAGImporter.ExtraBoilerplate: IActiveText;
resourcestring
  sStatementPrefix = 'This snippet was imported from the ';
  sStatementLinkText = 'SWAG Pascal Archive';
  sStatementPostfix = '. ';
  sLicensePrefix = 'Unless stated otherwise this snippet is licensed under '
    + 'the ';
  sLicenseLinkText = 'BSD 3-Clause License';
  sLicensePostfix = '.';
const
  // URLs of web pages referenced from links in boilerplate
  SWAGDBURI = 'https://github.com/delphidabbler/swag';
  BSD3URI = 'http://opensource.org/licenses/BSD-3-Clause';
var
  // Active text attributes for links included in boilerplate
  SWAGDBURIAttr: IActiveTextAttrs;
  BSD3URIAttr: IActiveTextAttrs;
begin
  if not Assigned(fExtraBoilerplate) then
  begin
    SWAGDBURIAttr := TActiveTextFactory.CreateAttrs(
      TActiveTextAttr.Create('href', SWAGDBURI)
    );
    BSD3URIAttr := TActiveTextFactory.CreateAttrs(
      TActiveTextAttr.Create('href', BSD3URI)
    );
    fExtraBoilerplate := TActiveTextFactory.CreateActiveText;
    fExtraBoilerplate.AddElem(
      TActiveTextFactory.CreateActionElem(ekPara, fsOpen)
    );
    fExtraBoilerplate.AddElem(
      TActiveTextFactory.CreateTextElem(sStatementPrefix)
    );
    fExtraBoilerplate.AddElem(
      TActiveTextFactory.CreateActionElem(ekLink, SWAGDBURIAttr, fsOpen)
    );
    fExtraBoilerplate.AddElem(
      TActiveTextFactory.CreateTextElem(sStatementLinkText)
    );
    fExtraBoilerplate.AddElem(
      TActiveTextFactory.CreateActionElem(ekLink, SWAGDBURIAttr, fsClose)
    );
    fExtraBoilerplate.AddElem(
      TActiveTextFactory.CreateTextElem(sStatementPostfix)
    );
    fExtraBoilerplate.AddElem(
      TActiveTextFactory.CreateTextElem(sLicensePrefix)
    );
    fExtraBoilerplate.AddElem(
      TActiveTextFactory.CreateActionElem(ekLink, BSD3URIAttr, fsOpen)
    );
    fExtraBoilerplate.AddElem(
      TActiveTextFactory.CreateTextElem(sLicenseLinkText)
    );
    fExtraBoilerplate.AddElem(
      TActiveTextFactory.CreateActionElem(ekLink, BSD3URIAttr, fsClose)
    );
    fExtraBoilerplate.AddElem(
      TActiveTextFactory.CreateTextElem(sLicensePostfix)
    );
    fExtraBoilerplate.AddElem(
      TActiveTextFactory.CreateActionElem(ekPara, fsClose)
    );
  end;
  Result := fExtraBoilerplate;
end;

procedure TSWAGImporter.Import(const Callback: TProgressCallback);
var
  SWAGSnippet: TSWAGSnippet;
begin
  for SWAGSnippet in fImportList do
  begin
    if Assigned(Callback) then
      Callback(SWAGSnippet);
    ImportSnippet(SWAGSnippet);
  end;
end;

procedure TSWAGImporter.ImportSnippet(const SWAGSnippet: TSWAGSnippet);
var
  SnippetName: string;                // unique name of new snippet
  SnippetDetails: TSnippetEditData;   // data describing new snippet
begin
  SnippetName := MakeValidSnippetName(SWAGSnippet.ID);
  SnippetDetails := BuildSnippetInfo(SWAGSnippet);
  (Database as IDatabaseEdit).AddSnippet(SnippetName, SnippetDetails);
end;

procedure TSWAGImporter.IncludeSnippet(const SWAGSnippet: TSWAGSnippet);
begin
  fImportList.Add(SWAGSnippet);
end;

class function TSWAGImporter.MakeValidSnippetName(SWAGSnippetID: Cardinal):
  string;
var
  Appendix: Integer;
  RootName: string;
begin
  RootName := 'SWAG_' + IntToStr(SWAGSnippetID);
  Assert(IsValidIdent(RootName, False), ClassName
    + '.GetValidSnippetName: RootName is not a valid Pascal identifier');
  Result := RootName;
  Appendix := 0;
  while not TSnippetValidator.ValidateName(Result, True) do
  begin
    Inc(Appendix);
    Result := RootName + '_' + IntToStr(Appendix);
  end;
end;

procedure TSWAGImporter.Reset;
begin
  fImportList.Clear;
end;

class function TSWAGImporter.SWAGCategoryDesc: string;
var
  Cat: TCategory; // reserved SWAG category in code snippets database
begin
  Cat := Database.Categories.Find(TReservedCategories.SWAGCatID);
  Assert(Assigned(Cat),
    ClassName + '.SWAGCategoryDesc: Can''t find SWAG category');
  Result := Cat.Description;
end;

end.

