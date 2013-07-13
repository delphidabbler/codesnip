{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a class that converts snippets from the SWAG database into
 * CodeSnip format and adds them to the database.
}


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
  TSWAGImporter = class(TObject)
  strict private
    type
      TProgressCallback = reference to procedure (
        const SWAGSnippet: TSWAGSnippet);
    var
      fImportList: TList<TSWAGSnippet>;
      fExtraBoilerplate: IActiveText;
    function ExtraBoilerplate: IActiveText;
    function BuildSnippetInfo(const SWAGSnippet: TSWAGSnippet):
      TSnippetEditData;
    procedure ImportSnippet(const SWAGSnippet: TSWAGSnippet);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure IncludeSnippet(const SWAGSnippet: TSWAGSnippet);
    procedure Import(const Callback: TProgressCallback = nil);
    class function MakeValidSnippetName(SWAGSnippetID: Cardinal): string;
    class function SWAGCategoryDesc: string;
  end;


implementation


uses
  SysUtils,
  DB.UCategory,
  DB.UMain,
  DB.USnippetKind,
  UReservedCategories,
  USnippetValidator;


{ TSWAGImporter }

function TSWAGImporter.BuildSnippetInfo(const SWAGSnippet: TSWAGSnippet):
  TSnippetEditData;

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
  SWAGDBURI = 'http://swag.delphidabbler.com/';
  BSD3URI = 'http://opensource.org/licenses/BSD-3-Clause';
var
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
  SnippetName: string;
  SnippetDetails: TSnippetEditData;
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
  Cat: TCategory;
begin
  Cat := Database.Categories.Find(TReservedCategories.SWAGCatID);
  Assert(Assigned(Cat),
    ClassName + '.SWAGCategoryDesc: Can''t find SWAG category');
  Result := Cat.Description;
end;

end.

