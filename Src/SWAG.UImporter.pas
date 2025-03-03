{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that converts packets from the SWAG database into CodeSnip
 * format snippets and adds them to the database.
}


unit SWAG.UImporter;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  ActiveText.UMain,
  DB.UCategory,
  DB.UCollections,
  DB.USnippet,
  SWAG.UCommon;


type
  ///  <summary>Class that converts packets from the SWAG database into CodeSnip
  ///  database format snippets and imports them into the user defined
  ///  database.</summary>
  ///  <remarks>This class creates new user-defined snippets from records that
  ///  define a SWAG packet. It is for the caller to acquire the required SWAG
  ///  packets from the SWAG database.</remarks>
  TSWAGImporter = class(TObject)
  strict private
    type
      ///  <summary>Type of callback function used to notify the caller that a
      ///  packet has been imported as new snippet.</summary>
      ///  <param name="SWAGPacket">TSWAGPacket [in] Data of imported packet.
      ///  </param>
      TProgressCallback = reference to procedure (
        const SWAGPacket: TSWAGPacket);
    const
      {TODO -cVault: Let user select or create a category rather than imposing
              this one}
      ///  <summary>ID of category used to import snippets.</summary>
      SWAGCatID = '_swag_';
    var
      ///  <summary>List of SWAG packets to be imported.</summary>
      fImportList: TList<TSWAGPacket>;
      ///  <summary>Records the common active text that is included in the Extra
      ///  property of each imported CodeSnip snippet.</summary>
      fExtraBoilerplate: IActiveText;
    ///  <summary>Returns the common active text that will be included in the
    ///  Extra property of each imported CodeSnip snippet.</summary>
    function ExtraBoilerplate: IActiveText;
    ///  <summary>Records the data from the given SWAG packet into a data
    ///  structure suitable for adding as a snippet in CodeSnip's user database.
    ///  </summary>
    function BuildSnippetInfo(const SWAGPacket: TSWAGPacket):
      TSnippetEditData;
    ///  <summary>Imports (i.e. adds) the given SWAG packet into the user
    ///  database as a CodeSnip format snippet.</summary>
    procedure ImportPacketAsSnippet(const ACollectionID: TVaultID;
      const SWAGPacket: TSWAGPacket);

    class procedure EnsureSWAGCategoryExists;
  public
    ///  <summary>Constructs new object instance.</summary>
    constructor Create;
    ///  <summary>Destroys object instance.</summary>
    destructor Destroy; override;
    ///  <summary>Resets list of packets to import.</summary>
    ///  <remarks>After calling this method no packets are recorded for
    ///  inclusion in the import.</remarks>
    procedure Reset;
    ///  <summary>Records the given SWAG packet ready for import into the user
    ///  database.</summary>
    procedure IncludePacket(const SWAGPacket: TSWAGPacket);
    ///  <summary>Imports all the required SWAG packets into the user database
    ///  as new snippets.</summary>
    ///  <param name="ACollectionID"><c>TVaultID</c> [in] Vault into which
    ///  packets are imported.</param>
    ///  <param name="Callback">TProgressCallback [in] Optional callback to be
    ///  called after each SWAG packet is imported.</param>
    ///  <remarks>The packets that are imported are those that have been
    ///  recorded by calling IncludePacket.</remarks>
    procedure Import(const ACollectionID: TVaultID;
      const Callback: TProgressCallback = nil);
    ///  <summary>Description of the category in the user database used for all
    ///  imported SWAG packets.</summary>
    class function SWAGCategoryDesc: string;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  DB.UMain,
  DB.USnippetKind,
  USnippetValidator;


{ TSWAGImporter }

function TSWAGImporter.BuildSnippetInfo(const SWAGPacket: TSWAGPacket):
  TSnippetEditData;

  // Constructs and returns the new snippet's description as active text.
  function BuildDescription: IActiveText;
  begin
    Result := TActiveTextFactory.CreateActiveText;
    Result.AddElem(
      TActiveTextFactory.CreateActionElem(ekPara, fsOpen)
    );
    Result.AddElem(TActiveTextFactory.CreateTextElem(SWAGPacket.Title));
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
      TActiveTextFactory.CreateTextElem(Format(sAuthor, [SWAGPacket.Author]))
    );
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsClose));
  end;

begin
  Result.Init;
  Result.Props.Kind := skFreeform;
  Result.Props.Cat := SWAGCatID;
  Result.Props.Desc := BuildDescription;
  Result.Props.SourceCode := SWAGPacket.SourceCode;
  Result.Props.HiliteSource := not SWAGPacket.IsDocument;
  Result.Props.DisplayName := SWAGPacket.Title;
  Result.Props.Extra := BuildExtra;
  // TSnippetEditData.Refs properties can keep default values
end;

constructor TSWAGImporter.Create;
begin
  inherited Create;
  fImportList := TList<TSWAGPacket>.Create;
  EnsureSWAGCategoryExists;
end;

destructor TSWAGImporter.Destroy;
begin
  fImportList.Free;
  inherited;
end;

class procedure TSWAGImporter.EnsureSWAGCategoryExists;
resourcestring
  SWAGCatDesc = 'SWAG Imports';
var
  SWAGCatData: TCategoryData;
  SWAGCat: TCategory;
begin
  SWAGCat := Database.Categories.Find(SWAGCatID);
  if not Assigned(SWAGCat) then
  begin
    SWAGCatData.Init;
    SWAGCatData.Desc := SWAGCatDesc;
    (Database as IDatabaseEdit).AddCategory(SWAGCatID, SWAGCatData);
  end;
end;

function TSWAGImporter.ExtraBoilerplate: IActiveText;

  procedure AddText(const Text: string);
  begin
    fExtraBoilerplate.AddElem(
      TActiveTextFactory.CreateTextElem(Text)
    );
  end;

  procedure AddLink(const Text, URI: string);
  var
    HRefAttr: IActiveTextAttrs;
  begin
    HRefAttr := TActiveTextFactory.CreateAttrs(
      TActiveTextAttr.Create('href', URI)
    );
    fExtraBoilerplate.AddElem(
      TActiveTextFactory.CreateActionElem(ekLink, HRefAttr, fsOpen)
    );
    AddText(Text);
    fExtraBoilerplate.AddElem(
      TActiveTextFactory.CreateActionElem(ekLink, HRefAttr, fsClose)
    );
  end;

resourcestring
  sStatementPrefix = 'This snippet was imported from a packet in the ';
  sStatementLinkText = 'SWAG Pascal Code Collection';
  sStatementPostfix = '. ';
  sLicense1 = 'Unless the snippet states otherwise it is deemed to be licensed '
    + 'under either the ';
  sLGPLLinkText = 'GNU Lesser General Public License v2.1';
  sLicense2 = ' (source code) or the ';
  sFDLLinkText = 'GNU Free Documentation License v1.2';
  sLicense3 = ' (other documents), as appropriate. For further information '
    + 'please see the ';
  sSWAGLicenseLinkText = 'SWAG License document';
  sLicense4 = '.';
const
  // URLs of web pages referenced from links in boilerplate
  SWAGProjectURL = 'https://github.com/delphidabbler/swag';
  SWAGLicenseURL = SWAGProjectURL + '/blob/master/LICENSE.md';
  LGPLLicenseURL = 'https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html';
  FDPLicenseURL = 'https://www.gnu.org/licenses/old-licenses/fdl-1.2.html';

begin
  if not Assigned(fExtraBoilerplate) then
  begin
    fExtraBoilerplate := TActiveTextFactory.CreateActiveText;
    // Intro para
    fExtraBoilerplate.AddElem(
      TActiveTextFactory.CreateActionElem(ekPara, fsOpen)
    );
    AddText(sStatementPrefix);
    AddLink(sStatementLinkText, SWAGProjectURL);
    AddText(sStatementPostfix);
    fExtraBoilerplate.AddElem(
      TActiveTextFactory.CreateActionElem(ekPara, fsClose)
    );
    // License para
    fExtraBoilerplate.AddElem(
      TActiveTextFactory.CreateActionElem(ekPara, fsOpen)
    );
    AddText(sLicense1);
    AddLink(sLGPLLinkText, LGPLLicenseURL);
    AddText(sLicense2);
    AddLink(sFDLLinkText, FDPLicenseURL);
    AddText(sLicense3);
    AddLink(sSWAGLicenseLinkText, SWAGLicenseURL);
    AddText(sLicense4);
    fExtraBoilerplate.AddElem(
      TActiveTextFactory.CreateActionElem(ekPara, fsClose)
    );
  end;
  Result := fExtraBoilerplate;
end;

procedure TSWAGImporter.Import(const ACollectionID: TVaultID;
  const Callback: TProgressCallback);
var
  SWAGPacket: TSWAGPacket;
begin
  for SWAGPacket in fImportList do
  begin
    if Assigned(Callback) then
      Callback(SWAGPacket);
    ImportPacketAsSnippet(ACollectionID, SWAGPacket);
  end;
end;

procedure TSWAGImporter.ImportPacketAsSnippet(
  const ACollectionID: TVaultID; const SWAGPacket: TSWAGPacket);
var
  SnippetKey: string;                // unique ID of new snippet
  SnippetDetails: TSnippetEditData;   // data describing new snippet
begin
  SnippetKey := (Database as IDatabaseEdit).GetUniqueSnippetKey(ACollectionID);
  SnippetDetails := BuildSnippetInfo(SWAGPacket);
  (Database as IDatabaseEdit).AddSnippet(
    SnippetKey, ACollectionID, SnippetDetails
  );
end;

procedure TSWAGImporter.IncludePacket(const SWAGPacket: TSWAGPacket);
begin
  fImportList.Add(SWAGPacket);
end;

procedure TSWAGImporter.Reset;
begin
  fImportList.Clear;
end;

class function TSWAGImporter.SWAGCategoryDesc: string;
begin
  EnsureSWAGCategoryExists;
  Result := Database.Categories.Find(SWAGCatID).Description;
end;

end.

