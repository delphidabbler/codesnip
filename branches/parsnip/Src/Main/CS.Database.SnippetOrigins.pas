{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides a class that encapsulates information about a snippet's links to
 * other snippets, along with a null class that represents the absence of such a
 * link.
}


unit CS.Database.SnippetOrigins;

interface

uses
  CS.Database.Types,
  CS.Utils.Dates;

type
  // TODO: May need to be in a separate Synch Space unit, or in Types
  ///  <summary>Defines constants that identify different synch spaces.
  ///  </summary>
  TSnippetSynchSpaceIDs = record
  public
    const
      ///  <summary>Synch space used for snippets loaded directly from legacy
      ///  database.</summary>
      LegacyDB: TGUID = '{04AE1B53-6D60-48FA-B81F-E8CF15222F6D}';
      ///  <summary>Synch space used for snippets imported from files exported
      ///  from CodeSnip.</summary>
      Imports: TGUID = '{F2FBD770-EDD9-4A08-96DB-1621EED7C933}';
      ///  <summary>Synch space used for snippets imported from SWAG.</summary>
      SWAG: TGUID = '{6F5A3861-F322-47E3-9897-83B2669C144E}';
  end;

  TSnippetLinkInfo = class(TInterfacedObject, ISnippetLinkInfo)
  strict private
    var
      fSynchSpaceID: TGUID;
      fLinkedSnippetID: TSnippetID;
      fModified: TUTCDateTime;
  public
    constructor Create(const ASynchSpaceID: TGUID;
      const ALinkedSnippetID: TSnippetID; const AModified: TUTCDateTime);
      overload;
    constructor Create(Src: ISnippetLinkInfo); overload;
    function IsLinked: Boolean;
    function GetSynchSpaceID: TGUID;
    function GetLinkedSnippetID: TSnippetID;
    function GetModified: TUTCDateTime;
  end;

  TNullSnippetLinkInfo = class(TInterfacedObject, ISnippetLinkInfo)
  public
    function IsLinked: Boolean;
    function GetSynchSpaceID: TGUID;
    function GetLinkedSnippetID: TSnippetID;
    function GetModified: TUTCDateTime;
  end;

implementation

uses
  SysUtils;

{ TSnippetLinkInfo }

constructor TSnippetLinkInfo.Create(const ASynchSpaceID: TGUID;
  const ALinkedSnippetID: TSnippetID; const AModified: TUTCDateTime);
begin
  inherited Create;
  fSynchSpaceID := ASynchSpaceID;
  fLinkedSnippetID := ALinkedSnippetID;
  fModified := AModified;
end;

constructor TSnippetLinkInfo.Create(Src: ISnippetLinkInfo);
begin
  Create(Src.SynchSpaceID, Src.LinkedSnippetID, Src.Modified);
end;

function TSnippetLinkInfo.GetLinkedSnippetID: TSnippetID;
begin
  Result := fLinkedSnippetID;
end;

function TSnippetLinkInfo.GetModified: TUTCDateTime;
begin
  Result := fModified;
end;

function TSnippetLinkInfo.GetSynchSpaceID: TGUID;
begin
  Result := fSynchSpaceID;
end;

function TSnippetLinkInfo.IsLinked: Boolean;
begin
  Result := True;
end;

{ TNullSnippetLinkInfo }

function TNullSnippetLinkInfo.GetLinkedSnippetID: TSnippetID;
begin
  raise ENotSupportedException.Create(
    'GetLinkedSnippetID is not implemented in ' + ClassName
  );
end;

function TNullSnippetLinkInfo.GetModified: TUTCDateTime;
begin
  raise ENotImplemented.Create(
    'GetModified is not implemented in ' + ClassName
  );
end;

function TNullSnippetLinkInfo.GetSynchSpaceID: TGUID;
begin
  raise ENotImplemented.Create(
    'GetSynchSpaceID is not implemented in ' + ClassName
  );
end;

function TNullSnippetLinkInfo.IsLinked: Boolean;
begin
  Result := False;
end;

end.

