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
 * Provides a class that encapsulates information about a snippet's origins.
 * link.
}


unit CS.Database.SnippetOrigins;

interface

uses
  CS.Database.Types,
  CS.Utils.Dates;

type

  TSnippetOrigin = class(TInterfacedObject, ISnippetOrigin)
  strict private
    var
      fOriginalID: string;
      fSource: TSnippetOriginSource;
      fModified: TUTCDateTime;
  public
    constructor Create(const ASource: TSnippetOriginSource;
      const AOriginalID: string; const AModified: TUTCDateTime); overload;
    constructor Create(Src: ISnippetOrigin); overload;
    function IsLinked: Boolean;
    function GetSource: TSnippetOriginSource;
    function GetOriginalID: string;
    function GetModified: TUTCDateTime;
  end;

  TNullSnippetOrigin = class(TInterfacedObject, ISnippetOrigin)
  public
    function IsLinked: Boolean;
    function GetSource: TSnippetOriginSource;
    function GetOriginalID: string;
    function GetModified: TUTCDateTime;
  end;

implementation

uses
  SysUtils;

{ TSnippetOrigin }

constructor TSnippetOrigin.Create(const ASource: TSnippetOriginSource;
  const AOriginalID: string; const AModified: TUTCDateTime);
begin
  inherited Create;
  fSource := ASource;
  fOriginalID := AOriginalID;
  fModified := AModified;
end;

constructor TSnippetOrigin.Create(Src: ISnippetOrigin);
begin
  Create(Src.Source, Src.OriginalID, Src.Modified);
end;

function TSnippetOrigin.GetModified: TUTCDateTime;
begin
  Result := fModified;
end;

function TSnippetOrigin.GetOriginalID: string;
begin
  Result := fOriginalID;
end;

function TSnippetOrigin.GetSource: TSnippetOriginSource;
begin
  Result := fSource;
end;

function TSnippetOrigin.IsLinked: Boolean;
begin
  Result := True;
end;

{ TNullSnippetOrigin }

function TNullSnippetOrigin.GetModified: TUTCDateTime;
begin
  raise ENotImplemented.Create(
    'GetModified is not implemented in ' + ClassName
  );
end;

function TNullSnippetOrigin.GetOriginalID: string;
begin
  raise ENotSupportedException.Create(
    'GetOriginalID is not implemented in ' + ClassName
  );
end;

function TNullSnippetOrigin.GetSource: TSnippetOriginSource;
begin
  raise ENotSupportedException.Create(
    'GetSource is not implemented in ' + ClassName
  );
end;

function TNullSnippetOrigin.IsLinked: Boolean;
begin
  Result := False;
end;

end.

