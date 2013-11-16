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
 * Implements a record that encapsulates markup text of different kinds.
}


unit CS.Markup;

interface

type
  TMarkupKind = (mkPlainText, mkREML1, mkREML2, mkREML3);

  IMarkup = interface(IInterface)
    ['{71793A5D-A89B-4228-9EFD-E3B8488B75B6}']
    function GetSource: string;
    function GetKind: TMarkupKind;
    property Source: string read GetSource;
    property Kind: TMarkupKind read GetKind;
    function IsEmpty: Boolean;
  end;

  TMarkup = class(TInterfacedObject, IMarkup)
  strict private
    var
      fSource: string;
      fKind: TMarkupKind;
  public
    constructor Create(const ASource: string; const AKind: TMarkupKind);
    class function CreateEmpty: TMarkup; inline;
    function GetSource: string;
    function GetKind: TMarkupKind;
    property Source: string read GetSource;
    property Kind: TMarkupKind read GetKind;
    function IsEmpty: Boolean; inline;
    // TODO: function ToActiveText: IActiveText;
  end;

implementation

uses
  SysUtils;

{ TMarkup }

constructor TMarkup.Create(const ASource: string; const AKind: TMarkupKind);
begin
  fSource := ASource;
  fKind := AKind;
end;

class function TMarkup.CreateEmpty: TMarkup;
begin
  Result := TMArkup.Create(EmptyStr, mkPlainText);
end;

function TMarkup.GetKind: TMarkupKind;
begin
  Result := fKind;
end;

function TMarkup.GetSource: string;
begin
  Result := fSource;
end;

function TMarkup.IsEmpty: Boolean;
begin
  Result := fSource = EmptyStr;
end;

end.
