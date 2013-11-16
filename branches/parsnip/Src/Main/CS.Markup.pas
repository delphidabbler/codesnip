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

  TMarkup = record
  strict private
    var
      fSource: string;
      fKind: TMarkupKind;
  public
    constructor Create(const ASource: string; const AKind: TMarkupKind);
    class function CreateEmpty: TMarkup; static; inline;
    property Source: string read fSource;
    property Kind: TMarkupKind read fKind;
    function IsEmpty: Boolean; inline;
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

function TMarkup.IsEmpty: Boolean;
begin
  Result := fSource = EmptyStr;
end;

end.
