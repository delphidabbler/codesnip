{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2025, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Class helper for TRichEdit.
}

unit ClassHelpers.RichEdit;

interface

uses
  // Delphi
  ComCtrls,
  // Project
  URTFUtils;

type
  TRichEditHelper = class helper for TRichEdit
  public
    procedure Load(const ARTF: TRTF);
  end;

implementation

uses
  // Delphi
  SysUtils,
  Classes;

{ TRichEditHelper }

procedure TRichEditHelper.Load(const ARTF: TRTF);
var
  Stream: TStream;
begin
  PlainText := False;
  Stream := TMemoryStream.Create;
  try
    ARTF.ToStream(Stream);
    Stream.Position := 0;
    // must set MaxLength or long documents may not display
    MaxLength := Stream.Size;
    Lines.LoadFromStream(Stream, TEncoding.ASCII);
  finally
    Stream.Free;
  end;
end;

end.
