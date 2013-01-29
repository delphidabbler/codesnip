{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides objects that can read and write snippet selection files.
}


unit USelectionIOHandler;


interface


uses
  // Project
  USnippetIDListIOHandler;


type

  TSelectionFileReader = class(TSnippetIDListFileReader)
  public
    constructor Create;
  end;

type

  TSelectionFileWriter = class(TSnippetIDListFileWriter)
  public
    constructor Create;
  end;


implementation


const
  ///  <summary>File watermark. Uses characters that will be interpreted wrongly
  ///  if not UTF8 format.</summary>
  SelectionFileWatermark = #$25BA + ' CodeSnip Selections v1 ' + #$25C4;

{ TSelectionFileReader }

constructor TSelectionFileReader.Create;
begin
  inherited Create(SelectionFileWatermark);
end;


{ TSelectionFileWriter }

constructor TSelectionFileWriter.Create;
begin
  inherited Create(SelectionFileWatermark);
end;

end.

