{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines a record that provides information about the different snippet kinds
 * enumerated by TSnippetKind along with a static class that provides an
 * enumerable list of snippet kind information records.
}


unit DB.USnippetKind;


interface


uses
  // Project
  CS.Database.Types,
  UBaseObjects;


type
  ///  <summary>
  ///  Static class that provides a read-only, enumerable, list of
  ///  TSnippetKindInfo records, one for each snippet kind.
  ///  </summary>
  TSnippetKindInfoList = class(TNoConstructObject)
  strict private
    type
      ///  <summary>Array of snippet kind information records. Has element for
      ///  each snippet kind.</summary>
      TSnippetKindInfoArray = array[TSnippetKind] of TSnippetKindInfo;
    class var
      ///  <summary>Array of snippet kind info records.</summary>
      fItems: TSnippetKindInfoArray;
      ///  <summary>Flag indicating if fItems has been initialised.</summary>
      fInitialised: Boolean;
  strict private
    ///  <summary>Initialises fItems array.</summary>
    class procedure Init;
    ///  <summary>Read accessor for Items property.</summary>
    class function GetItems: TSnippetKindInfoArray; static;
  public
    ///  <summary>Enumerable array of snippet kind info records, one element for
    ///  each snippet kind.</summary>
    class property Items: TSnippetKindInfoArray read GetItems;
  end;


implementation


{ TSnippetKindInfoList }

class function TSnippetKindInfoList.GetItems: TSnippetKindInfoArray;
begin
  if not fInitialised then
  begin
    // The obvious thing to do is to initialise the Items[] array in a class
    // constructor. But the required resource strings are read as '' when
    // initialisation is done in a class constructor, so we need to jump thru
    // these hoops.
    Init;
    fInitialised := True;
  end;
  Result := fItems;
end;

class procedure TSnippetKindInfoList.Init;
resourcestring
  // Snippet kind descriptions
  sFreeForm         = 'Freeform';
  sRoutine          = 'Routine';
  sConstant         = 'Constant';
  sTypeDef          = 'Type Definition';
  sUnit             = 'Unit';
  sClass            = 'Class / Advanced Record';
const
  // Map of snippet kinds onto their descriptions
  Descriptions: array[TSnippetKind] of string = (
    sFreeform, sRoutine, sConstant, sTypeDef, sUnit, sClass
  );
var
  Kind: TSnippetKind;
begin
  for Kind := Low(TSnippetKind) to High(TSnippetKind) do
    fItems[Kind] := TSnippetKindInfo.Create(Kind, Descriptions[Kind]);
end;

end.

