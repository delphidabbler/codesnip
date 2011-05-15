{
 * USnippetKindInfo.pas
 *
 * Defines a record that provides information about the different snippet kinds
 * enumerated by TSnippetKind along with a static class that provides an
 * enumerable list of snippet kind information records.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is USnippetKindInfo.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit USnippetKindInfo;


interface


uses
  // Project
  UBaseObjects;


type

  {
  TSnippetKind:
    Enumeration of various supported kinds of snippets.
  }
  TSnippetKind = (
    skFreeform,   // free-form code - not in any of other supported formats
    skRoutine,    // procedure or function in standard format
    skConstant,   // constant definition in standard format
    skTypeDef     // type definition in standard format
  );

  {
  TSnippetKinds:
    Sets of snippet kinds.
  }
  TSnippetKinds = set of TSnippetKind;

  ///  <summary>
  ///  Provides read only information about a snippet kind
  ///  </summary>
  TSnippetKindInfo = record
  strict private
    fKind: TSnippetKind;    // Value of Kind property
    fDisplayName: string;   // Value of DisplayName property
  public
    ///  Initialises record with required property values.
    constructor Create(AKind: TSnippetKind; const ADisplayName: string);
    ///  Snippet kind.
    property Kind: TSnippetKind read fKind;
    ///  Display name (description) of snippet kind
    property DisplayName: string read fDisplayName;
  end;

  ///  <summary>
  ///  Static class that provides a read-only, enumerable, list of
  ///  TSnippetKindInfo records, one for each snippet kind.
  ///  </summary>
  TSnippetKindInfoList = class(TNoConstructObject)
  strict private
    type
      ///  Array of snippet kind information records. Has element for each
      ///  snippet kind.
      TSnippetKindInfoArray = array[TSnippetKind] of TSnippetKindInfo;
    ///  Array of snippet kind info records.
    class var fItems: TSnippetKindInfoArray;
    ///  Flag indicating if fItems has yet been initialised.
    class var fInitialised: Boolean;
    ///  Initialises fItems array.
    class procedure Init;
    ///  Read accessor for Items property.
    class function GetItems: TSnippetKindInfoArray; static;
  public
    ///  Enumerable array of snippet kind info records, one element for each
    ///  snippet kind.
    class property Items: TSnippetKindInfoArray read GetItems;
  end;


implementation


{ TSnippetKindInfo }

constructor TSnippetKindInfo.Create(AKind: TSnippetKind;
  const ADisplayName: string);
begin
  fKind := AKind;
  fDisplayName := ADisplayName;
end;

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
const
  // Map of snippet kinds onto their descriptions
  Descriptions: array[TSnippetKind] of string = (
    sFreeform, sRoutine, sConstant, sTypeDef
  );
var
  Kind: TSnippetKind;
begin
  for Kind := Low(TSnippetKind) to High(TSnippetKind) do
    fItems[Kind] := TSnippetKindInfo.Create(Kind, Descriptions[Kind]);
end;

end.

