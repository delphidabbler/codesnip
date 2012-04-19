{
 * DB.USnippetKind.pas
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
 * The Original Code is DB.USnippetKind, formerly USnippetKindInfo.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit DB.USnippetKind;


interface


uses
  // Project
  UBaseObjects;


type
  ///  <summary>
  ///  Enumeration of various supported kinds of snippets.
  ///  </summary>
  TSnippetKind = (
    skFreeform,   // free-form code - not in any of other supported formats
    skRoutine,    // procedure or function in standard format
    skConstant,   // constant definition in standard format
    skTypeDef,    // type definition in standard format
    skUnit        // complete source code unit
  );

type
  ///  <summary>
  ///  Set of snippet kinds.
  ///  </summary>
  TSnippetKinds = set of TSnippetKind;

type
  ///  <summary>
  ///  Provides read only information about a snippet kind
  ///  </summary>
  TSnippetKindInfo = record
  strict private
    ///  <summary>Value of Kind property.</summary>
    fKind: TSnippetKind;
    ///  <summary>Value of DisplayName property.</summary>
    fDisplayName: string;
  public
    ///  <summary>Initialises record with required property values.</summary>
    constructor Create(AKind: TSnippetKind; const ADisplayName: string);
    ///  <summary>Snippet kind.</summary>
    property Kind: TSnippetKind read fKind;
    ///  <summary>Display name (description) of snippet kind.</summary>
    property DisplayName: string read fDisplayName;
  end;

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
  sUnit             = 'Unit';
const
  // Map of snippet kinds onto their descriptions
  Descriptions: array[TSnippetKind] of string = (
    sFreeform, sRoutine, sConstant, sTypeDef, sUnit
  );
var
  Kind: TSnippetKind;
begin
  for Kind := Low(TSnippetKind) to High(TSnippetKind) do
    fItems[Kind] := TSnippetKindInfo.Create(Kind, Descriptions[Kind]);
end;

end.

