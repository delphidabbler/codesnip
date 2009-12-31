{
 * USnipKindListAdapter.pas
 *
 * Implements a class that adapts a list of snippet kinds by providing an
 * alternative interface to the list, sorted by the name of the snippet kind.
 * Designed for use with GUI controls.
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
 * The Original Code is USnipKindListAdapter.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit USnipKindListAdapter;


interface


uses
  // Delphi
  Classes,
  // Project
  ULists, USnippets;


type
  {
  TSnipKindListAdapter:
    Adapts a list of snippet kinds by providing an alternative interface to the
    list, sorted by the name of the snippet kind. Designed for use with GUI
    controls.
 }
  TSnipKindListAdapter = class(TObject)
  private
    fSnipKindList: TSortedObjectList;  // Sorted list of snippet kinds
    function CompareSnipKinds(const Obj1, Obj2: TObject): Integer;
      {Callback comparison method for passing to sorted list object. Compares
      two snippet kind objects.
        @param Obj1 [in] First snippet kind object to be compared.
        @param Obj2 [in] Second snippet kind object to be compared.
        @return -ve if Obj1 < Obj2, 0 if Obj1 = Obj2 or +ve if Obj1 > Obj2.
      }
  public
    constructor Create;
      {Class constructor. Sets up object with sorted list of all snippet kinds.
      }
    destructor Destroy; override;
      {Class desctrutor. Tears down object.
      }
    function IndexOf(const SnipKind: TSnippetKind): Integer;
      {Gets index of a snippet kind in sorted list.
        @param SnipKind[in] Required snippet kind.
        @return Index of snippet kind in list or -1 if not found.
      }
    procedure ToStrings(const Strings: TStrings);
      {Copies snippet kind description and related object to a string list.
        @param Strings [in] String list to receive information.
      }
    function SnippetKind(const Index: Integer): TSnippetKind;
      {Gets snippet kind at a specified index in the sorted list.
        @param Index [in] Index of snippet kind required.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Windows, {for inlining}
  // Project
  USnippetKindInfo;


{ TSnipKindListAdapter }

function TSnipKindListAdapter.CompareSnipKinds(const Obj1,
  Obj2: TObject): Integer;
  {Callback comparison method for passing to sorted list object. Compares two
  snippet kind objects.
    @param Obj1 [in] First snippet kind object to be compared.
    @param Obj2 [in] Second snippet kind object to be compared.
    @return -ve if Obj1 < Obj2, 0 if Obj1 = Obj2 or +ve if Obj1 > Obj2.
  }
var
  Kind1, Kind2: TSnippetKindInfo; // snippet kinds to be compared
begin
  Kind1 := Obj1 as TSnippetKindInfo;
  Kind2 := Obj2 as TSnippetKindInfo;
  Result := AnsiCompareText(Kind1.Description, Kind2.Description);
end;

constructor TSnipKindListAdapter.Create;
  {Class constructor. Sets up object with sorted list of all snippet kinds.
  }
var
  SnipKind: TSnippetKind; // loops thru all snippet kinds
begin
  inherited Create;
  fSnipKindList := TSortedObjectList.Create(False, CompareSnipKinds);
  for SnipKind := Low(TSnippetKind) to High(TSnippetKind) do
    fSnipKindList.Add(TSnippetKindInfoList.Instance[SnipKind])
end;

destructor TSnipKindListAdapter.Destroy;
  {Class desctrutor. Tears down object.
  }
begin
  FreeAndNil(fSnipKindList);
  inherited;
end;

function TSnipKindListAdapter.IndexOf(const SnipKind: TSnippetKind): Integer;
  {Gets index of a snippet kind in sorted list.
    @param SnipKind[in] Required snippet kind.
    @return Index of snippet kind in list or -1 if not found.
  }
var
  Idx: Integer; // loops through items of sorted list
begin
  Result := -1;
  for Idx := 0 to Pred(fSnipKindList.Count) do
    if (fSnipKindList[Idx] as TSnippetKindInfo).Kind = SnipKind then
    begin
      Result := Idx;
      Break;
    end;
end;

function TSnipKindListAdapter.SnippetKind(const Index: Integer): TSnippetKind;
  {Gets snippet kind at a specified index in the sorted list.
    @param Index [in] Index of snippet kind required.
  }
begin
  Result := (fSnipKindList[Index] as TSnippetKindInfo).Kind;
end;

procedure TSnipKindListAdapter.ToStrings(const Strings: TStrings);
  {Copies snippet kind description and related object to a string list.
    @param Strings [in] String list to receive information.
  }
var
  Elem: TObject; // each snippet kind in sorted list
begin
  for Elem in fSnipKindList do
    Strings.AddObject((Elem as TSnippetKindInfo).Description, Elem);
end;

end.

