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
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
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
  UContainers, USnippetKindInfo, USnippets;


type
  {
  TSnipKindListAdapter:
    Adapts a list of snippet kinds by providing an alternative interface to the
    list, sorted by the name of the snippet kind. Designed for use with GUI
    controls.
 }
  TSnipKindListAdapter = class(TObject)
  strict private
    var fSnipKindList:    // Sorted list of snippet kinds
      TSortedObjectList<TSnippetKindInfo>;
  public
    constructor Create;
      {Object constructor. Sets up object with sorted list of all snippet kinds.
      }
    destructor Destroy; override;
      {Object destructor. Tears down object.
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
  SysUtils, Windows {for inlining}, Generics.Defaults;


{ TSnipKindListAdapter }

constructor TSnipKindListAdapter.Create;
  {Object constructor. Sets up object with sorted list of all snippet kinds.
  }
var
  SnipKind: TSnippetKind; // loops thru all snippet kinds
begin
  inherited Create;
  fSnipKindList := TSortedObjectList<TSnippetKindInfo>.Create(
    TDelegatedComparer<TSnippetKindInfo>.Create(
      function (const Left, Right: TSnippetKindInfo): Integer
      begin
        Result := AnsiCompareText(Left.Description, Right.Description);
      end
    ),
    False
  );
  for SnipKind := Low(TSnippetKind) to High(TSnippetKind) do
    fSnipKindList.Add(TSnippetKindInfoList.Instance[SnipKind])
end;

destructor TSnipKindListAdapter.Destroy;
  {Object destructor. Tears down object.
  }
begin
  fSnipKindList.Free;
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
    if fSnipKindList[Idx].Kind = SnipKind then
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
  Result := fSnipKindList[Index].Kind;
end;

procedure TSnipKindListAdapter.ToStrings(const Strings: TStrings);
  {Copies snippet kind description and related object to a string list.
    @param Strings [in] String list to receive information.
  }
var
  Elem: TSnippetKindInfo; // each snippet kind in sorted list
begin
  for Elem in fSnipKindList do
    Strings.AddObject(Elem.Description, Elem);
end;

end.

