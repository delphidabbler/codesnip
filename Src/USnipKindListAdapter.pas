{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2024, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that adapts a list of snippet kinds by providing an
 * alternative interface to the list, sorted by the name of the snippet kind.
 * Designed for use with GUI controls.
}


unit USnipKindListAdapter;


interface


uses
  // Delphi
  Classes,
  // Project
  DB.USnippet, DB.USnippetKind, UContainers;


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
      TSortedList<TSnippetKindInfo>;
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
  Generics.Defaults,
  // Project
  UStrUtils;


{ TSnipKindListAdapter }

constructor TSnipKindListAdapter.Create;
  {Object constructor. Sets up object with sorted list of all snippet kinds.
  }
var
  SnipKindInfo: TSnippetKindInfo; // each snippet kin info record
begin
  inherited Create;
  fSnipKindList := TSortedList<TSnippetKindInfo>.Create(
    TDelegatedComparer<TSnippetKindInfo>.Create(
      function (const Left, Right: TSnippetKindInfo): Integer
      begin
        Result := StrCompareText(Left.DisplayName, Right.DisplayName);
      end
    )
  );
  for SnipKindInfo in TSnippetKindInfoList.Items do
    fSnipKindList.Add(SnipKindInfo);
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
  {Copies snippet kind description to a string list.
    @param Strings [in] String list to receive information.
  }
var
  Elem: TSnippetKindInfo; // each snippet kind in sorted list
begin
  for Elem in fSnipKindList do
    Strings.Add(Elem.DisplayName);
end;

end.

