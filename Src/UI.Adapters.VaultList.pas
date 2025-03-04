{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that adapts a list of snippet vaults by providing an
 * alternative interface to the list, sorted by description. Designed for use
 * with GUI controls.
}

unit UI.Adapters.VaultList;

interface

uses
  // Delphi
  Classes,
  // Project
  DB.Vaults,
  UContainers;

type

  ///  <summary>Class that adapts a list of snippet vaults by providing an
  ///  alternative interface to the list, sorted by description. Designed for
  ///  use with GUI controls.</summary>
  TVaultListAdapter = class(TObject)
  strict private
    var
      fVaultList: TSortedList<TVault>;

  public

    ///  <summary>Object constructor. Sets up object with a sorted list of
    ///  vaults.</summary>
    constructor Create;

    ///  <summary>Object destructor. Tears down object.</summary>
    destructor Destroy; override;

    ///  <summary>Copies vault descriptions to a string list.</summary>
    ///  <param name="AStrings"><c>TStrings</c> [in] String list that receives
    ///  vault descriptions.</param>
    procedure ToStrings(const AStrings: TStrings);

    ///  <summary>Gets the vault at a specified index in the sorted list.
    ///  </summary>
    ///  <param name="AIndex"><c>Integer</c> [in] Index of required vault.
    ///  </param>
    ///  <returns><c>TVault</c>. Required vault.</returns>
    function Vault(const AIndex: Integer): TVault;

    ///  <summary>Gets list index of the vault with the specified UID.</summary>
    function IndexOfUID(const AUID: TVaultID): Integer;
  end;

implementation

uses
  // Delphi
  Generics.Defaults,
  // Project
  UStrUtils;

{ TVaultListAdapter }

constructor TVaultListAdapter.Create;
var
  Vault: TVault;
begin
  inherited Create;
  fVaultList := TSortedList<TVault>.Create(
    TDelegatedComparer<TVault>.Create(
      function (const Left, Right: TVault): Integer
      begin
        Result := StrCompareText(Left.Name, Right.Name)
      end
    )
  );
  for Vault in TVaults.Instance do
    fVaultList.Add(Vault);
end;

destructor TVaultListAdapter.Destroy;
begin
  fVaultList.Free;
  inherited;
end;

function TVaultListAdapter.IndexOfUID(const AUID: TVaultID): Integer;
var
  Idx: Integer;
begin
  Result := -1;
  for Idx := 0 to Pred(fVaultList.Count) do
    if fVaultList[Idx].UID = AUID then
      Exit(Idx);
end;

procedure TVaultListAdapter.ToStrings(const AStrings: TStrings);
var
  Vault: TVault;
begin
  for Vault in fVaultList do
    AStrings.Add(Vault.Name);
end;

function TVaultListAdapter.Vault(const AIndex: Integer): TVault;
begin
  Result := fVaultList[AIndex];
end;

end.
