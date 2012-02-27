{
 * Compilers.USearchDirs.pas
 *
 * Implements a class that encapsulates a list of search directories for use
 * by the compiler.
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
 * The Original Code is Compilers.USearchDirs.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit Compilers.USearchDirs;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  Compilers.UGlobals, IntfCommon, UIStringList;


type
  ///  <summary>
  ///  Implements a list of search directories for use by compilers.
  ///  </summary>
  TSearchDirs = class(TInterfacedObject,
    ISearchDirs, IClonable
  )
  strict private
    var
      ///  <summary>Records directory names.</summary>
      fList: TList<string>;
  public
    ///  <summary>Creates new empty list object.</summary>
    constructor Create; overload;
    ///  <summary>Creates new list containg given directory names.</summary>
    constructor Create(const DirNames: TArray<string>); overload;
    ///  <summary>Tears down object.</summary>
    destructor Destroy; override;
    ///  <summary>Creates and returns a copy of this object.</summary>
    ///  <remarks>Method of IClonable.</remarks>
    function Clone: IInterface;
    ///  <summary>Creates and returns enumerator for directories list.</summary>
    ///  <remarks>Method of ISearchDirs.</remarks>
    function GetEnumerator: TEnumerator<string>;
    ///  <summary>Adds a new search directory to list.</summary>
    ///  <remarks>Method of ISearchDirs.</remarks>
    procedure Add(const DirName: string);
    ///  <summary>Clears list.</summary>
    ///  <remarks>Method of ISearchDirs.</remarks>
    procedure Clear;
    ///  <summary>Checks if list is empty.</summary>
    ///  <remarks>Method of ISearchDirs.</remarks>
    function IsEmpty: Boolean;
    ///  <summary>Returns an array containing the names of all directories in
    ///  the list.</summary>
    ///  <remarks>Method of ISearchDirs.</remarks>
    function ToStrings: TArray<string>;
  end;


implementation


uses
  // Delphi
  SysUtils, Generics.Defaults,
  // Project
  UComparers, UStrUtils;


{ TSearchDirs }

procedure TSearchDirs.Add(const DirName: string);
begin
  fList.Add(DirName);
end;

procedure TSearchDirs.Clear;
begin
  fList.Clear;
end;

function TSearchDirs.Clone: IInterface;
var
  NewInst: ISearchDirs;
  Dir: string;
begin
  NewInst := TSearchDirs.Create;
  for Dir in Self do
    NewInst.Add(Dir);
  Result := NewInst as IInterface;
end;

constructor TSearchDirs.Create;
begin
  inherited Create;
  fList := TList<string>.Create(TTextComparer.Create);
end;

constructor TSearchDirs.Create(const DirNames: TArray<string>);
var
  DirName: string;
begin
  Create;
  for DirName in DirNames do
    Add(DirName);
end;

destructor TSearchDirs.Destroy;
begin
  fList.Free;
  inherited;
end;

function TSearchDirs.GetEnumerator: TEnumerator<string>;
begin
  Result := fList.GetEnumerator;
end;

function TSearchDirs.IsEmpty: Boolean;
begin
  Result := fList.Count = 0;
end;

function TSearchDirs.ToStrings: TArray<string>;
var
  Idx: Integer;
begin
  SetLength(Result, fList.Count);
  for Idx := 0 to Pred(fList.Count) do
    Result[Idx] := fList[Idx];
end;

end.

