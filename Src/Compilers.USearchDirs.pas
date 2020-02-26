{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2011-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that encapsulates a list of search directories for use
 * by the compiler.
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
begin
  Result := fList.ToArray;
end;

end.

