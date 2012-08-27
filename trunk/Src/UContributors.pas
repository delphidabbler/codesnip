{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines a classes that encapsulates lists of database contributors taken from
 * files.
}


unit UContributors;


interface


uses
  // Delphi
  Classes;


type
  ///  <summary>
  ///  Abstract base class for classes that load and encapsulate a list of
  ///  database contributors.
  ///  </summary>
  TContributors = class abstract(TObject)
  strict private
    var
      ///  <summary>Stores list of contributors.</summary>
      fContributors: TStringList;
      ///  <summary>Value of IsError property.</summary>
      fIsError: Boolean;
    ///  <summary>Getter for Count property.</summary>
    function GetCount: Integer;
    ///  <summary>Getter for indexed Items[] property.</summary>
    function GetItem(const Idx: Integer): string;
  strict protected
    ///  <summary>Gets base name of file containing details of contributors.
    ///  </summary>
    function GetFileName: string; virtual; abstract;
  public
    ///  <summary>Sets up object and loads contributor information.</summary>
    constructor Create; virtual;
    ///  <summary>Tears down object.</summary>
    destructor Destroy; override;
    ///  <summary>Creates and returns object enumerator.</summary>
    function GetEnumerator: TStringsEnumerator;
    ///  <summary>Indexed list of contributors.</summary>
    property Items[const Idx: Integer]: string read GetItem; default;
    ///  <summary>Number of contributors.</summary>
    property Count: Integer read GetCount;
    ///  <summary>Flag indicating if there has been an error loading
    ///  contributors file.</summary>
    property IsError: Boolean read fIsError;
  end;

type
  ///  <summary>
  ///  Encapsulates a list of people who have contributed code to the CodeSnip
  ///  database.
  ///  </summary>
  TCodeContributors = class sealed(TContributors)
  strict protected
    ///  <summary>Gets base name of file containing details of code
    ///  contributors.</summary>
    function GetFileName: string; override;
  end;

type
  ///  <summary>
  ///  Encapsulates a list of people who have tested code in the CodeSnip
  ///  database.
  ///  </summary>
  TTesters = class sealed(TContributors)
  strict protected
    ///  <summary>Gets base name of file containing details of testers.
    ///  </summary>
    function GetFileName: string; override;
  end;

type
  ///  <summary>Class reference for contributors classes.</summary>
  TContributorsClass = class of TContributors;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UAppInfo;


{ TContributors }

constructor TContributors.Create;
var
  ContribFile: string;  // name of file containing contributor information
begin
  Assert(ClassType <> TContributors,
    ClassName + '.Create: must only be called from descendants.');
  inherited;
  fContributors := TStringList.Create;
  // Load contributor file
  ContribFile := IncludeTrailingPathDelimiter(TAppInfo.AppDataDir)
    + GetFileName;
  if FileExists(ContribFile) then
    try
      fContributors.LoadFromFile(ContribFile);
    except
      // swallow any exception: treat as empty contributor list
    end;
  // Error if list is empty: flag error
  fIsError := Count = 0;
end;

destructor TContributors.Destroy;
begin
  fContributors.Free;
  inherited;
end;

function TContributors.GetCount: Integer;
begin
  Result := fContributors.Count;
end;

function TContributors.GetEnumerator: TStringsEnumerator;
begin
  Result := fContributors.GetEnumerator;
end;

function TContributors.GetItem(const Idx: Integer): string;
begin
  Result := fContributors[Idx];
end;

{ TCodeContributors }

function TCodeContributors.GetFileName: string;
begin
  Result := 'contrib.txt'
end;

{ TTesters }

function TTesters.GetFileName: string;
begin
  Result := 'testers.txt';
end;

end.

