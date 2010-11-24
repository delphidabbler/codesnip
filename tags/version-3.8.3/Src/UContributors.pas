{
 * UContributors.pas
 *
 * Defines a classes that encapsulates lists of database contributors taken from
 * files.
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
 * The Original Code is UContributors.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UContributors;


interface


uses
  // Delphi
  Classes;


type

  TContributors = class;

  {
  TContributorsClass:
    Class reference for contributors classes
  }
  TContributorsClass = class of TContributors;

  {
  TContributorsEnum:
    Enumerator for contributors classes. Enables use of for .. in construct on
    TContributors descendants.
  }
  TContributorsEnum = class(TObject)
  strict private
    var
      fContribs: TContributors; // Reference to object being enumerated
      fIndex: Integer;          // Index of current object in enumeration
  public
    constructor Create(AContributors: TContributors);
      {Class constructor. Initialises enumeration.
        @param AContributors [in] Object to be enurmerated.
      }
    function GetCurrent: string;
      {Gets name of current contributor.
        @return Required name.
      }
    function MoveNext: Boolean;
      {Moves to next item in enumeration.
        @return True if there is a next item, False if beyond last item.
      }
    property Current: string read GetCurrent;
      {Name of current contributor}
  end;

  {
  TContributors:
    Abstract base class for classes that load and encapsulate a list of database
    contributors.
  }
  TContributors = class abstract(TObject)
  strict private
    var
      fContributors: TStringList;   // Stores list of contributors
      fIsError: Boolean;            // Value of IsError property
    function GetCount: Integer;
      {Read accessor for Count property.
        @return Number of contributors.
      }
    function GetItem(const Idx: Integer): string;
      {Read accessor for Items property.
        @param Idx [in] Index of requested contributor in Items[].
        @return Details of requested contributor.
      }
  strict protected
    function GetFileName: string; virtual; abstract;
      {Gets base name of file containing details of contributors.
        @return Required file base name.
      }
  public
    constructor Create; virtual;
      {Class constructor. Sets up object and loads contributor information.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    function GetEnumerator: TContributorsEnum;
      {Creates an enumerator for this object.
        @return Reference to new enumerator. Caller is repsonsible for freeing
          this object.
      }
    property Items[const Idx: Integer]: string read GetItem; default;
      {List of contributors}
    property Count: Integer read GetCount;
      {Number of contributors}
    property IsError: Boolean read fIsError;
      {Flag set true when contributors could not be loaded}
  end;

  {
  TCodeContributors:
    Encapsulates a list of people who have contributed code to the CodeSnip
    database.
  }
  TCodeContributors = class sealed(TContributors)
  strict protected
    function GetFileName: string; override;
      {Gets base name of file containing details of code contributors.
        @return Required file base name.
      }
  end;

  {
  TTesters:
    Encapsulates a list of people who have tested code in the CodeSnip
    database.
  }
  TTesters = class sealed(TContributors)
  strict protected
    function GetFileName: string; override;
      {Gets base name of file containing details of testers.
        @return Required file base name.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UAppInfo;


{ TContributors }

constructor TContributors.Create;
  {Class constructor. Sets up object and loads contributor information.
  }
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
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fContributors);
  inherited;
end;

function TContributors.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of contributors.
  }
begin
  Result := fContributors.Count;
end;

function TContributors.GetEnumerator: TContributorsEnum;
  {Creates an enumerator for this object.
    @return Reference to new enumerator. Caller is repsonsible for freeing this
      object.
  }
begin
  Result := TContributorsEnum.Create(Self);
end;

function TContributors.GetItem(const Idx: Integer): string;
  {Read accessor for Items property.
    @param Idx [in] Index of requested contributor in Items[].
    @return Details of requested contributor.
  }
begin
  Result := fContributors[Idx];
end;

{ TCodeContributors }

function TCodeContributors.GetFileName: string;
  {Gets base name of file containing details of code contributors.
    @return Required file base name.
  }
begin
  Result := 'contrib.txt'
end;

{ TTesters }

function TTesters.GetFileName: string;
  {Gets base name of file containing details of testers.
    @return Required file base name.
  }
begin
  Result := 'testers.txt';
end;

{ TContributorsEnum }

constructor TContributorsEnum.Create(AContributors: TContributors);
  {Class constructor. Initialises enumeration.
    @param AContributors [in] Object to be enurmerated.
  }
begin
  inherited Create;
  fContribs := AContributors;
  fIndex := -1;
end;

function TContributorsEnum.GetCurrent: string;
  {Gets name of current contributor.
    @return Required name.
  }
begin
  Result := fContribs[fIndex];
end;

function TContributorsEnum.MoveNext: Boolean;
  {Moves to next item in enumeration.
    @return True if there is a next item, False if beyond last item.
  }
begin
  Result := fIndex < Pred(fContribs.Count);
  if Result then
    Inc(fIndex);
end;

end.

