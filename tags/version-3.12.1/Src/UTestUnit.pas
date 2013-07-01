{
 * UTestUnit.pas
 *
 * Implements a class that generates Pascal units for use in test compiling
 * snippets.
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
 * The Original Code is UTestUnit.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UTestUnit;


interface


uses
  // Project
  USnippets;


type

  {
  TTestUnit:
    Class that generates Pascal units for use in test compiling snippets.
  }
  TTestUnit = class(TObject)
  strict private
    fSnippet: TRoutine; // Reference to snippet for which test unit is required
    function UnitName: string;
      {Generates name of test unit, based on snippet being tested.
        @return Name of the unit.
      }
    function UnitFileName: string;
      {Calculates full file path to test.
        @return Fully specified unit file name.
      }
  public
    constructor Create(const Snippet: TRoutine);
      {Class constructor. Sets up object to create test unit for a snippet.
        @param Snippet [in] Snippet for which we want test unit.
      }
    function GenerateUnitSource: string;
      {Generates source code of test unit.
        @return Required source code.
      }
    procedure SaveUnit(out FileName: string);
      {Generates source code of test unit and saves to file.
        @param FileName [out] Set to name of unit file, which is based on
          snippet under test.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  USourceGen, USystemInfo, UUtils;


{ TTestUnit }

constructor TTestUnit.Create(const Snippet: TRoutine);
  {Class constructor. Sets up object to create test unit for a snippet.
    @param Snippet [in] Snippet for which we want test unit.
  }
begin
  Assert(Assigned(Snippet), ClassName + '.Create: Snippet is nil');
  inherited Create;
  fSnippet := Snippet;
end;

function TTestUnit.GenerateUnitSource: string;
  {Generates source code of test unit.
    @return Required source code.
  }
begin
  with TSourceGen.Create do
    try
      IncludeSnippet(fSnippet);
      // Must use Self.UnitName below for Delphis that defined TObject.UnitName
      // otherwise the TObject version is used.
      Result := UnitAsString(Self.UnitName);
    finally
      Free;
    end;
end;

procedure TTestUnit.SaveUnit(out FileName: string);
  {Generates source code of test unit and saves to file.
    @param FileName [out] Set to name of unit file, which is based on
      snippet under test.
  }
begin
  FileName := UnitFileName;
  StringToFile(GenerateUnitSource, FileName);
end;

function TTestUnit.UnitFileName: string;
  {Calculates full file path to test.
    @return Fully specified unit file name.
  }
const
  cPasExt = '.pas'; // file extension for Pascal unit:
begin
  // Unit file name is in temp folder
  Result := IncludeTrailingPathDelimiter(TSystemFolders.Temp)
    + UnitName + cPasExt;
end;

function TTestUnit.UnitName: string;
  {Generates name of test unit, based on snippet being tested.
    @return Name of the unit.
  }
const
  cUnitPrefix = 'U_'; // unit file name prefix
begin
  // Unit name is same as Snippet being tested, but with prefix to make unique
  Result := cUnitPrefix + fSnippet.Name;
end;

end.

