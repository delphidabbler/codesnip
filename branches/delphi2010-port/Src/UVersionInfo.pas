{
 * UVersionInfo.pas
 *
 * Provides details of the application's version information and provides a
 * record used to manipulate version numbers.
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
 * The Original Code is UVersionInfo.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UVersionInfo;


interface


uses
  // DelphiDabbler library
  PJVersionInfo,
  // Project
  UBaseObjects;


type

  {
  TVersionNumber:
    Record representing the four fields of a version number.
  }
  TVersionNumber = record
    V1: Word;   // Major version number
    V2: Word;   // Minor version number
    V3: Word;   // Revision version number
    V4: Word;   // Build number
    { TODO -oSelf -cNote : Delphi 2010 Fix: Changed from constructor to class
      method. }
    class function Nul: TVersionNumber; static;
      {Creates a nul version number with all fields set to zero.
        @return Required nul record.
      }
    class operator LessThanOrEqual(Ver1, Ver2: TVersionNumber): Boolean;
      {Operator overload that compares two version numbers to check if first is
      less than or equal to the second.
        @param Ver1 [in] First version number.
        @param Ver2 [in] Second version number.
        @return True if Ver1 <= Ver2, False otherwise.
      }
    class operator LessThan(Ver1, Ver2: TVersionNumber): Boolean;
      {Operator overload that compares two version numbers to check if first is
      less than second.
        @param Ver1 [in] First version number.
        @param Ver2 [in] Second version number.
        @return True if Ver1 < Ver2, False otherwise.
      }
    class operator GreaterThan(Ver1, Ver2: TVersionNumber): Boolean;
      {Operator overload that compares two version numbers to check if first is
      greater than second.
        @param Ver1 [in] First version number.
        @param Ver2 [in] Second version number.
        @return True if Ver1 > Ver2, False otherwise.
      }
    class operator GreaterThanOrEqual(Ver1, Ver2: TVersionNumber): Boolean;
      {Operator overload that compares two version numbers to check if first is
      greater than or equal to the second.
        @param Ver1 [in] First version number.
        @param Ver2 [in] Second version number.
        @return True if Ver1 >= Ver2, False otherwise.
      }
    class operator Equal(Ver1, Ver2: TVersionNumber): Boolean;
      {Operator overload that compares two version numbers to check for
      equality.
        @param Ver1 [in] First version number.
        @param Ver2 [in] Second version number.
        @return True if Ver1 = Ver2, False otherwise.
      }
    class operator NotEqual(Ver1, Ver2: TVersionNumber): Boolean;
      {Operator overload that compares two version numbers to check for
      inequality.
        @param Ver1 [in] First version number.
        @param Ver2 [in] Second version number.
        @return True if Ver1 <> Ver2, False otherwise.
      }
    class operator Implicit(Ver: TPJVersionNumber): TVersionNumber;
      {Operator overload that perfoms implicit conversion of a TPJVersionNumber
      (from PJVersionInfo unit) to a TVersionNumber.
        @param Ver [in] Version number to be converted.
        @return Converted version number.
      }
    class operator Implicit(Str: string): TVersionNumber;
      {Operator overload that performs implicit conversion of a dotted quad
      string to a TVersionNumber.
        @param Str [in] String to be converted. Must be in dotted quad format.
        @return Converted version number.
        @except EConvertError. raised if string in wrong format.
      }
    class operator Implicit(Ver: TVersionNumber): string;
      {Operator overload that performs implicit conversion of TVersionNumber to
      string as dotted quad.
        @param Ver [in] Version number to be converted.
        @return Version number as dotted quad.
      }
  end;

  {
  TVersionInfo:
    Extracts version information from version resources. Extends TPJVersionInfo
    component by providing some convenience class methods.
  }
  TVersionInfo = class(TNoConstructObject)
  public
    class function ProductVersionStr: string;
      {Gets product version string from string table. May differ from
      ProductVersionNumberStr.
        @return Product version string.
      }
    class function ProductVersionNumberStr: string;
      {Dotted quad representation of product version number from fixed file
      information.
        @return Version number string in form 9.9.9.9.
      }
    class function ProductVerNum: TVersionNumber;
      {Product version number from fixed file information.
        @return Required version number record.
      }
    class function FileVersionNumberStr: string;
      {Dotted quad representation of file version number from fixed file
      information.
        @return Version number string in form 9.9.9.9.
      }
    class function LegalCopyrightStr: string;
      {Gets copyright information from string table.
        @return Required copyright information.
      }
    class function SpecialBuildStr: string;
      {Gets special build information from string table.
        @return Required copyright information.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UIStringList;


{ TVersionInfo }

class function TVersionInfo.FileVersionNumberStr: string;
  {Dotted quad representation of file version number from fixed file
  information.
    @return Version number string in form 9.9.9.9.
  }
begin
  with TPJVersionInfo.Create(nil) do
    try
      // casts TPJVersionNumber to TVersionNumber & TVersionNumber to string
      Result := TVersionNumber(FileVersionNumber);
    finally
      Free;
    end;
end;

class function TVersionInfo.LegalCopyrightStr: string;
  {Gets copyright information from string table.
    @return Required copyright information.
  }
begin
  with TPJVersionInfo.Create(nil) do
    try
      Result := LegalCopyright;
    finally
      Free;
    end;
end;

class function TVersionInfo.ProductVerNum: TVersionNumber;
  {Product version number from fixed file information.
    @return Required version number record.
  }
begin
  with TPJVersionInfo.Create(nil) do
    try
      Result := ProductVersionNumber;   // implicit typecast
    finally
      Free;
    end;
end;

class function TVersionInfo.ProductVersionNumberStr: string;
  {Dotted quad representation of product version number from fixed file
  information.
    @return Version number string in form 9.9.9.9.
  }
begin
  Result := ProductVerNum;  // uses implicit typecast
end;

class function TVersionInfo.ProductVersionStr: string;
  {Gets product version string from string table. May differ from
  ProductVersionNumberStr.
    @return Product version string.
  }
begin
  with TPJVersionInfo.Create(nil) do
    try
      Result := ProductVersion;
    finally
      Free;
    end;
end;

class function TVersionInfo.SpecialBuildStr: string;
  {Gets special build information from string table.
    @return Required copyright information.
  }
begin
  with TPJVersionInfo.Create(nil) do
    try
      Result := SpecialBuild;
    finally
      Free;
    end;
end;

{ TVersionNumber }

class operator TVersionNumber.Equal(Ver1, Ver2: TVersionNumber): Boolean;
  {Operator overload that compares two version numbers to check for equality.
    @param Ver1 [in] First version number.
    @param Ver2 [in] Second version number.
    @return True if Ver1 = Ver2, False otherwise.
  }
begin
  Result := (Ver1.V1 = Ver2.V1) and (Ver1.V2 = Ver2.V2) and
    (Ver1.V3 = Ver2.V3) and (Ver1.V4 = Ver2.V4);
end;

class operator TVersionNumber.GreaterThan(Ver1, Ver2: TVersionNumber): Boolean;
  {Operator overload that compares two version numbers to check if first is
  greater than second.
    @param Ver1 [in] First version number.
    @param Ver2 [in] Second version number.
    @return True if Ver1 > Ver2, False otherwise.
  }
begin
  Result := True;
  if (Ver1.V1 > Ver2.V1) then
    Exit;
  if (Ver1.V1 = Ver2.V1) then
  begin
    if (Ver1.V2 > Ver2.V2) then
      Exit;
    if (Ver1.V2 = Ver2.V2) then
    begin
      if (Ver1.V3 > Ver2.V3) then
        Exit;
      if (Ver1.V3 = Ver2.V3) then
      begin
        if (Ver1.V4 > Ver2.V4) then
          Exit;
      end;
    end;
  end;
  Result := False;
end;

class operator TVersionNumber.GreaterThanOrEqual(Ver1,
  Ver2: TVersionNumber): Boolean;
  {Operator overload that compares two version numbers to check if first is
  greater than or equal to the second.
    @param Ver1 [in] First version number.
    @param Ver2 [in] Second version number.
    @return True if Ver1 >= Ver2, False otherwise.
  }
begin
  Result := (Ver1 = Ver2) or (Ver1 > Ver2);
end;

class operator TVersionNumber.Implicit(Ver: TVersionNumber): string;
  {Operator overload that performs implicit conversion of TVersionNumber to
  string as dotted quad.
    @param Ver [in] Version number to be converted.
    @return Version number as dotted quad.
  }
begin
  Result := Format('%d.%d.%d.%d', [Ver.V1, Ver.V2, Ver.V3, Ver.V4]);
end;

class operator TVersionNumber.Implicit(Str: string): TVersionNumber;
  {Operator overload that performs implicit conversion of a dotted quad string
  to a TVersionNumber.
    @param Str [in] String to be converted. Must be in dotted quad format.
    @return Converted version number.
    @except EConvertError. raised if string in wrong format.
  }
var
  Parts: IStringList; // contains parts of version number
begin
  Parts := TIStringList.Create(Str, '.', False, True);
  while Parts.Count < 4 do
    Parts.Add('0');
  Result.V1 := StrToInt(Parts[0]);
  Result.V2 := StrToInt(Parts[1]);
  Result.V3 := StrToInt(Parts[2]);
  Result.V4 := StrToInt(Parts[3]);
end;

class operator TVersionNumber.Implicit(Ver: TPJVersionNumber): TVersionNumber;
  {Operator overload that perfoms implicit conversion of a TPJVersionNumber
  (from PJVersionInfo unit) to a TVersionNumber.
    @param Ver [in] Version number to be converted.
    @return Converted version number.
  }
begin
  Result.V1 := Ver.V1;
  Result.V2 := Ver.V2;
  Result.V3 := Ver.V3;
  Result.V4 := Ver.V4;
end;

class operator TVersionNumber.LessThan(Ver1, Ver2: TVersionNumber): Boolean;
  {Operator overload that compares two version numbers to check if first is
  less than second.
    @param Ver1 [in] First version number.
    @param Ver2 [in] Second version number.
    @return True if Ver1 < Ver2, False otherwise.
  }
begin
  Result := not (Ver1 >= Ver2);
end;

class operator TVersionNumber.LessThanOrEqual(Ver1,
  Ver2: TVersionNumber): Boolean;
  {Operator overload that compares two version numbers to check if first is
  less than or equal to the second.
    @param Ver1 [in] First version number.
    @param Ver2 [in] Second version number.
    @return True if Ver1 <= Ver2, False otherwise.
  }
begin
  Result := not (Ver1 > Ver2);
end;

class operator TVersionNumber.NotEqual(Ver1, Ver2: TVersionNumber): Boolean;
  {Operator overload that compares two version numbers to check for inequality.
    @param Ver1 [in] First version number.
    @param Ver2 [in] Second version number.
    @return True if Ver1 <> Ver2, False otherwise.
  }
begin
  Result := not (Ver1 = Ver2);
end;

class function TVersionNumber.Nul: TVersionNumber;
  {Creates a nul version number with all fields set to zero.
    @return Required nul record.
  }
begin
  Result.V1 := 0;
  Result.V2 := 0;
  Result.V3 := 0;
  Result.V4 := 0;
end;

end.

