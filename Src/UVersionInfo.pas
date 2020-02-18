{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides details of the application's version information and provides a
 * record used to manipulate version numbers.
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
  public
    V1: Word;   // Major version number
    V2: Word;   // Minor version number
    V3: Word;   // Revision version number
    V4: Word;   // Build number
    class function Nul: TVersionNumber; static;
      {Creates a nul version number with all fields set to zero.
        @return Required nul record.
      }
    ///  <summary>Attempts to convert a string to a version number.</summary>
    ///  <param name="S">string [in] String to convert.</param>
    ///  <param name="V">Word [out] Converted version number.</param>
    ///  <returns>Boolean. True if conversion succeeded, False otherwise.
    ///  </returns>
    ///  <remarks>String must represent a dotted quad of non-negative integers,
    ///  separated by dots, where each integer must be representable as a Word.
    ///  </remarks>
    class function TryStrToVersionNumber(const S: string;
      out V: TVersionNumber): Boolean; static;
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
    class operator Explicit(Ver: TVersionNumber): TPJVersionNumber;
      {Operator overload that performs an explicit conversion of a
      TVersionNumber to a TPJVersionNumber (from PJVersionInfo unit).
        @param Ver [in] Version number to be converted.
        @return Converted version number.
      }
    class operator Explicit(Ver: TVersionNumber): string;
      {Operator overload that performs explicit conversion of a version number
      to a string in dotted quad format.
        @param Ver [in] Version number to be converted.
        @return Dotted quad string.
      }
  end;

  {
  TVersionInfo:
    Extracts version information from version resources. Uses TPJVersionInfo to
    perform the actual work.
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
  UIStringList,
  UUtils;


{ TVersionInfo }

class function TVersionInfo.FileVersionNumberStr: string;
  {Dotted quad representation of file version number from fixed file
  information.
    @return Version number string in form 9.9.9.9.
  }
begin
  with TPJVersionInfo.Create(nil) do
    try
      // casts TPJVersionNumber directly to string
      Result := FileVersionNumber;
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
      Result := ProductVersionNumber; // implicit type cast
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
  with TPJVersionInfo.Create(nil) do
    try
      // casts TPJVersionNumber directly to string
      Result := ProductVersionNumber;
    finally
      Free;
    end;
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
  Result := TPJVersionNumber(Ver1) = TPJVersionNumber(Ver2);
end;

class operator TVersionNumber.Explicit(Ver: TVersionNumber): TPJVersionNumber;
  {Operator overload that performs an explicit conversion of a
  TVersionNumber to a TPJVersionNumber (from PJVersionInfo unit).
    @param Ver [in] Version number to be converted.
    @return Converted version number.
  }
begin
  Result.V1 := Ver.V1;
  Result.V2 := Ver.V2;
  Result.V3 := Ver.V3;
  Result.V4 := Ver.V4;
end;

class operator TVersionNumber.Explicit(Ver: TVersionNumber): string;
  {Operator overload that performs explicit conversion of a version number to a
  string in dotted quad format.
    @param Ver [in] Version number to be converted.
    @return Dotted quad string.
  }
begin
  Result := Format('%d.%d.%d.%d', [Ver.V1, Ver.V2, Ver.V3, Ver.V4]);
end;

class operator TVersionNumber.GreaterThan(Ver1, Ver2: TVersionNumber): Boolean;
  {Operator overload that compares two version numbers to check if first is
  greater than second.
    @param Ver1 [in] First version number.
    @param Ver2 [in] Second version number.
    @return True if Ver1 > Ver2, False otherwise.
  }
begin
  Result := TPJVersionNumber(Ver1) > TPJVersionNumber(Ver2);
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
  Result := TPJVersionNumber(Ver1) >= TPJVersionNumber(Ver2);
end;

class operator TVersionNumber.Implicit(Str: string): TVersionNumber;
  {Operator overload that performs implicit conversion of a dotted quad string
  to a TVersionNumber.
    @param Str [in] String to be converted. Must be in dotted quad format.
    @return Converted version number.
    @except EConvertError. raised if string in wrong format.
  }
resourcestring
  sError = '"%s" is not a valid version number';
begin
  if not TryStrToVersionNumber(Str, Result) then
    raise EConvertError.CreateFmt(sError, [Str]);
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
  Result := TPJVersionNumber(Ver1) < TPJVersionNumber(Ver2);
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
  Result := TPJVersionNumber(Ver1) <= TPJVersionNumber(Ver2);
end;

class operator TVersionNumber.NotEqual(Ver1, Ver2: TVersionNumber): Boolean;
  {Operator overload that compares two version numbers to check for inequality.
    @param Ver1 [in] First version number.
    @param Ver2 [in] Second version number.
    @return True if Ver1 <> Ver2, False otherwise.
  }
begin
  Result := TPJVersionNumber(Ver1) <> TPJVersionNumber(Ver2);
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

class function TVersionNumber.TryStrToVersionNumber(const S: string;
  out V: TVersionNumber): Boolean;
var
  Parts: IStringList; // contains parts of version number
begin
  Parts := TIStringList.Create(S, '.', False, True);
  while Parts.Count < 4 do
    Parts.Add('0');
  if not TryStrToWord(Parts[0], V.V1) then
    Exit(False);
  if not TryStrToWord(Parts[1], V.V2) then
    Exit(False);
  if not TryStrToWord(Parts[2], V.V3) then
    Exit(False);
  if not TryStrToWord(Parts[3], V.V4) then
    Exit(False);
  Result := True;
end;

end.

