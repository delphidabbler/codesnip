{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2020, Peter Johnson (gravatar.com/delphidabbler).
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
  strict private
    ///  <summary>Converts version number to a string.</summary>
    function ToString: string;
  public
    type
      ///  <summary>Enumeration that specifies whether intervals used in range
      ///  checks are open, closed or half-open.</summary>
      ///  <remarks>
      ///  <para><c>iepOpen</c> - open interval (x,y)</para>
      ///  <para><c>iepHalfOpenLo</c> - half open interval (x,y]</para>
      ///  <para><c>iepHalfOpenHi</c> - half open interval [x,y)</para>
      ///  <para><c>iepClosed</c> - closed interval [x,y]</para>
      ///  </remarks>
      TIntervalEndPoints = (iepOpen, iepHalfOpenLo, iepHalfOpenHi, iepClosed);
  public
    V1: Word;   // Major version number
    V2: Word;   // Minor version number
    V3: Word;   // Revision version number
    V4: Word;   // Build number
    ///  <summary>Checks if this version number is Null.</summary>
    ///  <returns>Boolean. True if null, False otherwise.</returns>
    function IsNull: Boolean;
    class function Nul: TVersionNumber; static;
      {Creates a nul version number with all fields set to zero.
        @return Required nul record.
      }
    ///  <summary>Checks if the current version number is contained in the range
    ///  Lo..Hi with range endpoints determined by EndPoints.</summary>
    ///  <remarks>Note that when Lo=Hi the intervals (Lo,Hi), (Lo,Hi] and
    ///  [Lo,Hi) represent the empty set, while [Lo,Hi] = {Lo}. When Lo &gt; Hi
    ///  all interval types represent the empty set.</remarks>
    function IsInRange(const Lo, Hi: TVersionNumber;
      const EndPoints: TIntervalEndPoints): Boolean;
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
    class operator Implicit(Ver: TVersionNumber): string;
      {Operator overload that performs implicit conversion of a version number
      to a string in dotted quad format.
        @param Ver [in] Version number to be converted.
        @return Dotted quad string.
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
  UExceptions,
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
  Result := Ver.ToString;
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

function TVersionNumber.IsInRange(const Lo, Hi: TVersionNumber;
  const EndPoints: TIntervalEndPoints): Boolean;
begin
  case EndPoints of
    iepOpen: Result := (Lo < Self) and (Self < Hi);
    iepHalfOpenLo: Result := (Lo < Self) and (Self <= Hi);
    iepHalfOpenHi: Result := (Lo <= Self) and (Self < Hi);
    iepClosed: Result := (Lo <= Self) and (Self <= Hi);
    else
      raise EBug.Create('TVersionNumber.IsInRange: invalid Kind parameter');
  end;
end;

function TVersionNumber.IsNull: Boolean;
begin
  Result := Self = TVersionNumber.Nul;
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

class operator TVersionNumber.Implicit(Ver: TVersionNumber): string;
  {Operator overload that performs implicit conversion of a version number to a
  string in dotted quad format.
    @param Ver [in] Version number to be converted.
    @return Dotted quad string.
  }
begin
  Result := Ver.ToString;
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

function TVersionNumber.ToString: string;
begin
  Result := Format('%d.%d.%d.%d', [V1, V2, V3, V4]);
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

