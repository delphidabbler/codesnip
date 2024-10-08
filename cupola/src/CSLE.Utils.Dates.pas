{
  This Source Code Form is subject to the terms of the Mozilla Public License,
  v. 2.0. If a copy of the MPL was not distributed with this file, You can
  obtain one at https://mozilla.org/MPL/2.0/

  Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).

  Data type that encapsulates a date time in UTC.

  NOTE:
    This code is based on CS.Utils.Dates.pas from the abandoned CodeSnip
    pavilion branch.
    See https://tinyurl.com/ym48fv4m
}

unit CSLE.Utils.Dates;

interface

uses
  System.SysUtils,
  System.DateUtils;

type
  ///  <summary>Encapsulates a UTC Date/Time along with various operations on
  ///  it.</summary>
  TUTCDateTime = record
  strict private
    var
      ///  <summary>Encapsulated UTC date/time value.</summary>
      fValue: TDateTime;

    ///  <summary>Rounds a given <c>TDateTime</c> to nearest second and returns
    ///  result.</summary>
    class function RoundDTToNearestSec(const DT: TDateTime): TDateTime; static;

  public
    ///  <summary>Constructs record from a <c>TDateTime</c> value.</summary>
    ///  <param name="DT">[in] Date time. Must be a valid c>TDateTime</c>.
    ///  </param>
    ///  <param name="IsUTC">[in] Indicates whether <c>DT</c> specifies a UTC
    ///  date/time (<c>True</c>) or a local time (<c>False</c>).</param>
    ///  <param name="RoundToSec">[in] Specifies whether <c>DT</c> is to be
    ///  rounded to the nearest second (<c>True</c>) or not (<c>False</c>).
    ///  </param>
    constructor Create(const DT: TDateTime; const IsUTC: Boolean;
      const RoundToSec: Boolean = False); overload;

    ///  <summary>Creates a new record containing a null date time value, with
    ///  no meaningful value.</summary>
    ///  <returns>Required new null <c>TUTCDateTime</c> record.</returns>
    class function CreateNull: TUTCDateTime; static; inline;

    ///  <summary>Creates a new UTC record from an ISO8601 string.</summary>
    ///  <param name="Str">[in] String containing date/time in ISO8601 format.
    ///  </param>
    ///  <returns><c>TUTCDateTime</c> containing date in UTC format.</returns>
    ///  <exception><c>EDateTimeException</c> raised if <c>Str</c> is not a
    ///  valid ISO8601 date.</exception>
    ///  <remarks>The date formats considered valid are those that
    ///  <c>System.DateUtils.ISO8601ToDate</c> considers valid. Be warned that
    ///  this is not entirely consistent! It is safest to use the full
    ///  <c>2023-07-01T10:17:25.123Z</c> or
    ///  <c>2023-07-01T10:17:25.123+04:00</c> style of formatting. You can omit
    ///  milliseconds and preceeding dot. Hyphens and colons can be omitted. The
    ///  <c>2023-07-01</c> format is acceptable and implies midnight in the UTC
    ///  (Zulu) time zone.</remarks>
    class function CreateFromISO8601String(const Str: string): TUTCDateTime;
      static;

    ///  <summary>Creates a new record for the current date and time, converted
    ///  to UTC.</summary>
    ///  <param name="RoundToSec">[in] Specifies whether the time is to be
    ///  rounded to the nearest second (<c>True</c>) or not (<c>False</c>).
    ///  </param>
    ///  <returns>Required new record.</returns>
    class function Now(const RoundToSec: Boolean = False): TUTCDateTime; static;

    ///  <summary>Checks if a string is a valid ISO 8601 string.</summary>
    ///  <remarks>The date formats considered valid are those that
    ///  <c>System.DateUtils.ISO8601ToDate</c> considers valid. See the remarks
    ///  in the <c>CreateFromISO8601String</c> doc comments for more
    ///  information.</remarks>
    ///  <param name="Str">[in] Date string to be checked.</param>
    ///  <returns><c>True</c> if <c>Str</c> is a valid ISO 8601 date,
    ///  <c>False</c> if not.</returns>
    class function IsValidISO8601String(const Str: string): Boolean; static;

    ///  <summary>Checks if the UTC date is null.</summary>
    ///  <returns><c>True</c> if null, <c>False</c> if not.</returns>
    function IsNull: Boolean;

    ///  <summary>Rounds the UTC date to the nearest second.</summary>
    ///  <returns>A new <c>TUTCDateTime</c> record containing the rounded UTC
    ///  date.</returns>
    function RoundToNearestSecond: TUTCDateTime;

    ///  <summary>Converts record into a <c>TDateTime</c> value.</summary>
    ///  <returns>Required <c>TDateTime</c> value.</returns>
    function ToDateTime: TDateTime; inline;

    ///  <summary>Converts record into a formatted string as specified by a
    ///  given template, using symbols from the current locale.</summary>
    ///  <param name="AFormat">[in] Template string that specifies required date
    ///  format.</param>
    ///  <returns>Required formatted string.</returns>
    ///  <remarks><c>AFormat</c> uses the same formatting characters as the
    ///  VCL's <c>FormatDateTime</c> function.</remarks>
    function ToString(const AFormat: string): string; overload;

    ///  <summary>Converts record into a formatted string as specified by a
    ///  given template, using given formatting symbols.</summary>
    ///  <param name="AFormat">[in] Template string that specifies required date
    ///  format.</param>
    ///  <param name="AFormatSettings">[in] Specifies the symbols to use in the
    ///  formatted string.</param>
    ///  <returns>Required formatted string.</returns>
    ///  <remarks><c>AFormat</c> uses the same formatting characters as the
    ///  VCL's <c>FormatDateTime</c> function.</remarks>
    function ToString(const AFormat: string;
      const AFormatSettings: TFormatSettings): string; overload;

    ///  <summary>Converts record into a valid ISO 8601 string, optionally
    ///  rounded to the nearest second.</summary>
    ///  <param name="RoundToSec">[in] Specifies whether the time is to be
    ///  rounded to the nearest second (<c>True</c>) or not (<c>False</c>).
    ///  </param>
    ///  <returns>Required ISO 8601 format string.</returns>
    function ToISO8601String(const RoundToSec: Boolean = False): string;

    ///  <summary>Compares two UTC dates for equality.</summary>
    class operator Equal(const Left, Right: TUTCDateTime): Boolean;

    ///  <summary>Compares two UTC dates for inequality.</summary>
    class operator NotEqual(const Left, Right: TUTCDateTime): Boolean;

    ///  <summary>Checks if UTC date <c>Left</c> is greater than <c>Right</c>.
    ///  </summary>
    class operator GreaterThan(const Left, Right: TUTCDateTime): Boolean;

    ///  <summary>Checks if UTC date <c>Left</c> is greater than or equal to
    ///  <c>Right</c>.</summary>
    class operator GreaterThanOrEqual(const Left, Right: TUTCDateTime): Boolean;

    ///  <summary>Checks if UTC date <c>Left</c> is less than <c>Right</c>.
    ///  </summary>
    class operator LessThan(const Left, Right: TUTCDateTime): Boolean;

    ///  <summary>Checks if UTC date <c>Left</c> is less than or equal to
    ///  <c>Right</c>.</summary>
    class operator LessThanOrEqual(const Left, Right: TUTCDateTime): Boolean;
  end;

implementation

uses
  System.Types,
  CSLE.Utils.Conversions;

{ TUTCDateTime }

constructor TUTCDateTime.Create(const DT: TDateTime; const IsUTC: Boolean;
  const RoundToSec: Boolean);
begin
  if IsUTC then
    fValue := DT
  else
    fValue := TTimeZone.Local.ToUniversalTime(DT);

  if RoundToSec then
    fValue := RoundDTToNearestSec(fValue);
end;

class function TUTCDateTime.CreateFromISO8601String(const Str: string):
  TUTCDateTime;
begin
  // Following call will raise EDateTimeException on invalid ISO8601 date string
  var DT := System.DateUtils.ISO8601ToDate(Str, True);  // returns UTC
  Result := TUTCDateTime.Create(DT, True);
end;

class function TUTCDateTime.CreateNull: TUTCDateTime;
begin
  Result := TUTCDateTime.Create(0.0, True);
end;

class operator TUTCDateTime.Equal(const Left, Right: TUTCDateTime): Boolean;
begin
  Result := SameDateTime(Left.fValue, Right.fValue);
end;

class operator TUTCDateTime.GreaterThan(const Left,
  Right: TUTCDateTime): Boolean;
begin
  Result := CompareDateTime(Left.fValue, Right.fValue) = GreaterThanValue;
end;

class operator TUTCDateTime.GreaterThanOrEqual(const Left,
  Right: TUTCDateTime): Boolean;
begin
  Result := CompareDateTime(Left.fValue, Right.fValue) <> LessThanValue;
end;

function TUTCDateTime.IsNull: Boolean;
begin
  Result := SameDateTime(fValue, 0.0);
end;

class function TUTCDateTime.IsValidISO8601String(const Str: string): Boolean;
begin
  var Value: TDateTime;
  Result := System.DateUtils.TryISO8601ToDate(Str, Value);
end;

class operator TUTCDateTime.LessThan(const Left, Right: TUTCDateTime): Boolean;
begin
  Result := CompareDateTime(Left.fValue, Right.fValue) = LessThanValue;
end;

class operator TUTCDateTime.LessThanOrEqual(const Left,
  Right: TUTCDateTime): Boolean;
begin
  Result := CompareDateTime(Left.fValue, Right.fValue) <> GreaterThanValue;
end;

class operator TUTCDateTime.NotEqual(const Left, Right: TUTCDateTime): Boolean;
begin
  Result := not SameDateTime(Left.fValue, Right.fValue);
end;

class function TUTCDateTime.Now(const RoundToSec: Boolean): TUTCDateTime;
begin
  // System.SysUtils.Now returns a local time, so convert to UTC
  Result := TUTCDateTime.Create(System.SysUtils.Now, False, RoundToSec);
end;

class function TUTCDateTime.RoundDTToNearestSec(const DT: TDateTime): TDateTime;
begin
  if MilliSecondOf(DT) >= 500 then
    Result := IncSecond(DT)
  else
    Result := DT;
  Result := RecodeMilliSecond(Result, 0);
end;

function TUTCDateTime.RoundToNearestSecond: TUTCDateTime;
begin
  Result := TUTCDateTime.Create(fValue, True, True);
end;

function TUTCDateTime.ToDateTime: TDateTime;
begin
  Result := fValue;
end;

function TUTCDateTime.ToISO8601String(const RoundToSec: Boolean): string;
begin
  if RoundToSec then
  begin
    // Don't use DateToISO8601 since it won't truncate millis
    Result := FormatDateTime(
      'yyyy"-"mm"-"dd"T"hh":"nn":"ss"Z"', RoundDTToNearestSec(fValue)
    );
  end
  else
    Result := System.DateUtils.DateToISO8601(fValue, True);
end;

function TUTCDateTime.ToString(const AFormat: string;
  const AFormatSettings: TFormatSettings): string;
begin
  Result := FormatDateTime(AFormat, fValue, AFormatSettings);
end;

function TUTCDateTime.ToString(const AFormat: string): string;
begin
  Result := FormatDateTime(AFormat, fValue);
end;

end.
