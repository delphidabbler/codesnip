{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2016, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Date and time utilities.
}


unit CS.Utils.Dates;


interface

uses
  SysUtils,
  DateUtils {in interface for inlining},
  Windows;

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
    ///  <summary>Tries to convert a string in ISO 8601 format to its
    ///  constituent parts.</summary>
    ///  <param name="Str"><c>string</c> [in] String in ISO 8601 format.</param>
    ///  <param name="Y"><c>Word</c> [out] Year component of date.</param>
    ///  <param name="M"><c>Word</c> [out] Month component of date.</param>
    ///  <param name="D"><c>Word</c> [out] Day component of date.</param>
    ///  <param name="H"><c>Word</c> [out] Hour component of date.</param>
    ///  <param name="N"><c>Word</c> [out] Minute component of date.</param>
    ///  <param name="MS"><c>Word</c> [out] Millisecond component of date.
    ///  </param>
    ///  <returns><c>Boolean</c>. <c>True</c> if <c>Str</c> is a valid ISO
    ///  8601 format date or <c>False</c> if <c>Str</c> is invalid.</returns>
    ///  <remarks>
    ///  <para>When conversion fails and <c>False</c> is returned none of the
    ///  values of the out parameters are defined.</para>
    ///  <para>We only recognise ISO 8601 strings in form
    ///  YYYY-MM-DD&quot;T&quot;HH:MM:SS&quot;Z&quot;,
    ///  YYYY-MM-DD&quot;T&quot;HH:MM:SS.XXX&quot;Z&quot; or
    ///  YYYY-MM-DD&quot;T&quot;HH:MM:SS,XXX&quot;Z&quot;.</para>
    ///  </remarks>
    class function TryConvertISO8601String(const Str: string;
      out Y, M, D, H, N, S, MS: Word): Boolean; static;
    ///  <summary>Tries to convert a string in SQL <c>DATETIME</c> format to its
    ///  constituent parts.</summary>
    ///  <param name="Str"><c>string</c> [in] String in SQL <c>DATETIME</c>
    ///  format.</param>
    ///  <param name="Y"><c>Word</c> [out] Year component of date.</param>
    ///  <param name="M"><c>Word</c> [out] Month component of date.</param>
    ///  <param name="D"><c>Word</c> [out] Day component of date.</param>
    ///  <param name="H"><c>Word</c> [out] Hour component of date.</param>
    ///  <param name="N"><c>Word</c> [out] Minute component of date.</param>
    ///  <returns><c>Boolean</c>. <c>True</c> if <c>Str</c> is a valid SQL
    ///  <c>DATETIME</c> format date or <c>False</c> if <c>Str</c> is invalid.
    ///  </returns>
    ///  <remarks>When conversion fails and <c>False</c> is returned none of the
    ///  values of the out parameters are defined.</remarks>
    class function TryConvertSQLDateTime(const SQLDateTime: string;
      out Y, M, D, H, N, S: Word): Boolean; static;
  public
    ///  <summary>Constructs record from a TDateTime that is assumed to be a UTC
    ///  date.</summary>
    ///  <param name="UTC"><c>TDateTime</c> [in] UTC date time. Must be a valid
    ///  <c>TDateTime</c>.</param>
    ///  <param name="RoundToSec"><c>Boolean</c> [in] Specifies whether
    ///  <c>UTC</c> is to be rounded to the nearest second (<c>True</c>) or not
    ///  (<c>False</c>).</param>
    constructor Create(const UTC: TDateTime; const RoundToSec: Boolean = False);
      overload;
    ///  <summary>Constructs record from a date/time that is assumed to be in
    ///  UTC.</summary>
    ///  <param name="Year"><c>Word</c> [in] The year.</param>
    ///  <param name="Month"><c>Word</c> [in] The month of the year.</param>
    ///  <param name="Day"><c>Word</c> [in] The day of the month.</param>
    ///  <param name="Hour"><c>Word</c> [in] Hour of the day (optional).</param>
    ///  <param name="Minute"><c>Word</c> [in] The minute of the hour
    ///  (optional).</param>
    ///  <param name="Second"><c>Word</c> [in] The second of the minute
    ///  (optional).</param>
    ///  <param name="MS"><c>Word</c> [in] The millisecond of the second
    ///  (optional).</param>
    ///  <exception><c>EConvertError</c> raised if any parameter is invalid.
    ///  </exception>
    constructor Create(const Year, Month, Day: Word; const Hour: Word = 0;
      const Minute: Word = 0; const Second: Word = 0; const MS: Word = 0);
      overload;
    ///  <summary>Creates a new record from a local date / time in the current
    ///  locale. The date is converted to UTC.</summary>
    ///  <param name="DT"><c>TDateTime</c> [in] Local date time. Must be a valid
    ///  <c>TDateTime</c>.</param>
    ///  <param name="RoundToSec"><c>Boolean</c> [in] Specifies whether
    ///  <c>UTC</c> is to be rounded to the nearest second (<c>True</c>) or not
    ///  (<c>False</c>).</param>
    ///  <returns>New TUTCDateTime record.</returns>
    class function CreateFromLocalDateTime(const DT: TDateTime;
      const RoundToSec: Boolean = False): TUTCDateTime; static;
    ///  <summary>Creates a new record containing a null date time value, with
    ///  no meaningful value.</summary>
    ///  <returns>Required new null record.</returns>
    class function CreateNull: TUTCDateTime; static; inline;
    ///  <summary>Creates a new UTC record from an ISO8601 string.</summary>
    ///  <param name="Str"><c>string</c> [in] String containing date/time in
    ///  ISO8601 format.</param>
    ///  <returns>Required UTC record.</returns>
    ///  <exception><c>EConvertError</c> raised if <c>Str</c> is not a valid ISO
    ///  8601 date.</exception>
    ///  <remarks>See the private <c>TryConvertISO8601String</c> method for
    ///  details of supported ISO 8601 formats.</remarks>
    class function CreateFromISO8601String(const Str: string): TUTCDateTime;
      static;
    ///  <summary>Creates a new UTC record from a valid SQL date / time string.
    ///  </summary>
    ///  <param name="Str"><c>string</c> [in] String containing date/time in SQL
    ///  <c>DATETIME</c> format.</param>
    ///  <returns>Required UTC record.</returns>
    ///  <exception><c>EConvertError</c> raised if <c>Str</c> is not a valid SQL
    ///  <c>DATETIME</c> date.</exception>
    class function CreateFromSQLDateTime(const SQLDateTime: string):
      TUTCDateTime; static;
    ///  <summary>Creates a new record for the current date and time, converted
    ///  to UTC.</summary>
    ///  <param name="RoundToSec"><c>Boolean</c> [in] Specifies whether the time
    ///  is to be rounded to the nearest second (<c>True</c>) or not
    ///  (<c>False</c>).</param>
    ///  <returns>Required new record.</returns>
    class function Now(const RoundToSec: Boolean = False): TUTCDateTime; static;
    ///  <summary>Checks if a string is a valid ISO 8601 string.</summary>
    ///  <param name="Str"><c>string</c> [in] Date string to be checked.</param>
    ///  <returns><c>True</c> if <c>Str</c> is a valid ISO 8601 date,
    ///  <c>False</c> if not.</returns>
    ///  <remarks>See the private <c>TryConvertISO8601String</c> method for
    ///  details of supported ISO 8601 formats.</remarks>
    class function IsValidISO8601String(const Str: string): Boolean; static;
    ///  <summary>Converts record into a <c>TDateTime</c> value.</summary>
    ///  <returns>Required <c>TDateTime</c> value.</returns>
    function ToDateTime: TDateTime; inline;
    ///  <summary>Converts record into a valid ISO 8601 string, optionally
    ///  rounded to the nearest second.</summary>
    ///  <param name="RoundToSec"><c>Boolean</c> [in] Specifies whether the time
    ///  is to be rounded to the nearest second (<c>True</c>) or not
    ///  (<c>False</c>).</param>
    ///  <returns>Required ISO 8601 format string.</returns>
    function ToISO8601String(const RoundToSec: Boolean = False): string;
    ///  <summary>Converts record into a valid RFC 1123 string.</summary>
    ///  <returns>Required RFC 1123 format string.</returns>
    function ToRFC1123String: string;
    ///  <summary>Converts record into a formatted string as specified by a
    ///  given template, using symbols from the current locale.</summary>
    ///  <param name="AFormat"><c>string</c> [in] Template string that specifies
    ///  required date format.</param>
    ///  <returns>Required formatted string.</returns>
    ///  <remarks><c>AFormat</c> uses the same formatting characters as the
    ///  VCL's <c>FormatDateTime</c> function.</remarks>
    function ToString(const AFormat: string): string; overload;
    ///  <summary>Converts record into a formatted string as specified by a
    ///  given template, using given formatting symbols.</summary>
    ///  <param name="AFormat"><c>string</c> [in] Template string that specifies
    ///  required date format.</param>
    ///  <param name="AFormatSettings"><c>TFormatSettings</c> [in] Specifies
    ///  the symbols to use in the formatted string.</param>
    ///  <returns>Required formatted string.</returns>
    ///  <remarks><c>AFormat</c> uses the same formatting characters as the
    ///  VCL's <c>FormatDateTime</c> function.</remarks>
    function ToString(const AFormat: string;
      const AFormatSettings: TFormatSettings): string; overload;
    ///  <summary>Checks if the UTC date is null.</summary>
    ///  <returns><c>True</c> if null, <c>False</c> if not.</returns>
    function IsNull: Boolean;
    ///  <summary>Rounds the UTC date to the nearest second.</summary>
    ///  <returns>A new record containing the rounded UTC date.</returns>
    function RoundToNearestSecond: TUTCDateTime;
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
  Types,
  UUtils;

{ TUTCDateTime }

constructor TUTCDateTime.Create(const UTC: TDateTime;
  const RoundToSec: Boolean);
begin
  fValue := UTC;
  if not RoundToSec then
    fValue := UTC
  else
    fValue := RoundDTToNearestSec(UTC);
end;

constructor TUTCDateTime.Create(const Year, Month, Day, Hour, Minute, Second,
  MS: Word);
begin
  // Will raise EConvertError if any parameter is invalid
  fValue := EncodeDateTime(Year, Month, Day, Hour, Minute, Second, MS);
end;

class function TUTCDateTime.CreateFromISO8601String(
  const Str: string): TUTCDateTime;
var
  Y, M, D, H, N, S, MS: Word;
begin
  if not TryConvertISO8601String(Str, Y, M, D, H, N, S, MS) then
    raise EConvertError.CreateFmt('"%s" is not a valid ISO8601 date', [Str]);
  Result := TUTCDateTime.Create(Y, M, D, H, N, S, MS);
end;

class function TUTCDateTime.CreateFromLocalDateTime(const DT: TDateTime;
  const RoundToSec: Boolean): TUTCDateTime;
begin
  Result := TUTCDateTime.Create(
    TTimeZone.Local.ToUniversalTime(DT), RoundToSec
  );
end;

class function TUTCDateTime.CreateFromSQLDateTime(
  const SQLDateTime: string): TUTCDateTime;
var
  Y, M, D, H, N, S: Word;
begin
  if not TryConvertSQLDateTime(SQLDateTime, Y, M, D, H, N, S) then
    raise EConvertError.CreateFmt(
      '"%s" is not a valid SQL DATETIME value', [SQLDateTime]
    );
  Result := TUTCDateTime.Create(Y, M, D, H, N, S, 0);
end;

class function TUTCDateTime.CreateNull: TUTCDateTime;
begin
  Result := TUTCDateTime.Create(0.0);
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
var
  Y, M, D, H, N, S, MS: Word;
begin
  Result := TryConvertISO8601String(Str, Y, M, D, H, N, S, MS);
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
  Result := TUTCDateTime.CreateFromLocalDateTime(SysUtils.Now, RoundToSec);
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
  Result := TUTCDateTime.Create(RoundDTToNearestSec(fValue));
end;

function TUTCDateTime.ToDateTime: TDateTime;
begin
  Result := fValue;
end;

function TUTCDateTime.ToISO8601String(const RoundToSec: Boolean): string;
begin
  if RoundToSec then
    Result := FormatDateTime(
      'yyyy"-"mm"-"dd"T"hh":"nn":"ss"Z"', RoundDTToNearestSec(fValue)
    )
  else
    Result := FormatDateTime(
      'yyyy"-"mm"-"dd"T"hh":"nn":"ss"."zzz"Z"', fValue
    );
end;

function TUTCDateTime.ToRFC1123String: string;
const
  // Pattern to create RFC1123 date formats
  RFC1123Pattern = 'ddd, dd mmm yyyy HH":"nn":"ss "GMT"';
begin
  Result := FormatDateTime(RFC1123Pattern, RoundDTToNearestSec(fValue));
end;

function TUTCDateTime.ToString(const AFormat: string): string;
begin
  Result := FormatDateTime(AFormat, fValue);
end;

function TUTCDateTime.ToString(const AFormat: string;
  const AFormatSettings: TFormatSettings): string;
begin
  Result := FormatDateTime(AFormat, fValue, AFormatSettings);
end;

class function TUTCDateTime.TryConvertISO8601String(const Str: string; out Y, M,
  D, H, N, S, MS: Word): Boolean;
begin
  if Length(Str) < 20 then
    Exit(False);
  if not TryStrToWord(Copy(Str, 1, 4), Y) or not
    TryStrToWord(Copy(Str, 6, 2), M) or not
    TryStrToWord(Copy(Str, 9, 2), D) or not
    TryStrToWord(Copy(Str, 12, 2), H) or not
    TryStrToWord(Copy(Str, 15, 2), N) or not
    TryStrToWord(Copy(Str, 18, 2), S) then
    Exit(False);
  if CharInSet(Str[20], ['.', ',']) then
  begin
    if (Length(Str) < 24) or not TryStrToWord(Copy(Str, 21, 3), MS) then
      Exit(False);
  end
  else
    MS := 0;
  Result := IsValidDateTime(Y, M, D, H, N, S, MS);
end;

class function TUTCDateTime.TryConvertSQLDateTime(const SQLDateTime: string;
  out Y, M, D, H, N, S: Word): Boolean;
begin
  // Expected string format is YYYY-MM-DD HH:MM:SS
  // We'll accept any date and hour delimiters, but all values MUST be provided.
  if Length(SQLDateTime) <> 19 then  // Length('YYYY-MM-DD HH:MM:SS') = 19
    Exit(False);
  Result := TryStrToWord(Copy(SQLDateTime, 1, 4), Y) and
    TryStrToWord(Copy(SQLDateTime, 6, 2), M) and
    TryStrToWord(Copy(SQLDateTime, 9, 2), D) and
    TryStrToWord(Copy(SQLDateTime, 12, 2), H) and
    TryStrToWord(Copy(SQLDateTime, 15, 2), N) and
    TryStrToWord(Copy(SQLDateTime, 18, 2), S) and
    IsValidDateTime(Y, M, D, H, N, S, 0);
end;

end.

