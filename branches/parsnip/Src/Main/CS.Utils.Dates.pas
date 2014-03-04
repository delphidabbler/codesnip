{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2014, Peter Johnson (www.delphidabbler.com).
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
  TUTCDateTime = record
  strict private
    var
      fValue: TDateTime;
    class function RoundDTToNearestSec(const DT: TDateTime): TDateTime; static;
    class function TryConvertISO8601String(const Str: string;
      out Y, M, D, H, N, S, MS: Word): Boolean; static;
  public
    constructor Create(const UTC: TDateTime; const RoundToSec: Boolean = False);
      overload;
    constructor Create(const Year, Month, Day: Word; const Hour: Word = 0;
      const Minute: Word = 0; const Second: Word = 0; const MS: Word = 0);
      overload;
    class function CreateFromLocalDateTime(const DT: TDateTime;
      const RoundToSec: Boolean = False): TUTCDateTime; static;
    class function CreateNull: TUTCDateTime; static; inline;
    class function CreateFromISO8601String(const Str: string): TUTCDateTime;
      static;
    class function Now(const RoundToSec: Boolean = False): TUTCDateTime; static;
    class function IsValidISO8601String(const Str: string): Boolean; static;
    function ToDateTime: TDateTime; inline;
    function ToISO8601String(const RoundToSec: Boolean = False): string;
    function ToString(const AFormat: string): string; overload;
    function ToString(const AFormat: string;
      const AFormatSettings: TFormatSettings): string; overload;
    function IsNull: Boolean;
    function RoundToNearestSecond: TUTCDateTime;
    class operator Equal(const Left, Right: TUTCDateTime): Boolean;
    class operator NotEqual(const Left, Right: TUTCDateTime): Boolean;
    class operator GreaterThan(const Left, Right: TUTCDateTime): Boolean;
    class operator GreaterThanOrEqual(const Left, Right: TUTCDateTime): Boolean;
    class operator LessThan(const Left, Right: TUTCDateTime): Boolean;
    class operator LessThanOrEqual(const Left, Right: TUTCDateTime): Boolean;
  end;

  EUTCDateTime = class(Exception);

// TODO: re-implement some of these routines in terms of TUTCDataTime.

///  <summary>Creates a date stamp for current date in RFC1123 format.</summary>
///  <returns>string. Required date and time as date stamp in UTC/GMT.</returns>
function RFC1123DateStamp: string; inline;

///  <summary>Converts a date-time value in SQL format into a TDateTime.
///  </summary>
///  <param name="SQLDate">string [in] SQL format date-time value to be
///  converted.</param>
///  <returns>TDateTime. Converted value.</returns>
///  <remarks>SQLDate must be in YYYY-MM-DD hh:mm:ss format.</remarks>
function ParseSQLDateTime(const SQLDate: string): TDateTime;



implementation

uses
  Types,
  UUtils;

function RFC1123DateStamp: string;
const
  // Pattern to create RFC1123 date formats
  cRFC1123Pattern = 'ddd, dd mmm yyyy HH'':''nn'':''ss ''GMT''';
begin
  Result := FormatDateTime(cRFC1123Pattern, TUTCDateTime.Now.ToDateTime);
end;

function ParseSQLDateTime(const SQLDate: string): TDateTime;
begin
  Result := SysUtils.EncodeDate(
    SysUtils.StrToInt(Copy(SQLDate, 1, 4)),
    SysUtils.StrToInt(Copy(SQLDate, 6, 2)),
    SysUtils.StrToInt(Copy(SQLDate, 9, 2))
  )
  +
  SysUtils.EncodeTime(
    SysUtils.StrToInt(Copy(SQLDate, 12, 2)),
    SysUtils.StrToInt(Copy(SQLDate, 15, 2)),
    SysUtils.StrToInt(Copy(SQLDate, 18, 2)),
    0
  );
end;

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
    raise EConvertError.CreateFmt('"%s" is not a valid date', [Str]);
  Result := TUTCDateTime.Create(Y, M, D, H, N, S, MS);
end;

class function TUTCDateTime.CreateFromLocalDateTime(const DT: TDateTime;
  const RoundToSec: Boolean): TUTCDateTime;
begin
  Result := TUTCDateTime.Create(
    TTimeZone.Local.ToUniversalTime(DT), RoundToSec
  );
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
  // We only support ISO 8601 string in form YYYY-MM-DD"T"HH:MM:SS"Z" and
  // YYYY-MM-DD"T"HH:MM:SS.XXX"Z" or YYYY-MM-DD"T"HH:MM:SS,XXX"Z"
  // 1234567890 1 234567890123 4
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
  Result := True;
end;

end.

