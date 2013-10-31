{
 * URFC2822Date.pas
 *
 * Provides code for handling RFC2822 format dates. This version provides one
 * function to convert a RFC2822 date into a TDateTime.
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
 * The Original Code is URFC2822Date.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit URFC2822Date;


interface


uses
  // Project
  UExceptions;


function RFC2822DateToGMTDateTime(DateStr: string): TDateTime;
  {Converts a date in RFC2822 format into the corresponding TDateTime value in
  GMT (UTC).
    @param DateStr [in] RFC2822 date string to be converted.
    @return Converted date in GMT (UTC).
    @except ERFC2822Date raised if DateStr is malformed.
  }


type
  {
  ERFC2822Date:
    Type of exception raised when a RFC2822 date passed to
    RFC2822DateToGMTDateTime is malformed.
  }
  ERFC2822Date = class(ECodeSnip);


implementation


uses
  // Delphi
  SysUtils, StrUtils, DateUtils,
  // Project
  UIStringList, UStructs, UUtils;


function StrToWordInRange(const S: string; const Range: TRange;
  const ErrMsg: string): Word;
  {Converts a string representation of a number into a word value, checking it
  is in an expected range.
    @param S [in] String value to convert.
    @param Range [in] Range of valid values for the number.
    @param ErrMasg [in] Error message to use for any exception.
    @return Required number.
    @except ERFC2822Date raised if S does not contain a valid number or number
      is not in required range.
  }
var
  Value: Integer; // value of S as integer
begin
  if not TryStrToInt(S, Value) or not Range.Contains(Value) then
    raise ERFC2822Date.CreateFmt(ErrMsg, [S]);
  Result := Value;
end;

procedure ValidateDOW(const ADOW: string);
  {Validates text storing a day of the week as a valid day name. Returns if day
  of week is valid or raises exception if not.
    @param ADOW [in] Text to be checked.
    @except ERFC2822Date raised if ADOW is not a valid day name.
  }
resourcestring
  sErrMsg = 'Invalid day of week: "%s"';  // error message
const
  // Map of day numbers to day names
  cDOWs: array[1..7] of string = (
    'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'
  );
var
  DOW: string;  // reference each day of week
begin
  for DOW in cDOWs do
    if DOW = ADOW then
      Exit;
  raise ERFC2822Date.CreateFmt(sErrMsg, [ADOW]);
end;

function GetDay(const ADayStr: string): Word;
  {Converts day of month from string to Word value, checking value is valid for
  a day of month.
    @param ADayStr [in] String containing day number.
    @return Required day number.
    @except ERFC2822Date raised if ADayStr is not a valid number or is out of
      range for a day of month.
  }
resourcestring
  sErrMsg = 'Invalid day of month: "%s"'; // error message
begin
  Result := StrToWordInRange(ADayStr, TRange.Create(1, 31), sErrMsg);
end;

function GetMonth(const AMonthName: string): Word;
  {Converts month named in required RFC2822 format into a month number.
    @param AMonthName [in] String containing month name in RFC2822 format.
    @return Required month number.
    @except ERFC2822Date raised if AMonthName is not a valid RFC2822 month name.
  }
resourcestring
  sErrMsg = 'Invalid month: "%s"';  // error message
const
  // Map of month number to month names
  cMonths: array[1..12] of string = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
  );
var
  I: 1..12;   // loops through all elements of months map
begin
  for I := 1 to 12 do
  begin
    if cMonths[I] = AMonthName then
      Exit(I);
  end;
  raise ERFC2822Date.CreateFmt(sErrMsg, [AMonthName]);
end;

function GetYear(const AYearStr: string): Word;
  {Converts year from string to Word value, adjusting 2 and 3 digit years per
  RFC2822 specifications.
    @param AYearStr [in] String containing year.
    @return Required and adjusted, year number.
    @except ERFC2822Date raised if AYearStr is not a valid number or is out of
      range.
  }
resourcestring
  sErrMsg = 'Invalid year: "%s"'; // error message
var
  Value: Integer; // value of AYearStr as integer
begin
  // Check for valid Word value
  if not TryStrToInt(AYearStr, Value)
    or (Value < 0)
    or (Value > High(Word)) then
    raise ERFC2822Date.CreateFmt(sErrMsg, [AYearStr]);
  // Adjust for 2 and 3 digit years
  if TRange.Create(0, 49).Contains(Value) then
    Inc(Value, 2000)
  else if TRange.Create(50, 999).Contains(Value) then
    Inc(Value, 1900);
  Result := Value;
end;

function GetHour(const AHourStr: string): Word;
  {Converts hour from string to Word value, checking value is valid for an hour
  in the 24 hour clock.
    @param AHourStr [in] String containing hour.
    @return Required hour number.
    @except ERFC2822Date raised if AHourStr is not a valid number or is out of
      range for an hour.
  }
resourcestring
  sErrMsg = 'Invalid hour: "%s"'; // error message
begin
  Result := StrToWordInRange(AHourStr, TRange.Create(0, 23), sErrMsg);
end;

function GetMinute(const AMinStr: string): Word;
  {Converts minute from string to Word value, checking value is valid for a
  minute.
    @param AMinStr [in] String containing minute.
    @return Required minute number.
    @except ERFC2822Date raised if AMinStr is not a valid number or is out of
      range for a minute.
  }
resourcestring
  sErrMsg = 'Invalid minute: "%s"'; // error message
begin
  Result := StrToWordInRange(AMinStr, TRange.Create(0, 59), sErrMsg);
end;

function GetSecond(const ASecStr: string): Word;
  {Converts second from string to Word value, checking value is valid for a
  second.
    @param ASecStr [in] String containing second.
    @return Required second number.
    @except ERFC2822Date raised if ASecStr is not a valid number or is out of
      range for a second.
  }
resourcestring
  sErrMsg = 'Invalid second: "%s"'; // error message
begin
  Result := StrToWordInRange(ASecStr, TRange.Create(0, 59), sErrMsg)
end;

function GetTime(const TimeStr: string): TTime;
  {Converts a time string in format HH:MM:SS or HH:MM into a TTime value.
    @param TimeStr [in] Time string to converted.
    @return Converted value.
    @except ERFC2822Date raised if the time string is invalid.
  }
resourcestring
  sErrMsg = 'Invalid time: "%s"'; // error message
var
  Parts: IStringList;     // constituent parts of time: hr, min sec
  Hour, Min, Sec: Word;   // numeric values of hr, min, sec
const
  // Indices of time components in Parts
  HourPart = 0; // hour part (0..23)
  MinPart = 1;  // minute part (0..59)
  SecPart = 2;  // second part (0..59)
begin
  // Split time string to component parts: must be 2 or 3 parts (secs optional)
  Parts := TIStringList.Create(TimeStr, ':', True, False);
  if (Parts.Count < 2) or (Parts.Count > 3) then
    raise ERFC2822Date.CreateFmt(sErrMsg, [TimeStr]);
  // Get hour, minute and seconds as numbers
  Hour := GetHour(Parts[HourPart]);
  Min := GetMinute(Parts[MinPart]);
  if Parts.IsValidIndex(SecPart) then
    Sec := GetSecond(Parts[SecPart])
  else
    Sec := 0;   // if seconds no specified use 0
  Result := EncodeTime(Hour, Min, Sec, 0);
end;

procedure GetOffset(const AOffsetStr: string; out AHours, AMins: Integer);
  {Gets offset in minutes and seconds from GMT from an RFC2822 format offset
  string. Supports +9999, -9999 and text offsets. Unrecognised text offsets are
  parsed as -0000 per the RFC.
    @param AOffsetStr [in] The offset string to parse.
    @param AHours [out] Hours component of offset as signed number.
    @param AMins [out] Mins component of offset as signed number.
    @except ERFC2822Date raised if a malformed numeric offset is found.
  }
resourcestring
  // Error message
  sErrMsg = 'Invalid date offset: "%s"';
var
  Sign: Integer;  // sign of offset: -1 or +1
  I: Integer;     // loops thru elements of cObsZones table
const
  // Table of supported text time offsets
  cObsZones: array[1..10] of record
    Zone: string;   // offset time zone code
    Sign: Integer;  // sign of offset: -1 or +1
    Hour: Integer;  // offset hour: minutes always zero
  end = (
    (Zone: 'UT';  Sign: +1; Hour: 0),
    (Zone: 'GMT'; Sign: +1; Hour: 0),
    (Zone: 'EST'; Sign: -1; Hour: 5),
    (Zone: 'EDT'; Sign: -1; Hour: 4),
    (Zone: 'CST'; Sign: -1; Hour: 6),
    (Zone: 'CDT'; Sign: -1; Hour: 5),
    (Zone: 'MST'; Sign: -1; Hour: 7),
    (Zone: 'MDT'; Sign: -1; Hour: 6),
    (Zone: 'PST'; Sign: -1; Hour: 8),
    (Zone: 'PDT'; Sign: -1; Hour: 7)
  );
begin
  if (Length(AOffsetStr) = 5) and (CharInSet(AOffsetStr[1], ['+', '-'])) then
  begin
    // Offset in standard format ("+" | "-") 9999
    if AOffsetStr[1] = '-' then
      Sign := -1
    else
      Sign := 1;
    if not TryStrToInt(Copy(AOffsetStr, 2, 2), AHours)
      or not TryStrToInt(Copy(AOffsetStr, 4, 2), AMins) then
      raise ERFC2822Date.CreateFmt(sErrMsg, [AOffsetStr]);
  end
  else
  begin
    // Non-standard format: assume -0000 unless recognised code
    Sign := -1;
    AHours := 0;
    AMins := 0;
    for I := Low(cObsZones) to High(cObsZones) do
    begin
      if cObsZones[I].Zone = AOffsetStr then
      begin
        Sign := cObsZones[I].Sign;
        AHours := cObsZones[I].Hour;
      end;
    end;
  end;
  // Adjust offsets per sign
  AHours := Sign * AHours;
  AMins := Sign * AMins;
end;

function RFC2822DateToGMTDateTime(DateStr: string): TDateTime;
  {Converts a date in RFC2822 format into the corresponding TDateTime value in
  GMT (UTC).
    @param DateStr [in] RFC2822 date string to be converted.
    @return Converted date in GMT (UTC).
    @except ERFC2822Date raised if DateStr is malformed.
  }
resourcestring
  // Error messages
  sBadDate = 'Bad date format';
  sBadDay = 'Invalid day of month: "%s"';
var
  Parts: IStringList;               // component parts of date string
  Day, Month, Year: Word;           // day, month and year
  OffsetHours, OffsetMins: Integer; // offset hours and minutes
const
  // Indices of date components in Parts
  DOWPart = 0;    // day of week part (<day-name> ",")
  DayPart = 1;    // day number part
  MonthPart = 2;  // month number part
  YearPart = 3;   // year number part
  TimePart = 4;   // time component part (HH:MM:SS format)
  OffsetPart = 5; // offset from GMT part (+9999, -9999 or some valid text)
begin
  // Sun, 29 Aug 2010 13:06:03 +0000
  // Newlines allowed between fields: we compress to single white spaces
  DateStr := UUtils.CompressWhiteSpace(DateStr);
  // Split date into constituent parts
  Parts := TIStringList.Create(DateStr, ' ', False, True);
  if Parts.Count <> 6 then
    raise ERFC2822Date.Create(sBadDate);
  // Check day has a valid name: we don't check if it is correct for date since
  // not clear whether day should be correct before or after offset is applied.
  ValidateDOW(Copy(Parts[DOWPart], 1, 3));
  // Extract Day, Month and Year
  Day := GetDay(Parts[DayPart]);
  Month := GetMonth(Parts[MonthPart]);
  Year := GetYear(Parts[YearPart]);
  // Check date (Day, Month, Year) for validity
  if not TryEncodeDate(Year, Month, Day, Result) then
    raise ERFC2822Date.Create(sBadDate);
  if DaysInMonth(Result) < Day then
    raise ERFC2822Date.CreateFmt(sBadDay, [Day]);
  // Get time component
  Result := Result + GetTime(Parts[TimePart]);
  // Get any offset and adjust date accordingly
  GetOffset(Parts[OffsetPart], OffsetHours, OffsetMins);
  if OffsetHours <> 0 then
    Result := IncHour(Result, OffsetHours);
  if OffsetMins <> 0 then
    Result := IncMinute(Result, OffsetMins);
end;

end.
