{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Interface, implementation class and factory object used to encapsulate, read
 * and apply DOS file dates.
}



unit UDOSDateTime;


interface


uses
  // Project
  UBaseObjects;


type
  ///  <summary>
  ///  Interface supported by classes that encapsulate and manipulate a DOS file
  ///  date.
  ///  </summary>
  IDOSDateTime = interface(IInterface)
    ['{6213B468-744A-4AC4-AABE-615627534E27}']
    ///  <summary>
    ///  Compares file date object against another such object.
    ///  </summary>
    ///  <param name="DT">IDOSDateTime object to compare to.</param>
    ///  <returns>-ve if this date is less than DT, +ve if this date is greater
    ///  than DT and zero if the two objects are equal.</returns>
    function CompareTo(const DT: IDOSDateTime): Integer;
    ///  <summary>
    ///  Applies date stamp to a named file.
    ///  </summary>
    procedure ApplyToFile(const FileName: string);
    ///  <summary>
    ///  Returns date stamp.
    ///  </summary>
    function DateStamp: Integer;
  end;

type
  ///  <summary>
  ///  Factory class that creates instances of IDOSDateTime objects.
  ///  </summary>
  TDOSDateTimeFactory = class(TNoConstructObject)
  public
    ///  <summary>
    ///  Creates an IDOSDateTime instance from a DOS date stamp.
    ///  </summary>
    ///  <param name="DateStamp">DOS date stamp as an Integer. Assumed to be in
    ///  local time.</param>
    ///  <returns>IDOSDateTime instance.</returns>
    class function CreateFromDOSTimeStamp(const DateStamp: Integer):
      IDOSDateTime;
    ///  <summary>
    ///  Creates an IDOSDateTime instance from a Unix time stamp.
    ///  </summary>
    ///  <param name="UnixTimeStamp">Unix time stamp as Int64. Assumed to be in
    ///  GTM.</param>
    ///  <returns>IDOSDateTime instance.</returns>
    class function CreateFromUnixTimeStamp(
      const UnixTimeStamp: Int64): IDOSDateTime;
    ///  <summary>
    ///  Creates an IDOSDateTime instance from the date stamp of a file.
    ///  </summary>
    ///  <param name="FileName">Name of file whose date stamp, in local time, is
    ///  to be used.</param>
    ///  <returns>IDOSDateTime instance.</returns>
    class function CreateFromFile(const FileName: string): IDOSDateTime;
  end;


implementation


uses
  // Delphi
  SysUtils, DateUtils, Windows, Math,
  // Project
  IntfCommon, UExceptions, UUtils;


type
  ///  <summary>
  ///  Implementation of IDOSDateTime interface. All times are in local time.
  ///  </summary>
  TDOSDateTime = class(TInterfacedObject,
    IDOSDateTime, IAssignable
  )
  strict private
    var fDateStamp: Integer;    // DOS date stamp
  public
    ///  <summary>
    ///  Object constructor. Sets up object for a specified DOS date stamp.
    ///  </summary>
    constructor Create(const DateStamp: Integer);

    { IDOSDateTime methods }
    ///  <summary>
    ///  Compares file date object against another such object.
    ///  </summary>
    ///  <param name="DT">IDOSDateTime object to compare to.</param>
    ///  <returns>-ve if this date is less than DT, +ve if this date is greater
    ///  than DT and zero if the two objects are equal.</returns>
    ///  <remarks>
    ///  Comparisons are made within 2 seconds, i.e. two date stamps up to 2
    ///  seconds apart compare as equal. This is because DOS file stamps may
    ///  only be accurate to 2 seconds.
    ///  </remarks>
    function CompareTo(const DT: IDOSDateTime): Integer;
    ///  <summary>
    ///  Applies date stamp to a named file.
    ///  </summary>
    procedure ApplyToFile(const FileName: string);
    ///  <summary>
    ///  Returns date stamp.
    ///  </summary>
    function DateStamp: Integer;

    { IAssignable methods }
    ///  <summary>
    ///  Assigns properties of a given object to this object.
    ///  </summary>
    ///  <param name="Src">Object whose properties are to be copied. Must be non
    ///  nil and support IDOSDateTime.</param>
    procedure Assign(const Src: IInterface);
  end;

///  <summary>
///  Converts a given GMT time to local time, taking into account daylight
///  saving time.
///  </summary>
///  <param name="GMTTime">TDateTime [in] GMT time to be converted.</param>
///  <returns>TDateTime: Local date time.</returns>
///  <remarks>Required Windows NT platform.</remarks>
function GMTToLocalTime(GMTTime: TDateTime): TDateTime;
var
  GMTST: TSystemTime;     // GMT date as system time
  LocalST: TSystemTime;   // local date as system time
begin
  DateTimeToSystemTime(GMTTime, GMTST);
  Win32Check(SystemTimeToTzSpecificLocalTime(nil, GMTST, LocalST));
  Result := SystemTimeToDateTime(LocalST);
end;

{ TDOSDateTime }

procedure TDOSDateTime.ApplyToFile(const FileName: string);
begin
  // FileSetDate expects local time
  FileSetDate(FileName, Self.DateStamp);
end;

procedure TDOSDateTime.Assign(const Src: IInterface);
var
  DT: IDOSDateTime; // IDOSDateTime interface to object being assigned
begin
  if not Supports(Src, IDOSDateTime, DT) then
    raise EBug.Create(ClassName + '.Assign: Src is wrong type');
  fDateStamp := DT.DateStamp;
end;

function TDOSDateTime.CompareTo(const DT: IDOSDateTime): Integer;
const
  cDelta = 2; // DOS file stamps accurate to 2 sec on Win 9x
var
  Difference: Integer;  // difference between two date stamps
begin
  Difference := Self.DateStamp - DT.DateStamp;
  if InRange(Difference, -cDelta, cDelta) then
    Result := 0
  else
    Result := Difference;
end;

constructor TDOSDateTime.Create(const DateStamp: Integer);
begin
  inherited Create;
  fDateStamp := DateStamp;
end;

function TDOSDateTime.DateStamp: Integer;
begin
  Result := fDateStamp;
end;

{ TDOSDateTimeFactory }

class function TDOSDateTimeFactory.CreateFromDOSTimeStamp(
  const DateStamp: Integer): IDOSDateTime;
begin
  // Assume this date stamp is local time
  Result := TDOSDateTime.Create(DateStamp);
end;

class function TDOSDateTimeFactory.CreateFromFile(
  const FileName: string): IDOSDateTime;
begin
  // FileAge returns local time stamp
  Result := TDOSDateTime.Create(UUtils.FileAge(FileName));
end;

class function TDOSDateTimeFactory.CreateFromUnixTimeStamp(
  const UnixTimeStamp: Int64): IDOSDateTime;
var
  UDT: TDateTime; // unix time stamp as TDateTime
begin
  // UnixTimeStamp is in GMT: need to convert to local time
  UDT := GMTToLocalTime(UnixToDateTime(UnixTimeStamp));
  Result := TDOSDateTime.Create(DateTimeToFileDate(UDT));
end;

end.

