{
 * UDOSDateTime.pas
 *
 * Interface, implementation class and factory object used to encapsulate, read
 * and apply DOS file dates.
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
 * The Original Code is UDOSDateTime.pas
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



unit UDOSDateTime;


interface


uses
  // Project
  UBaseObjects;


type

  {
  IDOSDateTime:
    Interface that encapsulates a DOS file date and applies operations to it.
  }
  IDOSDateTime = interface(IInterface)
    ['{6213B468-744A-4AC4-AABE-615627534E27}']
    function Compare(const DT: IDOSDateTime): Integer;
      {Compare file date against another file date.
        @param DT [in] DOS date to compare.
        @return <0 if this date is less than DT, >0 if this date is more than
          DT and 0 if same (within 2 seconds),
      }
    procedure ApplyToFile(const FileName: string);
      {Applies date stamp to a file.
        @param FileName [in] Name of file to receive file stamp.
      }
    function DateStamp: Integer;
      {Returns date stamp of file.
        @return Date stamp as integer.
      }
  end;

  {
  TDOSDateTimeFactory:
    Factory that creates instances of IDOSDateTime.
  }
  TDOSDateTimeFactory = class(TNoConstructObject)
  public
    class function CreateFromDOSTimeStamp(
      const DateStamp: Integer): IDOSDateTime;
      {Creates IDOSDateTime instance with a specific date stamp.
        @param TimeStamp [in] Required DOS date stamp. Assumed to be in local
          time.
        @return IDOSDateTime instance.
      }
    class function CreateFromUnixTimeStamp(
      const UnixTimeStamp: Int64): IDOSDateTime;
      {Creates IDOSDateTime instance with date stamp derived from a Unix time
      stamp.
        @param TimeStamp [in] Unix time stamp. Assumed to be in GMT.
        @return IDOSDateTime instance.
      }
    class function CreateFromFile(const FileName: string): IDOSDateTime;
      {Creates IDOSDateTime instance with date stamp of a file.
        @param FileName [in] Name of file whose date stamp is to be used. The
          date stamp is converted to local time.
        @return IDOSDateTime instance.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, DateUtils, Windows, Math,
  // Project
  IntfCommon, UExceptions, USystemInfo, UUtils;


type
  {
  TDOSDateTime:
    Implementation of IDOSDateTime interface. All times are in local time.
  }
  TDOSDateTime = class(TInterfacedObject,
    IDOSDateTime, IAssignable
  )
  strict private
    fDateStamp: Integer;
      {DOS date stamp}
  protected
    { IDOSDateTime methods }
    function Compare(const DT: IDOSDateTime): Integer;
      {Compare file date against another file date.
        @param DT [in] DOS date to compare.
        @return <0 if this date is less than DT, >0 if this date is more than DT
          and 0 if same (within 2 seconds),
      }
    procedure ApplyToFile(const FileName: string);
      {Applies date stamp to a file.
        @param FileName [in] Name of file to receive file stamp.
      }
    function DateStamp: Integer;
      {Returns date stamp of file.
        @return Date stamp as integer.
      }
    { IAssignable methods }
    procedure Assign(const Src: IInterface);
      {Assigns properties of a given object to this object.
        @param Src [in] Object whose properties are to be copied. Must be non
          nil and support IDOSDateTime.
        @except EBug raised if Src is nil or does not support IDOSDateTime.
      }
  public
    constructor Create(const DateStamp: Integer);
      {Class constructor. Sets up object for a specified DOS date stamp.
        @param DateStamp [in] DOS date stamp in local file format.
      }
  end;


function GMTToLocalTime(GMTTime: TDateTime): TDateTime;
  {Converts a given GMT time to local time, taking into account daylight
  saving time.
    @param GMTTime [in] GMT time to be converted.
    @return Required local time.
  }
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
  {Applies date stamp to a file.
    @param FileName [in] Name of file to receive file stamp.
  }
begin
  // FileSetDate expects local time
  FileSetDate(FileName, Self.DateStamp);
end;

procedure TDOSDateTime.Assign(const Src: IInterface);
  {Assigns properties of a given object to this object.
    @param Src [in] Object whose properties are to be copied. Must be non nil
      and support IDOSDateTime.
    @except EBug raised if Src is nil or does not support IDOSDateTime.
  }
var
  DT: IDOSDateTime; // IDOSDateTime interface to object being assigned
begin
  if not Supports(Src, IDOSDateTime, DT) then
    raise EBug.Create(ClassName + '.Assign: Src is wrong type');
  fDateStamp := DT.DateStamp;
end;

function TDOSDateTime.Compare(const DT: IDOSDateTime): Integer;
  {Compare file date against another file date.
    @param DT [in] DOS date to compare.
    @return <0 if this date is less than DT, >0 if this date is more than DT and
      0 if same (within 2 seconds),
  }
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
  {Class constructor. Sets up object for a specified DOS date stamp.
    @param DateStamp [in] DOS date stamp in local file format.
  }
begin
  inherited Create;
  fDateStamp := DateStamp;
end;

function TDOSDateTime.DateStamp: Integer;
  {Returns date stamp of file.
    @return Date stamp as integer.
  }
begin
  Result := fDateStamp;
end;


{ TDOSDateTimeFactory }

class function TDOSDateTimeFactory.CreateFromDOSTimeStamp(
  const DateStamp: Integer): IDOSDateTime;
  {Creates IDOSDateTime instance with a specific date stamp.
    @param TimeStamp [in] Required DOS date stamp. Assumed to be in local time.
    @return IDOSDateTime instance.
  }
begin
  // Assume this date stamp is local time
  Result := TDOSDateTime.Create(DateStamp);
end;

class function TDOSDateTimeFactory.CreateFromFile(
  const FileName: string): IDOSDateTime;
  {Creates IDOSDateTime instance with date stamp of a file.
    @param FileName [in] Name of file whose date stamp is to be used. The date
      stamp is converted to local time.
    @return IDOSDateTime instance.
  }
begin
  // FileAge returns local time stamp
  Result := TDOSDateTime.Create(UUtils.FileAge(FileName));
end;

class function TDOSDateTimeFactory.CreateFromUnixTimeStamp(
  const UnixTimeStamp: Int64): IDOSDateTime;
  {Creates IDOSDateTime instance with date stamp derived from a Unix time stamp.
    @param TimeStamp [in] Unix time stamp. Assumed to be in GMT.
    @return IDOSDateTime instance.
  }
var
  UDT: TDateTime; // unix time stamp as TDateTime
begin
  Assert(TOSInfo.IsWinNT,
    ClassName + '.CreateFromUnixTimeStamp: NT platform required');
  // UnixTimeStamp is in GMT: need to convert to local time
  UDT := GMTToLocalTime(UnixToDateTime(UnixTimeStamp));
  Result := TDOSDateTime.Create(DateTimeToFileDate(UDT));
end;

end.

