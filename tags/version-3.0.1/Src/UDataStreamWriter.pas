{
 * UDataStreamWriter.pas
 *
 * Implements a stream wrapper class that can write numeric and string data to a
 * stream. Integers of various sizes are encoded as ASCII hex digits.
 *
 * Requires the DelphiDabbler Stream Extension Classes v2.0.1 or later.
 *
 * v1.0 of 11 Sep 2008  - Original version.
 * v1.1 of 22 May 2009  - Added new TDataStreamWriter.WriteSizedLongString
 *                        method.
 *
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
 * The Original Code is UDataStreamWriter.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UDataStreamWriter;


interface


uses
  // DelphiDabbler library
  PJStreamWrapper;


type

  {
  TDataStreamWriter:
    Stream wrapper that can write numeric and string data to a stream. Integers
    of various sizes are encoded as ASCII hex digits in stream.
  }
  TDataStreamWriter = class(TPJStreamWrapper)
  strict private
    procedure WriteHex(const Value: LongInt; const Count: Integer);
      {Writes hex digits to stream.
        @param Value [in] Value to be written.
        @param Count [in] Number of hex digits required.
      }
  public
    procedure WriteSmallInt(const Value: SmallInt);
      {Writes a 16 bit integer to the stream as hex digits.
        @param Value [in] Value to be written.
      }
    procedure WriteLongInt(const Value: LongInt);
      {Write a 32 bit integer to the stream as hex digits.
        @param Value [in] Value to be written.
      }
    procedure WriteString(const Str: string; const Length: Integer); overload;
      {Writes a fixed number of characters from a string to the stream.
        @param Str [in] String to be written.
        @param Length [in] Number of characters from string to write.
      }
    procedure WriteString(const Str: string); overload;
      {Writes a string to stream.
        @param Str [in] String to be written.
      }
    procedure WriteSizedString(const Str: string);
      {Writes a string to stream preceded by a 16 bit length as hex digits.
        @param Str [in] String to be written.
      }
    procedure WriteSizedLongString(const Str: string);
      {Writes a string to stream preceded by a 32 bit length as hex digits.
        @param Str [in] String to be written.
      }
  end;


implementation


uses
  // Delphi
  SysUtils;


{ TDataStreamWriter }

procedure TDataStreamWriter.WriteHex(const Value: LongInt;
  const Count: Integer);
  {Writes hex digits to stream.
    @param Value [in] Value to be written.
    @param Count [in] Number of hex digits required.
  }
begin
  WriteString(IntToHex(Value, Count), Count);
end;

procedure TDataStreamWriter.WriteLongInt(const Value: Integer);
  {Write a 32 bit integer to the stream as hex digits.
    @param Value [in] Value to be written.
  }
begin
  WriteHex(Value, 8);
end;

procedure TDataStreamWriter.WriteSizedLongString(const Str: string);
  {Writes a string to stream preceded by a 32 bit length as hex digits.
    @param Str [in] String to be written.
  }
begin
  WriteLongInt(Length(Str));
  WriteString(Str, Length(Str));
end;

procedure TDataStreamWriter.WriteSizedString(const Str: string);
  {Writes a string to stream preceded by a 16 bit length as hex digits.
    @param Str [in] String to be written.
  }
begin
  WriteSmallInt(Length(Str));
  WriteString(Str, Length(Str));
end;

procedure TDataStreamWriter.WriteSmallInt(const Value: SmallInt);
  {Writes a 16 bit integer to the stream as hex digits.
    @param Value [in] Value to be written.
  }
begin
  WriteHex(Value, 4);
end;

procedure TDataStreamWriter.WriteString(const Str: string);
  {Writes a string to stream.
    @param Str [in] String to be written.
  }
begin
  WriteString(Str, Length(Str));
end;

procedure TDataStreamWriter.WriteString(const Str: string;
  const Length: Integer);
  {Writes a fixed number of characters from a string to the stream.
    @param Str [in] String to be written.
    @param Length [in] Number of characters from string to write.
  }
begin
  BaseStream.WriteBuffer(Str[1], Length);
end;

end.

