{
 * UStrStreamWriter.pas
 *
 * Implements a class that can write strings to a stream.
 *
 * Requires the DelphiDabbler Streams library v2.0.1 or later.
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
 * The Original Code is UStrStreamWriter.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UStrStreamWriter;


interface


uses
  // DelphiDabbler library
  PJStreamWrapper;


type

  {
  TStrStreamWriter:
    Class that writes strings to a wrapped TStream.
  }
  TStrStreamWriter = class(TPJStreamWrapper)
  public
    procedure WriteStr(const Msg: string); overload;
      {Writes string to underlying stream.
        @param Msg [in] String to be written.
      }
    procedure WriteStr(const Fmt: string; const Args: array of const);
      overload;
      {Writes formatted string to underlying stream.
        @param Fmt [in] Format string.
        @param Args [in] Arguments to replace placeholders in format string.
      }
    procedure WriteStrLn; overload;
      {Writes a newline to underlying stream.
      }
    procedure WriteStrLn(const Msg: string); overload;
      {Writes string followed by newline to underlying stream.
        @param Msg [in] String to be written.
      }
    procedure WriteStrLn(const Fmt: string; const Args: array of const);
      overload;
      {Writes formatted string followed by newline to underlying stream.
        @param Fmt [in] Format string.
        @param Args [in] Arguments to replace placeholders in format string.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UConsts;


{ TStrStreamWriter }

procedure TStrStreamWriter.WriteStr(const Msg: string);
  {Writes string to underlying stream.
    @param Msg [in] String to be written.
  }
begin
  if (Msg <> '') then
    Self.BaseStream.WriteBuffer(Pointer(Msg)^, Length(Msg) * SizeOf(Char));
end;

procedure TStrStreamWriter.WriteStr(const Fmt: string;
  const Args: array of const);
  {Writes formatted string to underlying stream.
    @param Fmt [in] Format string.
    @param Args [in] Arguments to replace placeholders in format string.
  }
begin
  WriteStr(Format(Fmt, Args));
end;

procedure TStrStreamWriter.WriteStrLn;
  {Writes a newline to underlying stream.
  }
begin
  WriteStr(EOL);
end;

procedure TStrStreamWriter.WriteStrLn(const Msg: string);
  {Writes string followed by newline to underlying stream.
    @param Msg [in] String to be written.
  }
begin
  WriteStr(Msg);
  WriteStrLn;
end;

procedure TStrStreamWriter.WriteStrLn(const Fmt: string;
  const Args: array of const);
  {Writes formatted string followed by newline to underlying stream.
    @param Fmt [in] Format string.
    @param Args [in] Arguments to replace placeholders in format string.
  }
begin
  WriteStr(Fmt, Args);
  WriteStrLn;
end;

end.

