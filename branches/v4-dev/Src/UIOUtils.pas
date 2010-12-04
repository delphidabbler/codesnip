{
 * UIOUtils.pas
 *
 * Provides a container for assisting with common file operations.
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
 * The Original Code is UIOUtils.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UIOUtils;


interface


uses
  // Delphi
  SysUtils, Classes, Types;

type
  ///  <summary>
  ///  Container for methods that assist with common file operations.
  ///  </summary>
  ///  <remarks>
  ///  TFileIO is used instead of IOUtils.TFile because the assumptions TFile
  ///  makes about the use of byte order marks with encoded text files are not
  ///  compatible with the needs of this program.
  ///  </remarks>
  TFileIO = record
  strict private
    class function CheckBOM(const Bytes: TBytes; const Encoding: TEncoding):
      Boolean; static;
    class procedure BytesToStream(const Bytes: TBytes; const Stream: TStream);
      static;
    class function StreamToBytes(const Stream: TStream): TBytes; static;
  public
    ///  <summary>
    ///  Writes all the bytes from a byte array to a file.
    ///  </summary>
    ///  <param name="FileName">string [in] Name of file.</param>
    ///  <param name="Bytes">TBytes [in] Array of bytes to be written to file.
    ///  </param>
    class procedure WriteAllBytes(const FileName: string; const Bytes: TBytes);
      static;

    ///  <summary>
    ///  Writes text to a file.
    ///  </summary>
    ///  <param name="FileName">string [in] Name of file.</param>
    ///  <param name="Content">string [in] Text to be written to file.</param>
    ///  <param name="Encoding">TEncoding [in] Encoding to be used for text in
    ///  file.</param>
    ///  <param name="UseBOM">Boolean [in] Flag indicating whether BOM to be
    ///  written to file. If Encoding has no BOM then UseBOM has no effect.
    ///  </param>
    class procedure WriteAllText(const FileName, Content: string;
      const Encoding: TEncoding; const UseBOM: Boolean = False); static;

    ///  <summary>
    ///  Reads all bytes from a file into a byte array.
    ///  </summary>
    ///  <param name="FileName">string [in] Name of file.</param>
    ///  <returns>TBytes array containing the file's contents.</returns>
    class function ReadAllBytes(const FileName: string): TBytes; static;

    ///  <summary>
    ///  Reads all the text from a text file.
    ///  </summary>
    ///  <param name="FileName">string [in] Name of file.</param>
    ///  <param name="Encoding">TEncoding [in] Text encoding used by file.
    ///  </param>
    ///  <param name="HasBOM">Boolean [in] Flag indicating if file has a byte
    ///  order mark. Ignored if Encoding has no BOM.</param>
    ///  <returns>String containing contents of file.</returns>
    ///  <remarks>When HasBOM is true and Encoding has a BOM then the BOM must
    ///  begin the file, otherwise an exception is raised.</remarks>
    class function ReadAllText(const FileName: string;
      const Encoding: TEncoding; const HasBOM: Boolean = False): string; static;

    ///  <summary>
    ///  Reads all the lines of text from a text file.
    ///  </summary>
    ///  <param name="FileName">string [in] Name of file.</param>
    ///  <param name="Encoding">TEncoding [in] Text encoding used by file.
    ///  </param>
    ///  <param name="HasBOM">Boolean [in] Flag indicating if file has a byte
    ///  order mark. Ignored if Encoding has no BOM.</param>
    ///  <returns>TStringDynArray containing lines from file.</returns>
    ///  <remarks>When HasBOM is true and Encoding has a BOM then the BOM must
    ///  begin the file, otherwise an exception is raised.</remarks>
    class function ReadAllLines(const FileName: string;
      const Encoding: TEncoding; const HasBOM: Boolean = False):
      TStringDynArray; static;

  end;

type
  ///  <summary>Class of exception raised by UIOUtils code.</summary>
  EIOUtils = class(Exception);


implementation


resourcestring
  // Error messages
  sBadBOM = 'Preamble of file %s does not match expected encoding';


{ TFileIO }

class procedure TFileIO.BytesToStream(const Bytes: TBytes;
  const Stream: TStream);
begin
  Stream.Size := Length(Bytes);
  Stream.Position := 0;
  if Length(Bytes) > 0 then
    Stream.WriteBuffer(Pointer(Bytes)^, Length(Bytes));
end;

class function TFileIO.CheckBOM(const Bytes: TBytes; const Encoding: TEncoding):
  Boolean;
var
  Preamble: TBytes;
  I: Integer;
begin
  Preamble := Encoding.GetPreamble;
  if Length(Preamble) = 0 then
    Exit(False);
  if Length(Bytes) < Length(Preamble) then
    Exit(False);
  for I := 0 to Pred(Length(Preamble)) do
    if Bytes[I] <> Preamble[I] then
      Exit(False);
  Result := True;
end;

class function TFileIO.ReadAllBytes(const FileName: string): TBytes;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := StreamToBytes(FS);
  finally
    FS.Free;
  end;
end;

class function TFileIO.ReadAllLines(const FileName: string;
  const Encoding: TEncoding; const HasBOM: Boolean): TStringDynArray;
var
  Lines: TStrings;
  I: Integer;
begin
  Assert(Assigned(Encoding), 'TFileIO.ReadAllLines: Encoding is nil');
  Lines := TStringList.Create;
  try
    Lines.Text := ReadAllText(FileName, Encoding, HasBOM);
    SetLength(Result, Lines.Count);
    for I := 0 to Pred(Lines.Count) do
      Result[I] := Lines[I];
  finally
    Lines.Free;
  end;
end;

class function TFileIO.ReadAllText(const FileName: string;
  const Encoding: TEncoding; const HasBOM: Boolean): string;
var
  Content: TBytes;
  SizeOfBOM: Integer;
begin
  Assert(Assigned(Encoding), 'TFileIO.ReadAllBytes: Encoding is nil');
  Content := ReadAllBytes(FileName);
  if HasBOM then
  begin
    SizeOfBOM := Length(Encoding.GetPreamble);
    if (SizeOfBOM > 0) and not CheckBOM(Content, Encoding) then
      raise EIOUtils.CreateFmt(sBadBOM, [FileName]);
  end
  else
    SizeOfBOM := 0;
  Result := Encoding.GetString(Content, SizeOfBOM, Length(Content) - SizeOfBOM);
end;

class function TFileIO.StreamToBytes(const Stream: TStream): TBytes;
begin
  Stream.Position := 0;
  SetLength(Result, Stream.Size);
  if Stream.Size > 0 then
    Stream.ReadBuffer(Pointer(Result)^, Length(Result));
end;

class procedure TFileIO.WriteAllBytes(const FileName: string;
  const Bytes: TBytes);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmCreate);
  try
    BytesToStream(Bytes, FS);
  finally
    FS.Free;
  end;
end;

class procedure TFileIO.WriteAllText(const FileName, Content: string;
  const Encoding: TEncoding; const UseBOM: Boolean);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmCreate);
  try
    if UseBOM then
      BytesToStream(Encoding.GetPreamble, FS);
    BytesToStream(Encoding.GetBytes(Content), FS);
  finally
    FS.Free;
  end;
end;

end.

