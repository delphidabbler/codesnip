{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides a container for assisting with common file operations.
}


unit UIOUtils;


interface


uses
  // Delphi
  SysUtils, Classes;

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
    ///  <summary>
    ///  Appends whole contents of a byte array to a stream.
    ///  </summary>
    class procedure BytesToStream(const Bytes: TBytes; const Stream: TStream);
      static;
    ///  <summary>
    ///  Copies content of a whole stream into a byte array.
    ///  </summary>
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
    ///  Writes lines of text to a text file with lines separated by CRLF.
    ///  </summary>
    ///  <param name="FileName">string [in] Name of file.</param>
    ///  <param name="Lines">array of string [in] Array of lines of text to be
    ///  written.</param>
    ///  <param name="Encoding">TEncoding [in] Encoding to be used by file.
    ///  </param>
    ///  <param name="UseBOM">Boolean [in] Flag indicating whether BOM to be
    ///  written to file. If Encoding has no BOM then UseBOM has no effect.
    ///  </param>
    class procedure WriteAllLines(const FileName: string;
      const Lines: array of string; const Encoding: TEncoding;
      const UseBOM: Boolean = False); static;

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
    ///  <returns>TArray&lt;string&gt; containing lines from file.</returns>
    ///  <remarks>When HasBOM is true and Encoding has a BOM then the BOM must
    ///  begin the file, otherwise an exception is raised.</remarks>
    class function ReadAllLines(const FileName: string;
      const Encoding: TEncoding; const HasBOM: Boolean = False):
      TArray<string>; static;

    ///  <summary>
    ///  Copies content of one file to another.
    ///  </summary>
    ///  <param name="SrcFileName">string [in] Name of file to be copied.
    ///  </param>
    ///  <param name="DestFileName">string [in] Name of file to receive
    ///  contents of file named in SrcFileName.</param>
    ///  <remarks>SrcFileName and DestFileName must be different. SrcFileName
    ///  must exist. DestFileName is overwritten if it already exists.</remarks>
    class procedure CopyFile(const SrcFileName, DestFileName: string); static;
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

class procedure TFileIO.CopyFile(const SrcFileName, DestFileName: string);
begin
  TFileIO.WriteAllBytes(DestFileName, TFileIO.ReadAllBytes(SrcFileName));
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
  const Encoding: TEncoding; const HasBOM: Boolean): TArray<string>;
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

class procedure TFileIO.WriteAllLines(const FileName: string;
  const Lines: array of string; const Encoding: TEncoding;
  const UseBOM: Boolean);
var
  Line: string;
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;
  try
    for Line in Lines do
      SB.AppendLine(Line);
    WriteAllText(FileName, SB.ToString, Encoding, UseBOM);
  finally
    SB.Free;
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

