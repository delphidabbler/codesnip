{
 * UMainDBFileReader.pas
 *
 * Implements class that can read files from main database using correct
 * encoding.
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
 * The Original Code is UMainDBFileReader.pas
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


unit UMainDBFileReader;

interface

uses
  // Delphi
  SysUtils,
  // Project
  UEncodings, UIStringList;

type
  ///  <summary>
  ///  Loads files from main database, taking into account file encoding.
  ///  </summary>
  ///  <remarks>
  ///  All files in database folder are assumed to have the same encoding. Only
  ///  one file is tested.
  ///  </remarks>
  TMainDBFileReader = class(TObject)
  strict private
    var
      ///  <summary>Encoding to use when reading text files.</summary>
      fEncoding: TEncoding;
    ///  <summary>
    ///  Detects encoding used by specified file.
    ///  </summary>
    ///  <param name="FileName">string [in] Name of file.</param>
    ///  <returns>Required TEncoding instance. Callers should free this instance
    ///  if it not a standard encoding.</returns>
    ///  <remarks>
    ///  Main database files created by CodeSnip v3 and earlier use the Default
    ///  encoding while those created by v4 use UTF-8 files that have the UTF-8
    ///  preamble.
    ///  </remarks>
    function GetFileEncoding(const FileName: string): TEncoding;
  public
    ///  <summary>
    ///  Object constructor. Sets up object to use encoding used for given
    ///  specimen file.
    ///  </summary>
    constructor Create(const SpecimenFile: string);
    ///  <summary>
    ///  Object destructor. Tears down object.
    ///  </summary>
    destructor Destroy; override;
    ///  <summary>
    ///  Reads all text from a text file using known encoding.
    ///  </summary>
    ///  <param name="FileName">string [in] Name of text file to read.</param>
    ///  <returns>String containing contents of file.</returns>
    function ReadAllText(const FileName: string): string;
    ///  <summary>
    ///  Reads all lines from a text file using known encoding.
    ///  </summary>
    ///  <param name="FileName">string [in] Name of text file to read.</param>
    ///  <returns>IStringList object containing lines from file.</returns>
    function ReadAllStrings(const FileName: string): IStringList;
  end;

implementation


uses
  // Delphi
  Classes,
  // Project
  UIOUtils;


{ TMainDBFileReader }

constructor TMainDBFileReader.Create(const SpecimenFile: string);
begin
  inherited Create;
  fEncoding := GetFileEncoding(SpecimenFile);
end;

destructor TMainDBFileReader.Destroy;
begin
  TEncodingHelper.FreeEncoding(fEncoding);
  inherited;
end;

function TMainDBFileReader.GetFileEncoding(const FileName: string): TEncoding;

  ///  Checks if two byte arrays are equal.
  function BytesMatch(const BA1, BA2: TBytes): Boolean;
  var
    I: Integer;
  begin
    if Length(BA1) <> Length(BA2) then
      Exit(False);
    for I := 0 to Pred(Length(BA1)) do
      if BA1[I] <> BA2[I] then
        Exit(False);
    Result := True;
  end;

var
  FS: TFileStream;      // stream onto file
  Buffer: TBytes;       // buffer containing first few bytes of file
  UTF8Preamble: TBytes; // bytes of UTF-8 preamble
begin
  UTF8Preamble := TEncoding.UTF8.GetPreamble;
  Assert(Length(UTF8Preamble) > 0,
    ClassName + '.GetFileEncoding: UTF-8 preamble has zero length');
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    if FS.Size >= Length(UTF8Preamble) then
    begin
      // read first few bytes of file to see if they match UTF-8 preamble
      SetLength(Buffer, Length(UTF8Preamble));
      FS.ReadBuffer(Pointer(Buffer)^, Length(Buffer));
      if BytesMatch(Buffer, UTF8Preamble) then
        Exit(TEncoding.UTF8);
    end;
  finally
    FS.Free;
  end;
  Result := TEncoding.Default;
end;

function TMainDBFileReader.ReadAllStrings(const FileName: string): IStringList;
begin
  Result := TIStringList.Create(
    TFileIO.ReadAllLines(FileName, fEncoding, True)
  );
end;

function TMainDBFileReader.ReadAllText(const FileName: string): string;
begin
  Result := TFileIO.ReadAllText(FileName, fEncoding, True);
end;

end.
