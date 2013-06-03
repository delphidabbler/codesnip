{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2010-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Utility functions used to assist when working with program resources.
}


unit UResourceUtils;


interface


uses
  // Project
  UEncodings;


function MakeResourceURL(const ModuleName: string; const ResName: PChar;
  const ResType: PChar = nil): string; overload;
  {Returns a res:// protocol URL that references a resource in a module.
    @param ModuleName [in] Name of module containing the resource.
    @param ResName [in] Name of resource.
    @param ResType [in] Type of resource (omitted from URL if nil or not
      specified).
    @return Required res:// protocol URL.
  }

function MakeResourceURL(const Module: HMODULE; const ResName: PChar;
  const ResType: PChar = nil): string; overload;
  {Returns a res:// protocol URL that references a resource in a module.
    @param Module [in] Handle of module containing resource.
    @param ResName [in] Name of resource.
    @param ResType [in] Type of resource (omitted from URL if nil or not
      specified).
    @return Required res:// protocol URL.
  }

function MakeResourceURL(const ResName: string): string; overload;
  {Returns a res:// protocol URL that references a resource in the program's own
  RT_HTML resources.
    @param ResName [in] Name of resource.
    @return Required res:// protocol URL.
  }

function LoadResourceAsString(const Inst: HMODULE; const ResName: string;
  const ResType: PChar; const EncType: TEncodingType;
  const HasBOM: Boolean = False): string;
  {Loads a resource as a string.
    @param Inst [in] Handle of module containing resource.
    @param ResName [in] Name of resource.
    @param ResType [in] Type of resource.
    @param EncType [in] Type of encoding used for resource text.
    @param HasBOM [in] Indicates whether resource data is preceded by a byte
      order mark.
    @return Content of resource as a string.
  }


implementation


uses
  // Delphi
  SysUtils, Classes, Windows,
  // Project
  UExceptions, UURIEncode;


function MakeResourceURL(const ModuleName: string; const ResName: PChar;
  const ResType: PChar = nil): string; overload;
  {Returns a res:// protocol URL that references a resource in a module.
    @param ModuleName [in] Name of module containing the resource.
    @param ResName [in] Name of resource.
    @param ResType [in] Type of resource (omitted from URL if nil or not
      specified).
    @return Required res:// protocol URL.
  }

  // ---------------------------------------------------------------------------
  function ResNameOrTypeToString(R: PChar): string;
    {Returns string representation of a resource name or type. If name or type
    is already a string it is returned unchanged. If it is a numeric value its
    value is returned as a string, preceeded by '#'.
      @param R [in] Resource name or type.
      @return String representation of the resource name or type.
    }
  begin
    if HiWord(LongWord(R)) = 0 then
      // high word = 0 => numeric resource id
      // numeric value is stored in low word
      Result := Format('#%d', [LoWord(LongWord(R))])
    else
      // high word <> 0 => string value
      // PChar is implicitly converted to string
      Result := R;
  end;
  // ---------------------------------------------------------------------------

begin
  Assert(ModuleName <> '', 'MakeResourceURL: ModuleName is ''''');
  Assert(Assigned(ResName), 'MakeResourceURL: ResName is nil');
  // Resource starts with module name
  Result := 'res://' + URIEncode(ModuleName);
  // Resource type follows if specified
  if Assigned(ResType) then
    Result := Result + '/' + URIEncode(ResNameOrTypeToString(ResType));
  // Resource name is last in URL
  Result := Result + '/' + URIEncode(ResNameOrTypeToString(ResName));
end;

function MakeResourceURL(const Module: HMODULE; const ResName: PChar;
  const ResType: PChar = nil): string; overload;
  {Returns a res:// protocol URL that references a resource in a module.
    @param Module [in] Handle of module containing resource.
    @param ResName [in] Name of resource.
    @param ResType [in] Type of resource (omitted from URL if nil or not
      specified).
    @return Required res:// protocol URL.
  }
begin
  Result := MakeResourceURL(GetModuleName(Module), ResName, ResType);
end;

function MakeResourceURL(const ResName: string): string; overload;
  {Returns a res:// protocol URL that references a resource in the program's own
  RT_HTML resources.
    @param ResName [in] Name of resource.
    @return Required res:// protocol URL.
  }
begin
  Result := MakeResourceURL(HInstance, PChar(ResName));
end;

function LoadResourceAsString(const Inst: HMODULE; const ResName: string;
  const ResType: PChar; const EncType: TEncodingType; const HasBOM: Boolean):
  string;
  {Loads a resource as a string.
    @param Inst [in] Handle of module containing resource.
    @param ResName [in] Name of resource.
    @param ResType [in] Type of resource.
    @param EncType [in] Type of encoding used for resource text.
    @param HasBOM [in] Indicates whether resource data is preceded by a byte
      order mark.
    @return Content of resource as a string.
  }

  { TODO: This code duplicates TFileIO.CheckBOM - extract common code to
          TEncodingHelper. }
  // Checks if the given byte array starts with byte order mark of the given
  // encoding. If the encoding has no BOM then False is returned.
  function CheckBOM(const Bytes: TBytes; const Encoding: TEncoding): Boolean;
  var
    Preamble: TBytes; // preamble for the encoding (BOM)
    I: Integer;       // loops through each byte of Preamble
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

var
  RS: TResourceStream;  // stream onto resource
  Content: TBytes;      // resource contents as byte array
  Encoding: TEncoding;  // encoding to use for string conversion
  SizeOfBOM: Integer;   // size of any byte order mark
begin
  Encoding := TEncodingHelper.GetEncoding(EncType);
  try
    RS := TResourceStream.Create(Inst, ResName, ResType);
    try
      { TODO: much of this code duplicates TFileIO.ReadAllText. Need to perform
              a suitable refactoring. }
      SetLength(Content, RS.Size);
      if RS.Size > 0 then
        RS.ReadBuffer(Pointer(Content)^, Length(Content));
      if HasBOM then
      begin
        SizeOfBOM := Length(Encoding.GetPreamble);
        if (SizeOfBOM > 0) and not CheckBOM(Content, Encoding) then
          raise EBug.Create('LoadResourceAsString: Invalid BOM in resource');
      end
      else
        SizeOfBOM := 0;
      Result := Encoding.GetString(
        Content, SizeOfBOM, Length(Content) - SizeOfBOM
      );
    finally
      RS.Free;
    end;
  finally
    TEncodingHelper.FreeEncoding(Encoding);
  end;
end;

end.

