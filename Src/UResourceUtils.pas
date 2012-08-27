{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2010-2012, Peter Johnson (www.delphidabbler.com).
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
  const ResType: PChar; const EncType: TEncodingType): string;
  {Loads a resource as a string.
    @param Inst [in] Handle of module containing resource.
    @param ResName [in] Name of resource.
    @param ResType [in] Type of resource.
    @param EncType [in] Type of encoding used for resource text.
    @return Content of resource as a string.
  }


implementation


uses
  // Delphi
  SysUtils, Classes, Windows,
  // Project
  UURIEncode;


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
  const ResType: PChar; const EncType: TEncodingType): string;
  {Loads a resource as a string.
    @param Inst [in] Handle of module containing resource.
    @param ResName [in] Name of resource.
    @param ResType [in] Type of resource.
    @param EncType [in] Type of encoding used for resource text.
    @return Content of resource as a string.
  }
var
  RS: TResourceStream;  // stream onto resource
  SS: TStringStream;    // stream used to convert resource stream to string
  Encoding: TEncoding;  // encoding to use for string conversion
begin
  Encoding := TEncodingHelper.GetEncoding(EncType);
  try
    SS := TStringStream.Create('', Encoding, False);
    try
      RS := TResourceStream.Create(Inst, ResName, ResType);
      try
        SS.CopyFrom(RS, 0);
      finally
        RS.Free;
      end;
      Result := SS.DataString
    finally
      SS.Free;
    end;
  finally
    TEncodingHelper.FreeEncoding(Encoding);
  end;
end;

end.
