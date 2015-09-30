{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Various utility routines and classes for working with COM.
}


unit CS.Utils.COM;

interface

uses
  // Delphi
  Classes,
  ActiveX,
  // Project
  IntfCommon,
  UIStringList;

type
  ///  <summary>Implementation of the IEnumString interface for TStrings objects
  ///  for use with COM.</summary>
  ///  <remarks>
  ///  <para>Uses the shell's task allocator to create the strings passed out
  ///  from the IEnumString.Next method. Callers must use the task allocator to
  ///  free these strings.</para>
  ///  <para>Based on code extracted from the uAutoComplete unit at
  ///  http://users.atw.hu/delphicikk/listaz.php?id=171</para>
  ///  </remarks>
  TCOMEnumString = class(TInterfacedObject, IEnumString)
  strict private
    var
      fStrings: TStringList;
      fCurrentIdx: Integer;
  public
    constructor Create(AStrings: TStrings);
    destructor Destroy; override;
    ///  <remarks>Method of IStringEnum.</remarks>
    function Next(celt: Longint; out elt; pceltFetched: PLongint): HResult;
      stdcall;
    ///  <remarks>Method of IStringEnum.</remarks>
    function Skip(celt: Longint): HResult; stdcall;
    ///  <remarks>Method of IStringEnum.</remarks>
    function Reset: HResult; stdcall;
    ///  <remarks>Method of IStringEnum.</remarks>
    function Clone(out enm: IEnumString): HResult; stdcall;
  end;

function TaskAllocWideString(const S: string): PWideChar;
  {Allocates memory for a wide string using the Shell's task allocator and
  copies a given string into the memory as a wide string. Caller is responsible
  for freeing the buffer and must use the shell's allocator to do this.
    @param S [in] String to convert.
    @return Pointer to buffer containing wide string.
  }

implementation

uses
  // Delphi
  SysUtils,
  ComObj,
  Windows;

function TaskAllocWideString(const S: string): PWideChar;
  {Allocates memory for a wide string using the Shell's task allocator and
  copies a given string into the memory as a wide string. Caller is responsible
  for freeing the buffer and must use the shell's allocator to do this.
    @param S [in] String to convert.
    @return Pointer to buffer containing wide string.
  }
var
  StrLen: Integer;  // length of string in bytes
begin
  // Store length of string, allowing for terminal #0
  StrLen := Length(S) + 1;
  // Allocate buffer for wide string using task allocator
  Result := CoTaskMemAlloc(StrLen * SizeOf(WideChar));
  if not Assigned(Result) then
    raise EOutOfMemory.Create('TaskAllocWideString: can''t allocate buffer.');
  // Convert string to wide string and store in buffer
  StringToWideChar(S, Result, StrLen);
end;

{ TCOMEnumString }

function TCOMEnumString.Clone(out enm: IEnumString): HResult;
begin
  Result := E_NOTIMPL;
  pointer(enm) := nil;
exit;
  try
    enm := TCOMEnumString.Create(fStrings);
    Result := S_OK;
  except
    on E: Exception do
    begin
      Pointer(enm) := nil;
      if E is EOutOfMemory then
        Result := E_OUTOFMEMORY
      else
        Result := E_UNEXPECTED;
    end;
  end;
end;

constructor TCOMEnumString.Create(AStrings: TStrings);
begin
  inherited Create;
  fCurrentIdx := 0;
  fStrings := TStringList.Create;
  fStrings.Assign(AStrings);
end;

destructor TCOMEnumString.Destroy;
begin
  fStrings.Free;
  inherited;
end;

function TCOMEnumString.Next(celt: Integer; out elt; pceltFetched: PLongint):
  HResult;
var
  EltFetched: Integer;
  EltStr: string;
begin
  EltFetched := 0;
  while (EltFetched < celt) and (fCurrentIdx < fStrings.Count) do
  begin
    EltStr := fStrings[fCurrentIdx];
    TPointerList(elt)[EltFetched] := TaskAllocWideString(EltStr);
    Inc(EltFetched);
    Inc(fCurrentIdx);
  end;
  if pceltFetched <> nil then
    pceltFetched^ := EltFetched;
  if EltFetched = celt then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function TCOMEnumString.Reset: HResult;
begin
  fCurrentIdx := 0;
  Result := S_OK;
end;

function TCOMEnumString.Skip(celt: Integer): HResult;
begin
  if (fCurrentIdx + celt) <= fStrings.Count then
  begin
    Inc(fCurrentIdx, celt);
    Result := S_OK;
  end
  else
  begin
    fCurrentIdx := fStrings.Count;
    Result := S_FALSE;
  end;
end;

initialization

OleInitialize(nil);

finalization

OleUninitialize;

end.

