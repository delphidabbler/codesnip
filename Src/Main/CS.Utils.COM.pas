{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2014-2016, Peter Johnson (www.delphidabbler.com).
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
      ///  <summary>String list being enumerated.</summary>
      fStrings: TStringList;
      ///  <summary>Current index in enueration.</summary>
      fCurrentIdx: Integer;
  public
    ///  <summary>Constructs a new instance of the enumerator.</summary>
    constructor Create(AStrings: TStrings);
    ///  <summary>Tidies up and destroys a the enumerator instances</summary>
    destructor Destroy; override;
    ///  <summary>Retrieves a specified number of items in the enumeration
    ///  sequence.</summary>
    ///  <param name="celt">LongInt [in] Number of elements to fetch from
    ///  enumeration.</param>
    ///  <param name="elt">Untyped [out] Wide string array of elements fetched
    ///  from enumeration. Strings must be freed using task allocator.</param>
    ///  <param name="pceltFetched">PLongint [in] Pointer to LongInt that
    ///  receives count of number of elements actually fetched. This parameter
    ///  may be nil if the count is not required.</param>
    ///  <returns>S_OK if number of strings fetched is the same as were
    ///  requested or S_FALSE otherwise.</returns>
    ///  <remarks>Method of IEnumString.</remarks>
    function Next(celt: Longint; out elt; pceltFetched: PLongint): HResult;
      stdcall;
    ///  <summary>Skips a specified number of items in the enumeration sequence.
    ///  </summary>
    ///  <param name="celt">LongInt [in] Number of elements to skip in the
    ///  enumeration.</param>
    ///  <returns>S_OK if the number of elements skipped equals the number
    ///  requested of S_FALSE otherwise.</returns>
    ///  <remarks>Method of IEnumString.</remarks>
    function Skip(celt: Longint): HResult; stdcall;
    ///  <summary>Resets the enumeration sequence to the beginning.</summary>
    ///  <returns>S_OK</returns>
    ///  <remarks>Method of IEnumString.</remarks>
    function Reset: HResult; stdcall;
    ///  <summary>Creates a new enumerator that contains the same enumeration
    ///  state as the current one.</summary>
    ///  <param name="enm">IEnumString [out] Contains reference to cloned
    ///  enumerator.</param>
    ///  <returns>S_OK on success, E_OUTOFMEMORY if unsufficient memory to
    ///  create enumerator or E_UNEXPECTED for any other error.</returns>
    ///  <remarks>Method of IEnumString.</remarks>
    function Clone(out enm: IEnumString): HResult; stdcall;
  end;

///  <summary>Allocates memory for a wide string using the Shell's task
///  allocator and copies a given string into the memory as a wide string.
///  </summary>
///  <param name="S">string [in] String to convert.</param>
///  <returns>Pointer to buffer containing the converted wide string.</returns>
///  <remarks>Caller is responsible for freeing the buffer and must use the
///  shell's allocator to do this.</remarks>
function TaskAllocWideString(const S: string): PWideChar;


implementation

uses
  // Delphi
  SysUtils,
  ComObj,
  Windows;

function TaskAllocWideString(const S: string): PWideChar;
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

