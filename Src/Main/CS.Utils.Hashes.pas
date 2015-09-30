{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Contains various hash functions.
}


unit CS.Utils.Hashes;


interface


///  <summary>Hash function for case sensitive strings.</summary>
///  <remarks>Uses Paul Larson hash.</remarks>
function StrHash(const S: string): Integer;

///  <summary>Hash function for case insensitive strings.</summary>
///  <remarks>Uses Paul Larson hash on lower cased string.</remarks>
function TextHash(const S: string): Integer;


implementation


uses
  // Project
  UStrUtils;


///  <summary>Paul Larson's string hash function.</summary>
///  <remarks>
///  <para>Converted from C# code at https://gist.github.com/zihotki/3725825.
///  </para>
///  <para>This hash has been checked on both short and sentence length English
///  language Unicode string hash keys and it performs better than ELFHash in
///  these cases.</para>
///  </remarks>
function PaulLarsonHash(const S: UnicodeString): Cardinal;
const
  Salt = 1756473;
var
  Idx: Integer;
begin
  {$RANGECHECKS OFF}
  Result := Salt;
  // we use indexed loop instead of for .. in because old style loop is faster
  for Idx := 1 to Length(S) do
    Result := Result * 101 + Ord(S[Idx]);
  {$RANGECHECKS ON}
end;

function StrHash(const S: string): Integer;
begin
  Result := Integer(PaulLarsonHash(S));
end;

function TextHash(const S: string): Integer;
begin
  Result := Integer(PaulLarsonHash(StrToLower(S)));
end;

end.

