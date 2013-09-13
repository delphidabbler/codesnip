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


{ TODO: Use Paul Larson hash in all string comparers. }

///  <summary>Paul Larson's string hash function.</summary>
///  <remarks>
///  <para>Converted from C# code at https://gist.github.com/zihotki/3725825.
///  </para>
///  <para>This hash has been checked on both short and sentence length English
///  language Unicode string hash keys and it performs better than ELFHash in
///  these cases.</para>
///  </remarks>
function PaulLarsonHash(const S: UnicodeString): Cardinal;


implementation


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

end.

