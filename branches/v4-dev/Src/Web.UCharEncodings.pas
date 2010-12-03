{
 * Web.UCharEncodings.pas
 *
 * Implements a class that defines and provides access to character sets and
 * encodings supported when accessing web services.
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
 * The Original Code is Web.UCharEncodings.pas, formerly
 * NsWebServices.UCharEncodings.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit Web.UCharEncodings;


interface


uses
  // Project
  UEncodings, UIStringList;


type

  ///  <summary>
  ///  Static class that provies access to character sets and their associated
  ///  encodings supported when accessing web servers.
  ///  </summary>
  TWebCharEncodings = class(TEncodingHelper)
  strict private
    ///  Character set used when none is specified.
    const DefaultEncoding = 'ISO-8859-1';
    ///  List of supported character sets.
    class var fSupportedCharSets: IStringList;
  strict protected
    ///  <summary>
    ///  Checks if a named character set is in the list of supported character
    ///  sets.
    ///  </summary>
    class function IsWantedCharSet(const CharSet: string): Boolean; override;
  public
    ///  <summary>
    ///  Creates list of supported encodings.
    ///  </summary>
    class constructor Create;
    ///  <summary>
    ///  Returns command separated list of accepted character set names suitable
    ///  for inclusion in HTTP headers.
    ///  </summary>
    class function AcceptCharSet: string;
    class function DefaultCharSet: string; override;
  end;


implementation


{ TWebCharEncodings }

class function TWebCharEncodings.AcceptCharSet: string;
var
  CharSet: string;
begin
  Result := '';
  for CharSet in CharSets do
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + CharSet;
  end;
end;

class constructor TWebCharEncodings.Create;
begin
  fSupportedCharSets := TIStringList.Create(
    ['UTF-8', 'UTF-16LE', 'UTF-16BE', 'ISO-8859-1', 'Windows-1252', 'ASCII']
  );
  // NOTE: UTF-16 has been left out because some problems with PHP's
  // mb_convert_encoding() with UTF-16 have been found, so it's not safe to
  // accept this encoding
end;

class function TWebCharEncodings.DefaultCharSet: string;
begin
  Result := ISO88591CharSetName;
end;

class function TWebCharEncodings.IsWantedCharSet(const CharSet: string):
  Boolean;
begin
  Result := fSupportedCharSets.Contains(CharSet);
end;

end.

