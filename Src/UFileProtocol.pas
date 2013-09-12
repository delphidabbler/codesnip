{
 * UFileProtocol.pas
 *
 * Implements a handler for the "file" URL protocol that displays a local file
 * in the associated program.
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
 * The Original Code is UFileProtocol.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UFileProtocol;


interface


implementation


uses
  // Delphi
  SysUtils, StrUtils, ExtActns,
  // Project
  UBrowseProtocol, UProtocols, UUtils;


{
  The file: protocol.

  The file protocol is of the form file://<hostname>/<path>. <hostname> can be
  omitted giving file:///<path> which is equivalent to file://localhost/<path>.
  <path> must be absolute.
  See http://equinox-project.org/spec/file-uri-spec.txt.

  The IE web browser control strips the file:// component of the protocol and
  converts the specified file into the OS file name. This means that by the time
  TFileProtocol receives the URL (via a TWebBrowser event handler), the URL has
  been converted into the actual OS file name.

  IE also supports direct specification of the file name, without file://
  prefix. UNC file names are also supported, with or without the file:// prefix.

  According to Microsoft, http://msdn.microsoft.com/en-us/library/aa767731.aspx,
  valid file paths understood by TWebBrowser are:

  + file:///C|/Dirs/FileName.ext         (browses file name)
  + file:///C|/Dirs/                     (browses directory)
  + C:\Dirs\                             (browses directory)
  + \Dirs\                               (directory on local primary drive)

  Experiment shows that "|" can be replaced by ":".

  Additionally UNC file names such as the following are valid and equivalent:
  + file://///servername/sharename/filename
  + file:///\\servername\sharename/filename
  + file://servername/sharename/filename
  \\servername\sharename\filename

  By the time TFileProtocol sees any of this it is in the form or either
  + C:\path\filename
  or
  + \\servername\sharename\filename

  So TFileProtocol checks to see if a URL is a file: protocol by checking the
  URL to see if it conforms with one of the above.

  NOTE TFileProtocol does not support the local primary drive format or
  directories. The URL must specify a file and it must exist.
}


type

  {
  TFileProtocol:
    Implements a handler for the file: protocol that causes a file to be
    displayed by its associated program.
  }
  TFileProtocol = class sealed(TBrowseProtocol)
  protected
    class function NormaliseURL(const URL: string): string; override;
      {Converts URL into its normal form. If URL contains file:// protocol the
      URL is converted into a standard absolute or UNC file name as appropriate.
        @param URL [in] URL to be normalised.
        @return Normalised URL. Any URL or file name expect for file:// is
          unchanged.
      }
  public
    class function SupportsProtocol(const URL: string): Boolean; override;
      {Checks if a URL uses the file: protocol.
        @param URL [in] URL whose protocol is to be checked.
        @return True if URL's protocol is file:, False if not.
      }
    function Execute: Boolean; override;
      {Executes referenced file using shell to display URL in default browser.
        @return True.
        @except raises exception if the file doesn't exist or is a directory.
      }
  end;


{ TFileProtocol }

function TFileProtocol.Execute: Boolean;
  {Executes referenced file using shell to display URL in default browser.
    @return True.
    @except raises exception if the file doesn't exist or is a directory.
  }
resourcestring
  // error message used if file does not exist
  sBadFile = 'File "%s" does not exist';
  sIsDir = '"%s" is a directory. File expected';
var
  FileName: string; // filename from URL
begin
  FileName := NormaliseURL(URL);
  // Perform checks: URL must not be a directory and must be absolute path
  if IsDirectory(FileName) then
    raise EProtocol.CreateFmt(sIsDir, [URL]);
  if not FileExists(FileName) then
    raise EProtocol.CreateFmt(sBadFile, [URL]);
  // We execute the resource using an action
  Result := inherited Execute;
end;

class function TFileProtocol.NormaliseURL(const URL: string): string;
  {Converts URL into its normal form. If URL contains file:// protocol the URL
  is converted into a standard absolute or UNC file name as appropriate.
    @param URL [in] URL to be normalised.
    @return Normalised URL. Any URL or file name expect for file:// is
      unchanged.
  }
const
  cProtocol = 'file://';  // file protocol
begin
  Result := URL;
  // url doesn't start with file:// so assume a simple file name
  if not AnsiStartsStr(cProtocol, Result) then
    Exit;
  // replace C| with C:
  Result := ReplaceStr(Result, '|', ':'); // change c| to c:
  // make all delimiters in unix format for processing
  Result := ReplaceStr(Result, '\', '/'); // change \ in path to /
  if AnsiStartsStr(cProtocol + '/', Result) then
    // starts with "file:///" => remove "file:///"
    //   file:///C:/filename
    //     => C:/filename
    //   file://///servername/sharename/filename
    //     => //servername/sharename/filename
    Delete(Result, 1, Length(cProtocol + '/'))
  else
    // starts with "file://" => remove "file:"
    //   file://servername/sharename/filename
    //     => //servername/sharename/filename
    Delete(Result, 1, Length(cProtocol) - 2);
  // change to DOS path delimiters
  Result := ReplaceStr(Result, '/', '\');
end;

class function TFileProtocol.SupportsProtocol(const URL: string): Boolean;
  {Checks if a URL uses the file: protocol.
    @param URL [in] URL whose protocol is to be checked.
    @return True if URL's protocol is file:, False if not.
  }
var
  FileName: string; // filename part of URL
begin
  FileName := NormaliseURL(URL);
  Result := IsValidAbsoluteFileName(FileName) or IsValidUNCFileName(FileName);
end;

initialization

// Register the protocol with the protocol factory
TProtocolFactory.RegisterProtocol(TFileProtocol);

end.

