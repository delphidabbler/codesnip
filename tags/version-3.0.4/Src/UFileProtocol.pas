{
 * UFileProtocol.pas
 *
 * Implements a handler for the "file" URL protocol that displays a local file
 * in the associated program.
 *
 * v1.0 of 30 Jun 2009  - Original version.
 *
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
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
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
begin
  // Perform checks: URL must not be a directory and must be absolute path
  if IsDirectory(URL) then
    raise EProtocol.CreateFmt(sIsDir, [URL]);
  if not FileExists(URL) then
    raise EProtocol.CreateFmt(sBadFile, [URL]);
  // We execute the resource using an action
  Result := inherited Execute;
end;

class function TFileProtocol.SupportsProtocol(const URL: string): Boolean;
  {Checks if a URL uses the file: protocol.
    @param URL [in] URL whose protocol is to be checked.
    @return True if URL's protocol is file:, False if not.
  }

  // ---------------------------------------------------------------------------
  function IsValidAbsoluteFileName(const FileName: string): Boolean;
    {Checks if a filename is a valid, complete, absolute local file path.
      @param FileName [in] File name to be checked.
      @return True if file name is valid absolute file path, false if not.
    }
  begin
    Result := (Length(FileName) > 3) and
      (UpCase(FileName[1]) in ['A'..'Z']) and
      (FileName[2] = ':') and (FileName[3] = '\');
  end;

  function IsValidUNCFileName(const FileName: string): Boolean;
    {Checks if a filename is a valid, complete, UNC file name.
      @param FileName [in] File name to be checked.
      @return True if file name is valid UNC name, false if not.
    }
  begin
    Result := (Length(FileName) > 5) and
      AnsiStartsStr('\\', FileName) and
      (PosEx('\', FileName, 4) >= 4);
  end;
  // ---------------------------------------------------------------------------

begin
  Result := IsValidAbsoluteFileName(URL) or IsValidUNCFileName(URL);
end;

initialization

// Register the protocol with the protocol factory
TProtocolFactory.RegisterProtocol(TFileProtocol);

end.

