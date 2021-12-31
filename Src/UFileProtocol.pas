{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a handler for the "file" URL protocol that displays a local file
 * in the associated program.
}


unit UFileProtocol;


interface


implementation


uses
  // Delphi
  SysUtils, ExtActns,
  // Project
  UBrowseProtocol, UProtocols, UStrUtils, UUtils;


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

  According to Microsoft, https://msdn.microsoft.com/en-us/library/aa767731.aspx,
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
  strict protected
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
  if not StrStartsStr(cProtocol, Result) then
    Exit;
  // replace C| with C:
  Result := StrReplace(Result, '|', ':'); // change c| to c:
  // make all delimiters in unix format for processing
  Result := StrReplace(Result, '\', '/');
  if StrStartsStr(cProtocol + '/', Result) then
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
  Result := StrReplace(Result, '/', '\');
end;

class function TFileProtocol.SupportsProtocol(const URL: string): Boolean;
  {Checks if a URL uses the file: protocol.
    @param URL [in] URL whose protocol is to be checked.
    @return True if URL's protocol is file:, False if not.
  }

  // ---------------------------------------------------------------------------
  function IsAbsoluteFileNameFormat(const FileName: string): Boolean;
    {Checks if a filename is in absolute local file path format. Name is not
    checked for valid characters.
      @param FileName [in] File name to be checked.
      @return True if file name is valid absolute file path, false if not.
    }
  begin
    Result := (Length(FileName) > 3)
      and IsValidDriveLetter(FileName[1])
      and (FileName[2] = ':') and (FileName[3] = '\');
  end;

  function IsUNCFileNameFormat(const FileName: string): Boolean;
    {Checks if a filename is in UNC file name format. Name is not checked for
    valid characters.
      @param FileName [in] File name to be checked.
      @return True if file name is valid UNC name, false if not.
    }
  begin
    Result := (Length(FileName) > 5)
      and StrStartsStr('\\', FileName)
      and (StrPos('\', FileName, 4) >= 4);
  end;
  // ---------------------------------------------------------------------------

var
  FileName: string; // filename part of URL
begin
  FileName := NormaliseURL(URL);
  Result := IsAbsoluteFileNameFormat(FileName) or IsUNCFileNameFormat(FileName);
end;

initialization

// Register the protocol with the protocol factory
TProtocolFactory.RegisterProtocol(TFileProtocol);

end.

