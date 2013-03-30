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
 * Implements a class that checks if a program update is available and provides
 * information about it if so.
}


unit UProgramUpdateChecker;


interface


uses
  // Project
  UVersionInfo;


type
  ///  <summary>Checks if a program update is available and provides information
  ///  about it if so.</summary>
  ///  <remarks>This class is a wrapper round the TProgramUpdateMgr class that
  ///  interacts with the online program update web service.</remarks>
  TProgramUpdateChecker = class(TObject)
  private
    var
      ///  <summary>Value of CurrentVersion property.</summary>
      fCurrentVersion: TVersionNumber;
      ///  <summary>Value of LatestVersion property.</summary>
      fLatestVersion: TVersionNumber;
      ///  <summary>Value of DownloadURL property.</summary>
      fDownloadURL: string;
  public
    ///  <summary>Version number of currently executing program.</summary>
    property CurrentVersion: TVersionNumber read fCurrentVersion;
    ///  <summary>Version number of latest available program release.</summary>
    property LatestVersion: TVersionNumber read fLatestVersion;
    ///  <summary>URL from which latest version can be downloaded.</summary>
    ///  <remarks>This property is the empty string if no update is available.
    ///  </remarks>
    property DownloadURL: string read fDownloadURL;
    ///  <summary>Informs if an update to the current program is available.
    ///  </summary>
    ///  <returns>Boolean. True if an update is available, False otherwise.
    ///  </returns>
    function IsUpdateAvailable: Boolean;
    ///  <summary>Checks with web service if a program update is available and
    ///  sets properties accordingly.</summary>
    ///  <param name="CallerInfo">string [in] Information about calling code
    ///  that is sent to web service.</param>
    ///  <returns>Boolean. True if an update is available, False otherwise.
    ///  </returns>
    function Execute(const CallerInfo: string): Boolean;
  end;


implementation


uses
  // Project
  UAppInfo, Web.UProgramUpdateMgr;


{ TProgramUpdateChecker }

function TProgramUpdateChecker.Execute(const CallerInfo: string): Boolean;
var
  ProgUpdateMgr: TProgramUpdateMgr;
begin
  fCurrentVersion := TAppInfo.ProgramReleaseVersion;
  ProgUpdateMgr := TProgramUpdateMgr.Create;
  try
    ProgUpdateMgr.SignOn(CallerInfo);
    fLatestVersion := ProgUpdateMgr.LatestProgramVersion;
    Result := IsUpdateAvailable;
    if Result then
      fDownloadURL := ProgUpdateMgr.DownloadURL
    else
      fDownloadURL := '';
  finally
    ProgUpdateMgr.Free;
  end;
end;

function TProgramUpdateChecker.IsUpdateAvailable: Boolean;
begin
  Result := fCurrentVersion < fLatestVersion;
end;

end.
