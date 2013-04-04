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
      ///  <summary>Information specific to the code using this object.
      ///  </summary>
      fCallerInfo: string;
  public
    ///  <summary>Constructs a new object instance.</summary>
    ///  <param name="CallerInfo">string [in] Information about calling code.
    ///  </param>
    constructor Create(const CallerInfo: string);
    ///  <summary>Version number of currently executing program.</summary>
    property CurrentVersion: TVersionNumber read fCurrentVersion;
    ///  <summary>Version number of latest available program release.</summary>
    ///  <remarks>Returns 0.0.0.0 if Execute has not been called.</remarks>
    property LatestVersion: TVersionNumber read fLatestVersion;
    ///  <summary>URL from which latest version can be downloaded.</summary>
    ///  <remarks>This property is the empty string if no update is available or
    ///  if Execute has not been called.</remarks>
    property DownloadURL: string read fDownloadURL;
    ///  <summary>Informs if an update to the current program is available.
    ///  </summary>
    ///  <returns>Boolean. True if an update is available, False otherwise.
    ///  </returns>
    ///  <remarks>Always returns False if Execute has not been called.</remarks>
    function IsUpdateAvailable: Boolean;
    ///  <summary>Checks with web service if a program update is available and
    ///  sets properties accordingly.</summary>
    ///  <returns>Boolean. True if an update is available, False otherwise.
    ///  </returns>
    function Execute: Boolean;
  end;


implementation


uses
  // Project
  UAppInfo, Web.UProgramUpdateMgr;


{ TProgramUpdateChecker }

constructor TProgramUpdateChecker.Create(const CallerInfo: string);
begin
  inherited Create;
  fCallerInfo := CallerInfo;
  fCurrentVersion := TAppInfo.ProgramReleaseVersion;
end;

function TProgramUpdateChecker.Execute: Boolean;
var
  ProgUpdateMgr: TProgramUpdateMgr;
begin
  ProgUpdateMgr := TProgramUpdateMgr.Create;
  try
    ProgUpdateMgr.SignOn(fCallerInfo);
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

