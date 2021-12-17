{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Provides a class that reads and validates the SWAG collection's version
 * information from file.
}


unit SWAG.UVersion;


interface


uses
  // VCL
  SysUtils,
  // Project
  UBaseObjects,
  UExceptions,
  UVersionInfo;


type
  ///  <summary>Class that reads and validates the SWAG collection's version
  ///  information from file.</summary>
  TSWAGVersion = class(TNoPublicConstructObject)
  strict private
    const
      ///  <summary>SWAG version file name, without path.</summary>
      SWAGVersionFileName = 'VERSION';
    var
      ///  <summary>Stores fully specified path to SWAG version file.</summary>
      fSWAGFilePath: TFileName;
    ///  <summary>Reads and returns the version string from the version file.
    ///  </summary>
    function ReadVersionStr: string;
    ///  <summary>Reads, parses and validates version information from file.
    ///  </summary>
    ///  <returns>TVersionNumber. Structure containing version information.
    ///  </returns>
    ///  <exception>ECorruptSWAGVersion raised if version information in file
    ///  is corrupt, missing, or if the file is missing.</exception>
    ///  <exception>EUnsupportedSWAGVersion raised if the version information
    ///  read represents an unsupported format.</exception>
    function ReadAndValidateVersionFile: TVersionNumber;
  strict protected
    ///  <summary>Constructor that can only be called from with the class.
    ///  </summary>
    ///  <param name="SWAGDir">TFileName [in] Directory where version file
    ///  should be located.</param>
    constructor InternalCreate(const SWAGDir: TFileName);
  public
    const
      ///  <summary>First supported SWAG version.</summary>
      LowestSupportedVersion: TVersionNumber = (V1: 1; V2: 0; V3: 0; V4: 0);
      ///  <summary>Lowest SWAG version that is NOT supported.</summary>
      LowestUnSupportedVersion: TVersionNumber = (V1: 1; V2: 1; V3: 0; V4: 0);
    ///  <summary>Validates the version file. Returns normally if there are no
    ///  errors or raises an exception if an error is found.</summary>
    ///  <param name="SWAGDir">TFileName [in] Directory where version file
    ///  should be located.</param>
    ///  <exception>ECorruptSWAGVersion raised if version information in file
    ///  is corrupt, missing, or if the file is missing.</exception>
    ///  <exception>EUnsupportedSWAGVersion raised if the version information
    ///  read represents an unsupported format.</exception>
    class procedure ValidateVersionFile(const SWAGDir: TFileName);
    ///  <summary>Gets SWAG version from file.</summary>
    ///  <param name="SWAGDir">TFileName [in] Directory where version file
    ///  should be located.</param>
    ///  <returns>TVersionNumber. Structure containing version information.
    ///  </returns>
    ///  <exception>ECorruptSWAGVersion raised if version information in file
    ///  is corrupt, missing, or if the file is missing.</exception>
    ///  <exception>EUnsupportedSWAGVersion raised if the version information
    ///  read represents an unsupported format.</exception>
   class function GetVersion(const SWAGDir: TFileName): TVersionNumber;
  end;

  ///  <summary>Base class for exceptions raised in this unit.</summary>
  ///  <remarks>Exceptions of this type should NOT be raised directly: use
  ///  sub-classes.</remarks>
  ESWAGVersion = class(ECodeSnip);
  ///  <summary>Type of exception raised when the SWAG version file is missing
  ///  or corrupt.</summary>
  ECorruptSWAGVersion = class(ESWAGVersion);
  ///  <summary>Type of exception raised when the SWAG version is outside the
  ///  range of supported versions.</summary>
  EUnsupportedSWAGVersion = class(ESWAGVersion);


implementation


uses
  // VCL
  IOUtils,
  // Project
  UIOUtils;


{ TSWAGVersion }

class function TSWAGVersion.GetVersion(const SWAGDir: TFileName):
  TVersionNumber;
begin
  with InternalCreate(SWAGDir) do
    try
      Result := ReadAndValidateVersionFile;
    finally
      Free;
    end;
end;

constructor TSWAGVersion.InternalCreate(const SWAGDir: TFileName);
begin
  inherited InternalCreate;
  fSWAGFilePath := IncludeTrailingPathDelimiter(SWAGDir) + SWAGVersionFileName;
end;

function TSWAGVersion.ReadAndValidateVersionFile: TVersionNumber;
var
  VerStr: string;
resourcestring
  sMissingOrEmptyFile = 'Missing or empty %s file';
  sCorruptFile = 'Corrupt %s file';
  sOutOfRange = 'SWAG version %0:s is not supported. '
    + 'The version must be at least v%1:s and less than v%2:s';
begin
  VerStr := ReadVersionStr;
  if VerStr = '' then
    raise ECorruptSWAGVersion.CreateFmt(
      sMissingOrEmptyFile, [SWAGVersionFileName]
    );
  if not TVersionNumber.TryStrToVersionNumber(VerStr, Result) then
    raise ECorruptSWAGVersion.CreateFmt(sCorruptFile, [SWAGVersionFileName]);
  if (Result < LowestSupportedVersion)
    or (Result >= LowestUnSupportedVersion) then
    raise EUnsupportedSWAGVersion.CreateFmt(
      sOutOfRange,
      [
        string(Result),
        string(LowestSupportedVersion),
        string(LowestUnSupportedVersion)
      ]
    );
end;

function TSWAGVersion.ReadVersionStr: string;
begin
  if not TFile.Exists(fSWAGFilePath) then
    Exit('');
  Result := TFileIO.ReadAllText(fSWAGFilePath, TEncoding.UTF8, False);
end;

class procedure TSWAGVersion.ValidateVersionFile(const SWAGDir: TFileName);
begin
  with InternalCreate(SWAGDir) do
    try
      ReadAndValidateVersionFile;
    finally
      Free;
    end;
end;

end.
