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
  TSWAGVersion = class(TNoPublicConstructObject)
  strict private
    const
      LowestSupportedVersion: TVersionNumber = (V1: 1; V2: 0; V3: 0; V4: 0);
      LowestUnSupportedVersion: TVersionNumber = (V1: 1; V2: 1; V3: 0; V4: 0);
      SWAGVersionFileName = 'VERSION';
    var
      fSWAGFilePath: TFileName;
    function ReadVersionStr: string;
    function ReadAndValidateVersionFile: TVersionNumber;
  strict protected
    constructor InternalCreate(const SWAGDir: TFileName);
  public
    class procedure ValidateVersionFile(const SWAGDir: TFileName);
    class function GetVersion(const SWAGDir: TFileName): TVersionNumber;
  end;

  ESWAGVersion = class(ECodeSnip);
  ECorruptSWAGVersion = class(ESWAGVersion);
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
