{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Object used to manage main Code Snippets database updates.
}


unit UDBUpdateMgr;


interface


uses
  // Delphi
  SysUtils,
  // Project
  UExceptions;


type
  ///  <summary>Type of exception raised when validating database update.
  ///  </summary>
  EDBUpdateValidationError = class(ECodeSnip);

type
  ///  <summary>Manages the process of installing a main database update from a
  ///  local directory containing required database files.</summary>
  TDBUpdateMgr = class(TObject)
  public
    type
      ///  <summary>Type of event triggered to report progress when updating
      ///  local database files.</summary>
      ///  <param name="Sender">TObject [in] Object that triggered event.
      ///  </param>
      ///  <param name="Progress">Bytes [in] Pecentage progress to date.</param>
      TProgressEvent = procedure(Sender: TObject; const Percentage: Single)
        of object;

  strict private
    var
      ///  <summary>Directory when the local database files are stored.
      ///  </summary>
      fLocalDir: string;
      ///  <summary>Directory containing updated database files.</summary>
      fUpdateDir: string;
      ///  <summary>Event handler for OnFileUpdateProgress event.</summary>
      fOnFileUpdateProgress: TProgressEvent;

    ///  <summary>Handles file updater's OnProgress event by passing it on to
    ///  this class&#39; own OnFileUpdateProgress event.</summary>
    ///  <param name="Sender">TObject [in] Object that triggered event.</param>
    ///  <param name="Percentage">Single [in] Percentage progress.</param>
    procedure FileUpdateProgressHandler(Sender: TObject;
      const Percentage: Single);

    ///  <summary>Updates the files in the local database from the files in the
    ///  updated data directory.</summary>
    procedure UpdateLocalDatabase;

  public

    ///  <summary>Constructs and initialises a new object instance.</summary>
    ///  <param name="LocalDir">string [in] Directory that contains the local
    ///  copy of the Code Snippets database.</param>
    ///  <param name="UpdateDir">string [in] Directory containing updated
    ///  database files.
    ///  </param>
    constructor Create(const LocalDir, UpdateDir: string);

    ///  <summary>Performs a database update.</summary>
    ///  <exception>EDBUpdateValidationError. Raised if database update fails
    ///  any validation checks.</exception>
    ///  <exception>Exception. May propagate any Exception or sub class if an
    ///  unexpected error is raised.</exception>
    procedure Execute;

    ///  <summary>Validates package of update files in given directory.
    ///  </summary>
    ///  <exception>EDBUpdateValidationError raised if any validation test
    ///  fails.</exception>
    class procedure ValidateUpdate(const UpdateDir: string);

    ///  <summary>Event triggered to report progress when updating local
    ///  database files.</summary>
    property OnFileUpdateProgress: TProgressEvent
      read fOnFileUpdateProgress write fOnFileUpdateProgress;
  end;


implementation


uses
  // Delphi
  IOUtils,
  // Project
  DB.UMetaData,
  UAppInfo,
  UFileUpdater,
  UStrUtils,
  UUtils;


{ TDBUpdateMgr }

constructor TDBUpdateMgr.Create(const LocalDir, UpdateDir: string);
begin
  inherited Create;
  fLocalDir := LocalDir;
  fUpdateDir := UpdateDir;
  EnsureFolders(fLocalDir);
end;

procedure TDBUpdateMgr.Execute;
begin
  ValidateUpdate(fUpdateDir);
  UpdateLocalDatabase
end;

procedure TDBUpdateMgr.FileUpdateProgressHandler(Sender: TObject;
  const Percentage: Single);
begin
  if not Assigned(fOnFileUpdateProgress) then
    Exit;
  fOnFileUpdateProgress(Self, Percentage);
end;

procedure TDBUpdateMgr.UpdateLocalDatabase;
var
  Updater: TFileUpdater;  // object that performs file updates.
begin
  Updater := TFileUpdater.Create(fLocalDir, fUpdateDir);
  try
    Updater.OnProgress := FileUpdateProgressHandler;
    Updater.Execute;
  finally
    Updater.Free;
  end;
end;

class procedure TDBUpdateMgr.ValidateUpdate(const UpdateDir: string);
resourcestring
  // Error messages
  sBadDirNameError = 'Update directory "%s" does not exist.';
  sEmptyDirError = 'Update directory "%s" is empty.';
  sIsSubDirError = 'The update directory must not be within CodeSnip''s '
    + 'application or user data directories, i.e. "%s" or "%s".';
  sUnrootedDirError = 'A full path to the update directory must be specified.';
  sInvalidDatabaseError = 'The update directory does not contain valid '
    + 'database files.';
  sUnrecognisedDatabaseError = 'The files in "%s" are not recognised as a '
    + 'valid database.';
  sUnsupportedDatabaseError = 'Database in "%0:s" is version %1:s. This '
    + 'version is not currently supported by CodeSnip.';
  sCurruptDatabaseError = 'The update database in "%s" is corrupt';
var
  Dir: string;
  MetaData: IDBMetaData;
begin
  Assert(UpdateDir <> '',
    ClassName + '.ValidateUpdate: UpdateDir cannot be empty string');

  Dir := ExcludeTrailingPathDelimiter(UpdateDir);

  // Dir must be rooted
  if not TPath.IsPathRooted(Dir) then
    raise EDBUpdateValidationError.Create(sUnrootedDirError);

  // Dir must exist
  if not TDirectory.Exists(Dir, False) then
    raise EDBUpdateValidationError.CreateFmt(sBadDirNameError, [Dir]);

  // Dir must not be located in CodeSnip's app or user data directories
  if StrStartsText(
    IncludeTrailingPathDelimiter(TAppInfo.CommonAppDir),
    IncludeTrailingPathDelimiter(Dir)
  ) or StrStartsText(
    IncludeTrailingPathDelimiter(TAppInfo.UserAppDir),
    IncludeTrailingPathDelimiter(Dir)
  ) then
    raise EDBUpdateValidationError.CreateFmt(
      sIsSubDirError, [TAppInfo.CommonAppDir, TAppInfo.UserAppDir]
    );

  // Dir must not be empty
  if TDirectory.IsEmpty(Dir) then
    raise EDBUpdateValidationError.CreateFmt(sEmptyDirError, [Dir]);

  // Check contents
  MetaData := TMainDBMetaDataFactory.UpdateMetaDataInstance(Dir);

  // check if data files are recognised as valid database
  if not MetaData.IsRecognised then
    raise EDBUpdateValidationError.CreateFmt(sUnrecognisedDatabaseError, [Dir]);

  // seems to be valid database: check if it's a supported version
  if not MetaData.IsSupportedVersion then
    raise EDBUpdateValidationError.CreateFmt(
      sUnsupportedDatabaseError, [Dir, string(MetaData.GetVersion)]
    );

  // database version supported: check if required meta data files and master db
  // file are present
  if MetaData.IsCorrupt
    or not TFile.Exists(Dir + PathDelim + 'categories.ini', False) then
    raise EDBUpdateValidationError.CreateFmt(sCurruptDatabaseError, [Dir]);
end;

end.

