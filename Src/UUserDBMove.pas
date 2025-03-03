{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that moves a collection's data to a new location.
}


unit UUserDBMove;


interface


uses
  // Project
  DB.UCollections,
  UDirectoryCopier;


type
  ///  <summary>Class that moves a collection's data to a new location.
  ///  </summary>
  TUserDBMove = class(TObject)
  public
    type
      ///  <summary>Type of event triggered to report progress when moving a
      ///  collection's data files.</summary>
      ///  <param name="Sender">TObject [in] TUserDBMove instance that triggered
      ///  the event.</param>
      ///  <param name="Percent">Byte [in] Percentage of operation that has been
      ///  completed.</param>
      TProgress = procedure(Sender: TObject; const Percent: Byte)
        of object;
  strict private
    var
      ///  <summary>Reference to event handler for OnCopyFile event.</summary>
      fOnCopyFile: TProgress;
      ///  <summary>Reference to event handler for OnDeleteFile event.</summary>
      fOnDeleteFile: TProgress;
      ///  <summary>Directory containg existing collection data.</summary>
      fSourceDir: string;
      ///  <summary>Required new collection data directory.</summary>
      fDestDir: string;
      ///  <summary>Vault to be moved.</summary>
      fCollection: TVault;
      ///  <summary>Instance of class used to perform directory move.</summary>
      fDirCopier: TDirectoryCopier;
    ///  <summary>Validates source and destination directories.</summary>
    ///  <exception>Raises EInOutError exception if either directory is not
    ///  valid.</exception>
    procedure ValidateDirectories;
    ///  <summary>Handles TDirectoryCopier.OnAfterCopyDir event to update the
    ///  collection data directory.</summary>
    ///  <remarks>Collection data location is updated once the collection's data
    ///  has been copied but before the old collection data directory is
    ///  deleted.</remarks>
    procedure SetNewDBDirectory(Sender: TObject);
    ///  <summary>Handles TDirectoryCopier.OnCopyFileProgress event and passes
    ///  the given progress percentage on to this class' similar OnCopyFile
    ///  event.</summary>
    procedure ReportCopyProgress(Sender: TObject; const Percent: Single);
    ///  <summary>Handles TDirectoryCopier.OnDeleteFileProgress event and passes
    ///  the given progress percentage on to this class' similar OnDeleteFile
    ///  event.</summary>
    procedure ReportDeleteProgress(Sender: TObject; const Percent: Single);
  public
    ///  <summary>Constructs and initialises new object instance.</summary>
    constructor Create;
    ///  <summary>Destroys current object instance.</summary>
    destructor Destroy; override;
    ///  <summary>Moves vault data from its current directory to the given new
    ///  directory.</summary>
    ///  <exceptions>Raises EInOutError exceptions if an error occurs.
    ///  </exceptions>
    procedure MoveTo(const ACollection: TVault; const ADirectory: string);
    ///  <summary>Event triggered just before file copying begins and once for
    ///  each file copied. Reports progress towards completion of copy
    ///  operation.</summary>
    property OnCopyFile: TProgress read fOnCopyFile write fOnCopyFile;
    ///  <summary>Event triggered just before file deletion begins and once for
    ///  each file deleted. Reports progress towards completion of delete
    ///  operation.</summary>
    property OnDeleteFile: TProgress read fOnDeleteFile write fOnDeleteFile;
  end;


implementation


uses
  // Delphi
  SysUtils, IOUtils,
  // Project
  UAppInfo,
  UStrUtils;


{ TUserDBMove }

constructor TUserDBMove.Create;
begin
  inherited Create;
  fDirCopier := TDirectoryCopier.Create;
  fDirCopier.OnAfterCopyDir := SetNewDBDirectory;
  fDirCopier.OnCopyFileProgress := ReportCopyProgress;
  fDirCopier.OnDeleteFileProgress := ReportDeleteProgress;
end;

destructor TUserDBMove.Destroy;
begin
  fDirCopier.Free;
  inherited;
end;

procedure TUserDBMove.MoveTo(const ACollection: TVault;
  const ADirectory: string);
begin
  fCollection := ACollection;
  fSourceDir := ExcludeTrailingPathDelimiter(ACollection.Storage.Directory);
  fDestDir := ExcludeTrailingPathDelimiter(ADirectory);
  ValidateDirectories;
  fDirCopier.Move(fSourceDir, fDestDir);
end;

procedure TUserDBMove.ReportCopyProgress(Sender: TObject;
  const Percent: Single);
begin
  if Assigned(fOnCopyFile) then
    fOnCopyFile(Self, Round(Percent));
end;

procedure TUserDBMove.ReportDeleteProgress(Sender: TObject;
  const Percent: Single);
begin
  if Assigned(fOnDeleteFile) then
    fOnDeleteFile(Self, Round(Percent));
end;

procedure TUserDBMove.SetNewDBDirectory(Sender: TObject);
var
  Collections: TVaults;
begin
  Collections := TVaults.Instance;
  // record new location BEFORE deleting old directory
  fCollection.Storage.Directory := fDestDir;
  Collections.Update(fCollection);
  // Persist collections immediately to save new directory ASAP to prevent
  // directory change being lost following a program crash.
  Collections.Save;
end;

procedure TUserDBMove.ValidateDirectories;
resourcestring
  sSameNames = 'The new collection data directory is the same as the current '
    + 'directory.';
  sSourceMissing = 'No collection data found';
  sCantMoveToSubDir = 'Can''t move the collection into a sub-directory of the '
    + 'its existing data directory';
  sDestMustBeRooted = 'A full path to the new collectio data directory must be '
    + 'provided.';
  sDestMustBeEmpty = 'The new collection data directory must be empty';
begin
  if not TPath.IsPathRooted(fDestDir) then
    raise EInOutError.Create(sDestMustBeRooted);

  if not TDirectory.Exists(fSourceDir) or TDirectory.IsEmpty(fSourceDir) then
    raise EInOutError.Create(sSourceMissing);

  if TDirectory.Exists(fDestDir) and not TDirectory.IsEmpty(fDestDir) then
    raise EInOutError.Create(sDestMustBeEmpty);

  if SameFileName(fSourceDir, fDestDir) then
    raise EInOutError.Create(sSameNames);

  if StrStartsText(
    IncludeTrailingPathDelimiter(fCollection.Storage.Directory), fDestDir
  ) then
    raise EInOutError.Create(sCantMoveToSubDir);
end;

end.

