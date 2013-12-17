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
 * Implements a class that moves the snippets database to a new location.
}


// TODO: Rename this unit to remove "user" from name (CS.Database.Relocate ?)


unit UUserDBMove;


interface


uses
  // Delphi
  Classes;


type
  ///  <summary>Class that moves the snippets database to a new location.
  ///  </summary>
  TDBMove = class(TObject)
  public
    type
      ///  <summary>Type of event triggered by TDBMove to report progress when
      ///  moving the database files.</summary>
      ///  <param name="Sender">TObject [in] TDBMove instance that triggered the
      ///  event.</param>
      ///  <param name="Percent">Byte [in] Percentage of operation that has been
      ///  completed.</param>
      TProgress = procedure(Sender: TObject; const Percent: Byte) of object;
  strict private
    var
      ///  <summary>Reference to event handler for OnCopyFile event.</summary>
      fOnCopyFile: TProgress;
      ///  <summary>Reference to event handler for OnDeleteFile event.</summary>
      fOnDeleteFile: TProgress;
      ///  <summary>List of files in the database.</summary>
      fDBFiles: TStrings;
      ///  <summary>Existing database directory.</summary>
      fSourceDir: string;
      ///  <summary>Required new database directory.</summary>
      fDestDir: string;
    ///  <summary>Triggers OnCopyFile event, with a percentage completeness
    ///  based on the given number of files copied.</summary>
    procedure NotifyCopyFile(FileCount: Cardinal);
    ///  <summary>Triggers OnDeleteFile event, with a percentage completeness
    ///  based on the given number of files deleted.</summary>
    procedure NotifyDeleteFile(FileCount: Cardinal);
    ///  <summary>Validates source and destination directories.</summary>
    ///  <exceptions>Raises EInOutError exception if either directory is not
    ///  valid.</exceptions>
    procedure ValidateDirectories;
    ///  <summary>Calculates a returns percentage progress of towards a goal.
    ///  </summary>
    ///  <param name="Count">Cardinal [in] Number of transactions completed
    ///  towards goal.</param>
    ///  <param name="Goal">Cardinal [in] Number of transactions required to
    ///  reach goal.</param>
    function GetProgress(Count, Goal: Cardinal): Byte;
    ///  <summary>Performs database move operation and records new location.
    ///  </summary>
    procedure DoMove;
    ///  <summary>Copies database file with given index in file list from old to
    ///  new database directories.</summary>
    ///  <remarks>Triggers OnCopyFile event when the file has been copied.
    ///  </remarks>
    procedure CopyFile(const FileIdx: Cardinal);
    ///  <summary>Deletes database file with given index in file list from old
    ///  database directory.</summary>
    ///  <remarks>Triggers OnDeleteFile event when the file has been deleted.
    ///  </remarks>
    procedure DeleteFile(const FileIdx: Cardinal);
  public
    ///  <summary>Constructs and initialises new object instance.</summary>
    constructor Create;
    ///  <summary>Destroys current object instance.</summary>
    destructor Destroy; override;
    ///  <summary>Moves database from its current directory to the given new
    ///  directory.</summary>
    ///  <exceptions>Raises EInOutError exceptions if an error occurs.
    ///  </exceptions>
    procedure MoveTo(const ADirectory: string);
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
  SysUtils,
  IOUtils,
  Math,
  Windows {for inlining},
  // Project
  UAppInfo,
  UDOSDateTime,
  UIOUtils,
  UStrUtils,
  UUtils;


{ TDBMove }

procedure TDBMove.CopyFile(const FileIdx: Cardinal);
var
  SrcFile, DestFile: string;
  FileDate: IDOSDateTime;
begin
  SrcFile := fSourceDir + PathDelim + fDBFiles[FileIdx];
  DestFile := fDestDir + PathDelim + fDBFiles[FileIdx];
  FileDate := TDOSDateTimeFactory.CreateFromFile(SrcFile);
  TFileIO.CopyFile(SrcFile, DestFile);
  FileDate.ApplyToFile(DestFile);
  NotifyCopyFile(FileIdx);
end;

constructor TDBMove.Create;
begin
  inherited Create;
  fDBFiles := TStringList.Create;
end;

procedure TDBMove.DeleteFile(const FileIdx: Cardinal);
begin
  SysUtils.DeleteFile(fSourceDir + PathDelim + fDBFiles[FileIdx]);
  NotifyDeleteFile(FileIdx);
end;

destructor TDBMove.Destroy;
begin
  fDBFiles.Free;
  inherited;
end;

procedure TDBMove.DoMove;
var
  FileIdx: Cardinal;
begin
  fDBFiles.Clear;
  ListFiles(fSourceDir, '*.*', fDBFiles, False, True);
  // copy files
  NotifyCopyFile(0);
  EnsureFolders(fDestDir);
  for FileIdx := 0 to Pred(fDBFiles.Count) do
    CopyFile(FileIdx);
  // record new location BEFORE deleting old directory
  TAppInfo.ChangeUserDataDir(fDestDir);
  // delete files
  NotifyDeleteFile(0);
  for FileIdx := 0 to Pred(fDBFiles.Count) do
    DeleteFile(FileIdx);
  SysUtils.RemoveDir(fSourceDir);
end;

function TDBMove.GetProgress(Count, Goal: Cardinal): Byte;
begin
  Assert(Goal > 0, ClassName + '.GetProgress: Goal is zero');
  Count := Min(Goal, Count);
  Result := Round(100 * Count / Goal);
end;

procedure TDBMove.MoveTo(const ADirectory: string);
begin
  fSourceDir := ExcludeTrailingPathDelimiter(TAppInfo.UserDataDir);
  fDestDir := ExcludeTrailingPathDelimiter(ADirectory);
  ValidateDirectories;
  DoMove;
end;

procedure TDBMove.NotifyCopyFile(FileCount: Cardinal);
begin
  if Assigned(fOnCopyFile) then
    fOnCopyFile(Self, GetProgress(FileCount, fDBFiles.Count));
end;

procedure TDBMove.NotifyDeleteFile(FileCount: Cardinal);
begin
  if Assigned(fOnDeleteFile) then
    fOnDeleteFile(Self, GetProgress(FileCount, fDBFiles.Count));
end;

procedure TDBMove.ValidateDirectories;
resourcestring
  sSameNames = 'The new database directory is the same as the current '
    + 'directory.';
  sSourceMissing = 'No database found';
  sCantMoveToSubDir = 'Can''t move database into a sub-directory of the '
    + 'existing database directory';
  sDestMustBeRooted = 'A full path to the new database directory must be '
    + 'provided.';
  sDestMustBeEmpty = 'The new database directory must be empty';
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
    IncludeTrailingPathDelimiter(TAppInfo.UserDataDir), fDestDir
  ) then
    raise EInOutError.Create(sCantMoveToSubDir);
end;

end.

