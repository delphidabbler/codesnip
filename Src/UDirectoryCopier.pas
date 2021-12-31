{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2020-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that performs shallow directory copies and moves.
}


unit UDirectoryCopier;

interface

uses
  // VCL
  Classes,
  Windows, // for inlining
  SysUtils,
  // Project
  UIStringList;

type

  ///  <summary>Class that makes shallow copies/movesof directories.</summary>
  ///  <remarks>A shallow copy is one that copies files at the top level of the
  ///  directory only and does not copy sub-directories.</remarks>
  TDirectoryCopier = class(TObject)
  public
    type
      ///  <summary>Type of event triggered by TDirectoryCopier to report
      ///  progress when copying or moving directories.</summary>
      ///  <param name="Sender">TObject [in] TDirectoryCopier instance that
      ///  triggered the event.</param>
      ///  <param name="Percent">Byte [in] Percentage of operation that has been
      ///  completed.</param>
      TProgressEvent = procedure(Sender: TObject; const Percent: Single)
        of object;
  strict private
    var
      fOnBegin: TNotifyEvent;
      fOnCopyFileProgress: TProgressEvent;
      fOnAfterCopyDir: TNotifyEvent;
      fOnDeleteFileProgress: TProgressEvent;
      fOnAfterDeleteDir: TNotifyEvent;
      fOnEnd: TNotifyEvent;
    ///  <summary>Copies given file from given source directory to given
    ///  destination directory.</summary>
    procedure DoCopyFile(const SrcDir, DestDir, FileName: string);
    ///  <summary>Deletes given file from given directory.</summary>
    procedure DoDeleteFile(const Dir, FileName: string); inline;
    ///  <summary>Copies all top level files from the given source directory to
    ///  the given destination directory.</summary>
    procedure DoCopyDir(const SrcDir, DestDir: string);
    ///  <summary>Deletes all top level files from the given directory then
    ///  deletes the directory itself, if empty.</summary>
    procedure DoDeleteDir(const Dir: string);
    ///  <summary>Triggers OnBegin event.</summary>
    procedure NotifyBegin;
    ///  <summary>Triggers OnEnd event.</summary>
    procedure NotifyEnd;
    ///  <summary>Triggers OnCopyFile event, with a percentage completeness
    ///  based on the given number of files copied and total number of files.
    ///  </summary>
    procedure NotifyCopyFile(FileIdx, TotalFiles: Cardinal); inline;
    ///  <summary>Triggers OnDeleteFile event, with a percentage completeness
    ///  based on the given number of files deleted and total number of files.
    ///  </summary>
    procedure NotifyDeleteFile(FileIdx, TotalFiles: Cardinal); inline;
    ///  <summary>Triggers OnAfterCopyDir event.</summary>
    procedure NotifyAfterCopyDir; inline;
    ///  <summary>Triggers OnAfterDeleteDir event.</summary>
    procedure NotifyAfterDeleteDir; inline;
    ///  <summary>Calculates percentage progress of towards a goal.</summary>
    ///  <param name="Count">Cardinal [in] Number of transactions completed
    ///  towards goal.</param>
    ///  <param name="Goal">Cardinal [in] Number of transactions required to
    ///  reach goal. Must be greater than 0.</param>
    ///  <returns>Single: Progress as a percentage between 0.0 and 100.0.
    ///  </returns>
    ///  <remarks>If Count is greater than than Goal then 100.0 is returned.
    ///  </remarks>
    function GetProgress(Count, Goal: Cardinal): Single;
    ///  <summary>Gets a list of all top level files in the given directory.
    ///  </summary>
    function GetFiles(const Dir: string): IStringList;
  public
    ///  <summary>Event triggered before operation begins.</summary>
    property OnBegin: TNotifyEvent
      read fOnBegin write fOnBegin;
    ///  <summary>Event triggered after operation ends.</summary>
    property OnEnd: TNotifyEvent
      read fOnEnd write fOnEnd;
    ///  <summary>Event triggered just before file copying begins and once for
    ///  each file copied. Reports progress towards completion of copy
    ///  operation.</summary>
    property OnCopyFileProgress: TProgressEvent
      read fOnCopyFileProgress write fOnCopyFileProgress;
    ///  <summary>Event triggered when all files have been copied at the end of
    ///  a copy operation.</summary>
    property OnAfterCopyDir: TNotifyEvent
      read fOnAfterCopyDir write fOnAfterCopyDir;
    ///  <summary>Event triggered just before file deletion begins and once for
    ///  each file deleted. Reports progress towards completion of delete
    ///  operation.</summary>
    property OnDeleteFileProgress: TProgressEvent
      read fOnDeleteFileProgress write fOnDeleteFileProgress;
    ///  <summary>Events triggered when all files have been deleted at the end
    ///  of a delete operation.</summary>
    property OnAfterDeleteDir: TNotifyEvent
      read fOnAfterDeleteDir write fOnAfterDeleteDir;
    ///  <summary>Moves all top level files from the given source directory to
    ///  the given destination directory.</summary>
    ///  <remarks>The top level files will be deleted from the source directory
    ///  after they have been copied. The directory will be deleted only if it
    ///  contains no subdirectories. The destination directory will be created
    ///  if it does not exist.</remarks>
    procedure Move(const SrcDir, DestDir: string);
    ///  <summary>Copies all top level files from the given source directory to
    ///  the given destination directory.</summary>
    ///  <remarks>Any subdirectories of the source directory will not be copied.
    ///  The destination directory will be created if it does not exist.
    ///  </remarks>
    procedure Copy(const SrcDir, DestDir: string);
  end;

implementation

uses
  // VCL
  Math,
  // Project
  UDOSDateTime,
  UIOUtils,
  UUtils;

{ TDirectoryCopier }

procedure TDirectoryCopier.Copy(const SrcDir, DestDir: string);
begin
  NotifyBegin;
  DoCopyDir(SrcDir, DestDir);
  NotifyEnd;
end;

procedure TDirectoryCopier.DoCopyDir(const SrcDir, DestDir: string);
var
  Files: IStringList;
  Idx: Integer;
begin
  Files := GetFiles(SrcDir);
  NotifyCopyFile(0, Files.Count);
  EnsureFolders(DestDir);
  for Idx := 0 to Pred(Files.Count) do
  begin
    DoCopyFile(SrcDir, DestDir, Files[Idx]);
    NotifyCopyFile(Idx + 1, Files.Count);
  end;
  NotifyAfterCopyDir;
end;

procedure TDirectoryCopier.DoCopyFile(const SrcDir, DestDir, FileName: string);
var
  SrcFile, DestFile: string;
  FileDate: IDOSDateTime;
begin
  SrcFile := SrcDir + PathDelim + FileName;
  DestFile := DestDir + PathDelim + FileName;
  FileDate := TDOSDateTimeFactory.CreateFromFile(SrcFile);
  TFileIO.CopyFile(SrcFile, DestFile);
  FileDate.ApplyToFile(DestFile);
end;

procedure TDirectoryCopier.DoDeleteDir(const Dir: string);
var
  Files: IStringList;
  Idx: Integer;
begin
  Files := GetFiles(Dir);
  NotifyDeleteFile(0, Files.Count);
  for Idx := 0 to Pred(Files.Count) do
  begin
    DoDeleteFile(Dir, Files[Idx]);
    NotifyDeleteFile(Idx + 1, Files.Count);
  end;
  SysUtils.RemoveDir(Dir);
  NotifyAfterDeleteDir;
end;

procedure TDirectoryCopier.DoDeleteFile(const Dir, FileName: string);
begin
  SysUtils.DeleteFile(ExcludeTrailingPathDelimiter(Dir) + PathDelim + FileName);
end;

function TDirectoryCopier.GetFiles(const Dir: string): IStringList;
var
  Files: TStringList;
begin
  Files := TStringList.Create;
  try
    ListFiles(ExcludeTrailingPathDelimiter(Dir), '*.*', Files, False, True);
    Result := TIStringList.Create(Files);
  finally
    Files.Free
  end;
end;

function TDirectoryCopier.GetProgress(Count, Goal: Cardinal): Single;
begin
  Assert(Goal > 0, ClassName + '.GetProgress: Goal is zero');
  if Count >= Goal then
    Exit(100.0);
  Result := 100.0 * Count / Goal;
end;

procedure TDirectoryCopier.Move(const SrcDir, DestDir: string);
begin
  NotifyBegin;
  DoCopyDir(SrcDir, DestDir);
  DoDeleteDir(SrcDir);
  NotifyEnd;
end;

procedure TDirectoryCopier.NotifyAfterCopyDir;
begin
  if Assigned(fOnAfterCopyDir) then
    fOnAfterCopyDir(Self);
end;

procedure TDirectoryCopier.NotifyAfterDeleteDir;
begin
  if Assigned(fOnAfterDeleteDir) then
    fOnAfterDeleteDir(Self);
end;

procedure TDirectoryCopier.NotifyBegin;
begin
  if Assigned(fOnBegin) then
    fOnBegin(Self);
end;

procedure TDirectoryCopier.NotifyCopyFile(FileIdx, TotalFiles: Cardinal);
begin
  if Assigned(fOnCopyFileProgress) then
    fOnCopyFileProgress(Self, GetProgress(FileIdx, TotalFiles));
end;

procedure TDirectoryCopier.NotifyDeleteFile(FileIdx, TotalFiles: Cardinal);
begin
  if Assigned(fOnCopyFileProgress) then
    fOnDeleteFileProgress(Self, GetProgress(FileIdx, TotalFiles));
end;

procedure TDirectoryCopier.NotifyEnd;
begin
  if Assigned(fOnEnd) then
    fOnEnd(Self);
end;

end.
