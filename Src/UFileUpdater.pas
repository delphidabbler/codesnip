{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a class that updates local CodeSnip data files from a directory
 * containing updated files.
}


unit UFileUpdater;


interface


uses
  // Delphi
  Classes;


type
  ///  <summary>Class that updates local CodeSnip data files from a directory
  ///  containing updated files.</summary>
  TFileUpdater = class(TObject)
  public
    type
      ///  <summary>Type of event triggered when reporting progress in updating
      ///  data files.</summary>
      ///  <param name="Sender">TObject [in] TFileUpdater object instance that
      ///  triggered the event.</param>
      ///  <param name="Progress">Bytes [in] Pecentage progress to date.</param>
      TProgressEvent = procedure(Sender: TObject; const Percentage: Single)
        of object;
  strict private
    var
      ///  <summary>Main database directory containg data files to be updated.
      ///  </summary>
      fLocalDir: string;
      ///  <summary>Directory containing updated database files.</summary>
      fUpdateDir: string;
      ///  <summary>Reference to any OnProgess event handler.</summary>
      fOnProgress: TProgressEvent;
    ///  <summary>Reverts data files to state they were in before update.
    ///  </summary>
    procedure UndoUpdate;
    ///  <summary>Handles directory copier object&#39;s OnCopyFileProgress
    ///  events and triggers this object&#39;s OnProgress event with same
    ///  parameters.</summary>
    ///  <param name="Sender">TObject [in] Not used.</param>
    ///  <param name="Progress">Bytes [in] Pecentage progress to date.</param>
    procedure ProgressEventHandler(Sender: TObject; const Percent: Single);
  public
    ///  <summary>Object constructor. Initialises object.</summary>
    ///  <param name="LocalDir">string [in] Directory storing database data
    ///  files that receives updated files.</param>
    ///  <param name="UpdateDir">string [in] Directory where updated database
    ///  files are located.</param>
    constructor Create(const LocalDir, UpdateDir: string);
    ///  <summary>Performs file updates.</summary>
    procedure Execute;
    ///  <summary>Event that reports progress when updating local files.
    ///  </summary>
    ///  <remarks>Triggered once for each file processed.</remarks>
    property OnProgress: TProgressEvent read fOnProgress write fOnProgress;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UDataBackupMgr,
  UDirectoryCopier,
  UUtils;


{ TFileUpdater }

constructor TFileUpdater.Create(const LocalDir, UpdateDir: string);
begin
  inherited Create;
  fLocalDir := LocalDir;
  fUpdateDir := UpdateDir;
end;

procedure TFileUpdater.Execute;
var
  DirCopier: TDirectoryCopier;
begin
  TDataBackupMgr.Backup;
  try
    // Clear local data directory
    UUtils.DeleteFiles(fLocalDir, '*.*');
    DirCopier := TDirectoryCopier.Create;
    try
      DirCopier.OnBegin := nil;
      DirCopier.OnCopyFileProgress := ProgressEventHandler;
      DirCopier.OnEnd := nil;
      DirCopier.Copy(fUpdateDir, fLocalDir);
    finally
      DirCopier.Free;
    end;
  except
    // Error: restore backup
    UndoUpdate;
    raise;
  end;
  // OK: delete backup
  TDataBackupMgr.DeleteBackup;
end;

procedure TFileUpdater.ProgressEventHandler(Sender: TObject;
  const Percent: Single);
begin
  if Assigned(fOnProgress) then
    fOnProgress(Self, Percent);
end;

procedure TFileUpdater.UndoUpdate;
begin
  TDataBackupMgr.RestoreBackup;
  TDataBackupMgr.DeleteBackup;
end;

end.

