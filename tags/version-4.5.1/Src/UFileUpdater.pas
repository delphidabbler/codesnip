{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Object that updates local CodeSnip data files using a supplied stream of
 * data.
}


unit UFileUpdater;


interface


uses
  // Delphi
  Classes,
  // Project
  UDataStreamIO, UEncodings, UExceptions;


type
  ///  <summary>
  ///  Class that updates local CodeSnip data files using a supplied stream of
  ///  data.
  ///  </summary>
  TFileUpdater = class(TObject)
  public
    type
      ///  <summary>Type of event triggered when reporting progress in updating
      ///  data files.</summary>
      ///  <param name="Sender">TObject [in] Object instance that triggered the
      ///  event.</param>
      ///  <param name="FilesHandled">Cardinal [in] Number of files updated to
      ///  date.</param>
      ///  <param name="TotalFiles">Cardinal [in] Total number of files to be
      ///  updated.</param>
      TProgressEvent = procedure(Sender: TObject; const FilesHandled,
        TotalFiles: Cardinal) of object;
  strict private
    var
      ///  <summary>Used to read formatted text data stream.</summary>
      fReader: TTextStreamReader;
      ///  <summary>Local data directory.</summary>
      fLocalDir: string;
      ///  <summary>Reference to any OnProgess event handler.</summary>
      fOnProgress: TProgressEvent;
    ///  <summary>
    ///  Reverts data files to state they were in before update.
    ///  </summary>
    procedure UndoUpdate;
    ///  <summary>
    ///  Creates a file from information in data stream.
    ///  </summary>
    procedure UpdateFile;
    ///  <summary>
    ///  Writes a local database file.
    ///  </summary>
    ///  <param name="Name">string [in] Name of file.</param>
    ///  <param name="Content">string [in] Content of file.</param>
    ///  <param name="UnixDate">Int64 [in] Unix format GMT date stamp to be
    ///  applied to file.</param>
    procedure WriteFile(const Name, Content: string; const UnixDate: Int64);
    ///  <summary>
    ///  Triggers any assigned OnProgress event handler with information about
    ///  file update progress to date.
    ///  </summary>
    ///  <param name="FilesHandled">Cardinal [in] Number of files updated to
    ///  date.</param>
    ///  <param name="TotalFiles">Cardinal [in] Total number of files to be
    ///  updated.</param>
    procedure ReportProgress(const FilesHandled, TotalFiles: Cardinal);
  public
    ///  <summary>Object constructor. Initialises object.</summary>
    ///  <param name="LocalDir">string [in] Directory storing local data files
    ///  that receives updated files.</param>
    ///  <param name="UpdateData">TEncodedData [in] Update data.</param>
    constructor Create(const LocalDir: string; const UpdateData: TEncodedData);
    ///  <summary>Object destructor. Tears down object.</summary>
    destructor Destroy; override;
    ///  <summary>Performs file updates.</summary>
    procedure Execute;
    ///  <summary>Event that reports progress when updating local files.
    ///  </summary>
    ///  <remarks>Triggered once for each file processed.</remarks>
    property OnProgress: TProgressEvent read fOnProgress write fOnProgress;
  end;

type
  ///  <summary>
  ///  Class of exception raised by TFileUpdater.
  ///  </summary>
  EFileUpdater = class(ECodeSnip);


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UDataBackupMgr, UDOSDateTime, UIOUtils, UUtils;


resourcestring
  // Exception messages
  cDataCorruptError = 'The data is corrupt.';


{
  Data stream format
  ------------------

  Stream comprises text characters from a MBCS encoding. Numbers are encoded as
  hex, which must be single byte characters.

  Format is:

    FileCount: SmallInt;      - number of files encoded in datastream

  followed by FileCount file records of:

    Name: SizedString;        - name of file
    UnixDate: Int64;          - file's modification date (GMT) in Unix format
                                (must be converted to local time and DOS date
                                stamp format)
    Content: SizedString;     - file contents

  Data types are:

    SmallInt      - 16 bit integer encoded as 4 hex digits
    Int64         - 64 bit integer encoded as 16 hex digits
    SizedString   - SmallInt specifying string length followed by specified
                    number of characters
    String[32]    - 32 character fixed length string
}


{ TFileUpdater }

constructor TFileUpdater.Create(const LocalDir: string;
  const UpdateData: TEncodedData);
begin
  inherited Create;
  fLocalDir := LocalDir;
  fReader := TTextStreamReader.Create(
    TBytesStream.Create(UpdateData.Data),
    TEncodingHelper.GetEncoding(UpdateData.EncodingType),
    [dsOwnsStream, dsOwnsEncoding]
  );
end;

destructor TFileUpdater.Destroy;
begin
  fReader.Free;
  inherited;
end;

procedure TFileUpdater.Execute;
var
  FileCount: Integer;   // number of files to copy to local directory
  CopiedCount: Integer; // number of files copied
begin
  TDataBackupMgr.Backup;
  try
    // Clear local data directory
    UUtils.DeleteFiles(fLocalDir, '*.*');
    // Copy in new files
    FileCount := fReader.ReadInt16;
    CopiedCount := 0;
    while CopiedCount < FileCount do
    begin
      UpdateFile;
      Inc(CopiedCount);
      ReportProgress(CopiedCount, FileCount);
    end;
  except
    // Error: restore backup
    UndoUpdate;
    raise;
  end;
  // OK: delete backup
  TDataBackupMgr.DeleteBackup;
end;

procedure TFileUpdater.ReportProgress(const FilesHandled, TotalFiles: Cardinal);
begin
  if Assigned(fOnProgress) then
    fOnProgress(Self, FilesHandled, TotalFiles);
end;

procedure TFileUpdater.UndoUpdate;
begin
  TDataBackupMgr.RestoreBackup;
  TDataBackupMgr.DeleteBackup;
end;

procedure TFileUpdater.UpdateFile;
var
  Name: string;       // name of file
  UnixDate: Int64;    // file update date per server: Unix format & GMT
  Content: string;    // file content
begin
  // Get info about file from data stream
  Name := fReader.ReadSizedString16;
  UnixDate := fReader.ReadInt64;
  Content := fReader.ReadSizedString16;
  // and create file
  WriteFile(Name, Content, UnixDate);
end;

procedure TFileUpdater.WriteFile(const Name, Content: string;
  const UnixDate: Int64);
var
  FilePath: string;   // full path to local file
  Date: IDOSDateTime; // object that encapsulates DOS date time value
begin
  FilePath := IncludeTrailingPathDelimiter(fLocalDir) + Name;
  TFileIO.WriteAllText(FilePath, Content, TEncoding.UTF8, True);
  Date := TDOSDateTimeFactory.CreateFromUnixTimeStamp(UnixDate);
  Date.ApplyToFile(FilePath);
end;

end.

