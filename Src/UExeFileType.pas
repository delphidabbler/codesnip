{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Exposes a function that determines if a file is executable and, if so, what
 * type of file it is.
}


unit UExeFileType;


interface


type

  {
  TExeFileKind:
    The kinds of files recognised.
  }
  TExeFileKind = (
    fkUnknown,  // unknown file kind: not an executable
    fkError,    // error file kind: used for files that don't exist
    fkDOS,      // DOS executable
    fkExe32,    // 32 bit executable
    fkExe16,    // 16 bit executable
    fkDLL32,    // 32 bit DLL
    fkDLL16,    // 16 bit DLL
    fkVXD       // virtual device driver
  );


function ExeFileType(const FileName: string): TExeFileKind;
  {Examines a file and checks if it is an executable file, and if so what kind
  of executable file it is.
    @param FileName [in] Name of file to examine.
    @return Kind of executable or error code if file does not exist.
  }


implementation


uses
  // Delphi
  Classes, SysUtils, Windows;


function ExeFileType(const FileName: string): TExeFileKind;
  {Examines a file and checks if it is an executable file, and if so what kind
  of executable file it is.
    @param FileName [in] Name of file to examine.
    @return Kind of executable or error code if file does not exist.
  }
const
  cDOSRelocOffset = $18;  // offset of "pointer" to DOS relocation table
  cWinHeaderOffset = $3C; // offset of "pointer" to windows header in file
  cNEAppTypeOffset = $0D; // offset in NE windows header app type field
  cDOSMagic = $5A4D;      // magic number identifying a DOS executable
  cNEMagic = $454E;       // magic number identifying a NE executable (Win 16)
  cPEMagic = $4550;       // magic nunber identifying a PE executable (Win 32)
  cLEMagic = $454C;       // magic number identifying a Virtual Device Driver
  cNEDLLFlag = $80;       // flag in NE app type field indicating a DLL
var
  FS: TFileStream;            // stream to executable file
  WinMagic: Word;             // word that contains PE or NE magic numbers
  HdrOffset: LongInt;         // offset of windows header in exec file
  ImgHdrPE: TImageFileHeader; // PE file header record
  DOSHeader: TImageDosHeader; // DOS header
  AppFlagsNE: Byte;           // byte defining DLLs in NE format
  DOSFileSize: Integer;       // size of DOS file
begin
  try
    // Open stream onto file: raises exception if can't be read
    FS := TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone);
    try
      // Assume unknown file
      Result := fkUnknown;
      // Any exec file is at least size of DOS header long
      if FS.Size < SizeOf(DOSHeader) then
        Exit;
      FS.ReadBuffer(DOSHeader, SizeOf(DOSHeader));
      // DOS files begin with "MZ"
      if DOSHeader.e_magic <> cDOSMagic then
        Exit;
      // DOS files have length >= size indicated at offset $02 and $04
      // (offset $02 indicates length of file mod 512 and offset $04 indicates
      // no. of 512 pages in file)
      if (DOSHeader.e_cblp = 0) then
        DOSFileSize := DOSHeader.e_cp * 512
      else
        DOSFileSize := (DOSHeader.e_cp - 1) * 512 + DOSHeader.e_cblp;
      if FS.Size <  DOSFileSize then
        Exit;
      // DOS file relocation offset must be within DOS file size.
      if DOSHeader.e_lfarlc > DOSFileSize then
        Exit;
      // We know we have an executable file: assume its a DOS program
      Result := fkDOS;
      // Try to find offset of Windows program header
      if FS.Size <= cWinHeaderOffset + SizeOf(LongInt) then
        // file too small for windows header "pointer": it's a DOS file
        Exit;
      // read it
      FS.Position := cWinHeaderOffset;
      FS.ReadBuffer(HdrOffset, SizeOf(LongInt));
      // Now try to read first word of Windows program header
      if FS.Size <= HdrOffset + SizeOf(Word) then
        // file too small to contain header: it's a DOS file
        Exit;
      FS.Position := HdrOffset;
      // This word should be NE, PE or LE per file type: check which
      FS.ReadBuffer(WinMagic, SizeOf(Word));
      case WinMagic of
        cPEMagic:
        begin
          // 32 bit Windows application: now check whether app or DLL
          if FS.Size < HdrOffset + SizeOf(LongWord) + SizeOf(ImgHdrPE) then
            // file not large enough for image header: assume DOS
            Exit;
          // read Windows image header
          FS.Position := HdrOffset + SizeOf(LongWord);
          FS.ReadBuffer(ImgHdrPE, SizeOf(ImgHdrPE));
          if (ImgHdrPE.Characteristics and IMAGE_FILE_DLL) = IMAGE_FILE_DLL then
            // characteristics indicate a 32 bit DLL
            Result := fkDLL32
          else
            // characteristics indicate a 32 bit application
            Result := fkExe32;
        end;
        cNEMagic:
        begin
          // We have 16 bit Windows executable: check whether app or DLL
          if FS.Size <= HdrOffset + cNEAppTypeOffset + SizeOf(AppFlagsNE) then
            // app flags field would be beyond EOF: assume DOS
            Exit;
          // read app flags byte
          FS.Position := HdrOffset + cNEAppTypeOffset;
          FS.ReadBuffer(AppFlagsNE, SizeOf(AppFlagsNE));
          if (AppFlagsNE and cNEDLLFlag) = cNEDLLFlag then
            // app flags indicate DLL
            Result := fkDLL16
          else
            // app flags indicate program
            Result := fkExe16;
        end;
        cLEMagic:
          // We have a Virtual Device Driver
          Result := fkVXD;
        else
          // DOS application
          {Do nothing - DOS result already set};
      end;
    finally
      FS.Free;
    end;
  except
    // Exception raised in function => error result
    Result := fkError;
  end;
end;

end.

