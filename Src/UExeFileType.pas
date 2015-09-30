{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
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
    fkVXD,      // virtual device driver
    fkExe64,    // 64 bit executable
    fkDLL64,    // 64 bit DLL
    fkROM       // ROM image (PE format)
  );

  {
  TExeFileKinds:
    Set of TExeFileKind values.
  }
  TExeFileKinds = set of TExeFileKind;


function ExeFileType(const FileName: string): TExeFileKind;
  {Examines a file and checks if it is an executable file, and if so what kind
  of executable file it is.
    @param FileName [in] Name of file to examine.
    @return Kind of executable or error code if file does not exist.
  }


implementation


uses
  // Delphi
  Classes,
  SysUtils,
  Windows;


function ExeFileType(const FileName: string): TExeFileKind;
const
  cWinHeaderOffset = $3C; // offset of "pointer" to windows header in file
  cNEAppTypeOffset = $0D; // offset in NE windows header app type field
  cDOSMagic = $5A4D;      // magic number identifying a DOS executable
  cNEMagic = $454E;       // magic number identifying a NE executable (Win 16)
  cPEMagic = $4550;       // magic nunber identifying a PE executable (Win 32)
  cLEMagic = $454C;       // magic number identifying a Virtual Device Driver
  cNEDLLFlag = $80;       // flag in NE app type field indicating a DLL
  cPEDLLFlag = $2000;     // flag in PE Characteristics field indicating s DLL
  cPE32Magic = $10B;      // magic number identifying 32 bit PE executable
  cPE64Magic = $20B;      // magic number identifying 64 bit executable
  cPEROMMagic = $107;     // magic number identifying ROM image
var
  FS: TFileStream;                      // stream onto executable file
  WinMagic: Word;                       // word that contains PE/NE/LE magic #s
  HdrOffset: LongInt;                   // offset of windows header in exec file
  DOSHeader: IMAGE_DOS_HEADER;          // DOS header record
  PEFileHdr: IMAGE_FILE_HEADER;         // PE file header record
  PEOptHdrMagic: Word;                  // PE "optional" header magic #
  AppFlagsNE: Byte;                     // byte defining DLLs in NE format
  DOSFileSize: Integer;                 // size of DOS file
  IsPEDLL: Boolean;                     // whether PE file is DLL
begin
  try
    // Open stream onto file: raises exception if can't be read
    FS := TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone);
    try
      // Assume unkown file
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
          // 'PE' signature followed by to 0 bytes
          FS.ReadBuffer(WinMagic, SizeOf(Word));
          if WinMagic <> 0 then
            Exit;
          // 32 or 64 bit Windows application: now check whether app or DLL
          // by reading file header record and checking Characteristics field
          if FS.Size < HdrOffset + SizeOf(LongWord) + SizeOf(PEFileHdr)
            + SizeOf(PEOptHdrMagic) then
            Exit;
          FS.Position := HdrOffset + SizeOf(LongWord);
          FS.ReadBuffer(PEFileHdr, SizeOf(PEFileHdr));
          IsPEDLL := (PEFileHdr.Characteristics and cPEDLLFlag)
            = cPEDLLFlag;
          // check if 32 bit, 64 bit (or ROM) by reading Word value following
          // file header (actually this is first field of "optional" PE header)
          // read magic number at start of "optional" PE header that follows
          FS.ReadBuffer(PEOptHdrMagic, SizeOf(PEOptHdrMagic));
          case PEOptHdrMagic of
            cPE32Magic:
              if IsPEDLL then
                Result := fkDLL32
              else
                Result := fkExe32;
            cPE64Magic:
              if IsPEDLL then
                Result := fkDLL64
              else
                Result := fkExe64;
            cPEROMMagic:
              Result := fkROM;
            else
              Result := fkUnknown;  // unknown PE magic number
          end;
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

