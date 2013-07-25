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
 * Static classes that provide information about the host system.
 *
 * Requires DelphiDabbler System information unit v3 or later.
}


unit USystemInfo;


interface


uses
  // Delphi
  ShlObj,
  // DelphiDabbler library
  PJSysInfo,
  // Project
  UBaseObjects;


type

  {
  TOSVer:
    Record that records operation system major and minor version numbers.
  }
  TOSVer = packed record
    VerHi: Word;  // Major OS version number
    VerLo: Word;  // Minor OS version number
    constructor Create(const AVerHi, AVerLo: Word);
      {Record constructor. Sets initial field values.
        @param AVerHi [in] Major version number.
        @param AVerLo [in] Minor version number.
      }
    class operator GreaterThanOrEqual(const R1, R2: TOSVer): Boolean;
      {Overload for >= operator on TOSVer.
        @param R1 [in] First record to be checked.
        @param R2 [in] Second record to be checked.
        @return True in R1 >= R2, False otherwise.
      }
  end;

  {
  TOSInfo:
    Exposes TPJOSInfo class to rest of program under a new name and extends to
    add new methods. Adds a constructor that enforces nature as static class by
    causing and assertion failure when called.
  }
  TOSInfo = class(TPJOSInfo)
  strict private
    class function CheckForKernelFn(const FnName: string): Boolean;
      {Checks if a specified function exists in OSs kernel.
        @param FnName [in] Name of required function.
        @return True if function is present in kernel, false if not.
      }
  public
    const Win2K: TOSVer = (VerHi: 5; VerLo: 0);
      {Identifies Windows 2K}
    const WinXP: TOSVer = (VerHi: 5; VerLo: 1);
      {Identifies Windows XP}
    const WinVista: TOSVer = (VerHi: 6; VerLo: 0);
      {Identifies Windows Vista}
    constructor Create;
      {Class constructor. Causes an assertion failure if called. The object must
      not be, and is never, constructed.
      }
    class function IsVistaOrLater: Boolean;
      {Checks if the underlying operating system is Windows Vista or later.
      Ignores any OS emulation.
        @return True if OS is Vista or later, False if not.
      }
    class function IsXPOrLater: Boolean;
      {Checks if the underlying operating system is Windows XP or later. Ignores
      any OS emulation.
        @return True if OS is XP or later, False if not.
      }
    class function CheckReportedOS(const MinVer: TOSVer): Boolean;
      {Checks if the OS reported by Windows is the same as or later than a
      specified version number.
        @param MinVer [in] Minimum OS version required.
        @return True if OS reported by Windows is same as or later than MinVer.
      }
  end;

  ///  <summary>Static class that provides information about the installed
  ///  version of Internet Explorer.</summary>
  TIEInfo = class(TNoConstructObject)
  strict private
    const
      ///  <summary>Majic value indicating that browser version number has not
      ///  yet been determined.</summary>
      UnsetMajorVersion = $FFFF;
    class var
      ///  <summary>Records browser's major version number.</summary>
      ///  <remarks>Is set to UnsetMajorVersion until calculated.</remarks>
      fMajorVersion: Word;
  strict private
    ///  <summary>Returns version number string of current Internet Explorer
    ///  installation.</summary>
    class function GetIEVersionStr: string;
    ///  <summary>Calculates major version number of Internet Explorer if it has
    ///  not already been calculated.</summary>
    ///  <remarks>See http://support.microsoft.com/kb/969393/en-us.</remarks>
    class procedure InitMajorVersion;
  public
    const
      ///  <summary>Minimum version of Internet Explorer required to run
      ///  CodeSnip.</summary>
      MinSupportedVersion = 6;
  public
    ///  <summary>Static class constructor: initialises class.</summary>
    class constructor Create;
    ///  <summary>Returns the major version number of the installed version of
    ///  Internet Explorer.</summary>
    class function MajorVersion: Word;
    ///  <summary>Checks if the installed version of Internet Explorer is
    //// supported by CodeSnip.</summary>
    class function IsSupportedBrowser: Boolean; inline;
    ///  <summary>Checks if the installed version of Internet Explorer supports
    ///  the "max-height" CSS property.</summary>
    class function SupportsCSSMaxHeight: Boolean; inline;
    ///  <summary>Checks if the installed version of Internet Explorer supports
    ///  the "overflow-x" CSS property sufficiently well to be used.</summary>
    class function SupportsCSSOverflowX: Boolean; inline;
    ///  <summary>Checks if the installed version of Internet Explorer has the
    ///  "auto" overflow bug and needs to have its behaviour fixed when using
    ///  "auto" with the "overflow-x" property.</summary>
    ///  <remarks>Always returns False if Internet Explorer does not have
    ///  sufficient support for the "overflow-x" property.</remarks>
    class function RequiresCSSOverflowXFix: Boolean; inline;
  end;

  {
  TComputerInfo:
    Exposes TPJComputerInfo class to rest of program under a new name with no
    additions or changes other than to enforce nature as static class by adding
    a constructor that causes an assertion failure when called.
  }
  TComputerInfo = class(TPJComputerInfo)
    constructor Create;
      {Class constructor. Causes an assertion failure if called. The object must
      not be, and is never, constructed.
      }
  end;

  {
  TSystemFolders:
    Exposes TPJSystemFolders class to rest of program under a new name and
    extends to add new methods. Adds a constructor that enforces nature as
    static class by causing and assertion failure when called.
  }
  TSystemFolders = class(TPJSystemFolders)
  strict protected
    class procedure FreePIDL(PIDL: PItemIDList);
      {Uses to shell allocator to free the memory used by a PIDL.
        @param PIDL [in] PIDL that is to be freed.
      }
    class function PIDLToFolderPath(PIDL: PItemIDList): string;
      {Returns the full path to a file system folder described by a PIDL.
        @param PIDL [in] PIDL describing folder.
        @return Full path to folder described by PIDL or '' if PIDL refers to
          virtual folder.
      }
    class function SpecialFolderPath(CSIDL: Integer): string;
      {Returns the full path to a special file system folder.
        @param CSIDL [in] Constant specifying the special folder.
        @return Folder path or '' if the special folder is virtual or CSIDL not
          supported on the OS.
      }
  public
    constructor Create;
      {Class constructor. Causes an assertion failure if called. The object must
      not be, and is never, constructed.
      }
    class function CommonAppData: string;
      {Gets common application data directory.
        @return Required directory.
      }
    class function PerUserAppData: string;
      {Gets the current user's application data directory.
        @return Required directory.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Registry, Windows, ActiveX, ShFolder,
  // Project
  UIStringList;


{ TOSVer }

constructor TOSVer.Create(const AVerHi, AVerLo: Word);
  {Record constructor. Sets initial field values.
    @param AVerHi [in] Major version number.
    @param AVerLo [in] Minor version number.
  }
begin
  VerHi := AVerHi;
  VerLo := AVerLo;
end;

class operator TOSVer.GreaterThanOrEqual(const R1, R2: TOSVer): Boolean;
  {Overload for >= operator on TOSVer.
    @param R1 [in] First record to be checked.
    @param R2 [in] Second record to be checked.
    @return True in R1 >= R2, False otherwise.
  }
begin
  Result := (R1.VerHi > R2.VerHi)
    or ((R1.VerHi = R2.VerHi) and (R1.VerLo >= R2.VerLo));
end;

{ TOSInfo }

class function TOSInfo.CheckForKernelFn(const FnName: string): Boolean;
  {Checks if a specified function exists in OSs kernel.
    @param FnName [in] Name of required function.
    @return True if function is present in kernel, false if not.
  }
const
  cKernelDLL = 'kernel32.dll';  // name of kernel DLL
var
  PFunction: Pointer; // pointer to required function if exists
begin
  // Try to load GetProductInfo func from Kernel32: present if Vista
  PFunction := GetProcAddress(GetModuleHandle(cKernelDLL), PChar(FnName));
  Result := Assigned(PFunction);
end;

class function TOSInfo.CheckReportedOS(const MinVer: TOSVer): Boolean;
  {Checks if the OS reported by Windows is the same as or later than a specified
  version number.
    @param MinVer [in] Minimum OS version required.
    @return True if OS reported by Windows is same as or later than MinVer.
  }
begin
  Result := TOSVer.Create(TOSInfo.MajorVersion, TOSInfo.MinorVersion)
    >= MinVer;
end;

constructor TOSInfo.Create;
  {Class constructor. Causes an assertion failure if called. The object must not
  be, and is never, constructed.
  }
begin
  Assert(False, ClassName + '.Create: Constructor can''t be called');
end;

class function TOSInfo.IsVistaOrLater: Boolean;
  {Checks if the underlying operating system is Windows Vista or later. Ignores
  any OS emulation.
    @return True if OS is Vista or later, False if not.
  }
begin
  // The "GetProductInfo" API function only exists in the kernel of Vista and
  // Win 2008 server and later
  Result := CheckForKernelFn('GetProductInfo');
end;

class function TOSInfo.IsXPOrLater: Boolean;
  {Checks if the underlying operating system is Windows XP or later. Ignores
  any OS emulation.
    @return True if OS is XP or later, False if not.
  }
begin
  // The "ActivateActCtx" API function only exists in the kernel of XP and Win
  // 2003 server and later
  Result := CheckForKernelFn('ActivateActCtx');
end;

{ TComputerInfo }

constructor TComputerInfo.Create;
  {Class constructor. Causes an assertion failure if called. The object must not
  be, and is never, constructed.
  }
begin
  Assert(False, ClassName + '.Create: Constructor can''t be called');
end;

{ TSystemFolders }

class function TSystemFolders.CommonAppData: string;
  {Gets common application data directory.
    @return Required directory.
  }
begin
  Result := SpecialFolderPath(CSIDL_COMMON_APPDATA);
end;

constructor TSystemFolders.Create;
  {Class constructor. Causes an assertion failure if called. The object must not
  be, and is never, constructed.
  }
begin
  Assert(False, ClassName + '.Create: Constructor can''t be called');
end;

class procedure TSystemFolders.FreePIDL(PIDL: PItemIDList);
  {Uses to shell allocator to free the memory used by a PIDL.
    @param PIDL [in] PIDL that is to be freed.
  }
var
  Malloc: IMalloc;  // shell's allocator
begin
  if Succeeded(SHGetMalloc(Malloc)) then
    Malloc.Free(PIDL);
end;

class function TSystemFolders.PerUserAppData: string;
  {Gets the current user's application data directory.
    @return Required directory.
  }
begin
  Result := SpecialFolderPath(CSIDL_APPDATA);
end;

class function TSystemFolders.PIDLToFolderPath(PIDL: PItemIDList): string;
  {Returns the full path to a file system folder described by a PIDL.
    @param PIDL [in] PIDL describing folder.
    @return Full path to folder described by PIDL or '' if PIDL refers to
      virtual folder.
  }
begin
  SetLength(Result, MAX_PATH);
  if SHGetPathFromIDList(PIDL, PChar(Result)) then
    Result := PChar(Result)
  else
    Result := '';
end;

class function TSystemFolders.SpecialFolderPath(CSIDL: Integer): string;
  {Returns the full path to a special file system folder.
    @param CSIDL [in] Constant specifying the special folder.
    @return Folder path or '' if the special folder is virtual or CSIDL not
      supported on the OS.
  }
var
  PIDL: PItemIDList;  // PIDL of the special folder
begin
  Result := '';
  if Succeeded(SHGetSpecialFolderLocation(0, CSIDL, PIDL)) then
  begin
    try
      Result := ExcludeTrailingPathDelimiter(PIDLToFolderPath(PIDL));
    finally
      FreePIDL(PIDL);
    end;
  end
end;

{ TIEInfo }

class constructor TIEInfo.Create;
begin
  fMajorVersion := UnsetMajorVersion;
end;

class function TIEInfo.GetIEVersionStr: string;
var
  Reg: TRegistry; // registry access object
const
  IERegKey = 'Software\Microsoft\Internet Explorer';
  RegValName = 'Version';         // name of registry value for IE up to 9
  RegValNameIE10 = 'svcVersion';  // name of registry value for IE 10
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly(IERegKey) then
    begin
      if Reg.ValueExists(RegValNameIE10) then
        Result := Reg.ReadString(RegValNameIE10)
      else if Reg.ValueExists(RegValName) then
        Result := Reg.ReadString(RegValName);
    end;
  finally
    Reg.Free;
  end;
end;

class procedure TIEInfo.InitMajorVersion;
var
  Vers: IStringList;  // receives parts of version number string
begin
  if fMajorVersion <> UnsetMajorVersion then
    Exit;
  Vers := TIStringList.Create(GetIEVersionStr, '.', False, True);
  if Vers.Count > 0 then
    fMajorVersion := StrToIntDef(Vers[0], 0)
  else
    fMajorVersion := 0;
end;

class function TIEInfo.IsSupportedBrowser: Boolean;
begin
  Result := MajorVersion >= MinSupportedVersion;
end;

class function TIEInfo.MajorVersion: Word;
begin
  if fMajorVersion = UnsetMajorVersion then
    InitMajorVersion;
  Result := fMajorVersion;
end;

class function TIEInfo.RequiresCSSOverflowXFix: Boolean;
begin
  // Can't require "overflow-x" fix if "overflow-x" is not itself supported
  Result := SupportsCSSOverflowX;
end;

class function TIEInfo.SupportsCSSMaxHeight: Boolean;
begin
  Result := MajorVersion >= 7;
end;

class function TIEInfo.SupportsCSSOverflowX: Boolean;
begin
  Result := MajorVersion >= 9;
end;

end.

