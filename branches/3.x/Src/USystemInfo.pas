{
 * USystemInfo.pas
 *
 * Static classes that provide information about the host system.
 *
 * Requires DelphiDabbler System information unit v3 or later.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is USystemInfo.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2013 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit USystemInfo;


interface


uses
  // Delphi
  ShlObj,
  // DelphiDabbler library
  PJSysInfo;


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
  private
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
    class function BrowserVer: Word;
      {Gets the major version number of the installed internet explorer browser.
      Works for all version if IE from v4 onwards. See
      http://support.microsoft.com/kb/969393/en-us.
        @return Browser version >=4 or 0 if earlier browser or on error.
      }
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

class function TOSInfo.BrowserVer: Word;
  {Gets the major version number of the installed internet explorer browser.
  Works for all version if IE from v4 onwards. See
  http://support.microsoft.com/kb/969393/en-us.
    @return Browser version >=4 or 0 if earlier browser or on error.
  }
var
  Reg: TRegistry;     // registry access object
const
  cRegKey = 'Software\Microsoft\Internet Explorer'; // required registry key
  cRegValue = 'Version';      // name of required registry value
  cRegValue10 = 'svcVersion'; // name of required registry value for IE10

  // Get major version number from named registry value
  function GetVer(const ValName: string): Word;
  var
    Vers: IStringList;  // receives parts of version number string;
    begin
      Vers := TIStringList.Create(Reg.ReadString(ValName), '.', False, True);
      // we want most significant version number
      if (Vers.Count > 0) then
        Result := StrToIntDef(Vers[0], 0)
      else
        Result := 0;
    end;

begin
  Result := 0;
  if CheckReportedOS(WinXP) then
    Reg := TRegistry.Create(KEY_READ or KEY_WOW64_64KEY)
  else
    // KEY_WOW64_64KEY is not supported on Windows 2000
    Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey(cRegKey, False) then
    begin
      if Reg.ValueExists(cRegValue10) then
        Result := GetVer(cRegValue10)
      else if Reg.ValueExists(cRegValue) then
        Result := GetVer(cRegValue);
    end;
  finally
    Reg.Free;
  end;
end;

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

end.

