{
 * USystemInfo.pas
 *
 * Static class that provide information about the host system.
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
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
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
  // DelphiDabbler library
  PJSysInfo;


type

  {
  TOSVer:
    Record that records operation system major and minor version numbers.
  }
  TOSVer = packed record
    VerHi: Word;  // major OS version number
    VerLo: Word;  // minor OS version number
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
    add new methods.
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


implementation


uses
  // Delphi
  SysUtils, Registry, Windows,
  // Project
  UIStringList;


{ TOSInfo }

class function TOSInfo.BrowserVer: Word;
  {Gets the major version number of the installed internet explorer browser.
  Works for all version if IE from v4 onwards. See
  http://support.microsoft.com/kb/969393/en-us.
    @return Browser version >=4 or 0 if earlier browser or on error.
  }
var
  Reg: TRegistry;     // registry access object
  Vers: IStringList;  // receives parts of version number string
const
  cRegKey = 'Software\Microsoft\Internet Explorer'; // required registry key
  cRegValue = 'Version';  // name of required registry value
begin
  Result := 0;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly(cRegKey) then
    begin
      if Reg.ValueExists(cRegValue) then
      begin
        Vers := TIStringList.Create(
          Reg.ReadString(cRegValue), '.', False, True
        );
        // we want most significant version number
        if (Vers.Count > 0) then
          Result := StrToIntDef(Vers[0], 0);
      end;
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

end.

