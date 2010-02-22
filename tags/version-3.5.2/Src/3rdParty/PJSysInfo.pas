{
 * PJSysInfo.pas
 *
 * System information unit. Contains various static classes, constant and type
 * definitions and global variables for use in providing information about the
 * computer and the operating system.
 *
 * Also contains a deprecated component and some functions code for backward
 * compatibility with older versions of the unit. This code is not compiled by
 * default, but compilation can be enabled by defining the
 * PJSYSINFO_COMPILE_DEPRECATED symbol.
 *
 * $Rev$
 * $Date$
 *
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
 * The Original Code is PJSysInfo.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   Guillermo Fazzolari (bug fix in v2.0.1)
 *   Laurent Pierre (PRODUCT_* constants and suggested GetProductInfo API code
 *     used in v3.0)
 *
 * ***** END LICENSE BLOCK *****
}


unit PJSysInfo;


// Conditional defines

// NOTE: Deprecated code will not be compiled and no component will be
// registered unless PJSYSINFO_COMPILE_DEPRECATED is defined before compiling.
// You can either define in project options or on the command line, or you can
// remove the '.' character before the '$' in the following line.
{.$DEFINE PJSYSINFO_COMPILE_DEPRECATED}

// Assume all required facilities available
{$DEFINE REGISTRYEX}        // extra TRegistry methods available
{$DEFINE WARNDIRS}          // $WARN compiler directives available
{$DEFINE DEPRECATED}        // deprecated directive available
{$DEFINE EXCLUDETRAILING}   // ExcludeTrailing... SysUtils functions valid
{$DEFINE MESSAGEDIRS}       // $MESSAGE compiler directives available
{$DEFINE HASLONGWORD}       // LongWord type defined

// Undefine facilities not available in earlier compilers
// Note: Delphi 1/2 not included since code will not compile on these compilers
{$IFDEF VER100} // Delphi 3
  {$UNDEF REGISTRYEX}
  {$UNDEF WARNDIRS}
  {$UNDEF DEPRECATED}
  {$UNDEF EXCLUDETRAILING}
  {$UNDEF MESSAGEDIRS}
  {$UNDEF HASLONGWORD}
{$ENDIF}
{$IFDEF VER120} // Delphi 4
  {$UNDEF WARNDIRS}
  {$UNDEF DEPRECATED}
  {$UNDEF EXCLUDETRAILING}
  {$UNDEF MESSAGEDIRS}
{$ENDIF}
{$IFDEF VER130} // Delphi 5
  {$UNDEF WARNDIRS}
  {$UNDEF DEPRECATED}
  {$UNDEF MESSAGEDIRS}
{$ENDIF}
{$IFDEF VER140} // Delphi 6
  {$UNDEF WARNDIRS}
{$ENDIF}

{$IFDEF PJSYSINFO_COMPILE_DEPRECATED}
  {$IFDEF MESSAGEDIRS}
    {$MESSAGE Hint '*** Compiling deprecated functions and component ***'}
  {$ENDIF}
{$ENDIF}


interface


uses
  // Delphi
  SysUtils, Classes, Windows;


// -----------------------------------------------------------------------------
// Windows types and constants
// -----------------------------------------------------------------------------

type

  {
  _OSVERSIONINFOEXA, OSVERSIONINFOEXA, TOSVersionInfoExA, POSVersionInfoExA:
    ANSI version of the Win API OSVERSIONINFOEX structure.
  }
  _OSVERSIONINFOEXA = packed record
    dwOSVersionInfoSize: DWORD;               // size of structure
    dwMajorVersion: DWORD;                    // major OS version number
    dwMinorVersion: DWORD;                    // minor OS version number
    dwBuildNumber: DWORD;                     // OS build number
    dwPlatformId: DWORD;                      // OS platform identifier
    szCSDVersion: array[0..127] of AnsiChar;  // service pack or extra info
    wServicePackMajor: WORD;                  // service pack major version no.
    wServicePackMinor: WORD;                  // service pack minor version no.
    wSuiteMask: WORD;                         // bitmask that stores OS suite(s)
    wProductType: Byte;                       // additional info about system
    wReserved: Byte;                          // reserved for future use
  end;
  OSVERSIONINFOEXA = _OSVERSIONINFOEXA;
  TOSVersionInfoExA = _OSVERSIONINFOEXA;
  POSVersionInfoExA = ^TOSVersionInfoExA;

  {
  _OSVERSIONINFOEXW, OSVERIONINFOEXW, TOSVersionInfoExW, POSVersionInfoExW:
    Unicode version of the Win API OSVERSIONINFOEX structure.
  }
  _OSVERSIONINFOEXW = packed record
    dwOSVersionInfoSize: DWORD;               // size of structure
    dwMajorVersion: DWORD;                    // major OS version number
    dwMinorVersion: DWORD;                    // minor OS version number
    dwBuildNumber: DWORD;                     // OS build number
    dwPlatformId: DWORD;                      // OS platform identifier
    szCSDVersion: array[0..127] of WideChar;  // service pack or extra info
    wServicePackMajor: WORD;                  // service pack major version no.
    wServicePackMinor: WORD;                  // service pack minor version no.
    wSuiteMask: WORD;                         // bitmask that stores OS suite(s)
    wProductType: Byte;                       // additional info about system
    wReserved: Byte;                          // reserved for future use
  end;
  OSVERSIONINFOEXW = _OSVERSIONINFOEXW;
  TOSVersionInfoExW = _OSVERSIONINFOEXW;
  POSVersionInfoExW = ^TOSVersionInfoExW;

  {
  _OSVERSIONINFOEX, OSVERSIONINFOEX, TOSVersionInfoEx, POSVersionInfoEx:
    Default version of the Win API OSVERSIONINFOEX structure.
    From Delphi 2009 UNICODE is defined when the Unicode API is used, so we use
    this to decide which structure to use as default.
  }
  {$IFDEF UNICODE}
  _OSVERSIONINFOEX = _OSVERSIONINFOEXW;
  OSVERSIONINFOEX = OSVERSIONINFOEXW;
  TOSVersionInfoEx = TOSVersionInfoExW;
  POSVersionInfoEx = POSVersionInfoExW;
  {$ELSE}
  _OSVERSIONINFOEX = _OSVERSIONINFOEXA;
  OSVERSIONINFOEX = OSVERSIONINFOEXA;
  TOSVersionInfoEx = TOSVersionInfoExA;
  POSVersionInfoEx = POSVersionInfoExA;
  {$ENDIF}

const
  // These Windows-defined constants are required for use with TOSVersionInfoEx
  // NT Product types
  VER_NT_WORKSTATION                        = 1;
  VER_NT_DOMAIN_CONTROLLER                  = 2;
  VER_NT_SERVER                             = 3;
  // Mask representing NT product suites
  VER_SUITE_SMALLBUSINESS                   = $00000001;
  VER_SUITE_ENTERPRISE                      = $00000002;
  VER_SUITE_BACKOFFICE                      = $00000004;
  VER_SUITE_COMMUNICATIONS                  = $00000008;
  VER_SUITE_TERMINAL                        = $00000010;
  VER_SUITE_SMALLBUSINESS_RESTRICTED        = $00000020;
  VER_SUITE_EMBEDDEDNT                      = $00000040;
  VER_SUITE_DATACENTER                      = $00000080;
  VER_SUITE_SINGLEUSERTS                    = $00000100;
  VER_SUITE_PERSONAL                        = $00000200;
  VER_SUITE_SERVERAPPLIANCE                 = $00000400;
  VER_SUITE_BLADE                           = VER_SUITE_SERVERAPPLIANCE;
  VER_SUITE_EMBEDDED_RESTRICTED             = $00000800;
  VER_SUITE_SECURITY_APPLIANCE              = $00001000;
  VER_SUITE_STORAGE_SERVER                  = $00002000;
  VER_SUITE_COMPUTE_SERVER                  = $00004000;
  VER_SUITE_WH_SERVER                       = $00008000;

  //
  // These Windows-defined constants are required for use with the
  // GetProductInfo API call (Vista and later)
  // ** Thanks to Laurent Pierre for providing these defintions.
  // ** Additional definitions were obtained from
  //    http://msdn.microsoft.com/en-us/library/ms724358
  //
  PRODUCT_BUSINESS                          = $00000006;
  PRODUCT_BUSINESS_N                        = $00000010;
  PRODUCT_CLUSTER_SERVER                    = $00000012;
  PRODUCT_DATACENTER_SERVER                 = $00000008;
  PRODUCT_DATACENTER_SERVER_CORE            = $0000000C;
  PRODUCT_DATACENTER_SERVER_CORE_V          = $00000027;
  PRODUCT_DATACENTER_SERVER_V               = $00000025;
  PRODUCT_ENTERPRISE                        = $00000004;
  PRODUCT_ENTERPRISE_E                      = $00000046;  // new for 6.1
  PRODUCT_ENTERPRISE_N                      = $0000001B;
  PRODUCT_ENTERPRISE_SERVER                 = $0000000A;
  PRODUCT_ENTERPRISE_SERVER_CORE            = $0000000E;
  PRODUCT_ENTERPRISE_SERVER_CORE_V          = $00000029;
  PRODUCT_ENTERPRISE_SERVER_IA64            = $0000000F;
  PRODUCT_ENTERPRISE_SERVER_V               = $00000026;
  PRODUCT_HOME_BASIC                        = $00000002;
  PRODUCT_HOME_BASIC_E                      = $00000043;  // new for 6.1
  PRODUCT_HOME_BASIC_N                      = $00000005;
  PRODUCT_HOME_PREMIUM                      = $00000003;
  PRODUCT_HOME_PREMIUM_E                    = $00000044;  // new for 6.1
  PRODUCT_HOME_PREMIUM_N                    = $0000001A;
  PRODUCT_HOME_SERVER                       = $00000013;
  PRODUCT_HYPERV                            = $0000002A;
  PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT  = $0000001E;
  PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING   = $00000020;
  PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY    = $0000001F;
  PRODUCT_PROFESSIONAL                      = $00000030;  // new for 6.1
  PRODUCT_PROFESSIONAL_E                    = $00000045;  // new for 6.1
  PRODUCT_PROFESSIONAL_N                    = $00000031;  // new for 6.1
  PRODUCT_SERVER_FOR_SMALLBUSINESS          = $00000018;
  PRODUCT_SERVER_FOR_SMALLBUSINESS_V        = $00000023;
  PRODUCT_SERVER_FOUNDATION                 = $00000021;
  PRODUCT_SMALLBUSINESS_SERVER              = $00000009;
  PRODUCT_SMALLBUSINESS_SERVER_PREMIUM      = $00000019;
  PRODUCT_STANDARD_SERVER                   = $00000007;
  PRODUCT_STANDARD_SERVER_CORE              = $0000000D;
  PRODUCT_STANDARD_SERVER_CORE_V            = $00000028;
  PRODUCT_STANDARD_SERVER_V                 = $00000024;
  PRODUCT_STARTER                           = $0000000B;
  PRODUCT_STARTER_E                         = $00000042;  // new for 6.1
  PRODUCT_STARTER_N                         = $0000002F;  // new for 6.1
  PRODUCT_STORAGE_ENTERPRISE_SERVER         = $00000017;
  PRODUCT_STORAGE_EXPRESS_SERVER            = $00000014;
  PRODUCT_STORAGE_STANDARD_SERVER           = $00000015;
  PRODUCT_STORAGE_WORKGROUP_SERVER          = $00000016;
  PRODUCT_UNDEFINED                         = $00000000;
  PRODUCT_ULTIMATE                          = $00000001;
  PRODUCT_ULTIMATE_E                        = $00000047;  // new for 6.1
  PRODUCT_ULTIMATE_N                        = $0000001C;
  PRODUCT_WEB_SERVER                        = $00000011;
  PRODUCT_WEB_SERVER_CORE                   = $0000001D;
  PRODUCT_UNLICENSED                        = $ABCDABCD;
  
  //
  // These constants are required for use with GetSystemMetrics to detect
  // certain editions. GetSystemMetrics returns non-zero when passed these flags
  // if the associated edition is present.
  // Obtained from http://msdn.microsoft.com/en-us/library/ms724385
  //
  SM_TABLETPC     = 86;   // Detects XP Tablet Edition
  SM_MEDIACENTER  = 87;   // Detects XP Media Center Edition
  SM_STARTER      = 88;   // Detects Vista Starter Edition
  SM_SERVERR2     = 89;   // Detects Windows Server 2003 R2

  //
  // These constants are required when examining the
  // TSystemInfo.wProcessorArchitecture member
  // Only constants marked * are currently defined in the MS 2008 SDK
  //
  PROCESSOR_ARCHITECTURE_UNKNOWN    = $FFFF; // Unknown architecture.
  PROCESSOR_ARCHITECTURE_INTEL          = 0; // x86 *
  PROCESSOR_ARCHITECTURE_MIPS           = 1; // MIPS architecture
  PROCESSOR_ARCHITECTURE_ALPHA          = 2; // Alpha architecture
  PROCESSOR_ARCHITECTURE_PPC            = 3; // PPC architecture
  PROCESSOR_ARCHITECTURE_SHX            = 4; // SHX architecture
  PROCESSOR_ARCHITECTURE_ARM            = 5; // ARM  architecture
  PROCESSOR_ARCHITECTURE_IA64           = 6; // Intel Itanium Processor Family *
  PROCESSOR_ARCHITECTURE_ALPHA64        = 7; // Alpha64 architecture
  PROCESSOR_ARCHITECTURE_MSIL           = 8; // MSIL architecture
  PROCESSOR_ARCHITECTURE_AMD64          = 9; // x64 (AMD or Intel) *
  PROCESSOR_ARCHITECTURE_IA32_ON_WIN64 = 10; // IA32 on Win64 architecture

  //
  // These constants are provided in case the obsolete
  // TSystemInfo.dwProcessorType needs to be used.
  // Constants marked Windows CE are only used on Windows mobile and are only
  // provided here for completeness.
  // Only constants marked * are defined in the MS SDK 6.1
  //
  PROCESSOR_INTEL_386     = 386;   // Intel i386 processor *
  PROCESSOR_INTEL_486     = 486;   // Intel i486 processor *
  PROCESSOR_INTEL_PENTIUM = 586;   // Intel Pentium processor *
  PROCESSOR_INTEL_IA64    = 2200;  // Intel IA64 processor *
  PROCESSOR_AMD_X8664     = 8664;  // AMD X86 64 processor *
  PROCESSOR_MIPS_R4000    = 4000;  // MIPS R4000, R4101, R3910 processor
  PROCESSOR_ALPHA_21064   = 21064; // Alpha 210 64 processor
  PROCESSOR_PPC_601       = 601;   // PPC 601 processor
  PROCESSOR_PPC_603       = 603;   // PPC 603 processor
  PROCESSOR_PPC_604       = 604;   // PPC 604 processor
  PROCESSOR_PPC_620       = 620;   // PPC 620 processor
  PROCESSOR_HITACHI_SH3   = 10003; // Hitachi SH3 processor (Windows CE)
  PROCESSOR_HITACHI_SH3E  = 10004; // Hitachi SH3E processor (Windows CE)
  PROCESSOR_HITACHI_SH4   = 10005; // Hitachi SH4 processor (Windows CE)
  PROCESSOR_MOTOROLA_821  = 821;   // Motorola 821 processor (Windows CE)
  PROCESSOR_SHx_SH3       = 103;   // SHx SH3 processor (Windows CE)
  PROCESSOR_SHx_SH4       = 104;   // SHx SH4 processor (Windows CE)
  PROCESSOR_STRONGARM     = 2577;  // StrongARM processor (Windows CE)
  PROCESSOR_ARM720        = 1824;  // ARM 720 processor (Windows CE)
  PROCESSOR_ARM820        = 2080;  // ARM 820 processor (Windows CE)
  PROCESSOR_ARM920        = 2336;  // ARM 920 processor (Windows CE)
  PROCESSOR_ARM_7TDMI     = 70001; // ARM 7TDMI processor (Windows CE)
  PROCESSOR_OPTIL         = $494F; // MSIL processor


type

  {
  TPJOSPlatform:
    Enumerated type for OS platforms
  }
  TPJOSPlatform = (
    ospWinNT,               // Windows NT platform
    ospWin9x,               // Windows 9x platform
    ospWin32s               // Win32s platform
  );

  {
  TPJOSProduct:
    Enumerated type identifying OS product. NOTE: New values are always appended
    to the structure so as not to destroy any earlier mappings.
  }
  TPJOSProduct = (
    osUnknownWinNT,         // Unknown Windows NT OS
    osWinNT,                // Windows NT (up to v4)
    osWin2K,                // Windows 2000
    osWinXP,                // Windows XP
    osUnknownWin9x,         // Unknown Windows 9x OS
    osWin95,                // Windows 95
    osWin98,                // Windows 98
    osWinMe,                // Windows Me
    osUnknownWin32s,        // Unknown OS running Win32s
    osWinSvr2003,           // Windows Server 2003
    osUnknown,              // Completely unknown Windows
    osWinVista,             // Windows Vista
    osWinSvr2003R2,         // Windows Server 2003 R2
    osWinSvr2008,           // Windows Server 2008
    osWinLater,             // A later version of Windows than v6.1
    osWin7,                 // Windows 7
    osWinSvr2008R2          // Windows Server 2008 R2
  );

  {
  TPJProcessorArchitecture:
    Enumerated type identifying processor architecture.
  }
  TPJProcessorArchitecture = (
    paUnknown,    // Unknown architecture
    paX64,        // X64 (AMD or Intel)
    paIA64,       // Intel Itanium processor family (IPF)
    paX86         // Intel 32 bit
  );

  {
  TPJBootMode:
    Enumerated type informing how the system was booted.
  }
  TPJBootMode = (
    bmUnknown,        // unknown boot mode
    bmNormal,         // normal boot
    bmSafeMode,       // booted in safe mode
    bmSafeModeNetwork // botted in safe node with networking
  );

  {
  EPJSysInfo:
    Exceptions raised by code in this unit
  }
  EPJSysInfo = class(Exception);

  {
  TPJOSInfo:
    Static class that provides operating system version information.
  }
  TPJOSInfo = class(TObject)
  private
    class function EditionFromProductInfo: string;
      {Gets product edition from value return from GetProductInfo API.
        @return Required product edition or '' if product not recognised.
      }
    class function CheckSuite(const Suite: Integer): Boolean;
      {Checks if a given suite is installed on an NT system.
        @param Suites [in] One of the VER_SUITE_* flags.
        @return True if suite is installed. False if not installed or if not an
          NT operating system.
      }
    class function EditionFromReg: string;
      {Gets product edition from registry. Needed to get edition for NT4 pre
      SP6.
        @return Required product edition.
      }
    class function IsNT4SP6a: Boolean;
      {Checks registry to see if NT4 Service Pack 6a is installed.
        @return True if service pack is installed, False otherwise.
      }
    class function ProductTypeFromReg: string;
      {Gets code describing product type from registry. Used to get product type
      for NT4 SP5 and earlier.
        @return Required product type or '' if registry key or value can't be
          found.
      }
  public
    class function IsWin9x: Boolean;
      {Checks if a OS if one the Windows 9x line, i.e. Windows 95, Windows 98 or
      Windows Me.
        @return True if Win9x OS, False otherwise.
      }
    class function IsWinNT: Boolean;
      {Checks if OS is one of NT line (NT, 2000, XP, 2003 Server, Vista, 2008
      Server).
        @return True if WinNT, False otherwise.
      }
    class function IsWin32s: Boolean;
      {Checks if running on Win32s. This is unlikely to ever return true since
      Delphi is not believed to run on Win32s itself.
        @return True if Win32s, False otherwise.
      }
    class function IsWow64: Boolean;
      {Checks if application is running under WOW64 on a 64 bit operating
      system.
        @return True if running on WOW64, False otherwise.
      }
    class function IsServer: Boolean;
      {Checks if operating system is a server.
        @return True if server OS, False otherise.
      }
    class function IsMediaCenter: Boolean;
      {Checks if Windows Media Center is installed.
        @return True if Media Center installed, False otherwise.
      }
    class function IsTabletPC: Boolean;
      {Checks if this is Tablet PC operating system.
        @return True if Tablet PC, False otherwise.
      }
    class function HasPenExtensions: Boolean;
      {Checks if system has Pen Extensions installed.
        @return True if extensions installed, False otherwise.
      }
    class function Platform: TPJOSPlatform;
      {Identifies the OS platform.
        @return Identifier representing the platform.
        @except EPJSysInfo raised if can't determine platform.
      }
    class function Product: TPJOSProduct;
      {Identifies an OS product.
        @return Identifier representing the product.
      }
    class function ProductName: string;
      {Gets the name of the OS product.
        @return Name of product or '' if product not known. If product is later
          than most recent known version, "Post-Vista Windows" is returned.
      }
    class function MajorVersion: Integer;
      {Gets the operating system's major version number.
        @return Required version number.
      }
    class function MinorVersion: Integer;
      {Gets the operating system's minor version number.
        @return Required version number.
      }
    class function BuildNumber: Integer;
      {Gets operating system's build number.
        @return Required build number.
      }
    class function ServicePack: string;
      {Gets name of any service pack installed.
        @return Name of service pack or '' if no service pack installed.
      }
    class function ServicePackMajor: Integer;
      {Gets major version number of any installed NT service pack.
        @return Required version number or 0 if no service pack installed or
          running on Win9x.
      }
    class function ServicePackMinor: Integer;
      {Gets minor version number of any installed NT service pack.
        @return Required version number. Invalid if ServicePackMajor returns 0.
      }
    class function Edition: string;
      {Gets the product edition for an NT operating system.
        @return Required edition or '' is OS is not NT.
      }
    class function Description: string;
      {Gets full description of operating system.
        @return Required description.
      }
    class function ProductID: string;
      {Determines the Windows product ID.
        @return Required product id string.
      }
  end;

  {
  TPJComputerInfo:
    Static class that provides information about the computer.
  }
  TPJComputerInfo = class(TObject)
  public
    class function ComputerName: string;
      {Gets name of computer.
        @return Computer name.
      }
    class function UserName: string;
      {Gets name of currently logged on user.
        @return User name.
      }
    class function MACAddress: string;
      {Gets MAC address of first ethernet adapter on computer.
        @return Required MAC address or '' if no ethernet adapter found.
      }
    class function Processor: TPJProcessorArchitecture;
      {Gets processor architecture.
        @return Identifier describing  processor architecture.
      }
    class function ProcessorCount: Cardinal;
      {Gets number of processors in computer.
        @return Number of processors.
      }
    class function Is64Bit: Boolean;
      {Checks if running on 64 bit processor.
        @param True if running on 64 bit processor, False if 32 bit.
      }
    class function IsNetworkPresent: Boolean;
      {Checks if a network is present.
        @return True if network present, False otherwise.
      }
    class function BootMode: TPJBootMode;
      {Determines operating system mode used when computer was last booted.
        @return Required boot mode.
      }
  end;

  {
  TPJSystemFolders:
    Static class that provides paths of the system's standard folders.
  }
  TPJSystemFolders = class(TObject)
  public
    class function CommonFiles: string;
      {Gets fully qualified name of Common Files folder.
        @return Required folder name.
      }
    class function ProgramFiles: string;
      {Gets fully qualified name of Program Files folder
        @return Required folder name.
      }
    class function Windows: string;
      {Gets fully qualified name of Windows folder.
        @return Required folder name.
      }
    class function System: string;
      {Gets fully qualified name of Windows system folder
        @return Required folder name.
      }
    class function SystemWow64: string;
      {Gets the fully qualified path name of the folder folder used to store
      shared 32 bit code on 64 bit Windows.
        @return Required folder name. Always returns '' on 32 bit Windows.
      }
    class function Temp: string;
      {Gets fully qualified name of system's temporary folder.
        @return Required folder name.
      }
  end;


  {$IFDEF PJSYSINFO_COMPILE_DEPRECATED}
  {
  TPJSysInfo:
    Component that provides system information.
  }
  TPJSysInfo = class(TComponent)
  private
    function GetComputerName: string;
      {Read access method for ComputerName property.
        @return Property value.
      }
    function GetUserName: string;
      {Read access method for UserName property.
        @return Property value.
      }
    function GetCommonFilesFolder: string;
      {Read access method for CommonFilesFolder property.
        @return Property value.
      }
    function GetProgramFilesFolder: string;
      {Read access method for ProgramFilesFolder property.
        @return Property value.
      }
    function GetSystemFolder: string;
      {Read access method for SystemFolder property.
        @return Property value.
      }
    function GetTempFolder: string;
      {Read access method for TempFolder property.
        @return Property value.
      }
    function GetWindowsFolder: string;
      {Read access method for WindowsFolder property.
        @return Property value.
      }
    function GetOSDesc: string;
      {Read access method for OSDesc property.
        @return Property value.
      }
    function GetOSBuildNumber: Integer;
      {Read access method for OSBuildNumber property.
        @return Property value.
      }
    function GetOSMajorVersion: Integer;
      {Read access method for OSMajorVersion property.
        @return Property value.
      }
    function GetOSMinorVersion: Integer;
      {Read access method for OSMinorVersion property.
        @return Property value.
      }
    function GetOSPlatform: TPJOSPlatform;
      {Read access method for OSPlatform property.
        @return Property value.
      }
    function GetOSProduct: TPJOSProduct;
      {Read access method for OSProduct property.
        @return Property value.
      }
    function GetOSProductName: string;
      {Read access method for OSProductName property.
        @return Property value.
      }
    function GetOSProductType: string;
      {Read access method for OSProductType property.
        @return Property value.
      }
    function GetOSServicePack: string;
      {Read access method for OSServicePack property.
        @return Property value.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Ensures only one instance of component can be placed
      on a form.
        @param AOwner [in] Owning component.
        @except EPJSysInfo raised if there is already a component present.
      }
    property ComputerName: string read GetComputerName;
      {Name of computer}
    property UserName: string read GetUserName;
      {Name of currently logged on user}
    property CommonFilesFolder: string read GetCommonFilesFolder;
      {Fully qualified name of common files folder}
    property ProgramFilesFolder: string read GetProgramFilesFolder;
      {Fully qualified name of program files folder}
    property SystemFolder: string read GetSystemFolder;
      {Fully qualified name of Windows system folder}
    property TempFolder: string read GetTempFolder;
      {Fully qualified name of current temporary folder}
    property WindowsFolder: string read GetWindowsFolder;
      {Fully qualified name of Windows folder}
    property OSBuildNumber: Integer read GetOSBuildNumber;
      {Operating system build number}
    property OSDesc: string read GetOSDesc;
      {Full description of operating system: included product name, suite and
      build numbers as applicable}
    property OSMajorVersion: Integer read GetOSMajorVersion;
      {Major version number of operating system}
    property OSMinorVersion: Integer read GetOSMinorVersion;
      {Minor version number of operating system}
    property OSPlatform: TPJOSPlatform read GetOSPlatform;
      {Operating system platform identifier}
    property OSProduct: TPJOSProduct read GetOSProduct;
      {Operating system product identifier}
    property OSProductName: string read GetOSProductName;
      {Name of operating system}
    property OSProductType: string read GetOSProductType;
      {Type of operating system - for NT. Always empty string for Win9x}
    property OSServicePack: string read GetOSServicePack;
      {Name of any service pack for NT or additional product info for Win9x}
  end {$IFDEF DEPRECATED}deprecated{$ENDIF};
  {$ENDIF}

{$IFDEF PJSYSINFO_COMPILE_DEPRECATED}

function SIGetComputerName: string;
  {$IFDEF DEPRECATED}deprecated;{$ENDIF}
  {Gets name of computer.
    @return Computer name.
  }

function SIGetUserName: string;
  {$IFDEF DEPRECATED}deprecated;{$ENDIF}
  {Gets name of current user.
    @return Required user name.
  }

function SIGetCommonFilesFolder: string;
  {$IFDEF DEPRECATED}deprecated;{$ENDIF}
  {Gets fully qualified name of Common Files folder.
    @return Required folder name.
  }

function SIGetProgramFilesFolder: string;
  {$IFDEF DEPRECATED}deprecated;{$ENDIF}
  {Gets fully qualified name of Program Files folder.
    @return Required folder name.
  }

function SIGetSystemFolder: string;
  {$IFDEF DEPRECATED}deprecated;{$ENDIF}
  {Gets fully qualified name of Windows system folder.
    @return Required folder name.
  }

function SIGetTempFolder: string;
  {$IFDEF DEPRECATED}deprecated;{$ENDIF}
  {Gets fully qualified name of current temporary folder.
    @return Required folder name.
  }

function SIGetWindowsFolder: string;
  {$IFDEF DEPRECATED}deprecated;{$ENDIF}
  {Gets fully name of Windows folder.
    @return Required folder name.
  }

function SIGetOSBuildNumber: Integer;
  {$IFDEF DEPRECATED}deprecated;{$ENDIF}
  {Gets build number of operating system.
    @return Required build number.
  }

function SIGetOSDesc: string;
  {$IFDEF DEPRECATED}deprecated;{$ENDIF}
  {Get full description of operating system.
    @return Required description including product name, suite and build numbers
      as applicable.
  }

function SIGetOSMajorVersion: Integer;
  {$IFDEF DEPRECATED}deprecated;{$ENDIF}
  {Gets major version of OS.
    @return Required version number
  }

function SIGetOSMinorVersion: Integer;
  {$IFDEF DEPRECATED}deprecated;{$ENDIF}
  {Gets minor version of OS.
    @return Required version number.
  }

function SIGetOSPlatform: TPJOSPlatform;
  {$IFDEF DEPRECATED}deprecated;{$ENDIF}
  {Gets code identifying OS platform program is running on.
    @return Required platform identifier.
  }

function SIGetOSProduct: TPJOSProduct;
  {$IFDEF DEPRECATED}deprecated;{$ENDIF}
  {Gets code identifying OS product program is running on.
    @return Required product identifier.
  }

function SIGetOSProductName: string;
  {$IFDEF DEPRECATED}deprecated;{$ENDIF}
  {Gets product name of operating system.
    @return Required product name.
  }

function SIGetOSProductType: string;
  {$IFDEF DEPRECATED}deprecated;{$ENDIF}
  {Gets type of OS product.
    @return Product type for Win NT and empty string for other OSs.
  }

function SIGetOSServicePack: string;
  {$IFDEF DEPRECATED}deprecated;{$ENDIF}
  {Gets service pack info for NT or additional release info for Win9x.
    @return Required service pack / additional information.
  }

{$ENDIF}

{$IFNDEF HASLONGWORD}
type
  // Define LongWord for compilers that don't have it
  LongWord = Windows.DWORD;
{$ENDIF}

var

  //
  // Global variables providing extended information about the OS version
  //

  Win32HaveExInfo: Boolean = False;
    {True if extended version info available (NT4, SP6 and later)}
  // Following variables only valid if Win32HaveExInfo is True
  Win32ServicePackMajor: Integer = 0;
    {Major version number of the latest Service Pack installed on the system. If
    no service pack has been installed the value is 0}
  Win32ServicePackMinor: Integer = 0;
    {Minor version number of the latest Service Pack installed on the system}
  Win32SuiteMask: Integer = 0;
    {Bit flags that identify the product suites available on the system. Value
    is a combination of the VER_SUITE_* flags defined above}
  Win32ProductType: Integer = 0;
    {Additional information about the system. Value is one of the VER_NT_* flags
    defined above}

  Win32HaveProductInfo: Boolean = False;
    {True if product info is available on the OS, i.e. if GetProductInfo API
    function is available}
  Win32ProductInfo: LongWord = 0;
    {Product info for the operating system. Contains 0 if Win32HaveProductInfo
    is false}


{$IFDEF PJSYSINFO_COMPILE_DEPRECATED}
procedure Register;
  {Registers component.
  }
{$ENDIF}


implementation


uses
  // Delphi
  Registry, Nb30;


{$IFDEF PJSYSINFO_COMPILE_DEPRECATED}
procedure Register;
  {Registers component.
  }
begin
  {$IFDEF WARNDIRS}{$WARN SYMBOL_DEPRECATED OFF}{$ENDIF}
  RegisterComponents('DelphiDabbler', [TPJSysInfo]);
  {$IFDEF WARNDIRS}{$WARN SYMBOL_DEPRECATED ON}{$ENDIF}
end;
{$ENDIF}


resourcestring
  // Error messages
  sUnknownPlatform = 'Unrecognized operating system platform';
  sUnknownProduct = 'Unrecognised operating system product';
  sBadRegType =  'Unsupported registry type';
  sBadProcHandle = 'Bad process handle';
  {$IFDEF PJSYSINFO_COMPILE_DEPRECATED}
  sDupInstErr = 'Only one %0:s component is permitted on a form: '
    + '%1:s is already present on %2:s';
  {$ENDIF}


const
  // Map of product codes per GetProductInfo API to product names
  // ** Laurent Pierre supplied original code on which this map is based
  //    It has been modified and extended using MSDN documentation at
  //    http://msdn.microsoft.com/en-us/library/ms724358
  cProductMap: array[1..52] of record
    Id: Cardinal; // product ID
    Name: string; // product name
  end = (
    (Id: PRODUCT_BUSINESS;
      Name: 'Business Edition';),
    (Id: PRODUCT_BUSINESS_N;
      Name: 'Business N Edition';),
    (Id: PRODUCT_CLUSTER_SERVER;
      Name: 'Cluster Server Edition';),
    (Id: PRODUCT_DATACENTER_SERVER;
      Name: 'Server Datacenter Edition (full installation)';),
    (Id: PRODUCT_DATACENTER_SERVER_CORE;
      Name: 'Server Datacenter Edition (core installation)';),
    (Id: PRODUCT_DATACENTER_SERVER_CORE_V;
      Name: 'Server Datacenter Edition without Hyper-V (core installation)';),
    (Id: PRODUCT_DATACENTER_SERVER_V;
      Name: 'Server Datacenter Edition without Hyper-V (full installation)';),
    (Id: PRODUCT_ENTERPRISE;
      Name: 'Enterprise Edition';),
    (Id: PRODUCT_ENTERPRISE_E;
      Name: 'Enterprise E Edition';),
    (Id: PRODUCT_ENTERPRISE_N;
      Name: 'Enterprise N Edition';),
    (Id: PRODUCT_ENTERPRISE_SERVER;
      Name: 'Server Enterprise Edition (full installation)';),
    (Id: PRODUCT_ENTERPRISE_SERVER_CORE;
      Name: 'Server Enterprise Edition (core installation)';),
    (Id: PRODUCT_ENTERPRISE_SERVER_CORE_V;
      Name: 'Server Enterprise Edition without Hyper-V (core installation)';),
    (Id: PRODUCT_ENTERPRISE_SERVER_IA64;
      Name: 'Server Enterprise Edition for Itanium-based Systems';),
    (Id: PRODUCT_ENTERPRISE_SERVER_V;
      Name: 'Server Enterprise Edition without Hyper-V (full installation)';),
    (Id: PRODUCT_HOME_BASIC;
      Name: 'Home Basic Edition';),
    (Id: PRODUCT_HOME_BASIC_E;
      Name: 'Home Basic E Edition';),
    (Id: PRODUCT_HOME_BASIC_N;
      Name: 'Home Basic N Edition';),
    (Id: PRODUCT_HOME_PREMIUM;
      Name: 'Home Premium Edition';),
    (Id: PRODUCT_HOME_PREMIUM_E;
      Name: 'Home Premium E Edition';),
    (Id: PRODUCT_HOME_PREMIUM_N;
      Name: 'Home Premium N Edition';),
    (Id: PRODUCT_HOME_SERVER;
      Name: 'Home Server Edition';),
    (Id: PRODUCT_HYPERV;
      Name: 'Microsoft Hyper-V Server'),
    (Id: PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT;
      Name: 'Windows Essential Business Server Management Server';),
    (Id: PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING;
      Name: 'Windows Essential Business Server Messaging Server';),
    (Id: PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY;
      Name: 'Windows Essential Business Server Security Server';),
    (Id: PRODUCT_PROFESSIONAL;
      Name: 'Professional Edition';),
    (Id: PRODUCT_PROFESSIONAL_E;
      Name: 'Professional E Edition';),
    (Id: PRODUCT_PROFESSIONAL_N;
      Name: 'Professional N Edition';),
    (Id: PRODUCT_SERVER_FOR_SMALLBUSINESS;
      Name: 'Server for Small Business Edition';),
    (Id: PRODUCT_SERVER_FOR_SMALLBUSINESS_V;
      Name: 'Windows Server 2008 without Hyper-V for Windows Essential Server '
        + 'Solutions';),
    (Id: PRODUCT_SERVER_FOUNDATION;
      Name: 'Server Foundation';),
    (Id: PRODUCT_SMALLBUSINESS_SERVER;
      Name: 'Small Business Server';),
    (Id: PRODUCT_SMALLBUSINESS_SERVER_PREMIUM;
      Name: 'Small Business Server Premium Edition';),
    (Id: PRODUCT_STANDARD_SERVER;
      Name: 'Server Standard Edition (full installation)';),
    (Id: PRODUCT_STANDARD_SERVER_CORE;
      Name: 'Server Standard Edition (core installation)';),
    (Id: PRODUCT_STANDARD_SERVER_CORE_V;
      Name: 'Server Standard Edition without Hyper-V (core installation)';),
    (Id: PRODUCT_STANDARD_SERVER_V;
      Name: 'Server Standard Edition without Hyper-V (full installation)';),
    (Id: PRODUCT_STARTER;
      Name: 'Starter Edition';),
    (Id: PRODUCT_STARTER_E;
      Name: 'Starter E Edition';),
    (Id: PRODUCT_STARTER_N;
      Name: 'Starter N Edition';),
    (Id: PRODUCT_STORAGE_ENTERPRISE_SERVER;
      Name: 'Storage Server Enterprise Edition';),
    (Id: PRODUCT_STORAGE_EXPRESS_SERVER;
      Name: 'Storage Server Express Edition';),
    (Id: PRODUCT_STORAGE_STANDARD_SERVER;
      Name: 'Storage Server Standard Edition';),
    (Id: PRODUCT_STORAGE_WORKGROUP_SERVER;
      Name: 'Storage Server Workgroup Edition';),
    (Id: PRODUCT_UNDEFINED;
      Name: 'An unknown product';),
    (Id: PRODUCT_ULTIMATE;
      Name: 'Ultimate Edition';),
    (Id: PRODUCT_ULTIMATE_E;
      Name: 'Ultimate E Edition';),
    (Id: PRODUCT_ULTIMATE_N;
      Name: 'Ultimate N Edition';),
    (Id: PRODUCT_WEB_SERVER;
      Name: 'Web Server Edition';),
    (Id: PRODUCT_WEB_SERVER_CORE;
      Name: 'Web Server Edition (core installation)';),
    (Id: Cardinal(PRODUCT_UNLICENSED);
      Name: 'Unlicensed product';)
  );

var
  // Records processor architecture information
  pvtProcessorArchitecture: Word = 0;


// -----------------------------------------------------------------------------
// Helper routines
// -----------------------------------------------------------------------------

{$IFDEF WARNDIRS}{$WARN UNSAFE_TYPE OFF}{$ENDIF}
function LoadKernelFunc(const FuncName: string): Pointer;
  {Loads a function from the OS kernel.
    @param FuncName [in] Name of required function.
    @return Pointer to function or nil if function not found in kernel.
  }
const
  cKernel = 'kernel32.dll'; // kernel DLL
begin
  Result := GetProcAddress(GetModuleHandle(cKernel), PChar(FuncName));
end;
{$IFDEF WARNDIRS}{$WARN UNSAFE_TYPE ON}{$ENDIF}

procedure InitPlatformIdEx;
  {Initialise global variables with extended OS version information if possible.
  }
type
  // Function type of the GetProductInfo API function
  TGetProductInfo = function(OSMajor, OSMinor, SPMajor, SPMinor: DWORD;
    out ProductType: DWORD): BOOL; stdcall;
  // Function type of the GetNativeSystemInfo and GetSystemInfo functions
  TGetSystemInfo = procedure(var lpSystemInfo: TSystemInfo); stdcall;
var
  OSVI: TOSVersionInfoEx;           // extended OS version info structure
  POSVI: POSVersionInfo;            // pointer to OS version info structure
  GetProductInfo: TGetProductInfo;  // pointer to GetProductInfo API function
  GetSystemInfoFn: TGetSystemInfo;  // fn used to get system info
  SI: TSystemInfo;                  // structure from GetSystemInfo API call
begin
  // Clear the structure
  FillChar(OSVI, SizeOf(OSVI), 0);
  // Get pointer to structure of non-extended type (GetVersionEx
  // requires a non-extended structure and we need this pointer to get
  // it to accept our extended structure!!)
  {$IFDEF WARNDIRS}{$WARN UNSAFE_CODE OFF}{$ENDIF}
  {$TYPEDADDRESS OFF}
  POSVI := @OSVI;
  {$TYPEDADDRESS ON}
  // Try to get exended information
  OSVI.dwOSVersionInfoSize := SizeOf(TOSVersionInfoEx);
  Win32HaveExInfo := GetVersionEx(POSVI^);
  {$IFDEF WARNDIRS}{$WARN UNSAFE_CODE ON}{$ENDIF}
  if Win32HaveExInfo then
  begin
    // We have extended info: store details in global vars
    Win32ServicePackMajor := OSVI.wServicePackMajor;
    Win32ServicePackMinor := OSVI.wServicePackMinor;
    Win32SuiteMask := OSVI.wSuiteMask;
    Win32ProductType := OSVI.wProductType;
    // Try to get product info
    GetProductInfo := LoadKernelFunc('GetProductInfo');
    Win32HaveProductInfo := Assigned(GetProductInfo);
    if Win32HaveProductInfo then
    begin
      if not GetProductInfo(
        Win32MajorVersion, Win32MinorVersion,
        Win32ServicePackMajor, Win32ServicePackMinor,
        Win32ProductInfo
      ) then
        Win32ProductInfo := PRODUCT_UNDEFINED;
    end;
  end;
  // Get processor architecture
  // prefer to use GetNativeSystemInfo() API call if available, otherwise use
  // GetSystemInfo()
  GetSystemInfoFn := LoadKernelFunc('GetNativeSystemInfo');
  if not Assigned(GetSystemInfoFn) then
    GetSystemInfoFn := Windows.GetSystemInfo;
  GetSystemInfoFn(SI);
  pvtProcessorArchitecture := SI.wProcessorArchitecture;
end;

{$IFNDEF EXCLUDETRAILING}
function ExcludeTrailingPathDelimiter(const DirOrPath: string) : string;
  {Removes any trailing '\' from given directory or path. Used for versions of
  Delphi that don't implement this routine in SysUtils.
    @param DirOrPath [in] Directory or path name to process.
    @return Directory name with any trailing backslash removed.
  }
begin
  Result := DirOrPath;
  while (Result <> '') and (Result[Length(Result)] = '\') do
    Result := Copy(Result, 1, Length(Result) - 1);
end;
{$ENDIF}

function RegOpenKeyReadOnly(const Reg: TRegistry; const Key: string): Boolean;
  {Uses registry object to open a key as read only. On versions of Delphi that
  can't open keys as read only the key is opened normally
    @param Reg [in] Registry object used to open key.
    @param Key [in] Name of registry key to be opened.
    @return True if key opened, False otherwise.
  }
begin
  {$IFDEF REGISTRYEX}
  Result := Reg.OpenKeyReadOnly(Key);
  {$ELSE}
  Result := Reg.OpenKey(Key, False);
  {$ENDIF}
end;

function GetRegistryString(const RootKey: HKEY;
  const SubKey, Name: string): string;
  {Gets a string value from the registry
    @param RootKey [in] Required registry hive.
    @param SubKey [in] Registry key within hive.
    @param Name [in] Name of registry value.
    @return Required value. Integer values are converted to strings. '' returned
      if value or key not found.
    @except EPJSysInfo raised if value is not string or integer.
  }
var
  Reg: TRegistry;          // registry access object
  ValueInfo: TRegDataInfo; // info about registry value
begin
  Result := '';
  // Open registry at required root key
  Reg := TRegistry.Create;
  try
    Reg.RootKey := RootKey;
    // Open registry key and check value exists
    if RegOpenKeyReadOnly(Reg, SubKey)
      and Reg.ValueExists(Name) then
    begin
      // Check if registry value is string or integer
      Reg.GetDataInfo(Name, ValueInfo);
      case ValueInfo.RegData of
        rdString, rdExpandString:
          // string value: just return it
          Result := Reg.ReadString(Name);
        rdInteger:
          // integer value: convert to string
          Result := IntToStr(Reg.ReadInteger(Name));
        else
          // unsupported value: raise exception
          raise EPJSysInfo.Create(sBadRegType);
      end;
    end;
  finally
    // Close registry
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function GetCurrentVersionRegStr(ValName: string): string;
  {Gets string info from Windows current version key in registry.
    @param ValName [in] Name of registry value under the current version key.
    @return Required string value or '' if value doesn't exist.
  }
const
  // required registry string
  cWdwCurrentVer = '\Software\Microsoft\Windows\CurrentVersion';
begin
  Result := GetRegistryString(
    Windows.HKEY_LOCAL_MACHINE, cWdwCurrentVer, ValName
  );
end;


{$IFDEF PJSYSINFO_COMPILE_DEPRECATED}
// -----------------------------------------------------------------------------
// Public routines
// -----------------------------------------------------------------------------

function SIGetCommonFilesFolder: string;
  {Gets fully qualified name of Common Files folder.
    @return Required folder name.
  }
begin
  Result := TPJSystemFolders.CommonFiles;
end;

function SIGetComputerName: string;
  {Gets name of computer.
    @return Computer name.
  }
begin
  Result := TPJComputerInfo.ComputerName;
end;

function SIGetOSBuildNumber: Integer;
  {Gets build number of operating system.
    @return Required build number.
  }
begin
  Result := TPJOSInfo.BuildNumber;
end;

function SIGetOSDesc: string;
  {Get full description of operating system.
    @return Required description including product name, suite and build numbers
      as applicable.
  }
begin
  Result := TPJOSInfo.Description;
end;

function SIGetOSMajorVersion: Integer;
  {Gets major version of OS.
    @return Required version number
  }
begin
  Result := TPJOSInfo.MajorVersion;
end;

function SIGetOSMinorVersion: Integer;
  {Gets minor version of OS.
    @return Required version number.
  }
begin
  Result := TPJOSInfo.MinorVersion;
end;

function SIGetOSPlatform: TPJOSPlatform;
  {Gets code identifying OS platform program is running on.
    @return Required platform identifier.
  }
begin
  Result := TPJOSInfo.Platform;
end;

function SIGetOSProduct: TPJOSProduct;
  {Gets code identifying OS product program is running on.
    @return Required product identifier.
  }
begin
  Result := TPJOSInfo.Product;
end;

function SIGetOSProductName: string;
  {Gets product name of operating system.
    @return Required product name.
  }
begin
  Result := TPJOSInfo.ProductName;
end;

function SIGetOSProductType: string;
  {Gets type of OS product.
    @return Product type for Win NT and empty string for other OSs.
  }
begin
  Result := TPJOSInfo.Edition;
end;

function SIGetOSServicePack: string;
  {Gets service pack info for NT or additional release info for Win9x.
    @return Required service pack / additional information.
  }
begin
  Result := TPJOSInfo.ServicePack;
end;

function SIGetProgramFilesFolder: string;
  {Gets fully qualified name of Program Files folder.
    @return Required folder name.
  }
begin
  Result := TPJSystemFolders.ProgramFiles;
end;

function SIGetSystemFolder: string;
  {Gets fully qualified name of Windows system folder.
    @return Required folder name.
  }
begin
  Result := TPJSystemFolders.System;
end;

function SIGetTempFolder: string;
  {Gets fully qualified name of current temporary folder.
    @return Required folder name.
  }
begin
  Result := TPJSystemFolders.Temp;
end;

function SIGetUserName: string;
  {Gets name of current user.
    @return Required user name.
  }
begin
  Result := TPJComputerInfo.UserName;
end;

function SIGetWindowsFolder: string;
  {Gets fully name of Windows folder.
    @return Required folder name.
  }
begin
  Result := TPJSystemFolders.Windows;
end;
{$ENDIF}

{$IFDEF PJSYSINFO_COMPILE_DEPRECATED}
// -----------------------------------------------------------------------------
// Component implementation
// -----------------------------------------------------------------------------

{ TPJSysInfo }

constructor TPJSysInfo.Create(AOwner: TComponent);
  {Class constructor. Ensures only one instance of component can be placed on a
  form.
    @param AOwner [in] Owning component.
    @except EPJSysInfo raised if there is already a component present.
  }
var
  Idx: Integer; // loops thru components on Owner form
begin
  // Ensure that component is unique
  for Idx := 0 to Pred(AOwner.ComponentCount) do
    if AOwner.Components[Idx] is ClassType then
      raise EPJSysInfo.CreateFmt(
        sDupInstErr,
        [ClassName, AOwner.Components[Idx].Name, AOwner.Name]
      );
  // All OK: go ahead and create component
  inherited Create(AOwner);
end;

function TPJSysInfo.GetCommonFilesFolder: string;
  {Read access method for CommonFilesFolder property.
    @return Property value.
  }
begin
  Result := TPJSystemFolders.CommonFiles;
end;

function TPJSysInfo.GetComputerName: string;
  {Read access method for ComputerName property.
    Result := Property value.
  }
begin
  Result := TPJComputerInfo.ComputerName;
end;

function TPJSysInfo.GetOSBuildNumber: Integer;
  {Read access method for OSBuildNumber property.
    @return Property value.
  }
begin
  Result := TPJOSInfo.BuildNumber;
end;

function TPJSysInfo.GetOSDesc: string;
  {Read access method for OSDesc property.
    @return Property value.
  }
begin
  Result := TPJOSInfo.Description;
end;

function TPJSysInfo.GetOSMajorVersion: Integer;
  {Read access method for OSMajorVersion property.
    @return Property value.
  }
begin
  Result := TPJOSInfo.MajorVersion;
end;

function TPJSysInfo.GetOSMinorVersion: Integer;
  {Read access method for OSMinorVersion property.
    @return Property value.
  }
begin
  Result := TPJOSInfo.MinorVersion;
end;

function TPJSysInfo.GetOSPlatform: TPJOSPlatform;
  {Read access method for OSPlatform property.
    @return Property value.
  }
begin
  Result := TPJOSInfo.Platform;
end;

function TPJSysInfo.GetOSProduct: TPJOSProduct;
  {Read access method for OSProduct property.
    @return Property value.
  }
begin
  Result := TPJOSInfo.Product;
end;

function TPJSysInfo.GetOSProductName: string;
  {Read access method for OSProductName property.
    @return Property value.
  }
begin
  Result := TPJOSInfo.ProductName;
end;

function TPJSysInfo.GetOSProductType: string;
  {Read access method for OSProductType property.
    @return Property value.
  }
begin
  Result := TPJOSInfo.Edition;
end;

function TPJSysInfo.GetOSServicePack: string;
  {Read access method for OSServicePack property.
    @return Property value.
  }
begin
  Result := TPJOSInfo.ServicePack;
end;

function TPJSysInfo.GetProgramFilesFolder: string;
  {Read access method for ProgramFilesFolder property.
    @return Property value.
  }
begin
  Result := TPJSystemFolders.ProgramFiles;
end;

function TPJSysInfo.GetSystemFolder: string;
  {Read access method for SystemFolder property.
    @return Property value.
  }
begin
  Result :=  TPJSystemFolders.System;
end;

function TPJSysInfo.GetTempFolder: string;
  {Read access method for TempFolder property.
    @return Property value.
  }
begin
  Result := TPJSystemFolders.Temp;
end;

function TPJSysInfo.GetUserName: string;
  {Read access method for UserName property.
    @return Property value.
  }
begin
  Result := TPJComputerInfo.UserName;
end;

function TPJSysInfo.GetWindowsFolder: string;
  {Read access method for WindowsFolder property.
    @return Property value.
  }
begin
  Result := TPJSystemFolders.Windows;
end;
{$ENDIF}


// -----------------------------------------------------------------------------
// Static class implementations
// -----------------------------------------------------------------------------

{ TPJOSInfo }

class function TPJOSInfo.BuildNumber: Integer;
  {Gets operating system's build number.
    @return Required build number.
  }
begin
  Result := Win32BuildNumber;
end;

class function TPJOSInfo.CheckSuite(const Suite: Integer): Boolean;
  {Checks if a given suite is installed on an NT system.
    @param Suites [in] One of the VER_SUITE_* flags.
    @return True if suite is installed. False if not installed or if not an NT
      operating system.
  }
begin
  Result := Win32SuiteMask and Suite <> 0;
end;

class function TPJOSInfo.Description: string;
  {Gets full description of operating system.
    @return Required description.
  }

  // ---------------------------------------------------------------------------
  procedure AppendToResult(const Str: string);
    {Adds a non-empty string to end of result, preceeded by space.
      @param Str [in] String to append.
    }
  begin
    if Str <> '' then
      Result := Result + ' ' + Str;
  end;
  // ---------------------------------------------------------------------------

begin
  // Start with product name
  Result := ProductName;
  case Platform of
    ospWinNT:
    begin
      // We have an NT OS
      // append any product type
      if Product = osWinNT then
      begin
        // For NT3/4 append version number after product
        AppendToResult(Format('%d.%d', [MajorVersion, MinorVersion]));
        AppendToResult(Edition);
        AppendToResult(ServicePack);  // does nothing if no service pack
        AppendToResult(Format('(Build %d)', [BuildNumber]));
      end
      else
      begin
        // Windows 2000 and later: don't include version number
        AppendToResult(Edition);
        AppendToResult(ServicePack);  // does nothing if no service pack
        AppendToResult(Format('(Build %d)', [BuildNumber]));
      end;
    end;
    ospWin9x:
      // We have a Win 95 line OS: append service pack
      AppendToResult(ServicePack);
  end;
end;

class function TPJOSInfo.Edition: string;
  {Gets the product edition for an NT operating system.
    @return Required edition or '' is OS is not NT.
  }
begin
  // This method is based on sample C++ code from MSDN
  Result := '';
  case Product of
    osWinVista, osWinSvr2008, osWin7, osWinSvr2008R2:
    begin
      // For v6.0 and later we ignore the suite mask and use the new
      // PRODUCT_ flags from the GetProductInfo() function to determine the
      // edition
      Result := EditionFromProductInfo;
      // append 64-bit if 64 bit system
      if pvtProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64 then
        Result := Result + ' (64-bit)';
      // can detect 32-bit if required by checking if
      // pvtProcessorArchitecture = PROCESSOR_ARCHITECTURE_INTEL then
    end;
    osWinSvr2003, osWinSvr2003R2:
    begin
      // We check different processor architectures and act accordingly
      // This code closely based on MS's sample code found at
      // http://msdn2.microsoft.com/en-us/library/ms724429
      if pvtProcessorArchitecture = PROCESSOR_ARCHITECTURE_IA64 then
      begin
        if CheckSuite(VER_SUITE_DATACENTER) then
          Result := 'Datacenter Edition for Itanium-based Systems'
        else if CheckSuite(VER_SUITE_ENTERPRISE) then
          Result := 'Enterprise Edition for Itanium-based Systems';
      end
      else if (pvtProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
      begin
        if CheckSuite(VER_SUITE_DATACENTER) then
          Result := 'Datacenter x64 Edition'
        else if CheckSuite(VER_SUITE_ENTERPRISE) then
          Result := 'Enterprise x64 Edition'
        else
          Result := 'Standard x64 Edition';
      end
      else
      begin
        if CheckSuite(VER_SUITE_COMPUTE_SERVER) then
          Result := 'Compute Cluster Edition'
        else if CheckSuite(VER_SUITE_DATACENTER) then
          Result := 'Datacenter Edition'
        else if CheckSuite(VER_SUITE_BLADE) then
          Result := 'Web Edition'
        else if CheckSuite(VER_SUITE_STORAGE_SERVER) then
          Result := 'Storage Server'
        // According to MSDN we can't rely on VER_SUITE_SMALLBUSINESS since it
        // is not removed when upgrading to standard or enterprises editions.
        // When installing Small Business edition both VER_SUITE_SMALLBUSINESS
        // and VER_SUITE_SMALLBUSINESS_RESTRICTED are set. When installed
        // standard edition VER_SUITE_SMALLBUSINESS_RESTRICTED gets unset while
        // VER_SUITE_SMALLBUSINESS remains. So, we first check for the
        // Enterprise edition and exclude Small Business if we find that.
        // Since there is no flag for Standard Edition we check for both
        // VER_SUITE_SMALLBUSINESS and VER_SUITE_SMALLBUSINESS_RESTRICTED and
        // assume Small Business if we find both, otherwise we assume Standard
        // edition.
        else if CheckSuite(VER_SUITE_ENTERPRISE) then
          Result := 'Enterprise Edition'
        else if CheckSuite(VER_SUITE_SMALLBUSINESS) and
          CheckSuite(VER_SUITE_SMALLBUSINESS_RESTRICTED) then
          Result := 'Small Business Edition'
        else
          Result := 'Standard Edition';
      end;
    end;
    osWinXP:
    begin
      if GetSystemMetrics(SM_STARTER) <> 0 then
        Result := 'Starter Edition'
      else if (Win32MajorVersion = 5) and (Win32MinorVersion = 2) and
        not IsServer and  // XP Pro 64 has version 5.2 not 5.1!
        (pvtProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
        Result := 'Professional x64 Edition'
      else if CheckSuite(VER_SUITE_PERSONAL) then
        Result := 'Home Edition'
      else
        Result := 'Professional';
    end;
    osWin2K:
    begin
      if IsServer then
      begin
        if CheckSuite(VER_SUITE_DATACENTER) then
          Result := 'Datacenter Server'
        else if CheckSuite(VER_SUITE_ENTERPRISE) then
          Result := 'Advanced Server'
        else
          Result := 'Server';
      end
      else
        Result := 'Professional';
    end;
    osWinNT:
    begin
      if Win32HaveExInfo then
      begin
        // This is NT SP6 or later: got info from OS
        if IsServer then
        begin
          if CheckSuite(VER_SUITE_ENTERPRISE) then
            Result := 'Enterprise Edition'
          else
            Result := 'Server';
        end
        else
          Result := 'Workstation'
      end
      else
        // NT before SP6: we read required info from registry
        Result := EditionFromReg;
    end;
  end;
end;

class function TPJOSInfo.EditionFromProductInfo: string;
  {Gets product edition from value return from GetProductInfo API.
    @return Required product edition or '' if product not recognised.
  }
var
  Idx: Integer; // loops through entries in cProductMap[]
begin
  Result := '';
  for Idx := Low(cProductMap) to High(cProductMap) do
  begin
    if cProductMap[Idx].Id = Win32ProductInfo then
    begin
      Result := cProductMap[Idx].Name;
      Exit;
    end;
  end;
end;

class function TPJOSInfo.EditionFromReg: string;
  {Gets product edition from registry. Needed to get edition for NT4 pre SP6.
    @return Required product edition.
  }
var
  EditionCode: string;  // OS product edition code stored in registry
begin
  EditionCode := ProductTypeFromReg;
  if CompareText(EditionCode, 'WINNT') = 0 then
    Result := 'WorkStation'
  else if CompareText(EditionCode, 'LANMANNT') = 0 then
    Result := 'Server'
  else if CompareText(EditionCode, 'SERVERNT') = 0 then
    Result := 'Advanced Server';
  Result := Result + Format(
    ' %d.%d', [Win32MajorVersion, Win32MinorVersion]
  );
end;

class function TPJOSInfo.HasPenExtensions: Boolean;
  {Checks if system has Pen Extensions installed.
    @return True if extensions installed, False otherwise.
  }
begin
  Result := GetSystemMetrics(SM_PENWINDOWS) <> 0;
end;

class function TPJOSInfo.IsMediaCenter: Boolean;
  {Checks if Windows Media Center is installed.
    @return True if Media Center installed, False otherwise.
  }
begin
  Result := GetSystemMetrics(SM_MEDIACENTER) <> 0;
end;

class function TPJOSInfo.IsNT4SP6a: Boolean;
  {Checks registry to see if NT4 Service Pack 6a is installed.
    @return True if service pack is installed, False otherwise.
  }
var
  Reg: TRegistry;
begin
  if (Product = osWinNT)
    and (Win32MajorVersion = 4)
    and (CompareText(Win32CSDVersion, 'Service Pack 6') = 0) then
  begin
    // System is reporting NT4 SP6
    // we have SP 6a if particular registry key exists
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      Result := Reg.KeyExists(
        'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Hotfix\Q246009'
      );
    finally
      Reg.Free;
    end;
  end
  else
    // System not reporting NT4 SP6, so not SP6a!
    Result := False;
end;

class function TPJOSInfo.IsServer: Boolean;
  {Checks if operating system is a server.
    @return True if server OS, False otherise.
  }
begin
  if Win32HaveExInfo then
    // Check product type from extended OS info
    Result := Win32ProductType <> VER_NT_WORKSTATION
  else
    // Check product type stored in registry
    Result := CompareText(ProductTypeFromReg, 'WINNT') <> 0;;
end;

class function TPJOSInfo.IsTabletPC: Boolean;
  {Checks if this is Tablet PC operating system.
    @return True if Tablet PC, False otherwise.
  }
begin
  Result := GetSystemMetrics(SM_TABLETPC) <> 0;
end;

class function TPJOSInfo.IsWin32s: Boolean;
  {Checks if running on Win32s. This is unlikely to ever return true since
  Delphi is not believed to run on Win32s itself.
    @return True if Win32s, False otherwise.
  }
begin
  Result := Platform = ospWin32s;
end;

class function TPJOSInfo.IsWin9x: Boolean;
  {Checks if a OS if one the Windows 9x line, i.e. Windows 95, Windows 98 or
  Windows Me.
    @return True if Win9x OS, False otherwise.
  }
begin
  Result := Platform = ospWin9x;
end;

class function TPJOSInfo.IsWinNT: Boolean;
  {Checks if OS is one of NT line (NT, 2000, XP, 2003 Server, Vista, 2008
  Server).
    @return True if WinNT, False otherwise.
  }
begin
  Result := Platform = ospWinNT;
end;

class function TPJOSInfo.IsWow64: Boolean;
  {Checks if application is running under WOW64 on a 64 bit operating system.
    @return True if running on WOW64, False otherwise.
  }
type
  TIsWow64Process = function( // Type of IsWow64Process API fn
    Handle: THandle;
    var Res: BOOL
  ): BOOL; stdcall;
var
  IsWow64Result: BOOL;              // result from IsWow64Process
  IsWow64Process: TIsWow64Process;  // IsWow64Process fn reference
begin
  // Try to load required function from kernel32
  IsWow64Process := LoadKernelFunc('IsWow64Process');
  if Assigned(IsWow64Process) then
  begin
    // Function is implemented: call it
    if not IsWow64Process(GetCurrentProcess, IsWow64Result) then
      raise Exception.Create(sBadProcHandle);
    // Return result of function
    Result := IsWow64Result;
  end
  else
    // Function not implemented: can't be running on Wow64
    Result := False;
end;

class function TPJOSInfo.MajorVersion: Integer;
  {Gets the operating system's major version number.
    @return Required version number.
  }
begin
  Result := Win32MajorVersion;
end;

class function TPJOSInfo.MinorVersion: Integer;
  {Gets the operating system's minor version number.
    @return Required version number.
  }
begin
  Result := Win32MinorVersion;
end;

class function TPJOSInfo.Platform: TPJOSPlatform;
  {Identifies the OS platform.
    @return Identifier representing the platform.
    @except EPJSysInfo raised if can't determine platform.
  }
begin
  case Win32Platform of
    VER_PLATFORM_WIN32_NT: Result := ospWinNT;
    VER_PLATFORM_WIN32_WINDOWS: Result := ospWin9x;
    VER_PLATFORM_WIN32s: Result := ospWin32s;
    else raise EPJSysInfo.Create(sUnknownPlatform);
  end;
end;

class function TPJOSInfo.Product: TPJOSProduct;
  {Identifies an OS product.
    @return Identifier representing the product.
  }
begin
  Result := osUnknown;
  case Platform of
    ospWin9x:
    begin
      // Win 9x platform: only major version is 4
      Result := osUnknownWin9x;
      case Win32MajorVersion of
        4:
        begin
          case Win32MinorVersion of
            0: Result := osWin95;
            10: Result := osWin98;
            90: Result := osWinMe;
          end;
        end;
      end;
    end;
    ospWinNT:
    begin
      // NT platform OS
      Result := osUnknownWinNT;
      case Win32MajorVersion of
        3, 4:
        begin
          // NT 3 or 4
          case Win32MinorVersion of
            0: Result := osWinNT;
          end;
        end;
        5:
        begin
          // Windows 2000 or XP
          case Win32MinorVersion of
            0:
              Result := osWin2K;
            1:
              Result := osWinXP;
            2:
            begin
              if GetSystemMetrics(SM_SERVERR2) <> 0 then
                Result := osWinSvr2003R2
              else
              begin
                if not IsServer and
                  (pvtProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
                  Result := osWinXP // XP Pro X64
                else
                  Result := osWinSvr2003
              end
            end;
          end;
        end;
        6:
        begin
          case Win32MinorVersion of
            0:
              if not IsServer then
                Result := osWinVista
              else
                Result := osWinSvr2008;
            1:
              if not IsServer then
                Result := osWin7
              else
                Result := osWinSvr2008R2;
            else
              // Higher minor version: must be an unknown later OS
              Result := osWinLater
          end;
        end;
        else
          // Higher major version: must be an unknown later OS
          Result := osWinLater;
      end;
    end;
    ospWin32s:
      // Windows 32s: probably won't ever get this
      Result := osUnknownWin32s;
  end;
end;

class function TPJOSInfo.ProductID: string;
  {Determines the Windows product ID.
    @return Required product id string.
  }
const
  // Registry keys for Win 9x/NT
  cRegKey: array[Boolean] of string = (
    'Software\Microsoft\Windows\CurrentVersion',
    'Software\Microsoft\Windows NT\CurrentVersion'
  );
begin
  Result := GetRegistryString(
    HKEY_LOCAL_MACHINE, cRegKey[IsWinNT], 'ProductID'
  );
end;

class function TPJOSInfo.ProductName: string;
  {Gets the name of the OS product.
    @return Name of product or '' if product not known. If product is later than
      most recent known version, "Post-Vista Windows" is returned.
  }
begin
  case Product of
    osUnknownWinNT, osUnknownWin9x, osUnknownWin32s: Result := '';
    osWinNT: Result := 'Windows NT';
    osWin2K: Result := 'Windows 2000';
    osWinXP: Result := 'Windows XP';
    osWinVista: Result := 'Windows Vista';
    osWinSvr2008: Result := 'Windows Server 2008';
    osWin95: Result := 'Windows 95';
    osWin98: Result := 'Windows 98';
    osWinMe: Result := 'Windows Me';
    osWinSvr2003: Result := 'Windows Server 2003';
    osWinSvr2003R2: Result := 'Windows Server 2003 R2';
    osWinLater: Result := Format(
      'Windows Version %d.%d', [Win32MajorVersion, Win32MinorVersion]
    );
    osWin7: Result := 'Windows 7';
    osWinSvr2008R2: Result := 'Windows Server 2008 R2';
    else
      raise EPJSysInfo.Create(sUnknownProduct);
  end;
end;

class function TPJOSInfo.ProductTypeFromReg: string;
  {Gets code describing product type from registry. Used to get product type for
  NT4 SP5 and earlier.
    @return Required product type or '' if registry key or value can't be found.
  }
begin
  Result := GetRegistryString(
    HKEY_LOCAL_MACHINE,
    'SYSTEM\CurrentControlSet\Control\ProductOptions',
    'ProductType'
  );
end;

class function TPJOSInfo.ServicePack: string;
  {Gets name of any service pack installed.
    @return Name of service pack or '' if no service pack installed.
  }
begin
  // Assume to service pack
  Result := '';
  case Platform of
    ospWin9x:
      // On the Windows 9x platform we decode the service pack info
      if Win32CSDVersion <> '' then
      begin
        case Product of
          osWin95:
            {$IFDEF UNICODE}
            if CharInSet(Win32CSDVersion[1], ['B', 'b', 'C', 'c']) then
            {$ELSE}
            if Win32CSDVersion[1] in ['B', 'b', 'C', 'c'] then
            {$ENDIF}
              Result := 'OSR2';
          osWin98:
            {$IFDEF UNICODE}
            if CharInSet(Win32CSDVersion[1], ['A', 'a']) then
            {$ELSE}
            if Win32CSDVersion[1] in ['A', 'a'] then
            {$ENDIF}
              Result := 'SE';
        end;
      end;
    ospWinNT:
      // On Windows NT we return service pack string, unless NT4 SP6 when we
      // need to check whether actually SP6 or SP6a
      if IsNT4SP6a then
        Result := 'Service Pack 6a' // do not localize
      else
        Result := Win32CSDVersion;
  end;
end;

class function TPJOSInfo.ServicePackMajor: Integer;
  {Gets major version number of any installed NT service pack.
    @return Required version number or 0 if no service pack installed or running
      on Win9x.
  }
begin
  Result := Win32ServicePackMajor;
end;

class function TPJOSInfo.ServicePackMinor: Integer;
  {Gets minor version number of any installed NT service pack.
    @return Required version number. Invalid if ServicePackMajor returns 0.
  }
begin
  Result := Win32ServicePackMinor;
end;

{ TPJComputerInfo }

class function TPJComputerInfo.BootMode: TPJBootMode;
  {Determines operating system mode used when computer was last booted.
    @return Required boot mode.
  }
begin
  case GetSystemMetrics(SM_CLEANBOOT) of
    0: Result := bmNormal;
    1: Result := bmSafeMode;
    2: Result := bmSafeModeNetwork;
    else Result := bmUnknown;
  end;
end;

class function TPJComputerInfo.ComputerName: string;
  {Gets name of computer.
    @return Computer name.
  }
var
  PComputerName: array[0..MAX_PATH] of Char;// buffer for name returned from API
  Size: DWORD;                              // size of name buffer
begin
  Size := MAX_PATH;
  if Windows.GetComputerName(PComputerName, Size) then
    Result := PComputerName
  else
    Result := '';
end;

class function TPJComputerInfo.Is64Bit: Boolean;
  {Checks if running on 64 bit processor.
    @param True if running on 64 bit processor, False if 32 bit.
  }
begin
  Result := Processor in [paX64, paIA64];
end;

class function TPJComputerInfo.IsNetworkPresent: Boolean;
  {Checks if a network is present.
    @return True if network present, False otherwise.
  }
begin
  Result := GetSystemMetrics(SM_NETWORK) and 1 = 1;
end;

class function TPJComputerInfo.MACAddress: string;
  {Gets MAC address of first ethernet adapter on computer.
    @return Required MAC address or '' if no ethernet adapter found.
  }
{$IFDEF WARNDIRS}{$WARN UNSAFE_CODE OFF}{$ENDIF}
{$IFDEF WARNDIRS}{$WARN UNSAFE_TYPE OFF}{$ENDIF}
type
  // Based on code at MSDN knowledge base Q118623 article at
  // http://support.microsoft.com/kb/q118623/}
  // According to MSDN this method should fail on Windows 6.0 (Vista) and later.
  // It has been known to fail on Vista, but works on Vista Home Premium SP1!
  // It would seem that the call will succeed if there's an active network with
  // Netbios over TCP enabled.
  // Again according to Microsoft, the function is unreliable on Windows 95, 98
  // and Me.

  // This type is defined in MSDN sample code, but tests have found this is
  // not needed (on XP Pro) and Adapter can be of type TAdapterStatus. This
  // method uses the type in case other OSs require it
  TAStat = packed record
    Adapt: TAdapterStatus;
    NameBuff: array [0..29] of TNameBuffer;
  end;
var
  Adapter: TAStat;          // info about a network adapter
  AdapterList: TLanaEnum;   // numbers for current LAN adapters
  Ncb: TNCB;                // network control block descriptor
  I: Integer;               // loops thru all adapters in list

  // ---------------------------------------------------------------------------
  function NetBiosSucceeded(const RetCode: AnsiChar): Boolean;
    {Checks if call to NetBios API function succeeded.
      @param RetCode [in] Return code of NetBios API call.
      @return True for successful return code, false otherwise.
    }
  begin
    Result := UCHAR(RetCode) = NRC_GOODRET;
  end;
  // ---------------------------------------------------------------------------

begin
  // Assume not adapter
  Result := '';
  // Get list of adapters
  FillChar(Ncb, SizeOf(Ncb), 0);
  Ncb.ncb_command := AnsiChar(NCBENUM);
  Ncb.ncb_buffer := PAnsiChar(@AdapterList);
  Ncb.ncb_length := SizeOf(AdapterList);
  if not NetBiosSucceeded(Netbios(@Ncb)) then
    Exit;
  // Get status of each adapter, exiting when first valid one reached
  // MSDN cautions us not to assume lana[0] is valid
  for I := 0 to Pred(Integer(AdapterList.length)) do
  begin
    // reset the adapter
    FillChar(Ncb, SizeOf(Ncb), 0);
    Ncb.ncb_command := AnsiChar(NCBRESET);
    Ncb.ncb_lana_num := AdapterList.lana[I];
    if not NetBiosSucceeded(Netbios(@Ncb)) then
      Exit;
    // get status of adapter
    FillChar(Ncb, SizeOf(Ncb), 0);
    Ncb.ncb_command := AnsiChar(NCBASTAT);
    Ncb.ncb_lana_num := AdapterList.lana[i];
    Ncb.ncb_callname := '*               ';
    Ncb.ncb_buffer := PAnsiChar(@Adapter);
    Ncb.ncb_length := SizeOf(Adapter);
    if NetBiosSucceeded(Netbios(@Ncb)) then
    begin
      // we have a MAC address: return it
      with Adapter.Adapt do
        Result := Format(
          '%.2x-%.2x-%.2x-%.2x-%.2x-%.2x',
          [
            Ord(adapter_address[0]),
            Ord(adapter_address[1]),
            Ord(adapter_address[2]),
            Ord(adapter_address[3]),
            Ord(adapter_address[4]),
            Ord(adapter_address[5])
          ]
        );
      Exit;
    end;
  end;
{$IFDEF WARNDIRS}{$WARN UNSAFE_TYPE ON}{$ENDIF}
{$IFDEF WARNDIRS}{$WARN UNSAFE_CODE ON}{$ENDIF}
end;

class function TPJComputerInfo.Processor: TPJProcessorArchitecture;
  {Gets processor architecture.
    @return Identifier describing  processor architecture.
  }
begin
  case pvtProcessorArchitecture of
    PROCESSOR_ARCHITECTURE_INTEL: Result := paX86;
    PROCESSOR_ARCHITECTURE_AMD64: Result := paX64;
    PROCESSOR_ARCHITECTURE_IA64:  Result := paIA64;
    else Result := paUnknown;
  end;
end;

class function TPJComputerInfo.ProcessorCount: Cardinal;
  {Gets number of processors in computer.
    @return Number of processors.
  }
var
  SI: TSystemInfo;  // contains system information
begin
  GetSystemInfo(SI);
  Result := SI.dwNumberOfProcessors;
end;

class function TPJComputerInfo.UserName: string;
  {Gets name of currently logged on user.
    @return User name.
  }
var
  PUserName: array[0..MAX_PATH] of Char;  // buffer for name returned from API
  Size: DWORD;                            // size of name buffer
begin
  Size := MAX_PATH;
  if Windows.GetUserName(PUserName, Size) then
    Result := PUserName
  else
    Result := '';
end;

{ TPJSystemFolders }

class function TPJSystemFolders.CommonFiles: string;
  {Gets fully qualified name of Common Files folder.
    @return Required folder name.
  }
begin
  Result :=  ExcludeTrailingPathDelimiter(
    GetCurrentVersionRegStr('CommonFilesDir')
  );
end;

class function TPJSystemFolders.ProgramFiles: string;
  {Gets fully qualified name of Program Files folder
    @return Required folder name.
  }
begin
  Result :=  ExcludeTrailingPathDelimiter(
    GetCurrentVersionRegStr('ProgramFilesDir')
  );
end;

class function TPJSystemFolders.System: string;
  {Gets fully qualified name of Windows system folder
    @return Required folder name.
  }
var
  PFolder: array[0..MAX_PATH] of Char;  // buffer to hold name returned from API
begin
  if GetSystemDirectory(PFolder, MAX_PATH) <> 0 then
    Result := ExcludeTrailingPathDelimiter(PFolder)
  else
    Result := '';
end;

class function TPJSystemFolders.SystemWow64: string;
  {Gets the fully qualified path name of the folder folder used to store shared
  32 bit code on 64 bit Windows.
    @return Required folder name. Always returns '' on 32 bit Windows.
  }
type
  {$IFDEF WARNDIRS}{$WARN UNSAFE_TYPE OFF}{$ENDIF}
  // type of GetSystemWow64DirectoryFn API function
  TGetSystemWow64Directory = function(lpBuffer: PChar; uSize: UINT): UINT;
    stdcall;
  {$IFDEF WARNDIRS}{$WARN UNSAFE_TYPE ON}{$ENDIF}
var
  PFolder: array[0..MAX_PATH] of Char;  // buffer to hold name returned from API
  GetSystemWow64Directory: TGetSystemWow64Directory;  // API function
begin
  Result := '';
  {$IFDEF UNICODE}
  GetSystemWow64Directory := LoadKernelFunc('GetSystemWow64DirectoryW');
  {$ELSE}
  GetSystemWow64Directory := LoadKernelFunc('GetSystemWow64DirectoryA');
  {$ENDIF}
  if not Assigned(GetSystemWow64Directory) then
    Exit;
  if GetSystemWow64Directory(PFolder, MAX_PATH) <> 0 then
    Result := ExcludeTrailingPathDelimiter(PFolder);
end;

class function TPJSystemFolders.Temp: string;
  {Gets fully qualified name of system's temporary folder.
    @return Required folder name.
  }
var
  PathBuf: array[0..MAX_PATH] of Char;  // buffer to hold name returned from API
begin
  if GetTempPath(MAX_PATH, PathBuf) <> 0 then
    Result := ExcludeTrailingPathDelimiter(PathBuf)
  else
    Result := '';
end;

class function TPJSystemFolders.Windows: string;
  {Gets fully qualified name of Windows folder.
    @return Required folder name.
  }
var
  PFolder: array[0..MAX_PATH] of Char;  // buffer to hold name returned from API
begin
  if GetWindowsDirectory(PFolder, MAX_PATH) <> 0 then
    Result := ExcludeTrailingPathDelimiter(PFolder)
  else
    Result := '';
end;

initialization

// Initialize global variables from extended OS and product info
InitPlatformIdEx;

end.

