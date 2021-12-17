{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Provides locale information in an OS neutral way and declares locale
 * identifiers.
}


unit ULocales;


interface


uses
  // Delphi
  Windows;


const

  //
  // LCTYPE definitions
  // The first block are also declared in Windows unit and are redeclared here
  // for convenience.
  // See MSDN and SDK help for meanings of constants
  //

  // All versions of Windows
  LOCALE_ILANGUAGE              = $00000001;
  LOCALE_SLANGUAGE              = $00000002;
  LOCALE_SENGLANGUAGE           = $00001001;
  LOCALE_SABBREVLANGNAME        = $00000003;
  LOCALE_SNATIVELANGNAME        = $00000004;
  LOCALE_ICOUNTRY               = $00000005;
  LOCALE_SCOUNTRY               = $00000006;
  LOCALE_SENGCOUNTRY            = $00001002;
  LOCALE_SABBREVCTRYNAME        = $00000007;
  LOCALE_SNATIVECTRYNAME        = $00000008;
  LOCALE_IGEOID                 = $0000005B;
  LOCALE_IDEFAULTLANGUAGE       = $00000009;
  LOCALE_IDEFAULTCOUNTRY        = $0000000A;
  LOCALE_IDEFAULTCODEPAGE       = $0000000B;
  LOCALE_IDEFAULTANSICODEPAGE   = $00001004;
  LOCALE_IDEFAULTMACCODEPAGE    = $00001011;
  LOCALE_SLIST                  = $0000000C;
  LOCALE_IMEASURE               = $0000000D;
  LOCALE_SDECIMAL               = $0000000E;
  LOCALE_STHOUSAND              = $0000000F;
  LOCALE_SGROUPING              = $00000010;
  LOCALE_IDIGITS                = $00000011;
  LOCALE_ILZERO                 = $00000012;
  LOCALE_INEGNUMBER             = $00001010;
  LOCALE_SNATIVEDIGITS          = $00000013;
  LOCALE_SCURRENCY              = $00000014;
  LOCALE_SINTLSYMBOL            = $00000015;
  LOCALE_SMONDECIMALSEP         = $00000016;
  LOCALE_SMONTHOUSANDSEP        = $00000017;
  LOCALE_SMONGROUPING           = $00000018;
  LOCALE_ICURRDIGITS            = $00000019;
  LOCALE_IINTLCURRDIGITS        = $0000001A;
  LOCALE_ICURRENCY              = $0000001B;
  LOCALE_INEGCURR               = $0000001C;
  LOCALE_SDATE                  = $0000001D;
  LOCALE_STIME                  = $0000001E;
  LOCALE_SSHORTDATE             = $0000001F;
  LOCALE_SLONGDATE              = $00000020;
  LOCALE_STIMEFORMAT            = $00001003;
  LOCALE_IDATE                  = $00000021;
  LOCALE_ILDATE                 = $00000022;
  LOCALE_ITIME                  = $00000023;
  LOCALE_ITIMEMARKPOSN          = $00001005;
  LOCALE_ICENTURY               = $00000024;
  LOCALE_ITLZERO                = $00000025;
  LOCALE_IDAYLZERO              = $00000026;
  LOCALE_IMONLZERO              = $00000027;
  LOCALE_S1159                  = $00000028;
  LOCALE_S2359                  = $00000029;
  LOCALE_ICALENDARTYPE          = $00001009;
  LOCALE_IOPTIONALCALENDAR      = $0000100B;
  LOCALE_IFIRSTDAYOFWEEK        = $0000100C;
  LOCALE_IFIRSTWEEKOFYEAR       = $0000100D;
  LOCALE_SDAYNAME1              = $0000002A;
  LOCALE_SDAYNAME2              = $0000002B;
  LOCALE_SDAYNAME3              = $0000002C;
  LOCALE_SDAYNAME4              = $0000002D;
  LOCALE_SDAYNAME5              = $0000002E;
  LOCALE_SDAYNAME6              = $0000002F;
  LOCALE_SDAYNAME7              = $00000030;
  LOCALE_SABBREVDAYNAME1        = $00000031;
  LOCALE_SABBREVDAYNAME2        = $00000032;
  LOCALE_SABBREVDAYNAME3        = $00000033;
  LOCALE_SABBREVDAYNAME4        = $00000034;
  LOCALE_SABBREVDAYNAME5        = $00000035;
  LOCALE_SABBREVDAYNAME6        = $00000036;
  LOCALE_SABBREVDAYNAME7        = $00000037;
  LOCALE_SMONTHNAME1            = $00000038;
  LOCALE_SMONTHNAME2            = $00000039;
  LOCALE_SMONTHNAME3            = $0000003A;
  LOCALE_SMONTHNAME4            = $0000003B;
  LOCALE_SMONTHNAME5            = $0000003C;
  LOCALE_SMONTHNAME6            = $0000003D;
  LOCALE_SMONTHNAME7            = $0000003E;
  LOCALE_SMONTHNAME8            = $0000003F;
  LOCALE_SMONTHNAME9            = $00000040;
  LOCALE_SMONTHNAME10           = $00000041;
  LOCALE_SMONTHNAME11           = $00000042;
  LOCALE_SMONTHNAME12           = $00000043;
  LOCALE_SMONTHNAME13           = $0000100E;
  LOCALE_SABBREVMONTHNAME1      = $00000044;
  LOCALE_SABBREVMONTHNAME2      = $00000045;
  LOCALE_SABBREVMONTHNAME3      = $00000046;
  LOCALE_SABBREVMONTHNAME4      = $00000047;
  LOCALE_SABBREVMONTHNAME5      = $00000048;
  LOCALE_SABBREVMONTHNAME6      = $00000049;
  LOCALE_SABBREVMONTHNAME7      = $0000004A;
  LOCALE_SABBREVMONTHNAME8      = $0000004B;
  LOCALE_SABBREVMONTHNAME9      = $0000004C;
  LOCALE_SABBREVMONTHNAME10     = $0000004D;
  LOCALE_SABBREVMONTHNAME11     = $0000004E;
  LOCALE_SABBREVMONTHNAME12     = $0000004F;
  LOCALE_SABBREVMONTHNAME13     = $0000100F;
  LOCALE_SPOSITIVESIGN          = $00000050;
  LOCALE_SNEGATIVESIGN          = $00000051;
  LOCALE_IPOSSIGNPOSN           = $00000052;
  LOCALE_INEGSIGNPOSN           = $00000053;
  LOCALE_IPOSSYMPRECEDES        = $00000054;
  LOCALE_IPOSSEPBYSPACE         = $00000055;
  LOCALE_INEGSYMPRECEDES        = $00000056;
  LOCALE_INEGSEPBYSPACE         = $00000057;

  // Windows 9x and NT 4 and later
  LOCALE_FONTSIGNATURE          = $00000058;
  LOCALE_SISO639LANGNAME        = $00000059;
  LOCALE_SISO3166CTRYNAME       = $0000005A;

  // Windows XP and later
  LOCALE_IDEFAULTEBCDICCODEPAGE = $00001012;
  LOCALE_IPAPERSIZE             = $0000100A;
  LOCALE_SENGCURRNAME           = $00001007;
  LOCALE_SNATIVECURRNAME        = $00001008;
  LOCALE_SYEARMONTH             = $00001006;
  LOCALE_SSORTNAME              = $00001013;
  LOCALE_IDIGITSUBSTITUTION     = $00001014;

  // Windows Vista and later
  LOCALE_SNAME                  = $0000005C;
  LOCALE_SDURATION              = $0000005D;
  LOCALE_SKEYBOARDSTOINSTALL    = $0000005E;
  LOCALE_SSHORTESTDAYNAME1      = $00000060;
  LOCALE_SSHORTESTDAYNAME2      = $00000061;
  LOCALE_SSHORTESTDAYNAME3      = $00000062;
  LOCALE_SSHORTESTDAYNAME4      = $00000063;
  LOCALE_SSHORTESTDAYNAME5      = $00000064;
  LOCALE_SSHORTESTDAYNAME6      = $00000065;
  LOCALE_SSHORTESTDAYNAME7      = $00000066;
  LOCALE_SISO639LANGNAME2       = $00000067;
  LOCALE_SISO3166CTRYNAME2      = $00000068;
  LOCALE_SNAN                   = $00000069;
  LOCALE_SPOSINFINITY           = $0000006A;
  LOCALE_SNEGINFINITY           = $0000006B;
  LOCALE_SSCRIPTS               = $0000006C;
  LOCALE_SPARENT                = $0000006D;
  LOCALE_SCONSOLEFALLBACKNAME   = $0000006E;
  LOCALE_SLANGDISPLAYNAME       = $0000006F;

  // Windows 98, Windows NT 4.0 and later
  LOCALE_RETURN_NUMBER          = $20000000;

  //
  //  Enumeration flags. Windows Vista and later.
  //

  LOCALE_ALL              = 0;
  LOCALE_WINDOWS          = $00000001;
  LOCALE_SUPPLEMENTAL     = $00000002;
  LOCALE_ALTERNATE_SORTS  = $00000004;
  LOCALE_REPLACEMENT      = $00000008;

  //
  // String based locale names: Windows Vista and later
  //

  LOCALE_NAME_USER_DEFAULT: PWideChar   = nil;
  LOCALE_NAME_INVARIANT: PWideChar      = '';
  LOCALE_NAME_SYSTEM_DEFAULT: PWideChar = '!x-sys-default-locale';


///  <summary>Gets information about a LocaleID in an OS-safe way.</summary>
///  <param name="LocaleID">LCID [in] Identifies locale about which information
///  is required.</param>
///  <param name="InfoType">LCTYPE [in] Type of information required (a LOCALE_*
///  identifier).</param>
///  <returns>string. Requested information.</returns>
///  <exception>EOSError raised if locale information can't be found.
///  </exception>
function GetLocaleInfo(const LocaleID: LCID; const InfoType: LCTYPE): string;

///  <summary>Returns the default ANSI code page for the system.</summary>
function DefaultAnsiCodePage: Cardinal;

///  <summary>Returns the default language identifier for the current user.
///  </summary>
function DefaultLanguageID: Cardinal;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  USystemInfo;


function DefaultAnsiCodePage: Cardinal;
begin
  Result := Windows.GetACP;
end;

function DefaultLanguageID: Cardinal;
var
  LangId: string; // language ID as string
begin
  LangId := GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_ILANGUAGE);
  Result := StrToInt('$' + LangId);
end;

type
  ///  <summary>Type of GetLocaleInfoEx function in Kernel32 that retrieves
  ///  information about a locale specified by name.</summary>
  ///  <remarks>GetLocaleInfoEx should be used in preference to GetLocaleInfo on
  ///  Windows Vista and later since GetLocaleInfo can be buggy on these OSs.
  ///  </remarks>
  ///  <param name="lpLocaleName">PWideChar [in] Name of locale based on
  ///  language tagging conventions of RFC 4646.</param>
  ///  <param name="LCType">LCTYPE [in] Type of locale information to retrieve.
  ///  </param>
  ///  <param name="lpLCData">PWideChar [in] Pointer to buffer to receive
  ///  information. May be nil if cchData is 0.</param>
  ///  <param name="cchData">Integer [in] Size, in WideChars, of the data buffer
  ///  pointed to by lpLCData. Can be 0 to get required buffer size.</param>
  ///  <returns>Integer. Number of WideChars stored in lpLCData if cchData is
  ///  non-zero; size of data buffer required if cchData is zero; 0 if function
  ///  fails.</returns>
  TGetLocaleInfoEx = function(lpLocaleName: PWideChar; LCType: LCTYPE;
    lpLCData: PWideChar; cchData: Integer): Integer; stdcall;


var
  ///  Pointer to GetLocaleInfoEx function in Kernel32.
  ///  Set to nil if the function is not supported.
  APIGetLocaleInfoEx: TGetLocaleInfoEx = nil;


///  <summary>Queries the OS for information for a specified locale.</summary>
///  <remarks>
///  <para>This Unicode version works on NT systems.</para>
///  <para>Based on code from SysUtils which, according to comments there, works
///  correctly for Asian WinNT.</para>
///  </remarks>
///  <param name="LocaleID">LCID [in] Identifies locale about which information
///  is required.</param>
///  <param name="InfoType">LCTYPE [in] Type of information required.</param>
///  <returns>string. Requested information.</returns>
///  <exception>EOSError raised if locale information can't be found.
///  </exception>
function GetLocaleInfoW(const LocaleID: LCID; const InfoType: LCTYPE): string;
var
  Buffer: array[0..1023] of WideChar; // buffer to store required information
begin
  Buffer[0] := #0;
  if Windows.GetLocaleInfoW(
    LocaleID, InfoType, Buffer, SizeOf(Buffer) div SizeOf(WideChar)
  ) = 0 then
    RaiseLastOSError;
  Result := Buffer;
end;

///  <summary>Queries the OS for information for a specified locale.</summary>
///  <remarks>According to MSDN this method is preferred on Vista or later since
///  the GetLocaleInfo() API function can provide erroneous information on
///  Vista.</remarks>
///  <param name="LocaleID">LCID [in] Identifies locale about which information
///  is required.</param>
///  <param name="InfoType">LCTYPE [in] Type of information required.</param>
///  <returns>string. Requested information.</returns>
///  <exception>EOSError raised if locale information can't be found.
///  </exception>
function GetLocaleInfoEx(const LocaleID: LCID; const InfoType: LCTYPE): string;
var
  Buffer: array[0..1023] of WideChar; // buffer to store required information
  LocaleName: string;                 // name of locale
begin
  Assert(Assigned(APIGetLocaleInfoEx),
    'GetLocaleInfoEx: APIGetLocaleInfoEx is nil');
  LocaleName := GetLocaleInfoW(LocaleID, LOCALE_SNAME);
  Buffer[0] := #0;
  if APIGetLocaleInfoEx(
    PWideChar(WideString(LocaleName)),
    InfoType,
    Buffer,
    SizeOf(Buffer) div SizeOf(WideChar)
  ) = 0 then
    RaiseLastOSError;
  Result := Buffer; // silently converts to string
end;

function GetLocaleInfo(const LocaleID: LCID; const InfoType: LCTYPE): string;
begin
  if Assigned(APIGetLocaleInfoEx) then
    Result := GetLocaleInfoEx(LocaleID, InfoType)
  else
    Result := GetLocaleInfoW(LocaleID, InfoType)
end;


initialization

// Try to load GetLocaleInfoEx API function (Vista and later)
@APIGetLocaleInfoEx := GetProcAddress(
  GetModuleHandle('kernel32.dll'), 'GetLocaleInfoEx'
);


end.

