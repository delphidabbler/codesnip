{
 * ULocales.pas
 *
 * Provides locale information in an OS neutral way and declares locale
 * identifiers.
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
 * The Original Code is ULocales.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
  //

  // All versions of Windows
  LOCALE_ILANGUAGE              = $00000001;
    {language id}
  LOCALE_SLANGUAGE              = $00000002;
    {localized name of language}
  LOCALE_SENGLANGUAGE           = $00001001;
    {English name of language}
  LOCALE_SABBREVLANGNAME        = $00000003;
    {abbreviated language name}
  LOCALE_SNATIVELANGNAME        = $00000004;
    {native name of language}
  LOCALE_ICOUNTRY               = $00000005;
    {country code}
  LOCALE_SCOUNTRY               = $00000006;
    {localized name of country}
  LOCALE_SENGCOUNTRY            = $00001002;
    {English name of country}
  LOCALE_SABBREVCTRYNAME        = $00000007;
    {abbreviated country name}
  LOCALE_SNATIVECTRYNAME        = $00000008;
    {native name of country}
  LOCALE_IGEOID                 = $0000005B;
    {geographical location id}
  LOCALE_IDEFAULTLANGUAGE       = $00000009;
    {default language id}
  LOCALE_IDEFAULTCOUNTRY        = $0000000A;
    {default country code}
  LOCALE_IDEFAULTCODEPAGE       = $0000000B;
    {default oem code page}
  LOCALE_IDEFAULTANSICODEPAGE   = $00001004;
    {default ansi code page}
  LOCALE_IDEFAULTMACCODEPAGE    = $00001011;
    {default mac code page}
  LOCALE_SLIST                  = $0000000C;
    {list item separator}
  LOCALE_IMEASURE               = $0000000D;
    {0 = metric, 1 = US}
  LOCALE_SDECIMAL               = $0000000E;
    {decimal separator}
  LOCALE_STHOUSAND              = $0000000F;
    {thousand separator}
  LOCALE_SGROUPING              = $00000010;
    {digit grouping}
  LOCALE_IDIGITS                = $00000011;
    {number of fractional digits}
  LOCALE_ILZERO                 = $00000012;
    {leading zeros for decimal}
  LOCALE_INEGNUMBER             = $00001010;
    {negative number mode}
  LOCALE_SNATIVEDIGITS          = $00000013;
    {native digits for 0-9}
  LOCALE_SCURRENCY              = $00000014;
    {local monetary symbol}
  LOCALE_SINTLSYMBOL            = $00000015;
    {intl monetary symbol}
  LOCALE_SMONDECIMALSEP         = $00000016;
    {monetary decimal separator}
  LOCALE_SMONTHOUSANDSEP        = $00000017;
    {monetary thousand separator}
  LOCALE_SMONGROUPING           = $00000018;
    {monetary grouping}
  LOCALE_ICURRDIGITS            = $00000019;
    {local monetary digits}
  LOCALE_IINTLCURRDIGITS        = $0000001A;
    {intl monetary digits}
  LOCALE_ICURRENCY              = $0000001B;
    {positive currency mode}
  LOCALE_INEGCURR               = $0000001C;
    {negative currency mode}
  LOCALE_SDATE                  = $0000001D;
    {date separator (derived from LOCALE_SSHORTDATE, use that instead)}
  LOCALE_STIME                  = $0000001E;
    {time separator (derived from LOCALE_STIMEFORMAT, use that instead)}
  LOCALE_SSHORTDATE             = $0000001F;
    {short date format string}
  LOCALE_SLONGDATE              = $00000020;
    {long date format string}
  LOCALE_STIMEFORMAT            = $00001003;
    {time format string}
  LOCALE_IDATE                  = $00000021;
    {short date format ordering (derived from LOCALE_SSHORTDATE, use that
    instead)}
  LOCALE_ILDATE                 = $00000022;
    {long date format ordering (derived from LOCALE_SLONGDATE, use that
    instead)}
  LOCALE_ITIME                  = $00000023;
    {time format specifier (derived from LOCALE_STIMEFORMAT, use that
    instead)}
  LOCALE_ITIMEMARKPOSN          = $00001005;
    {time marker position (derived from LOCALE_STIMEFORMAT, use that
    instead)}
  LOCALE_ICENTURY               = $00000024;
    {century format specifier (short date, LOCALE_SSHORTDATE is preferred)}
  LOCALE_ITLZERO                = $00000025;
    {leading zeros in time field (derived from LOCALE_STIMEFORMAT, use that
    instead)}
  LOCALE_IDAYLZERO              = $00000026;
    {leading zeros in day field (short date, LOCALE_SSHORTDATE is preferred)}
  LOCALE_IMONLZERO              = $00000027;
    {leading zeros in month field (short date, LOCALE_SSHORTDATE is preferred)}
  LOCALE_S1159                  = $00000028;
    {AM designator}
  LOCALE_S2359                  = $00000029;
    {PM designator}
  LOCALE_ICALENDARTYPE          = $00001009;
    {type of calendar specifier}
  LOCALE_IOPTIONALCALENDAR      = $0000100B;
    {additional calendar types specifier}
  LOCALE_IFIRSTDAYOFWEEK        = $0000100C;
    {first day of week specifier}
  LOCALE_IFIRSTWEEKOFYEAR       = $0000100D;
    {first week of year specifier}
  LOCALE_SDAYNAME1              = $0000002A;
    {long name for Monday}
  LOCALE_SDAYNAME2              = $0000002B;
    {long name for Tuesday}
  LOCALE_SDAYNAME3              = $0000002C;
    {long name for Wednesday}
  LOCALE_SDAYNAME4              = $0000002D;
    {long name for Thursday}
  LOCALE_SDAYNAME5              = $0000002E;
    {long name for Friday}
  LOCALE_SDAYNAME6              = $0000002F;
    {long name for Saturday}
  LOCALE_SDAYNAME7              = $00000030;
    {long name for Sunday}
  LOCALE_SABBREVDAYNAME1        = $00000031;
    {abbreviated name for Monday}
  LOCALE_SABBREVDAYNAME2        = $00000032;
    {abbreviated name for Tuesday}
  LOCALE_SABBREVDAYNAME3        = $00000033;
    {abbreviated name for Wednesday}
  LOCALE_SABBREVDAYNAME4        = $00000034;
    {abbreviated name for Thursday}
  LOCALE_SABBREVDAYNAME5        = $00000035;
    {abbreviated name for Friday}
  LOCALE_SABBREVDAYNAME6        = $00000036;
    {abbreviated name for Saturday}
  LOCALE_SABBREVDAYNAME7        = $00000037;
    {abbreviated name for Sunday}
  LOCALE_SMONTHNAME1            = $00000038;
    {long name for January}
  LOCALE_SMONTHNAME2            = $00000039;
    {long name for February}
  LOCALE_SMONTHNAME3            = $0000003A;
    {long name for March}
  LOCALE_SMONTHNAME4            = $0000003B;
    {long name for April}
  LOCALE_SMONTHNAME5            = $0000003C;
    {long name for May}
  LOCALE_SMONTHNAME6            = $0000003D;
    {long name for June}
  LOCALE_SMONTHNAME7            = $0000003E;
    {long name for July}
  LOCALE_SMONTHNAME8            = $0000003F;
    {long name for August}
  LOCALE_SMONTHNAME9            = $00000040;
    {long name for September}
  LOCALE_SMONTHNAME10           = $00000041;
    {long name for October}
  LOCALE_SMONTHNAME11           = $00000042;
    {long name for November}
  LOCALE_SMONTHNAME12           = $00000043;
    {long name for December}
  LOCALE_SMONTHNAME13           = $0000100E;
    {long name for 13th month (if exists)}
  LOCALE_SABBREVMONTHNAME1      = $00000044;
    {abbreviated name for January}
  LOCALE_SABBREVMONTHNAME2      = $00000045;
    {abbreviated name for February}
  LOCALE_SABBREVMONTHNAME3      = $00000046;
    {abbreviated name for March}
  LOCALE_SABBREVMONTHNAME4      = $00000047;
    {abbreviated name for April}
  LOCALE_SABBREVMONTHNAME5      = $00000048;
    {abbreviated name for May}
  LOCALE_SABBREVMONTHNAME6      = $00000049;
    {abbreviated name for June}
  LOCALE_SABBREVMONTHNAME7      = $0000004A;
    {abbreviated name for July}
  LOCALE_SABBREVMONTHNAME8      = $0000004B;
    {abbreviated name for August}
  LOCALE_SABBREVMONTHNAME9      = $0000004C;
    {abbreviated name for September}
  LOCALE_SABBREVMONTHNAME10     = $0000004D;
    {abbreviated name for October}
  LOCALE_SABBREVMONTHNAME11     = $0000004E;
    {abbreviated name for November}
  LOCALE_SABBREVMONTHNAME12     = $0000004F;
    {abbreviated name for December}
  LOCALE_SABBREVMONTHNAME13     = $0000100F;
    {abbreviated name for 13th month (if exists)}
  LOCALE_SPOSITIVESIGN          = $00000050;
    {positive sign}
  LOCALE_SNEGATIVESIGN          = $00000051;
    {negative sign}
  LOCALE_IPOSSIGNPOSN           = $00000052;
    {positive sign position (derived from INEGCURR)}
  LOCALE_INEGSIGNPOSN           = $00000053;
    {negative sign position (derived from INEGCURR)}
  LOCALE_IPOSSYMPRECEDES        = $00000054;
    {mon sym precedes pos amt (derived from ICURRENCY)}
  LOCALE_IPOSSEPBYSPACE         = $00000055;
    {mon sym sep by space from pos amt (derived from ICURRENCY)}
  LOCALE_INEGSYMPRECEDES        = $00000056;
    {mon sym precedes neg amt (derived from INEGCURR)}
  LOCALE_INEGSEPBYSPACE         = $00000057;
    {mon sym sep by space from neg amt (derived from INEGCURR)}

  // Windows 9x and NT 4 and later
  LOCALE_FONTSIGNATURE          = $00000058;
    {font signature}
  LOCALE_SISO639LANGNAME        = $00000059;
    {ISO abbreviated language name}
  LOCALE_SISO3166CTRYNAME       = $0000005A;
    {ISO abbreviated country name}

  // Windows XP and later
  LOCALE_IDEFAULTEBCDICCODEPAGE = $00001012;
    {default ebcdic code page}
  LOCALE_IPAPERSIZE             = $0000100A;
    {default paper size: 1 = letter, 5 = legal, 8 = a3, 9 = a4}
  LOCALE_SENGCURRNAME           = $00001007;
    {english name of currency}
  LOCALE_SNATIVECURRNAME        = $00001008;
    {native name of currency}
  LOCALE_SYEARMONTH             = $00001006;
    {year month format string}
  LOCALE_SSORTNAME              = $00001013;
    {sort name}
  LOCALE_IDIGITSUBSTITUTION     = $00001014;
    {determines shape of digits: 0 = context, 1 = none, 2 = national}

  // Windows Vista and later
  LOCALE_SNAME                  = $0000005C;
    {locale name (eg: en-us)}
  LOCALE_SDURATION              = $0000005D;
    {time duration format}
  LOCALE_SKEYBOARDSTOINSTALL    = $0000005E;
    {semicolon-delimited list of keyboards to potentially install}
  LOCALE_SSHORTESTDAYNAME1      = $00000060;
    {Shortest day name for Monday}
  LOCALE_SSHORTESTDAYNAME2      = $00000061;
    {Shortest day name for Tuesday}
  LOCALE_SSHORTESTDAYNAME3      = $00000062;
    {Shortest day name for Wednesday}
  LOCALE_SSHORTESTDAYNAME4      = $00000063;
    {Shortest day name for Thursday}
  LOCALE_SSHORTESTDAYNAME5      = $00000064;
    {Shortest day name for Friday}
  LOCALE_SSHORTESTDAYNAME6      = $00000065;
    {Shortest day name for Saturday}
  LOCALE_SSHORTESTDAYNAME7      = $00000066;
    {Shortest day name for Sunday}
  LOCALE_SISO639LANGNAME2       = $00000067;
    {3 character ISO abbreviated language name}
  LOCALE_SISO3166CTRYNAME2      = $00000068;
    {3 character ISO country name}
  LOCALE_SNAN                   = $00000069;
    {Not a Number}
  LOCALE_SPOSINFINITY           = $0000006A;
    {+ Infinity}
  LOCALE_SNEGINFINITY           = $0000006B;
    {- Infinity}
  LOCALE_SSCRIPTS               = $0000006C;
    {Typical scripts in the locale}
  LOCALE_SPARENT                = $0000006D;
    {Fallback name for resources}
  LOCALE_SCONSOLEFALLBACKNAME   = $0000006E;
    {Fallback name for within the console}
  LOCALE_SLANGDISPLAYNAME       = $0000006F;
    {Language Display Name for a language}

  // Windows 98, Windows NT 4.0 and later
  LOCALE_RETURN_NUMBER          = $20000000;
    {Return number instead of string}


  //
  //  Enumeration flags. Windows Vista and later.
  //
  LOCALE_ALL              = 0;
    {enumerate all named based locales}
  LOCALE_WINDOWS          = $00000001;
    {shipped locales and/or replacements for them}
  LOCALE_SUPPLEMENTAL     = $00000002;
    {supplemental locales only}
  LOCALE_ALTERNATE_SORTS  = $00000004;
    {alternate sort locales}
  LOCALE_REPLACEMENT      = $00000008;
    {replacement locales only (supplemental - custom)}


  //
  // String based locale names: Windows Vista and later
  //
  LOCALE_NAME_USER_DEFAULT: PWideChar   = nil;
  LOCALE_NAME_INVARIANT: PWideChar      = '';
  LOCALE_NAME_SYSTEM_DEFAULT: PWideChar = '!x-sys-default-locale';


function GetLocaleInfo(const LocaleID: LCID; const InfoType: LCTYPE): string;
  {Gets information about a LocaleID in an OS-safe way.
    @param LocaleID [in] Identifies locale about which information is required.
    @param InfoType [in] Type of information required (a LOCALE_* identifier).
    @return Required information.
    @except EOSError raised if locale information can't be found.
  }

function DefaultAnsiCodePage: Cardinal;
  {Gets the default Ansi code page of the system.
    @return Required code page.
  }

function DefaultLanguageID: Cardinal;
  {Gets the id of default language for the current user.
    @return Required language id.
  }


implementation


uses
  // Delphi
  SysUtils,
  // Project
  USystemInfo;


function DefaultAnsiCodePage: Cardinal;
  {Gets the default Ansi code page of the system.
    @return Required code page.
  }
begin
  Result := Windows.GetACP;
end;

function DefaultLanguageID: Cardinal;
  {Gets the id of default language for the current user.
    @return Required language id.
  }
var
  LangId: string; // language ID as string
begin
  LangId := GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_ILANGUAGE);
  Result := StrToInt('$' + LangId);
end;

type
  {
  TGetLocaleInfoEx:
    Type of GetLocaleInfoEx function in Kernel32. Retrieves information about a
    locale specified by name. Should be used in preference to GetLocaleInfo on
    Windows Vista and later since GetLocaleInfo can be buggy on these OSs.
      @param lpLocaleName [in] Name of locale based on language tagging
        conventions of RFC 4646.
      @param LCType [in] Type of locale information to retrieve.
      @param lpLCData [in] Pointer to buffer to receive information. May be nil
        if cchData is 0.
      @param cchData [in] Size, in WideChars, of the data buffer pointed to by
        lpLCData. Can be 0 to get required buffer size.
      @return Number of WideChars stored in lpLCData if cchData is non-zero,
        size of data buffer required if cchData is zero and 0 if function fails.
  }
  TGetLocaleInfoEx = function(lpLocaleName: PWideChar; LCType: LCTYPE;
    lpLCData: PWideChar; cchData: Integer): Integer; stdcall;


var
  APIGetLocaleInfoEx: TGetLocaleInfoEx = nil;
    {Pointer to GetLocaleInfoEx function in Kernel32. Nil if the function is
    not supported}


function GetLocaleInfoW(const LocaleID: LCID; const InfoType: LCTYPE): string;
  {Queries the OS for information for a specified locale. This Unicode version
  works on NT systems. Based on code from SysUtils which, according to
  Borland, this works correctly for Asian WinNT.
    @param LocaleID [in] Identifies locale about which information is required.
    @param InfoType [in] Type of information required.
    @return Required information.
    @except EOSError raised if locale information can't be found.
  }
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

function GetLocaleInfoEx(const LocaleID: LCID; const InfoType: LCTYPE): string;
  {Query the OS for information for a specified locale by name. According to
  MSDN this method is preferred on Vista or later since GetLocaleInfo() API
  function can provide erroneous information on Vista.
    @param LocaleID [in] Identifies locale about which information is required.
    @param InfoType [in] Type of information required.
    @return Required information.
    @except EOSError raised if locale information can't be found.
  }
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
  {Gets information about a LocaleID in an OS-safe way.
    @param LocaleID [in] Identifies locale about which information is required.
    @param InfoType [in] Type of information required (a LOCALE_* identifier).
    @return Required information.
    @except EOSError raised if locale information can't be found.
  }
begin
  Assert(TOSInfo.IsWinNT, 'GetLocaleInfo: NT platform required');
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

