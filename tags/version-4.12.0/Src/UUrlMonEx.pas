{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Imports a function from UrlMon.dll that is only present on later versions of
 * Windows.
}


unit UUrlMonEx;


interface


uses
  // Delphi
  Windows, UrlMon;


const
  // Internet Explorer feature controls.
  // For documentation see http://msdn.microsoft.com/en-us/library/ms537169
  FEATURE_OBJECT_CACHING                  = 0;
  FEATURE_ZONE_ELEVATION                  = 1;
  FEATURE_MIME_HANDLING                   = 2;
  FEATURE_MIME_SNIFFING                   = 3;
  FEATURE_WINDOW_RESTRICTIONS             = 4;
  FEATURE_WEBOC_POPUPMANAGEMENT           = 5;
  FEATURE_BEHAVIORS                       = 6;
  FEATURE_DISABLE_MK_PROTOCOL             = 7;
  FEATURE_LOCALMACHINE_LOCKDOWN           = 8;
  FEATURE_SECURITYBAND                    = 9;
  FEATURE_RESTRICT_ACTIVEXINSTALL         = 10;
  FEATURE_VALIDATE_NAVIGATE_URL           = 11;
  FEATURE_RESTRICT_FILEDOWNLOAD           = 12;
  FEATURE_ADDON_MANAGEMENT                = 13;
  FEATURE_PROTOCOL_LOCKDOWN               = 14;
  FEATURE_HTTP_USERNAME_PASSWORD_DISABLE  = 15;
  FEATURE_SAFE_BINDTOOBJECT               = 16;
  FEATURE_UNC_SAVEDFILECHECK              = 17;
  FEATURE_GET_URL_DOM_FILEPATH_UNENCODED  = 18;
  FEATURE_TABBED_BROWSING                 = 19;
  FEATURE_SSLUX                           = 20;
  FEATURE_DISABLE_NAVIGATION_SOUNDS       = 21;
  FEATURE_DISABLE_LEGACY_COMPRESSION      = 22;
  FEATURE_FORCE_ADDR_AND_STATUS           = 23;
  FEATURE_XMLHTTP                         = 24;
  FEATURE_DISABLE_TELNET_PROTOCOL         = 25;
  FEATURE_FEEDS                           = 26;
  FEATURE_BLOCK_INPUT_PROMPTS             = 27;
  FEATURE_ENTRY_COUNT                     = 28;

const
  // Flags that specify where to set an Internet Explorer feature control
  // For documentation see http://msdn.microsoft.com/en-us/library/ms537168
  SET_FEATURE_ON_THREAD                   = $00000001;
  SET_FEATURE_ON_PROCESS                  = $00000002;
  SET_FEATURE_IN_REGISTRY                 = $00000004;
  SET_FEATURE_ON_THREAD_LOCALMACHINE      = $00000008;
  SET_FEATURE_ON_THREAD_INTRANET          = $00000010;
  SET_FEATURE_ON_THREAD_TRUSTED           = $00000020;
  SET_FEATURE_ON_THREAD_INTERNET          = $00000040;
  SET_FEATURE_ON_THREAD_RESTRICTED        = $00000080;

type
  ///  <summary>Type of CoInternetSetFeatureEnabled function imported from
  ///  UrlMon.dll used to enable or disable an Internet Explorer feature
  ///  control.</summary>
  ///  <param name="FeatureEntry">DWORD [in] Specifies feature control to enable
  ///  or disable.</param>
  ///  <param name="Flags">DWORD [in] Specifies where to set the feature control
  ///  value.</param>
  ///  <param name="Enable">BOOL [in] Specifies whether the feature control is
  ///  enables (True) or disabled (False).</param>
  ///  <returns>HResult. S_OK on success, E_FAIL on failure (or, in this
  ///  implementation, E_NOTIMPL if the function is not implemented.</returns>
  ///  <remarks>Supported from Windows XP SP2, Windows Server 2003 (and Internet
  ///  Explorer 6.0).</remarks>
  TCoInternetSetFeatureEnabled = function(
    FeatureEntry: DWORD;
    Flags: DWORD;
    Enable: BOOL
  ): HResult; stdcall;

var
  // Pointer to CoInternetSetFeatureEnabled function imported from UrlMon.dll,
  // or to a fallback function if UrlMon.dll does not export
  // CoInternetSetFeatureEnabled.
  // NOTE: The fallback function does nothing and simply returns E_NOTIMPL.
  CoInternetSetFeatureEnabled: TCoInternetSetFeatureEnabled;


implementation


uses
  // Delphi
  SysUtils;

var
  // Handle to UrlMon.dll</summary>
  LibHandle: THandle;

///  <summary>Fallback function for use when UrlMon.dll does not export
///  CoInternetSetFeatureEnabled.</summary>
///  <remarks>Does nothing and returns E_NOTIMPL.</remarks>
function CoInternetSetFeatureEnabledFallback(FeatureEntry: DWORD;
  Flags: DWORD; Enable: BOOL): HResult;  stdcall;
begin
  Result := E_NOTIMPL;
end;

///  <summary>Initialises CoInternetSetFeatureEnabled function pointer.
///  </summary>
procedure Init;
begin
  LibHandle := SafeLoadLibrary('urlmon.dll');
  CoInternetSetFeatureEnabled := nil;
  if LibHandle <> 0 then
    CoInternetSetFeatureEnabled := GetProcAddress(
      LibHandle, 'CoInternetSetFeatureEnabled'
    );
  if not Assigned(CoInternetSetFeatureEnabled) then
    CoInternetSetFeatureEnabled := CoInternetSetFeatureEnabledFallback;
end;

initialization

Init;

finalization

FreeLibrary(LibHandle);

end.
