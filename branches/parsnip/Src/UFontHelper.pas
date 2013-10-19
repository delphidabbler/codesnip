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
 * Implements a static class used to assist when working with fonts.
}


unit UFontHelper;


interface


uses
  // Delphi
  Classes, Graphics,
  // Project
  UBaseObjects;


type

  ///  <summary>Container for methods for assisting with font related
  ///  operations.</summary>
  TFontHelper = class(TNoConstructObject)
  public
    ///  <summary>Checks if a font with the given name exists on the system.
    ///  </summary>
    class function FontExists(const FontName: string): Boolean;
    ///  <summary>Returns an array containing the names of all mono-spaced fonts
    ///  that are installed on the system.</summary>
    class function ListMonoSpaceFonts: TArray<string>;
    ///  <summary>Sets the given font object to the default UI font for the
    ///  underlying operating system.</summary>
    class procedure SetDefaultFont(const Font: TFont);
    ///  <summary>Sets all the given font objects to the default UI font for the
    ///  underlying operating system.</summary>
    class procedure SetDefaultFonts(const Fonts: array of TFont);
    ///  <summary>Updates the given font object to have the same font name as
    ///  the default UI font of the underlying operating system.</summary>
    class procedure SetDefaultBaseFont(const BaseFont: TFont);
    ///  <summary>Updates all the given font object to have the same font name
    ///  as the default UI font of the underlying operating system.</summary>
    class procedure SetDefaultBaseFonts(const Fonts: array of TFont);
    ///  <summary>Sets the given font object to an appropriate content font for
    ///  the underlying operating system.</summary>
    class procedure SetContentFont(const Font: TFont);
    ///  <summary>Sets the given font object to the program's default mono-space
    ///  font.</summary>
    class procedure SetDefaultMonoFont(const Font: TFont);
    ///  <summary>Returns the name of the program's default mono-spaced font.
    ///  </summary>
    class function GetDefaultMonoFontName: string;
    ///  <summary>Clones the font with the given font handle and returns the
    ///  handle of the cloned font.</summary>
    ///  <remarks>The caller is responsible for releasing the returned handle.
    ///  </remarks>
    class function CloneFontHandle(const Handle: THandle): THandle;
  strict private
    const
      ///  <summary>Default UI font name used for OSs earlier than Windows XP.
      ///  </summary>
      DefaultFontName = 'Arial';
      ///  <summary>Default UI font size used for OSs earlier than Windows XP.
      ///  </summary>
      DefaultFontSize = 8;
      ///  <summary>Default content font name used for OSs earlier than Windows
      ///  XP.</summary>
      DefaultContentFontName = DefaultFontName;
      ///  <summary>Default content font size used for OSs earlier than Windows
      ///  XP.</summary>
      DefaultContentFontSize = DefaultFontSize;
      ///  <summary>Default UI font name used by Windows Vista and later.
      ///  </summary>
      VistaFontName = 'Segoe UI';
      ///  <summary>Default UI font size used by Windows Vista and later.
      ///  </summary>
      VistaFontSize = 9;
      ///  <summary>Default content font name used by Windows Vista and later.
      ///  </summary>
      VistaContentFontName = 'Calibri';
      ///  <summary>Default content font size used by Windows Vista and later.
      ///  </summary>
      VistaContentFontSize = 10;
      ///  <summary>Default UI font name used by Windows XP.</summary>
      XPFontName = 'Tahoma';
      ///  <summary>Default UI font size used by Windows XP.</summary>
      XPFontSize = DefaultFontSize;
      ///  <summary>Default content font name used by Windows XP.</summary>
      XPContentFontName = 'Verdana';
      ///  <summary>Default content font size used by Windows XP.</summary>
      XPContentFontSize = DefaultContentFontSize;
      ///  <summary>Default mono spaced font name used for any OS.</summary>
      DefaultMonoFontName = 'Courier New';
      ///  <summary>Default mono spaced font size used for any OS.</summary>
      DefaultMonoFontSize = 8;
  end;


implementation


uses
  // Delphi
  SysUtils, Windows, Forms,
  // Project
  UGraphicUtils, USystemInfo;


///  <summary>EnumFontFamilies() callback function used to add mono-spaced fonts
///  to a string list.</summary>
///  <remarks>For more info about this callback see
///  http://msdn.microsoft.com/en-us/library/aa911409.aspx</remarks>
function MonoFontFamilyProc(PLF: PEnumLogFont; PNTM: PNewTextMetric;
  FontType: Integer; List: TStrings): Integer; stdcall;
begin
  if (PLF.elfLogFont.lfPitchAndFamily and $F) = FIXED_PITCH then
    List.Add(PLF.elfLogFont.lfFaceName);
  Result := 1;
end;

{ TFontHelper }

class function TFontHelper.CloneFontHandle(const Handle: THandle): THandle;
var
  LogFont: TLogFont;  // logical font info for memo control's font
begin
  Result := 0;
  if GetObject(Handle, SizeOf(LogFont), @LogFont) > 0 then
    Result := CreateFontIndirect(LogFont);
  Assert(Result <> 0, ClassName + '.CloneFontHandle: Can''t clone handle');
end;

class function TFontHelper.FontExists(const FontName: string): Boolean;
begin
  Result := Screen.Fonts.IndexOf(FontName) >= 0;
end;

class function TFontHelper.GetDefaultMonoFontName: string;
begin
  Result := DefaultMonoFontName;
end;

class function TFontHelper.ListMonoSpaceFonts: TArray<string>;
var
  DC: HDC;              // device context required for API call
  FontNames: TStrings;  // receives list of font names
begin
  FontNames := TStringList.Create;
  try
    // Get a device context needed for API call
    DC := GetDC(0);
    try
      // Enumerate all font families: handle each font in MonoFontFamilyProc()
      EnumFontFamilies(DC, nil, @MonoFontFamilyProc, LPARAM(FontNames));
    finally
      ReleaseDC(0, DC);
    end;
    Result := FontNames.ToStringArray;
  finally
    FontNames.Free;
  end
end;

class procedure TFontHelper.SetContentFont(const Font: TFont);
begin
  // Set default content font, size and style
  Font.Name := DefaultContentFontName;
  Font.Size := DefaultContentFontSize;
  Font.Style := [];
  if TOSInfo.CheckReportedOS(TOSInfo.WinVista) then
  begin
    // We have Vista or later - use Calibri if installed
    if FontExists(VistaContentFontName) then
    begin
      Font.Name := VistaContentFontName;
      Font.Size := VistaContentFontSize;
    end;
  end
  else
  begin
    // Earlier OS than Vista (i.e. 2000 or XP)
    if FontExists(XPContentFontName) then
    begin
      Font.Name := XPContentFontName;
      Font.Size := XPContentFontSize;
    end;
  end;
end;

class procedure TFontHelper.SetDefaultBaseFont(const BaseFont: TFont);
var
  DefaultFont: TFont;   // default font per OS
  FontDelta: Integer;   // amount to increment font size by
begin
  // Create default font
  DefaultFont := TFont.Create;
  try
    SetDefaultFont(DefaultFont);
    // font delta is difference between normal default font size and that used
    // on a specific OS (e.g. Vista uses Segoe UI 9 rather than MS Sans Serif 8)
    FontDelta := DefaultFont.Size - DefaultFontSize;
    // change base font name and size as required
    BaseFont.Name := DefaultFont.Name;
    BaseFont.Size := BaseFont.Size + FontDelta;
  finally
    FreeAndNil(DefaultFont);
  end;
end;

class procedure TFontHelper.SetDefaultBaseFonts(const Fonts: array of TFont);
var
  Font: TFont;
begin
  for Font in Fonts do
    SetDefaultBaseFont(Font);
end;

class procedure TFontHelper.SetDefaultFont(const Font: TFont);
begin
  // Set default font, size and style
  Font.Name := DefaultFontName;
  Font.Size := DefaultFontSize;
  Font.Style := [];
  if TOSInfo.CheckReportedOS(TOSInfo.WinVista) then
  begin
    // Vista or later
    if FontExists(VistaFontName) then
    begin
      Font.Name := VistaFontName;
      Font.Size := VistaFontSize;
    end;
  end
  else
  begin
    // Earlier OS than Vista (i.e. 2000 or XP)
    if FontExists(XPFontName) then
    begin
      Font.Name := XPFontName;
      Font.Size := XPFontSize;
    end;
  end;
end;

class procedure TFontHelper.SetDefaultFonts(const Fonts: array of TFont);
var
  Font: TFont;
begin
  for Font in Fonts do
    SetDefaultFont(Font);
end;

class procedure TFontHelper.SetDefaultMonoFont(const Font: TFont);
begin
  Font.Name := DefaultMonoFontName;
  Font.Size := DefaultMonoFontSize;
  Font.Style := [];
end;

end.

