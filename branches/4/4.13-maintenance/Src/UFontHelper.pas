{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2015, Peter Johnson (www.delphidabbler.com).
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

  {
  TFontHelper:
    Static class used to assist when working with fonts.
  }
  TFontHelper = class(TNoConstructObject)
  strict private
    class function FontExists(const FontName: string): Boolean;
      {Checks if named font exists on sytem.
        @param FontName [in] Name of required font.
        @return True if font exists, False if not.
      }
  public
    class procedure ListMonoSpaceFonts(const List: TStrings);
      {Lists all mono-space fonts on system.
        @param List [in] Receives list of fonts. Cleared before fonts added.
      }
    class procedure ListCommonFontSizes(const List: TStrings);
      {Lists all commonly used font sizes.
        @param List [in] Receives list of font sizes. Cleared before sizes
          added.
      }
    class procedure SetDefaultFont(const Font: TFont);
      {Sets a font to be the default UI font for the underlying operating
      system.
        @param Font [in] Font to be set.
      }
    class procedure SetDefaultFonts(const Fonts: array of TFont);
    class procedure SetDefaultBaseFont(const BaseFont: TFont);
      {Updates a font to use the face of the underlying operating system. Style,
      colour etc are preserved and point size may be adjusted to retain relative
      size to base font.
        @param Font [in] Font to be updated.
      }
    class procedure SetDefaultBaseFonts(const Fonts: array of TFont);
    class procedure SetContentFont(const Font: TFont);
      {Sets a font to be the appropriate content font for the underlying
      operating system.
        @param Font [in] Font to be set.
      }
    class procedure SetDefaultMonoFont(const Font: TFont);
      {Sets a font to be used as the default mono spaced font used by the
      program.
        @param Font [in] Font to be set.
      }
    class function CloneFontHandle(const Handle: THandle): THandle;
      {Clones a font described by a font handle. Takes a copy of the specified
      font.
        @param Handle [in] Handle to font to be cloned.
        @return Handle to cloned font. Caller is responsible for releasing the
          handle.
      }
  strict private
    const
      FallbackFontName = 'Arial';                 // Fallback font name
      FallbackFontSize = 8;                       // Fallback font size

      VistaFontName = 'Segoe UI';                 // Vista default font name
      VistaFontSize = 9;                          // Vista default font size
      VistaContentFontName = 'Calibri';           // Vista content font name
      VistaContentFontSize = 10;                  // Vista content font size

      XPFontName = 'Tahoma';                      // XP default font name
      XPFontSize = FallbackFontSize;              // XP default font size
      XPContentFontName = 'Verdana';              // XP content font name
      XPContentFontSize = FallbackFontSize;       // XP content font size

      DefaultMonoFontName = 'Courier New';        // Default mono font name
      DefaultMonoFontSize = 8;                    // Default mono font size
  end;


implementation


uses
  // Delphi
  SysUtils, Windows, Forms,
  // Project
  UGraphicUtils, UStrUtils;


{ TFontHelper }

function MonoFontFamilyProc(PLF: PEnumLogFont; PNTM: PNewTextMetric;
  FontType: Integer; List: TStrings): Integer; stdcall;
  {EnumFontFamilies() callback function used to add mono-spaced fonts to a list.
    @param PLF [in] Structure containing info about a logical font.
    @param PNTM [in] Not used.
    @param FontType [in] Not used.
    @param List [in] List to which mono-spaced fonts are added.
  }
begin
  // check for fixed pitch font and filter out all "vertical" fonts that start
  // with "@" (see http://tinyurl.com/6ul6rfo for details of vertical fonts).
  if ((PLF.elfLogFont.lfPitchAndFamily and $F) = FIXED_PITCH)
    and not StrStartsStr('@', PLF.elfLogFont.lfFaceName) then
    List.Add(PLF.elfLogFont.lfFaceName);
  Result := 1;
end;

class function TFontHelper.CloneFontHandle(const Handle: THandle): THandle;
  {Clones a font described by a font handle. Takes a copy of the specified font.
    @param Handle [in] Handle to font to be cloned.
    @return Handle to cloned font. Caller is responsible for releasing the
      handle.
  }
var
  LogFont: TLogFont;  // logical font info for memo control's font
begin
  Result := 0;
  if GetObject(Handle, SizeOf(LogFont), @LogFont) > 0 then
    Result := CreateFontIndirect(LogFont);
  Assert(Result <> 0, ClassName + '.CloneFontHandle: Can''t clone handle');
end;

class function TFontHelper.FontExists(const FontName: string): Boolean;
  {Checks if named font exists on sytem.
    @param FontName [in] Name of required font.
    @return True if font exists, False if not.
  }
begin
  Result := Screen.Fonts.IndexOf(FontName) >= 0;
end;

class procedure TFontHelper.ListCommonFontSizes(const List: TStrings);
  {Lists all commonly used font sizes.
    @param List [in] Receives list of font sizes. Cleared before sizes added.
  }
var
  FontSize: Integer;  // loops thru all font sizes
begin
  Assert(Assigned(List), ClassName + '.ListCommonFontSizes: List is nil');
  List.Clear;
  for FontSize := 7 to 32 do
    List.Add(IntToStr(FontSize));
end;

class procedure TFontHelper.ListMonoSpaceFonts(const List: TStrings);
  {Lists all mono-space fonts on system.
    @param List [in] Receives list of fonts. Cleared before fonts added.
  }
var
  DC: HDC;  // device context required for API call
begin
  Assert(Assigned(List), ClassName + '.ListMonoSpaceFonts: List is nil');
  List.Clear;
  // Get a device context needed for API call
  DC := GetDC(0);
  try
    // Enumerate all font families: handle each font in MonoFontFamilyProc()
    EnumFontFamilies(DC, nil, @MonoFontFamilyProc, Integer(List));
  finally
    ReleaseDC(0, DC);
  end;
end;

class procedure TFontHelper.SetContentFont(const Font: TFont);
  {Sets a font to be the appropriate content font for the underlying operating
  system.
    @param Font [in] Font to be set.
  }
begin
  // Try Vista & later content font. If that fails try XP/Win2k font. One of the
  // two should always work, but in case fonts have been uninstalled, use a
  // fallback font.
  if FontExists(VistaContentFontName) then
  begin
    Font.Name := VistaContentFontName;
    Font.Size := VistaContentFontSize;
  end
  else if FontExists(XPContentFontName) then
  begin
    Font.Name := XPContentFontName;
    Font.Size := XPContentFontSize;
  end
  else
  begin
    Font.Name := FallbackFontName;
    Font.Size := FallbackFontSize;
  end;
  Font.Style := [];
end;

class procedure TFontHelper.SetDefaultBaseFont(const BaseFont: TFont);
  {Updates a font to use the face of the underlying operating system. Style,
  colour etc are preserved and point size may be adjusted to retain relative
  size to base font.
    @param Font [in] Font to be updated.
  }
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
    FontDelta := DefaultFont.Size - FallbackFontSize;
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
  {Sets a font to be the default UI font for the underlying operating system.
    @param Font [in] Font to be set.
  }
begin
  // Try Vista & later default font. If that fails try XP/Win2k font. One of the
  // two should always work, but in case fonts have been uninstalled, use a
  // fallback font.
  if FontExists(VistaFontName) then
  begin
    Font.Name := VistaFontName;
    Font.Size := VistaFontSize;
  end
  else if FontExists(XPFontName) then
  begin
    Font.Name := XPFontName;
    Font.Size := XPFontSize;
  end
  else
  begin
    Font.Name := FallbackFontName;
    Font.Size := FallbackFontSize;
  end;
  Font.Style := [];
end;

class procedure TFontHelper.SetDefaultFonts(const Fonts: array of TFont);
var
  Font: TFont;
begin
  for Font in Fonts do
    SetDefaultFont(Font);
end;

class procedure TFontHelper.SetDefaultMonoFont(const Font: TFont);
  {Sets a font to be used as the default mono spaced font used by the program.
    @param Font [in] Font to be set to default mono font.
  }
begin
  Font.Name := DefaultMonoFontName;
  Font.Size := DefaultMonoFontSize;
  Font.Style := [];
end;

end.

