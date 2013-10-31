{
 * UFontHelper.pas
 *
 * Implements a static class used to assist when working with fonts.
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
 * The Original Code is UFontHelper.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
    class function IsTrueTypeFont(const Font: TFont): Boolean;
      {Checks if a font is a true type font.
        @param Font [in] Font to be checked.
        @return True if Font is true type, False if not.
      }
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
    class procedure SetDefaultFont(const Font: TFont;
      const ForceTrueType: Boolean);
      {Sets a font to be the default UI font for the underlying operating
      system.
        @param Font [in] Font to be set.
        @param ForceTrueType [in] Whether content font must be true type.
      }
    class procedure SetDefaultBaseFont(const BaseFont: TFont;
      const ForceTrueType: Boolean);
      {Updates a font to use the face of the underlying operating system. Style,
      colour etc are preserved and point size may be adjusted to retain relative
      size to base font.
        @param Font [in] Font to be updated.
        @param ForceTrueType [in] Whether content font must be true type.
      }
    class procedure SetContentFont(const Font: TFont;
      const ForceTrueType: Boolean);
      {Sets a font to be the appropriate content font for the underlying
      operating system.
        @param Font [in] Font to be set.
        @param ForceTrueType [in] Whether content font must be true type.
      }
    class procedure SetDefaultMonoFont(const Font: TFont;
      const ForceTrueType: Boolean);
      {Sets a font to be used as the default mono spaced font used by the
      program.
        @param Font [in] Font to be set.
        @param ForceTrueType [in] Whether font must be true type.
      }
    class function CloneFontHandle(const Handle: THandle): THandle;
      {Clones a font described by a font handle. Takes a copy of the specified
      font.
        @param Handle [in] Handle to font to be cloned.
        @return Handle to cloned font. Caller is responsible for releasing the
          handle.
      }
  public
    const
      DefaultMonoFontName = 'Courier New';      // Default mono font name
  strict private
    const
      DefaultFontName = 'MS Sans Serif';        // Default font name
      DefaultTTFontName = 'Arial';              // Default TT font name
      DefaultFontSize = 8;                      // Default font size

      DefaultContentFontName = DefaultFontName; // Default content font name
      DefaultTTContentFontName                  // Default TT content font name
        = DefaultTTFontName;
      DefaultContentFontSize = DefaultFontSize; // Default content font size

      VistaFontName = 'Segoe UI';               // Vista default font name
      VistaFontSize = 9;                        // Vista default font size
      VistaContentFontName = 'Calibri';         // Vista content font name
      VistaContentFontSize = 10;                // Vista content font size

      XPFontName = 'Tahoma';                    // XP default font name
      XPFontSize = DefaultFontSize;             // XP default font size
      XPContentFontName = 'Verdana';            // XP content font name
      XPContentFontSize                         // XP content font size
        = DefaultContentFontSize;
      DefaultTTMonoFontName                     // Default TT mono font
        = DefaultMonoFontName;
      DefaultMonoFontSize = 8;                  // Default mono font size
  end;


implementation


uses
  // Delphi
  SysUtils, StrUtils, Windows, Forms,
  // Project
  UGraphicUtils, USystemInfo;


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
    and not StartsStr('@', PLF.elfLogFont.lfFaceName) then
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

class function TFontHelper.IsTrueTypeFont(const Font: TFont): Boolean;
  {Checks if a font is a true type font.
    @param Font [in] Font to be checked.
    @return True if Font is true type, False if not.
  }
var
  DC: HDC;          // device context in which font is selected
  TM: TTextMetric;  // text metrics for font in DC
begin
  DC := CreateDisplayDC;
  try
    SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, TM);
    Result := (TM.tmPitchAndFamily and TMPF_TRUETYPE) = TMPF_TRUETYPE;
  finally
    DeleteDC(DC);
  end;
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

class procedure TFontHelper.SetContentFont(const Font: TFont;
  const ForceTrueType: Boolean);
  {Sets a font to be the appropriate content font for the underlying operating
  system.
    @param Font [in] Font to be set.
    @param ForceTrueType [in] Whether content font must be true type.
  }
begin
  Assert(TOSInfo.IsWinNT, ClassName + '.SetContentFont: NT platform required');
  // Set default content font, size and style
  Font.Name := DefaultContentFontName;
  Font.Size := DefaultContentFontSize;
  Font.Style := [];
  if TOSInfo.CheckReportedOS(TOSInfo.WinVista) then
  begin
    // We have Vista - use Calibri if installed
    if FontExists(VistaContentFontName) then
    begin
      Font.Name := VistaContentFontName;
      Font.Size := VistaContentFontSize;
    end;
  end
  else if TOSInfo.CheckReportedOS(TOSInfo.Win2K) then
  begin
    // We have Win 2K or XP - use Verdana if installed
    if FontExists(XPContentFontName) then
    begin
      Font.Name := XPContentFontName;
      Font.Size := XPContentFontSize;
    end;
  end;
  // Force font to true type if required and necessary
  if ForceTrueType and not IsTrueTypeFont(Font) then
    Font.Name := DefaultTTContentFontName;
end;

class procedure TFontHelper.SetDefaultBaseFont(const BaseFont: TFont;
  const ForceTrueType: Boolean);
  {Updates a font to use the face of the underlying operating system. Style,
  colour etc are preserved and point size may be adjusted to retain relative
  size to base font.
    @param Font [in] Font to be updated.
    @param ForceTrueType [in] Whether content font must be true type.
  }
var
  DefaultFont: TFont;   // default font per OS
  FontDelta: Integer;   // amount to increment font size by
begin
  // Create default font
  DefaultFont := TFont.Create;
  try
    SetDefaultFont(DefaultFont, ForceTrueType);
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

class procedure TFontHelper.SetDefaultFont(const Font: TFont;
  const ForceTrueType: Boolean);
  {Sets a font to be the default UI font for the underlying operating system.
    @param Font [in] Font to be set.
    @param ForceTrueType [in] Whether content font must be true type.
  }
begin
  Assert(TOSInfo.IsWinNT, ClassName + '.SetDefaultFont: NT platform required');
  // Set default font, size and style
  Font.Name := DefaultFontName;
  Font.Size := DefaultFontSize;
  Font.Style := [];
  if TOSInfo.CheckReportedOS(TOSInfo.WinVista) then
  begin
    // We have Vista - use Segoe UI if installed
    if FontExists(VistaFontName) then
    begin
      Font.Name := VistaFontName;
      Font.Size := VistaFontSize;
    end;
  end
  else if TOSInfo.CheckReportedOS(TOSInfo.Win2K) then
  begin
    // We have Win 2K or XP - use Tahoma if installed
    if FontExists(XPFontName) then
    begin
      Font.Name := XPFontName;
      Font.Size := XPFontSize;
    end;
  end;
  // Force font to true type if required and necessary
  if ForceTrueType and not IsTrueTypeFont(Font) then
    Font.Name := DefaultTTFontName;
end;

class procedure TFontHelper.SetDefaultMonoFont(const Font: TFont;
  const ForceTrueType: Boolean);
  {Sets a font to be used as the default mono spaced font used by the program.
    @param Font [in] Font to be set to default mono font.
    @param ForceTrueType [in] Whether font must be true type.
  }
begin
  if ForceTrueType then
    Font.Name := DefaultTTMonoFontName
  else
    Font.Name := DefaultMonoFontName;
  Font.Size := DefaultMonoFontSize;
  Font.Style := [];
end;

end.

