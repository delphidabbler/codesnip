unit UI.ThemeManager;

interface

type
  TThemeManager = class
  strict private
    const
      // Theme style names
      DarkModeTheme = 'Carbon';
      LightModeTheme = 'Windows10';
      ThemeNames: array[0..1] of string = (DarkModeTheme, LightModeTheme);
    class function IndexOfTheme(ATheme: string): Integer;
    class function ApplyTheme(ATheme: string): Boolean;
    class function GetCurrentTheme: string;
   public
    const
      Themes: array[0..1] of string = ('Dark', 'Light');
    class function ApplyCurrentTheme: Boolean;
  end;

implementation

uses
  System.SysUtils,
  Vcl.Themes,

  UPreferences,
  UStrUtils;

{ TThemeManager }

class function TThemeManager.ApplyCurrentTheme: Boolean;
begin
  Result := ApplyTheme(GetCurrentTheme);
end;

class function TThemeManager.ApplyTheme(ATheme: string): Boolean;
begin
  Result := False;
  var Idx := IndexOfTheme(ATheme);
  if Idx < 0 then
    Exit;
  Result := True;
  if not TStyleManager.TrySetStyle(ThemeNames[Idx], False) then
    Exit(False);
end;

class function TThemeManager.GetCurrentTheme: string;
begin
  Result := Preferences.ThemeName;
end;

class function TThemeManager.IndexOfTheme(ATheme: string): Integer;
begin
  Result := -1;
  for var Idx := Low(Themes) to High(Themes) do
    if StrSameText(ATheme, Themes[Idx]) then
      Exit(Idx);
end;

end.
