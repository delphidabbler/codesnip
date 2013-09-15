{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Singleton object that gives access to persistent configuration information.
 * the user.
}


unit CS.Config;

interface

uses
  USingleton,
  CS.SourceCode.Hiliter.Themes,
  CS.SourceCode.Languages;

type
  TConfig = class(TSingleton)
  strict private
    var
      fSourceCodeLanguages: TSourceCodeLanguages;
      fHiliterThemes: TSyntaxHiliteThemes;
    function GetSourceCodeLanguages: TSourceCodeLanguages;
    function GetHiliterThemes: TSyntaxHiliteThemes;
  strict protected
    procedure Initialize; override;
    procedure Finalize; override;
  public
    class function Instance: TConfig; inline;
    property SourceCodeLanguages: TSourceCodeLanguages
      read GetSourceCodeLanguages;
    property HiliterThemes: TSyntaxHiliteThemes
      read GetHiliterThemes;
  end;

implementation

uses
  Windows,
  CS.SourceCode.Hiliter.Themes.Persist,
  CS.SourceCode.Languages.Persist;

{ TConfig }

procedure TConfig.Finalize;
begin
  // TODO: save languages here once user defined languages have been enabled
  // TODO: save themes here once use defined themes have been enabled
  fHiliterThemes.Free;
  fSourceCodeLanguages.Free;
end;

function TConfig.GetHiliterThemes: TSyntaxHiliteThemes;
begin
  if not Assigned(fHiliterThemes) then
  begin
    fHiliterThemes := TSyntaxHiliteThemes.Create;
    TSyntaxHiliteThemesIO.LoadThemesFromResources(
      fHiliterThemes, 'HILITETHEMES', RT_RCDATA
    );
    // TODO: load user defined themes
  end;
  Result := fHiliterThemes;
end;

function TConfig.GetSourceCodeLanguages: TSourceCodeLanguages;
begin
  if not Assigned(fSourceCodeLanguages) then
  begin
    fSourceCodeLanguages := TSourceCodeLanguages.Create;
    TSourceCodeLanguagesIO.LoadFromResources(
      fSourceCodeLanguages, 'SOURCECODELANGUAGES', RT_RCDATA
    );
    // TODO: load user defined languages
  end;
  Result := fSourceCodeLanguages;
end;

procedure TConfig.Initialize;
begin
end;

class function TConfig.Instance: TConfig;
begin
  // Because this is a singleton calling Instance is the same as calling Create.
  // The look-ups performed when Create is incur a small penalty each time this
  // method is call, but this is probably not significant.
  Result := Create;
end;

end.
