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
  CS.SourceCode.Languages,
  UAppInfo;

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
  if Assigned(fHiliterThemes) then
  begin
    TSyntaxHiliteThemesIO.SaveThemes(
      fHiliterThemes, TAppInfo.HiliteThemesFileName
    );
    fHiliterThemes.Free;
  end;
  if Assigned(fSourceCodeLanguages) then
  begin
    TSourceCodeLanguagesIO.Save(
      fSourceCodeLanguages, TAppInfo.SourceCodeLanguagesFileName
    );
    fSourceCodeLanguages.Free;
  end;
end;

function TConfig.GetHiliterThemes: TSyntaxHiliteThemes;
begin
  if not Assigned(fHiliterThemes) then
  begin
    fHiliterThemes := TSyntaxHiliteThemes.Create;
    TSyntaxHiliteThemesIO.LoadThemesFromResources(
      fHiliterThemes, 'HILITETHEMES', RT_RCDATA
    );
    TSyntaxHiliteThemesIO.LoadThemes(
      fHiliterThemes, TAppInfo.HiliteThemesFileName
    );
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
    TSourceCodeLanguagesIO.Load(
      fSourceCodeLanguages, TAppInfo.SourceCodeLanguagesFileName
    );
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
