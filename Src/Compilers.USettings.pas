{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2022-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Class that reads and writes settings that apply to all compilers.
}


unit Compilers.USettings;

interface

uses
  // Delphi
  SysUtils,
  // Project
  UBaseObjects,
  USettings;

type
  ///  <summary>Manages settings that apply to all compilers.</summary>
  TCompilerSettings = class(TNoConstructObject)
  strict private
    const
      AllCompilersConfigSection = ssCompilers;
      PermitStartupDetectionKey = 'PermitStartupDetection';
    class function ReadStorage: ISettingsSection;
    class procedure DoSaveProperty(const WriteProp: TProc<ISettingsSection>);
    class procedure SaveProperty(const Key: string; const Value: Boolean);
    class function GetPermitStartupDetection: Boolean; static;
    class procedure SetPermitStartupDetection(const Value: Boolean); static;
  public
    class property PermitStartupDetection: Boolean
      read GetPermitStartupDetection write SetPermitStartupDetection
      default True;
  end;

implementation

{ TCompilerSettings }

class procedure TCompilerSettings.DoSaveProperty(
  const WriteProp: TProc<ISettingsSection>);
var
  Stg: ISettingsSection;
begin
  Stg := ReadStorage;
  WriteProp(Stg);
  Stg.Save;
end;

class function TCompilerSettings.GetPermitStartupDetection: Boolean;
begin
  Result := ReadStorage.GetBoolean(PermitStartupDetectionKey, True);
end;

class function TCompilerSettings.ReadStorage: ISettingsSection;
begin
  Result := Settings.ReadSection(AllCompilersConfigSection);
end;

class procedure TCompilerSettings.SaveProperty(const Key: string;
  const Value: Boolean);
begin
  DoSaveProperty(
    procedure(Stg: ISettingsSection)
    begin
      Stg.SetBoolean(Key, Value)
    end
  );
end;

class procedure TCompilerSettings.SetPermitStartupDetection(
  const Value: Boolean);
begin
  SaveProperty(PermitStartupDetectionKey, Value);
end;

end.

