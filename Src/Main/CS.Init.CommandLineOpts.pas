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
 * Provides a static advanced record that encapsulates options passed to the
 * program on its command line.
}


unit CS.Init.CommandLineOpts;

interface

type
  ///  <summary>Encapsulates options passed to the program on its command line.
  ///  </summary>
  TCommandLineOpts = record
  strict private
    const
      LocalHostSwitch = 'localhost';
      PortableSwitch = 'portable';
      NoSplashSwitch = 'no-splash';
    class var
      fUseLocalHost: Boolean;
      fIsPortable: Boolean;
      fNoSplash: Boolean;
  public
    ///  <summary>Parses the command line on startup.</summary>
    class constructor Create;
    ///  <summary>Checks if the program is to use a test web server on localhost
    ///  when accessing the internet.</summary>
    ///  <returns>True if localhost is to be used or False otherwise.</returns>
    class function UseLocalHost: Boolean; static; inline;
    ///  <summary>Checks whether the program is to run in portable mode.
    ///  </summary>
    ///  <returns>True if the program is to run in portable mode or False if it
    ///  to run in standard mode (the default).</returns>
    class function IsPortable: Boolean; static; inline;
    ///  <summary>Checks whether the program is to display its splash screen.
    ///  </summary>
    ///  <returns>True if the splash screen is to be hidden or False if the
    ///  splash screen is to be displayed.</returns>
    class function NoSplash: Boolean; static; inline;
  end;

implementation

uses
  SysUtils;

{ TCommandLineOpts }

class constructor TCommandLineOpts.Create;
begin
  fUseLocalHost := FindCmdLineSwitch(LocalHostSwitch, True);
  fIsPortable := FindCmdLineSwitch(PortableSwitch, True);
  fNoSplash := FindCmdLineSwitch(NoSplashSwitch, True);
end;

class function TCommandLineOpts.IsPortable: Boolean;
begin
  Result := fIsPortable;
end;

class function TCommandLineOpts.NoSplash: Boolean;
begin
  Result := fNoSplash;
end;

class function TCommandLineOpts.UseLocalHost: Boolean;
begin
  Result := fUseLocalHost;
end;

end.
