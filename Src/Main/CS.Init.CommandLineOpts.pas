{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2016, Peter Johnson (www.delphidabbler.com).
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
      PortableSwitch = 'portable';
      NoSplashSwitch = 'no-splash';
    class var
      fIsPortable: Boolean;
      fNoSplash: Boolean;
      fTestServerHost: string;
  public
    ///  <summary>Parses the command line on startup.</summary>
    class constructor Create;
    ///  <summary>Returns the name of the server that hosts web services used by
    ///  CodeSnip when under testing. This server receives updated web services
    ///  before they are released to the production server.</summary>
    ///  <remarks>
    ///  <para>The name of this server must be passed on the command line via
    ///  the <c>--test-server</c> option. If this option is not specified then
    ///  <c>TestServerHost</c> returns the empty string.</para>
    ///  <para>The format of the command line switch is
    ///  <c>--test-server=server-name</c> or
    ///  <c>--test-server=server-name:port</c> where <c>server-name</c> is the
    ///  name of the test server and <c>port</c> is the port number it is
    ///  operating on, for example <c>--test-server=localhost:8080</c> or
    ///  <c>--test-server=test.delphidabbler.com</c>. The
    ///  port number and its preceding ':' character can be omitted if the
    ///  server is on port 80.</para>
    ///  </remarks>
    class function TestServerHost: string; static; inline;
    ///  <summary>Checks if the program is using a test web server.</summary>
    ///  <returns><c>Boolean</c>. <c>True</c> if a test web server is being
    ///  used, <c>False</c> if the production web server is being used.
    ///  </returns>
    ///  <remarks>
    ///  <para><c>True</c> is returned iff a valid <c>--test-server</c> command
    ///  line option was supplied.</para>
    ///  <para><c>--test-server</c> should only be specified by developers with
    ///  access to a suitable test server.</para>
    ///  </remarks>
    class function UseTestServer: Boolean; static; inline;
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
  // Delphi
  SysUtils,
  // Project
  UStrUtils;

{ TCommandLineOpts }

class constructor TCommandLineOpts.Create;

  function ParseTestServerCommand: string;
  const
    TestServerSwitch = '--test-server';
    Separator = '=';
  var
    Idx: Integer;
    ParamName: string;
    ParamValue: string;
  begin
    for Idx := 1 to ParamCount do
    begin
      if not StrContainsStr(Separator, ParamStr(Idx)) then
        Continue;
      StrSplit(ParamStr(Idx), Separator, ParamName, ParamValue);
      if not StrSameStr(TestServerSwitch, ParamName) then
        Continue;
      if ParamValue = EmptyStr then
        Continue;
      Exit(ParamValue);
    end;
    Result := EmptyStr;
  end;

begin
  fTestServerHost := ParseTestServerCommand;
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

class function TCommandLineOpts.TestServerHost: string;
begin
  Result := fTestServerHost;
end;

class function TCommandLineOpts.UseTestServer: Boolean;
begin
  Result := fTestServerHost <> EmptyStr;
end;

end.
