{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2016, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides records and static that provide information about URLs, web services
 * and proxy servers.
}


unit Web.UInfo;


interface


uses
  // Project
  UBaseObjects;


type
  ///  <summary>Record that contains information about a web service.</summary>
  TWebServiceInfo = record
    ///  <summary>Template for web service's URI.</summary>
    ///  <remarks>Must contain a single '%s' placeholder for web host</remarks>
    ScriptURI: string;
    ///  <summary>User agent to be used when accessing web service.</summary>
    UserAgent: string;
    ///  <summary>MIME type of media used by web service.</summary>
    MediaType: string;
    ///  <summary>Constructs a record with given field values.</summary>
    ///  <param name="AScriptURLTplt"><c>string</c> [in] Template for web
    ///  service's URI. Must contain a single '%s' placeholder for web host.
    ///  </param>
    ///  <param name="AUserAgent"><c>string</c> [in] User agent to use when
    ///  accessing web service.</param>
    ///  <param name="AMediaType"><c>string</c> [in] Optional. Media type used
    ///  by web service.</param>
    constructor Create(const AScriptURLTplt, AUserAgent: string;
      const AMediaType: string = 'text/*');
  end;

type
  ///  <summary>Record that contains informtion about a web proxy server.
  ///  </summary>
  TWebProxyInfo = record
    ///  <summary>Whether to use a proxy server.</summary>
    UseProxy: Boolean;
    ///  <summary>IP address of proxy server.</summary>
    IPAddress: string;
    ///  <summary>Proxy server port.</summary>
    Port: Word;
    ///  <summary>User name: optional.</summary>
    UserName: string;
    ///  <summary>Password: optional.</summary>
    Password: string;
  end;

type
  ///  <summary>Static class that provides information about URLs and any proxy
  ///  server used by CodeSnip.</summary>
  TWebInfo = class(TNoConstructObject)
  strict private
    const
      ///  <summary>Name of server that hosts tested and released web services
      ///  that are used by CodeSnip.</summary>
      ///  <remarks>There is also a test server that can be used by CodeSnip
      ///  where new and updated web services are tested - see
      ///  <c>TestServerHost</c> below.</remarks>
      ProductionServerHost = 'delphidabbler.com';
      ///  <summary>URL of DelphiDabbler website.</summary>
      WebsiteURL = 'http://' + ProductionServerHost;
      ///  <summary>Template for URL of Code Snippets news feed.</summary>
      ///  <remarks>'%d' placeholder must be replaced by the required number of
      ///  days into the past the news feed should cover. Passing <c>0</c> as
      ///  the number of days results in all news items being returned.
      ///  </remarks>
      NewsFeedTplt = WebSiteURL + '/feeds/site-news-feed?id=codesnip&days=%d';
  strict private
    ///  <summary>Returns the name of the server that hosts web services that
    ///  are used by CodeSnip.</summary>
    ///  <remarks>By default is the production server (as specified by the
    ///  <c>ProductionServerHost</c> constant). CodeSnip will instead use a
    ///  test server (as returned by the <c>TestServerHost</c> method) if the
    ///  name and port of the test server is passed on the command line via the
    ///  <c>--test-server</c> command line option.</remarks>
    class function Host: string;
  public
    const
      ///  <summary>URL of CodeSnip's GitHub page.</summary>
      GitHubURL = 'https://github.com/delphidabbler/codesnip';
      ///  <summary>URL of home page on DelphiDabbler website.</summary>
      DelphiDabblerHomeURL = WebsiteURL + '/';
      ///  <summary>URL of home page of the CodeSnip project.</summary>
      ProgramHomeURL = WebsiteURL + '/url/codesnip-home';
      ///  <summary>URL of the online Code Snippets database.</summary>
      DatabaseURL = WebsiteURL + '/url/csdb';
      ///  <summary>URL used to make donations towards the CodeSnip project.
      ///  </summary>
      ///  <remarks>This URL redirects to the correct page on PayPal.</remarks>
      DonateURL = 'https://www.paypal.com/donate/?'
        + 'token=LYnh7_DXV-YqTmX3Bilr9rCPN89oANmBTZmRCdNHu_qFSk2jo_'
        + 'WzTYTXCE165U9hXEmwq0&country.x=GB&locale.x=GB';
      ///  <summary>URL used to view and report CodeSnip bugs on GitHub.
      ///  </summary>
      BugTrackerURL = GitHubURL + '/issues';
      ///  <summary>URL of CodeSnip's FAQ web page.</summary>
      ///  <remarks>This is the CodeSnip FAQ project on GitHub.</remarks>
      FAQsURL = 'https://github.com/delphidabbler/codesnip-faq/'
        + 'blob/master/README.md';
  public
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
    ///  <para>The server must be using the <c>http://</c> protocol.</para>
    ///  </remarks>
    class function TestServerHost: string;
    ///  <summary>Builds the URL of the CodeSnip news feed.</summary>
    ///  <param name="Age"><c>Word</c> [in] Maximum age, in days, of news items
    ///  to be included in the feed.</param>
    ///  <returns><c>string</c>. Required URL.</returns>
    class function NewsFeedURL(const Age: Word): string;
    ///  <summary>Builds the URL of a web service.</summary>
    ///  <param name="URLTplt"><c>string</c>. [in] Template of URL of web
    ///  service script. Must contain a '%s' placeholder for host name.</param>
    ///  <returns><c>string</c>. Required URL.</returns>
    class function WebServiceURL(const URLTplt: string): string;
    ///  <summary>Gets information about any required web proxy.</summary>
    ///  <remarks>The web proxy information is read from settings.</remarks>
    class function WebProxyInfo: TWebProxyInfo;
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
    class function UsingTestServer: Boolean;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  USettings, UStrUtils;


{ TWebInfo }

class function TWebInfo.Host: string;
begin
  if UsingTestServer then
    Result := TestServerHost
  else
    Result := ProductionServerHost;
end;

class function TWebInfo.NewsFeedURL(const Age: Word): string;
begin
  Result := Format(NewsFeedTplt, [Age]);
end;

class function TWebInfo.TestServerHost: string;
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

class function TWebInfo.UsingTestServer: Boolean;
begin
  Result := TestServerHost <> EmptyStr;
end;

class function TWebInfo.WebProxyInfo: TWebProxyInfo;
var
  ProxySection: ISettingsSection; // settings section containing proxy info
begin
  ProxySection := Settings.ReadSection(ssProxyServer);
  Result.UseProxy := ProxySection.GetBoolean('UseProxy', False);
  if Result.UseProxy then
  begin
    Result.IPAddress := ProxySection.GetString('IPAddress');
    Result.Port := ProxySection.GetInteger('Port', 80);
    Result.UserName := ProxySection.GetString('UserName');
    Result.Password := ProxySection.GetEncryptedString('Password');
  end;
end;

class function TWebInfo.WebServiceURL(const URLTplt: string): string;
begin
  Assert(StrContainsText('%s', URLTplt),
    ClassName + '.WebServiceURL: URLTplt contains no host placeholder');
  Result := Format(URLTplt, [Host]);
end;

{ TWebServiceInfo }

constructor TWebServiceInfo.Create(const AScriptURLTplt, AUserAgent,
  AMediaType: string);
begin
  ScriptURI := TWebInfo.WebServiceURL(AScriptURLTplt); // URL of script on host
  UserAgent := AUserAgent;
  MediaType := AMediaType;
end;

end.

