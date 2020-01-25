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
      ///  <summary>URL of CodeSnip's project page on GitHub.</summary>
      GitHubSiteURL = 'https://github.com/delphidabbler/codesnip';
      ///  <summary>Name of server that hosts tested and released web services
      ///  that are used by CodeSnip.</summary>
      ///  <remarks>There is also a test server that can be used by CodeSnip
      ///  where new and updated web services are tested - see
      ///  <c>TestServerHost</c> below.</remarks>
      ProductionServerHost = 'delphidabbler.com';   // TODO -cwebsvc : remove this
      ///  <summary>URL of DelphiDabbler website.</summary>
      WebsiteURL = 'http://' + ProductionServerHost;  // TODO -cwebsvc : remove this
  strict private
    ///  <summary>Returns the name of the server that hosts web services that
    ///  are used by CodeSnip.</summary>
    ///  <remarks>By default is the production server (as specified by the
    ///  <c>ProductionServerHost</c> constant). CodeSnip will instead use a
    ///  test server (as returned by the <c>TestServerHost</c> method) if the
    ///  name and port of the test server is passed on the command line via the
    ///  <c>--test-server</c> command line option.</remarks>
    class function Host: string;                         // TODO -cwebsvc: remove this
  public
    const
      ///  <summary>URL of home page of the CodeSnip project.</summary>
      ProgramHomeURL = GitHubSiteURL;
      /// <summary>URL of the CodeSnip blog</summary>
      ProgramBlogURL = 'http://codesnip-app.blogspot.com/';
      ///  <summary>URL of the online Code Snippets database.</summary>
      DatabaseURL = WebsiteURL + '/url/csdb';  // TODO -cwebsvc -cQuery: replace with URL of DDab CS DB on GitHub ??
      ///  <summary>URL used to view and report CodeSnip bugs.</summary>
      ///  <remarks>This URL will redirect to the actual bug tracker which may
      ///  be on an external site such as SourceForge or GitHub.</remarks>
      BugTrackerURL = GitHubSiteURL + '/issues';
      ///  <summary>URL of CodeSnip's FAQ web page.</summary>
      ///  <remarks>This URL will redirect to the FAQ location which may be on
      ///  an external site such as GitHub or SourceForge.</remarks>
      FAQsURL = WebsiteURL + '/url/codesnip-faq'; // TODO -cwebsvc -cQuery: replace with URL of FAQs if they moved to GitHub
  public
    ///  <summary>Builds the URL of a web service.</summary>
    ///  <param name="URLTplt"><c>string</c>. [in] Template of URL of web
    ///  service script. Must contain a '%s' placeholder for host name.</param>
    ///  <returns><c>string</c>. Required URL.</returns>       // TODO -cwebsvc: remove this
    class function WebServiceURL(const URLTplt: string): string;
    ///  <summary>Gets information about any required web proxy.</summary>
    ///  <remarks>The web proxy information is read from settings.</remarks>
    class function WebProxyInfo: TWebProxyInfo;               // TODO -cwebsvc: remove this
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  CS.Init.CommandLineOpts,
  USettings,
  UStrUtils;


{ TWebInfo }

class function TWebInfo.Host: string;
begin
  if TCommandLineOpts.UseTestServer then
    Result := TCommandLineOpts.TestServerHost
  else
    Result := ProductionServerHost;
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

