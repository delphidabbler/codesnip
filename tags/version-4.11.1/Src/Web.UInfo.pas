{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2014, Peter Johnson (www.delphidabbler.com).
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
    ///  <param name="AScriptURLTplt">string [in] Template for web service's
    ///  URI.</param>
    ///  <param name="AUserAgent">string [in] User agent to use when accessing
    ///  web service.</param>
    ///  <param name="AMediaType">string [in] Optional. Media type used by web
    ///  service.</param>
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
      ///  <summary>Remote DelphiDabbler web server.</summary>
      RemoteHost = 'delphidabbler.com';
      ///  <summary>URL of DelphiDabbler website.</summary>
      WebsiteURL = 'http://' + RemoteHost;
      ///  <summary>Template for URL of Code Snippets news feed.</summary>
      ///  <remarks>'%d' placeholder must be replaced by the required number of
      ///  days into the past the news feed should cover.</remarks>
      NewsFeedTplt = WebSiteURL + '/feeds/site-news-feed?id=codesnip&days=%d';
  strict private
    ///  <summary>Returns the name of the host server to be used.</summary>
    ///  <remarks>This is the remote web server unless the '-localhost# switch
    ///  was passed on the command line when localhost server is returned.
    ///  </remarks>
    class function Host: string;
  public
    const
      ///  <summary>Local web server.</summary>
      ///  <remarks>Used for test purposes.</remarks>
      LocalHost = 'localhost:8080';
      ///  <summary>URL of home page on DelphiDabbler website.</summary>
      DelphiDabblerHomeURL = WebsiteURL + '/';
      ///  <summary>URL of home page of the CodeSnip project.</summary>
      ProgramHomeURL = WebsiteURL + '/url/codesnip-home';
      ///  <summary>URL of the online Code Snippets database.</summary>
      DatabaseURL = WebsiteURL + '/url/csdb';
      ///  <summary>URL used to make donations towards the CodeSnip project.
      ///  </summary>
      ///  <summary>This URL redirects to the correct page on PayPal.</summary>
      DonateURL = WebsiteURL + '/url/donate-cs';
      ///  <summary>URL used to view and report CodeSnip bugs.</summary>
      BugTrackerURL = WebsiteURL + '/url/codesnip-bugs';
      ///  <summary>URL of CodeSnip's FAQ web page.</summary>
      FAQsURL = WebsiteURL + '/url/codesnip-faq';
  public
    ///  <summary>Builds the URL of the CodeSnip news feed.</summary>
    ///  <param name="Age">Word [in] Maximum age, in days, of news items to be
    ///  included in the feed.</param>
    ///  <returns>string. Required URL.</returns>
    class function NewsFeedURL(const Age: Word): string;
    ///  <summary>Builds the URL of a web service.</summary>
    ///  <param name="URLTplt">string. [in] Template of URL of web service
    ///  script. Must contain a '%s' placeholder for host name.</param>
    ///  <returns>string. Required URL.</returns>
    class function WebServiceURL(const URLTplt: string): string;
    ///  <summary>Gets information about any required web proxy.</summary>
    ///  <remarks>The web proxy information is read from settings.</remarks>
    class function WebProxyInfo: TWebProxyInfo;
    ///  <summary>Checks if the program is using the web server on localhost.
    ///  </summary>
    ///  <returns>Boolean. True if localhost is being used, False if the remote,
    ///  production, server is being used.</returns>
    ///  <remarks>
    ///  <para>True is returned iff the '-localhost' switch was passed on the
    ///  command line.</para>
    ///  <para>Localhost should only be used by developers with access to a
    ///  suitable test server running as 'locahost'.</para>
    ///  </remarks>
    class function UsingLocalHost: Boolean;
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
  if UsingLocalHost then
    Result := LocalHost
  else
    Result := RemoteHost;
end;

class function TWebInfo.NewsFeedURL(const Age: Word): string;
begin
  Result := Format(NewsFeedTplt, [Age]);
end;

class function TWebInfo.UsingLocalHost: Boolean;
begin
  Result := FindCmdLineSwitch('localhost', True);
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

