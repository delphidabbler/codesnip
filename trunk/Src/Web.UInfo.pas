{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Record that provides information about URLs, web services and proxy servers.
}


unit Web.UInfo;


interface


uses
  // Project
  UBaseObjects;


type

  {
  TWebServiceInfo:
    Record that provides information about a web service.
  }
  TWebServiceInfo = record
    ScriptURI: string;  // URI of web service script
    UserAgent: string;  // User agent string required by web service
    MediaType: string;  // MIME type of media used
    constructor Create(const AScriptURLTplt, AUserAgent: string;
      const AMediaType: string = 'text/*');
      {Record constructor. Sets all record fields.
        @param AScriptURLTplt [in] Template of web service script name. Must
          contain "%s" as a placeholder for host name.
        @param AUserAgent [in] User agent string.
        @param AMediaType [in] MIME type of media used.
      }
  end;

  {
  TWebProxyInfo:
    Record that provides informtion about a web proxy
  }
  TWebProxyInfo = record
    UseProxy: Boolean;  // Whether to use a proxy server
    IPAddress: string;  // IP address of server
    Port: Word;         // Port of proxy server
    UserName: string;   // Optional user name
    Password: string;   // Optional password
  end;

  {
  TWebInfo:
    Static class that provides information about URLs and any proxy server used
    by CodeSnip.
  }
  TWebInfo = class(TNoConstructObject)
  strict private
    const RemoteHost = 'www.delphidabbler.com'; // remote web server
    const WebsiteURL = 'http://' + RemoteHost;  // delphidabbler website URL
    const NewsFeedTplt = WebSiteURL +           // news feed url template
      '/feeds/site-news-feed?id=codesnip&days=%d';
    class function Host: string;
      {Determines the host server. The server depends on whether the -localhost
      switch was passed on the command line.
        @return Required host server, either localhost or the remote server.
      }
  public
    const LocalHost = 'localhost';
      {Local web server (for tests)}
    const DelphiDabblerHomeURL = WebsiteURL + '/';
      {DelphiDabbler site home page}
    const ProgramHomeURL = WebsiteURL + '/url/codesnip-home';
      {CodeSnip's home page on DelphiDabbler.com}
    const ProgramDownloadURL = WebsiteURL + '/url/codesnip-download';
      {CodeSnip's download page of DelphiDabbler.com}
    const DatabaseURL = WebsiteURL + '/url/csdb';
      {Online Code Snippets database on DelphiDabbler.com}
    const ContactPageURL = WebsiteURL + '/contact';
      {Contact page on DelphiDabbler.com}
    const DonateURL = WebsiteURL + '/url/donate-cs';
      {Donation page. Redirects to required page to permit URLs to change
      without modifying this code}
    const BugTrackerURL = WebsiteURL + '/url/codesnip-bugs';
      {Bug tracker page}
    const FAQsURL = WebsiteURL + '/url/codesnip-faq';
      {CodeSnip FAQs page}
    class function NewsFeedURL(const Age: Word): string;
      {Gets the URL of the CodeSnip news feed .
        @param Age [in] Maximum age of included news items in days.
        @return Required URL.
      }
    class function WebServiceURL(const URLTplt: string): string;
      {Builds the URL of a webservice.
        @param URLTplt [in] Template of URL of web service script. Must contain
          "%s" as a placeholder for host name.
        @return URL of required script on active host.
      }
    class function WebProxyInfo: TWebProxyInfo;
      {Gets information about any web proxy to be used from settings.
        @return Required information.
      }
    class function UsingLocalHost: Boolean;
      {Checks if program is using localhost web server.
        @return True if localhost being used, False if not.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  USettings, UStrUtils;


{ TWebInfo }

class function TWebInfo.Host: string;
  {Determines the host server. The server depends on whether the -localhost
  switch was passed on the command line.
    @return Required host server, either localhost or the remote server.
  }
begin
  if UsingLocalHost then
    Result := LocalHost
  else
    Result := RemoteHost;
end;

class function TWebInfo.NewsFeedURL(const Age: Word): string;
  {Gets the URL of the CodeSnip news feed .
    @param Age [in] Maximum age of included news items in days.
    @return Required URL.
  }
begin
  Result := Format(NewsFeedTplt, [Age]);
end;

class function TWebInfo.UsingLocalHost: Boolean;
  {Checks if program is using localhost web server.
    @return True if localhost being used, False if not.
  }
begin
  // We are using local host if -localhost (or /localhost) command line switch
  // provided.
  Result := FindCmdLineSwitch('localhost', True);
end;

class function TWebInfo.WebProxyInfo: TWebProxyInfo;
  {Gets information about any web proxy to be used from settings.
    @return Required information.
  }
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
  {Builds the URL of a webservice.
    @param URLTplt [in] Template of URL of web service script. Must contain "%s"
      as a placeholder for host name.
    @return URL of required script on active host.
  }
begin
  Assert(StrContainsText('%s', URLTplt),
    ClassName + '.WebServiceURL: URLTplt contains no host placeholder');
  Result := Format(URLTplt, [Host]);
end;

{ TWebServiceInfo }

constructor TWebServiceInfo.Create(const AScriptURLTplt, AUserAgent,
  AMediaType: string);
  {Record constructor. Sets all record fields.
    @param AScriptURLTplt [in] Template of web service script name. Must contain
      "%s" as a placeholder for host name.
    @param AScriptName [in] Name of script on web server. Converted into
      full URI.
    @param AUserAgent [in] User agent string.
    @param AMediaType [in] MIME type of media used.
  }
begin
  ScriptURI := TWebInfo.WebServiceURL(AScriptURLTplt); // URL of script on host
  UserAgent := AUserAgent;
  MediaType := AMediaType;
end;

end.

