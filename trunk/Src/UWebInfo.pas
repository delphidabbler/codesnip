{
 * UWebInfo.pas
 *
 * Static class that provides information about various web used by CodeSnip,
 * along with a record that defines a web service.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is UWebInfo.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UWebInfo;


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
    constructor Create(const AScriptName, AUserAgent: string;
      const AMediaType: string = 'text/*');
      {Record constructor. Sets all record fields.
        @param AScriptName [in] Name of script on web server. Converted into
          full URI.
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
    Static class that provides information about various web used by CodeSnip.
  }
  TWebInfo = class(TNoConstructObject)
  strict private
    const LocalHost = 'localhost';              // local web server (for tests)
    const RemoteHost = 'www.delphidabbler.com'; // remote web server
    const WebsiteURL = 'http://' + RemoteHost;  // delphidabbler website URL
    class function Host: string;
      {Determines host server depending on command line switch passed to program.
        @return Required host server. If -localhost switch provided this is
        localhost, otherwise it is the remote server.
      }
  public
    const DelphiDabblerHomeURL = WebsiteURL + '/';
      {DelphiDabbler site home page}
    const ProgramHomeURL = WebsiteURL + '/software/codesnip';
      {CodeSnip's home page on DelphiDabbler.com}
    const DatabaseURL = WebsiteURL + '/codesnip';
      {Online Code Snippets database on DelphiDabbler.com}
    const ContactPageURL = WebsiteURL + '/contact';
      {Contact page on DelphiDabbler.com}
    const DonateURL = WebsiteURL + '/url/donate-cs';
      {Donation page. Redirects to required page to permit URLs to change
      without modifying this code}
    class function WebServiceURL(const Script: string): string;
      {Builds the URL of a webservice.
        @param Script [in] Base name of web service script.
        @return URL of required script on active host.
      }
    class function WebProxyInfo: TWebProxyInfo;
      {Gets information about any web proxy to be used from settings.
        @return Required information.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  USettings;


{ TWebInfo }

class function TWebInfo.Host: string;
  {Determines host server depending on command line switch passed to program.
    @return Required host server. If -localhost switch provided this is
    localhost, otherwise it is the remote server.
  }
begin
  if FindCmdLineSwitch('localhost', ['-', '\'], True) then
    Result := LocalHost
  else
    Result := RemoteHost;
end;

class function TWebInfo.WebProxyInfo: TWebProxyInfo;
  {Gets information about any web proxy to be used from settings.
    @return Required information.
  }
var
  ProxySection: ISettingsSection; // settings section containing proxy info
begin
  ProxySection := Settings.ReadSection(ssProxyServer);
  Result.UseProxy := Boolean(
    StrToIntDef(ProxySection.ItemValues['UseProxy'], 0)
  );
  if Result.UseProxy then
  begin
    Result.IPAddress := ProxySection.ItemValues['IPAddress'];
    Result.Port := StrToIntDef(ProxySection.ItemValues['Port'], 80);
    Result.UserName := ProxySection.ItemValues['UserName'];
    Result.Password := ProxySection.GetEncryptedItemValue('Password');
  end;
end;

class function TWebInfo.WebServiceURL(const Script: string): string;
  {Builds the URL of a webservice.
    @param Script [in] Base name of web service script.
    @return URL of required script on active host.
  }
begin
  Result := Format('http://%0:s/websvc/%1:s', [Host, Script]);
end;

{ TWebServiceInfo }

constructor TWebServiceInfo.Create(const AScriptName, AUserAgent,
  AMediaType: string);
  {Record constructor. Sets all record fields.
    @param AScriptName [in] Name of script on web server. Converted into
      full URI.
    @param AUserAgent [in] User agent string.
    @param AMediaType [in] MIME type of media used.
  }
begin
  ScriptURI := TWebInfo.WebServiceURL(AScriptName); // URL of script on host
  UserAgent := AUserAgent;
  MediaType := AMediaType;
end;

end.

