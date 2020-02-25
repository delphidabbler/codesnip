{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Provides records and static that provide information about URLs, web services
 * and proxy servers.
}


unit UUrl;


interface


type
  ///  <summary>Advanced record that provides various URLs used by the program.
  ///  </summary>
  TURL = record
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
  public
    const
      ///  <summary>URL of CodeSnip's GitHub page.</summary>
      GitHubURL = 'https://github.com/delphidabbler/codesnip';
      ///  <summary>URL of the GitHub page where DelphiDabbler Code Snippets
      ///  database releases are hosted.</summary>
      CSDBReleaseURL = 'https://github.com/'
        + 'delphidabbler/code-snippets/releases';
      ///  <summary>URL of the GitHub page where SWAG database releases are
      ///  hosted.</summary>
      SWAGReleaseURL = 'https://github.com/delphidabbler/swag/releases';
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
      /// <summary>URL of the the CodeSnip blog.</summary>
      BlogURL = 'http://codesnip-app.blogspot.com/';
  end;


implementation


end.

