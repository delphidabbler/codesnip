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


type
  ///  <summary>Static class that provides information about URLs and any proxy
  ///  server used by CodeSnip.</summary>
  TWebInfo = record
  strict private
    const
      ///  <summary>URL of CodeSnip's project page on GitHub.</summary>
      GitHubSiteURL = 'https://github.com/delphidabbler/codesnip';
  public
    const
      ///  <summary>URL of home page of the CodeSnip project on GitHub.
      ///  </summary>
      ProgramHomeURL = GitHubSiteURL;

      /// <summary>URL of the CodeSnip blog</summary>
      ProgramBlogURL = 'http://codesnip-app.blogspot.com/';

      ///  <summary>URL of the online Code Snippets database</summary>
      // TODO -cWeb: Add DatabaseURL const with URL of online Code Snippets Database on GitHub when made public ??

      ///  <summary>URL used to view and report CodeSnip bugs</summary>
      BugTrackerURL = GitHubSiteURL + '/issues';

      ///  <summary>URL of CodeSnip's FAQ web page.</summary>
      // TODO -cWeb: Add FAQsURL const with URL of FAQs if they moved to GitHub
  end;


implementation


end.

