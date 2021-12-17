{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Provides details of various URLs used by CodeSnip.
}


unit UUrl;


interface


type
  ///  <summary>Advanced record that provides various URLs used by the program.
  ///  </summary>
  TURL = record
  strict private
    const
      ///  <summary>URL of DelphiDabbler's account on GitHub repository.
      ///  </summary>
      DDabGitHub = 'https://github.com/delphidabbler';
      ///  <summary>URL of the SWAG database's GitHub repository.</summary>
      SWAGRepo = DDabGitHub + '/swag';
      ///  <summary>URL of the CodeSnip FAQ's GitHub repository.</summary>
      CodeSnipFAQRepo = DDabGitHub + '/codesnip-faq';
  public
    const
      ///  <summary>URL of CodeSnip's GitHub repository.</summary>
      CodeSnipRepo = DDabGitHub + '/codesnip';

      ///  <summary>URL used to view and report CodeSnip bugs on GitHub.
      ///  </summary>
      CodeSnipBugTracker = CodeSnipRepo + '/issues';

      ///  <summary>URL of CodeSnip's FAQ web page.</summary>
      ///  <remarks>This is the CodeSnip FAQ project on GitHub.</remarks>
      CodeSnipFAQReadMe = CodeSnipFAQRepo + '/blob/master/README.md';

      ///  <summary>URL of the Code Snippet database's GitHub repository.
      ///  </summary>
      CodeSnippetsDBRepo = DDabGitHub + '/code-snippets';

      ///  <summary>URL of the GitHub page where DelphiDabbler Code Snippets
      ///  database releases are hosted.</summary>
      CodeSnippetsDBReleases = CodeSnippetsDBRepo + '/releases';

      ///  <summary>URL of the GitHub page where SWAG database releases are
      ///  hosted.</summary>
      SWAGReleases = SWAGRepo + '/releases';

      /// <summary>URL of the the CodeSnip blog.</summary>
      CodeSnipBlog = 'https://codesnip-app.blogspot.com/';

  end;


implementation


end.

