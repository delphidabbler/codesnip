{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Custom action used to request editing of a named user-defined snippet.
}


unit UEditSnippetAction;


interface


uses
  // Delphi
  Classes;


type
  ///  <summary>
  ///  Custom action used to request that a named user defined snippet is
  ///  edited.
  ///  </summary>
  TEditSnippetAction = class(TBasicAction)
  strict private
    var
      ///  <summary>Value of SnippetName property.</summary>
      fSnippetName: string;
  public
    ///  <summary>Name of snippet to be edited.</summary>
    property SnippetName: string read fSnippetName write fSnippetName;
  end;


implementation

end.

