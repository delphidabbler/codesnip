{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Custom action used to request editing of a user-defined snippet.
}


unit UEditSnippetAction;


interface


uses
  // Delphi
  Classes,
  // Project
  DB.SnippetIDs;


type
  ///  <summary>Custom action used to request that a snippet is edited.
  ///  </summary>
  TEditSnippetAction = class(TBasicAction)
  strict private
    var
      ///  <summary>Value of ID property.</summary>
      fID: TSnippetID;
  public
    ///  <summary>ID of snippet to be edited.</summary>
    property ID: TSnippetID read fID write fID;
  end;


implementation

end.

