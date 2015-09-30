{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a custom action used to request deletion of a tag from a snippet's
 * tag list.
}


unit CS.Actions.RemoveTag;


interface


uses
  // Delphi
  Classes,
  // Project
  CS.Database.Types;


type
  ///  <summary>Custom action used to request deletion of a tag from a snippet's
  ///  tag list.</summary>
  TRemoveTagAction = class(TBasicAction)
  strict private
    var
      ///  <summary>Value of SnippetID property.</summary>
      fSnippetID: TSnippetID;
      ///  <summary>Value of Tag property.</summary>
      fTag: TTag;
  public
    ///  <summary>ID of snippet from which tag is to be removed.</summary>
    property SnippetID: TSnippetID read fSnippetID write fSnippetID;
    ///  <summary>Tag to be removed from snippet.</summary>
    property Tag: TTag read fTag write fTag;
  end;


implementation

end.
