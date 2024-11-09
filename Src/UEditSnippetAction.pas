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
  Classes;


{TODO -cCollections: Add a collection ID property, or change Key property to ID of
        type TSnippetID.}

type
  ///  <summary>
  ///  Custom action used to request that a user defined snippet is edited.
  ///  </summary>
  TEditSnippetAction = class(TBasicAction)
  strict private
    var
      ///  <summary>Value of Key property.</summary>
      fKey: string;
  public
    ///  <summary>Key of snippet to be edited.</summary>
    property Key: string read fKey write fKey;
  end;


implementation

end.

