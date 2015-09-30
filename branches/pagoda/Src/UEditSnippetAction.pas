{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Custom action used to request editing of a specfied snippet.
}


unit UEditSnippetAction;


interface


uses
  // Delphi
  Classes,
  // Project
  CS.Database.Types;


type
  ///  <summary>Custom action used to request that a specified snippet is
  ///  edited.</summary>
  TEditSnippetAction = class(TBasicAction)
  strict private
    var
      ///  <summary>Value of SnippetID property.</summary>
      fSnippetID: TSnippetID;
  public
    ///  <summary>ID of snippet to be edited.</summary>
    property SnippetID: TSnippetID read fSnippetID write fSnippetID;
  end;


implementation

end.

