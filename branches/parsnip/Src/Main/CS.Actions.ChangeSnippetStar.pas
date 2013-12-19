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
 * Implements a custom action that changes or sets the value of a snippet's
 * Starred property.
}


unit CS.Actions.ChangeSnippetStar;

interface

uses
  // Delphi
  Classes,
  // Project
  CS.Database.Types,
  UUserDBMgr;

type
  ///  <summary>Custom action that changes or sets the value of a snippet's
  ///  Starred property.</summary>
  TChangeSnippetStarAction = class(TBasicAction)
  strict private
    var
      ///  <summary>Value of SnippetID property.</summary>
      fSnippetID: TSnippetID;
      ///  <summary>Value of State property.</summary>
      fState: Boolean;
  public
    ///  <summary>Constructs new instance of the action, with the given owner.
    ///  </summary>
    constructor Create(AOwner: TComponent); override;
    ///  <summary>Performs the action by updating the state of the required
    ///  snippet's Starred property.
    ///  </summary>
    ///  <remarks>Any OnExecute event handler is ignored.</remarks>
    ///  <returns>Boolean. False to indicate OnExecute event handler not called.
    ///  </returns>
    function Execute: Boolean; override;
    ///  <summary>ID of snippet to be updated.</summary>
    property SnippetID: TSnippetID read fSnippetID write fSnippetID;
    ///  <summary>Flag indicating new state of snippet's Starred property.
    ///  </summary>
    property State: Boolean read fState write fState;
  end;

implementation

{ TChangeSnippetStarAction }

constructor TChangeSnippetStarAction.Create(AOwner: TComponent);
begin
  inherited;
  fSnippetID := TSnippetID.CreateNull;
end;

function TChangeSnippetStarAction.Execute: Boolean;
begin
  Assert(not fSnippetID.IsNull, ClassName + '.Execute: SnippetID is null');
  TDBModificationMgr.UpdateSnippetStarredState(fSnippetID, State);
  Result := False;
end;

end.
