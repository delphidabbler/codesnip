{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Custom action used to request display of a snippet by key and vault ID.
}


unit USnippetAction;


interface


uses
  // Delphi
  Classes,
  // Project
  DB.Vaults,
  IntfNotifier;


{TODO -cVault: Combine the two TSnippetAction properties into a single
        TSnippetID property.}

type

  ///  <summary>
  ///  Custom action used to request display of a snippet.
  ///  </summary>
  ///  <remarks>
  ///  Required snippet is uniquely identified by its key and vault ID.
  ///  </remarks>
  TSnippetAction = class(TBasicAction, ISetNotifier)
  strict private
    var
      ///  <summary>Value of Key property.</summary>
      fKey: string;
      ///  <summary>Value of VaultID property.</summary>
      fVaultID: TVaultID;
      ///  <summary>Value of NewTab property.</summary>
      fNewTab: Boolean;
      ///  <summary>Reference to Notifier object.</summary>
      fNotifier: INotifier;
  public
    ///  <summary>Performs the action by displaying a snippet in the main
    ///  display.</summary>
    ///  <remarks>
    ///  <para>Notifier object is used to cause the snippet to be displayed.
    ///  </para>
    ///  <para>Any OnExecute event handler is ignored.</para>
    ///  </remarks>
    ///  <returns>Boolean. False to indicate OnExecute event handler not called.
    ///  </returns>
    function Execute: Boolean; override;
    ///  <summary>Stores reference to given notifier object.</summary>
    ///  <remarks>Implements ISetNotifier.SetNotifier</remarks>
    procedure SetNotifier(const Notifier: INotifier);
    ///  <summary>Key of snippet to be displayed.</summary>
    property Key: string read fKey write fKey;
    ///  <summary>ID of the vault containing the snippet to be displayed.
    ///  </summary>
    property VaultID: TVaultID read fVaultID write fVaultID;
    ///  <summary>Flag indicating if snippet is to be displayed in a new detail
    ///  pane tab.</summary>
    property NewTab: Boolean read fNewTab write fNewTab;
  end;


implementation


uses
  // Project
  DB.Main,
  DB.Snippets,
  UView;


{ TSnippetAction }

function TSnippetAction.Execute: Boolean;
var
  Snippet: TSnippet;    // snippet to be displayed
begin
  Assert(Assigned(fNotifier), ClassName + '.Execute: Notifier not set');
  Assert(Key <> '', ClassName + '.Execute: Key not provided');

  Snippet := Database.Snippets.Find(Key, fVaultID);
  Assert(Assigned(Snippet), ClassName + '.Execute: Key not valid');

  // Create a view item for snippet and get notifier to display it
  fNotifier.ShowViewItem(TViewFactory.CreateSnippetView(Snippet), NewTab);
  Result := False;
end;

procedure TSnippetAction.SetNotifier(const Notifier: INotifier);
begin
  fNotifier := Notifier;
end;

end.

