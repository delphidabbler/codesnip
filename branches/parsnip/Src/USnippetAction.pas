{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Custom action used to request display of a snippet by name.
}


unit USnippetAction;


interface


uses
  // Delphi
  Classes,
  // Project
  IntfNotifier,
  USnippetIDs;


type

  ///  <summary>Custom action used to request display of a snippet.</summary>
  ///  <remarks>Required snippet is uniquely identified by its ID.</remarks>
  TSnippetAction = class(TBasicAction, ISetNotifier)
  strict private
    var
      ///  <summary>Value of SnippetID property.</summary>
      fSnippetID: TSnippetID;
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
    ///  <summary>ID of snippet to be displayed.</summary>
    property SnippetID: TSnippetID read fSnippetID write fSnippetID;
    ///  <summary>Flag indicating if snippet is to be displayed in new detail
    ///  pane tab.</summary>
    property NewTab: Boolean read fNewTab write fNewTab;
  end;


implementation


uses
  // Project
  DB.UMain, DB.USnippet, UView;


{ TSnippetAction }

function TSnippetAction.Execute: Boolean;
var
  Snippet: TSnippet;    // snippet to be displayed
begin
  Assert(Assigned(fNotifier), ClassName + '.Execute: Notifier not set');
  Snippet := Database._Snippets.Find(SnippetID);
  Assert(Assigned(Snippet), ClassName + '.Execute: SnippetName not valid');
  // Create a view item for snippet and get notifier to display it
  fNotifier.ShowViewItem(TViewFactory.CreateSnippetView(Snippet), NewTab);
  Result := False;
end;

procedure TSnippetAction.SetNotifier(const Notifier: INotifier);
begin
  fNotifier := Notifier;
end;

end.

