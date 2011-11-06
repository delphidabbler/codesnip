{
 * USnippetAction.pas
 *
 * Custom action used to request display of a snippet by name.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is USnippetAction.pas, formerly URoutineAction.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit USnippetAction;


interface


uses
  // Delphi
  Classes,
  // Project
  IntfNotifier;


type

  ///  <summary>
  ///  Custom action used to request display of a snippet.
  ///  </summary>
  ///  <remarks>
  ///  Required snippet is uniquely identified by its name and whether it is
  ///  user defined or not.
  ///  </remarks>
  TSnippetAction = class(TBasicAction, ISetNotifier)
  strict private
    var
      ///  <summary>Value of SnippetName property.</summary>
      fSnippetName: string;
      ///  <summary>Value of UserDefined property.</summary>
      fUserDefined: Boolean;
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
    function Execute: Boolean; override;
    ///  <summary>Stores reference to given notifier object.</summary>
    ///  <remarks>Implements ISetNotifier.SetNotifier</remarks>
    procedure SetNotifier(const Notifier: INotifier);
    ///  <summary>Name of snippet to be displayed.</summary>
    property SnippetName: string read fSnippetName write fSnippetName;
    ///  <summary>Flag indicating whether snippet to be displayed is user
    ///  defined.</summary>
    property UserDefined: Boolean read fUserDefined write fUserDefined;
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
  Assert(SnippetName <> '', ClassName + '.Execute: SnippetName not provided');
  Snippet := Database.Snippets.Find(SnippetName, UserDefined);
  Assert(Assigned(Snippet), ClassName + '.Execute: SnippetName not valid');
  // Create a view item for snippet and get notifier to display it
  // TODO: change TSnippetAction to allow for NewTab property??
  fNotifier.ShowViewItem(TViewItemFactory.CreateSnippetView(Snippet), False);
  Result := False;
end;

procedure TSnippetAction.SetNotifier(const Notifier: INotifier);
begin
  fNotifier := Notifier;
end;

end.

