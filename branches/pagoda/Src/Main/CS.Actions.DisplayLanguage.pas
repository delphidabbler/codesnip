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
 * Implements a custom action that requests display of a source code language.
}


unit CS.Actions.DisplayLanguage;


interface


uses
  // Delphi
  Classes,
  // Project
  CS.SourceCode.Languages,
  IntfNotifier;


type
  ///  <summary>Custom action that requests display of a source code language.
  ///  </summary>
  TDisplayLanguageAction = class(TBasicAction, ISetNotifier)
  strict private
    var
      ///  <summary>Value of Tag property.</summary>
      fLanguageID: TSourceCodeLanguageID;
      ///  <summary>Value of NewTab property.</summary>
      fNewTab: Boolean;
      ///  <summary>Reference to Notifier object.</summary>
      fNotifier: INotifier;
  public
    ///  <summary>Performs the action by displaying the source code language in
    ///  the main display.</summary>
    ///  <remarks>
    ///  <para>Notifier object is used to cause the language to be displayed.
    ///  </para>
    ///  <para>Any OnExecute event handler is ignored.</para>
    ///  </remarks>
    ///  <returns>Boolean. False to indicate OnExecute event handler not called.
    ///  </returns>
    function Execute: Boolean; override;
    ///  <summary>Stores reference to given notifier object.</summary>
    ///  <remarks>Implements ISetNotifier.SetNotifier</remarks>
    procedure SetNotifier(const Notifier: INotifier);
    ///  <summary>ID of language to be displayed.</summary>
    property LanguageID: TSourceCodeLanguageID read fLanguageID
      write fLanguageID;
    ///  <summary>Flag indicating if language is to be displayed in a new detail
    ///  pane tab.</summary>
    property NewTab: Boolean read fNewTab write fNewTab;
  end;


implementation


uses
  // Project
  CS.Config,
  UView;


{ TDisplayLanguageAction }

function TDisplayLanguageAction.Execute: Boolean;
begin
  Assert(Assigned(fNotifier), ClassName + '.Execute: Notifier not set');
  // Create a view item for tag and get notifier to display it
  fNotifier.ShowViewItem(
    TViewFactory.CreateSourceCodeLanguageView(
      TConfig.Instance.SourceCodeLanguages[LanguageID]
    ),
    NewTab
  );
  Result := False;
end;

procedure TDisplayLanguageAction.SetNotifier(const Notifier: INotifier);
begin
  fNotifier := Notifier;
end;

end.
