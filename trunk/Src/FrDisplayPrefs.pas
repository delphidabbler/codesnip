{
 * FrGeneralPrefs.pas
 *
 * Implements a frame that allows user to set general application preferences.
 * Designed for use as one of the tabs in the preferences dialog box.
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
 * The Original Code is FrGeneralPrefs.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FrDisplayPrefs;


interface


uses
  // Delphi
  Controls, StdCtrls, Classes,
  // Project
  FrPrefsBase, UMeasurement, UPreferences;


type

  {
  TGeneralPrefsFrame:
    Frame that allows user to set general application preferences. Can persist
    preferences entered by user. Note: Designed for use in preferences dialog
    box.
  }
  TDisplayPrefsFrame = class(TPrefsBaseFrame)
    lblOverviewTree: TLabel;
    cbOverviewTree: TComboBox;
    chkHideEmptySections: TCheckBox;
    chkSnippetsInNewTab: TCheckBox;
    procedure chkHideEmptySectionsClick(Sender: TObject);
  strict private
    var
      ///  <summary>Flag indicating if changes affect UI.</summary>
      fUIChanged: Boolean;
    procedure SelectOverviewTreeState(const State: TOverviewStartState);
      {Selects combo box item associated with a overview treeview startup state.
        @param State [in] Startup state to be selected.
      }
    function OverviewTreeStateDesc(const State: TOverviewStartState): string;
      {Gets description of an overview treeview startup state.
        @param State [in] State for which description is required.
        @return Required description.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up frame and populates controls.
        @param AOwner [in] Component that owns frame.
      }
    procedure Activate(const Prefs: IPreferences); override;
      {Called when page activated. Updates controls.
        @param Prefs [in] Object that provides info used to update controls.
      }
    procedure Deactivate(const Prefs: IPreferences); override;
      {Called when page is deactivated. Stores information entered by user.
        @param Prefs [in] Object used to store information.
      }
    ///  <summary>Checks if preference changes require that main window UI is
    ///  updated.</summary>
    ///  <remarks>Called when dialog box containing frame is closing.</remarks>
    function UIUpdated: Boolean; override;
    procedure ArrangeControls; override;
      {Arranges controls on frame. Called after frame has been sized.
      }
    function DisplayName: string; override;
      {Caption that is displayed in the tab sheet that contains this frame when
      displayed in the preference dialog box.
        @return Required display name.
      }
    class function Index: Byte; override;
      {Index number that determines the location of the tab containing this
      frame when displayed in the preferences dialog box.
        @return Required index number.
      }
  end;


implementation


uses
  // Delphi
  Math,
  // Project
  FmPreferencesDlg, UCtrlArranger, UGraphicUtils;


{$R *.dfm}


{ TGeneralPrefsFrame }

procedure TDisplayPrefsFrame.Activate(const Prefs: IPreferences);
  {Called when page activated. Updates controls.
    @param Prefs [in] Object that provides info used to update controls.
  }
begin
  SelectOverviewTreeState(Prefs.OverviewStartState);
  chkHideEmptySections.Checked := not Prefs.ShowEmptySections;
  chkSnippetsInNewTab.Checked := Prefs.ShowNewSnippetsInNewTabs;
end;

procedure TDisplayPrefsFrame.ArrangeControls;
  {Arranges controls on frame. Called after frame has been sized.
  }
begin
  TCtrlArranger.AlignLefts(
    [lblOverviewTree, chkHideEmptySections, chkSnippetsInNewTab], 0
  );
  TCtrlArranger.MoveToRightOf(lblOverviewTree, cbOverviewTree, 8);
  chkHideEmptySections.Width := Self.Width - 16;
  chkSnippetsInNewTab.Width := Self.Width - 16;
  TCtrlArranger.AlignVCentres(0, [lblOverviewTree, cbOverviewTree]);
  TCtrlArranger.MoveBelow(
    [lblOverviewTree, cbOverviewTree], chkHideEmptySections, 8
  );
  TCtrlArranger.MoveBelow(chkHideEmptySections, chkSnippetsInNewTab, 8);
end;

procedure TDisplayPrefsFrame.chkHideEmptySectionsClick(Sender: TObject);
  {Handles clicks on "Hide Empty Sections" check box. Flags UI preferences has
  having changed.
    @param Sender [in] Ignored.
  }
begin
  fUIChanged := True;
end;

constructor TDisplayPrefsFrame.Create(AOwner: TComponent);
  {Class constructor. Sets up frame and populates controls.
    @param AOwner [in] Component that owns frame.
  }
var
  OTStateIdx: TOverviewStartState;  // loops thru each overview tree start state
begin
  inherited;
  HelpKeyword := 'DisplayPrefs';
  // Populate overview tree start state
  for OTStateIdx := Low(TOverviewStartState) to High(TOverviewStartState) do
    cbOverviewTree.Items.AddObject(
      OverviewTreeStateDesc(OTStateIdx), TObject(OTStateIdx)
    );
end;

procedure TDisplayPrefsFrame.Deactivate(const Prefs: IPreferences);
  {Called when page is deactivated. Stores information entered by user.
    @param Prefs [in] Object used to store information.
  }
begin
  Prefs.ShowNewSnippetsInNewTabs := chkSnippetsInNewTab.Checked;
  Prefs.ShowEmptySections := not chkHideEmptySections.Checked;
  Prefs.OverviewStartState := TOverviewStartState(
    cbOverviewTree.Items.Objects[cbOverviewTree.ItemIndex]
  );
end;

function TDisplayPrefsFrame.DisplayName: string;
  {Caption that is displayed in the tab sheet that contains this frame when
  displayed in the preference dialog box.
    @return Required display name.
  }
resourcestring
  sDisplayName = 'Display'; // display name
begin
  Result := sDisplayName;
end;

class function TDisplayPrefsFrame.Index: Byte;
  {Index number that determines the location of the tab containing this frame
  when displayed in the preferences dialog box.
    @return Required index number.
  }
begin
  Result := 10;
end;

function TDisplayPrefsFrame.OverviewTreeStateDesc(
  const State: TOverviewStartState): string;
  {Gets description of an overview treeview startup state.
    @param State [in] State for which description is required.
    @return Required description.
  }
resourcestring
  // Startup state descriptions
  sOTSExpanded = 'Fully expanded';
  sOTSCollapsed = 'Fully collapsed';
const
  // Map of overview tree start states to descriptions
  cOTSStartStates: array[TOverviewStartState] of string = (
    sOTSExpanded, sOTSCollapsed
  );
begin
  Result := cOTSStartStates[State];
end;

procedure TDisplayPrefsFrame.SelectOverviewTreeState(
  const State: TOverviewStartState);
  {Selects combo box item associated with a overview treeview startup state.
    @param State [in] Startup state to be selected.
  }
var
  CBIdx: Integer; // loops through each entry in combo box
begin
  for CBIdx := 0 to Pred(cbOverviewTree.Items.Count) do
  begin
    if State = TOverviewStartState(cbOverviewTree.Items.Objects[CBIdx]) then
    begin
      cbOverviewTree.ItemIndex := CBIdx;
      Break;
    end;
  end;
end;

function TDisplayPrefsFrame.UIUpdated: Boolean;
begin
  Result := fUIChanged;
end;

initialization

// Register frame with preferences dialog box
TPreferencesDlg.RegisterPage(TDisplayPrefsFrame);

end.

