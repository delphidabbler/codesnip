{
 * FmDependenciesDlg.pas
 *
 * Implements a dialog box that recursively displays all the dependencies of a
 * snippet.
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
 * The Original Code is FmDependenciesDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmDependenciesDlg;


interface


uses
  // Delphi
  ComCtrls, StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericViewDlg, UBaseObjects, USnippetIDs, USnippets, USnippetsTVDraw;


type
  {
  TDependenciesDlg:
    Dialog box that displays all the dependencies of a snippet in a tree view.
  }
  TDependenciesDlg = class(TGenericViewDlg, INoPublicConstruct)
    tvDependencies: TTreeView;
    lblCircularRef: TLabel;
    lblNoDependencies: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure tvDependenciesCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
  strict private
    type
      {
      TTVDraw:
        Object used to draw snippets tree view nodes.
      }
      TTVDraw = class(TSnippetsTVDraw)
      strict private
        fRootID: TSnippetID;  // ID of snippet whose dependency nodes displayed
      strict protected
        function IsUserDefinedNode(const Node: TTreeNode): Boolean;
          override;
          {Checks if a node represents a user defined snippets object.
            @param Node [in] Node to be checked.
            @return True if node represents user defined object, False if not.
          }
        function IsErrorNode(const Node: TTreeNode): Boolean;
          override;
          {Checks if a node represents an error condition.
            @param Node [in] Node to be checked.
            @return True if node represents error condition, False if not.
          }
      public
        constructor Create(const RootID: TSnippetID);
          {Class constructor. Sets up object.
            @param ID [in] ID snippet for which dependencies are displayed.
          }
      end;
    var
      fSnippetID: TSnippetID;     // Snippet whose dependencies are displayed
      fDependsList: TRoutineList; // List of dependencies to be displayed
      fTVDraw: TTVDraw;           // Customises appearance of tree view}
    procedure PopulateTreeView;
      {Populates treeview with nodes for each snippet in dependency list.
      }
    procedure AddDependencies(const Parent: TTreeNode;
      const DependsList: TRoutineList);
      {Adds tree nodes for snippets in a dependency list.
        @param Parent [in] Parent node for nodes from dependency list.
        @param DependsList [in] Dependency list containing snippets to be added
          to treeview.
      }
    procedure DisplayCircularRefWarning;
      {Displays circular reference warning label.
      }
    function GetDisplayName: string;
      {Gets display name for snippet for which dependencies are being displayed.
        @return Required display name: snippet's name if available, otherwise an
          untitled string.
      }
  strict protected
    procedure ConfigForm; override;
      {Configure controls on form.
      }
    procedure ArrangeForm; override;
      {Arranges controls on form.
      }
    procedure InitForm; override;
      {Initialises form by placing focus on close button rather than treeview.
      }
  public
    class procedure Execute(const AOwner: TComponent;
      const SnippetID: TSnippetID; const DependsList: TRoutineList); overload;
      {Displays dialog box containing details of a snippet's dependencies.
        @param AOwner [in] Component that owns the dialog box.
        @param SnippetID [in] ID of snippet for which dependencies are to be
          displayed.
      }
    class procedure Execute(const AOwner: TComponent; const Snippet: TRoutine);
      overload;
      {Displays dialog box containing details of a snippet's dependencies.
        @param AOwner [in] Component that owns the dialog box.
        @param Snippet [in] Snippet for which dependencies are to be displayed.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics,
  // Project
  UColours, UFontHelper, USnippetKindInfo, FmGenericDlg;

{$R *.dfm}


{ TDependenciesDlg }

procedure TDependenciesDlg.AddDependencies(const Parent: TTreeNode;
  const DependsList: TRoutineList);
  {Adds tree nodes for snippets in a dependency list.
    @param Parent [in] Parent node for nodes from dependency list.
    @param DependsList [in] Dependency list containing snippets to be added to
      treeview.
  }
var
  RequiredSnippet: TRoutine;  // iterates through snippets in DependsList
  ChildNode: TTreeNode;       // a node added to treeview
begin
  for RequiredSnippet in DependsList do
  begin
    // Add node for snippet from dependency list
    ChildNode := tvDependencies.Items.AddChild(
      Parent,
      RequiredSnippet.Name
        + ' ('
        + TSnippetKindInfoList.Instance[RequiredSnippet.Kind].Description
        + ')'
    );
    ChildNode.Data := RequiredSnippet;  // reference to associated snippet
    // Check for circular reference. If detetected display warning otherwise
    // recursively add child nodes for snippet's dependency list
    if (RequiredSnippet.ID <> fSnippetID) then
      AddDependencies(ChildNode, RequiredSnippet.Depends)
    else
      DisplayCircularRefWarning;
  end;
end;

procedure TDependenciesDlg.ArrangeForm;
  {Arranges controls on form.
  }
begin
  inherited;
  // Position "no dependencies" message label in form
  lblNoDependencies.Left :=
    (pnlBody.ClientWidth - lblNoDependencies.Width) div 2;
  lblNoDependencies.Top :=
    (pnlBody.ClientHeight - lblNoDependencies.Height) div 2;
  // Adjust size of treeview
  if lblCircularRef.Visible then
  begin
    // move label
    lblCircularRef.Top := pnlBody.Top + pnlBody.ClientHeight
      - lblCircularRef.Height - 8;
    // circular reference: make room to see circular reference label
    tvDependencies.Align := alTop;
    tvDependencies.Height := lblCircularRef.Top - 6;
  end
  else
    // no circular reference: use all space for treeview
    tvDependencies.Align := alClient;
end;

procedure TDependenciesDlg.ConfigForm;
  {Configure controls on form.
  }
resourcestring
  sNoDepends = '%s has no dependencies';  // message when no dependencies
  sTitle = 'Dependencies for %s';         // form caption template
begin
  inherited;
  // Set form caption
  Caption := Format(sTitle, [GetDisplayName]);
  // Set "no dependencies" label in case needed and make bold
  lblNoDependencies.Caption := Format(sNoDepends, [GetDisplayName]);
  lblNoDependencies.Font.Style := [fsBold];
  // Set "circular reference" label's colour and visibility
  lblCircularRef.Font.Color := clWarningText;
  lblCircularRef.Visible := False;
  // Provide custom draw support for treeview
  fTVDraw := TTVDraw.Create(fSnippetID);
  tvDependencies.OnCustomDrawItem := fTVDraw.CustomDrawItem;
  // Populate treeview with dependency information
  PopulateTreeView;
  // Hide treeview, revealing "no dependencies" label if no dependencies
  if tvDependencies.Items.Count = 0 then
    tvDependencies.Visible := False;
end;

procedure TDependenciesDlg.DisplayCircularRefWarning;
  {Displays circular reference warning label.
  }
begin
  lblCircularRef.Visible := True;
end;

class procedure TDependenciesDlg.Execute(const AOwner: TComponent;
  const Snippet: TRoutine);
  {Displays dialog box containing details of a snippet's dependencies.
    @param AOwner [in] Component that owns the dialog box.
    @param Snippet [in] Snippet for which dependencies are to be displayed.
  }
begin
  Execute(AOwner, Snippet.ID, Snippet.Depends);
end;

class procedure TDependenciesDlg.Execute(const AOwner: TComponent;
  const SnippetID: TSnippetID; const DependsList: TRoutineList);
  {Displays dialog box containing details of a snippet's dependencies.
    @param AOwner [in] Component that owns the dialog box.
    @param SnippetIS [in] ID of snippet for which dependencies are to be
      displayed.
  }
begin
  with InternalCreate(AOwner) do
    try
      fSnippetID := SnippetID;
      fDependsList := DependsList;
      ShowModal;
    finally
      Free;
    end;
end;

procedure TDependenciesDlg.FormDestroy(Sender: TObject);
  {Form destruction event handler. Frees owned object.
    @param Sender [in] Not used.
  }
begin
  inherited;
  FreeAndNil(fTVDraw);
end;

function TDependenciesDlg.GetDisplayName: string;
  {Gets display name for snippet for which dependencies are being displayed.
    @return Required display name: snippet's name if available, otherwise an
      untitled string.
  }
resourcestring
  sUntitled = '<Untitled Snippet>'; // display name when snippet has no name
begin
  Result := fSnippetID.Name;
  if Result = '' then
    Result := sUntitled;
end;

procedure TDependenciesDlg.InitForm;
  {Initialises form by placing focus on close button rather than treeview.
  }
begin
  inherited;
  btnClose.SetFocus;
end;

procedure TDependenciesDlg.PopulateTreeView;
  {Populates treeview with nodes for each snippet in dependency list.
  }
begin
  tvDependencies.Items.BeginUpdate;
  try
    tvDependencies.Items.Clear;
    AddDependencies(nil, fDependsList);
    tvDependencies.FullExpand;
  finally
    tvDependencies.Items.EndUpdate;
  end;
end;

procedure TDependenciesDlg.tvDependenciesCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
  {Handles event triggered when tree view wants to collapse. We don't allow
  this.
    @param Sender [in] Not used.
    @param Node [in] Not used.
    @param AllowChange [in/out] Set false to prevent tree view collapsing.
  }
begin
  AllowCollapse := False;
end;

{ TDependenciesDlg.TTVDraw }

constructor TDependenciesDlg.TTVDraw.Create(
  const RootID: TSnippetID);
  {Class constructor. Sets up object.
    @param ID [in] ID snippet for which dependencies are displayed.
  }
begin
  inherited Create;
  fRootID := RootID;
end;

function TDependenciesDlg.TTVDraw.IsErrorNode(
  const Node: TTreeNode): Boolean;
  {Checks if a node represents an error condition.
    @param Node [in] Node to be checked.
    @return True if node represents error condition, False if not.
  }
begin
  Result := Assigned(Node.Data) and (TRoutine(Node.Data).ID = fRootID);
end;

function TDependenciesDlg.TTVDraw.IsUserDefinedNode(
  const Node: TTreeNode): Boolean;
  {Checks if a node represents a user defined snippets object.
    @param Node [in] Node to be checked.
    @return True if node represents user defined object, False if not.
  }
begin
  if not Assigned(Node.Data) then
    Result := True
  else
    Result := TRoutine(Node.Data).UserDefined;
end;

end.

