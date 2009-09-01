{
 * UCommandBars.pas
 *
 * Defines various classes used to configure one or more command bars owned by
 * a container. Command bars are UI elements used to issue commands, e.g. menus,
 * toolbars etc.
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
 * The Original Code is UCommandBars.pas
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


unit UCommandBars;


interface


uses
  // Delphi
  Classes, ActnList, Menus, ComCtrls, ImgList,
  // Project
  ULists;


type

  {
  TCommandBarID:
    Valid values for command bar IDs used to uniquely identify command bars
    within a container.
  }
  TCommandBarID = 0..High(SmallInt);

  {
  ICommandBarConfig:
    Interface supported by objects that can configure one or more
    "command bars".
  }
  ICommandBarConfig = interface(IInterface)
    ['{B70EAC6F-F0AB-4318-B36A-CBC3C89DC9A5}']
    procedure AddAction(const Action: TCustomAction;
      const ID: TCommandBarID);
      {Adds an new command action to a command bar.
        @param Action [in] Action to be added.
        @param Kind [in] Id of command bar to receive command bar item.
      }
    procedure AddSpacer(const ID: TCommandBarID);
      {Adds a spacer to a command bar.
        @param Kind [in] Id of command bar to receive spacer.
      }
    procedure SetImages(const Images: TCustomImageList);
      {Specifies image list to be used by all command bars.
        @param Images [in] Image list to be used.
      }
  end;

  {
  TCommandBarWrapper:
    Abstract base class for classes that wrap individual command bars to add
    actions and spacers and to reference image lists.
  }
  TCommandBarWrapper = class abstract(TComponent)
  public
    procedure AddAction(const Action: TCustomAction); virtual; abstract;
      {Adds a new command action to the wrapped command bar.
        @param Action [in] Action to be added.
      }
    procedure AddSpacer; virtual; abstract;
      {Adds a new spacer to the wrapped command bar.
      }
    procedure SetImages(const Images: TCustomImageList); virtual; abstract;
      {Specifies the image list to be used by the wrapped command bar.
        @param Images [in] Image list to be used.
      }
  end;

  {
  TPopupMenuWrapperClass:
    Reference to TPopupMenuWrapper class and descendants.
  }
  TPopupMenuWrapperClass = class of TPopupMenuWrapper;

  {
  TPopupMenuWrapper:
    Class that wraps a popup menu to add actions and spacers and to use a
    specified image list.
  }
  TPopupMenuWrapper = class(TCommandBarWrapper)
  strict private
    fMenu: TPopupMenu;    // Value of Menu property
    procedure MenuPopupHandler(Sender: TObject);
      {Handles popup menu's OnPopup event by initialising menu and hiding
      disabled menu items. Assumes menu items are not nested.
        @param Sender [in] Not used.
      }
  strict protected
    procedure InitMenu; virtual;
      {Method called method menu pops up to permit customisation. Does nothing.
      Sub classes can override.
      }
    procedure PrepareMenu; virtual;
      {Prepares menu before it pops up. Calls InitMenu then hides any disabled
      menu items. Assumes menu items are not nested.
      }
    property Menu: TPopupMenu read fMenu;
      {Reference to wrapped popup menu}
  public
    constructor Create(const Menu: TPopupMenu); reintroduce; virtual;
      {Class constructor. Sets up wrapper for a popup menu.
        @param Menu [in] Popup menu to be wrapped.
      }
    procedure AddAction(const Action: TCustomAction); override;
      {Adds a new command action to the wrapped popup menu.
        @param Action [in] Action to be added.
      }
    procedure AddSpacer; override;
      {Adds a new spacer to the wrapped poup menu.
      }
    procedure SetImages(const Images: TCustomImageList); override;
      {Specifies the image list to be used by the wrapped popup menu.
        @param Images [in] Image list to be used.
      }
  end;

  {
  TToolBarWrapper:
    Class that wraps a toolbar to add actions and spacers and to use a specified
    image list.
  }
  TToolBarWrapper = class(TCommandBarWrapper)
  strict private
    fToolBar: TToolBar; // wrapped toolbar
  public
    constructor Create(const Toolbar: TToolBar); reintroduce;
      {Class constructor. Sets up wrapper for a toobar.
        @param Menu [in] Toolbar to be wrapped.
      }
    procedure AddAction(const Action: TCustomAction); override;
      {Adds a new command action to the wrapped toobar.
        @param Action [in] Action to be added.
      }
    procedure AddSpacer; override;
      {Adds a new spacer to the wrapped toolbar.
      }
    procedure SetImages(const Images: TCustomImageList); override;
      {Specifies the image list to be used by the wrapped toolbar.
        @param Images [in] Image list to be used.
      }
  end;

  {
  TCommandBarMgr:
    Contained class that manages the configuration of one or more command bars
    on behalf an outer object.
  }
  TCommandBarMgr = class(TContainedObject, ICommandBarConfig)
  strict private
    fCommandBars: TIntegerList;   // Map of command bar IDs to command bars
    fImageList: TCustomImageList; // Images list used by all command bars
    procedure UpdateImageLists;
      {Updates image list used by with all managed command bars.
      }
  protected // do not make strict
    { ICommandBarConfig methods }
    procedure AddAction(const Action: TCustomAction;
      const ID: TCommandBarID);
      {Adds an new command action to a command bar.
        @param Action [in] Action to be added.
        @param Kind [in] Id of command bar to receive command bar item.
      }
    procedure AddSpacer(const ID: TCommandBarID);
      {Adds a spacer to a command bar.
        @param Kind [in] Id of command bar to receive spacer.
      }
    procedure SetImages(const Images: TCustomImageList); virtual;
      {Specifies image list to be used by all command bars.
        @param Images [in] Image list to be used.
      }
  public
    constructor Create(const Controller: IInterface);
      {Class constructor. Creates contained object.
        @param Controller [in] IInterface reference to containing object.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure AddCommandBar(const ID: TCommandBarID;
      const CommandBar: TCommandBarWrapper);
      {Adds a new command bar to command bar manager.
        @param ID [in] ID of command bar.
        @param CommandBar [in] command bar object to be added.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UMenuHelper, UToolButtonEx;


{ TCommandBarMgr }

procedure TCommandBarMgr.AddAction(const Action: TCustomAction;
  const ID: TCommandBarID);
  {Adds an new command action to a command bar.
    @param Action [in] Action to be added.
    @param Kind [in] Id of command bar to receive command bar item.
  }
var
  CmdBar: TCommandBarWrapper; // required command bar
begin
  CmdBar := fCommandBars.FindObject(ID) as TCommandBarWrapper;
  Assert(Assigned(CmdBar), ClassName + '.AddAction: ID not found');
  (fCommandBars.FindObject(ID) as TCommandBarWrapper).AddAction(Action);
end;

procedure TCommandBarMgr.AddCommandBar(const ID: TCommandBarID;
  const CommandBar: TCommandBarWrapper);
  {Adds a new command bar to command bar manager.
    @param ID [in] ID of command bar.
    @param CommandBar [in] command bar object to be added.
  }
begin
  fCommandBars.Add(ID, CommandBar);
  CommandBar.SetImages(fImageList);
end;

procedure TCommandBarMgr.AddSpacer(const ID: TCommandBarID);
  {Adds a spacer to a command bar.
    @param Kind [in] Id of command bar to receive spacer.
  }
var
  CmdBar: TCommandBarWrapper; // required command bar
begin
  CmdBar := fCommandBars.FindObject(ID) as TCommandBarWrapper;
  Assert(Assigned(CmdBar), ClassName + '.AddSpacer: ID not found');
  (fCommandBars.FindObject(ID) as TCommandBarWrapper).AddSpacer;
end;

constructor TCommandBarMgr.Create(const Controller: IInterface);
  {Class constructor. Creates contained object.
    @param Controller [in] IInterface reference to containing object.
  }
begin
  inherited Create(Controller);
  fCommandBars := TIntegerList.Create;
end;

destructor TCommandBarMgr.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fCommandBars);
  inherited;
end;

procedure TCommandBarMgr.SetImages(const Images: TCustomImageList);
  {Specifies image list to be used by all command bars.
    @param Images [in] Image list to be used.
  }
begin
  fImageList := Images;
  UpdateImageLists;
end;

procedure TCommandBarMgr.UpdateImageLists;
  {Updates image list used by with all managed command bars.
  }
var
  Idx: Integer; // loops through all managed command bars
begin
  for Idx := 0 to Pred(fCommandBars.Count) do
    (fCommandBars.Objects[Idx] as TCommandBarWrapper).SetImages(fImageList);
end;

{ TPopupMenuWrapper }

procedure TPopupMenuWrapper.AddAction(const Action: TCustomAction);
  {Adds a new command action to the wrapped popup menu.
    @param Action [in] Action to be added.
  }
begin
  fMenu.Items.Add(CreateMenuItem(fMenu, TMenuItem, Action));
end;

procedure TPopupMenuWrapper.AddSpacer;
  {Adds a new spacer to the wrapped poup menu.
  }
begin
  fMenu.Items.Add(CreateMenuSpacer(fMenu, TMenuItem));
end;

constructor TPopupMenuWrapper.Create(const Menu: TPopupMenu);
  {Class constructor. Sets up wrapper for a popup menu.
    @param Menu [in] Popup menu to be wrapped.
  }
begin
  inherited Create(Menu);
  fMenu := Menu;
  fMenu.OnPopup := MenuPopupHandler;
end;

procedure TPopupMenuWrapper.InitMenu;
  {Method called method menu pops up to permit customisation. Does nothing. Sub
  classes can override.
  }
begin
  // Do nothing
end;

procedure TPopupMenuWrapper.MenuPopupHandler(Sender: TObject);
  {Handles popup menu's OnPopup event by initialising menu and hiding disabled
  menu items. Assumes menu items are not nested.
    @param Sender [in] Not used.
  }
begin
  PrepareMenu;
end;

procedure TPopupMenuWrapper.PrepareMenu;
  {Prepares menu before it pops up. Calls InitMenu then hides any disabled
  menu items. Assumes menu items are not nested.
  }
var
  Idx: Integer;   // loops through all menu items
  MI: TMenuItem;  // references each menu item
begin
  InitMenu;
  for Idx := 0 to Pred(fMenu.Items.Count) do
  begin
    MI := fMenu.Items[Idx];
    if Assigned(MI.Action) then
      MI.Action.Update;
    MI.Visible := MI.Enabled;
  end;
end;

procedure TPopupMenuWrapper.SetImages(const Images: TCustomImageList);
  {Specifies the image list to be used by the wrapped popup menu.
    @param Images [in] Image list to be used.
  }
begin
  fMenu.Images := Images;
end;

{ TToolBarWrapper }

procedure TToolBarWrapper.AddAction(const Action: TCustomAction);
  {Adds a new command action to the wrapped toobar.
    @param Action [in] Action to be added.
  }
begin
  TToolButtonEx.NewHostedButton(fToolBar, Action);
end;

procedure TToolBarWrapper.AddSpacer;
  {Adds a new spacer to the wrapped toolbar.
  }
begin
  TToolButtonEx.NewHostedSeparator(fToolBar);
end;

constructor TToolBarWrapper.Create(const Toolbar: TToolBar);
  {Class constructor. Sets up wrapper for a toobar.
    @param Menu [in] Toolbar to be wrapped.
  }
begin
  inherited Create(Toolbar);
  fToolbar := Toolbar;
end;

procedure TToolBarWrapper.SetImages(const Images: TCustomImageList);
  {Specifies the image list to be used by the wrapped toolbar.
    @param Images [in] Image list to be used.
  }
begin
  fToolBar.Images := Images;
end;

end.

