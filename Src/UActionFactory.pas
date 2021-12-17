{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines static factory class that can create various kinds of actions.
}


unit UActionFactory;


interface


uses
  // Delphi
  Classes, ActnList,
  // Project
  UBaseObjects;


type

  {
  TActionFactory:
    Static factory class that can create various kinds of actions and,
    optionally set their OnExecute event handlers.
  }
  TActionFactory = class(TNoConstructObject)
  strict private
    class function CreateAction(const ActionClass: TBasicActionClass;
      const AOwner: TComponent;
      const OnExecHandler: TNotifyEvent): TBasicAction;
      {Creates an action of specified class and sets OnExecute handler if
      provided.
        @param ActionClass [in] Class of action to create.
        @param AOwner [in] Owner of action.
        @param OnExecHandler [in] Handler for action's OnExecute event.
        @return Reference to newly created action.
      }
  public
    class function CreateSnippetAction(const AOwner: TComponent;
      const OnExecHandler: TNotifyEvent = nil): TBasicAction;
      {Creates a Snippet action and sets OnExecute handler if provided.
        @param AOwner [in] Owner of action.
        @param OnExecHandler [in] Handler for action's OnExecute event.
        @return Reference to newly created action.
      }
    class function CreateEditSnippetAction(const AOwner: TComponent;
      const OnExecHandler: TNotifyEvent = nil): TBasicAction;
      {Creates an Edit Snippet action and sets OnExecute handler if provided.
        @param AOwner [in] Owner of action.
        @param OnExecHandler [in] Handler for action's OnExecute event.
        @return Reference to newly created action.
      }
    class function CreateViewItemAction(const AOwner: TComponent;
      const OnExecHandler: TNotifyEvent = nil): TBasicAction;
      {Creates a View Item action and sets OnExecute handler if provided.
        @param AOwner [in] Owner of action.
        @param OnExecHandler [in] Handler for action's OnExecute event.
        @return Reference to newly created action.
      }
    class function CreateLinkAction(const AOwner: TComponent;
      const OnExecHandler: TNotifyEvent = nil): TCustomAction;
      {Create a Link action and sets OnExecute handler if provided.
        @param AOwner [in] Owner of action.
        @param OnExecHandler [in] Handler for action's OnExecute event.
        @return Reference to newly created action.
      }
    class function CreateCategoryAction(const AOwner: TComponent;
      const OnExecHandler: TNotifyEvent = nil): TBasicAction;
      {Creates a Category action and sets OnExecute handler if provided.
        @param AOwner [in] Owner of action.
        @param OnExecHandler [in] Handler for action's OnExecute event.
        @return Reference to newly created action.
      }
    class function CreateDetailTabAction(const AOwner: TComponent;
      const OnExecHandler: TNotifyEvent = nil): TBasicAction;
      {Creates a Detail Pane Tab Selection action and sets OnExecute handler if
      provided.
        @param AOwner [in] Owner of action.
        @param OnExecHandler [in] Handler for action's OnExecute event.
        @return Reference to newly created action.
      }
  end;


implementation


uses
  // Delphi
  StdActns,
  // Project
  UCategoryAction,
  UDetailTabAction,
  UEditSnippetAction,
  ULinkAction,
  USnippetAction,
  UViewItemAction;


{ TActionFactory }

class function TActionFactory.CreateAction(
  const ActionClass: TBasicActionClass; const AOwner: TComponent;
  const OnExecHandler: TNotifyEvent): TBasicAction;
  {Creates an action of specified class and sets OnExecute handler if provided.
    @param ActionClass [in] Class of action to create.
    @param AOwner [in] Owner of action.
    @param OnExecHandler [in] Handler for action's OnExecute event.
    @return Reference to newly created action.
  }
begin
  Result := ActionClass.Create(AOwner);
  Result.OnExecute := OnExecHandler;
end;

class function TActionFactory.CreateCategoryAction(const AOwner: TComponent;
  const OnExecHandler: TNotifyEvent): TBasicAction;
  {Creates a Category action and sets OnExecute handler if provided.
    @param AOwner [in] Owner of action.
    @param OnExecHandler [in] Handler for action's OnExecute event.
    @return Reference to newly created action.
  }
begin
  Result := CreateAction(TCategoryAction, AOwner, OnExecHandler);
end;

class function TActionFactory.CreateDetailTabAction(const AOwner: TComponent;
  const OnExecHandler: TNotifyEvent): TBasicAction;
  {Creates a Detail Pane Tab Selection action and sets OnExecute handler if
  provided.
    @param AOwner [in] Owner of action.
    @param OnExecHandler [in] Handler for action's OnExecute event.
    @return Reference to newly created action.
  }
begin
  Result := CreateAction(TDetailTabAction, AOwner, OnExecHandler);
end;

class function TActionFactory.CreateEditSnippetAction(const AOwner: TComponent;
  const OnExecHandler: TNotifyEvent): TBasicAction;
  {Creates an Edit Snipper action and sets OnExecute handler if provided.
    @param AOwner [in] Owner of action.
    @param OnExecHandler [in] Handler for action's OnExecute event.
    @return Reference to newly created action.
  }
begin
  Result := CreateAction(TEditSnippetAction, AOwner, OnExecHandler);
end;

class function TActionFactory.CreateLinkAction(const AOwner: TComponent;
  const OnExecHandler: TNotifyEvent): TCustomAction;
  {Create a Link action and sets OnExecute handler if provided.
    @param AOwner [in] Owner of action.
    @param OnExecHandler [in] Handler for action's OnExecute event.
    @return Reference to newly created action.
  }
begin
  Result := CreateAction(TLinkAction, AOwner, OnExecHandler) as TCustomAction;
end;

class function TActionFactory.CreateSnippetAction(const AOwner: TComponent;
  const OnExecHandler: TNotifyEvent): TBasicAction;
  {Creates a Snippet action and sets OnExecute handler if provided.
    @param AOwner [in] Owner of action.
    @param OnExecHandler [in] Handler for action's OnExecute event.
    @return Reference to newly created action.
  }
begin
  Result := CreateAction(TSnippetAction, AOwner, OnExecHandler);
end;

class function TActionFactory.CreateViewItemAction(
  const AOwner: TComponent; const OnExecHandler: TNotifyEvent): TBasicAction;
  {Creates a View Item action and sets OnExecute handler if provided.
    @param AOwner [in] Owner of action.
    @param OnExecHandler [in] Handler for action's OnExecute event.
    @return Reference to newly created action.
  }
begin
  Result := CreateAction(TViewItemAction, AOwner, OnExecHandler);
end;

end.

