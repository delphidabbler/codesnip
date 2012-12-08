{
 * UActionFactory.pas
 *
 * Defines static factory class that can create various kinds of actions.
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
 * The Original Code is UActionFactory.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
    class function CreateCompLogAction(const AOwner: TComponent;
      const OnExecHandler: TNotifyEvent = nil): TBasicAction;
      {Creates a Compiler Log action and sets OnExecute handler if provided.
        @param AOwner [in] Owner of action.
        @param OnExecHandler [in] Handler for action's OnExecute event.
        @return Reference to newly created action.
      }
    class function CreateHintAction(const AOwner: TComponent;
      const OnExecHandler: TNotifyEvent = nil): TBasicAction;
      {Creates a Hint action and sets OnExecute handler if provided.
        @param AOwner [in] Owner of action.
        @param OnExecHandler [in] Handler for action's OnExecute event.
        @return Reference to newly created action.
      }
    class function CreateRoutineAction(const AOwner: TComponent;
      const OnExecHandler: TNotifyEvent = nil): TBasicAction;
      {Creates a Routine action and sets OnExecute handler if provided.
        @param AOwner [in] Owner of action.
        @param OnExecHandler [in] Handler for action's OnExecute event.
        @return Reference to newly created action.
      }
    class function CreateEditRoutineAction(const AOwner: TComponent;
      const OnExecHandler: TNotifyEvent = nil): TBasicAction;
      {Creates an Edit Routine actions and sets OnExecute handler if provided.
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
  end;


implementation


uses
  // Delphi
  StdActns,
  // Project
  UCategoryAction, UCompLogAction, UEditRoutineAction, ULinkAction,
  URoutineAction, UViewItemAction;


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

class function TActionFactory.CreateCompLogAction(const AOwner: TComponent;
  const OnExecHandler: TNotifyEvent): TBasicAction;
  {Creates a Compiler Log action and sets OnExecute handler if provided.
    @param AOwner [in] Owner of action.
    @param OnExecHandler [in] Handler for action's OnExecute event.
    @return Reference to newly created action.
  }
begin
  Result := CreateAction(TCompLogAction, AOwner, OnExecHandler);
end;

class function TActionFactory.CreateEditRoutineAction(const AOwner: TComponent;
  const OnExecHandler: TNotifyEvent): TBasicAction;
  {Creates an Edit Routine actions and sets OnExecute handler if provided.
    @param AOwner [in] Owner of action.
    @param OnExecHandler [in] Handler for action's OnExecute event.
    @return Reference to newly created action.
  }
begin
  Result := CreateAction(TEditRoutineAction, AOwner, OnExecHandler);
end;

class function TActionFactory.CreateHintAction(const AOwner: TComponent;
  const OnExecHandler: TNotifyEvent): TBasicAction;
  {Creates a Hint action and sets OnExecute handler if provided.
    @param AOwner [in] Owner of action.
    @param OnExecHandler [in] Handler for action's OnExecute event.
    @return Reference to newly created action.
  }
begin
  Result := CreateAction(THintAction, AOwner, OnExecHandler);
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

class function TActionFactory.CreateRoutineAction(const AOwner: TComponent;
  const OnExecHandler: TNotifyEvent): TBasicAction;
  {Creates a Routine action and sets OnExecute handler if provided.
    @param AOwner [in] Owner of action.
    @param OnExecHandler [in] Handler for action's OnExecute event.
    @return Reference to newly created action.
  }
begin
  Result := CreateAction(TRoutineAction, AOwner, OnExecHandler);
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

