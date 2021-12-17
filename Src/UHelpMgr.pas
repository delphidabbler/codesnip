{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Provides an interface that the program uses to access help. Also exposes a
 * routine that enables an object that implements the interface to be registered
 * as the program's help manager and makes that object available as a singleton.
}


unit UHelpMgr;


interface


type

  {
  IHelpMgr:
    Implementation independent interface to object that can display program's
    help system.
  }
  IHelpMgr = interface(IInterface)
    ['{17CF0CF6-8161-4986-9BE6-AAB19729B826}']
    procedure ShowHelp(const AKeyword: string);
      {Display help topic specified by an A-Link keyword.
        @param AKeyword Required A-Link keyword.
      }
    procedure ShowContents;
      {Display help contents tab and default help page.
      }
  end;

function HelpMgr: IHelpMgr;
  {Returns reference to singleton object that implements IHelpMgr, ensuring it
  exists.
    @return Singleton object.
  }

procedure RegisterHelpMgr(const AHelpMgr: IHelpMgr);
  {Registers a help manager for use in displaying help.
    @param AHelpMgr [in] Help manager to be used.
  }


implementation


var
  // Stores reference to singleton help manager object
  PvtHelpMgr: IHelpMgr = nil;

function HelpMgr: IHelpMgr;
  {Returns reference to singleton object that implements IHelpMgr, ensuring it
  exists.
    @return Singleton object.
  }
begin
  Assert(Assigned(PvtHelpMgr), 'HelpMgr: PvtHelpMgr is nil');
  Result := PvtHelpMgr;
end;

procedure RegisterHelpMgr(const AHelpMgr: IHelpMgr);
  {Registers a help manager for use in displaying help.
    @param AHelpMgr [in] Help manager to be used.
  }
begin
  Assert(Assigned(AHelpMgr), 'RegisterHelpMgr: AHelpMgr is nil');
  PvtHelpMgr := AHelpMgr;
end;


initialization


finalization


// Free the singleton
PvtHelpMgr := nil;


end.

