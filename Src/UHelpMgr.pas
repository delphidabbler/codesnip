{
 * UHelpMgr.pas
 *
 * Provides an interface that the program uses to access help. Also exposes a
 * routine that enables an object that implements the interface to be registered
 * as the program's help manager and makes that object available as a singleton.
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
 * The Original Code is UHelpMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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

