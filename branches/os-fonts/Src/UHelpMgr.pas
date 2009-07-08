{ ##
  @FILE                     UHelpMgr.pas
  @COMMENTS                 Provides an interface that the program uses to
                            access help. Also exposes a routine that enables an
                            object that implements the interface to be
                            registered as the program's help manager and makes
                            that object available as a singleton.
  @PROJECT_NAME             CodeSnip
  @PROJECT_DESC             Offline viewer for routines from the online
                            DelphiDabbler CodeSnip database.
  @DEPENDENCIES             None
  @HISTORY(
    @REVISION(
      @VERSION              0.1
      @DATE                 30/11/2005
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.0
      @DATE                 28/05/2006
      @COMMENTS             + Improved and corrected comments.
                            + Relocated and rationalised $WARN directives.
                            + Removed unused unit reference.
                            + Changed to get executable file directory from
                              TAppInfo static class.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 26/10/2006
      @COMMENTS             + Declared new IHelpMgr interface.
                            + Moved THelpMgr to implementation section and
                              changed to descend from TInterfacedObject and
                              implement IHelpMgr. Renamed as THTMLHelpMgr since
                              object implements help system via HTML help.
                            + Changed private implementation variable to be of
                              IHelpMgr type.
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 11/11/2006
      @COMMENTS             + Now gets help file name from TAppInfo rather than
                              naming it in this method.
                            + Removed unecessary fHelpFile field and now empty
                              constructor.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 04/02/2007
      @COMMENTS             Total rewrite.
                            + Moved help manager implementation to separate
                              UHTMLHelpMgr unit.
                            + This unit now simply (a) declares IHTMLMgr
                              interface (b) provides function that accesses
                              singleton and (c) provides new function to
                              register a IHTMLMgr implementation to used as
                              help manager defined and instantiated elsewhere.
    )
  )
}


{
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
 * Portions created by the Initial Developer are Copyright (C) 2005-2007 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
}


unit UHelpMgr;


interface


uses
  // Delphi
  Classes;


type

  {
  IHelpMgr:
    Implementation independent interface to object that can display program's
    help system.
  }
  IHelpMgr = interface(IInterface)
    ['{17CF0CF6-8161-4986-9BE6-AAB19729B826}']
    procedure ShowHelp(const AKeyword: string); overload;
      {Display help topic specified by an A-Link keyword.
        @param AKeyword Required A-Link keyword.
      }
    procedure ShowHelp(const HelpContext: THelpContext); overload;
      {Display help topic per a help context number.
        @param HelpContext Required topic's help context number.
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
  Assert(Assigned(PvtHelpMgr),                             // ** do not localise
    'HelpMgr: PvtHelpMgr is nil');
  Result := PvtHelpMgr;
end;

procedure RegisterHelpMgr(const AHelpMgr: IHelpMgr);
  {Registers a help manager for use in displaying help.
    @param AHelpMgr [in] Help manager to be used.
  }
begin
  Assert(Assigned(AHelpMgr),                               // ** do not localise
    'RegisterHelpMgr: AHelpMgr is nil');
  PvtHelpMgr := AHelpMgr;
end;


initialization


finalization

// Free the singleton
PvtHelpMgr := nil;

end.

