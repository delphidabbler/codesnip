{
 * Browser.UControlHelper.pas
 *
 * Defines a static class that provides helper methods for manipulating and
 * interogating web browser controls.
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
 * The Original Code is Browser.UControlHelper.pas, formerly UWBHelper.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit Browser.UControlHelper;


interface


uses
  // Delphi
  ShDocVw,
  // Project
  UBaseObjects;


type

  {
  TWBControlHelper:
    Static class that provides helper methods for manipulating and interogating
    web browser controls.
  }
  TWBControlHelper = class(TNoConstructObject)
  strict private
    class procedure WaitForDocToLoad(const WB: TWebBrowser);
      {Pauses until a browser control's document has fully loaded.
        @param WB [in] Browser control whose document we are waiting for.
      }
    class procedure CheckValidDoc(const WB: TWebBrowser);
      {Checks that the current document in a web browser control is a valid HTML
      document.
        @param WB [in] Browser control whose document is to be checked.
        @raise EBug if document is not valid.
      }
  public
    class procedure WaitForValidDocToLoad(const WB: TWebBrowser);
      {Pauses until a browser control's document has fully loaded then checks
      that the document is a valid HTML document.
        @param WB [in] Browser control whose document we are waiting for.
        @raise EBug if document is not valid.
      }
    class function IsCommandEnabled(const WB: TWebBrowser;
      const CmdId: OLECMDID): Boolean;
      {Checks if a OLE command is enabled on a browser control.
        @param WB [in] Browser control on which command is to be tested.
        @param CmdId [in] Id of OLE command.
        @return True if command is enabled or False if not.
      }
    class procedure ExecCommand(const WB: TWebBrowser; const CmdId: OLECMDID);
      {Executes an OLE command on a browser control.
        @param WB [in] Browser control on which command is to be tested.
        @param CmdId [in] Id of OLE command.
      }
  end;


implementation


uses
  // Delphi
  Forms, Windows,
  // Project
  UExceptions, UHTMLDocHelper, UUtils;


{ TWBControlHelper }

class procedure TWBControlHelper.CheckValidDoc(const WB: TWebBrowser);
  {Checks that the current document in a web browser control is a valid HTML
  document.
    @param WB [in] Browser control whose document is to be checked.
    @raise EBug if document is not valid.
  }
begin
  Assert(Assigned(WB), ClassName + '.CheckValidDoc: WB is nil');
  if not Assigned(WB.Document) then
    raise EBug.Create(ClassName + '.CheckValidDoc: Document not assigned');
  if not THTMLDocHelper.IsValidDocument(WB.Document) then
    raise EBug.Create(
      ClassName + '.CheckValidDoc: Document is not a valid HTML document'
    );
end;

class procedure TWBControlHelper.ExecCommand(const WB: TWebBrowser;
  const CmdId: OLECMDID);
  {Executes an OLE command on a browser control.
    @param WB [in] Browser control on which command is to be tested.
    @param CmdId [in] Id of OLE command.
  }
begin
  Assert(Assigned(WB), ClassName + '.ExecCommand: WB is nil');
  WB.ExecWB(CmdId, OLECMDEXECOPT_DONTPROMPTUSER);
end;

class function TWBControlHelper.IsCommandEnabled(const WB: TWebBrowser;
  const CmdId: OLECMDID): Boolean;
  {Checks if a OLE command is enabled on a browser control.
    @param WB [in] Browser control on which command is to be tested.
    @param CmdId [in] Id of OLE command.
    @return True if command is enabled or False if not.
  }
begin
  Assert(Assigned(WB), ClassName + '.IsCommandEnabled: WB is nil');
  Result := (WB.QueryStatusWB(CmdId) and OLECMDF_ENABLED) <> 0;
end;

class procedure TWBControlHelper.WaitForDocToLoad(const WB: TWebBrowser);
  {Pauses until a browser control's document has fully loaded.
    @param WB [in] Browser control whose document we are waiting for.
  }
begin
  Assert(Assigned(WB), ClassName + '.WaitForDocToLoad: WB is nil');
  while WB.ReadyState <> READYSTATE_COMPLETE do
    UUtils.Pause(5);
end;

class procedure TWBControlHelper.WaitForValidDocToLoad(const WB: TWebBrowser);
  {Pauses until a browser control's document has fully loaded then checks that
  the document is a valid HTML document.
    @param WB [in] Browser control whose document we are waiting for.
    @raise EBug if document is not valid.
  }
begin
  WaitForDocToLoad(WB);
  CheckValidDoc(WB);
end;

end.

