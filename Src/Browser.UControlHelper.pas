{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines a static class that provides helper methods for manipulating and
 * interogating web browser controls.
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
  UExceptions, UHTMLDOMHelper, UUtils;


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
  if not THTMLDOMHelper.IsValidDocument(WB.Document) then
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

