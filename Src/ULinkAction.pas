{
 * ULinkAction.pas
 *
 * Implements a custom action used to trigger an HTML anchor element.
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
 * The Original Code is ULinkAction.pas
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


unit ULinkAction;


interface


uses
  // Delphi
  ActnList;


type

  {
  TLinkAction:
    Custom action used to trigger an HTML anchor element.
  }
  TLinkAction = class(TCustomAction)
  strict private
    fLink: IDispatch; // Value of Link property
    procedure SetLink(const Value: IDispatch);
      {Sets link element and action's caption.
        @param Value [in] New link element.
      }
  strict protected
    function Anchor: IDispatch;
      {Gets anchor associated with link element. This is either element of
      itself or an enclosing anchor element.
        @return IDispatch interface of anchor element or nil if there is no
          anchor.
      }
    function GetURL: string;
      {Gets URL accessed by anchor.
        @return URL or '' if link element is not an anchor.
      }
  public
    function Execute: Boolean; override;
      {Calls OnExecute handler if one is assigned, otherwise clicks the link.
        @return Returns True if OnExecute handler assigned or False if not.
      }
    function Update: Boolean; override;
      {Calls OnUpdate handler if one is assigned, otherwise sets visibility of
      action.
        @return Returns True if OnUpdate handler assigned or False if not.
      }
    property Link: IDispatch read fLink write SetLink;
      {HTML anchor element, or child element, that action is to be associated
      with}
  end;


implementation


uses
  // Project
  UAnchors;


resourcestring
  // Action captions for different link types
  sOpen = 'Open Link';
  sOpenInBrowser = 'Open Link In Browser';
  sDisplaySnippet = 'Display Snippet';
  sDisplayCategory = 'Display Category';
  sExecCommand = 'Execute Command';
  sShowHelp = 'Show Help Topic';


const
  // Map of anchor kinds to action captions
  cCaptions: array[TAnchorKind] of string = (
    sOpenInBrowser,   // akExternal
    sDisplaySnippet,  // akRoutine
    sDisplayCategory, // akCategory
    sExecCommand,     // akCommand
    sShowHelp,        // akHelp
    sOpen,            // akUnknown
    ''                // akError
  );

{ TLinkAction }

function TLinkAction.Anchor: IDispatch;
  {Gets anchor associated with link element. This is either element of itself
  or an enclosing anchor element.
    @return IDispatch interface of anchor element or nil if there is no anchor.
  }
begin
  Result := TAnchors.FindEnclosingAnchor(fLink);
end;

function TLinkAction.Execute: Boolean;
  {Calls OnExecute handler if one is assigned, otherwise clicks the link.
    @return Returns True if OnExecute handler assigned or False if not.
  }
begin
  Result := inherited Execute;
  if not Result then
    TAnchors.Click(Anchor);
end;

function TLinkAction.GetURL: string;
  {Gets URL accessed by anchor.
    @return URL or '' if link element is not an anchor.
  }
begin
  Result := TAnchors.GetURL(Anchor);
end;

procedure TLinkAction.SetLink(const Value: IDispatch);
  {Sets link element and action's caption.
    @param Value [in] New link element.
  }
begin
  fLink := Value;
  Caption := cCaptions[TAnchors.AnchorKind(Anchor)];
end;

function TLinkAction.Update: Boolean;
  {Calls OnUpdate handler if one is assigned, otherwise sets visibility of
  action.
    @return Returns True if OnUpdate handler assigned or False if not.
  }
begin
  Result := inherited Update;
  if not Result then
    // set invisible if element is not an anchor
    Visible := Anchor <> nil;
end;

end.

