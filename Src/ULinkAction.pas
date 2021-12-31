{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a custom action used to trigger an HTML anchor element.
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
      {Calls OnUpdate handler if one is assigned, otherwise sets visibility and
      enabled state of action.
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
    sDisplaySnippet,  // akSnippet
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
  {Calls OnUpdate handler if one is assigned, otherwise sets visibility and
  enabled state of action.
    @return Returns True if OnUpdate handler assigned or False if not.
  }
begin
  Result := inherited Update;
  if not Result then
  begin
    // set invisible and disabled if element is not an anchor
    Visible := Anchor <> nil;
    Enabled := Visible;
  end;
end;

end.

