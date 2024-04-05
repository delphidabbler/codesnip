{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines a form that hosts the program's easter egg.
}


unit FmEasterEgg;


interface


uses
  // Delphi
  ExtCtrls, Controls, Forms, Classes,
  // Project
  Browser.UHTMLEvents, IntfAligner, FmBase, FrBrowserBase, FrEasterEgg;


type

  {
  TEasterEggForm:
    Form that hosts easter egg frame and contained HTML document. Aligns form,
    fades in and out and responds to events in HTML.
  }
  TEasterEggForm = class(TBaseForm)
    timerReveal: TTimer;
    frmEasterEgg: TEasterEggFrame;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  strict private
    procedure RevealTick(Sender: TObject);
      {Timer event handler used to fade out the form when closing.
        @param Sender [in] Not used.
      }
    procedure HideTick(Sender: TObject);
      {Timer event handler used to fade out the form when closing.
        @param Sender [in] Not used.
      }
    procedure BrowserEventHandler(Sender: TObject;
      const EventInfo: THTMLEventInfo);
      {Handles easter egg frame's OnHTMLEvent event. Checks for a click on the
      'cancel-btn' image and closes dialog if detected.
        @param Sender [in] Not used.
        @param EventInfo [in] Object providing information about the event.
      }
  strict protected
    procedure InitForm; override;
      {Initialises controls on form. Loads Easter Egg frame's HTML and starts
      fade-in animation.
      }
    function GetAligner: IFormAligner; override;
      {Gets object to be used to align form to owner.
        @return Required aligner.
      }
  public
    class procedure Execute(const AOwner: TComponent);
      {Displays easter egg modally.
        @param AOwner [in] Component that owns this form.
      }
  end;


implementation


uses
  // Project
  UConsts, UFormAligner, UDlgHelper, UUtils;


{$R *.dfm}

{ TEasterEggForm }

procedure TEasterEggForm.BrowserEventHandler(Sender: TObject;
  const EventInfo: THTMLEventInfo);
  {Handles easter egg frame's OnHTMLEvent event. Checks for a click on the
  'cancel-btn' image and closes dialog if detected.
    @param Sender [in] Not used.
    @param EventInfo [in] Object providing information about the event.
  }
const
  cCancelImgId = 'cancel-btn';  // id of cancel "button" image
begin
  if EventInfo.IsEvent(
      THTMLDocumentEvents2Sink.EventIntf,
      THTMLDocumentEvents2Sink.DISPID_OnClick
    )
    and EventInfo.ElemHasId(cCancelImgId) then
  begin
    // Click on cancel image detected. Prevent event from bubbling up and close
    // dialog
    EventInfo.Cancelled := True;
    Close;
  end;
end;

class procedure TEasterEggForm.Execute(const AOwner: TComponent);
  {Displays easter egg modally.
    @param AOwner [in] Component that owns this form.
  }
var
  EggForm: TEasterEggForm;
begin
  EggForm := Create(AOwner);
  try
    EggForm.ShowModal;
  finally
    EggForm.Free;
  end;
end;

procedure TEasterEggForm.FormClose(Sender: TObject; var Action: TCloseAction);
  {Form close event handler. Fades out display before allowing form to close.
    @param Sender [in] Not used.
    @param Action [in/out] Not used or changed.
  }
begin
  inherited;
  timerReveal.OnTimer := HideTick;
  timerReveal.Enabled := True;
  while timerReveal.Enabled do
    Pause(10);
end;

procedure TEasterEggForm.FormCreate(Sender: TObject);
  {Form creation event handler. Performs initialisation.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // Set owner control as parent of this form's window
  TDlgHelper.SetDlgParentToOwner(Self);
  // Assign browser event handler.
  frmEasterEgg.OnHTMLEvent := BrowserEventHandler;
end;

procedure TEasterEggForm.FormKeyPress(Sender: TObject; var Key: Char);
  {Form key press event handler. Closes form if ESC key is detected.
    @param Sender [in] Not used.
    @param Key [in/out] Character code of key pressed. Not modified.
  }
begin
  inherited;
  if Key = ESC then
    Close;
end;

function TEasterEggForm.GetAligner: IFormAligner;
  {Gets object to be used to align form to owner.
    @return Required aligner.
  }
begin
  Result := TSimpleFormAligner.Create;
end;

procedure TEasterEggForm.HideTick(Sender: TObject);
  {Timer event handler used to fade out the form when closing.
    @param Sender [in] Not used.
  }
const
  cAlphaDelta = 24;  // change made to alpha channel on each tick
begin
  if AlphaBlendValue <= cAlphaDelta then
  begin
    AlphaBlendValue := 0;
    timerReveal.Enabled := False;
  end
  // fade out: slower when form is nearly opaque
  else if AlphaBlendValue > 196 then
    AlphaBlendValue := AlphaBlendValue - cAlphaDelta div 2
  else
    AlphaBlendValue := AlphaBlendValue - cAlphaDelta;
end;

procedure TEasterEggForm.InitForm;
  {Initialises controls on form. Loads Easter Egg frame's HTML and starts
  fade-in animation.
  }
begin
  inherited;
  frmEasterEgg.Initialise;
  timerReveal.OnTimer := RevealTick;
  timerReveal.Enabled := True;
end;

procedure TEasterEggForm.RevealTick(Sender: TObject);
  {Timer event handler used to fade out the form when closing.
    @param Sender [in] Not used.
  }
const
  cAlphaDelta = 6;  // change made to alpha channel on each tick
begin
  if AlphaBlendValue >= 255 - cAlphaDelta then
  begin
    AlphaBlendValue := 255;
    timerReveal.Enabled := False;
  end
  // fade in: slower when form is nearly opaque
  else if AlphaBlendValue <= 196 then
    AlphaBlendValue := AlphaBlendValue + cAlphaDelta
  else
    AlphaBlendValue := AlphaBlendValue + cAlphaDelta  div 2;
end;

end.

