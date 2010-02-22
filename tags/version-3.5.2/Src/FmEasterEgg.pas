{
 * FmEasterEgg.pas
 *
 * Defines a form that hosts the program's easter egg.
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
 * The Original Code is FmEasterEgg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmEasterEgg;


interface


uses
  // Delphi
  ExtCtrls, Controls, Forms, Classes, Windows,
  // Project
  IntfAligner, FmBase, FrBrowserBase, FrEasterEgg, UHTMLEvents;


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
  private
    type
      {
      TAligner:
        Class that can centre this form over the owning control.
      }
      TAligner = class(TInterfacedObject, IFormAligner)
      protected // do not make strict
        procedure AlignForm(const AForm: TCustomForm);
          {Aligns splash form over main form.
            @param AForm [in] Form to be aligned.
          }
      end;
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
  // Delphi
  Graphics,
  // Project
  UColours, UConsts, UGraphicUtils, UDlgHelper, UStructs, UUtils;


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
  if (EventInfo.DispatchId = cDocEventOnClick) and
    (EventInfo.Args.srcElement.id = cCancelImgId) then
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
begin
  with Create(AOwner) do
    try
      ShowModal;
    finally
      Free;
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
  Result := TAligner.Create;
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
  cAlphaDelta = 4;  // change made to alpha channel on each tick
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

{ TEasterEggForm.TAligner }

procedure TEasterEggForm.TAligner.AlignForm(const AForm: TCustomForm);
  {Aligns splash form over main form.
    @param AForm [in] Form to be aligned.
  }
var
  FormBounds: TRectEx;    // bounds of easter egg form
  Owner: TWinControl;     // owner wincontrol
  OwnerBounds: TRectEx;   // screen bounds of owner control
  WorkArea: TRectEx;      // desktop work area
begin
  Assert(AForm.Owner is TWinControl,
    ClassName + '.AlignForm: AForm.Owner must be a TWinControl');
  // Get bounds of owner control
  Owner := AForm.Owner as TWinControl;
  if Owner.Parent = nil then
    // no parent: bounds are already in screen coords
    OwnerBounds := Owner.BoundsRect
  else
  begin
    // parented control: bounds are relative to parent => map to screen coords
    OwnerBounds.TopLeft := Owner.ClientToScreen(
      Owner.BoundsRect.TopLeft
    );
    OwnerBounds.BottomRight := Owner.ClientToScreen(
      Owner.BoundsRect.BottomRight
    );
  end;
  // Calculate form bounds
  FormBounds := AForm.BoundsRect;
  FormBounds.OffsetBy(
    OwnerBounds.Left - FormBounds.Left +
      (OwnerBounds.Width - FormBounds.Width) div 2,
    OwnerBounds.Top - FormBounds.Top +
      (OwnerBounds.Height - FormBounds.Height) div 2
  );
  // Ensure form is in work area
  WorkArea := Screen.MonitorFromRect(FormBounds).WorkareaRect;
  if FormBounds.Right > WorkArea.Right then
    FormBounds.OffsetBy(WorkArea.Right - FormBounds.Right, 0);
  if FormBounds.Left < WorkArea.Left then
    FormBounds.OffsetBy(WorkArea.Left - FormBounds.Left, 0);
  if FormBounds.Bottom > WorkArea.Bottom then
    FormBounds.OffsetBy(0, WorkArea.Bottom - FormBounds.Bottom);
  if FormBounds.Top < WorkArea.Top then
    FormBounds.OffsetBy(0, WorkArea.Top - FormBounds.Top);
  // Place the form
  AForm.BoundsRect := FormBounds;
end;

end.

