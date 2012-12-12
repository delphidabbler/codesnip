{
 * FmSplash.pas
 *
 * Implements a splash screen with timeout.
 *
 * v1.0 of 25 Feb 2007  - Original version.
 * v1.1 of 27 Sep 2007  - Changed DefaultMonitor property to position splash
 *                        form on same monitor as main form.
 *                      - Changed to use renamed IFormAligner interface.
 *                      - Fixed bug where splash screen was displayed in wrong
 *                        place when application form was off the screen when
 *                        closed.
 * v1.2 of 29 Oct 2007  - Applied lock to form closure method so that main code
 *                        and timer cannot access the method at the same time.
 * v1.3 of 15 Dec 2008  - Modified to use TRectEx record instead of TRect.
 *                      - Made private and protected sections strict.
 * v1.4 of 13 May 2009  - Now gets program and company name from TAppInfo
 *                        instead of UGlobals unit.
 * v1.5 of 17 Jun 2009  - Splash form content now comes from a gif image.
 *                        Program's version number is overlayed on the image.
 *                      - Made TSplashAligner's private section strict.
 *
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
 * The Original Code is FmSplash.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FmSplash;


interface


uses
  // Delphi
  ExtCtrls, Classes, Controls, Forms,
  // Project
  FmBase, IntfAligner;


type

  {
  TSplashForm:
    Class that implements a splash screen that displays for a defined minimum
    amount of time.
  }
  TSplashForm = class(TBaseForm)
    pbMain: TPaintBox;
    tmMinDisplay: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pbMainPaint(Sender: TObject);
    procedure tmMinDisplayTimer(Sender: TObject);
  strict private
    fCloseRequested: Boolean; // Records if RequestClose method has been called
    fTimeOut: Boolean;        // Records if minimum display time has elapsed
    fTryToCloseLock: Integer; // prevent simultaneous access to TryToCloseLock
    procedure TryToClose;
      {Closes form only if RequestClose method has been called and if minimum
      display time has elapsed.
      }
  strict protected
    function GetAligner: IFormAligner; override;
      {Creates and returns reference to an object that is used to align the form
      to the owner.
        @return Required aligner object instance.
      }
  public
    procedure RequestClose;
      {Requests that form should close. If minimum display time has expired form
      will close, otherwise request will be noted and form will not close.
      }
  end;


var
  SplashForm: TSplashForm;  // global variable that records instance of form


implementation


uses
  // Delphi
  Windows, Graphics,
  // 3rd party
  GIFImage,
  // Project
  UAppInfo, UColours, UStructs, UWindowSettings;


{$R *.dfm}


type

  {
  TSplashAligner:
    Class that can centre a splash form over the application's main form.
  }
  TSplashAligner = class(TInterfacedObject, IFormAligner)
  strict private
    function GetMainFormBounds(const AForm: TCustomForm): TRectEx;
      {Gets bounds of main form from persistent storage or calculates it if
      we can't read persistent storage.
        @param AForm [in] Form to be aligned.
        @return Required bounds rectangle.
      }
  protected // do not make strict
    { IFormAligner method }
    procedure AlignForm(const AForm: TCustomForm);
      {Aligns splash form over main form.
        @param AForm [in] Form to be aligned.
      }
  end;

  {
  TOwnerWindowSettings:
    Class that gets bounds rectangle of main form from persistent storage.
  }
  TOwnerWindowSettings = class(TWindowSettings)
  public
    function GetWdwState(out BoundsRect: TRectEx;
      out State: TWindowState): Boolean;
      {Retrieves main form's bounds rectangle and window state from persistent
      storage.
        @param BoundsRect [out] Set to bounding rectangle of main form.
        @param State [out] Set to window state of main form.
        @return True if information read from storage OK, False if can't read
          information. When False values of out parameters are undefined.
      }
  end;


{ TSplashForm }

procedure TSplashForm.FormClose(Sender: TObject; var Action: TCloseAction);
  {Handles form's OnClose event. Frees the form object.
    @param Sender [in] Not used.
    @param Action [in/out] Set to value that ensures form is freed.
  }
begin
  inherited;
  Action := caFree;
  SplashForm := nil;
end;

function TSplashForm.GetAligner: IFormAligner;
  {Creates and returns reference to an object that is used to align the form to
  the owner.
    @return Required aligner object instance.
  }
begin
  Result := TSplashAligner.Create;
end;

procedure TSplashForm.pbMainPaint(Sender: TObject);
  {Paints form's image on paint box in response to paint box's OnPaint event.
    @param Sender [in] Not used.
  }
var
  GIF: TGIFImage;   // main splash image
begin
  // Load and display splash screen image
  GIF := TGIFImage.Create;
  try
    GIF.LoadFromResourceName(HInstance, 'SPLASHIMAGE');
    Canvas.Draw(0, 0, GIF);
  finally
    GIF.Free;
  end;

  // Draw release version info
  // 1: as shadow text
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Name := 'Tahoma';
  Canvas.Font.Size := 9;
  Canvas.Font.Style := [fsBold];
  // draw text shadow
  Canvas.Font.Color := clSplashShadowText;
  Canvas.TextOut(
    36,
    112,
    TAppInfo.ProgramReleaseInfo
  );
  // 2: as foreground text
  Canvas.Font.Color := clSplashPlainText;
  Canvas.TextOut(
    37,
    113,
    TAppInfo.ProgramReleaseInfo
  );
end;

procedure TSplashForm.RequestClose;
  {Requests that form should close. If minimum display time has expired form
  will close, otherwise request will be noted and form will not close.
  }
begin
  fCloseRequested := True;
  TryToClose;
end;

procedure TSplashForm.tmMinDisplayTimer(Sender: TObject);
  {Handles minimum display timer's OnTimer event. This event fires when form's
  minimum display time has expired. An attempt is made to close the form. This
  only succeeds if RequestClose method has been called.
    @param Sender [in] Not used.
  }
begin
  inherited;
  tmMinDisplay.Enabled := False;  // prevent timer from firing again
  fTimeOut := True;               // note that form has timed out
  TryToClose;
end;

procedure TSplashForm.TryToClose;
  {Closes form only if RequestClose method has been called and if minimum
  display time has elapsed.
  }
begin
  // Wait until lock is cleared
  while fTryToCloseLock > 0 do
    Application.ProcessMessages;
  // Lock entry to method
  InterlockedIncrement(fTryToCloseLock);
  try
    // Close document if closure requested and time out reached
    if fCloseRequested and fTimeOut then
      Close;
  finally
    // Unlock method
    InterlockedDecrement(fTryToCloseLock);
  end;
end;

{ TSplashAligner }

procedure TSplashAligner.AlignForm(const AForm: TCustomForm);
  {Aligns splash form over main form.
    @param AForm [in] Form to be aligned.
  }
var
  AlignBounds: TRectEx; // bounds rectangle over which form is to be aligned
begin
  // Centre form within main form's bounds
  AlignBounds := GetMainFormBounds(AForm);
  AForm.Left := (AlignBounds.Right + AlignBounds.Left - AForm.Width) div 2;
  AForm.Top := (AlignBounds.Bottom + AlignBounds.Top - AForm.Height) div 2;
end;

function TSplashAligner.GetMainFormBounds(const AForm: TCustomForm): TRectEx;
  {Gets bounds of main form from persistent storage or calculates it if we can't
  read persistent storage.
    @param AForm [in] Form to be aligned.
    @return Required bounds rectangle.
  }
var
  State: TWindowState;  // window state read from storage
begin
  // We get main form's bounds from persistent storage: we have to do this since
  // the splash form may be displayed before main form is aligned.
  // If we can't read from persistent storage or form is maximized we centre
  // splash form in work area. This works because main form is centred when
  // storage can't be read, and maximized form takes all of work area.
  with TOwnerWindowSettings.Create(AForm) do
    try
      if not GetWdwState(Result, State) or (State = wsMaximized) then
        Result := Screen.WorkAreaRect;  // we use workarea of primary monitor
    finally
      Free;
    end;
end;

{ TOwnerWindowSettings }

function TOwnerWindowSettings.GetWdwState(out BoundsRect: TRectEx;
  out State: TWindowState): Boolean;
  {Retrieves main form's bounds rectangle and window state from persistent
  storage.
    @param BoundsRect [out] Set to bounding rectangle of main form.
    @param State [out] Set to window state of main form.
    @return True if information read from storage OK, False if can't read
      information. When False values of out parameters are undefined.
  }
var
  Left: Integer;      // location of left hand side of window
  Top: Integer;       // location of top of window
  Width: Integer;     // width of window
  Height: Integer;    // height of window
  StateCode: Integer; // state of window as ordinal
  WorkArea: TRectEx;  // work area of current monitor
begin
  // Set defaults
  Left := 0;
  Top := 0;
  Width := 0;
  Height := 0;
  StateCode := -1;
  // Get details from storage
  ReadWdwState(Left, Top, Width, Height, StateCode);
  // Check if read OK
  Result := (Width > 0) and (Height > 0) and (StateCode <> -1);
  if Result then
  begin
    // We read OK: set out params
    State := TWindowState(StateCode);
    BoundsRect := TRectEx.CreateBounds(Left, Top, Width, Height);
    // Adjust bounds within work area on required monitor
    WorkArea := Screen.MonitorFromRect(BoundsRect).WorkareaRect;
    if BoundsRect.Right > WorkArea.Right then
      BoundsRect.OffsetBy(WorkArea.Right - BoundsRect.Right, 0);
    if BoundsRect.Left < Workarea.Left then
      BoundsRect.OffsetBy(WorkArea.Left - BoundsRect.Left, 0);
    if BoundsRect.Bottom > WorkArea.Bottom then
      BoundsRect.OffsetBy(0, WorkArea.Bottom - BoundsRect.Bottom);
    if BoundsRect.Top < WorkArea.Top then
      BoundsRect.OffsetBy(0, WorkArea.Top - BoundsRect.Top);
  end;
end;

end.
