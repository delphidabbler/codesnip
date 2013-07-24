{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * TProgressBar descendant that displays a "marquee" that repeatedly cycles a
 * progress bar display.
}


unit UMarquee;


interface


uses
  // Delphi
  Classes, Controls, ComCtrls, ExtCtrls;


type

  {
  TMarquee:
    TProgressBar descendant that displays a "marquee" that repeatedly cycles
    a progress bar display. For use when a progress bar is required but the
    length of time / maximum value to be displayed is not known. Emulates
    marquee on older OSs and uses OS provided facilities on later OSs.
  }
  TMarquee = class(TProgressBar)
  strict private
    fTimer: TTimer;
      {Timer object used to emulate marquee on older OSs}
    procedure TickHandler(Sender: TObject);
      {Event handler for timer OnTimer event. Updates position of progress bar.
        @param Sender [in] Not used.
      }
    function IsBuiltInMarqueeAvailable: Boolean;
      {Checks if were are to use OS's built in marquee.
        @return True if built in marquee is to be used, False if we are to
          emulate it.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up object.
        @param AOwner [in] Owning component.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure CreateParams(var Params: TCreateParams); override;
      {Sets window parameters. This overidden method sets the marquee style if
      supported.
        @param Params [in/out] In: current window parameters. Out: Modified
          parameters if changed.
      }
    procedure Start;
      {Starts marquee running.
      }
    procedure Stop;
      {Stops marquee running.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Windows, Messages,
  // Project
  USystemInfo, UThemesEx;


const
  // Constants defined in later versions on CommCtrl.h
  PBS_MARQUEE = $08;              // progress bar marquee style
  PBM_SETMARQUEE = WM_USER + 10;  // progress bar marquee message
  // Other constants
  cInterval = 20;                 // update interval for marquee


{ TMarquee }

constructor TMarquee.Create(AOwner: TComponent);
  {Class constructor. Sets up object.
    @param AOwner [in] Owning component.
  }
begin
  inherited;
  Smooth := True;
end;

procedure TMarquee.CreateParams(var Params: TCreateParams);
  {Sets window parameters. This overidden method sets the marquee style if
  supported.
    @param Params [in/out] In: current window parameters. Out: Modified
      parameters if changed.
  }
begin
  inherited;
  if IsBuiltInMarqueeAvailable then
    // tells progress bar to use marquee
    Params.Style := Params.Style or PBS_MARQUEE;
end;

destructor TMarquee.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fTimer);
  inherited;
end;

function TMarquee.IsBuiltInMarqueeAvailable: Boolean;
  {Checks if were are to use OS's built in marquee.
    @return True if built in marquee is to be used, False if we are to emulate
      it.
  }
begin
  // We only use built in marquee on Vista or later when themes are enabled
  Result := TOSInfo.IsVistaOrLater and ThemeServicesEx.ThemesEnabled;
end;

procedure TMarquee.Start;
  {Starts marquee running.
  }
begin
  if IsBuiltInMarqueeAvailable then
    // using built in marquee: send message to activate it
    Perform(PBM_SETMARQUEE, 1, cInterval)
  else
  begin
    // emulating marquee: create and activate timer
    fTimer := TTimer.Create(nil);
    fTimer.Interval := cInterval;
    fTimer.OnTimer := TickHandler;
    fTimer.Enabled := True;
    Position := 0;
    Max := 48;
  end;
end;

procedure TMarquee.Stop;
  {Stops marquee running.
  }
begin
  if IsBuiltInMarqueeAvailable then
    // using built in marquee: send message to deactivate it
    Perform(PBM_SETMARQUEE, 0, 0)
  else
  begin
    // emulating marquee: destroy timer
    if Assigned(fTimer) then
    begin
      fTimer.Enabled := False;
      FreeAndNil(fTimer);
    end;
  end;
end;

procedure TMarquee.TickHandler(Sender: TObject);
  {Event handler for timer OnTimer event. Updates position of progress bar.
    @param Sender [in] Not used.
  }
begin
  Position := (Position + 1) mod (Max + 1);
end;

end.
