{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2013, Peter Johnson (www.delphidabbler.com).
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
  Classes, ComCtrls;


type

  {
  TMarquee:
    TProgressBar descendant that displays a "marquee" that repeatedly cycles
    a progress bar display. For use when a progress bar is required but the
    length of time / maximum value to be displayed is not known. Emulates
    marquee on older OSs and uses OS provided facilities on later OSs.
  }
  TMarquee = class abstract(TProgressBar)
  strict private
    class function IsBuiltInMarqueeAvailable: Boolean;
      {Checks if were are to use OS's built in marquee.
        @return True if built in marquee is to be used, False if we are to
          emulate it.
      }
  strict protected
    const
      // Update interval for marquee in ms.
      UpdateInterval = 20;
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up object.
        @param AOwner [in] Owning component.
      }
    class function CreateInstance(AOwner: TComponent): TMarquee;
    procedure Start; virtual; abstract;
      {Starts marquee running.
      }
    procedure Stop; virtual; abstract;
      {Stops marquee running.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Messages, Controls, ExtCtrls,
  // Project
  USystemInfo, UThemesEx;


type
  TEmulatedMarquee = class sealed(TMarquee)
  strict private
    fTimer: TTimer;
      {Timer object used to emulate marquee on older OSs}
    procedure TickHandler(Sender: TObject);
      {Event handler for timer OnTimer event. Updates position of progress bar.
        @param Sender [in] Not used.
      }
  public
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
  end;

  TNativeMarquee = class sealed(TMarquee)
  strict private
    const
      // Constants defined in CommCtrl.h
      PBS_MARQUEE = $08;              // progress bar marquee style
      PBM_SETMARQUEE = WM_USER + 10;  // progress bar marquee message
  public
    procedure CreateParams(var Params: TCreateParams); override;
      {Sets window parameters. This overidden method sets the marquee style.
        @param Params [in/out] In: current window parameters. Out: Modified
          parameters.
      }
    procedure Start; override;
    procedure Stop; override;
  end;

{ TMarquee }

constructor TMarquee.Create(AOwner: TComponent);
  {Class constructor. Sets up object.
    @param AOwner [in] Owning component.
  }
begin
  if ClassType = TMarquee then
    raise ENoConstructException.CreateFmt(
      '%s.Create can only be called by instantiating subclasses. '
        + 'To create the required instance call CreateInstance instead.',
      [ClassName]
    );
  inherited Create(AOwner);
  Smooth := True;
end;

class function TMarquee.CreateInstance(AOwner: TComponent): TMarquee;
begin
  if IsBuiltInMarqueeAvailable then
    Result := TNativeMarquee.Create(AOwner)
  else
    Result := TEmulatedMarquee.Create(AOwner);
end;

class function TMarquee.IsBuiltInMarqueeAvailable: Boolean;
  {Checks if were are to use OS's built in marquee.
    @return True if built in marquee is to be used, False if we are to emulate
      it.
  }
begin
  // We only use built in marquee on Vista or later when themes are enabled
  Result := TOSInfo.IsVistaOrLater and ThemeServicesEx.ThemesEnabled;
end;

{ TEmulatedMarquee }

destructor TEmulatedMarquee.Destroy;
begin
  Stop;   // ensure timer has been stopped
  inherited;
end;

procedure TEmulatedMarquee.Start;
begin
  // timer used to move emulated marquee along
  fTimer := TTimer.Create(nil);
  fTimer.Interval := UpdateInterval;
  fTimer.OnTimer := TickHandler;
  fTimer.Enabled := True;
  // initialise progress bar to suitable values for marquee
  Position := 0;
  Max := 48;
end;

procedure TEmulatedMarquee.Stop;
begin
  // destroy any timer
  if Assigned(fTimer) then
  begin
    fTimer.Enabled := False;
    FreeAndNil(fTimer);
  end;
end;

procedure TEmulatedMarquee.TickHandler(Sender: TObject);
begin
  Position := (Position + 1) mod (Max + 1);
end;

{ TNativeMarquee }

procedure TNativeMarquee.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // tell progress bar to use marquee
  Params.Style := Params.Style or PBS_MARQUEE;
end;

procedure TNativeMarquee.Start;
begin
  // activate marquee
  Perform(PBM_SETMARQUEE, 1, UpdateInterval);
end;

procedure TNativeMarquee.Stop;
begin
  // deactivate marquee
  Perform(PBM_SETMARQUEE, 0, 0);
end;

end.

