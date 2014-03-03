{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2014, Peter Johnson (www.delphidabbler.com).
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
  ///  <summary>Base class for a TProgressBar descendant component that can
  ///  display a "marquee" that repeatedly cycles the progress bar display.
  ///  </summary>
  ///  <remarks>
  ///  <para>Requires different concrete implementations depending on whether
  ///  the underlying operating system supports the progress bar marquee
  ///  natively.</para>
  ///  <para>Callers must construct instances using the CreateInstance class
  ///  method. The Create constructor must not be called.</para>
  ///  </remarks>
  TMarquee = class abstract(TProgressBar)
  strict private
    ///  <summary>Checks if the underlying operating system supports the
    ///  progress bar natively.</summary>
    class function IsBuiltInMarqueeAvailable: Boolean;
  strict protected
    const
      ///  <summary>Update interval for marquee in ms.</summary>
      UpdateInterval = 20;

  public
    ///  <summary>Initialises a new component instance for sub-classes.
    ///  </summary>
    ///  <param name="AOwner">TComponent [in] Reference to owning component.
    ///  </param>
    ///  <exception>ENoConstructException raised if constructor called directly
    ///  on TMarquee instead of via sub-classes.</exception>
    ///  <remarks>This constructor must not be called directly, only by
    ///  sub-classes. Users of TMarquee must call CreateInstance instead.
    ///  </remarks>
    constructor Create(AOwner: TComponent); override;

    ///  <summary>Constructs and returns a concrete instance of the component
    ///  that implements the marquee in a manner suitable for the underlying
    ///  operating system.</summary>
    ///  <param name="AOwner">TComponent [in] Reference to owning component.
    ///  </param>
    ///  <remarks>On OSs that support the marquee progress bar natively the
    ///  created instance uses the native support. On older OSs without native
    ///  support the concrete instance emulates the marquee.</remarks>
    class function CreateInstance(AOwner: TComponent): TMarquee;

    ///  <summary>Start the marquee running.</summary>
    procedure Start; virtual; abstract;

    ///  <summary>Stop the marquee.</summary>
    procedure Stop; virtual; abstract;
  end;


implementation


uses
  // Delphi
  SysUtils, Messages, Controls, ExtCtrls,
  // Project
  USystemInfo, UThemesEx;


type
  ///  <summary>Progress bar component that can emulate a marquee.</summary>
  ///  <remarks>For use on operating systems that do not have native support for
  ///  marquees within the progress bar.</remarks>
  TEmulatedMarquee = class sealed(TMarquee)
  strict private
    ///  <summary>Timer object used to update marquee display.</summary>
    fTimer: TTimer;
    ///  <summary>Handles timer events by updating the progress bar position.
    ///  </summary>
    procedure TickHandler(Sender: TObject);
  public
    ///  <summary>Destroys object instance. Ensures the timer has been stopped.
    ///  </summary>
    destructor Destroy; override;
    ///  <summary>Starts the marquee emulation running.</summary>
    procedure Start; override;
    ///  <summary>Stops the marquee emulation.</summary>
    procedure Stop; override;
  end;

  ///  <summary>Progress bar component that displays a marquee natively.
  ///  </summary>
  ///  <remarks>For use on operating systems that have native support for
  ///  marquees within a progress bar.</remarks>
  TNativeMarquee = class sealed(TMarquee)
  strict private
    const
      ///  <summary>Progress bar marquee window style.</summary>
      ///  <remarks>From CommCtrl.h.</remarks>
      PBS_MARQUEE = $08;              // progress bar marquee style
      ///  <summary>Progress bar marquee activation / deactivation message.
      ///  </summary>
      ///  <remarks>From CommCtrl.h.</remarks>
      PBM_SETMARQUEE = WM_USER + 10;
  public
    ///  <summary>Modifies the progress bar's window style to enable use of the
    ///  marquee.</summary>
    procedure CreateParams(var Params: TCreateParams); override;
    ///  <summary>Starts the marquee running.</summary>
    procedure Start; override;
    ///  <summary>Stops the marquee running.</summary>
    procedure Stop; override;
  end;

{ TMarquee }

constructor TMarquee.Create(AOwner: TComponent);
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
begin
  // We only use built in marquee on Vista or later when themes are enabled
  Result := TOSInfo.IsReallyWindowsVistaOrGreater
    and ThemeServicesEx.ThemesEnabled;
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
  if Assigned(fTimer) then
  begin
    fTimer.Enabled := False;
    FreeAndNil(fTimer); // FreeAndNil is necessary here
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
  Params.Style := Params.Style or PBS_MARQUEE;
end;

procedure TNativeMarquee.Start;
begin
  Perform(PBM_SETMARQUEE, 1, UpdateInterval); // activates marquee
end;

procedure TNativeMarquee.Stop;
begin
  Perform(PBM_SETMARQUEE, 0, 0);  // deactivates marquee
end;

end.

