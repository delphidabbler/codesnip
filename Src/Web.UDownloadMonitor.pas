{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Manages download progress reporting for objects that interact with web
 * service.
}

// TODO -cwebsvc -cQuery: Check if this unit is this still required

unit Web.UDownloadMonitor;


interface


uses
  // Indy
  IdHTTP, IdComponent, IdGlobal {needed for conditional defines below};


// TWorkEvent and TWorkBeginEvent have different signatures between Indy v10.1.x
// and v10.2.x and later. The following is an attempt to decide which signature
// to use. Should this unit fail to compile, you can define INDY_WORKEVENT_INT64
// in the project options or on the compiler command line to override this
// code. Define INDY_WORKEVENT_INT64 to use a Int64 parameter and or define
// INDY_WORKEVENT_INT32 to use a 32 bit integer as the last paramter of both
// event handler types.
{$IF not Defined(INDY_WORKEVENT_INT64) and not Defined(INDY_WORKEVENT_INT64)}
  {$IF gsIdVersion >= '10.2'}
    {$DEFINE INDY_WORKEVENT_INT64}
    {$UNDEF INDY_WORKEVENT_INT32}
  {$ELSE}
    {$UNDEF INDY_WORKEVENT_INT64}
    {$DEFINE INDY_WORKEVENT_INT32}
  {$IFEND}
{$IFEND}
{$IF Defined(INDY_WORKEVENT_INT32) and Defined(INDY_WORKEVENT_INT64)}
  {$MESSAGE FATAL
    'Can''t define both INDY_WORKEVENT_INT32 and INDY_WORKEVENT_INT32'}
{$IFEND}


type

  {
  TDownloadMonitorCallback:
    Type of callback method called by TDownloadMonitor when download information
    changes.
  }
  TDownloadMonitorCallback = procedure of object;

  {
  TDownloadMonitor:
    Manages download progress reporting for web service object.
  }
  TDownloadMonitor = class(TObject)
  strict private
    fCallback: TDownloadMonitorCallback;
      {Reference to method called to notify changes in download state}
    fBytesExpected: Integer;
      {Number of bytes expected in current download}
    fBytesReceived: Integer;
      {Number of bytes received to date in curent download}
    procedure HTTPWorkHandler(Sender: TObject; AWorkMode: TWorkMode;
      AWorkCount:
      {$IFDEF INDY_WORKEVENT_INT64}Int64{$ENDIF}
      {$IFDEF INDY_WORKEVENT_INT32}Integer{$ENDIF});
      {Handles Indy HTTP client's OnWork event. We process only download events.
      Upload events are ignored. Updates record of bytes received.
        @param Sender [in] Not used.
        @param AWorkMode [in] Whether download (read) or upload (write) event.
        @param AWorkCount [in] Number of bytes received to date.
      }
    procedure HTTPWorkBeginHandler(Sender: TObject; AWorkMode: TWorkMode;
      AWorkCountMax:
      {$IFDEF INDY_WORKEVENT_INT64}Int64{$ENDIF}
      {$IFDEF INDY_WORKEVENT_INT32}Integer{$ENDIF});
      {Handles Indy HTTP client's OnWorkBegin event. We process only download
      events. Upload events are ignored. Records number of expected bytes in
      download.
        @param Sender [in] Not used.
        @param AWorkMode [in] Whether download (read) or upload (write) event.
        @param AWorkCountMax [in] Number of bytes expected in download.
      }
    procedure HTTPWorkEndHandler(Sender: TObject; AWorkMode: TWorkMode);
      {Handles Indy HTTP client's OnWorkEnd event. We process only download
      events. Upload events are ignored. Finalises download.
        @param Sender [in] Not used.
        @param AWorkMode [in] Whether download (read) or upload (write) event.
      }
    procedure NotifyChange;
      {Notifies change in download state by calling callback method.
      }
    procedure Init(const BytesExpected: Integer);
      {Initialises new download.
        @param BytesExpected [in] Number of bytes to be downloaded.
      }
    procedure Update(const BytesReceived: Integer);
      {Updates download statistics.
        @param BytesReceived [in] Number of bytes received to date.
      }
    procedure Done;
      {Finalises a download. Adjusts bytes received and calls callback.
      }
  public
    constructor Create(const HTTP: TIdHTTP;
      const Callback: TDownloadMonitorCallback);
      {Class constructor. Sets up object to monitor downloads received by Indy
      HTTP components.
        @param HTTP [in] Reference to Indy HTTP component we use to intercept
          download events. Must not be nil.
        @param Callback [in] Method to call to notify of download updates. Must
          not be nil.
      }
    property BytesReceived: Integer
      read fBytesReceived;
      {Total number of bytes received in current download}
    property BytesExpected: Integer
      read fBytesExpected;
      {Total number of bytes expected in current download}
  end;


implementation


{ TDownloadMonitor }

constructor TDownloadMonitor.Create(const HTTP: TIdHTTP;
  const Callback: TDownloadMonitorCallback);
  {Class constructor. Sets up object to monitor downloads received by Indy HTTP
  components.
    @param HTTP [in] Reference to Indy HTTP component we use to intercept
      download events. Must not be nil.
    @param Callback [in] Method to call to notify of download updates. Must not
      be nil.
    }
begin
  Assert(Assigned(HTTP), ClassName + '.Create: HTTP is nil');
  Assert(Assigned(Callback), ClassName + '.Create: Callback is nil');
  inherited Create;
  HTTP.OnWork := HTTPWorkHandler;
  HTTP.OnWorkBegin := HTTPWorkBeginHandler;
  HTTP.OnWorkEnd := HTTPWorkEndHandler;
  fCallback := Callback;
end;

procedure TDownloadMonitor.Done;
  {Finalises a download. Adjusts bytes received and calls callback.
  }
begin
  fBytesReceived := fBytesExpected;
  NotifyChange;
end;

procedure TDownloadMonitor.HTTPWorkBeginHandler(Sender: TObject;
  AWorkMode: TWorkMode;
  AWorkCountMax:
  {$IFDEF INDY_WORKEVENT_INT64}Int64{$ENDIF}
  {$IFDEF INDY_WORKEVENT_INT32}Integer{$ENDIF});
  {Handles Indy HTTP client's OnWorkBegin event. We process only download
  events. Upload events are ignored. Records number of expected bytes in
  download.
    @param Sender [in] Not used.
    @param AWorkMode [in] Whether download (read) or upload (write) event.
    @param AWorkCountMax [in] Number of bytes expected in download.
  }
begin
  if AWorkMode = wmRead then
    Init(AWorkCountMax)
end;

procedure TDownloadMonitor.HTTPWorkEndHandler(Sender: TObject;
  AWorkMode: TWorkMode);
  {Handles Indy HTTP client's OnWorkEnd event. We process only download events.
  Upload events are ignored. Finalises download.
    @param Sender [in] Not used.
    @param AWorkMode [in] Whether download (read) or upload (write) event.
  }
begin
  if AWorkMode = wmRead then
    Done;
end;

procedure TDownloadMonitor.HTTPWorkHandler(Sender: TObject;
  AWorkMode: TWorkMode;
  AWorkCount:
  {$IFDEF INDY_WORKEVENT_INT64}Int64{$ENDIF}
  {$IFDEF INDY_WORKEVENT_INT32}Integer{$ENDIF});
  {Handles Indy HTTP client's OnWork event. We process only download events.
  Upload events are ignored. Updates record of bytes received.
    @param Sender [in] Not used.
    @param AWorkMode [in] Whether download (read) or upload (write) event.
    @param AWorkCount [in] Number of bytes received to date.
  }
begin
  if AWorkMode = wmRead then
    Update(AWorkCount);
end;

procedure TDownloadMonitor.Init(const BytesExpected: Integer);
  {Initialises new download.
    @param BytesExpected [in] Number of bytes to be downloaded.
  }
begin
  fBytesExpected := BytesExpected;
  fBytesReceived := 0;
  NotifyChange;
end;

procedure TDownloadMonitor.NotifyChange;
  {Notifies change in download state by calling callback method.
  }
begin
  fCallback;
end;

procedure TDownloadMonitor.Update(const BytesReceived: Integer);
  {Updates download statistics.
    @param BytesReceived [in] Number of bytes received to date.
  }
begin
  fBytesReceived := BytesReceived;
  NotifyChange;
end;

end.

