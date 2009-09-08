{
 * UDownloadMonitor.pas
 *
 * Manages download progress reporting for web service object.
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
 * The Original Code is UDownloadMonitor.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}



unit UDownloadMonitor;


interface


uses
  // Indy
  IdHTTP, IdComponent;


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
  private
    fCallback: TDownloadMonitorCallback;
      {Reference to method called to notify changes in download state}
    fBytesExpected: Integer;
      {Number of bytes expected in current download}
    fBytesReceived: Integer;
      {Number of bytes received to date in current download}
    procedure HTTPWorkHandler(Sender: TObject; AWorkMode: TWorkMode;
      const AWorkCount: Integer);
      {Handles Indy HTTP client's OnWork event. We process only download events.
      Upload events are ignored. Updates record of bytes received.
        @param Sender [in] Not used.
        @param AWorkMode [in] Whether download (read) or upload (write) event.
        @param AWorkCount [in] Number of bytes received to date.
      }
    procedure HTTPWorkBeginHandler(Sender: TObject; AWorkMode: TWorkMode;
      const AWorkCountMax: Integer);
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
  Assert(Assigned(HTTP),
    'TWebServiceProgress.Create: HTTP is nil');            // ** do not localise
  Assert(Assigned(Callback),
    'TWebServiceProgress.Create: Callback is nil');        // ** do not localise
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
  AWorkMode: TWorkMode; const AWorkCountMax: Integer);
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
  AWorkMode: TWorkMode; const AWorkCount: Integer);
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

