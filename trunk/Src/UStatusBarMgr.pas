{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements class that manages display of status information and hints in a
 * status bar.
}


unit UStatusBarMgr;


interface


uses
  // Delphi
  Windows, Graphics, ComCtrls;


type

  {
  TStatusBarMgr:
    Class that manages display of status information and hints in a status bar.
  }
  TStatusBarMgr = class(TObject)
  strict private
    var
      fStatusBar: TStatusBar;
        {Reference to managed status bar}
      fSearchGlyph: TBitmap;
        {Stores reference to glyph used to indicate kind of latest search}
      fModifiedGlyph: TBitmap;
        {Stores glyph displayed when user database has been modified}
      fSearchInfoVisible: Boolean;
        {Flag noting whether search information is to be displayed or not}
      fUserDBInfoVisible: Boolean;
        {Flag noting whether user database modification indicator is displayed}
    const
      cDBPanel = 0;             // index of database info panel
      cSearchPanel = 1;         // index of search info panel
      cUserPanel = 2;           // index of user-defined database info panel
      cSimplePanel = cDBPanel;  // index of simple message panel
    procedure DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
      {Handles status bar's OnDrawPanel event called whenever an owner-draw
      status panel needs to be displayed.
        @param StatusBar [in] Reference to status bar containing panel.
        @param Panel [in] Reference to status bar panel to be drawn.
        @param Rect [in] Bounding rectangle of panel relative to status bar.
      }
    procedure ShowSnippetsInfo;
      {Displays snippets statistics in status bar.
      }
    procedure ShowSearchInfo;
      {Causes information about a search to be displayed in status bar.
      }
    procedure HideSearchInfo;
      {Prevents search information from being displayed in status bar.
      }
    procedure ShowUserDBInfo;
      {Displays information about state of user defined database.
      }
    procedure HideUserDBInfo;
      {Prevents user defined database information from being displayed in status
      bar.
      }
  public
    constructor Create(const SB: TStatusBar);
      {Class constructor. Sets up object to manage a status bar.
        @param SB [in] Status bar to be managed.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure ShowHint(const Hint: string);
      {Displays a hint in status bar or clears hint text if no hint is provided.
        @param Hint [in] Hint to be displayed or '' to restore previous message.
      }
    procedure ShowSimpleMessage(const Msg: string);
      {Displays a simple text message in first panel of status bar and hides
      results displayed in other panels.
        @param Msg [in] Message to be displayed.
      }
    procedure Update;
      {Updates status bar and refreshes information displayed.
      }
  end;


implementation


{
  NOTES
  ------------------------------------------------------------------------------
  Status bar has three panels (indexed 0..2) used as follows:

  + Panel[0]: Displays database statistics or a simple prompt. Status bar
    default drawing is used. When a simple prompty is displayed Panel[1] and
    Panel[2] are hidden.
  + Panel[1]: Displays information about latest search. A glyph indicating
    search type is displayed. The panel is owner-drawn.
  + Panel[2]: Displays a modification flag and glyph if user defined database
    has been modified since last save. Nothing is displayed when database is
    not modified. The panel is owner-drawn.

  The status bar is also used to display hints when the mouse passes over
  various UI elements. This is done by switching the status bar into SimplePanel
  mode and displaying the hint in the single panel. Once the hint is deactivated
  the main display is restored by switching the status bar out of SimplePanel
  mode.
}


uses
  // Delphi
  SysUtils, Forms,
  // Project
  DB.UMain, UQuery, USearch, UStructs;


{ TStatusBarMgr }

constructor TStatusBarMgr.Create(const SB: TStatusBar);
  {Class constructor. Sets up object to manage a status bar.
    @param SB [in] Status bar to be managed.
  }
resourcestring
  // Message displayed when user database has been modified
  sModified = 'Modified';
begin
  Assert(Assigned(SB), ClassName + '.Create: SB is nil');
  inherited Create;
  fSearchInfoVisible := False;  // we don't display search info by default
  fUserDBInfoVisible := False;  // we don't display modified flag by default
  fSearchGlyph := TBitmap.Create;
  fModifiedGlyph := TBitmap.Create;
  fModifiedGlyph.LoadFromResourceName(HInstance, 'MODIFIED');
  // Record reference to status bar
  fStatusBar := SB;
  // Ensure auto-hinting is switched off
  fStatusBar.AutoHint := False;
  // Enable owner drawing for second panel in status bar
  fStatusBar.OnDrawPanel := DrawPanel;
  fStatusBar.Panels[cSearchPanel].Style := psOwnerDraw;
  fStatusBar.Panels[cUserPanel].Style := psOwnerDraw;
  fStatusBar.Panels[cUserPanel].Text := sModified;
end;

destructor TStatusBarMgr.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fModifiedGlyph);
  FreeAndNil(fSearchGlyph);
  inherited;
end;

procedure TStatusBarMgr.DrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
  {Handles status bar's OnDrawPanel event called whenever an owner-draw status
  panel needs to be displayed.
    @param StatusBar [in] Reference to status bar containing panel.
    @param Panel [in] Reference to status bar panel to be drawn.
    @param Rect [in] Bounding rectangle of panel relative to status bar.
  }

  procedure DrawGlyphInPanel(const Glyph: TBitmap; const Transparent: TColor);
    {Draws a glyph at the left of a status bar panel, centred vertically.
      @param Glyph [in] Glyph to be displayed.
      @param Transparent [in] Colour key to be drawn transparently.
    }
  var
    BoundsRect: TRectEx;  // Bounding rectangle of panel
    DestRect: TRect;      // Rectangle in panel where glyph is drawn
    SrcRect: TRect;       // Bounding rectangle of glyph
  begin
    BoundsRect := Rect;
    DestRect := TRectEx.CreateBounds(
      BoundsRect.Left + 2,
      BoundsRect.Top + (BoundsRect.Height - Glyph.Height) div 2,
      Glyph.Width,
      Glyph.Height
    );
    SrcRect := TRectEx.CreateBounds(0, 0, Glyph.Width, Glyph.Height);
    StatusBar.Canvas.BrushCopy(DestRect, Glyph, SrcRect, Transparent);
  end;

  procedure DrawTextInPanel(const LeftMargin, RightMargin: Integer;
    Text: string);
    {Draws left aligned text in current status panel.
      @param LeftMargin [in] Offset of text from left of panel.
      @param RightMargin [in] Defines right hand edge of test in panel. Text is
        clipped and an ellipsis drawn if it overflows this margin.
      @param Text [in] Text to be displayed.
    }
  var
    BoundsRect: TRectEx;  // bounding rectangle of panel
    TextRect: TRect;      // rectangle in which to display text
    TextH: Integer;       // height of text
  begin
    BoundsRect := Rect;
    TextH := StatusBar.Canvas.TextHeight(Text);
    // need this rect calculation since vertical align flag to Canvas.TextRect
    // doesn't work so we need to set top of text rect to required value
    TextRect := TRectEx.Create(
      BoundsRect.Left + LeftMargin,
      BoundsRect.Top + (BoundsRect.Height - TextH) div 2,
      BoundsRect.Right - RightMargin,
      Rect.Bottom
    );
    StatusBar.Canvas.TextRect(
      TextRect, Text, [tfLeft, tfTop, tfEndEllipsis]
    );
  end;

begin
  // Clear the panel
  StatusBar.Canvas.FillRect(Rect);
  case Panel.ID of
    cSearchPanel:
    begin
      // We do nothing else if there's no glyph or search info not to be shown
      if not Assigned(fSearchGlyph) or not fSearchInfoVisible then
        Exit;
      DrawGlyphInPanel(fSearchGlyph, clFuchsia);
      DrawTextInPanel(fSearchGlyph.Width + 6, 0, Panel.Text);
    end;
    cUserPanel:
    begin
      // We do nothing else if there's no glyph or user db info not to be shown
      if not Assigned(fModifiedGlyph) or not fUserDBInfoVisible then
        Exit;
      DrawGlyphInPanel(fModifiedGlyph, clWhite);
      DrawTextInPanel(fModifiedGlyph.Width + 6, 12, Panel.Text);
    end
    else
      // Do nothing: we only support drawing of second panel (Panel.ID=1)
  end;
end;

procedure TStatusBarMgr.HideSearchInfo;
  {Prevents search information from being displayed in status bar.
  }
begin
  // This method does not directly hide the information, but flags that it
  // should be hidden then causes the status bar to update itself using stored
  // data. The DrawPanel method is called by the status bar to draw the required
  // panel.
  fSearchInfoVisible := False;
  fStatusBar.Repaint;
end;

procedure TStatusBarMgr.HideUserDBInfo;
  {Prevents user defined database information from being displayed in status
  bar.
  }
begin
  // This method does not directly hide the information, but flags that it
  // should be hidden then causes the status bar to update itself using stored
  // data. The DrawPanel method is called by the status bar to draw the required
  // panel.
  fUserDBInfoVisible := False;
  fStatusBar.Repaint;
end;

procedure TStatusBarMgr.ShowHint(const Hint: string);
  {Displays a hint in status bar and restores previous message if no hint is
  provided.
    @param Hint [in] Hint to be displayed or '' to restore previous message.
  }
begin
  if Hint <> '' then
  begin
    // We have hint: hide status bar info and display hint
    // we use simple panel to display hint in whole of status bar
    fStatusBar.SimplePanel := True;
    fStatusBar.SimpleText := Hint;
  end
  else
  begin
    // No hint: we restore normal status bar info by switching off simple panel
    fStatusBar.SimplePanel := False;
    fStatusBar.SimpleText := '';
  end;
  // Force status bar to repaint immediately
  Application.ProcessMessages;
end;

procedure TStatusBarMgr.ShowSearchInfo;
  {Causes information about a search to be displayed in status bar.
  }
resourcestring
  // Text displayed in search panel
  sNoSearch = 'All snippets selected';
  sSearchActive = '%d snippets selected';
begin
  // This method does not directly display the information, but records it and
  // causes the status bar to update itself using the stored data. The DrawPanel
  // method is called by the status bar to draw the panel.

  // Store text describing search result
  if Query.LatestSearch.Criteria.IsNull then
    fStatusBar.Panels[cSearchPanel].Text := sNoSearch
  else
    fStatusBar.Panels[cSearchPanel].Text
      := Format(sSearchActive, [Query.Selection.Count]);
  // Store glyph that indicates latest search type
  fSearchGlyph.Assign((Query.LatestSearch.Criteria as ISearchUIInfo).Glyph);
  // Ensure search info panel of status bar is displayed
  fSearchInfoVisible := True;
  // Force status bar to repaint itself
  fStatusBar.Repaint;
end;

procedure TStatusBarMgr.ShowSimpleMessage(const Msg: string);
  {Displays a simple text message in first panel of status bar and hides results
  displayed in other panels.
    @param Msg [in] Message to be displayed.
  }
begin
  // Set first panel of status bar to message and force repaint
  fStatusBar.Panels[cSimplePanel].Text := Msg;
  Application.ProcessMessages;
  // Hide any info displayed in other panels
  HideSearchInfo;
  HideUserDBInfo;
end;

procedure TStatusBarMgr.ShowSnippetsInfo;
  {Displays snippets statistics in status bar.
  }
var
  TotalSnippets: Integer;     // number of snippets in database
  TotalUserSnippets: Integer; // number of user-defined snippets in database
  TotalCategories: Integer;   // total number of categories
  DisplayText: string;        // text to display in database
resourcestring
  // status bar messages
  sNoUserInfo = '%0:d snippets in %1:d categories';
  sWithUserInfo = '%0:d snippets (%2:d user defined) in %1:d categories';
begin
  // Calculate database stats
  TotalSnippets := Database.Snippets.Count;
  TotalUserSnippets := Database.Snippets.Count(True);
  TotalCategories := Database.Categories.Count;
  // Build display text and display it
  if TotalUserSnippets = 0 then
    DisplayText := Format(sNoUserInfo, [TotalSnippets, TotalCategories])
  else
    DisplayText := Format(
      sWithUserInfo, [TotalSnippets, TotalCategories, TotalUserSnippets]
    );
  fStatusBar.Panels[cDBPanel].Text := DisplayText;
end;

procedure TStatusBarMgr.ShowUserDBInfo;
  {Displays information about state of user defined database.
  }
begin
  // This method does not directly display the information, but records whether
  // panel needs to be drawn or hidden. The DrawPanel method is called by the
  // status bar to draw the panel.

  // We hide message if database not updated
  fUserDBInfoVisible := (Database as IDatabaseEdit).Updated;
  fStatusBar.Repaint;
end;

procedure TStatusBarMgr.Update;
  {Updates status bar and refreshes information displayed.
  }
begin
  ShowSnippetsInfo;
  ShowUserDBInfo;
  ShowSearchInfo;
end;

end.

