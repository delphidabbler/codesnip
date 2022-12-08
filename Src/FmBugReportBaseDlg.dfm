inherited BugReportBaseDlg: TBugReportBaseDlg
  Caption = 'BugReportBaseDlg'
  ClientWidth = 967
  ExplicitWidth = 979
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    Color = clWindow
    ParentBackground = False
    object lblBugTracker: TLabel
      AlignWithMargins = True
      Left = 7
      Top = 448
      Width = 200
      Height = 30
      Cursor = crHandPoint
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Go to the Bug Tracker'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = lblBugTrackerClick
    end
    object lblBugTrackerKey: TLabel
      Left = 231
      Top = 448
      Width = 64
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '(Alt+B)'
    end
  end
  object alMain: TActionList
    Left = 8
    Top = 296
    object actBugTracker: TAction
      Caption = 'actBugTracker'
      ShortCut = 32834
      OnExecute = actBugTrackerExecute
    end
  end
end
