inherited BugReportBaseDlg: TBugReportBaseDlg
  Caption = 'BugReportBaseDlg'
  ClientHeight = 1844
  ClientWidth = 2561
  Font.Height = -200
  ExplicitWidth = 2585
  ExplicitHeight = 1908
  PixelsPerInch = 168
  TextHeight = 265
  inherited bvlBottom: TBevel
    Left = 77
    Top = 2777
    Width = 3537
    Height = 23
    Margins.Left = 28
    Margins.Top = 28
    Margins.Right = 28
    Margins.Bottom = 28
    ExplicitLeft = 77
    ExplicitTop = 2777
    ExplicitWidth = 3537
    ExplicitHeight = 23
  end
  inherited pnlBody: TPanel
    Left = 77
    Top = 77
    Width = 786
    Height = 2637
    Margins.Left = 28
    Margins.Top = 28
    Margins.Right = 28
    Margins.Bottom = 28
    Color = clWindow
    ParentBackground = False
    ExplicitLeft = 77
    ExplicitTop = 77
    ExplicitWidth = 786
    ExplicitHeight = 2637
    object lblBugTracker: TLabel
      AlignWithMargins = True
      Left = 7
      Top = 448
      Width = 186
      Height = 23
      Cursor = crHandPoint
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Go to the Bug Tracker'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = lblBugTrackerClick
    end
    object lblBugTrackerKey: TLabel
      Left = 182
      Top = 448
      Width = 617
      Height = 265
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '(Alt+B)'
    end
  end
  inherited btnHelp: TButton
    Left = 2928
    Top = 2851
    Width = 702
    Height = 236
    Margins.Left = 28
    Margins.Top = 28
    Margins.Right = 28
    Margins.Bottom = 28
    ExplicitLeft = 2928
    ExplicitTop = 2851
    ExplicitWidth = 702
    ExplicitHeight = 236
  end
  inherited btnClose: TButton
    Left = 711
    Top = 931
    Width = 229
    Height = 77
    Margins.Left = 9
    Margins.Top = 9
    Margins.Right = 9
    Margins.Bottom = 9
    ExplicitLeft = 711
    ExplicitTop = 931
    ExplicitWidth = 229
    ExplicitHeight = 77
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
