inherited DependenciesDlg: TDependenciesDlg
  Caption = 'DependenciesDlg'
  ExplicitWidth = 474
  ExplicitHeight = 354
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    object lblCircularRef: TLabel
      Left = 0
      Top = 269
      Width = 135
      Height = 13
      Caption = 'Circular Reference Detected'
      Visible = False
    end
    object lblNoDependencies: TLabel
      Left = 0
      Top = 0
      Width = 3
      Height = 13
    end
    object tvDependencies: TTreeView
      Left = 0
      Top = 0
      Width = 377
      Height = 257
      Align = alTop
      Indent = 19
      ReadOnly = True
      ShowButtons = False
      ShowLines = False
      ShowRoot = False
      TabOrder = 0
      OnCollapsing = tvDependenciesCollapsing
    end
  end
end
