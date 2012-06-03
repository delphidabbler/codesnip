inherited DependenciesDlg: TDependenciesDlg
  Caption = 'DependenciesDlg'
  ExplicitWidth = 474
  ExplicitHeight = 354
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    object pcBody: TPageControl
      Left = 0
      Top = 0
      Width = 377
      Height = 281
      ActivePage = tsDependsUpon
      Align = alClient
      TabOrder = 0
      OnMouseDown = pcBodyMouseDown
      object tsDependsUpon: TTabSheet
        Caption = 'Depends Upon'
        object lblCircularRef: TLabel
          Left = 0
          Top = 240
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
          Width = 369
          Height = 237
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
      object tsRequiredBy: TTabSheet
        Caption = 'Required By'
        ImageIndex = 1
        object lblNoDependents: TLabel
          Left = 8
          Top = 8
          Width = 3
          Height = 13
        end
        object lbDependents: TListBox
          Left = 0
          Top = 0
          Width = 369
          Height = 253
          Style = lbOwnerDrawFixed
          Align = alClient
          TabOrder = 0
          OnDrawItem = lbDependentsDrawItem
        end
      end
    end
  end
end
