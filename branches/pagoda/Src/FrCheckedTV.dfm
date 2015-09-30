object CheckedTVFrame: TCheckedTVFrame
  Left = 0
  Top = 0
  Width = 201
  Height = 210
  TabOrder = 0
  TabStop = True
  OnEnter = FrameEnter
  object tvChecked: TTreeView
    Left = 0
    Top = 0
    Width = 201
    Height = 210
    Align = alClient
    HideSelection = False
    Images = ilChecks
    Indent = 19
    ReadOnly = True
    ShowButtons = False
    ShowLines = False
    ShowRoot = False
    TabOrder = 0
    OnCollapsing = tvCheckedCollapsing
    OnCreateNodeClass = tvCheckedCreateNodeClass
    OnDeletion = tvCheckedDeletion
    OnKeyPress = tvCheckedKeyPress
    OnMouseDown = tvCheckedMouseDown
    OnMouseMove = tvCheckedMouseMove
  end
  object ilChecks: TImageList
    Left = 152
    Top = 64
  end
end
