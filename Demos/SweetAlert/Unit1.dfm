object Form108: TForm108
  Left = 0
  Top = 0
  Width = 601
  Height = 326
  Color = clWhite
  PixelsPerInch = 96
  TextHeight = 13
  object EWButton1: TEWButton
    Left = 24
    Top = 24
    Width = 137
    Height = 41
    OnClick = EWButton1Click
    ButtonType = btSuccess
    Text = 'Success'
  end
  object EWButton3: TEWButton
    Left = 24
    Top = 80
    Width = 137
    Height = 41
    OnClick = EWButton3Click
    ButtonType = btWarning
    Text = 'Warning'
  end
  object EWButton2: TEWButton
    Left = 24
    Top = 136
    Width = 137
    Height = 41
    OnClick = EWButton2Click
    ButtonType = btDanger
    Text = 'Warning'
  end
  object EWButton4: TEWButton
    Left = 24
    Top = 224
    Width = 137
    Height = 41
    OnClick = EWButton4Click
    ButtonType = btInfo
    Text = 'Confirmation'
  end
  object EWLabel1: TEWLabel
    Left = 200
    Top = 224
    Width = 329
    Height = 41
    Font.Variant = fvNormal
    Font.Size = 20
    Font.Style = []
    Font.Color = clBlack
    Text = ''
  end
  object EWSweetAlert1: TEWSweetAlert
    OnConfirmResult = EWSweetAlert1ConfirmResult
    OnButtonClicked = False
    Left = 216
    Top = 56
  end
end
