object bsBaseServerController: TbsBaseServerController
  OldCreateOrder = False
  Height = 150
  Width = 215
  object IdHTTPServer1: TIdHTTPServer
    Bindings = <>
    DefaultPort = 8080
    OnCommandGet = IdHTTPServer1CommandGet
    Left = 56
    Top = 32
  end
end
