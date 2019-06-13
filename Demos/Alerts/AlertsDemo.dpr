program AlertsDemo;

uses
  Vcl.Forms,
  ewStartup,
  EWServerControllerBase,
  ServerController in 'ServerController.pas' {EWServerController: TEWBaseServerController},
  Unit1 in 'Unit1.pas' {Form62: TEWForm};

{$R *.res}

begin

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  EWRun;

end.