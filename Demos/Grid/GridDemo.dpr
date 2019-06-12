program GridDemo;

uses
  Vcl.Forms,
  ewStartup,
  EWServerControllerBase,
  ServerController in 'ServerController.pas' {EWServerController: TEWBaseServerController},
  Unit1 in 'Unit1.pas' {Form53: TEWForm};

{$R *.res}

begin

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  EWRun;

end.