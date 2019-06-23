program SweetAlertDemo;

uses
  Vcl.Forms,
  ewStartup,
  EWServerControllerBase,
  ServerController in 'ServerController.pas' {EWServerController: TEWBaseServerController},
  SessionDataUnit in 'SessionDataUnit.pas' {EWSessionData: TDataModule},
  Unit1 in 'Unit1.pas' {Form108: TEWForm};

{$R *.res}

begin

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  EWRun;

end.