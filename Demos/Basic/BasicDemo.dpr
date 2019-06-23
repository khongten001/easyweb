program BasicDemo;

uses
  Vcl.Forms,
  ewStartup,
  EWServerControllerBase,
  ServerController in 'ServerController.pas' {EWServerController: TEWBaseServerController},
  Unit1 in 'Unit1.pas' {Form1: TEWForm},
  Unit2 in 'Unit2.pas' {Form2: TEWForm},
  SessionDataUnit in 'SessionDataUnit.pas' {EWSessionData: TDataModule};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  EWRun;

end.