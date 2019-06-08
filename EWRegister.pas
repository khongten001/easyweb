unit ewRegister;

interface

procedure Register;

implementation

uses
  ToolsApi,
  DesignIntf,
  DesignEditors,
  Classes,
  EWBase,
  EWForm,
  EWServerControllerBase,
  EWButtons,
  EWInputs,
  EWProgressBars,
  EWCheckBox,
  EWLabel,
  EWIdeWizard,
  EWIdeHelpers,
  EWImages,
  EWTimer;

procedure Register;
begin
  RegisterPackageWizard(TEWApplicationWizard.Create);
  RegisterPackageWizard(TEWFormWizard.Create);

  RegisterCustomModule(TewForm, TCustomModule);
  RegisterCustomModule(TewBaseServerController, TCustomModule);
  RegisterCustomModule(TewForm, TCustomModule);
  RegisterComponents('EasyWeb', [TewButton]);
  RegisterComponents('EasyWeb', [TewDropDown]);
  RegisterComponents('EasyWeb', [TewButtonGroup]);
  RegisterComponents('EasyWeb', [TewInput]);
  RegisterComponents('EasyWeb', [TewProgressBar]);
  RegisterComponents('EasyWeb', [TewCheckBox]);
  RegisterComponents('EasyWeb', [TewLabel]);
  RegisterComponents('EasyWeb', [TEWImage]);
  RegisterComponents('EasyWeb', [TEWTimer]);
end;

end.
