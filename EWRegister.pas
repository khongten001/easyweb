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
  EWEdits,
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
  RegisterCustomModule(TEWForm, TCustomModule);
  RegisterCustomModule(TEWBaseServerController, TCustomModule);
  RegisterCustomModule(TEWForm, TCustomModule);
  RegisterComponents('EasyWeb', [TEWButton]);
  RegisterComponents('EasyWeb', [TEWDropDown]);
  RegisterComponents('EasyWeb', [TEWButtonGroup]);
  RegisterComponents('EasyWeb', [TEWEdit]);
  RegisterComponents('EasyWeb', [TEWProgressBar]);
  RegisterComponents('EasyWeb', [TEWCheckBox]);
  RegisterComponents('EasyWeb', [TEWLabel]);
  RegisterComponents('EasyWeb', [TEWImage]);
  RegisterComponents('EasyWeb', [TEWTimer]);
end;

end.
