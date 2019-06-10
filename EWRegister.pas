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
  EWTimer,
  EWTypes,
  EWSpacer;

type
  TEWFontFamilyList = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

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
  RegisterComponents('EasyWeb', [TEWSpacer]);
  RegisterComponents('EasyWeb', [TEWTimer]);
  RegisterPropertyEditor(TypeInfo(string), TEWFont, 'Family', TEWFontFamilyList);

end;

{ TEWFontFamilyList }

function TEWFontFamilyList.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList];
end;

procedure TEWFontFamilyList.GetValues(Proc: TGetStrProc);
begin
  // provide a list of system sounds
  Proc('Arial, Helvetica, sans-serif');
  Proc('"Arial Black", Gadget, sans-serif');
  Proc('"Comic Sans MS", cursive, sans-serif');
  Proc('Impact, Charcoal, sans-serif');
  Proc('"Lucida Sans Unicode", "Lucida Grande", sans-serif');
  Proc('Tahoma, Geneva, sans-serif');
  Proc('"Trebuchet MS", Helvetica, sans-serif');
  Proc('Verdana, Geneva, sans-serif');
  Proc('Georgia, serif');
  Proc('"Palatino Linotype", "Book Antiqua", Palatino, serif');
  Proc('"Times New Roman", Times, serif');
  Proc('"Courier New", Courier, monospace');
  Proc('"Lucida Console", Monaco, monospace');
end;

end.
