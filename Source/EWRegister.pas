{***************************************************************************}
{                                                                           }
{           EasyWeb - Bootstrap Framework for Delphi                        }
{                                                                           }
{           Copyright (c) 2019 Graham Murt                                  }
{                                                                           }
{           https://bitbucket.org/gmurt/easyweb/                            }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

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
  EWLayout,
  EWNavBar,
  EWDialogs,
  EWCheckGroup,
  EWTable,
  EWHtmlForm,
  EWSweetAlert,
  EWCustomHTML;


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
  RegisterComponents('EasyWeb', [TEWComboBox]);

  RegisterComponents('EasyWeb', [TEWMemo]);
  RegisterComponents('EasyWeb', [TEWProgressBar]);
  RegisterComponents('EasyWeb', [TEWCheckBox]);
  RegisterComponents('EasyWeb', [TEWLabel]);
  RegisterComponents('EasyWeb', [TEWImage]);
  RegisterComponents('EasyWeb', [TEWLayout]);
  RegisterComponents('EasyWeb', [TEWLayoutGrid]);
  RegisterComponents('EasyWeb', [TEWTimer]);
  RegisterComponents('EasyWeb', [TEWNavBar]);
  RegisterComponents('EasyWeb', [TEWDialog]);
  RegisterComponents('EasyWeb', [TEWCheckBoxGroup]);
  RegisterComponents('EasyWeb', [TEWRadioGroup]);
  RegisterComponents('EasyWeb', [TEWTable]);
  RegisterComponents('EasyWeb', [TEWHTMLForm]);
  RegisterComponents('EasyWeb', [TEWSweetAlert]);
  RegisterComponents('EasyWeb', [TEWCustomHTML]);

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
