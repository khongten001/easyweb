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

unit EWIntf;

interface

uses Classes, EWTypes;

type
  IEWForm = interface
    ['{F409A6AB-8C59-41FE-93A9-4F70B5584D86}']
    function GetHtml: string;
    function SessionID: string;
    procedure ForceReload;
    property Html: string read GetHtml;

  end;





  IEWBaseComponent = interface
    ['{9097EA58-A758-4100-BF04-B4AA2F42AA7B}']

    procedure DoEvent(AParams: TStrings);

    function GetName: string;
    function GetHasChanged: Boolean;
    function GetScript: string;
    procedure GetGlobalVars(AStrings: TStrings);
    procedure GetRequiredThirdPartySrc(AStrings: TStrings);
    function GetHtml: string;
    procedure Changed;
    property Html: string read GetHtml;
    property HasChanged: Boolean read GetHasChanged;
    property Name: string read GetName;
    property Script: string read GetScript;

  end;

  IEWBaseVisualObject = interface
    ['{C55735AD-4A52-4E28-9F0B-DB1BDEC998A4}']
    //function CssCommaText: string;
    function GetScript: string;
    procedure GetEventListners(AListners: TStrings);
    procedure GetGlobalVars(AStrings: TStrings);
    procedure DoMouseEnter(AParams: TStrings);
    procedure DoMouseLeave(AParams: TStrings);
    procedure DoClick(AParams: TStrings);
    procedure DoRightClick(AParams: TStrings);
    procedure DoDblClick(AParams: TStrings);
    procedure DoOnChange(AParams: TStrings);
    property Script: string read GetScript;
  end;

  IEWBaseObjectItemClickable = interface
    ['{8FB82898-F12F-42E0-849F-F5A7666D2C8C}']
    function GetItems: TStrings;
    function GetItemIndex: integer;

    procedure SetItems(const Value: TStrings);
    procedure SetItemIndex(Value: integer);
    property Items: TStrings read GetItems write SetItems;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
  end;

  IEWInput = interface
    ['{1A2FFFC0-1068-47D2-9A6C-369435365999}']
    function GetPlaceHolder: string;
    function GetText: string;
    procedure DoOnEnter;
    procedure DoOnExit;
    procedure DoOnKeyDown(AParams: TStrings);
    procedure DoOnKeyPress(AParams: TStrings);
    procedure DoOnKeyUp(AParams: TStrings);
    procedure SetText(const Value: string);
    procedure SetPlaceHolder(const AText: string);
    property PlaceHolder: string read GetPlaceHolder write SetPlaceHolder;
    property Text: string read GetText write SetText;
  end;

  IEWButton = interface
    ['{4E9F7BF6-E14F-48A4-9CE5-AAD6AF10AA2F}']
    function GetButtonType: TewButtonType;
    procedure SetButtonType(const Value: TewButtonType);
    function GetText: string;
    procedure SetText(const Value: string);
    property ButtonType: TewButtonType read GetButtonType write SetButtonType;
    property Text: string read GetText write SetText;
  end;

  IEWButtonGroup = interface
    ['{17311FF1-9315-452E-A3B4-FF51064F56EB}']
    function GetButtonType: TewButtonType;
    procedure SetButtonType(const Value: TewButtonType);
    function GetText: string;
    procedure SetText(const Value: string);
    property ButtonType: TewButtonType read GetButtonType write SetButtonType;
    property Text: string read GetText write SetText;
  end;

  IEWProgressBar = interface
    ['{C617A3DA-89C6-4C9F-BFB2-9D5C92244048}']
    function GetMax: integer;
    function GetMin: integer;
    procedure SetMax(const Value: integer);
    procedure SetMin(const Value: integer);
    function GetPosition: integer;
    procedure SetPosition(const Value: integer);
    property Max: integer read GetMax write SetMax;
    property Min: integer read GetMin write SetMin;
    property Position: integer read GetPosition write SetPosition;
  end;

  IEWCheckBox = interface
    ['{32E96CDF-3CF6-472B-B54B-A9741AD72023}']
    function GetChecked: Boolean;
    function GetText: string;
    procedure SetText(const Value: string);
    procedure SetChecked(Value: Boolean);
    property Checked: Boolean read GetChecked write SetChecked;
    property Text: string read GetText write SetText;
  end;

  IEWLabel = interface
    ['{F3599CDF-90E6-4A8C-9FDA-3228E2F52369}']
    function GetText: string;
    procedure SetText(const Value: string);
    property Text: string read GetText write SetText;
  end;

  IEWImage = interface
    ['{EB70D8DD-3838-4A74-982D-3C0CF6E1F172}']
  end;

  IEWLayout = interface
    ['{94056032-0A61-443A-A354-94668A118032}']
  end;

  IEWLayoutGrid = interface
    ['{135D1656-1DFA-4AAD-BC6C-CA6559F8F4F7}']
  end;

  IEWTimer = interface
    ['{A694881C-5FD0-458E-9964-23E5B3D2CD40}']
    procedure DoTimer;
  end;

  IEWNavBar = interface
    ['{2EE70962-FAF0-4952-98CC-E9AAB88975BC}']
    procedure DoItemClick(ASender: TObject; AData: string);
  end;

  IEWCheckGroup = interface
  ['{BF4D7D4B-7C53-4E92-B4A3-5758B78CE558}']

  end;

  IEWTable = interface
    ['{11A23B05-AC88-4B3C-A475-D04CE905F2A4}']

  end;

  IEWHTMLForm = interface
    ['{A7DE2A3A-4B61-454D-B62B-DFDF87F6182E}']

  end;

    IEWCustomHTML = interface
    ['{5FCBFE57-B074-4EC2-953F-042CCB9504A1}']

  end;




implementation

end.
