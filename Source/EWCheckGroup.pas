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

unit EWCheckGroup;

interface

uses Classes, EWIntf, EWBase, VCL.Graphics, EWTypes;

type
  TEWBaseCheckGroup = class;

  TEWCheckGroupType = (cgCheckBoxGroup, cgRadioGroup);

  TEWCheckGroupItem = class(TCollectionItem)
  private
    FCheckGroup: TEWBaseCheckGroup;
    FText: string;
    FEnabled: Boolean;
    FChecked: Boolean;
    procedure SetText(const Value: string);
    procedure Changed;
    procedure SetEnabled(const Value: Boolean);
    procedure SetChecked(const Value: Boolean);
  public
    function GetHtml: string;
    procedure Assign(Source: TPersistent); override;
  published
    constructor Create(Collection: TCollection); override;
    property Text: string read FText write SetText;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Checked: Boolean read FChecked write SetChecked;
  end;

  TEWCheckGroupItemCollection = class(TCollection)
  private
    FCheckGroup: TEWBaseCheckGroup;
  protected
    function GetItem(Index: Integer): TEWCheckGroupItem;
    procedure SetItem(Index: Integer; Value: TEWCheckGroupItem);
    function GetHtml: string;
  public
    constructor Create(ACheckGroup: TEWBaseCheckGroup);
    function Add: TEWCheckGroupItem;
    function Insert( Index: Integer ): TEWCheckGroupItem;
    property Items[index: Integer]: TEWCheckGroupItem read GetItem write SetItem; default;
  end;

  TEWBaseCheckGroup = class(TEWBaseObject, IEWCheckGroup)
  private
    FItems: TEWCheckGroupItemCollection;
    FCheckType: TEWCheckGroupType;
    FOnItemClick: TEWClickItemEvent;
    procedure SetCheckType(const Value: TEWCheckGroupType);
    procedure SetItems(const Value: TEWCheckGroupItemCollection);
  protected
    function ReplaceTokens(AHtml: string): string; override;
    procedure GetEventListners(AListners: TStrings); override;
    function GenerateHtml: string; override;
    procedure Paint; override;
    procedure DoEvent(AParams: TStrings); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property CheckType: TEWCheckGroupType read FCheckType write SetCheckType default cgRadioGroup;
    property Items: TEWCheckGroupItemCollection read FItems write SetItems;
    property OnClickItem: TEWClickItemEvent read FOnItemClick write FOnItemClick;
  end;

  TEWRadioGroup = class(TEWBaseCheckGroup)
  private
    FItemIndex: integer;
    procedure SetItemIndex(const Value: integer);
  protected
    procedure DoEvent(AParams: TStrings); override;

  public
    property ItemIndex: integer read FItemIndex write SetItemIndex default -1;
  end;

  TEWCheckBoxGroup = class(TEWBaseCheckGroup)
  protected
    procedure DoEvent(AParams: TStrings); override;
  end;

implementation

uses Types, SysUtils, EWConst;

{ TEWLabel }


constructor TEWBaseCheckGroup.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TEWCheckGroupItemCollection.Create(Self);

end;

destructor TEWBaseCheckGroup.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TEWBaseCheckGroup.DoEvent(AParams: TStrings);
var
  AIndex: integer;
begin
  inherited;
  if AParams.Values[C_EVENT] = C_CLICK then
  begin
    AIndex := StrToInt(AParams.Values[C_INDEX]);
    if Assigned(FOnItemClick) then
      FOnItemClick(Self, FItems[AIndex].Text, AIndex);
  end;

end;

function TEWBaseCheckGroup.GenerateHtml: string;
begin
  inherited;
  Result := ReplaceTokens(C_HTML_CHECKGROUP);
end;

procedure TEWBaseCheckGroup.GetEventListners(AListners: TStrings);
var
  ICount: integer;
begin
  inherited;
  for ICount := 0 to FItems.Count-1 do
    AddObjectEvent(Name+C_ITEM+ICount.ToString, C_CLICK, [C_INDEX+'='+ICount.ToString], AListners, '');
end;

procedure TEWBaseCheckGroup.Paint;
var
  r: TRect;
  AStr: string;
  AFormat: TTextFormat;
begin
  inherited;
  r := ClientRect;
  AFormat := [tfSingleLine];
  AStr := DesignTimeCaption;
  Canvas.TextRect(r, AStr, AFormat);
end;


function TEWBaseCheckGroup.ReplaceTokens(AHtml: string): string;
begin
  Result := inherited ReplaceTokens(AHtml);
  Result := StringReplace(Result, '%items%', FItems.GetHtml, []);
end;

procedure TEWBaseCheckGroup.SetCheckType(const Value: TEWCheckGroupType);
begin
  if FCheckType <> Value then
  begin
    FCheckType := Value;
    Changed;
  end;
end;

procedure TEWBaseCheckGroup.SetItems(const Value: TEWCheckGroupItemCollection);
begin
  FItems.Assign(Value);
end;

{ TEWCheckGroupItem }

procedure TEWCheckGroupItem.Assign(Source: TPersistent);
begin
  inherited;
  Text := (Source as TEWCheckGroupItem).Text;
  Enabled := (Source as TEWCheckGroupItem).Enabled;
end;

procedure TEWCheckGroupItem.Changed;
var
  I: IEWBaseComponent;
begin
  if Supports(FCheckGroup, IEWBaseComponent, I) then
    i.Changed;
end;

constructor TEWCheckGroupItem.Create(Collection: TCollection);
begin
  inherited;
  FCheckGroup := TEWCheckGroupItemCollection(Collection).FCheckGroup;
  FEnabled := True;
  FText := '';
  FChecked := False;
end;

function TEWCheckGroupItem.GetHtml: string;
var
  AName: string;
  AChecked: string;
  ADisabled: string;
  AType: string;
begin
  AType := '';
  AChecked := '';
  ADisabled := '';
  if (FCheckGroup is TEWRadioGroup) then AType := C_RADIO;
  if (FCheckGroup is TEWCheckBoxGroup) then AType := C_CHECKBOX;
  if Checked then AChecked := C_CHECKED;
  if FEnabled = False then ADisabled := C_DISABLED;
  AName := FCheckGroup.Name+C_ITEM+Index.ToString;

  Result := C_HTML_CHECKGROUP_ITEM;
  Result := StringReplace(Result, '%id%', AName, []);
  Result := StringReplace(Result, '%name%', AName, []);
  Result := StringReplace(Result, '%type%', AType, []);
  Result := StringReplace(Result, '%index%', Index.ToString, []);
  Result := StringReplace(Result, '%checked%', AChecked, []);
  Result := StringReplace(Result, '%disabled%', ADisabled, []);
  Result := StringReplace(Result, '%text%', FText, []);
end;

procedure TEWCheckGroupItem.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    Changed;
  end;
end;

procedure TEWCheckGroupItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TEWCheckGroupItem.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

{ TEWCheckGroupItemCollection }

function TEWCheckGroupItemCollection.Add: TEWCheckGroupItem;
begin
  Result := TEWCheckGroupItem.Create(Self);
end;

constructor TEWCheckGroupItemCollection.Create(ACheckGroup: TEWBaseCheckGroup);
begin
  inherited Create(TEWCheckGroupItem);
  FCheckGroup := ACheckGroup;
end;

function TEWCheckGroupItemCollection.GetHtml: string;
var
  ICount: integer;
begin
  Result := '';
  for ICount := 0 to Count-1 do
    Result := Result + Items[ICount].GetHtml;

end;

function TEWCheckGroupItemCollection.GetItem(Index: Integer): TEWCheckGroupItem;
begin
  Result := inherited Items[index] as TEWCheckGroupItem;
end;

function TEWCheckGroupItemCollection.Insert(Index: Integer): TEWCheckGroupItem;
begin
  Result := inherited insert( index ) as TEWCheckGroupItem;
end;

procedure TEWCheckGroupItemCollection.SetItem(Index: Integer;
  Value: TEWCheckGroupItem);
begin
  inherited SetItem(index, Value);
end;

{ TEWRadioGroup }

procedure TEWRadioGroup.DoEvent(AParams: TStrings);
begin
  inherited DoEvent(AParams);
  if AParams.Values[C_EVENT] = C_CLICK then
    ItemIndex := StrToIntDef(AParams.Values[C_INDEX], -1);
end;

procedure TEWRadioGroup.SetItemIndex(const Value: integer);
var
  ICount: integer;
begin
  if FItemIndex <> Value then
  begin
    FItemIndex := Value;
    for ICount := 0 to FItems.Count-1 do
    FItems[ICount].Checked := FItemIndex = ICount;

    Changed;
  end;
end;

{ TEWCheckBoxGroup }

procedure TEWCheckBoxGroup.DoEvent(AParams: TStrings);
var
  AIndex: integer;
begin
  inherited DoEvent(AParams);
  if AParams.Values[C_EVENT] = C_CLICK then
  begin
    AIndex := StrToIntDef(AParams.Values[C_INDEX], -1);
    FItems[AIndex].Checked := not FItems[AIndex].Checked;
  end;
end;

end.
