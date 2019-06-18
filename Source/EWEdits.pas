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

unit EWEdits
;

interface

uses Classes, VCL.Controls, EWIntf, EWBase, EWTypes;

type
  TEWComboBox = class;

  TEWBaseEdit = class(TEWBaseObject, IEWInput)
  private
    FText: string;
    FPlaceHolder: string;
    FOnExit: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FOnKeyDown: TEWKeyEvent;
    FOnKeyUp: TEWKeyEvent;
    FReadOnly: Boolean;
    FRequired: Boolean;

    function GetText: string;
    procedure SetText(const Value: string);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetRequired(const Value: Boolean);
  protected
    procedure GetEventListners(AListners: TStrings); override;
    procedure DoOnEnter;
    procedure DoOnExit;
    function DesignTimeCaption: string; override;

    procedure DoEvent(AParams: TStrings); override;

    procedure DoOnKeyDown(AParams: TStrings);
    procedure DoOnKeyPress(AParams: TStrings);
    procedure DoOnKeyUp(AParams: TStrings);
    procedure DoOnChange(AParams: TStrings); override;
    function GetPlaceHolder: string;
    procedure SetPlaceHolder(const AText: string);
    procedure Paint; override;
    property OnChange;
    property OnKeyDown: TEWKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyUp: TEWKeyEvent read FOnKeyUp write FOnKeyUp;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Enabled;

    property PlaceHolder: string read GetPlaceHolder write SetPlaceHolder;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property Required: Boolean read FRequired write SetRequired default False;
    property Text: string read GetText write SetText;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;

  end;

  TEWEdit = class(TEWBaseEdit)
  private
    FInputType: TEWInputType;
    function GetInputTypeStr: string;
    procedure SetInputType(const Value: TEWInputType);
  protected
    function ReplaceTokens(AHtml: string): string; override;
    procedure GetEventListners(AListners: TStrings); override;
    function GenerateHtml: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property InputType: TEWInputType read FInputType write SetInputType default itText;
    property OnKeyDown;
    property OnKeyUp;
    property OnChange;
  end;

  TEWMemo = class(TEWBaseEdit)
  protected
    function ReplaceTokens(AHtml: string): string; override;
    procedure GetEventListners(AListners: TStrings); override;
    function GenerateHtml: string; override;
  end;

  TEWComboBoxItem = class(TCollectionItem)
  private
    FComboBox: TEWComboBox;
    FText: string;
    procedure SetText(const Value: string);
    procedure Changed;
  public
    function GetHtml: string;
    procedure Assign(Source: TPersistent); override;
  published
    constructor Create(Collection: TCollection); override;
    property Text: string read FText write SetText;
  end;

  TEWComboBoxItemCollection = class(TCollection)
  private
    FComboBox: TEWComboBox;
  protected
    function GetItem(Index: Integer): TEWComboBoxItem;
    procedure SetItem(Index: Integer; Value: TEWComboBoxItem);
  public
    constructor Create(AComboBox: TEWComboBox);
    function GetHtml: string;
    function Add: TEWComboBoxItem;
    function Insert( Index: Integer ): TEWComboBoxItem;
    property Items[index: Integer]: TEWComboBoxItem read GetItem write SetItem; default;
  end;

  TEWComboBox = class(TEWBaseEdit)
  private
    FItems: TEWComboBoxItemCollection;
    FItemIndex: integer;
    procedure DoChange;
    procedure SetItems(const Value: TEWComboBoxItemCollection);
    procedure SetItemIndex(const Value: integer);
  protected
    procedure GetEventListners(AListners: TStrings); override;
    function ReplaceTokens(AHtml: string): string; override;

    function GenerateHtml: string; override;
    procedure DoEvent(AParams: TStrings); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Items: TEWComboBoxItemCollection read FItems write SetItems;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property OnChange;
  end;

implementation

uses System.Types, VCL.Graphics, SysUtils, EWConst;

{ TEWBaseEdit }

constructor TEWBaseEdit.Create(AOwner: TComponent);
begin
  inherited;
  FPlaceHolder := '';
  FText := '';
  FRequired := False;
  FReadOnly := False;
end;

function TEWBaseEdit.DesignTimeCaption: string;
begin
  Result := Text;
  if Result = '' then
    Result := FPlaceHolder;
  if Result = '' then
    Result := Name;
end;

procedure TEWBaseEdit.DoEvent(AParams: TStrings);
var
  AEvent: string;
begin
  inherited DoEvent(AParams);
  AEvent := AParams.Values[C_EVENT];
  if AEvent = C_FOCUS then DoOnEnter;
  if AEvent = C_BLUR then DoOnExit;
  if AEvent = C_KEYDOWN then DoOnKeyDown(AParams);
  if AEvent = C_KEYUP then DoOnKeyUp(AParams);
end;

procedure TEWBaseEdit.DoOnChange(AParams: TStrings);
begin
  FText := AParams.Values[C_VALUE];
  inherited DoOnChange(AParams);
end;

procedure TEWBaseEdit.DoOnEnter;
begin
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TEWBaseEdit.DoOnExit;
begin
  if Assigned(FOnEnter) then
    FOnExit(Self);
end;

procedure TEWBaseEdit.DoOnKeyDown(AParams: TStrings);
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self, StrToInt(AParams.Values[C_VALUE]));
end;

procedure TEWBaseEdit.DoOnKeyPress(AParams: TStrings);
begin
  FText := AParams.Values[C_VALUE];
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TEWBaseEdit.DoOnKeyUp(AParams: TStrings);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self, StrToInt(AParams.Values[C_VALUE]));
end;

procedure TEWBaseEdit.GetEventListners(AListners: TStrings);
begin
  inherited;
  if Assigned(FOnKeyDown) then AddOnKeyDownEvent(AListners);
  if Assigned(FOnKeyUp) then AddOnKeyUpEvent(AListners);
  if Assigned(FOnEnter) then AddEnterEvent(AListners);
  if Assigned(FOnExit) then AddExitEvent(AListners);
end;



constructor TEWEdit.Create(AOwner: TComponent);
begin
  inherited;
  FInputType := itText;
end;

procedure TEWEdit.GetEventListners(AListners: TStrings);
begin
  inherited;
  if Assigned(OnChange) then AddOnKeyPressEvent(AListners);
end;

function TEWEdit.GenerateHtml: string;
begin
  Result := ReplaceTokens(C_HTML_EDIT);
end;

function TEWEdit.GetInputTypeStr: string;
begin
  case FInputType of
    itDate: Result := C_DATE;
    itDateTime: Result := C_DATETIME_LOCAL;
    itEmail: Result := C_EMAIL;
    itHidden: Result := C_HIDDEN;
    itMonthYear: Result := C_MONTH;
    itNumber: Result := C_NUMBER;
    itPassword: Result := C_PASSWORD;
    itText: Result := C_TEXT;
    itTime: Result := C_TIME;
    itUrl: Result := C_URL;
  end;
end;

function TEWEdit.ReplaceTokens(AHtml: string): string;
begin
  Result := inherited replaceTokens(AHtml);
  Result := StringReplace(Result, '%type%', GetInputTypeStr, []);
  Result := StringReplace(Result, '%placeholder%', FPlaceHolder, []);
  Result := StringReplace(Result, '%text%', FText, []);
end;

function TEWBaseEdit.GetPlaceHolder: string;
begin
  Result := FPlaceHolder;
end;

function TEWBaseEdit.GetText: string;
begin
  Result := FText;
end;

procedure TEWBaseEdit.Paint;
var
  ARect: TRect;
  AText: string;
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clSilver;
  Canvas.RoundRect(ClientRect, 8, 8);
  Canvas.Font.Size := 11;
  ARect := ClientRect;
  InflateRect(ARect, -6, -6);
  AText := DesignTimeCaption;
  Canvas.TextRect(ARect, AText, [tfVerticalCenter, tfSingleLine]);
end;

procedure TEWEdit.SetInputType(const Value: TEWInputType);
begin
  if FInputType <> Value then
  begin
    FInputType := Value;
    Changed;
  end;
end;

procedure TEWBaseEdit.SetPlaceHolder(const AText: string);
begin
  if AText <> FPlaceHolder then
  begin
    FPlaceHolder := AText;
    Changed;
  end;
end;

procedure TEWBaseEdit.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    Changed;
  end;
end;

procedure TEWBaseEdit.SetRequired(const Value: Boolean);
begin
  if FRequired <> Value then
  begin
    FRequired := Value;
    Changed;
  end;
end;

procedure TEWBaseEdit.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;


{ TEWMemo }

procedure TEWMemo.GetEventListners(AListners: TStrings);
begin
  inherited;
  if Assigned(OnChange) then AddOnKeyPressEvent(AListners);
end;

function TEWMemo.ReplaceTokens(AHtml: string): string;
begin
  Result := inherited ReplaceTokens(AHtml);
  Result := StringReplace(Result, '%text%', FText, []);
end;

function TEWMemo.GenerateHtml: string;
begin
  Result := ReplaceTokens(C_HTML_TEXT_AREA);
end;


{ TEWComboBox }

constructor TEWComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TEWComboBoxItemCollection.Create(Self);
end;

destructor TEWComboBox.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TEWComboBox.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TEWComboBox.DoEvent(AParams: TStrings);
var
  AEvent: string;
begin
  inherited DoEvent(AParams);
  AEvent := AParams.Values[C_EVENT];
  if AEvent = C_CHANGE then
  begin
    FItemIndex := StrToIntDef(AParams.Values[C_VALUE], -1);
    DoChange;
  end;
end;

procedure TEWComboBox.GetEventListners(AListners: TStrings);
begin
  inherited;
  if Assigned(OnChange) then AddObjectEvent(Name, C_CHANGE, [], AListners, 'document.getElementById(''' + Name +''').selectedIndex');
end;

function TEWComboBox.ReplaceTokens(AHtml: string): string;
begin
  Result := inherited ReplaceTokens(AHtml);
  Result := StringReplace(Result, '%items%', FItems.GetHtml, []);
end;

function TEWComboBox.GenerateHtml: string;
begin
  inherited;
  Result := ReplaceTokens(C_HTML_COMBOBOX);
end;

procedure TEWComboBox.SetItemIndex(const Value: integer);
begin
  if FItemIndex <> Value then
  begin
    FItemIndex := Value;
    DoChange;
    Changed;
  end;
end;

procedure TEWComboBox.SetItems(const Value: TEWComboBoxItemCollection);
begin
  FItems.Assign(Value);
  Changed;
end;

{ TEWComboBoxItem }

procedure TEWComboBoxItem.Assign(Source: TPersistent);
begin
  inherited;
  Text := (Source as TEWComboBoxItem).Text;
end;

procedure TEWComboBoxItem.Changed;
var
  I: IEWBaseComponent;
begin
  if Supports(FComboBox, IEWBaseComponent, I) then
    i.Changed;
end;

constructor TEWComboBoxItem.Create(Collection: TCollection);
begin
  inherited;
  FComboBox := TEWComboBoxItemCollection(Collection).FComboBox;
  FText := '';
end;

function TEWComboBoxItem.GetHtml: string;
var
  ASelected: string;
begin
  ASelected := '';
  if Index = FComboBox.ItemIndex then
    ASelected := C_SELECTED;
  Result := StringReplace(C_HTML_COMBOBOX_ITEM, '%selected%', ASelected, []);
  Result := StringReplace(Result, '%text%', FText, []);
end;

procedure TEWComboBoxItem.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

{ TEWComboBoxItemCollection }

function TEWComboBoxItemCollection.Add: TEWComboBoxItem;
begin
    Result := TEWComboBoxItem.Create(Self);
end;

constructor TEWComboBoxItemCollection.Create(AComboBox: TEWComboBox);
begin
  inherited Create(TEWComboBoxItem);
  FComboBox := AComboBox;
end;

function TEWComboBoxItemCollection.GetHtml: string;
var
  ICount: integer;
begin
  Result := '';
  for ICount := 0 to Count-1 do
    Result := Result + Items[ICount].GetHtml;
end;

function TEWComboBoxItemCollection.GetItem(Index: Integer): TEWComboBoxItem;
begin
  Result := inherited Items[index] as TEWComboBoxItem;
end;

function TEWComboBoxItemCollection.Insert(Index: Integer): TEWComboBoxItem;
begin
  Result := inherited insert( index ) as TEWComboBoxItem;
end;

procedure TEWComboBoxItemCollection.SetItem(Index: Integer;
  Value: TEWComboBoxItem);
begin
  inherited SetItem(index, Value);
end;

end.
