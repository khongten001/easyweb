unit EWCheckbox;

interface

uses Classes, EWIntf, EWBase, EWTypes;

type
  TEWCheckBox = class(TEWBaseObject, IEWCheckBox)
  private
    FText: string;
    FChecked: Boolean;
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
    function GetText: string;
    procedure SetText(const Value: string);
  protected
    function DesignTimeCaption: string; override;
    function GetHtml: string; override;
    procedure DoClick(AParams: TStrings); override;
  published
    property Checked: Boolean read GetChecked write SetChecked;
    property Text: string read GetText write SetText;
  end;

implementation


{ TEWCheckBox }

function TEWCheckBox.DesignTimeCaption: string;
begin
  Result := Text;
  if Result = '' then
    Result := Name;
end;

procedure TEWCheckBox.DoClick(AParams: TStrings);
begin
  FChecked := AParams.Values['value'] = 'true';
  inherited;
end;

function TEWCheckBox.GetChecked: Boolean;
begin
  Result := FChecked;
end;

function TEWCheckBox.GetHtml: string;
var
  AChecked: string;
begin
  AChecked := '';
  if FChecked then
    AChecked := 'checked';
  Result := '<input '+GetCss+' class="input-checkbox " name="'+Name+'" id="'+Name+'" style="height:20px;width:20px" type="checkbox">';
end;

function TEWCheckBox.GetText: string;
begin
  Result := FText;
end;

procedure TEWCheckBox.SetChecked(Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    Changed;
  end;
end;

procedure TEWCheckBox.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;


end.
