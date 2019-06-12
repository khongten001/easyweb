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
    procedure AddClickEvent(AEvents: TStrings); override;
  published
    property Align;
    property Checked: Boolean read GetChecked write SetChecked;
    property Text: string read GetText write SetText;
  end;

implementation


{ TEWCheckBox }

procedure TEWCheckBox.AddClickEvent(AEvents: TStrings);
begin
  AddObjectEvent(Name, 'click', [], AEvents, 'document.getElementById("'+Name+'Cbx").checked');
end;

function TEWCheckBox.DesignTimeCaption: string;
begin
  Result := Text;
  if Result = '' then
    Result := Name;
end;

procedure TEWCheckBox.DoClick(AParams: TStrings);
begin
  Checked := AParams.Values['value'] = 'true';
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
  Result := '<div id="'+Name+'" '+GetCss+'>'+
            '<input id="'+Name+'Cbx" class="form-check-input" type="checkbox" name="'+Name+'" '+AChecked+'>'+
            '<label id="'+Name+'Lbl" class="form-check-label" for="'+Name+'">'+FText+'</label>'+
            '</div>';
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
