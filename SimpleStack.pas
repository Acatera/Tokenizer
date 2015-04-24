unit SimpleStack;

interface

type
  TStack = class
  strict private
    fActualCount: Integer;
    fCount: integer;
    fArray: array of string;
    procedure IncrementArray;
  public
    function Peek: string;
    function Pop: string;
    procedure Push(Value: string); overload;
    procedure Push(Value: double); overload;
    function HasNext: Boolean;
    function Count: integer;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

function TStack.Count: integer;
begin
  Result := fCount;
end;

constructor TStack.Create;
begin
  fActualCount := 10;
  SetLength(fArray, fActualCount);
end;

destructor TStack.Destroy;
begin
  SetLength(fArray, 0);
  inherited;
end;

function TStack.HasNext: Boolean;
begin
  Result := Count > 0;
end;

procedure TStack.IncrementArray;
begin
  if (fActualCount = fCount) then begin
    Inc(fActualCount, 10);
    SetLength(fArray, fActualCount);
  end;
end;

function TStack.Peek: string;
begin
  if (Count > 0) then 
    Result := fArray[Count - 1]
  else 
    raise Exception.Create('Peek error: empty stack');
end;

function TStack.Pop: string;
begin
  if (Count > 0) then begin
    Result := fArray[Count - 1];
    Dec(fCount); //No need to modify the array;
  end else
    raise Exception.Create('Pop error: empty stack');
end;

procedure TStack.Push(Value: string);
begin
  IncrementArray;
  Inc(fCount);
  fArray[Count - 1] := Value;
end;

procedure TStack.Push(Value: double);
begin
  Push(FloatToStr(Value));
end;

end.
