unit Evaluator;

interface

uses
  Classes;

type
  TStack = class(TStringList)
  public
    Reversed: Boolean;
    function Peek: string;
    function PopS: string;
    function PopN: Double;
    procedure Push(Value: string); overload;
    procedure Push(Value: double); overload;
    function HasNext: Boolean;
  end;
  TToken = record
    Symbol: string;
    IsOperator: boolean
  end;
  TTokens = array of TToken;
  
procedure TestEvaluator;  

implementation

uses
  SysUtils, Math;

var
  Str: string;
  iLen: Cardinal;
  expr: string;

function OpPrec(op: string): Byte;
begin
  if ((op = '+') or (op = '-')) then
  
    Result := 1
  else if ((op = '*') or (op = '/')) then
    Result := 2
  else if (op = '^') then
    Result := 3
  else if ((op = '(') or (op = ')')) then
    Result := 0;
end;

function GetNextToken(var Str: string): TToken;
var
  i: Integer;
begin
  if ((Str <> '') and (Str[1] in ['+', '-', '*', '/', '^', '%', '(', ')'])) then begin
    Result.Symbol := Str[1];
    Result.IsOperator := True;
    Delete(Str, 1, 1);
    Exit;
  end;
  
  for i := 0 to Length(Str) do begin
    if Str[i] in ['+', '-', '*', '/', '^', '%', '(', ')'] then begin
      Result.Symbol := Copy(Str, 1, i - 1);
      Result.IsOperator := False;
      Delete(Str, 1, i - 1);
      Exit;
    end;
  end;
  Result.Symbol := Str;
  Result.IsOperator := False;
  Str := '';
end;
  
function TokenizeThis(Str: string): TTokens;
var
  cToken: TToken;
  ResLength: integer;
  i: integer;
begin
  Str := StringReplace(Str, ' ', '', [rfReplaceAll]);
  ResLength := 10;
  SetLength(Result, 10);
  i := 0;
  while (Str <> '') do begin
    cToken := GetNextToken(Str);
    if i = ResLength then begin
      Inc(ResLength, 10);
      SetLength(Result, ResLength);
    end;
    Result[i] := cToken;
    Inc(i);
  end;
  SetLength(Result, i);
end;

function EvalExpr(Expression: string): string;
var
  ops: TStack;
  output: TStack;
  i: integer;
  T: TToken;
  stack: TStack;
  v: double;
  t1, t2: double;
  Tokens: TTokens;
begin
//  Expression := StringReplace(Expression, ' ', '', [rfReplaceAll]);

  ops := TStack.Create;
  output := TStack.Create;
  stack := TStack.Create;
  {
While there are tokens to be read:
  Read a token.
  If the token is a number, then add it to the output queue.
  //If the token is a function token, then push it onto the stack.
  //If the token is a function argument separator (e.g., a comma):
  //  Until the token at the top of the stack is a left parenthesis, pop operators off the stack onto the output queue. If no left parentheses are encountered, either the separator was misplaced or parentheses were mismatched.
  If the token is an operator, o1, then:
    while there is an operator token, o2, at the top of the operator stack, and either
***   o1 is left-associative and its precedence is less than or equal to that of o2, or
      o1 is right associative, and has precedence less than that of o2,
    then pop o2 off the operator stack, onto the output queue;
    push o1 onto the operator stack.
  If the token is a left parenthesis, then push it onto the stack.
  If the token is a right parenthesis:
    Until the token at the top of the stack is a left parenthesis, pop operators off the stack onto the output queue.
    Pop the left parenthesis from the stack, but not onto the output queue.
    ////If the token at the top of the stack is a function token, pop it onto the output queue.
    /////If the stack runs out without finding a left parenthesis, then there are mismatched parentheses.
When there are no more tokens to read:
  While there are still operator tokens in the stack:
    If the operator token on the top of the stack is a parenthesis, then there are mismatched parentheses.
    Pop the operator onto the output queue.
Exit.

}
  Writeln('Infix: ' + Expression);
  Tokens := TokenizeThis(Expression);
  for i := 0 to Length(Tokens) - 1 do begin
    T := Tokens[i];
    if (not T.IsOperator) then begin
      output.Push(T.Symbol)
    end else if (T.Symbol[1] in ['+', '-', '*', '/', '^']) then begin
      while (ops.Count > 0) and (OpPrec(ops[ops.Count - 1][1]) >= OpPrec(T.Symbol)) do begin
        output.Push(ops.PopS);
      end;
      ops.Push(T.Symbol);
    end else if (T.Symbol = '(') then begin
      ops.Push(T.Symbol);
    end else if (T.Symbol = ')') then begin
      while (ops.HasNext and (ops.Peek <> '(')) do
        output.Push(ops.PopS);
      if (ops.Peek = '(') then
        ops.PopS;
    end;
  end;

  while (ops.HasNext) do 
    if ((ops.Peek = '(') or (ops.Peek = ')')) then begin
      Result := 'Mismatched parentheses';
      Exit;
    end else
    output.Push(ops.PopS);
  
  ops.Clear;

  Writeln('Postfix: ' + output.CommaText);
  {
While there are input tokens left
  Read the next token from input.
  If the token is a value
    Push it onto the stack.
  Otherwise, the token is an operator (operator here includes both operators and functions).
    It is known a priori that the operator takes n arguments.
    If there are fewer than n values on the stack
      (Error) The user has not input sufficient values in the expression.
    Else, Pop the top n values from the stack.
    Evaluate the operator, with the values as arguments.
    Push the returned results, if any, back onto the stack.
If there is only one value in the stack
  That value is the result of the calculation.
Otherwise, there are more values in the stack
  (Error) The user input has too many values.
}

  
  output.Reversed := True;
  while (output.HasNext) do begin
    T.Symbol := output.PopS;
    if (not (T.Symbol[1] in ['+', '-', '*', '/', '^', '%'])) then
//    if (T in ['0'..'9']) then
      stack.Push(T.Symbol)
    else begin
      t1 := stack.PopN;
      t2 := stack.PopN;
      
      if (T.Symbol = '+') then
        v := t2 + t1
      else if (T.Symbol = '-') then
        v := t2 - t1
      else if (T.Symbol = '*') then
        v := t2 * t1
      else if (T.Symbol = '/') then
        v := t2 / t1
      else if (T.Symbol = '^') then
        v := Math.Power(t2, t1);    
      stack.Push(v);
    end;  
  end;
  
  if (stack.count = 1) then
    result := stack.PopS
  else  
    result := 'error';
  
  stack.Free;
  output.Free;
  ops.Free;
  { Result := exp; }
end;
  
{ TStack }

function TStack.HasNext: Boolean;
begin
  Result := Count > 0;
end;

function TStack.Peek: string;
begin
  if (Count > 0) then begin
    if Reversed then begin
      Result := Get(0);
    end else begin
      Result := Get(Count - 1);
    end;
  end else
    Writeln('Peek error: index OOB');
end;

function TStack.PopN: Double;
begin
  if (Count > 0) then begin
    if Reversed then begin
      Result := StrToFloatDef(Get(0), 0);
      Delete(0);
    end else begin
      Result := StrToFloatDef(Get(Count - 1), 0);
      Delete(Count - 1);
    end;
  end else
    Writeln('PopN error: index OOB');
end;

function TStack.PopS: string;
begin
  if (Count > 0) then begin
    if Reversed then begin
      Result := Get(0);
      Delete(0);
    end else begin
      Result := Get(Count - 1);
      Delete(Count - 1);
    end;
  end else
    Writeln('PopN error: index OOB');
end;

procedure TStack.Push(Value: string);
begin
  Add(Value);
end;

procedure TStack.Push(Value: double);
begin
  Add(FloatToStr(Value));
end;

procedure TestEvaluator;
var
  expr: string;
  i: integer;
  Tokens: TTokens;
  
begin
  expr := '3 + 4 * 2 / ( 1 - 5 ) ^ (2*8) ^ (3 + 3)';
  expr := '1.23 - 4654 + (123 * (0-12.39))';
//  expr := '2 ^ 3 * 4';
//  expr := '3 + 4 * 2 / 1 - 5 ^ 2 ^ 3';
//  expr := '(1 + 2) * (2+4)';
  
//  Writeln('3 + 4 = ' + EvalExpr('3 + 4'));
//  Writeln('3 * 4 = ' + EvalExpr('3 * 4'));
//  Writeln('3 - 4 = ' + EvalExpr('3 - 4'));
//  Writeln('3 / 4 = ' + EvalExpr('3 / 4'));
  Writeln(Expr + '=' + EvalExpr(expr));

  Tokens := TokenizeThis(expr);
  for i := 0 to Length(Tokens) - 1 do
    WriteLn(Tokens[i].Symbol);

  Readln(expr);
end;

end.
