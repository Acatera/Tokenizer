unit Evaluator;

interface

uses
  Classes, SimpleStack;

type
  TAssociativity = (asNone, asLeft, asRight);
  TParity = (paUnary, paBinary);
  
  TOperator = record
    Simbol: string;
    Precedence: byte;
    Associativity: TAssociativity;
    Parity: TParity;
    procedure Create(Simbol: string; Precedence: Byte; Associativity: TAssociativity; Parity: TParity);
  end;

procedure TestEvaluator;  

implementation

uses
  SysUtils, Math, Tokenizer;

var
  Str: string;
  iLen: Cardinal;
  expr: string;
  OperatorMap: array[0..8] of TOperator;
  FunctionsList: TDynStrArray;
  ConstantsList: TDynStrArray;

procedure TOperator.Create(Simbol: string; Precedence: Byte; Associativity: TAssociativity; Parity: TParity);
begin
  Self.Simbol := Simbol;
  Self.Precedence := Precedence;
  Self.Associativity := Associativity;
  Self.Parity := Parity;
end;

procedure GenerateConstantsList;
begin
  SetLength(ConstantsList, 10);
  ConstantsList[0] := 'pi';
end;

procedure GenerateFunctionsList;
begin
  SetLength(FunctionsList, 100);
  FunctionsList[0] := 'sin';
  FunctionsList[1] := 'cos';             
  FunctionsList[2] := 'sqr';
  FunctionsList[3] := 'sqrt';
  FunctionsList[4] := 'pow';
//  FunctionsList[5] := 'sum';
end;
  
procedure GenerateOperatorMap;
begin
  OperatorMap[0].Create('(', 0, asLeft, paBinary);
  OperatorMap[1].Create(')', 0, asLeft, paBinary);
  OperatorMap[2].Create('+', 1, asLeft, paBinary);
  OperatorMap[3].Create('-', 1, asLeft, paBinary);
  OperatorMap[4].Create('*', 2, asLeft, paBinary);
  OperatorMap[5].Create('/', 2, asLeft, paBinary);
  OperatorMap[6].Create('^', 3, asLeft, paBinary);
  OperatorMap[7].Create('_', 1, asRight, paUnary); //subst char for unary -
  OperatorMap[8].Create('.', 6, asLeft, paBinary); //for float values
end;

function IsConstant(Str: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  Str := LowerCase(Str);
  for i := 0 to Length(ConstantsList) - 1 do
    if (Str = LowerCase(ConstantsList[i])) then begin
      Result := True;
      Break;
    end;
end;

function IsFunction(Str: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  Str := LowerCase(Str);
  for i := 0 to Length(FunctionsList) - 1 do
    if (Str = LowerCase(FunctionsList[i])) then begin
      Result := True;
      Break;
    end;
end;
  
function IsOperator(Str: string): Boolean;
var
  i: integer;
begin
  Result := False;
  Str := LowerCase(Str);
  for i := 0 to Length(OperatorMap) - 1 do
    if (OperatorMap[i].Simbol = Str) then begin
       Result := True;
       Break;
    end;
end;

function IsNumber(Str: string): Boolean;
begin
  Result := (Str = FloatToStr(StrToFloatDef(Str, 0)));
end;

function GetOperatorStruct(Str: string): TOperator;
var
  i: Integer;
begin
  for i := 0 to Length(OperatorMap) - 1 do
    if (OperatorMap[i].Simbol = Str) then begin
       Result := OperatorMap[i];
       Break;
    end;
end;

function IsParenthesis(Str: string): boolean;
begin
  Result := (Str = '(') or (Str = ')');
end;

{
  While there are tokens to be read:
    Read a token.
    If the token is a number, then add it to the output queue.
    If the token is a function token, then push it onto the stack.
    If the token is a function argument separator (e.g., a comma):
      Until the token at the top of the stack is a left parenthesis, pop operators off the stack onto the output queue. If no left parentheses are encountered, either the separator was misplaced or parentheses were mismatched.
    If the token is an operator, o1, then:
      while there is an operator token, o2, at the top of the operator stack, and either
        o1 is left-associative and its precedence is less than or equal to that of o2, or
        o1 is right associative, and has precedence less than that of o2,
      then pop o2 off the operator stack, onto the output queue;
      push o1 onto the operator stack.
    If the token is a left parenthesis, then push it onto the stack.
    If the token is a right parenthesis:
      Until the token at the top of the stack is a left parenthesis, pop operators off the stack onto the output queue.
      Pop the left parenthesis from the stack, but not onto the output queue.
      ////If the token at the top of the stack is a function token, pop it onto the output queue.
      ////If the stack runs out without finding a left parenthesis, then there are mismatched parentheses.
  When there are no more tokens to read:
    While there are still operator tokens in the stack:
      If the operator token on the top of the stack is a parenthesis, then there are mismatched parentheses.
      Pop the operator onto the output queue.
  Exit.
}

function ToInfix(Expression: string): TDynStrArray;
var
  Op: TOperator;
  Tokens: TDynStrArray;
  Tokenizer: TTokenizer;
  i: Integer;
  Stack: TStack;
  Output: TStack;
  LastToken: string;
  SawParenthesis: Boolean;
begin
  Tokenizer := TTokenizer.Create;
  try
    Tokens := Tokenizer.Tokenize(Expression);
  finally
    Tokenizer.Free;
  end;
  
  if (Length(Tokens) = 0) then Exit; //guard
                        
  GenerateOperatorMap;
  GenerateFunctionsList;
  GenerateConstantsList;

  if (Tokens[0] = '-') then 
    Tokens[0] := '_';
    
  for i := 0 to Length(Tokens) - 2 do begin //We'll check the current and next, needs to be -2;
    if ((Tokens[i + 1] = '-') and (IsOperator(Tokens[i]))) then
      Tokens[i + 1] := '_';
  end;
  
  Output := TStack.Create;
  Stack := TStack.Create;
  try
    for i := 0 to Length(Tokens) - 1 do begin
      if (IsNumber(Tokens[i]) or IsConstant(Tokens[i])) then begin 
        Output.Push(Tokens[i]);        
        Continue;
      end; 

      if (IsFunction(Tokens[i])) then begin
        Stack.Push(Tokens[i]);
        Continue;
      end; 

      if (Tokens[i] = ',') then begin
        SawParenthesis := False;
        while ((Stack.HasNext) and (Stack.Peek <> '(')) do begin
          Output.Push(Stack.Pop);
        end;
        if (Stack.Peek = '(') then
          SawParenthesis := True;
        if (not SawParenthesis) then
          raise Exception.Create('Mismatched parentheses or comma misplaced');
        Continue;  
      end;
      
      if (IsOperator(Tokens[i])) then begin
        Op := GetOperatorStruct(Tokens[i]);
        if (not IsParenthesis(Tokens[i])) then begin
          while (Stack.HasNext) and (
                ((Op.Precedence <= GetOperatorStruct(Stack.Peek).Precedence) and (Op.Associativity = asLeft)) or
                ((Op.Precedence < GetOperatorStruct(Stack.Peek).Precedence) and (Op.Associativity = asRight))) do
            Output.Push(Stack.Pop);
          Stack.Push(Op.Simbol);
        end else if (Op.Simbol = '(') then begin
          Stack.Push(Op.Simbol);
        end else if (Op.Simbol = ')') then begin
          SawParenthesis := False;
          while (Stack.HasNext and (Stack.Peek <> '(')) do begin
            Output.Push(Stack.Pop);
          end;
          if (Stack.Peek = '(') then begin
            SawParenthesis := True;
            Stack.Pop;
          end;
          
          if (IsFunction(Stack.Peek)) then
            Output.Push(Stack.Pop);
          if (not SawParenthesis) then
            raise Exception.Create('Mismatched parentheses');  
        end;
        Continue;
      end;
      raise Exception.Create('Unknown identifier: ' + Tokens[i]);
    end;

    while (Stack.HasNext) do begin
      if ((Stack.Peek = '(') or (Stack.Peek = ')')) then begin
        raise Exception.Create('Mismatched parentheses');
      end else
        Output.Push(Stack.Pop);
    end;

    SetLength(Result, Output.Count);
  
    i := 0;
    while (Output.HasNext) do begin
      Result[i] := Output.Pop;
      Inc(i);
    end;
  finally
    Output.Free;
    Stack.Free;
  end;
end;

function RPNToVal(Expression: TDynStrArray): Double;
var
  Stack: TStack;
  i: Integer;
  Symbol: string;
  S: string;
  Term1: Extended;
  Term2: Extended;
  Value: Extended;
begin
  Stack := TStack.Create;
  try
    for i := Length(Expression) - 1 downto 0 do begin
      Symbol := Expression[i];
      if (IsNumber(Symbol)) then begin
        Stack.Push(Symbol);
        Continue;
      end;

      if (IsConstant(Symbol)) then begin
        if (LowerCase(Symbol) = 'pi') then
          Stack.Push(FloatToStr(PI));
        Continue;  
      end;
      
      if (IsFunction(Symbol)) then begin
        if (LowerCase(Symbol) = 'sin') then begin
          Value := StrToFloat(Stack.Pop);
          Value := Sin(Value);
          Stack.Push(Value);
        end else if (LowerCase(Symbol) = 'cos') then begin
          Value := StrToFloat(Stack.Pop);
          Value := Cos(Value);
          Stack.Push(Value);
        end else if (LowerCase(Symbol) = 'sqr') then begin
          Value := StrToFloat(Stack.Pop);
          Value := Sqr(Value);
          Stack.Push(Value);
        end else if (LowerCase(Symbol) = 'sqrt') then begin
          Value := StrToFloat(Stack.Pop);
          Value := Sqrt(Value);
          Stack.Push(Value);
        end else if (LowerCase(Symbol) = 'pow') then begin
          Term1 := StrToFloat(Stack.Pop);
          Term2 := StrToFloat(Stack.Pop);
          Value := Power(Term2, Term1);
          Stack.Push(Value);
        end else if (LowerCase(Symbol) = 'sum') then begin
          Value := StrToFloat(Stack.Pop) + StrToFloat(Stack.Pop) + StrToFloat(Stack.Pop);
          Stack.Push(Value);
        end
          else begin
            raise Exception.Create('Invalid function: ' + Symbol);
          end;
        Continue;
      end;
      
      if (IsOperator(Symbol)) then begin
        if (Symbol = '.') then begin
          S := Stack.Pop;
          S := Stack.Pop + '.' + S;
          Value := StrToFloatDef(S, 0);
        end else if (Symbol = '_') then begin
          Value := -StrToFloat(Stack.Pop);
        end else begin
          Term1 := StrToFloat(Stack.Pop);
          Term2 := StrToFloat(Stack.Pop);
      
          if (Symbol = '+') then
            Value := Term2 + Term1
          else if (Symbol = '-') then
            Value := Term2 - Term1
          else if (Symbol = '*') then
            Value := Term2 * Term1
          else if (Symbol = '/') then
            Value := Term2 / Term1
          else if (Symbol = '^') then
            Value := Math.Power(Term2, Term1)
          else 
            raise Exception.Create('Invalid operator: ' + Symbol);
        end;
        Stack.Push(Value);
      end;  
    end;

    if (Stack.count = 1) then
      Result := StrToFloat(stack.Pop)
    else  
      raise Exception.Create('Error');
  finally
    Stack.Free;
  end;
end;

function EvalExpr(Expression: string): string;
begin
  Result := FloatToStr(RPNToVal(ToInfix(Expression)));
end;

procedure TestEvaluator;
var
  expr: string;
  i: integer;
begin
  expr := '3 + 4 * 2 / ( 1 - 5 ) ^ (2*8) ^ (3 + 3)';
  expr := '1.23 - 4654 + (123 * (0-12.39))';
//  expr := '2 ^ 3 * 4';
//  expr := '3 + 4 * 2 / 1 - 5 ^ 2 ^ 3';
//  expr := '(1 + 2) * (2+4)';
//  expr := '(1.24 + 1)';
//  expr := '2 + -(2 * 4)';
  expr := '-1 + 2';
//  expr := '2 + -1';
  expr := '-(2 * 4) + 2';
  expr := '10 ^ 2 * pi';
  expr := 'sin(1)';
  expr := 'sin(pi / 2)';
  expr := 'sin(-pi / 2)';
  expr := 'srt(4)';
  expr := 'pow(2, 2)';
  expr := 'sum(1, 2, 3)';
            
  Writeln(Expr + '=' + EvalExpr(expr));
  Readln(expr);
end;

{ TOperator }

end.
