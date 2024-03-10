unit FDMigrator.Engine;

interface

uses
  System.SysUtils,
  System.Classes,
  FireDac.Comp.Client,
  FireDac.Comp.Script,
  FireDac.UI.Intf;

type
  TStepState = (Applied, Pending);
  TStepsCriteria = set of TStepState;

  TStep = record
    ID: string;
    Name: string;
    Script: TFileName;
    function StateFor(Status: string): TStepState;
  end;

  TDbMigratorEngine = class
  private
    FMigratorDir: string;
    FStepFilesDir: string;
    FSeedFilesDir: string;
    FConnectionDefsFile: string;
    FStatusFile: string;
    FDefs: TArray<string>;

    FStatus: string;
    FDryRun: Boolean;
    function GetStepFilesDir: string;
    function GetDefs: TArray<string>;
    function GetStatus: string;
    procedure SetStatus(const Value: string);
    procedure LoadStatus;
    procedure SaveStatus;
    procedure OnScriptError(ASender, AInitiator: TObject;
      var AException: Exception);
    procedure OnScriptProgress(Sender: TObject);
    procedure OnConsolePut(AEngine: TFDScript; const AMessage: String;
      AKind: TFDScriptOutputKind);
    function CreateScriptObj(Connection: TFDConnection): TFDScript;
    procedure ApplyStep(Connection: TFDConnection; Script: TFDScript;
      Mode: string; Step: TStep);
    function PreprocessScript(const Script, Mode: string): string;
    procedure SetMigratorDir(const Value: string);
    procedure Reset;
    function CreateFile(const Path, FileName: string; const Content: string = ''): Boolean;
  public
    constructor Create(const WorkDir: TFileName; const DryRun: Boolean);
    procedure DbUp(N: Integer);
    procedure DbDown(N: Integer);
    function GetSteps(Criteria: TStepsCriteria; Reverse: Boolean = False): TArray<TStep>;
    procedure Init;
    procedure NewStep(StepName: string; TemplateName: string='default');

    property MigratorDir: string read FMigratorDir write SetMigratorDir;
    property StepFilesDir: string read GetStepFilesDir;
    property ConnectionDefsFile: string read  FConnectionDefsFile;
    property StatusFile: string read FStatusFile;
    property SeedFilesDir: string read FSeedFilesDir;

    property Status: string read GetStatus write SetStatus;
    property Defs: TArray<string> read GetDefs;

  end;

implementation

uses
{$I .\ToolDBs.inc}
  FireDac.Stan.Intf,
  FireDac.Stan.Util,
  FireDac.Stan.Consts,
  FireDac.Comp.ScriptCommands,
  System.RegularExpressions,
  System.StrUtils,
  System.IOUtils,
  System.Generics.Collections,
  System.Generics.Defaults, FDMigrator.Engine.Templates;

{ TStep }

function TStep.StateFor(Status: string): TStepState;
begin
  if Status >= ID then
    Result := Applied
  else
    Result := Pending;
end;

{ TDbMigratorEngine }

constructor TDbMigratorEngine.Create(const WorkDir: TFileName;  const DryRun: Boolean);
begin
  inherited Create;;
  MigratorDir := FDExpandStr(WorkDir);
end;

function TDbMigratorEngine.GetStepFilesDir: string;
begin
  if not TDirectory.Exists(FStepFilesDir) then
    raise Exception.CreateFmt('No Steps direcotry found in %s', [MigratorDir]);
  Result := FStepFilesDir;
end;

procedure TDbMigratorEngine.OnConsolePut(AEngine: TFDScript;
  const AMessage: String; AKind: TFDScriptOutputKind);
begin
  WriteLn(AMessage);
end;

procedure TDbMigratorEngine.OnScriptError(ASender, AInitiator: TObject;
  var AException: Exception);
begin

end;

procedure TDbMigratorEngine.OnScriptProgress(Sender: TObject);
begin
  var
  Script := Sender as TFDScript;
  if Script.TotalJobSize = 0 then
    Exit;

  if Script.ScriptOptions.ConsoleOutput then
    Exit;

  Write(#8#8#8#8#8);
  Write(Format('%4.0f%%', [0.1 * Script.TotalPct10Done]));
end;

function TDbMigratorEngine.CreateScriptObj(Connection: TFDConnection)  : TFDScript;
begin
  Result := TFDScript.Create(Connection);
  Result.Connection := Connection;
  Result.Macros.Add.Name := 'DriverID';
  Result.OnConsolePut := OnConsolePut;
  Result.OnProgress := OnScriptProgress;
  Result.OnError := OnScriptError;
  Result.ScriptOptions.DefaultScriptPath := StepFilesDir;
  Result.ScriptOptions.EChoCommands := ecSQL;
  Result.ScriptOptions.EChoCommandTrim := 500;
  Result.ScriptOptions.BreakOnError := True;
  Result.ScriptOptions.RaisePLSQLErrors := True;
  Result.ScriptOptions.FeedbackScript := True;
  Result.ScriptOptions.MacroExpand := True;
  Result.ScriptOptions.ConsoleOutput := False;
end;

function TDbMigratorEngine.PreprocessScript(const Script, Mode: string): string;
const
  Modes: array[Boolean] of string = ('up', 'down');
begin
  var RemovePattern := '^\-\-\s!\s*IF\s+MIGRATE\s*=\s*' + Modes[SameText(Mode, 'up')] + '\s*(.*?)^\-\-\s!\s*END\s+MIGRATE';
  var Regex := TRegEx.Create(RemovePattern, [roIgnoreCase, roSingleLine, roMultiLine]);
  Result := Regex.Replace(Script, '');
end;

procedure TDbMigratorEngine.NewStep(StepName: string; TemplateName: string ='default');
const
  TemplateNames: array[0..1] of string = ('default', 'table');
  Templates: array[-1..1] of string = ('', DefaultTT, TableTT);

begin
  var Template := Templates[IndexText(TemplateName, TemplateNames)];
  if Template.IsEmpty then
    raise Exception.CreateFmt('Invalid template name "%s"', [TemplateName]);
  var StepID := FormatDateTime('yyyymmddhhnnss', Now);
  StepName := Format('%s-%s.sql' ,[StepId, StepName]);
  StepName := StepName.Replace(' ', '_', [rfReplaceAll]);
  CreateFile(StepFilesDir, StepName, Template);
  WriteLn( StepName + ' Created!');
end;

procedure TDbMigratorEngine.ApplyStep(Connection: TFDConnection;
  Script: TFDScript; Mode: string; Step: TStep);
begin
  WriteLn(Format('%s %s:     ', [Mode, Step.Script]));
  for var Def in Defs do
  begin
    Connection.ConnectionDefName := Def;
    if not Script.ScriptOptions.ConsoleOutput then
      Write(Format('    %s (%s):     ', [Def, Connection.DriverName]));
    Script.Macros.MacroByName('DriverID').AsRaw := Connection.DriverName;
    var
    Content := TFile.ReadAllText(TPath.Combine(StepFilesDir, Step.Script));
    Content := Self.PreprocessScript(Content, Mode);
    var
    SQLStep := Script.SQLScripts.Add;
    SQLStep.SQL.Text := Content;
    Script.ValidateAll;
    if not FDryRun then
    begin
      Script.ExecuteAll;
    end;
    Script.SQLScripts.Clear;
    WriteLn;
  end;
end;

procedure TDbMigratorEngine.DbUp(N: Integer);
begin
  var
  Connection := TFDConnection.Create(nil);
  var
  Script := CreateScriptObj(Connection);
  try
    for var Step in GetSteps([Pending]) do
    begin
      if N = 0 then
        Break;
      ApplyStep(Connection, Script, 'up', Step);
      if Script.TotalErrors > 0 then
        WriteLn(' ERR')
      else
      begin
        if not Script.ScriptOptions.ConsoleOutput then
          WriteLn(' ok');
        if not FDryRun then
          Status := Step.ID;
      end;
      Dec(N);
    end;
  finally
    Connection.Free;
  end;
end;

procedure TDbMigratorEngine.DbDown(N: Integer);
begin
  var
  Connection := TFDConnection.Create(nil);
  var
  Script := CreateScriptObj(Connection);
  var
  Steps := GetSteps([Applied], True);
  var
  StepsCount := Length(Steps);
  SetLength(Steps, StepsCount + 1);
  try
    for var idx := 0 to StepsCount - 1 do
    begin
      if N = 0 then
        Break;
      var
      Step := Steps[idx];
      ApplyStep(Connection, Script, 'down', Step);
      if Script.TotalErrors > 0 then
        WriteLn(' ERR')
      else
      begin
        if not Script.ScriptOptions.ConsoleOutput then
          WriteLn(' ok');
        if not FDryRun then
          Status := Steps[idx + 1].ID;
      end;
      Dec(N);
    end;
  finally
    Connection.Free;
  end;
end;

procedure TDbMigratorEngine.LoadStatus;
begin
  if not TDirectory.Exists(FMigratorDir) then
    raise Exception.CreateFmt('Work dir not found: %s', [FMigratorDir]);
  if not TFile.Exists(StatusFile) then
    FStatus := ' '
  else
    FStatus := TFile.ReadAllText(StatusFile);
end;

procedure TDbMigratorEngine.SaveStatus;
begin
  var
  StatusFile := TPath.Combine(FMigratorDir, '.status');
  TFile.WriteAllText(StatusFile, FStatus);
end;

function TDbMigratorEngine.GetSteps(Criteria: TStepsCriteria;
  Reverse: Boolean = False): TArray<TStep>;
begin
  var
    Temp: TArray<TStep> := [];
  var
  StepFileRegEx := TRegEx.Create('(\d+)-(\w+).sql', [roIgnoreCase]);
  var
    Step: TStep;
  var
    Filter: TDirectory.TFilterPredicate;
  Filter := function(const Path: string; const SearchRec: TSearchRec): Boolean
    begin
      var
      Match := StepFileRegEx.Match(SearchRec.Name);
      Result := Match.Success;
      if not Result then
        Exit;
      Step.ID := Match.Groups[1].Value;
      Step.Name := Match.Groups[2].Value;
      Step.Script := Match.Value;
      if (Criteria = []) or (Step.StateFor(Status) in Criteria) then
        Temp := Temp + [Step];
    end;
  TDirectory.GetFiles(StepFilesDir, '*.sql',
    TSearchOption.soTopDirectoryOnly, Filter);
  var
    Comparer: IComparer<TStep>;
  if Reverse then
    Comparer := TComparer<TStep>.Construct(
      function(const Left, Right: TStep): Integer
      begin
        Result := -CompareStr(Left.ID, Right.ID);
      end)
  else
    Comparer := TComparer<TStep>.Construct(
      function(const Left, Right: TStep): Integer
      begin
        Result := CompareStr(Left.ID, Right.ID);
      end);

  TArray.Sort<TStep>(Temp, Comparer);
  Result := Temp;
end;

function TDbMigratorEngine.CreateFile(const Path: string; const FileName: string; const Content: string = ''): Boolean;
begin
  TDirectory.CreateDirectory(Path);
  var FullFileName := TPath.Combine(Path, FileName);
  Result := not TFile.Exists(FullFileName);
  if Result then
    TFile.WriteAllText(FullFileName, Content);
end;

procedure TDbMigratorEngine.Reset;
begin
  FStepFilesDir := TPath.Combine(FMigratorDir, 'Steps');
  FSeedFilesDir := TPath.Combine(FMigratorDir, 'Seeds');
  FConnectionDefsFile := TPath.Combine(MigratorDir, 'FDConnectionDefs.ini');
  FStatusFile := TPath.Combine(FMigratorDir, '.status');
  FStatus := '';
end;

procedure TDbMigratorEngine.Init;
begin
  CreateFile(MigratorDir,  'FConnectionDefsFile.ini', FDConnectionDefsTT);
  CreateFile(StepFilesDir,  '__prelude__', PreludeTT);
  CreateFile(StepFilesDir,  '__MySQL.prelude', MySQLPreludeTT);
  CreateFile(StepFilesDir,  '__MSSQL.prelude', MSSQLPreludeTT);
  SaveStatus;
end;

procedure TDbMigratorEngine.SetMigratorDir(const Value: string);
begin
  FMigratorDir := Value;
  Reset;
end;

procedure TDbMigratorEngine.SetStatus(const Value: string);
begin
  FStatus := Value;
  SaveStatus;
  Write('status updated')
end;

function TDbMigratorEngine.GetDefs: TArray<string>;
begin
  if FDefs = nil then
  begin
    if not TFile.Exists(FConnectionDefsFile) then
      raise Exception.CreateFmt('No ConnectionDef fileName found in %s', [MigratorDir]);
    FDManager.ConnectionDefFileName := FConnectionDefsFile;
    var
    List := TStringList.Create;
    try
      FDManager.GetConnectionNames(List);
      for var Def in List do
      begin
        if Def.StartsWith('!') then
          Continue;
        FDefs := FDefs + [Def];
      end;
    finally
      List.Free;
    end;
  end;
  Result := FDefs;
end;

function TDbMigratorEngine.GetStatus: string;
begin
  if FStatus.IsEmpty then
    LoadStatus;
  Result := FStatus;
end;

end.
