unit FDMigrator.Engine;

interface

uses
  System.SysUtils,
  REST.Json,

  System.Classes,
  FireDac.Comp.Client,
  FireDac.Comp.Script,
  FireDac.UI.Intf;

type
  TStepState = (Applied, Pending);
  TStepsCriteria = set of TStepState;
  {$SCOPEDENUMS ON}
  TStepIDKind = (TimeStamp, Sequence);
  {$SCOPEDENUMS OFF}

  TStep = record
    ID: string;
    Name: string;
    Script: TFileName;
    function StateFor(Status: string): TStepState;
  end;

  TFDMigratorCfg = class(Tpersistent)
  private
    FStatus: string;
    FStepKind: TStepIDKind;
    FStepFormat: string;
  public
    constructor Create;
    property Status: string read FStatus write FStatus;
    property StepKind: TStepIDKind read FStepKind write FStepKind;
    property StepFormat:  string read FStepFormat write FStepFormat;
  end;

  TDbMigratorEngine = class
  private
    FMigratorDir: string;
    FStepFilesDir: string;
    FSeedFilesDir: string;
    FConnectionDefsFile: string;
    FStatusFile: string;
    FConfigFile: string;
    FDefs: TArray<string>;

    FConfig: TFDMigratorCfg;
    FDryRun: Boolean;
    function GetDefs: TArray<string>;
    procedure LoadConfig;
    procedure OnScriptError(ASender, AInitiator: TObject;
      var AException: Exception);
    procedure OnScriptProgress(Sender: TObject);
    procedure OnConsolePut(AEngine: TFDScript; const AMessage: String;
      AKind: TFDScriptOutputKind);
    function CreateScriptObj(Connection: TFDConnection): TFDScript;
    function PreprocessScript(const Script, Mode: string): string;
    procedure SetMigratorDir(const Value: string);
    procedure Reset;
    function CreateFile(const Path, FileName: string;
      const Content: string = ''): Boolean;
    procedure CheckInitDone;
    procedure UpdateStatus(StepID: string);
    function ApplyStep(Connection: TFDConnection; Script: TFDScript;
      Mode: string; Step: TStep): Boolean;
    procedure ReportAppliedStep(Script: TFDScript);
    function GetNextStepID: string;
    procedure SaveConfig;
    function GetConfig: TFDMigratorCfg;
  public
    constructor Create(const WorkDir: TFileName; const DryRun: Boolean);
    destructor Destroy;override;
    {$REGION 'Commands'}
    procedure Init(Kind: TStepIdKind; Fmt: string);
    procedure DbUp(N: Integer);
    procedure DbDown(N: Integer);
    function GetSteps(Criteria: TStepsCriteria; Reverse: Boolean = False)
      : TArray<TStep>;
    procedure NewStep(StepName: string; TemplateName: string = 'default');
    {$ENDREGION}

    property MigratorDir: string read FMigratorDir write SetMigratorDir;
    property StepFilesDir: string read FStepFilesDir;
    property ConnectionDefsFile: string read FConnectionDefsFile;
    property StatusFile: string read FStatusFile;
    property ConfigFile: string read FConfigFile;
    property SeedFilesDir: string read FSeedFilesDir;
    property Config: TFDMigratorCfg read GetConfig;
//    property Status: string read GetStatus;
    property Defs: TArray<string> read GetDefs;
  end;

implementation

uses
  System.RegularExpressions,
  System.StrUtils,
  System.IOUtils,
{$I .\ToolDBs.inc}
  FireDac.Stan.Intf,
  FireDac.Stan.Util,
  FireDac.Stan.Consts,
  FireDac.Comp.ScriptCommands,
  System.Generics.Collections,
  System.Generics.Defaults,
  FDMigrator.Engine.Templates;

{ TStep }

function TStep.StateFor(Status: string): TStepState;
begin
  if Status >= ID then
    Result := Applied
  else
    Result := Pending;
end;

{ TFDMigratorCfg }

constructor TFDMigratorCfg.Create;
begin
  inherited;
  FStepKind := TStepIDKind.TimeStamp;
  FStepFormat := 'yyyymmddhhnnsszzz';
end;

{ TDbMigratorEngine }

constructor TDbMigratorEngine.Create(const WorkDir: TFileName;
  const DryRun: Boolean);
begin
  inherited Create;;
  MigratorDir := FDExpandStr(WorkDir);
end;

destructor TDbMigratorEngine.Destroy;
begin
  FConfig.Free;
  inherited;
end;

procedure TDbMigratorEngine.CheckInitDone;
begin
  if not TDirectory.Exists(FMigratorDir) then
    raise Exception.CreateFmt('Work dir "%s" not found. Use init command',
      [FMigratorDir]);

  if not TFile.Exists(FConnectionDefsFile) then
    raise Exception.CreateFmt
      ('ConnectionDef file "%s" not found. Use init command',
      [FConnectionDefsFile]);

  if not TDirectory.Exists(FStepFilesDir) then
    raise Exception.CreateFmt
      ('Steps direcotry "%s" not found. Use init command', [FStepFilesDir]);
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

function TDbMigratorEngine.CreateScriptObj(Connection: TFDConnection)
  : TFDScript;
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
  Modes: array [Boolean] of string = ('up', 'down');
begin
  var
  RemovePattern := '^\-\-\s!\s*IF\s+MIGRATE\s*=\s*' + Modes[SameText(Mode, 'up')
    ] + '\s*(.*?)^\-\-\s!\s*END\s+MIGRATE';
  var
  Regex := TRegEx.Create(RemovePattern, [roIgnoreCase, roSingleLine,
    roMultiLine]);
  Result := Regex.Replace(Script, '');
end;

function TDbMigratorEngine.GetNextStepID: string;
begin

  case Config.StepKind of
    TStepIDKind.TimeStamp:
      Result := FormatDateTime(Config.StepFormat, Now);
    TStepIDKind.Sequence:
      Result := FormatFloat(Config.StepFormat, 1 + Length(GetSteps([])));
  end;
end;

procedure TDbMigratorEngine.NewStep(StepName: string;
  TemplateName: string = 'default');
const
  TemplateNames: array [0 .. 1] of string = ('default', 'table');
  Templates: array [-1 .. 1] of string = ('', DefaultTT, TableTT);

begin
  CheckInitDone;
  var
  Template := Templates[IndexText(TemplateName, TemplateNames)];
  if Template.IsEmpty then
    raise Exception.CreateFmt('Invalid template name "%s"', [TemplateName]);
  var
  StepID := GetNextStepID;
  StepName := Format('%s-%s.sql', [StepID, StepName]);
  StepName := StepName.Replace(' ', '_', [rfReplaceAll]);
  CreateFile(StepFilesDir, StepName, Template);
  WriteLn(StepName + ' Created!');
end;

function TDbMigratorEngine.ApplyStep(Connection: TFDConnection;
  Script: TFDScript; Mode: string; Step: TStep): Boolean;
begin
  Result := True;
  WriteLn(Format('%s %s:     ', [Mode, Step.Script]));
  for var Def in Defs do
  begin
    Connection.ConnectionDefName := Def;
    if not Script.ScriptOptions.ConsoleOutput then
      Write(Format('    %s (%s):     ', [Def, Connection.DriverName]));
    Script.Macros.MacroByName('DriverID').AsRaw := Connection.DriverName;
    var
    Content := TFile.ReadAllText(TPath.Combine(StepFilesDir, Step.Script));
    Content := PreprocessScript(Content, Mode);
    var
    SQLStep := Script.SQLScripts.Add;
    SQLStep.SQL.Text := Content;
    Script.ValidateAll;
    if not FDryRun then
    begin
      Script.ExecuteAll;
    end;
    Script.SQLScripts.Clear;
    Result := Script.TotalErrors = 0;
    WriteLn;
  end;
end;

procedure TDbMigratorEngine.ReportAppliedStep(Script: TFDScript);
begin
  if Script.TotalErrors = 0 then
  begin
    if not Script.ScriptOptions.ConsoleOutput then
      WriteLn(' ok');
  end
  else
    WriteLn(' ERR');
  WriteLn('Updated status ' + Config.Status)
end;

procedure TDbMigratorEngine.UpdateStatus(StepID: string);
begin
  if not FDryRun then
    FConfig.Status := StepID;
  SaveConfig;
end;

procedure TDbMigratorEngine.DbUp(N: Integer);
begin
  if N = 0 then
    Exit;
  CheckInitDone;
  var
  Connection := TFDConnection.Create(nil);
  var
  NoSteps := True; // assume no steps to apply
  try
    var
    Script := CreateScriptObj(Connection);
    for var Step in GetSteps([Pending]) do
    begin
      NoSteps := False;
      if N = 0 then
        Break;
      if ApplyStep(Connection, Script, 'up', Step) then
        UpdateStatus(Step.ID);
      ReportAppliedStep(Script);
      Dec(N);
    end;
    if NoSteps then
      WriteLn('No pending steps found');
  finally
    Connection.Free;
  end;
end;

procedure TDbMigratorEngine.DbDown(N: Integer);
begin
  CheckInitDone;
  var
  Steps := GetSteps([Applied], True);
  var
  StepsCount := Length(Steps);
  // centinel to allow access 1 extra postion meaning "no step"
  SetLength(Steps, StepsCount + 1);

  var
  Connection := TFDConnection.Create(nil);
  try
    var
    Script := CreateScriptObj(Connection);
    var
    NoSteps := True; // assume no steps applied
    for var idx := 0 to StepsCount - 1 do
    begin
      NoSteps := False;
      if N = 0 then
        Break;
      var
      Step := Steps[idx];
      if ApplyStep(Connection, Script, 'down', Step) then
        UpdateStatus(Steps[idx + 1].ID);
      ReportAppliedStep(Script);
      Dec(N);
    end;
    if NoSteps then
      WriteLn('No applied steps found');
  finally
    Connection.Free;
  end;
end;

procedure TDbMigratorEngine.LoadConfig;
begin
  var JsonSrc: string;
  if not TFile.Exists(ConfigFile) then
    JsonSrc := DefaultConfigTT
  else
    JsonSrc := TFile.ReadAllText(ConfigFile, TEncoding.UTF8);

  var NewCfg := TJson.JsonToObject<TFDMigratorCfg>(JsonSrc);
  if NewCfg <> nil then
  begin
    FConfig.Free;
    FConfig := NewCfg;
  end;

end;

procedure TDbMigratorEngine.SaveConfig;
begin
  CheckInitDone;
  var JsonSrc := TJson.ObjectToJsonString(FConfig);
  TFile.WriteAllText(ConfigFile, JsonSrc, TEncoding.UTF8);
end;

function TDbMigratorEngine.GetSteps(Criteria: TStepsCriteria;
  Reverse: Boolean = False): TArray<TStep>;
begin
  CheckInitDone;
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
      if (Criteria = []) or (Step.StateFor(Config.Status) in Criteria) then
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

function TDbMigratorEngine.CreateFile(const Path: string;
const FileName: string; const Content: string = ''): Boolean;
begin
  TDirectory.CreateDirectory(Path);
  var
  FullFileName := TPath.Combine(Path, FileName);
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
  FConfigFile := TPath.Combine(FMigratorDir, '.fdmigrator.json');
  LoadConfig;
end;

procedure TDbMigratorEngine.Init(Kind: TStepIdKind; Fmt: string);
begin
  CreateFile(MigratorDir, 'FDConnectionDefs.ini', FDConnectionDefsTT);
  CreateFile(StepFilesDir, '__prelude__', PreludeTT);
  CreateFile(StepFilesDir, '__MySQL.prelude', MySQLPreludeTT);
  CreateFile(StepFilesDir, '__MSSQL.prelude', MSSQLPreludeTT);

  LoadConfig;//ensure FConfig is created;
  FConfig.StepKind := Kind;
  FConfig.StepFormat := Fmt;
  SaveConfig;
end;

procedure TDbMigratorEngine.SetMigratorDir(const Value: string);
begin
  FMigratorDir := Value;
  Reset;
end;

function TDbMigratorEngine.GetConfig: TFDMigratorCfg;
begin
  CheckInitDone;
  LoadConfig;
  Result := FConfig;
end;

function TDbMigratorEngine.GetDefs: TArray<string>;
begin
  if FDefs = nil then
  begin
    CheckInitDone;
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

end.
