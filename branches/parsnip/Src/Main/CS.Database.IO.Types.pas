unit CS.Database.IO.Types;

interface

uses
  CS.Database.SnippetsTable,
  CS.Database.Types,
  CS.Utils.Dates;

type

  IDatabaseLoader = interface(IInterface)
    ['{8AC208B5-D0D2-498D-92A8-BD62D4ECF02D}']
    procedure Load(const ATable: TDBSnippetsTable; out ATagSet: ITagSet;
      out ALastModified: TUTCDateTime);
    function DatabaseExists(const Path: string): Boolean;
  end;

implementation

end.
