unit CS.Database.Exceptions;

interface

uses
  UExceptions;

type
  EDatabaseFault = class(EBug);

  EDatabaseError = class(ECodeSnip);

implementation

end.
