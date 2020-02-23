(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified August 2019                                *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.constants.txt> }
unit Codebot.Constants;

{$i codebot.inc}

interface

resourcestring
  SRangeError = 'Range check error';
  SRangeMethodError = 'Index exceeds bounds range in method %s.%s';
  SListDuplicateError = 'Duplicate list items are not allowed';
  SIntOverflow = 'Arithmetic overflow';
  SInvalidOp = 'Invalid floating point operation';
  SZeroDivide = 'Floating point division by zero';
  SOverflow = 'Floating point overflow';
  SUnderflow = 'Floating point underflow';
  SObjectCheck = 'Object is nil';
  SInvalidCast = 'Invalid type cast';
  SBusInvalid = 'Bus error or misaligned data access';
  SAccessViolation = 'Access violation';
  SPrivInstruction = 'Privileged instruction';
  SControlBreak = 'Control break detected';
  SStackOverflow = 'Stack overflow';
  SIOFileHandleError = 'Unable to create file handle';
  SConvertError = 'Unable to convert %s to %s';
  SSynchronizeError = 'An error occured with thread syncrhonziation';
  SInvalidGraphicFormat = 'Invalid graphic format';
  SInvalidGraphicSize = 'Invalid graphic size';
  SCouldNotLockBits = 'Could not lock bits';
  SScanLine = 'Scan line access error';
  SSurfaceAccess = 'Surface is only available during the Render method of %s';

implementation

end.


