{ included by codebot.text.xml.pas }

{$ifdef linux}
{$region xml interface}
uses
  Codebot.Interop.Linux.Xml2;

type
  TXPathResult = array of Pointer;

function PCharToStr(P: PChar): string;
begin
  Result := P;
end;

{ libxml2 helper functions }

function xmlGetElementIndex(Node: xmlNodePtr): Integer;
var
  N: xmlNodePtr;
begin
  Result := 1;
  if Node._type <> XML_ELEMENT_NODE then
    Exit;
  if Node.parent = nil then
    Exit;
  N := Node.parent.children;
  while N <> Node do
  begin
    if N._type = XML_ELEMENT_NODE then
      Inc(Result);
    N := N.next;
  end;
end;

function xmlRelativeQuery(Node: xmlNodePtr; const XPath: string): string;
var
  S: string;
  I: Integer;
begin
  Result := XPath;
  if Result.Length = 0 then
    Exit;
  if Result[1] = '/' then
    Exit;
  if Result[1] = '.' then
    Exit;
  while Node <> nil do
  begin
    if Node._type <> XML_ELEMENT_NODE then
      Break;
    I := xmlGetElementIndex(Node);
    Str(I, S);
    Result := Node.name + '[' + S + ']/' + Result;
    Node := Node.parent;
  end;
  Result := '/' + Result;
end;

function xmlConvertXPath(PathObject: xmlXPathObjectPtr): TXPathResult;
var
  Nodes: xmlNodeSetPtr;
  List: xmlNodePtrPtr;
  Node: xmlNodePtr;
  I: Integer;
begin
  Result := nil;
  if (PathObject = nil) or (PathObject.nodesetval = nil) then
    Exit;
  Nodes := PathObject.nodesetval;
  if Nodes.nodeNr < 1 then
    Exit;
  SetLength(Result, Nodes.nodeNr);
  List := Nodes.nodeTab;
  for I := 0 to Nodes.nodeNr - 1 do
  begin
    Node := List^;
    Result[I] := Node;
    Inc(List);
  end;
end;

function xmlFirstElementChild(Node: xmlNodePtr) : xmlNodePtr;
begin
  Result := nil;
  if Node = nil then
    Exit;
  Node := Node.children;
  while Node <> nil do
    if Node._type = XML_ELEMENT_NODE then
    begin
      Result := Node;
      Break;
    end
    else
      Node := Node.next;
end;

function xmlLastElementChild(Node: xmlNodePtr) : xmlNodePtr;
var
  Last: xmlNodePtr;
begin
  Result := nil;
  if Node = nil then
    Exit;
  Node := Node.children;
  Last := nil;
  while Node <> nil do
    if Node._type = XML_ELEMENT_NODE then
    begin
      Last := Node;
      Node := Node.next;
    end
    else
      Node := Node.next;
  Result := Last;
end;

function xmlNextElementSibling(Node: xmlNodePtr) : xmlNodePtr;
begin
  Result := nil;
  if Node = nil then
    Exit;
  Node := Node.next;
  while Node <> nil do
    if Node._type = XML_ELEMENT_NODE then
    begin
      Result := Node;
      Break;
    end
    else
      Node := Node.next;
end;

function xmlGetElementChild(Node: xmlNodePtr; Index: Integer): xmlNodePtr;
begin
  Result := nil;
  if Node = nil then
    Exit;
  Node := xmlFirstElementChild(Node);
  if Node = nil then
    Exit;
  while Index > 0 do
  begin
    Dec(Index);
    Node := xmlNextElementSibling(Node);
    if Node = nil then
      Exit;
  end;
  Result := Node;
end;

function xmlGetElementChildCount(Node: xmlNodePtr): Integer;
begin
  Result := 0;
  if Node = nil then
    Exit;
  Node := xmlFirstElementChild(Node);
  if Node = nil then
    Exit;
  while Node <> nil do
  begin
    Inc(Result);
    Node := xmlNextElementSibling(Node);
  end;
end;

function xmlGetElementByName(Node: xmlNodePtr; const Name: string): xmlNodePtr;
begin
  Result := nil;
  if Node = nil then
    Exit;
  Node := xmlFirstElementChild(Node);
  while Node <> nil do
    if PCharToStr(Node.name) = Name then
    begin
      Result := Node;
      Break;
    end
    else
      Node := xmlNextElementSibling(Node);
end;

function xmlFirstAttributeChild(Node: xmlNodePtr) : xmlAttrPtr;
var
  Attr: xmlAttrPtr;
begin
  Result := nil;
  if Node = nil then
    Exit;
  Attr := Node.properties;
  while Attr <> nil do
    if Attr._type = XML_ATTRIBUTE_NODE then
    begin
      Result := Attr;
      Break;
    end
    else
      Attr := Attr.next;
end;

function xmlNextAttributeSibling(Attr: xmlAttrPtr) : xmlAttrPtr;
begin
  Result := nil;
  if Attr = nil then
    Exit;
  if Attr._type = XML_ATTRIBUTE_NODE then
    Attr := Attr.next
  else
    Exit;
  while Attr <> nil do
    if Attr._type = XML_ATTRIBUTE_NODE then
    begin
      Result := Attr;
      Break;
    end
    else
      Attr := Attr.next;
end;

function xmlGetAttributeChild(Node: xmlNodePtr; Index: Integer): xmlAttrPtr;
var
  Attr: xmlAttrPtr;
begin
  Result := nil;
  if Node = nil then
    Exit;
  Attr := xmlFirstAttributeChild(Node);
  if Attr = nil then
    Exit;
  while Index > 0 do
  begin
    Dec(Index);
    Attr := xmlNextAttributeSibling(Attr);
    if Attr = nil then
      Exit;
  end;
  Result := Attr;
end;

function xmlGetAttributeChildCount(Node: xmlNodePtr): Integer;
var
  Attr: xmlAttrPtr;
begin
  Result := 0;
  if Node = nil then
    Exit;
  Attr := xmlFirstAttributeChild(Node);
  if Attr = nil then
    Exit;
  while Attr <> nil do
  begin
    Inc(Result);
    Attr := xmlNextAttributeSibling(Attr);
  end;
end;

function xmlGetAttributeByName(Node: xmlNodePtr; const Name: string): xmlAttrPtr;
var
  Attr: xmlAttrPtr;
begin
  Result := nil;
  if Node = nil then
    Exit;
  Attr := xmlFirstAttributeChild(Node);
  while Attr <> nil do
    if PCharToStr(Attr.name) = Name then
    begin
      Result := Attr;
      Break;
    end
    else
      Attr := xmlNextAttributeSibling(Attr);
end;

function xmlGetAttributeByIndex(Node: xmlNodePtr; Index: Integer): xmlAttrPtr;
var
  Attr: xmlAttrPtr;
begin
  Result := nil;
  if (Node = nil) or (Index < 0) then
    Exit;
  Attr := xmlFirstAttributeChild(Node);
  while Attr <> nil do
  begin
    if Index = 0 then
    begin
      Result := Attr;
      Break;
    end;
    Dec(Index);
    Attr := xmlNextAttributeSibling(Attr);
  end;
end;

function xmlFirstTextChild(Node: xmlNodePtr) : xmlNodePtr;
begin
  Result := nil;
  if Node = nil then
    Exit;
  Node := Node.children;
  while Node <> nil do
    if Node._type = XML_TEXT_NODE then
    begin
      Result := Node;
      Break;
    end
    else
      Node := Node.next;
end;

function xmlNextTextSibling(Node: xmlNodePtr) : xmlNodePtr;
begin
  Result := nil;
  if Node = nil then
    Exit;
  Node := Node.next;
  while Node <> nil do
    if Node._type = XML_TEXT_NODE then
    begin
      Result := Node;
      Break;
    end
    else
      Node := Node.next;
end;

procedure xmlFormatDocument(Node: xmlNodePtr; Prefix: string);
var
  N, C: xmlNodePtr;
  S: string;
begin
  if Node = nil then
    Exit;
  S := Prefix + #9;
  N := xmlFirstElementChild(Node);
  while N <> nil do
  begin
    C := xmlNewText(PChar(S));
    xmlAddPrevSibling(N, C);
    xmlFormatDocument(N, S);
    N := xmlNextElementSibling(N);
  end;
  N := xmlLastElementChild(Node);
  if N <> nil then
  begin
    C := xmlNewText(PChar(Prefix));
    xmlAddChild(Node, C);
  end;
end;

procedure free(Ptr: Pointer); cdecl; external 'c';

function xmlGetText(Node: xmlNodePtr): string;
var
  B: PChar;
begin
  Result := '';
  if Node = nil then
    Exit;
  B := xmlNodeGetContent(Node);
  if B = nil then
    Exit;
  Result := B;
  free(B);
end;

procedure xmlRemoveEmptyText(Node: xmlNodePtr);
var
  N, B: xmlNodePtr;
  S: string;
begin
  if Node = nil then
    Exit;
  N := xmlFirstTextChild(Node);
  while N <> nil do
  begin
    S := StrTrim(xmlGetText(N));
    if S = '' then
    begin
      B := xmlNextTextSibling(N);
      xmlUnlinkNode(N);
      xmlFreeNode(N);
      N := B;
    end
    else
      N := xmlNextTextSibling(N);
  end;
  N := xmlFirstElementChild(Node);
  while N <> nil do
  begin
    xmlRemoveEmptyText(N);
    N := xmlNextElementSibling(N);
  end;
end;

type
  TDocument = class;
  TNodeList = class;

{ TNode }

  TNode = class(TInterfacedObject, INode)
  private
    FNode: xmlNodePtr;
    function Execute(XPath: string): TXPathResult;
  public
    function Instance: Pointer;
    function Next: INode;
    function SelectNode(const XPath: string): INode;
    function SelectList(const XPath: string): INodeList; overload;
    function SelectList(const XPath: string; out List: INodeList): Boolean; overload;
    function Force(const Path: string): INode;
    function GetDocument: IDocument;
    function GetParent: INode;
    function GetFiler: IFiler;
    function GetAttributes: INodeList;
    function GetNodes: INodeList;
    function GetKind: TNodeKind;
    function GetName: string;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetXml: string;
    procedure SetXml(const Value: string);
  public
    constructor Create(Node: xmlNodePtr);
  end;

{ TNodeList }

  TNodeList = class(TInterfacedObject, INodeList)
  private
    FNode: xmlNodePtr;
    FNodeType: xmlElementType;
  public
    function GetCount: Integer;
    function GetByName(const Name: string): INode; overload;
    function GetByIndex(Index: Integer): INode; overload;
    procedure Clear;
    procedure Add(Node: INode); overload;
    function Add(const Name: string): INode; overload;
    procedure Remove(Node: INode); overload;
    procedure Remove(const Name: string); overload;
  public
    constructor Create(Node: xmlNodePtr; NodeType: xmlElementType);
    function GetEnumerator: IEnumerator<INode>;
  end;

{ TXPathList }

  TXPathList = class(TInterfacedObject, INodeList)
  private
    FXPathResult: TXPathResult;
  public
    function GetCount: Integer;
    function GetByName(const Name: string): INode; overload;
    function GetByIndex(Index: Integer): INode; overload;
    procedure Clear;
    procedure Add({%H-}Node: INode); overload;
    function Add(const {%H-}Name: string): INode; overload;
    procedure Remove({%H-}Node: INode); overload;
    procedure Remove(const {%H-}Name: string); overload;
  public
    constructor Create(const XPathResult: TXPathResult);
    function GetEnumerator: IEnumerator<INode>;
  end;

{ TDocument }

  TDocument = class(TNode, IDocument)
  private
    function GetDocumentPtr: xmlDocPtr;
    procedure SetDocumentPtr(Value: xmlDocPtr);
  public
    procedure SetRoot(Value: INode);
    function GetRoot: INode;
    procedure Beautify;
    function CreateAttribute(const Name: string): INode;
    function CreateElement(const Name: string): INode;
    procedure Load(const FileName: string);
    procedure Save(const FileName: string);
  public
    constructor Create(Doc: xmlDocPtr);
    destructor Destroy; override;
  end;

function DocCreate: xmlNodePtr;
var
  Doc: xmlDocPtr;
begin
  Doc := xmlNewDoc('1.0');
  Doc._private := Pointer(1);
  Result := xmlNodePtr(Doc);
end;

function DocParseFile(const FileName: string): xmlNodePtr;
var
  Doc: xmlDocPtr;
begin
  Doc := xmlParseFile(PChar(FileName));
  if Doc = nil then
    Doc := xmlNewDoc('1.0');
  Doc._private := Pointer(1);
  Result := xmlNodePtr(Doc);
end;

function DocParseText(const Text: string): xmlNodePtr;
var
  Doc: xmlDocPtr;
begin
  Doc := xmlParseDoc(PChar(Text));
  if Doc = nil then
    Doc := xmlNewDoc('1.0');
  Doc._private := Pointer(1);
  Result := xmlNodePtr(Doc);
end;

function DocAddRef(Doc: xmlNodePtr): xmlNodePtr;
begin
  if Doc <> nil then
    InterLockedIncrement(Doc._private);
  Result := Doc;
end;

procedure DocRelease(Doc: xmlNodePtr);
var
  P: Pointer;
begin
  if Doc <> nil then
  begin
    P := InterLockedDecrement(Doc._private);
    if P = nil then
      xmlFreeDoc(xmlDocPtr(Doc));
  end;
end;

{ TNode }

constructor TNode.Create(Node: xmlNodePtr);
begin
  inherited Create;
  FNode := Node;
end;

function TNode.Instance: Pointer;
begin
  Result := Self;
end;

function TNode.Next: INode;
var
  N: xmlNodePtr;
  T: xmlElementType;
begin
  Result := nil;
  N := FNode;
  T := N._type;
  if T = XML_DOCUMENT_NODE then
    Exit;
  N := N.next;
  while N <> nil do
    if N._type = T then
      Exit(TNode.Create(N))
    else
      N := N.next;
end;

function TNode.GetDocument: IDocument;
var
  D: xmlDocPtr;
begin
  if Self is TDocument then
    Result := Self as IDocument
  else
  begin
    D := FNode.doc;
    if D <> nil then
      Result := TDocument.Create(D)
    else
      Result := nil;
  end;
end;

function TNode.GetParent: INode;
var
  N: xmlNodePtr;
begin
  N := FNode.parent;
  if N <> nil then
    Result := TNode.Create(N)
  else
    Result := nil;
end;

function TNode.GetFiler: IFiler;
begin
  Result := FilerCreate(GetDocument, Self);
end;

function TNode.Execute(XPath: string): TXPathResult;
var
  Context: xmlXPathContextPtr;
  PathObject: xmlXPathObjectPtr;
begin
  Context := xmlXPathNewContext(FNode.doc);
  Context.node := FNode;
  PathObject := xmlXPathEval(PChar(XPath), Context);
  Result := xmlConvertXPath(PathObject);
  xmlXPathFreeObject(PathObject);
  xmlXPathFreeContext(Context);
end;

function TNode.SelectNode(const XPath: string): INode;
var
  R: TXPathResult;
begin
  R := Execute(XPath);
  if Length(R) > 0 then
    Result := TNode.Create(R[0])
  else
    Result := nil;
end;

function TNode.SelectList(const XPath: string): INodeList;
begin
  Result := TXPathList.Create(Execute(XPath));
end;

function TNode.SelectList(const XPath: string; out List: INodeList): Boolean;
begin
  List := TXPathList.Create(Execute(XPath));
  Result := List.Count > 0;
end;

function TNode.Force(const Path: string): INode;
var
  Items: StringArray;
  D: IDocument;
  N, C: INode;
  S: string;
  I: Integer;
begin
  Result := nil;
  Items := Path.Split('/');
  if Items.IsEmpty then
    Exit;
  D := GetDocument;
  if D = nil then
    Exit;
  I := 0;
  S := Items[I];
  case FNode._type of
    XML_ELEMENT_NODE:
      N := Self;
    XML_DOCUMENT_NODE:
      begin
        Inc(I);
        N := D.Root;
        if (N = nil) or (N.Name <> S) then
        begin
          N := D.CreateElement(S);
          if N = nil then
            Exit;
          D.SetRoot(N);
        end;
      end;
  else
    Exit;
  end;
  while I < Items.Length do
  begin
    if N.Kind <> nkElement then
      Exit;
    S := Items[I];
    Inc(I);
    C := N.SelectNode(S);
    if C <> nil then
    begin
      N := C;
      Continue;
    end;
    if StrIsAttr(S) then
      N := N.Attributes.Add(S.Copy(2))
    else if StrIsIdent(S) then
      N := N.Nodes.Add(S);
  end;
  Result := N;
end;

function TNode.GetAttributes: INodeList;
begin
  Result := TNodeList.Create(FNode, XML_ATTRIBUTE_NODE);
end;

function TNode.GetNodes: INodeList;
begin
  Result := TNodeList.Create(FNode, XML_ELEMENT_NODE);
end;

function TNode.GetKind: TNodeKind;
begin
  case FNode._type of
    XML_ELEMENT_NODE:
      Result := nkElement;
    XML_ATTRIBUTE_NODE:
      Result := nkAttribute;
    XML_TEXT_NODE:
      Result := nkText;
    XML_DOCUMENT_NODE:
      Result := nkDocument;
  else
    Result := nkOther;
  end;
end;

function TNode.GetName: string;
begin
  case FNode._type of
    XML_ELEMENT_NODE:
      Result := FNode.name;
    XML_ATTRIBUTE_NODE:
      Result := FNode.name;
    XML_DOCUMENT_NODE:
      Result := 'DOCUMENT';
  else
    Result := FNode.name;
  end;
end;

function TNode.GetText: string;
begin
  if FNode._type = XML_DOCUMENT_NODE then
    Result := GetXml
  else
    Result := xmlGetText(FNode);
end;

procedure TNode.SetText(const Value: string);
var
  B: PChar;
begin
  case FNode._type of
    XML_ELEMENT_NODE,
    XML_ATTRIBUTE_NODE:
      begin
        B := xmlEncodeSpecialChars(FNode.doc, PChar(Value));
        xmlNodeSetContent(FNode, B);
        xmlMemFree(B);
      end;
    XML_DOCUMENT_NODE:
      SetXml(Value);
  else
    // Do nothing
  end;
end;

function TNode.GetXml: string;
var
  B: xmlOutputBufferPtr;
  S: string;
begin
  B := xmlAllocOutputBuffer(nil);
  xmlNodeDump(B.buffer, FNode.doc, FNode, 0, 1);
  S := B.buffer.content;
  xmlOutputBufferFlush(B);
  xmlOutputBufferClose(B);
  Result := S;
end;

procedure TNode.SetXml(const Value: string);
begin
  if FNode._type = XML_DOCUMENT_NODE then
  begin
    DocRelease(FNode);
    FNode := DocParseText(Value);
  end;
end;

{ TNodeListEnumerator }

type
  TNodeListEnumerator = class(TInterfacedObject, IEnumerator<INode>)
  private
    FList: INodeList;
    FPosition: Integer;
  public
    constructor Create(List: INodeList);
    function GetCurrent: INode;
    function MoveNext: Boolean;
    procedure Reset;
  end;

constructor TNodeListEnumerator.Create(List: INodeList);
begin
  inherited Create;
  FList := List;
  FPosition := -1;
end;

function TNodeListEnumerator.GetCurrent: INode;
begin
  Result := FList[FPosition];
end;

function TNodeListEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

procedure TNodeListEnumerator.Reset;
begin
  FPosition := -1;
end;

{ TNodeList }

constructor TNodeList.Create(Node: xmlNodePtr; NodeType: xmlElementType);
begin
  inherited Create;
  FNode := Node;
  FNodeType := NodeType;
end;

function TNodeList.GetEnumerator: IEnumerator<INode>;
begin
  Result := TNodeListEnumerator.Create(Self);
end;

procedure TNodeList.Clear;
var
  N: xmlNodePtr;
begin
  case FNodeType of
    XML_ELEMENT_NODE:
      while FNode.children <> nil do
      begin
        N := FNode.children;
        xmlUnlinkNode(N);
        xmlFreeNode(N);
      end;
    XML_ATTRIBUTE_NODE:
      while FNode.properties <> nil do
        xmlRemoveProp(FNode.properties);
  else
    // Do nothing ...
  end;
end;

procedure TNodeList.Add(Node: INode);
var
  N: xmlNodePtr;
begin
  N := TNode(Node.Instance).FNode;
  xmlUnlinkNode(N);
  TNode(Node.Instance).FNode := xmlAddChild(FNode, N);
end;

function TNodeList.Add(const Name: string): INode;
var
  N: xmlNodePtr;
  A: xmlAttrPtr;
begin
  case FNodeType of
    XML_ELEMENT_NODE:
      begin
        N := xmlNewNode(nil, PChar(Name));
        N := xmlAddChild(FNode, N);
        Result := TNode.Create(N);
      end;
    XML_ATTRIBUTE_NODE:
      begin
        A := xmlGetAttributeByName(FNode, Name);
        if A = nil then
          A := xmlNewProp(FNode, PChar(Name), ' ');
        Result := TNode.Create(xmlNodePtr(A));
      end;
  else
    Result := nil;
  end;
end;

procedure TNodeList.Remove(Node: INode);
var
  N: xmlNodePtr;
begin
  N := TNode(Node.Instance).FNode;
  xmlUnlinkNode(N);
  xmlFreeNode(N);
end;

procedure TNodeList.Remove(const Name: string);
var
  N: xmlNodePtr;
  A: xmlAttrPtr;
begin
  case FNodeType of
    XML_ELEMENT_NODE:
      begin
        N := xmlGetElementByName(FNode, Name);
        if N <> nil then
        begin
          xmlUnlinkNode(N);
          xmlFreeNode(N);
        end;
      end;
    XML_ATTRIBUTE_NODE:
      begin
        A := xmlGetAttributeByName(FNode, Name);
        if A <> nil then
          xmlRemoveProp(A);
      end;
  else
    // Do nothing ...
  end;
end;

function TNodeList.GetByIndex(Index: Integer): INode;
var
  N: xmlNodePtr;
  A: xmlAttrPtr;
begin
  Result := nil;
  case FNodeType of
    XML_ELEMENT_NODE:
      begin
        N := xmlGetElementChild(FNode, Index);
        if N <> nil then
          Result := TNode.Create(N);
      end;
    XML_ATTRIBUTE_NODE:
      begin
        A := xmlGetAttributeChild(FNode, Index);
        if (A <> nil) then
          Result := TNode.Create(N);
      end;
  else
    // Do nothing ...
  end;
end;

function TNodeList.GetByName(const Name: string): INode;
var
  N: xmlNodePtr;
  A: xmlAttrPtr;
begin
  Result := nil;
  case FNodeType of
    XML_ELEMENT_NODE:
      begin
        N := xmlGetElementByName(FNode, Name);
        if N <> nil then
          Result := TNode.Create(N);
      end;
    XML_ATTRIBUTE_NODE:
      begin
        A := xmlGetAttributeByName(FNode, Name);
        if A <> nil then
          Result := TNode.Create(xmlNodePtr(A));
      end;
  else
    // Do nothing ...
  end;
end;

function TNodeList.GetCount: Integer;
begin
  Result := 0;
  case FNodeType of
    XML_ELEMENT_NODE:
      Result := xmlGetElementChildCount(FNode);
    XML_ATTRIBUTE_NODE:
      Result := xmlGetAttributeChildCount(FNode);
  else
    // Do nothing ...
  end;
end;

{ TXPathListEnumerator }

type
  TXPathListEnumerator = class(TInterfacedObject, IEnumerator<INode>)
  private
    FList: TXPathResult;
    FPosition: Integer;
  public
    constructor Create(List: TXPathResult);
    function GetCurrent: INode;
    function MoveNext: Boolean;
    procedure Reset;
  end;

constructor TXPathListEnumerator.Create(List: TXPathResult);
begin
  inherited Create;
  FList := List;
  FPosition := -1;
end;

function TXPathListEnumerator.GetCurrent: INode;
begin
  Result := TNode.Create(FList[FPosition]);
end;

function TXPathListEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < Length(FList);
end;

procedure TXPathListEnumerator.Reset;
begin
  FPosition := -1;
end;

{ TXPathList }

constructor TXPathList.Create(const XPathResult: TXPathResult);
begin
  inherited Create;
  FXPathResult := XPathResult;
end;

function TXPathList.GetEnumerator: IEnumerator<INode>;
begin
  Result := TXPathListEnumerator.Create(FXPathResult);
end;

procedure TXPathList.Clear;
begin
end;

procedure TXPathList.Add(Node: INode);
begin
end;

function TXPathList.Add(const Name: string): INode;
begin
  Result := nil;
end;

procedure TXPathList.Remove(Node: INode);
begin
end;

procedure TXPathList.Remove(const Name: string);
begin
end;

function TXPathList.GetByIndex(Index: Integer): INode;
begin
  Result := nil;
  if (Index < 0) or (Index >= Length(FXPathResult)) then
    Exit;
  Result := TNode.Create(FXPathResult[Index]);
end;

function TXPathList.GetByName(const Name: string): INode;
var
  N: xmlNodePtr;
  I: Integer;
begin
  Result := nil;
  for I := 0 to Length(FXPathResult) - 1 do
  begin
    N := xmlNodePtr(FXPathResult[I]);
    if PCharToStr(N.name) = Name then
    begin
      Result := TNode.Create(N);
      Break;
    end;
  end;
end;

function TXPathList.GetCount: Integer;
begin
  Result := Length(FXPathResult);
end;

{ TDocument }

constructor TDocument.Create(Doc: xmlDocPtr);
begin
  inherited Create(nil);
  SetDocumentPtr(Doc);
end;

destructor TDocument.Destroy;
begin
  DocRelease(FNode);
  inherited Destroy;
end;

function TDocument.GetDocumentPtr: xmlDocPtr;
begin
  Result := xmlDocPtr(FNode);
end;

procedure TDocument.SetDocumentPtr(Value: xmlDocPtr);
begin
  if (FNode = nil) or (Pointer(Value) <> Pointer(FNode)) then
  begin
    DocRelease(FNode);
    FNode := xmlNodePtr(Value);
    if FNode = nil then
      FNode := DocCreate
    else
      DocAddRef(FNode);
  end;
end;

procedure TDocument.Beautify;
begin
  xmlRemoveEmptyText(FNode);
  xmlFormatDocument(xmlFirstElementChild(FNode), LineEnding);
end;

function TDocument.CreateAttribute(const Name: string): INode;
begin
  if StrIsIdent(Name) then
    Result := TNode.Create(xmlNodePtr(xmlNewDocProp(GetDocumentPtr, PChar(Name), ' ')))
  else
    Result := nil;
end;

function TDocument.CreateElement(const Name: string): INode;
begin
  if StrisIdent(Name) then
    Result := TNode.Create(xmlNewNode(nil, PChar(Name)))
  else
    Result := nil;
end;

procedure TDocument.SetRoot(Value: INode);
begin
  if Value = nil then
    SetDocumentPtr(nil)
  else
    xmlDocSetRootElement(GetDocumentPtr, TNode(Value.Instance).FNode);
end;

function TDocument.GetRoot: INode;
var
  N: xmlNodePtr;
begin
  N := xmlDocGetRootElement(GetDocumentPtr);
  if N <> nil then
    Result := TNode.Create(N)
  else
    Result := nil;
end;

procedure TDocument.Load(const FileName: string);
begin
  DocRelease(FNode);
  FNode := DocParseFile(FileName);
end;

procedure TDocument.Save(const FileName: string);
begin
  xmlSaveFile(PChar(FileName), GetDocumentPtr);
end;

{ DocumentCreate }

function DocumentCreate: IDocument;
begin
  Result := NewDocument;
end;

function NewDocument: IDocument;
begin
  Xml2Init(True);
  Result := TDocument.Create(nil);
end;

{$endregion}
{$endif}

