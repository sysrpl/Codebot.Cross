(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified August 2019                                *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.interop.linux.xml2.txt> }
unit Codebot.Interop.Linux.Xml2;

{$i codebot.inc}

interface

{$ifdef linux}
uses
  Codebot.Core;

type
  cint = LongInt;
  pcint = PLongInt;
  cuint = LongWord;
  cushort = Word;
  clong = Integer;
  culong = Cardinal;
  csize_t = Cardinal;
  cdouble = Double;

  xmlCharPtr = PChar;
  xmlCharPtrPtr = PPChar;
  xmlChar = Char;
  xmlBufferPtr = ^xmlBuffer;
  xmlNotationPtr = ^xmlNotation;
  xmlEnumerationPtr = ^xmlEnumeration;
  xmlAttributePtr = ^xmlAttribute;
  xmlElementContentPtr = ^xmlElementContent;
  xmlElementPtr = ^xmlElement;
  xmlNsPtr = ^xmlNs;
  xmlNsPtrPtr = ^xmlNsPtr;
  xmlNodePtr = ^xmlNode;
  xmlNodePtrPtr = ^xmlNodePtr;
  xmlDtdPtr = ^xmlDtd;
  xmlAttrPtr = ^xmlAttr;
  xmlIDPtr = ^xmlID;
  xmlRefPtr = ^xmlRef;
  xmlDocPtr = ^xmlDoc;
  xmlBufferAllocationSchemePtr = ^xmlBufferAllocationScheme;
  xmlParserInputBufferPtr = ^xmlParserInputBuffer;
  xmlOutputBufferPtr = ^xmlOutputBuffer;
  xmlXPathContextPtr = ^xmlXPathContext;
  xmlXPathParserContextPtr = ^xmlXPathParserContext;
  xmlNodeSetPtr = ^xmlNodeSet;
  xmlXPathObjectPtr = ^xmlXPathObject;
  xmlXPathObjectPtrPtr = ^xmlXPathObjectPtr;
  xmlXPathTypePtr = ^xmlXPathType;
  xmlXPathVariablePtr = ^xmlXPathVariable;
  xmlXPathFuncPtr = ^xmlXPathFunc;
  xmlXPathAxisPtr = ^xmlXPathAxis;
  xmlXPathCompExprPtr = ^xmlXPathCompExpr;
  xmlCharEncodingHandlerPtr = ^xmlCharEncodingHandler;
  xmlDictPtr = Pointer;
  xmlHashTablePtr = Pointer;

  xmlCharEncoding = (
    XML_CHAR_ENCODING_ERROR = -1, (* No char encoding detected *)
    XML_CHAR_ENCODING_NONE = 0, (* No char encoding detected *)
    XML_CHAR_ENCODING_UTF8 = 1, (* UTF-8 *)
    XML_CHAR_ENCODING_UTF16LE = 2, (* UTF-16 little endian *)
    XML_CHAR_ENCODING_UTF16BE = 3, (* UTF-16 big endian *)
    XML_CHAR_ENCODING_UCS4LE = 4, (* UCS-4 little endian *)
    XML_CHAR_ENCODING_UCS4BE = 5, (* UCS-4 big endian *)
    XML_CHAR_ENCODING_EBCDIC = 6, (* EBCDIC uh! *)
    XML_CHAR_ENCODING_UCS4_2143= 7, (* UCS-4 unusual ordering *)
    XML_CHAR_ENCODING_UCS4_3412= 8, (* UCS-4 unusual ordering *)
    XML_CHAR_ENCODING_UCS2=  9, (* UCS-2 *)
    XML_CHAR_ENCODING_8859_1 = 10, (* ISO-8859-1 ISO Latin 1 *)
    XML_CHAR_ENCODING_8859_2 = 11, (* ISO-8859-2 ISO Latin 2 *)
    XML_CHAR_ENCODING_8859_3 = 12, (* ISO-8859-3 *)
    XML_CHAR_ENCODING_8859_4 = 13, (* ISO-8859-4 *)
    XML_CHAR_ENCODING_8859_5 = 14, (* ISO-8859-5 *)
    XML_CHAR_ENCODING_8859_6 = 15, (* ISO-8859-6 *)
    XML_CHAR_ENCODING_8859_7 = 16, (* ISO-8859-7 *)
    XML_CHAR_ENCODING_8859_8 = 17, (* ISO-8859-8 *)
    XML_CHAR_ENCODING_8859_9 = 18, (* ISO-8859-9 *)
    XML_CHAR_ENCODING_2022_JP = 19, (* ISO-2022-JP *)
    XML_CHAR_ENCODING_SHIFT_JIS = 20, (* Shift_JIS *)
    XML_CHAR_ENCODING_EUC_JP = 21, (* EUC-JP *)
    XML_CHAR_ENCODING_ASCII = 22 (* pure ASCII *)
  );

  xmlCharEncodingInputFunc = function(_out: pchar; outlen: pcint; _in: pchar; inlen: pcint): cint; cdecl;
  xmlCharEncodingOutputFunc = function(_out: pchar; outlen: pcint; _in: pchar; inlen: pcint): cint; cdecl;

  xmlCharEncodingHandler = record
    name: pchar;
    input: xmlCharEncodingInputFunc;
    output: xmlCharEncodingOutputFunc;
  end;

(*
  * xmlBufferAllocationScheme:
  *
  * A buffer allocation scheme can be defined to either match exactly the
  * need or double it's allocated size each time it is found too small.
  *)
  xmlBufferAllocationScheme = (
    XML_BUFFER_ALLOC_DOUBLEIT,
    XML_BUFFER_ALLOC_EXACT,
    XML_BUFFER_ALLOC_IMMUTABLE
  );

(*
  * xmlBuffer:
  *
  * A buffer structure.
  *)
  xmlBuffer = record
    content: xmlCharPtr; (* The buffer content UTF8 *)
    use: cuint; (* The buffer size used *)
    size: cuint; (* The buffer size *)
    alloc: xmlBufferAllocationScheme; (* The realloc method *)
  end;

(*
  * The different element types carried by an XML tree.
  *
  * NOTE: This is synchronized with DOM Level1 values
  *       See http://www.w3.org/TR/REC-DOM-Level-1/
  *
  * Actually this had diverged a bit, and now XML_DOCUMENT_TYPE_NODE should
  * be deprecated to use an XML_DTD_NODE.
  *)
  xmlElementType = (
    XML_ELEMENT_NODE = 1,
    XML_ATTRIBUTE_NODE = 2,
    XML_TEXT_NODE = 3,
    XML_CDATA_SECTION_NODE = 4,
    XML_ENTITY_REF_NODE = 5,
    XML_ENTITY_NODE = 6,
    XML_PI_NODE = 7,
    XML_COMMENT_NODE = 8,
    XML_DOCUMENT_NODE = 9,
    XML_DOCUMENT_TYPE_NODE = 10,
    XML_DOCUMENT_FRAG_NODE = 11,
    XML_NOTATION_NODE = 12,
    XML_HTML_DOCUMENT_NODE = 13,
    XML_DTD_NODE = 14,
    XML_ELEMENT_DECL = 15,
    XML_ATTRIBUTE_DECL = 16,
    XML_ENTITY_DECL = 17,
    XML_NAMESPACE_DECL = 18,
    XML_XINCLUDE_START = 19,
    XML_XINCLUDE_END = 20
  );

(*
  * xmlNotation:
  *
  * A DTD Notation definition.
  *)
  xmlNotation = record
    name: xmlCharPtr; (* Notation name *)
    PublicID: xmlCharPtr; (* Public identifier, if any *)
    SystemID: xmlCharPtr; (* System identifier, if any *)
  end;

(*
  * xmlAttributeType:
  *
  * A DTD Attribute type definition.
  *)
  xmlAttributeType = (
    XML_ATTRIBUTE_CDATA = 1,
    XML_ATTRIBUTE_ID,
    XML_ATTRIBUTE_IDREF  ,
    XML_ATTRIBUTE_IDREFS,
    XML_ATTRIBUTE_ENTITY,
    XML_ATTRIBUTE_ENTITIES,
    XML_ATTRIBUTE_NMTOKEN,
    XML_ATTRIBUTE_NMTOKENS,
    XML_ATTRIBUTE_ENUMERATION,
    XML_ATTRIBUTE_NOTATION
  );

(*
  * xmlAttributeDefault:
  *
  * A DTD Attribute default definition.
  *)
  xmlAttributeDefault = (
    XML_ATTRIBUTE_NONE = 1,
    XML_ATTRIBUTE_REQUIRED,
    XML_ATTRIBUTE_IMPLIED,
    XML_ATTRIBUTE_FIXED
  );

(*
  * xmlEnumeration:
  *
  * List structure used when there is an enumeration in DTDs.
  *)
  xmlEnumeration = record
    next: xmlEnumerationPtr; (* next one *)
    name: xmlCharPtr;
  end;

(*
  * xmlAttribute:
  *
  * An Attribute declaration in a DTD.
  *)
  xmlAttribute = record
    _private: pointer; (* application data *)
    _type: xmlElementType; (* XML_ATTRIBUTE_DECL, must be second ! *)
    name: xmlCharPtr; (* Attribute name *)
    children: xmlNodePtr; (* NULL *)
    last: xmlNodePtr; (* NULL *)
    parent: xmlDtdPtr; (* -> DTD *)
    next: xmlNodePtr; (* next sibling link  *)
    prev: xmlNodePtr; (* previous sibling link  *)
    doc: xmlDocPtr; (* the containing document *)
    nexth: xmlAttributePtr; (* next in hash table *)
    atype: xmlAttributeType; (* The attribute type *)
    def: xmlAttributeDefault; (* the default *)
    defaultValue: xmlCharPtr; (* or the default value *)
    tree: xmlEnumerationPtr; (* or the enumeration tree if any *)
    prefix: xmlCharPtr; (* the namespace prefix if any *)
    elem: xmlCharPtr; (* Element holding the attribute *)
  end;

(*
  * xmlElementContentType:
  *
  * Possible definitions of element content types.
  *)
  xmlElementContentType = (
    XML_ELEMENT_CONTENT_PCDATA = 1,
    XML_ELEMENT_CONTENT_ELEMENT,
    XML_ELEMENT_CONTENT_SEQ,
    XML_ELEMENT_CONTENT_OR
  );

(*
  * xmlElementContentOccur:
  *
  * Possible definitions of element content occurrences.
  *)
  xmlElementContentOccur = (
    XML_ELEMENT_CONTENT_ONCE = 1,
    XML_ELEMENT_CONTENT_OPT,
    XML_ELEMENT_CONTENT_MULT,
    XML_ELEMENT_CONTENT_PLUS
  );

(*
  * xmlElementContent:
  *
  * An XML Element content as stored after parsing an element definition
  * in a DTD.
  *)
  xmlElementContent = record
    _type: xmlElementContentType; (* PCDATA, ELEMENT, SEQ or OR *)
    ocur: xmlElementContentOccur; (* ONCE, OPT, MULT or PLUS *)
    name: xmlCharPtr; (* Element name *)
    c1: xmlElementContentPtr; (* first child *)
    c2: xmlElementContentPtr; (* second child *)
    parent: xmlElementContentPtr; (* parent *)
    prefix: xmlCharPtr; (* Namespace prefix *)
  end;

(*
  * xmlElementTypeVal:
  *
  * The different possibilities for an element content type.
  *)
  xmlElementTypeVal = (
    XML_ELEMENT_TYPE_UNDEFINED = 0,
    XML_ELEMENT_TYPE_EMPTY = 1,
    XML_ELEMENT_TYPE_ANY,
    XML_ELEMENT_TYPE_MIXED,
    XML_ELEMENT_TYPE_ELEMENT
  );

(*
  * xmlElement:
  *
  * An XML Element declaration from a DTD.
  *)
  xmlElement = record
    _private: pointer; (* application data *)
    _type: xmlElementType; (* XML_ELEMENT_DECL, must be second ! *)
    name: xmlCharPtr; (* Element name *)
    children: xmlNodePtr; (* NULL *)
    last: xmlNodePtr; (* NULL *)
    parent: xmlDtdPtr; (* -> DTD *)
    next: xmlNodePtr; (* next sibling link  *)
    prev: xmlNodePtr; (* previous sibling link  *)
    doc: xmlDocPtr; (* the containing document *)
    etype: xmlElementTypeVal; (* The type *)
    content: xmlElementContentPtr; (* the allowed element content *)
    attributes: xmlAttributePtr; (* List of the declared attributes *)
    prefix: xmlCharPtr; (* the namespace prefix if any *)
    contModel: pointer;
  end;

  xmlNsType = xmlElementType;

(*
  * xmlNs:
  *
  * An XML namespace.
  * Note that prefix == NULL is valid, it defines the default namespace
  * within the subtree (until overridden).
  *
  * xmlNsType is unified with xmlElementType.
  *)
  xmlNs = record
    next: xmlNsPtr; (* next Ns link for this node  *)
    _type: xmlNsType; (* global or local *)
    href: xmlCharPtr; (* URL for the namespace *)
    prefix: xmlCharPtr; (* prefix for the namespace *)
    _private: pointer; (* application data *)
    context: xmlDocPtr; (* normally an xmlDoc *)
  end;

(*
  * xmlDtd:
  *
  * An XML DTD, as defined by <!DOCTYPE ... There is actually one for
  * the internal subset and for the external subset.
  *)
  xmlDtd = record
    _private: pointer; (* application data *)
    _type: xmlElementType; (* XML_DTD_NODE, must be second ! *)
    name: xmlCharPtr; (* Name of the DTD *)
    children: xmlNodePtr; (* the value of the property link *)
    last: xmlNodePtr; (* last child link *)
    parent: xmlDocPtr; (* child->parent link *)
    next: xmlNodePtr; (* next sibling link  *)
    prev: xmlNodePtr; (* previous sibling link  *)
    doc: xmlDocPtr; (* the containing document *)
    notations: pointer; (* Hash table for notations if any *)
    elements: pointer; (* Hash table for elements if any *)
    attributes: pointer; (* Hash table for attributes if any *)
    entities: pointer; (* Hash table for entities if any *)
    ExternalID: xmlCharPtr; (* External identifier for PUBLIC DTD *)
    SystemID: xmlCharPtr; (* URI for a SYSTEM or PUBLIC DTD *)
    pentities: pointer; (* Hash table for param entities if any *)
  end;

(*
  * xmlAttr:
  *
  * An attribute on an XML node.
  *)
  xmlAttr = record
    _private: pointer; (* application data *)
    _type: xmlElementType; (* XML_ATTRIBUTE_NODE, must be second ! *)
    name: xmlCharPtr; (* the name of the property *)
    children: xmlNodePtr; (* the value of the property *)
    last: xmlNodePtr; (* NULL *)
    parent: xmlNodePtr; (* child->parent link *)
    next: xmlAttrPtr; (* next sibling link  *)
    prev: xmlAttrPtr; (* previous sibling link  *)
    doc: xmlDocPtr; (* the containing document *)
    ns: xmlNsPtr; (* pointer to the associated namespace *)
    atype: xmlAttributeType; (* the attribute type if validating *)
    psvi: pointer; (* for type/PSVI informations *)
  end;

(*
  * xmlID:
  *
  * An XML ID instance.
  *)
  xmlID = record
    next: xmlIDPtr; (* next ID *)
    value: xmlCharPtr; (* The ID name *)
    attr: xmlAttrPtr; (* The attribute holding it *)
    name: xmlCharPtr; (* The attribute if attr is not available *)
    lineno: cint; (* The line number if attr is not available *)
    doc: xmlDocPtr; (* The document holding the ID *)
  end;

(*
  * xmlRef:
  *
  * An XML IDREF instance.
  *)
  xmlRef = record
    next: xmlRefPtr; (* next Ref *)
    value: xmlCharPtr; (* The Ref name *)
    attr: xmlAttrPtr; (* The attribute holding it *)
    name: xmlCharPtr; (* The attribute if attr is not available *)
    lineno: cint; (* The line number if attr is not available *)
  end;

(*
  * xmlNode:
  *
  * A node in an XML tree.
  *)
  xmlNode = record
    _private: pointer; (* application data *)
    _type: xmlElementType; (* type number, must be second ! *)
    name: xmlCharPtr; (* the name of the node, or the entity *)
    children: xmlNodePtr; (* parent->childs link *)
    last: xmlNodePtr; (* last child link *)
    parent: xmlNodePtr; (* child->parent link *)
    next: xmlNodePtr; (* next sibling link  *)
    prev: xmlNodePtr; (* previous sibling link  *)
    doc: xmlDocPtr; (* the containing document *)
    ns: xmlNsPtr; (* pointer to the associated namespace *)
    content: xmlCharPtr; (* the content *)
    properties: xmlAttrPtr;(* properties list *)
    nsDef: xmlNsPtr; (* namespace definitions on this node *)
    psvi: pointer; (* for type/PSVI informations *)
    line: cushort; (* line number *)
    extra: cushort; (* extra data for XPath/XSLT *)
  end;

  xmlDoc = record
    _private: pointer; (* application data *)
    _type: xmlElementType; (* XML_DOCUMENT_NODE, must be second ! *)
    name: pchar; (* name/filename/URI of the document *)
    children: xmlCharPtr; (* the document tree *)
    last: xmlCharPtr; (* last child link *)
    parent: xmlCharPtr; (* child->parent link *)
    next: xmlCharPtr; (* next sibling link  *)
    prev: xmlCharPtr; (* previous sibling link  *)
    doc: xmlDocPtr; (* autoreference to itself *)
    compression: cint; (* level of zlib compression *)
    standalone: cint; (* standalone document (no external refs)
              1 if standalone="yes"
              0 if standalone="no"
            -1 if there is no XML declaration
            -2 if there is an XML declaration, but no
          standalone attribute was specified *)
    intSubset: xmlDtdPtr; (* the document internal subset *)
    extSubset: xmlDtdPtr; (* the document external subset *)
    oldNs: xmlNsPtr; (* Global namespace, the old way *)
    version: xmlCharPtr; (* the XML version string *)
    encoding: xmlCharPtr; (* external initial encoding, if any *)
    ids: pointer; (* Hash table for ID attributes if any *)
    refs: pointer; (* Hash table for IDREFs attributes if any *)
    URL: xmlCharPtr; (* The URI for that document *)
    charset: cint; (* encoding of the in-memory content actually an xmlCharEncoding *)
    dict: xmlDictPtr; (* dict used to allocate names or NULL *)
    psvi: pointer; (* for type/PSVI informations *)
  end;

(*
  * xmlInputMatchCallback:
  * @filename: the filename or URI
  *
  * Callback used in the I/O Input API to detect if the current handler
  * can provide input fonctionnalities for this resource.
  *
  * Returns 1 if yes and 0 if another Input module should be used
  *)
  xmlInputMatchCallback = function(filename: pchar): cint; cdecl;

(*
  * xmlInputOpenCallback:
  * @filename: the filename or URI
  *
  * Callback used in the I/O Input API to open the resource
  *
  * Returns an Input context or NULL in case or error
  *)
  xmlInputOpenCallback = function(filename: pchar): pointer; cdecl;

(*
  * xmlInputReadCallback:
  * @context:  an Input context
  * @buffer:  the buffer to store data read
  * @len:  the length of the buffer in bytes
  *
  * Callback used in the I/O Input API to read the resource
  *
  * Returns the number of bytes read or -1 in case of error
  *)
  xmlInputReadCallback = function(context: pointer; buffer: pchar; len: cint): cint; cdecl;

(*
  * xmlInputCloseCallback:
  * @context:  an Input context
  *
  * Callback used in the I/O Input API to close the resource
  *
  * Returns 0 or -1 in case of error
  *)
  xmlInputCloseCallback = function(context: pointer): cint; cdecl;

(*
  * Those are the functions and datatypes for the library output
  * I/O structures.
  *)

(*
  * xmlOutputMatchCallback:
  * @filename: the filename or URI
  *
  * Callback used in the I/O Output API to detect if the current handler
  * can provide output fonctionnalities for this resource.
  *
  * Returns 1 if yes and 0 if another Output module should be used
  *)
  xmlOutputMatchCallback = function(filename: pchar): cint; cdecl;

(*
  * xmlOutputOpenCallback:
  * @filename: the filename or URI
  *
  * Callback used in the I/O Output API to open the resource
  *
  * Returns an Output context or NULL in case or error
  *)
  xmlOutputOpenCallback = function(filename: pchar): pointer; cdecl;

(*
  * xmlOutputWriteCallback:
  * @context:  an Output context
  * @buffer:  the buffer of data to write
  * @len:  the length of the buffer in bytes
  *
  * Callback used in the I/O Output API to write to the resource
  *
  * Returns the number of bytes written or -1 in case of error
  *)
  xmlOutputWriteCallback = function(context: pointer; buffer: pchar; len: cint): cint; cdecl;

(*
  * xmlOutputCloseCallback:
  * @context:  an Output context
  *
  * Callback used in the I/O Output API to close the resource
  *
  * Returns 0 or -1 in case of error
  *)
  xmlOutputCloseCallback = function(context: pointer): cint; cdecl;

  xmlParserInputBuffer = record
    context: pointer;
    readcallback: xmlInputReadCallback;
    closecallback: xmlInputCloseCallback;
    encoder: xmlCharEncodingHandlerPtr; (* I18N conversions to UTF-8 *)
    buffer: xmlBufferPtr; (* Local buffer encoded in UTF-8 *)
    raw: xmlBufferPtr; (* if encoder != NULL buffer for raw input *)
    compressed: cint; (* -1=unknown, 0=not compressed, 1=compressed *)
    error: cint;
    rawconsumed: culong; (* amount consumed from raw *)
  end;

  xmlOutputBuffer = record
    context: pointer;
    writecallback: xmlOutputWriteCallback;
    closecallback: xmlOutputCloseCallback;
    encoder: xmlCharEncodingHandlerPtr; (* I18N conversions to UTF-8 *)
    buffer: xmlBufferPtr; (* Local buffer encoded in UTF-8 or ISOLatin *)
    conv: xmlBufferPtr; (* if encoder != NULL buffer for output *)
    written: cint; (* total number of byte written *)
    error: cint;
  end;

(*
  * xmlErrorLevel:
  *
  * Indicates the level of an error
  *)
  xmlErrorLevel = (
    XML_ERR_NONE = 0,
    XML_ERR_WARNING = 1, (* A simple warning *)
    XML_ERR_ERROR = 2, (* A recoverable error *)
    XML_ERR_FATAL = 3    (* A fatal error *)
  );

(*
  * xmlErrorDomain:
  *
  * Indicates where an error may have come from
  *)
  xmlErrorDomain = (
    XML_FROM_NONE = 0,
    XML_FROM_PARSER, (* The XML parser *)
    XML_FROM_TREE, (* The tree module *)
    XML_FROM_NAMESPACE, (* The XML Namespace module *)
    XML_FROM_DTD, (* The XML DTD validation with parser context*)
    XML_FROM_HTML, (* The HTML parser *)
    XML_FROM_MEMORY, (* The memory allocator *)
    XML_FROM_OUTPUT, (* The serialization code *)
    XML_FROM_IO, (* The Input/Output stack *)
    XML_FROM_FTP, (* The FTP module *)
    XML_FROM_HTTP, (* The HTTP module *)
    XML_FROM_XINCLUDE, (* The XInclude processing *)
    XML_FROM_XPATH, (* The XPath module *)
    XML_FROM_XPOINTER, (* The XPointer module *)
    XML_FROM_REGEXP, (* The regular expressions module *)
    XML_FROM_DATATYPE, (* The W3C XML Schemas Datatype module *)
    XML_FROM_SCHEMASP, (* The W3C XML Schemas parser module *)
    XML_FROM_SCHEMASV, (* The W3C XML Schemas validation module *)
    XML_FROM_RELAXNGP, (* The Relax-NG parser module *)
    XML_FROM_RELAXNGV, (* The Relax-NG validator module *)
    XML_FROM_CATALOG, (* The Catalog module *)
    XML_FROM_C14N, (* The Canonicalization module *)
    XML_FROM_XSLT, (* The XSLT engine from libxslt *)
    XML_FROM_VALID, (* The XML DTD validation with valid context *)
    XML_FROM_CHECK, (* The error checking module *)
    XML_FROM_WRITER, (* The xmlwriter module *)
    XML_FROM_MODULE, (* The dynamically loaded module module*)
    XML_FROM_I18N, (* The module handling character conversion *)
    XML_FROM_SCHEMATRONV  (* The Schematron validator module *)
  );

(*
  * xmlError:
  *
  * An XML Error instance.
  *)
  xmlError = record
    domain: cint; (* What part of the library raised this error *)
    code: cint; (* The error code, e.g. an xmlParserError *)
    message: pchar;(* human-readable informative error message *)
    level: xmlErrorLevel;(* how consequent is the error *)
    _file: pchar; (* the filename *)
    line: cint; (* the line number if available *)
    str1: pchar; (* extra string information *)
    str2: pchar; (* extra string information *)
    str3: pchar; (* extra string information *)
    int1: cint; (* extra number information *)
    int2: cint; (* column number of the error or 0 if N/A (todo: rename this field when we would break ABI) *)
    ctxt: pointer; (* the parser context if available *)
    node: pointer; (* the node in the tree *)
  end;

  xmlStructuredErrorFunc = procedure(userData, error: Pointer); cdecl;

(*
  * A node-set (an unordered collection of nodes without duplicates).
  *)
  xmlNodeSet = record
    nodeNr: cint; (* number of nodes in the set *)
    nodeMax: cint; (* size of the array as allocated *)
    nodeTab: xmlNodePtrPtr; (* array of nodes in no particular order *)
    (* @@ with_ns to check wether namespace nodes should be looked at @@ *)
  end;

(*
  * An expression is evaluated to yield an object, which
  * has one of the following four basic types:
  *   - node-set
  *   - boolean
  *   - number
  *   - string
  *
  * @@ XPointer will add more types !
  *)

  xmlXPathObjectType = (
    XPATH_UNDEFINED = 0,
    XPATH_NODESET = 1,
    XPATH_BOOLEAN = 2,
    XPATH_NUMBER = 3,
    XPATH_STRING = 4,
    XPATH_POINT = 5,
    XPATH_RANGE = 6,
    XPATH_LOCATIONSET = 7,
    XPATH_USERS = 8,
    XPATH_XSLT_TREE = 9  (* An XSLT value tree, non modifiable *)
  );

  xmlXPathObject = record
    _type: xmlXPathObjectType;
    nodesetval: xmlNodeSetPtr;
    boolval: cint;
    floatval: cdouble;
    stringval: xmlCharPtr;
    user: pointer;
    index: cint;
    user2: pointer;
    index2: cint;
  end;

(*
  * xmlXPathConvertFunc:
  * @obj:  an XPath object
  * @type:  the number of the target type
  *
  * A conversion function is associated to a type and used to cast
  * the new type to primitive values.
  *
  * Returns -1 in case of error, 0 otherwise
  *)
  xmlXPathConvertFunc = function(obj: xmlXPathObjectPtr; _type: cint): cint; cdecl;

(*
  * Extra type: a name and a conversion function.
  *)
  xmlXPathType = record
    name: xmlCharPtr; (* the type name *)
    func: xmlXPathConvertFunc; (* the conversion function *)
  end;

(*
  * Extra variable: a name and a value.
  *)
  xmlXPathVariable = record
    name: xmlCharPtr; (* the variable name *)
    value: xmlXPathObjectPtr; (* the value *)
  end;

(*
  * xmlXPathEvalFunc:
  * @ctxt: an XPath parser context
  * @nargs: the number of arguments passed to the function
  *
  * An XPath evaluation function, the parameters are on the XPath context stack.
  *)

  xmlXPathEvalFunc = procedure(ctxt: xmlXPathParserContextPtr; nargs: cint); cdecl;

(*
  * Extra function: a name and a evaluation function.
  *)

  xmlXPathFunc = record
    name: xmlCharPtr; (* the function name *)
    func: xmlXPathEvalFunc; (* the evaluation function *)
  end;

(*
  * xmlXPathAxisFunc:
  * @ctxt:  the XPath interpreter context
  * @cur:  the previous node being explored on that axis
  *
  * An axis traversal function. To traverse an axis, the engine calls
  * the first time with cur == NULL and repeat until the function returns
  * NULL indicating the end of the axis traversal.
  *
  * Returns the next node in that axis or NULL if at the end of the axis.
  *)

  xmlXPathAxisFunc = function(ctxt: xmlXPathParserContextPtr; cur: xmlXPathObjectPtr): xmlXPathObjectPtr; cdecl;

(*
  * Extra axis: a name and an axis function.
  *)
  xmlXPathAxis = record
    name: xmlCharPtr; (* the axis name *)
    func: xmlXPathAxisFunc; (* the search function *)
  end;

(*
  * xmlXPathFunction:
  * @ctxt:  the XPath interprestation context
  * @nargs:  the number of arguments
  *
  * An XPath function.
  * The arguments (if any) are popped out from the context stack
  * and the result is pushed on the stack.
  *)
  xmlXPathFunction = procedure(ctxt: xmlXPathParserContextPtr; nargs: cint); cdecl;

(*
  * Function and Variable Lookup.
  *)

(*
  * xmlXPathVariableLookupFunc:
  * @ctxt:  an XPath context
  * @name:  name of the variable
  * @ns_uri:  the namespace name hosting this variable
  *
  * Prototype for callbacks used to plug variable lookup in the XPath
  * engine.
  *
  * Returns the XPath object value or NULL if not found.
  *)
  xmlXPathVariableLookupFunc = function(ctxt: pointer; name, ns_uri: xmlCharPtr): xmlXPathObjectPtr; cdecl;

(*
  * xmlXPathFuncLookupFunc:
  * @ctxt:  an XPath context
  * @name:  name of the function
  * @ns_uri:  the namespace name hosting this function
  *
  * Prototype for callbacks used to plug function lookup in the XPath
  * engine.
  *
  * Returns the XPath function or NULL if not found.
  *)
  xmlXPathFuncLookupFunc = function(ctxt: pointer; name, ns_uri: xmlCharPtr): xmlXPathFunction; cdecl;

(*
  * xmlXPathContext:
  *
  * Expression evaluation occurs with respect to a context.
  * he context consists of:
  *    - a node (the context node)
  *    - a node list (the context node list)
  *    - a set of variable bindings
  *    - a function library
  *    - the set of namespace declarations in scope for the expression
  * Following the switch to hash tables, this need to be trimmed up at
  * the next binary incompatible release.
  * The node may be modified when the context is passed to libxml2
  * for an XPath evaluation so you may need to initialize it again
  * before the next call.
  *)

  xmlXPathContext = record
    doc: xmlDocPtr; (* The current document *)
    node: xmlNodePtr; (* The current node *)

    nb_variables_unused: cint; (* unused (hash table) *)
    max_variables_unused: cint; (* unused (hash table) *)
    varHash: xmlHashTablePtr; (* Hash table of defined variables *)

    nb_types: cint; (* number of defined types *)
    max_types: cint; (* max number of types *)
    types: xmlXPathTypePtr; (* Array of defined types *)

    nb_funcs_unused: cint; (* unused (hash table) *)
    max_funcs_unused: cint; (* unused (hash table) *)
    funcHash: xmlHashTablePtr; (* Hash table of defined funcs *)

    nb_axis: cint; (* number of defined axis *)
    max_axis: cint; (* max number of axis *)
    axis: xmlXPathAxisPtr; (* Array of defined axis *)

    (* the namespace nodes of the context node *)
    namespaces: xmlNsPtrPtr; (* Array of namespaces *)
    nsNr: cint; (* number of namespace in scope *)
    user: pointer; (* function to free *)

    (* extra variables *)
    contextSize: cint; (* the context size *)
    proximityPosition: cint; (* the proximity position *)

    (* extra stuff for XPointer *)
    xptr: cint; (* is this an XPointer context? *)
    here: xmlNodePtr; (* for here() *)
    origin: xmlNodePtr; (* for origin() *)

    (* the set of namespace declarations in scope for the expression *)
    nsHash: xmlHashTablePtr; (* The namespaces hash table *)
    varLookupFunc: xmlXPathVariableLookupFunc;(* variable lookup func *)
    varLookupData: pointer; (* variable lookup data *)

    (* Possibility to link in an extra item *)
    extra: pointer; (* needed for XSLT *)

    (* The function name and URI when calling a function *)
    _function: xmlCharPtr;
    functionURI: xmlCharPtr;

    (* function lookup function and data *)
    funcLookupFunc: xmlXPathFuncLookupFunc;(* function lookup func *)
    funcLookupData: pointer; (* function lookup data *)

    (* temporary namespace lists kept for walking the namespace axis *)
    tmpNsList: xmlNsPtr; (* Array of namespaces *)
    tmpNsNr: cint; (* number of namespaces in scope *)

    (* error reporting mechanism *)
    userData: pointer; (* user specific data block *)
    error: xmlStructuredErrorFunc; (* the callback in case of errors *)
    lastError: xmlError; (* the last error *)
    debugNode: xmlNodePtr; (* the source node XSLT *)

    (* dictionary *)
    dict: xmlDictPtr; (* dictionary if any *)

    flags: cint; (* flags to control compilation *)

    (* Cache for reusal of XPath objects *)
    cache: pointer;
  end;

(*
  * The structure of a compiled expression form is not public.
  *)
  xmlXPathCompExpr = record end;

(*
  * xmlXPathParserContext:
  *
  * An XPath parser context. It contains pure parsing informations,
  * an xmlXPathContext, and the stack of objects.
  *)
  xmlXPathParserContext = record
    cur: xmlCharPtr; (* the current char being parsed *)
    base: xmlCharPtr; (* the full expression *)
    error: cint; (* error code *)
    context: xmlXPathContextPtr; (* the evaluation context *)
    value: xmlXPathObjectPtr; (* the current value *)
    valueNr: cint; (* number of values stacked *)
    valueMax: cint; (* max number of values stacked *)
    valueTab: xmlXPathObjectPtrPtr; (* stack of values *)
    comp: xmlXPathCompExprPtr; (* the precompiled expression *)
    xptr: cint; (* it this an XPointer expression *)
    ancestor: xmlNodePtr; (* used for walking preceding axis*)
  end;

var
  xmlStrdup: function(cur: xmlCharPtr): xmlCharPtr; cdecl;
  xmlStrndup: function(cur: xmlCharPtr; len: cint): xmlCharPtr; cdecl;
  xmlCharStrndup: function(cur: pchar; len: cint): xmlCharPtr; cdecl;
  xmlCharStrdup: function(cur: pchar): xmlCharPtr; cdecl;
  xmlStrsub: function(str: xmlCharPtr; start: cint; len: cint): xmlCharPtr; cdecl;
  xmlStrchr: function(str: xmlCharPtr; val: xmlChar): xmlCharPtr; cdecl;
  xmlStrstr: function(str: xmlCharPtr; val: xmlCharPtr): xmlCharPtr; cdecl;
  xmlStrcasestr: function(str: xmlCharPtr; val: xmlCharPtr): xmlCharPtr; cdecl;
  xmlStrcmp: function(str1: xmlCharPtr; str2: xmlCharPtr): cint; cdecl;
  xmlStrncmp: function(str1: xmlCharPtr; str2: xmlCharPtr; len: cint): cint; cdecl;
  xmlStrcasecmp: function(str1: xmlCharPtr; str2: xmlCharPtr): cint; cdecl;
  xmlStrncasecmp: function(str1: xmlCharPtr; str2: xmlCharPtr; len: cint): cint; cdecl;
  xmlStrEqual: function(str1: xmlCharPtr; str2: xmlCharPtr): cint; cdecl;
  xmlStrQEqual: function(pref: xmlCharPtr; name: xmlCharPtr; str: xmlCharPtr): cint; cdecl;
  xmlStrlen: function(str: xmlCharPtr): cint; cdecl;
  xmlStrcat: function(cur: xmlCharPtr; add: xmlCharPtr): xmlCharPtr; cdecl;
  xmlStrncat: function(cur: xmlCharPtr; add: xmlCharPtr; len: cint): xmlCharPtr; cdecl;
  xmlStrncatNew: function(str1: xmlCharPtr; str2: xmlCharPtr; len: cint): xmlCharPtr; cdecl;
  xmlStrPrintf: function(buf: xmlCharPtr; len: cint; msg: xmlCharPtr; args: array of const): cint; cdecl;
  xmlGetUTF8Char: function(utf: pchar; len: pcint): cint; cdecl;
  xmlCheckUTF8: function(utf: pchar): cint; cdecl;
  xmlUTF8Strsize: function(utf: xmlCharPtr; len: cint): cint; cdecl;
  xmlUTF8Strndup: function(utf: xmlCharPtr; len: cint): xmlCharPtr; cdecl;
  xmlUTF8Strpos: function(utf: xmlCharPtr; pos: cint): xmlCharPtr; cdecl;
  xmlUTF8Strloc: function(utf: xmlCharPtr; utfchar: xmlCharPtr): cint; cdecl;
  xmlUTF8Strsub: function(str: xmlCharPtr; start: cint; len: cint): xmlCharPtr; cdecl;
  xmlUTF8Strlen: function(utf: xmlCharPtr): cint; cdecl;
  xmlUTF8Size: function(utf: xmlCharPtr): cint; cdecl;
  xmlUTF8Charcmp: function(utf1: xmlCharPtr; utf2: xmlCharPtr): cint; cdecl;
  xmlEncodeSpecialChars: function(doc: xmlDocPtr; input: xmlCharPtr): xmlCharPtr; cdecl;
  xmlDetectCharEncoding: function(_in: pchar; len: cint): xmlCharEncoding; cdecl;
  xmlCharEncOutFunc: function(handler: xmlCharEncodingHandlerPtr; _out, _in: xmlBufferPtr): cint; cdecl;
  xmlCharEncInFunc: function(handler: xmlCharEncodingHandlerPtr; _out, _in: xmlBufferPtr): cint; cdecl;
  xmlCharEncFirstLine: function(handler: xmlCharEncodingHandlerPtr; _out, _in: xmlBufferPtr): cint; cdecl;
  xmlCharEncCloseFunc: function(handler: xmlCharEncodingHandlerPtr): cint; cdecl;
  xmlBufferCreate: function: xmlBufferPtr; cdecl;
  xmlBufferCreateSize: function(size: csize_t): xmlBufferPtr; cdecl;
  xmlBufferCreateStatic: function(mem: pointer; size: csize_t): xmlBufferPtr; cdecl;
  xmlBufferResize: function(buf: xmlBufferPtr; size: cuint): cint; cdecl;
  xmlBufferFree: procedure(buf: xmlBufferPtr); cdecl;
  xmlBufferAdd: function(buf: xmlBufferPtr; str: xmlCharPtr; len: cint): cint; cdecl;
  xmlBufferAddHead: function(buf: xmlBufferPtr; str: xmlCharPtr; len: cint): cint; cdecl;
  xmlBufferCat: function(buf: xmlBufferPtr; str: xmlCharPtr): cint; cdecl;
  xmlBufferCCat: function(buf: xmlBufferPtr; str: pchar): cint; cdecl;
  xmlBufferShrink: function(buf: xmlBufferPtr; len: cuint): cint; cdecl;
  xmlBufferGrow: function(buf: xmlBufferPtr; len: cuint): cint; cdecl;
  xmlBufferEmpty: procedure(buf: xmlBufferPtr); cdecl;
  xmlBufferContent: function(buf: xmlBufferPtr): xmlCharPtr; cdecl;
  xmlBufferSetAllocationScheme: procedure(buf: xmlBufferPtr; scheme: xmlBufferAllocationScheme); cdecl;
  xmlBufferLength: function(buf: xmlBufferPtr): cint; cdecl;
  xmlCreateIntSubset: function(doc: xmlDocPtr; name, ExternalID, SystemID: xmlCharPtr): xmlDtdPtr; cdecl;
  xmlNewDtd: function(doc: xmlDocPtr; name, ExternalID, SystemID: xmlCharPtr): xmlDtdPtr; cdecl;
  xmlGetIntSubset: function(doc: xmlDocPtr): xmlDtdPtr; cdecl;
  xmlFreeDtd: procedure(cur: xmlDtdPtr); cdecl;
  xmlNewNs: function(node: xmlNodePtr; href, prefix: xmlCharPtr): xmlNsPtr; cdecl;
  xmlFreeNs: procedure(cur: xmlNsPtr); cdecl;
  xmlFreeNsList: procedure(cur: xmlNsPtr); cdecl;
  xmlSaveFile: function(filename: xmlCharPtr; cur: xmlDocPtr): cint; cdecl;
  xmlParseFile: function(filename: xmlCharPtr): xmlDocPtr; cdecl;
  xmlParseDoc: function(cur: xmlCharPtr): xmlDocPtr; cdecl;
  xmlNewDoc: function(version: xmlCharPtr): xmlDocPtr; cdecl;
  xmlFreeDoc: procedure(cur: xmlDocPtr); cdecl;
  xmlNewDocProp: function(doc: xmlDocPtr; name, value: xmlCharPtr): xmlAttrPtr; cdecl;
  xmlNewProp: function(node: xmlNodePtr; name, value: xmlCharPtr): xmlAttrPtr; cdecl;
  xmlNewNsProp: function(node: xmlNodePtr; ns: xmlNsPtr; name, value: xmlCharPtr): xmlAttrPtr; cdecl;
  xmlNewNsPropEatName: function(node: xmlNodePtr; ns: xmlNsPtr; name, value: xmlCharPtr): xmlAttrPtr; cdecl;
  xmlFreePropList: procedure(cur: xmlAttrPtr); cdecl;
  xmlFreeProp: procedure(cur: xmlAttrPtr); cdecl;
  xmlCopyProp: function(target: xmlNodePtr; cur: xmlAttrPtr): xmlAttrPtr; cdecl;
  xmlCopyPropList: function(target: xmlNodePtr; cur: xmlAttrPtr): xmlAttrPtr; cdecl;
  xmlCopyDtd: function(dtd: xmlDtdPtr): xmlDtdPtr; cdecl;
  xmlCopyDoc: function(doc: xmlDocPtr; recursive: cint): xmlDocPtr; cdecl;
  xmlNewDocNode: function(doc: xmlDocPtr; ns: xmlNsPtr; name, content: xmlCharPtr): xmlNodePtr; cdecl;
  xmlNewDocNodeEatName: function(doc: xmlDocPtr; ns: xmlNsPtr; name, content: xmlCharPtr): xmlNodePtr; cdecl;
  xmlNewNode: function(ns: xmlNsPtr; name: xmlCharPtr): xmlNodePtr; cdecl;
  xmlNewNodeEatName: function(ns: xmlNsPtr; name: xmlCharPtr): xmlNodePtr; cdecl;
  xmlNewChild: function(parent: xmlNodePtr; ns: xmlNsPtr; name, content: xmlCharPtr): xmlNodePtr; cdecl;
  xmlNewDocText: function(doc: xmlDocPtr; content: xmlCharPtr): xmlNodePtr; cdecl;
  xmlNewText: function(content: xmlCharPtr): xmlNodePtr; cdecl;
  xmlNewDocPI: function(doc: xmlDocPtr; name, content: xmlCharPtr): xmlNodePtr; cdecl;
  xmlNewPI: function(name, content: xmlCharPtr): xmlNodePtr; cdecl;
  xmlNewDocTextLen: function(doc: xmlDocPtr; content: xmlCharPtr; len: cint): xmlNodePtr; cdecl;
  xmlNewTextLen: function(content: xmlCharPtr; len: cint): xmlNodePtr; cdecl;
  xmlNewDocComment: function(doc: xmlDocPtr; content: xmlCharPtr): xmlNodePtr; cdecl;
  xmlNewComment: function(content: xmlCharPtr): xmlNodePtr; cdecl;
  xmlNewCDataBlock: function(doc: xmlDocPtr; content: xmlCharPtr; len: cint): xmlNodePtr; cdecl;
  xmlNewCharRef: function(doc: xmlDocPtr; name: xmlCharPtr): xmlNodePtr; cdecl;
  xmlNewReference: function(doc: xmlDocPtr; name: xmlCharPtr): xmlNodePtr; cdecl;
  xmlCopyNode: function(node: xmlNodePtr; recursive: cint): xmlNodePtr; cdecl;
  xmlDocCopyNode: function(node: xmlNodePtr; doc: xmlDocPtr; recursive: cint): xmlNodePtr; cdecl;
  xmlDocCopyNodeList: function(doc: xmlDocPtr; node: xmlNodePtr): xmlNodePtr; cdecl;
  xmlCopyNodeList: function(node: xmlNodePtr): xmlNodePtr; cdecl;
  xmlNewTextChild: function(parent: xmlNodePtr; ns: xmlNsPtr; name, content: xmlCharPtr): xmlNodePtr; cdecl;
  xmlNewDocRawNode: function(doc: xmlDocPtr; ns: xmlNsPtr; name, content: xmlCharPtr): xmlNodePtr; cdecl;
  xmlNewDocFragment: function(doc: xmlDocPtr): xmlNodePtr; cdecl;
  xmlGetLineNo: function(node: xmlNodePtr): clong; cdecl;
  xmlGetNodePath: function(node: xmlNodePtr): xmlCharPtr; cdecl;
  xmlDocGetRootElement: function(doc: xmlDocPtr): xmlNodePtr; cdecl;
  xmlGetLastChild: function(parent: xmlNodePtr): xmlNodePtr; cdecl;
  xmlNodeIsText: function(node: xmlNodePtr): cint; cdecl;
  xmlIsBlankNode: function(node: xmlNodePtr): cint; cdecl;
  xmlDocSetRootElement: function(doc: xmlDocPtr; root: xmlNodePtr): xmlNodePtr; cdecl;
  xmlNodeSetName: procedure(cur: xmlNodePtr; name: xmlCharPtr); cdecl;
  xmlNodeSetContent: procedure(cur: xmlNodePtr; content: xmlCharPtr); cdecl;
  xmlAddChild: function(parent, cur: xmlNodePtr): xmlNodePtr; cdecl;
  xmlAddChildList: function(parent, cur: xmlNodePtr): xmlNodePtr; cdecl;
  xmlReplaceNode: function(old, cur: xmlNodePtr): xmlNodePtr; cdecl;
  xmlAddPrevSibling: function(cur, elem: xmlNodePtr): xmlNodePtr; cdecl;
  xmlAddSibling: function(cur, elem: xmlNodePtr): xmlNodePtr; cdecl;
  xmlAddNextSibling: function(cur, elem: xmlNodePtr): xmlNodePtr; cdecl;
  xmlUnlinkNode: procedure(cur: xmlNodePtr); cdecl;
  xmlTextMerge: function(first, second: xmlNodePtr): xmlNodePtr; cdecl;
  xmlTextConcat: function(node: xmlNodePtr; name: xmlCharPtr; len: cint): cint; cdecl;
  xmlFreeNodeList: procedure(cur: xmlNodePtr); cdecl;
  xmlFreeNode: procedure(cur: xmlNodePtr); cdecl;
  xmlSetTreeDoc: procedure(tree: xmlNodePtr; doc: xmlDocPtr); cdecl;
  xmlSetListDoc: procedure(list: xmlNodePtr; doc: xmlDocPtr); cdecl;
  xmlSearchNs: function(doc: xmlDocPtr; node: xmlNodePtr; nameSpace: xmlCharPtr): xmlNsPtr; cdecl;
  xmlSearchNsByHref: function(doc: xmlDocPtr; node: xmlNodePtr; href: xmlCharPtr): xmlNsPtr; cdecl;
  xmlGetNsList: function(doc: xmlDocPtr; node: xmlNodePtr): xmlNsPtr; cdecl;
  xmlSetNs: procedure(node: xmlNodePtr; ns: xmlNsPtr); cdecl;
  xmlCopyNamespace: function(cur: xmlNsPtr): xmlNsPtr; cdecl;
  xmlCopyNamespaceList: function(cur: xmlNsPtr): xmlNsPtr; cdecl;
  xmlRemoveProp: function(cur: xmlAttrPtr): cint; cdecl;
  xmlSetProp: function(node: xmlNodePtr; name, value: xmlCharPtr): xmlAttrPtr; cdecl;
  xmlSetNsProp: function(node: xmlNodePtr; ns: xmlNsPtr; name, value: xmlCharPtr): xmlAttrPtr; cdecl;
  xmlGetNoNsProp: function(node: xmlNodePtr; name: xmlCharPtr): xmlCharPtr; cdecl;
  xmlGetProp: function(node: xmlNodePtr; name: xmlCharPtr): xmlCharPtr; cdecl;
  xmlHasProp: function(node: xmlNodePtr; name: xmlCharPtr): xmlAttrPtr; cdecl;
  xmlHasNsProp: function(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlAttrPtr; cdecl;
  xmlGetNsProp: function(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlCharPtr; cdecl;
  xmlStringGetNodeList: function(doc: xmlDocPtr; value: xmlCharPtr): xmlNodePtr; cdecl;
  xmlStringLenGetNodeList: function(doc: xmlDocPtr; value: xmlCharPtr; len: cint): xmlNodePtr; cdecl;
  xmlNodeListGetString: function(doc: xmlDocPtr; list: xmlNodePtr; _inLine: cint): xmlCharPtr; cdecl;
  xmlNodeAddContent: procedure(cur: xmlNodePtr; content: xmlCharPtr); cdecl;
  xmlNodeAddContentLen: procedure(cur: xmlNodePtr; content: xmlCharPtr; len: cint); cdecl;
  xmlNodeGetContent: function(cur: xmlNodePtr): xmlCharPtr; cdecl;
  xmlNodeBufGetContent: function(buffer: xmlBufferPtr; cur: xmlNodePtr): cint; cdecl;
  xmlNodeGetLang: function(cur: xmlNodePtr): xmlCharPtr; cdecl;
  xmlNodeGetSpacePreserve: function(cur: xmlNodePtr): cint; cdecl;
  xmlNodeDump: function(buf: xmlBufferPtr; doc: xmlDocPtr; cur: xmlNodePtr; level: cint; format: cint): cint; cdecl;
  xmlInitCharEncodingHandlers: procedure; cdecl;
  xmlCleanupCharEncodingHandlers: procedure; cdecl;
  xmlRegisterCharEncodingHandler: procedure(handler: xmlCharEncodingHandlerPtr); cdecl;
  xmlGetCharEncodingHandler: function(enc: xmlCharEncoding): xmlCharEncodingHandlerPtr; cdecl;
  xmlFindCharEncodingHandler: function(name: pchar): xmlCharEncodingHandlerPtr; cdecl;
  xmlNewCharEncodingHandler: function(name: pchar; input: xmlCharEncodingInputFunc; output: xmlCharEncodingOutputFunc): xmlCharEncodingHandlerPtr; cdecl;
  xmlAddEncodingAlias: function(name: pchar; alias: pchar): cint; cdecl;
  xmlDelEncodingAlias: function(alias: pchar): cint; cdecl;
  xmlGetEncodingAlias: function(alias: pchar): pchar; cdecl;
  xmlCleanupEncodingAliases: procedure; cdecl;
  xmlParseCharEncoding: function(name: pchar): xmlCharEncoding; cdecl;
  xmlGetCharEncodingName: function(enc: xmlCharEncoding): pchar; cdecl;
  xmlCleanupInputCallbacks: procedure; cdecl;
  xmlPopInputCallbacks: function: cint; cdecl;
  xmlRegisterDefaultInputCallbacks: procedure; cdecl;
  xmlAllocParserInputBuffer: function(enc: xmlCharEncoding): xmlParserInputBufferPtr; cdecl;
  xmlParserInputBufferCreateFilename: function(URI: pchar; enc: xmlCharEncoding): xmlParserInputBufferPtr; cdecl;
  xmlParserInputBufferCreateFd: function(fd: cint; enc: xmlCharEncoding): xmlParserInputBufferPtr; cdecl;
  xmlParserInputBufferCreateMem: function(mem: pchar; size: cint; enc: xmlCharEncoding): xmlParserInputBufferPtr; cdecl;
  xmlParserInputBufferCreateStatic: function(mem: pchar; size: cint; enc: xmlCharEncoding): xmlParserInputBufferPtr; cdecl;
  xmlParserInputBufferCreateIO: function(ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; enc: xmlCharEncoding): xmlParserInputBufferPtr; cdecl;
  xmlParserInputBufferRead: function(_in: xmlParserInputBufferPtr; len: cint): cint; cdecl;
  xmlParserInputBufferGrow: function(_in: xmlParserInputBufferPtr; len: cint): cint; cdecl;
  xmlParserInputBufferPush: function(_in: xmlParserInputBufferPtr; len: cint; buf: pchar): cint; cdecl;
  xmlFreeParserInputBuffer: procedure(_in: xmlParserInputBufferPtr); cdecl;
  xmlParserGetDirectory: function(filename: pchar): pchar; cdecl;
  xmlCleanupOutputCallbacks: procedure; cdecl;
  xmlRegisterDefaultOutputCallbacks: procedure; cdecl;
  xmlAllocOutputBuffer: function(encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; cdecl;
  xmlOutputBufferCreateFilename: function(URI: pchar; encoder: xmlCharEncodingHandlerPtr; compression: cint): xmlOutputBufferPtr; cdecl;
  xmlOutputBufferCreateBuffer: function(buffer: xmlBufferPtr; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; cdecl;
  xmlOutputBufferCreateFd: function(fd: cint; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; cdecl;
  xmlOutputBufferCreateIO: function(iowrite: xmlOutputWriteCallback; ioclose: xmlOutputCloseCallback; ioctx: pointer; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; cdecl;
  xmlOutputBufferWrite: function(_out: xmlOutputBufferPtr; len: cint; buf: pchar): cint; cdecl;
  xmlOutputBufferWriteString: function(_out: xmlOutputBufferPtr; str: pchar): cint; cdecl;
  xmlOutputBufferWriteEscape: function(_out: xmlOutputBufferPtr; str: xmlCharPtr; escaping: xmlCharEncodingOutputFunc): cint; cdecl;
  xmlOutputBufferFlush: function(_out: xmlOutputBufferPtr): cint; cdecl;
  xmlOutputBufferClose: function(_out: xmlOutputBufferPtr): cint; cdecl;
  xmlXPathFreeObject: procedure(obj: xmlXPathObjectPtr); cdecl;
  xmlXPathNodeSetCreate: function(val: xmlNodePtr): xmlNodeSetPtr; cdecl;
  xmlXPathFreeNodeSetList: procedure(obj: xmlXPathObjectPtr); cdecl;
  xmlXPathFreeNodeSet: procedure(obj: xmlNodeSetPtr); cdecl;
  xmlXPathObjectCopy: function(val: xmlXPathObjectPtr): xmlXPathObjectPtr; cdecl;
  xmlXPathCmpNodes: function(node1, node2: xmlNodePtr): cint; cdecl;
  xmlXPathCastNumberToBoolean: function(val: cdouble): cint; cdecl;
  xmlXPathCastStringToBoolean: function(val: xmlCharPtr): cint; cdecl;
  xmlXPathCastNodeSetToBoolean: function(ns: xmlNodeSetPtr): cint; cdecl;
  xmlXPathCastToBoolean: function(ns: xmlXPathObjectPtr): cint; cdecl;
  xmlXPathCastBooleanToNumber: function(val: cint): cdouble; cdecl;
  xmlXPathCastStringToNumber: function(val: xmlCharPtr): cdouble; cdecl;
  xmlXPathCastNodeToNumber: function(val: xmlNodePtr): cdouble; cdecl;
  xmlXPathCastNodeSetToNumber: function(val: xmlNodeSetPtr): cdouble; cdecl;
  xmlXPathCastToNumber: function(val: xmlXPathObjectPtr): cdouble; cdecl;
  xmlXPathCastBooleanToString: function(val: cint): xmlCharPtr; cdecl;
  xmlXPathCastNumberToString: function(val: cdouble): xmlCharPtr; cdecl;
  xmlXPathCastNodeToString: function(val: xmlNodePtr): xmlCharPtr; cdecl;
  xmlXPathCastNodeSetToString: function(val: xmlNodeSetPtr): xmlCharPtr; cdecl;
  xmlXPathCastToString: function(val: xmlXPathObjectPtr): xmlCharPtr; cdecl;
  xmlXPathConvertBoolean: function(val: xmlXPathObjectPtr): xmlXPathObjectPtr; cdecl;
  xmlXPathConvertNumber: function(val: xmlXPathObjectPtr): xmlXPathObjectPtr; cdecl;
  xmlXPathConvertString: function(val: xmlXPathObjectPtr): xmlXPathObjectPtr; cdecl;
  xmlXPathNewContext: function(doc: xmlDocPtr): xmlXPathContextPtr; cdecl;
  xmlXPathFreeContext: procedure(ctxt: xmlXPathContextPtr); cdecl;
  xmlXPathContextSetCache: function(ctxt: xmlXPathContextPtr; active, value, options: cint): cint; cdecl;
  xmlXPathOrderDocElems: function(doc: xmlDocPtr): clong; cdecl;
  xmlXPathEval: function(str: xmlCharPtr; ctx: xmlXPathContextPtr): xmlXPathObjectPtr; cdecl;
  xmlXPathEvalExpression: function(str: xmlCharPtr; ctx: xmlXPathContextPtr): xmlXPathObjectPtr; cdecl;
  xmlXPathEvalPredicate: function(ctxt: xmlXPathContextPtr; res: xmlXPathObjectPtr): cint; cdecl;
  xmlXPathCompile: function(str: xmlCharPtr): xmlXPathCompExprPtr; cdecl;
  xmlXPathCtxtCompile: function(ctxt: xmlXPathContextPtr; str: xmlCharPtr): xmlXPathCompExprPtr; cdecl;
  xmlXPathCompiledEval: function(comp: xmlXPathCompExprPtr; ctxt: xmlXPathContextPtr): xmlXPathObjectPtr; cdecl;
  xmlXPathCompiledEvalToBoolean: function(comp: xmlXPathCompExprPtr; ctxt: xmlXPathContextPtr): cint; cdecl;
  xmlXPathFreeCompExpr: procedure(comp: xmlXPathCompExprPtr); cdecl;
  xmlXPathInit: procedure; cdecl;
  xmlXPathIsNaN: function(val: cdouble): cint; cdecl;
  xmlXPathIsInf: function(val: cdouble): cint; cdecl;
  xmlXPathRegisterNs: function(ctxt: pointer; prefix, ns_uri: xmlCharPtr): cint; cdecl;
  xmlMemFree: procedure(ptr: pointer); cdecl;
  xmlFree: procedure(ptr: pointer); cdecl;

function Xml2Init(ThrowExceptions: Boolean = False): Boolean;
{$endif}

implementation

{$ifdef unix}
const
  libxml2 = 'libxml2.' + SharedSuffix;
  libxml22 = libxml2 + '.2';

var
  Loaded: Boolean;
  Initialized: Boolean;
  FailedModuleName: string;
  FailedProcName: string;

function Xml2Init(ThrowExceptions: Boolean = False): Boolean;
var
  Module: HModule;

  procedure CheckExceptions;
  begin
    if (not Initialized) and (ThrowExceptions) then
      LibraryExceptProc(FailedModuleName, FailedProcName);
  end;

  function TryLoad(const ProcName: string; var Proc: Pointer): Boolean;
  begin
    FailedProcName := ProcName;
    Proc := LibraryGetProc(Module, ProcName);
    Result := Proc <> nil;
    if not Result then
      CheckExceptions;
  end;

begin
  ThrowExceptions := ThrowExceptions and (@LibraryGetProc <> nil);
  if Loaded then
  begin
    CheckExceptions;
    Exit(Initialized);
  end;
  Loaded:= True;
  if Initialized then
    Exit(True);
  Result := False;
  FailedModuleName := libxml2;
  FailedProcName := '';
  Module := LibraryLoad(libxml2, libxml22);
  if Module = ModuleNil then
  begin
    CheckExceptions;
    Exit;
  end;
  Result :=
    TryLoad('xmlStrndup', @xmlStrndup) and
    TryLoad('xmlCharStrndup', @xmlCharStrndup) and
    TryLoad('xmlCharStrdup', @xmlCharStrdup) and
    TryLoad('xmlStrsub', @xmlStrsub) and
    TryLoad('xmlStrchr', @xmlStrchr) and
    TryLoad('xmlStrstr', @xmlStrstr) and
    TryLoad('xmlStrcasestr', @xmlStrcasestr) and
    TryLoad('xmlStrcmp', @xmlStrcmp) and
    TryLoad('xmlStrncmp', @xmlStrncmp) and
    TryLoad('xmlStrcasecmp', @xmlStrcasecmp) and
    TryLoad('xmlStrncasecmp', @xmlStrncasecmp) and
    TryLoad('xmlStrEqual', @xmlStrEqual) and
    TryLoad('xmlStrQEqual', @xmlStrQEqual) and
    TryLoad('xmlStrlen', @xmlStrlen) and
    TryLoad('xmlStrcat', @xmlStrcat) and
    TryLoad('xmlStrncat', @xmlStrncat) and
    TryLoad('xmlStrncatNew', @xmlStrncatNew) and
    TryLoad('xmlStrPrintf', @xmlStrPrintf) and
    TryLoad('xmlGetUTF8Char', @xmlGetUTF8Char) and
    TryLoad('xmlCheckUTF8', @xmlCheckUTF8) and
    TryLoad('xmlUTF8Strsize', @xmlUTF8Strsize) and
    TryLoad('xmlUTF8Strndup', @xmlUTF8Strndup) and
    TryLoad('xmlUTF8Strpos', @xmlUTF8Strpos) and
    TryLoad('xmlUTF8Strloc', @xmlUTF8Strloc) and
    TryLoad('xmlUTF8Strsub', @xmlUTF8Strsub) and
    TryLoad('xmlUTF8Strlen', @xmlUTF8Strlen) and
    TryLoad('xmlUTF8Size', @xmlUTF8Size) and
    TryLoad('xmlUTF8Charcmp', @xmlUTF8Charcmp) and
    TryLoad('xmlEncodeSpecialChars', @xmlEncodeSpecialChars) and
    TryLoad('xmlDetectCharEncoding', @xmlDetectCharEncoding) and
    TryLoad('xmlCharEncOutFunc', @xmlCharEncOutFunc) and
    TryLoad('xmlCharEncInFunc', @xmlCharEncInFunc) and
    TryLoad('xmlCharEncFirstLine', @xmlCharEncFirstLine) and
    TryLoad('xmlCharEncCloseFunc', @xmlCharEncCloseFunc) and
    TryLoad('xmlBufferCreate', @xmlBufferCreate) and
    TryLoad('xmlBufferCreateSize', @xmlBufferCreateSize) and
    TryLoad('xmlBufferCreateStatic', @xmlBufferCreateStatic) and
    TryLoad('xmlBufferResize', @xmlBufferResize) and
    TryLoad('xmlBufferFree', @xmlBufferFree) and
    TryLoad('xmlBufferAdd', @xmlBufferAdd) and
    TryLoad('xmlBufferAddHead', @xmlBufferAddHead) and
    TryLoad('xmlBufferCat', @xmlBufferCat) and
    TryLoad('xmlBufferCCat', @xmlBufferCCat) and
    TryLoad('xmlBufferShrink', @xmlBufferShrink) and
    TryLoad('xmlBufferGrow', @xmlBufferGrow) and
    TryLoad('xmlBufferEmpty', @xmlBufferEmpty) and
    TryLoad('xmlBufferContent', @xmlBufferContent) and
    TryLoad('xmlBufferSetAllocationScheme', @xmlBufferSetAllocationScheme) and
    TryLoad('xmlBufferLength', @xmlBufferLength) and
    TryLoad('xmlCreateIntSubset', @xmlCreateIntSubset) and
    TryLoad('xmlNewDtd', @xmlNewDtd) and
    TryLoad('xmlGetIntSubset', @xmlGetIntSubset) and
    TryLoad('xmlFreeDtd', @xmlFreeDtd) and
    TryLoad('xmlNewNs', @xmlNewNs) and
    TryLoad('xmlFreeNs', @xmlFreeNs) and
    TryLoad('xmlFreeNsList', @xmlFreeNsList) and
    TryLoad('xmlSaveFile', @xmlSaveFile) and
    TryLoad('xmlParseFile', @xmlParseFile) and
    TryLoad('xmlParseDoc', @xmlParseDoc) and
    TryLoad('xmlNewDoc', @xmlNewDoc) and
    TryLoad('xmlFreeDoc', @xmlFreeDoc) and
    TryLoad('xmlNewDocProp', @xmlNewDocProp) and
    TryLoad('xmlNewProp', @xmlNewProp) and
    TryLoad('xmlNewNsProp', @xmlNewNsProp) and
    TryLoad('xmlNewNsPropEatName', @xmlNewNsPropEatName) and
    TryLoad('xmlFreePropList', @xmlFreePropList) and
    TryLoad('xmlFreeProp', @xmlFreeProp) and
    TryLoad('xmlCopyProp', @xmlCopyProp) and
    TryLoad('xmlCopyPropList', @xmlCopyPropList) and
    TryLoad('xmlCopyDtd', @xmlCopyDtd) and
    TryLoad('xmlCopyDoc', @xmlCopyDoc) and
    TryLoad('xmlNewDocNode', @xmlNewDocNode) and
    TryLoad('xmlNewDocNodeEatName', @xmlNewDocNodeEatName) and
    TryLoad('xmlNewNode', @xmlNewNode) and
    TryLoad('xmlNewNodeEatName', @xmlNewNodeEatName) and
    TryLoad('xmlNewChild', @xmlNewChild) and
    TryLoad('xmlNewDocText', @xmlNewDocText) and
    TryLoad('xmlNewText', @xmlNewText) and
    TryLoad('xmlNewDocPI', @xmlNewDocPI) and
    TryLoad('xmlNewPI', @xmlNewPI) and
    TryLoad('xmlNewDocTextLen', @xmlNewDocTextLen) and
    TryLoad('xmlNewTextLen', @xmlNewTextLen) and
    TryLoad('xmlNewDocComment', @xmlNewDocComment) and
    TryLoad('xmlNewComment', @xmlNewComment) and
    TryLoad('xmlNewCDataBlock', @xmlNewCDataBlock) and
    TryLoad('xmlNewCharRef', @xmlNewCharRef) and
    TryLoad('xmlNewReference', @xmlNewReference) and
    TryLoad('xmlCopyNode', @xmlCopyNode) and
    TryLoad('xmlDocCopyNode', @xmlDocCopyNode) and
    TryLoad('xmlDocCopyNodeList', @xmlDocCopyNodeList) and
    TryLoad('xmlCopyNodeList', @xmlCopyNodeList) and
    TryLoad('xmlNewTextChild', @xmlNewTextChild) and
    TryLoad('xmlNewDocRawNode', @xmlNewDocRawNode) and
    TryLoad('xmlNewDocFragment', @xmlNewDocFragment) and
    TryLoad('xmlGetLineNo', @xmlGetLineNo) and
    TryLoad('xmlGetNodePath', @xmlGetNodePath) and
    TryLoad('xmlDocGetRootElement', @xmlDocGetRootElement) and
    TryLoad('xmlGetLastChild', @xmlGetLastChild) and
    TryLoad('xmlNodeIsText', @xmlNodeIsText) and
    TryLoad('xmlIsBlankNode', @xmlIsBlankNode) and
    TryLoad('xmlDocSetRootElement', @xmlDocSetRootElement) and
    TryLoad('xmlNodeSetName', @xmlNodeSetName) and
    TryLoad('xmlNodeSetContent', @xmlNodeSetContent) and
    TryLoad('xmlAddChild', @xmlAddChild) and
    TryLoad('xmlAddChildList', @xmlAddChildList) and
    TryLoad('xmlReplaceNode', @xmlReplaceNode) and
    TryLoad('xmlAddPrevSibling', @xmlAddPrevSibling) and
    TryLoad('xmlAddSibling', @xmlAddSibling) and
    TryLoad('xmlAddNextSibling', @xmlAddNextSibling) and
    TryLoad('xmlUnlinkNode', @xmlUnlinkNode) and
    TryLoad('xmlTextMerge', @xmlTextMerge) and
    TryLoad('xmlTextConcat', @xmlTextConcat) and
    TryLoad('xmlFreeNodeList', @xmlFreeNodeList) and
    TryLoad('xmlFreeNode', @xmlFreeNode) and
    TryLoad('xmlSetTreeDoc', @xmlSetTreeDoc) and
    TryLoad('xmlSetListDoc', @xmlSetListDoc) and
    TryLoad('xmlSearchNs', @xmlSearchNs) and
    TryLoad('xmlSearchNsByHref', @xmlSearchNsByHref) and
    TryLoad('xmlGetNsList', @xmlGetNsList) and
    TryLoad('xmlSetNs', @xmlSetNs) and
    TryLoad('xmlCopyNamespace', @xmlCopyNamespace) and
    TryLoad('xmlCopyNamespaceList', @xmlCopyNamespaceList) and
    TryLoad('xmlRemoveProp', @xmlRemoveProp) and
    TryLoad('xmlSetProp', @xmlSetProp) and
    TryLoad('xmlSetNsProp', @xmlSetNsProp) and
    TryLoad('xmlGetNoNsProp', @xmlGetNoNsProp) and
    TryLoad('xmlGetProp', @xmlGetProp) and
    TryLoad('xmlHasProp', @xmlHasProp) and
    TryLoad('xmlHasNsProp', @xmlHasNsProp) and
    TryLoad('xmlGetNsProp', @xmlGetNsProp) and
    TryLoad('xmlStringGetNodeList', @xmlStringGetNodeList) and
    TryLoad('xmlStringLenGetNodeList', @xmlStringLenGetNodeList) and
    TryLoad('xmlNodeListGetString', @xmlNodeListGetString) and
    TryLoad('xmlNodeAddContent', @xmlNodeAddContent) and
    TryLoad('xmlNodeAddContentLen', @xmlNodeAddContentLen) and
    TryLoad('xmlNodeGetContent', @xmlNodeGetContent) and
    TryLoad('xmlNodeBufGetContent', @xmlNodeBufGetContent) and
    TryLoad('xmlNodeGetLang', @xmlNodeGetLang) and
    TryLoad('xmlNodeGetSpacePreserve', @xmlNodeGetSpacePreserve) and
    TryLoad('xmlNodeDump', @xmlNodeDump) and
    TryLoad('xmlInitCharEncodingHandlers', @xmlInitCharEncodingHandlers) and
    TryLoad('xmlCleanupCharEncodingHandlers', @xmlCleanupCharEncodingHandlers) and
    TryLoad('xmlRegisterCharEncodingHandler', @xmlRegisterCharEncodingHandler) and
    TryLoad('xmlGetCharEncodingHandler', @xmlGetCharEncodingHandler) and
    TryLoad('xmlFindCharEncodingHandler', @xmlFindCharEncodingHandler) and
    TryLoad('xmlNewCharEncodingHandler', @xmlNewCharEncodingHandler) and
    TryLoad('xmlAddEncodingAlias', @xmlAddEncodingAlias) and
    TryLoad('xmlDelEncodingAlias', @xmlDelEncodingAlias) and
    TryLoad('xmlGetEncodingAlias', @xmlGetEncodingAlias) and
    TryLoad('xmlCleanupEncodingAliases', @xmlCleanupEncodingAliases) and
    TryLoad('xmlParseCharEncoding', @xmlParseCharEncoding) and
    TryLoad('xmlGetCharEncodingName', @xmlGetCharEncodingName) and
    TryLoad('xmlCleanupInputCallbacks', @xmlCleanupInputCallbacks) and
    TryLoad('xmlPopInputCallbacks', @xmlPopInputCallbacks) and
    TryLoad('xmlRegisterDefaultInputCallbacks', @xmlRegisterDefaultInputCallbacks) and
    TryLoad('xmlAllocParserInputBuffer', @xmlAllocParserInputBuffer) and
    TryLoad('xmlParserInputBufferCreateFilename', @xmlParserInputBufferCreateFilename) and
    TryLoad('xmlParserInputBufferCreateFd', @xmlParserInputBufferCreateFd) and
    TryLoad('xmlParserInputBufferCreateMem', @xmlParserInputBufferCreateMem) and
    TryLoad('xmlParserInputBufferCreateStatic', @xmlParserInputBufferCreateStatic) and
    TryLoad('xmlParserInputBufferCreateIO', @xmlParserInputBufferCreateIO) and
    TryLoad('xmlParserInputBufferRead', @xmlParserInputBufferRead) and
    TryLoad('xmlParserInputBufferGrow', @xmlParserInputBufferGrow) and
    TryLoad('xmlParserInputBufferPush', @xmlParserInputBufferPush) and
    TryLoad('xmlFreeParserInputBuffer', @xmlFreeParserInputBuffer) and
    TryLoad('xmlParserGetDirectory', @xmlParserGetDirectory) and
    TryLoad('xmlCleanupOutputCallbacks', @xmlCleanupOutputCallbacks) and
    TryLoad('xmlRegisterDefaultOutputCallbacks', @xmlRegisterDefaultOutputCallbacks) and
    TryLoad('xmlAllocOutputBuffer', @xmlAllocOutputBuffer) and
    TryLoad('xmlOutputBufferCreateFilename', @xmlOutputBufferCreateFilename) and
    TryLoad('xmlOutputBufferCreateBuffer', @xmlOutputBufferCreateBuffer) and
    TryLoad('xmlOutputBufferCreateFd', @xmlOutputBufferCreateFd) and
    TryLoad('xmlOutputBufferCreateIO', @xmlOutputBufferCreateIO) and
    TryLoad('xmlOutputBufferWrite', @xmlOutputBufferWrite) and
    TryLoad('xmlOutputBufferWriteString', @xmlOutputBufferWriteString) and
    TryLoad('xmlOutputBufferWriteEscape', @xmlOutputBufferWriteEscape) and
    TryLoad('xmlOutputBufferFlush', @xmlOutputBufferFlush) and
    TryLoad('xmlOutputBufferClose', @xmlOutputBufferClose) and
    TryLoad('xmlXPathFreeObject', @xmlXPathFreeObject) and
    TryLoad('xmlXPathNodeSetCreate', @xmlXPathNodeSetCreate) and
    TryLoad('xmlXPathFreeNodeSetList', @xmlXPathFreeNodeSetList) and
    TryLoad('xmlXPathFreeNodeSet', @xmlXPathFreeNodeSet) and
    TryLoad('xmlXPathObjectCopy', @xmlXPathObjectCopy) and
    TryLoad('xmlXPathCmpNodes', @xmlXPathCmpNodes) and
    TryLoad('xmlXPathCastNumberToBoolean', @xmlXPathCastNumberToBoolean) and
    TryLoad('xmlXPathCastStringToBoolean', @xmlXPathCastStringToBoolean) and
    TryLoad('xmlXPathCastNodeSetToBoolean', @xmlXPathCastNodeSetToBoolean) and
    TryLoad('xmlXPathCastToBoolean', @xmlXPathCastToBoolean) and
    TryLoad('xmlXPathCastBooleanToNumber', @xmlXPathCastBooleanToNumber) and
    TryLoad('xmlXPathCastStringToNumber', @xmlXPathCastStringToNumber) and
    TryLoad('xmlXPathCastNodeToNumber', @xmlXPathCastNodeToNumber) and
    TryLoad('xmlXPathCastNodeSetToNumber', @xmlXPathCastNodeSetToNumber) and
    TryLoad('xmlXPathCastToNumber', @xmlXPathCastToNumber) and
    TryLoad('xmlXPathCastBooleanToString', @xmlXPathCastBooleanToString) and
    TryLoad('xmlXPathCastNumberToString', @xmlXPathCastNumberToString) and
    TryLoad('xmlXPathCastNodeToString', @xmlXPathCastNodeToString) and
    TryLoad('xmlXPathCastNodeSetToString', @xmlXPathCastNodeSetToString) and
    TryLoad('xmlXPathCastToString', @xmlXPathCastToString) and
    TryLoad('xmlXPathConvertBoolean', @xmlXPathConvertBoolean) and
    TryLoad('xmlXPathConvertNumber', @xmlXPathConvertNumber) and
    TryLoad('xmlXPathConvertString', @xmlXPathConvertString) and
    TryLoad('xmlXPathNewContext', @xmlXPathNewContext) and
    TryLoad('xmlXPathFreeContext', @xmlXPathFreeContext) and
    TryLoad('xmlXPathContextSetCache', @xmlXPathContextSetCache) and
    TryLoad('xmlXPathOrderDocElems', @xmlXPathOrderDocElems) and
    TryLoad('xmlXPathEval', @xmlXPathEval) and
    TryLoad('xmlXPathEvalExpression', @xmlXPathEvalExpression) and
    TryLoad('xmlXPathEvalPredicate', @xmlXPathEvalPredicate) and
    TryLoad('xmlXPathCompile', @xmlXPathCompile) and
    TryLoad('xmlXPathCtxtCompile', @xmlXPathCtxtCompile) and
    TryLoad('xmlXPathCompiledEval', @xmlXPathCompiledEval) and
    TryLoad('xmlXPathCompiledEvalToBoolean', @xmlXPathCompiledEvalToBoolean) and
    TryLoad('xmlXPathFreeCompExpr', @xmlXPathFreeCompExpr) and
    TryLoad('xmlXPathInit', @xmlXPathInit) and
    TryLoad('xmlXPathIsNaN', @xmlXPathIsNaN) and
    TryLoad('xmlXPathIsInf', @xmlXPathIsInf) and
    TryLoad('xmlXPathRegisterNs', @xmlXPathRegisterNs) and
    TryLoad('xmlMemFree', @xmlMemFree) and
    TryLoad('xmlFree', @xmlFree);
  if not Result then
    Exit;
  FailedModuleName := '';
  FailedProcName := '';;
  Initialized := True;
end;
{$endif}

end.

