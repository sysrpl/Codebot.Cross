(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified September 2013                             *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.interop.windows.imagecodecs.txt> }
unit Codebot.Interop.Windows.ImageCodecs;

{$i codebot.inc}

interface

{$ifdef windows}
uses
  Windows, ActiveX,
  Codebot.Core;

{ From WinCodec.h }

type
  IEnumerator = IUnknown;

const
  WINCODEC_SDK_VERSION = $0236;

  CLSID_WICImagingFactory: TGUID = '{CACAF262-9370-4615-A13B-9F5539DA4C0A}';
  GUID_VendorMicrosoft: TGUID = '{F0E749CA-EDEF-4589-A73A-EE0E626A2A2B}';
  CLSID_WICBmpDecoder: TGUID = '{6B462062-7CBF-400D-9FDB-813DD10F2778}';
  CLSID_WICPngDecoder: TGUID = '{389EA17B-5078-4CDE-B6EF-25C15175C751}';
  CLSID_WICIcoDecoder: TGUID = '{C61BFCDF-2E0F-4AAD-A8D7-E06BAFEBCDFE}';
  CLSID_WICJpegDecoder: TGUID = '{9456A480-E88B-43EA-9E73-0B2D9B71B1CA}';
  CLSID_WICGifDecoder: TGUID = '{381DDA3C-9CE9-4834-A23E-1F98F8FC52BE}';
  CLSID_WICTiffDecoder: TGUID = '{B54E85D9-FE23-499F-8B88-6ACEA713752B}';
  CLSID_WICWmpDecoder: TGUID = '{A26CEC36-234C-4950-AE16-E34AACE71D0D}';
  CLSID_WICBmpEncoder: TGUID = '{69BE8BB4-D66D-47C8-865A-ED1589433782}';
  CLSID_WICPngEncoder: TGUID = '{27949969-876A-41D7-9447-568F6A35A4DC}';
  CLSID_WICJpegEncoder: TGUID = '{1A34F5C1-4A5A-46DC-B644-1F4567E7A676}';
  CLSID_WICGifEncoder: TGUID = '{114F5598-0B22-40A0-86A1-C83EA495ADBD}';
  CLSID_WICTiffEncoder: TGUID = '{0131BE10-2001-4C5F-A9B0-CC88FAB64CE8}';
  CLSID_WICWmpEncoder: TGUID = '{AC4CE3CB-E1C1-44CD-8215-5A1665509EC2}';
  GUID_ContainerFormatBmp: TGUID = '{0AF1D87E-FCFE-4188-BDEB-A7906471CBE3}';
  GUID_ContainerFormatPng: TGUID = '{1B7CFAF4-713F-473C-BBCD-6137425FAEAF}';
  GUID_ContainerFormatIco: TGUID = '{A3A860C4-338F-4C17-919A-FBA4B5628F21}';
  GUID_ContainerFormatJpeg: TGUID = '{19E4A5AA-5662-4FC5-A0C0-1758028E1057}';
  GUID_ContainerFormatTiff: TGUID = '{163BCC30-E2E9-4F0B-961D-A3E9FDB788A3}';
  GUID_ContainerFormatGif: TGUID = '{1F8A5601-7D4D-4CBD-9C82-1BC8D4EEB9A5}';
  GUID_ContainerFormatWmp: TGUID = '{57A37CAA-367A-4540-916B-F183C5093A4B}';
  CLSID_WICImagingCategories: TGUID = '{FAE3D380-FEA4-4623-8C75-C6B61110B681}';
  CATID_WICBitmapDecoders: TGUID = '{7ED96837-96F0-4812-B211-F13C24117ED3}';
  CATID_WICBitmapEncoders: TGUID = '{AC757296-3522-4E11-9862-C17BE5A1767E}';
  CATID_WICPixelFormats: TGUID = '{2B46E70F-CDA7-473E-89F6-DC9630A2390B}';
  CATID_WICFormatConverters: TGUID = '{7835EAE8-BF14-49D1-93CE-533A407B2248}';
  CATID_WICMetadataReader: TGUID = '{05AF94D8-7174-4CD2-BE4A-4124B80EE4B8}';
  CATID_WICMetadataWriter: TGUID = '{ABE3B9A4-257D-4B97-BD1A-294AF496222E}';
  CLSID_WICDefaultFormatConverter: TGUID = '{1A3F11DC-B514-4B17-8C5F-2154513852F1}';
  CLSID_WICFormatConverterNChannel: TGUID = '{C17CABB2-D4A3-47D7-A557-339B2EFBD4F1}';
  CLSID_WICFormatConverterWMPhoto: TGUID = '{9CB5172B-D600-46BA-AB77-77BB7E3A00D9}';

  GUID_WICPixelFormatUndefined: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC900}';
  GUID_WICPixelFormatDontCare: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC900}';
  GUID_WICPixelFormat1bppIndexed: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC901}';
  GUID_WICPixelFormat2bppIndexed: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC902}';
  GUID_WICPixelFormat4bppIndexed: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC903}';
  GUID_WICPixelFormat8bppIndexed: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC904}';
  GUID_WICPixelFormatBlackWhite: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC905}';
  GUID_WICPixelFormat2bppGray: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC906}';
  GUID_WICPixelFormat4bppGray: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC907}';
  GUID_WICPixelFormat8bppGray: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC908}';
  GUID_WICPixelFormat16bppBGR555: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC909}';
  GUID_WICPixelFormat16bppBGR565: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC90A}';
  GUID_WICPixelFormat16bppGray: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC90B}';
  GUID_WICPixelFormat24bppBGR: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC90C}';
  GUID_WICPixelFormat24bppRGB: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC90D}';
  GUID_WICPixelFormat32bppBGR: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC90E}';
  GUID_WICPixelFormat32bppBGRA: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC90F}';
  GUID_WICPixelFormat32bppPBGRA: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC910}';
  GUID_WICPixelFormat32bppGrayFloat: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC911}';
  GUID_WICPixelFormat48bppRGBFixedPoint: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC912}';
  GUID_WICPixelFormat16bppGrayFixedPoint: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC913}';
  GUID_WICPixelFormat32bppBGR101010: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC914}';
  GUID_WICPixelFormat48bppRGB: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC915}';
  GUID_WICPixelFormat64bppRGBA: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC916}';
  GUID_WICPixelFormat64bppPRGBA: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC917}';
  GUID_WICPixelFormat96bppRGBFixedPoint: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC918}';
  GUID_WICPixelFormat128bppRGBAFloat: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC919}';
  GUID_WICPixelFormat128bppPRGBAFloat: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC91A}';
  GUID_WICPixelFormat128bppRGBFloat: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC91B}';
  GUID_WICPixelFormat32bppCMYK: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC91C}';
  GUID_WICPixelFormat64bppRGBAFixedPoint: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC91D}';
  GUID_WICPixelFormat64bppRGBFixedPoint: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC940}';
  GUID_WICPixelFormat128bppRGBAFixedPoint: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC91E}';
  GUID_WICPixelFormat128bppRGBFixedPoint: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC941}';
  GUID_WICPixelFormat64bppRGBAHalf: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC93A}';
  GUID_WICPixelFormat64bppRGBHalf: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC942}';
  GUID_WICPixelFormat48bppRGBHalf: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC93B}';
  GUID_WICPixelFormat32bppRGBE: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC93D}';
  GUID_WICPixelFormat16bppGrayHalf: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC93E}';
  GUID_WICPixelFormat32bppGrayFixedPoint: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC93F}';
  GUID_WICPixelFormat64bppCMYK: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC91F}';
  GUID_WICPixelFormat24bpp3Channels: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC920}';
  GUID_WICPixelFormat32bpp4Channels: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC921}';
  GUID_WICPixelFormat40bpp5Channels: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC922}';
  GUID_WICPixelFormat48bpp6Channels: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC923}';
  GUID_WICPixelFormat56bpp7Channels: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC924}';
  GUID_WICPixelFormat64bpp8Channels: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC925}';
  GUID_WICPixelFormat48bpp3Channels: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC926}';
  GUID_WICPixelFormat64bpp4Channels: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC927}';
  GUID_WICPixelFormat80bpp5Channels: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC928}';
  GUID_WICPixelFormat96bpp6Channels: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC929}';
  GUID_WICPixelFormat112bpp7Channels: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC92A}';
  GUID_WICPixelFormat128bpp8Channels: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC92B}';
  GUID_WICPixelFormat40bppCMYKAlpha: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC92C}';
  GUID_WICPixelFormat80bppCMYKAlpha: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC92D}';
  GUID_WICPixelFormat32bpp3ChannelsAlpha: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC92E}';
  GUID_WICPixelFormat40bpp4ChannelsAlpha: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC92F}';
  GUID_WICPixelFormat48bpp5ChannelsAlpha: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC930}';
  GUID_WICPixelFormat56bpp6ChannelsAlpha: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC931}';
  GUID_WICPixelFormat64bpp7ChannelsAlpha: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC932}';
  GUID_WICPixelFormat72bpp8ChannelsAlpha: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC933}';
  GUID_WICPixelFormat64bpp3ChannelsAlpha: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC934}';
  GUID_WICPixelFormat80bpp4ChannelsAlpha: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC935}';
  GUID_WICPixelFormat96bpp5ChannelsAlpha: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC936}';
  GUID_WICPixelFormat112bpp6ChannelsAlpha: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC937}';
  GUID_WICPixelFormat128bpp7ChannelsAlpha: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC938}';
  GUID_WICPixelFormat144bpp8ChannelsAlpha: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC939}';

const
  IID_IWICPALETTE: TGUID = '{00000040-A8F2-4877-BA0A-FD2B6645FB94}';
  IID_IWICBitmapSource: TGUID = '{00000120-A8F2-4877-BA0A-FD2B6645FB94}';
  IID_IWICFormatConverter: TGUID = '{00000301-A8F2-4877-BA0A-FD2B6645FB94}';
  IID_IWICBitmapScaler: TGUID = '{00000302-A8F2-4877-BA0A-FD2B6645FB94}';
  IID_IWICBitmapClipper: TGUID = '{E4FBCF03-223D-4E81-9333-D635556DD1B5}';
  IID_IWICBitmapFlipRotator: TGUID = '{5009834F-2D6A-41CE-9E1B-17C5AFF7A782}';
  IID_IWICBitmapLock: TGUID = '{00000123-A8F2-4877-BA0A-FD2B6645FB94}';
  IID_IWICBitmap: TGUID = '{00000121-A8F2-4877-BA0A-FD2B6645FB94}';
  IID_IWICColorContext: TGUID = '{3C613A02-34B2-44EA-9A7C-45AEA9C6FD6D}';
  IID_IWICColorTransform: TGUID = '{B66F034F-D0E2-40AB-B436-6DE39E321A94}';
  IID_IWICFastMetadataEncoder: TGUID = '{B84E2C09-78C9-4AC4-8BD3-524AE1663A2F}';
  IID_IWICStream: TGUID = '{135FF860-22B7-4DDF-B0F6-218F4F299A43}';
  IID_IWICEnumMetadataItem: TGUID = '{DC2BB46D-3F07-481E-8625-220C4AEDBB33}';
  IID_IWICMetadataQueryReader: TGUID = '{30989668-E1C9-4597-B395-458EEDB808DF}';
  IID_IWICMetadataQueryWriter: TGUID = '{A721791A-0DEF-4D06-BD91-2118BF1DB10B}';
  IID_IWICBitmapEncoder: TGUID = '{00000103-A8F2-4877-BA0A-FD2B6645FB94}';
  IID_IWICBitmapFrameEncode: TGUID = '{00000105-A8F2-4877-BA0A-FD2B6645FB94}';
  IID_IWICBitmapDecoder: TGUID = '{9EDDE9E7-8DEE-47EA-99DF-E6FAF2ED44BF}';
  IID_IWICBitmapSourceTransform: TGUID = '{3B16811B-6A43-4EC9-B713-3D5A0C13B940}';
  IID_IWICBitmapFrameDecode: TGUID = '{3B16811B-6A43-4EC9-A813-3D930C13B940}';
  IID_IWICBitmapCodecProgressNotification: TGUID = '{64C1024E-C3CF-4462-8078-88C2B11C46D9}';
  IID_IWICComponentInfo: TGUID = '{23BC3F0A-698B-4357-886B-F24D50671334}';
  IID_IWICFormatConverterInfo: TGUID = '{9F34FB65-13F4-4F15-BC57-3726B5E53D9F}';
  IID_IWICBitmapCodecInfo: TGUID = '{E87A44C4-B76E-4C47-8B09-298EB12A2714}';
  IID_IWICBitmapEncoderInfo: TGUID = '{94C9B4EE-A09F-4F92-8A1E-4A9BCE7E76FB}';
  IID_IWICBitmapDecoderInfo: TGUID = '{D8CD007F-D08F-4191-9BFC-236EA7F0E4B5}';
  IID_IWICPixelFormatInfo: TGUID = '{E8EDA601-3D48-431A-AB44-69059BE88BBE}';
  IID_IWICImagingFactory: TGUID = '{EC5EC8A9-C395-4314-9C77-54D7A935FF70}';
  IID_IWICDevelopRawNotificationCallback: TGUID = '{95C75A6E-3E8C-4EC2-85A8-AEBCC551E59B}';
  IID_IWICDevelopRaw: TGUID = '{FBEC5E44-F7BE-4B65-B7F8-C0C81FEF026D}';

  INTSAFE_E_ARITHMETIC_OVERFLOW                 = $80070216;  // 0X216 = 534 = ERROR_ARITHMETIC_OVERFLOW
  FACILITY_WINCODEC_ERR                         = $898;
  WINCODEC_ERR_BASE                             = $2000;
  WINCODEC_ERR_SEV                              = 1;
  WINCODEC_ERR_GENERIC_ERROR                    = E_FAIL;
  WINCODEC_ERR_INVALIDPARAMETER                 = E_INVALIDARG;
  WINCODEC_ERR_OUTOFMEMORY                      = E_OUTOFMEMORY;
  WINCODEC_ERR_NOTIMPLEMENTED                   = E_NOTIMPL;
  WINCODEC_ERR_ABORTED                          = E_ABORT;
  WINCODEC_ERR_ACCESSDENIED                     = E_ACCESSDENIED;
  WINCODEC_ERR_VALUEOVERFLOW                    = INTSAFE_E_ARITHMETIC_OVERFLOW;
  WINCODEC_ERR_WRONGSTATE                       = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F04));
  WINCODEC_ERR_VALUEOUTOFRANGE                  = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F05));
  WINCODEC_ERR_UNKNOWNIMAGEFORMAT               = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F07));
  WINCODEC_ERR_UNSUPPORTEDVERSION               = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F0B));
  WINCODEC_ERR_NOTINITIALIZED                   = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F0C));
  WINCODEC_ERR_ALREADYLOCKED                    = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F0D));
  WINCODEC_ERR_PROPERTYNOTFOUND                 = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F40));
  WINCODEC_ERR_PROPERTYNOTSUPPORTED             = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F41));
  WINCODEC_ERR_PROPERTYSIZE                     = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F42));
  WINCODEC_ERR_CODECPRESENT                     = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F43));
  WINCODEC_ERR_CODECNOTHUMBNAIL                 = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F44));
  WINCODEC_ERR_PALETTEUNAVAILABLE               = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F45));
  WINCODEC_ERR_CODECTOOMANYSCANLINES            = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F46));
  WINCODEC_ERR_INTERNALERROR                    = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F48));
  WINCODEC_ERR_SOURCERECTDOESNOTMATCHDIMENSIONS = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F49));
  WINCODEC_ERR_COMPONENTNOTFOUND                = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F50));
  WINCODEC_ERR_IMAGESIZEOUTOFRANGE              = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F51));
  WINCODEC_ERR_TOOMUCHMETADATA                  = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F52));
  WINCODEC_ERR_BADIMAGE                         = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F60));
  WINCODEC_ERR_BADHEADER                        = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F61));
  WINCODEC_ERR_FRAMEMISSING                     = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F62));
  WINCODEC_ERR_BADMETADATAHEADER                = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F63));
  WINCODEC_ERR_BADSTREAMDATA                    = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F70));
  WINCODEC_ERR_STREAMWRITE                      = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F71));
  WINCODEC_ERR_STREAMREAD                       = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F72));
  WINCODEC_ERR_STREAMNOTAVAILABLE               = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F73));
  WINCODEC_ERR_UNSUPPORTEDPIXELFORMAT           = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F80));
  WINCODEC_ERR_UNSUPPORTEDOPERATION             = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F81));
  WINCODEC_ERR_INVALIDREGISTRATION              = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F8A));
  WINCODEC_ERR_COMPONENTINITIALIZEFAILURE       = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F8B));
  WINCODEC_ERR_INSUFFICIENTBUFFER               = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F8C));
  WINCODEC_ERR_DUPLICATEMETADATAPRESENT         = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F8D));
  WINCODEC_ERR_PROPERTYUNEXPECTEDTYPE           = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F8E));
  WINCODEC_ERR_UNEXPECTEDSIZE                   = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F8F));
  WINCODEC_ERR_INVALIDQUERYREQUEST              = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F90));
  WINCODEC_ERR_UNEXPECTEDMETADATATYPE           = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F91));
  WINCODEC_ERR_REQUESTONLYVALIDATMETADATAROOT   = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F92));
  WINCODEC_ERR_INVALIDQUERYCHARACTER            = HResult((WINCODEC_ERR_SEV shl 31) or (FACILITY_WINCODEC_ERR shl 16) or (WINCODEC_ERR_BASE + $F93));

  WICRawChangeNotification_ExposureCompensation       = $00000001;
  WICRawChangeNotification_NamedWhitePoint            = $00000002;
  WICRawChangeNotification_KelvinWhitePoint           = $00000004;
  WICRawChangeNotification_RGBWhitePoint              = $00000008;
  WICRawChangeNotification_Contrast                   = $00000010;
  WICRawChangeNotification_Gamma                      = $00000020;
  WICRawChangeNotification_Sharpness                  = $00000040;
  WICRawChangeNotification_Saturation                 = $00000080;
  WICRawChangeNotification_Tint                       = $00000100;
  WICRawChangeNotification_NoiseReduction             = $00000200;
  WICRawChangeNotification_DestinationColorContext    = $00000400;
  WICRawChangeNotification_ToneCurve                  = $00000800;
  WICRawChangeNotification_Rotation                   = $00001000;
  WICRawChangeNotification_RenderMode                 = $00002000;

type
  WICRawToneCurvePoint = record
    Input: Double;
    Output: Double;
  end;

  WICRawToneCurve = record
    cPoints: UINT;
    aPoints: Array[0..0] of WICRawToneCurvePoint;
  end;

  WICColor = LongWord;
  REFWICPixelFormatGUID = TGUID;
  WICPixelFormatGUID = TGUID;

  PWICRect = ^WICRect;
  WICRect = record
    X: Integer;
    Y: Integer;
    Width: Integer;
    Height: Integer;
  end;

  WICBitmapPattern = record
    Position: ULARGE_INTEGER;
    Length: ULONG;
    Pattern: PBYTE;
    Mask: PBYTE;
    EndOfStream: Bool;
  end;

  PPROPBAG2 = ^TPROPBAG2;
  TPROPBAG2 = record
    dwType: DWORD;
    vt: Variant;
    cfType: TCLIPFORMAT;
    dwHint: DWORD;
    pstrName: PWideChar;
    clsid: TCLSID;
    end;

type
  WICColorContextType = (
    WICColorContextUninitialized = $0,
    WICColorContextProfile = $1,
    WICColorContextExifColorSpace = $2);

  WICBitmapCreateCacheOption = (
    WICBitmapNoCache = $0,
    WICBitmapCacheOnDemand = $1,
    WICBitmapCacheOnLoad = $2);

  WICDecodeOptions = (
    WICDecodeMetadataCacheOnDemand = $0,
    WICDecodeMetadataCacheOnLoad = $1);

  WICBitmapEncoderCacheOption = (
    WICBitmapEncoderCacheInMemory = $0,
    WICBitmapEncoderCacheTempFile = $1,
    WICBitmapEncoderNoCache = $2);

  WICComponentType = (
    WICDecoder = $1,
    WICEncoder = $2,
    WICPixelFormatConverter = $4,
    WICMetadataReader = $8,
    WICMetadataWriter = $10,
    WICPixelFormat = $20,
    WICAllComponents = $3F);

  WICComponentEnumerateOptions = (
    WICComponentEnumerateDefault = $0,
    WICComponentEnumerateRefresh = $1,
    WICComponentEnumerateUnsigned = $40000000,
    WICComponentEnumerateDisabled = $80000000);

  WICBitmapInterpolationMode = (
    WICBitmapInterpolationModeNearestNeighbor = $0,
    WICBitmapInterpolationModeLinear = $1,
    WICBitmapInterpolationModeCubic = $2,
    WICBitmapInterpolationModeFant = $3);

  WICBitmapPaletteType = (
    WICBitmapPaletteTypeCustom = $0,
    WICBitmapPaletteTypeMedianCut = $1,
    WICBitmapPaletteTypeFixedBW = $2,
    WICBitmapPaletteTypeFixedHalftone8 = $3,
    WICBitmapPaletteTypeFixedHalftone27 = $4,
    WICBitmapPaletteTypeFixedHalftone64 = $5,
    WICBitmapPaletteTypeFixedHalftone125  = $6,
    WICBitmapPaletteTypeFixedHalftone216  = $7,
    WICBitmapPaletteTypeFixedHalftone252  = $8,
    WICBitmapPaletteTypeFixedHalftone256  = $9,
    WICBitmapPaletteTypeFixedGray4  = $A,
    WICBitmapPaletteTypeFixedGray16  = $B,
    WICBitmapPaletteTypeFixedGray256  = $C);

  WICBitmapDitherType = (
    WICBitmapDitherTypeNone = $0,
    WICBitmapDitherTypeOrdered4x4 = $1,
    WICBitmapDitherTypeOrdered8x8 = $2,
    WICBitmapDitherTypeOrdered16x16 = $3,
    WICBitmapDitherTypeSpiral4x4 = $4,
    WICBitmapDitherTypeSpiral8x8 = $5,
    WICBitmapDitherTypeDualSpiral4x4  = $6,
    WICBitmapDitherTypeDualSpiral8x8  = $7,
    WICBitmapDitherTypeErrorDiffusion  = $8);

  WICBitmapAlphaChannelOption = (
    WICBitmapUseAlpha = $0,
    WICBitmapUsePremultipliedAlpha = $1,
    WICBitmapIgnoreAlpha = $2);

  WICBitmapTransformOptions = (
    WICBitmapTransformRotate0 = 0,
    WICBitmapTransformRotate90 = $1,
    WICBitmapTransformRotate180 = $2,
    WICBitmapTransformRotate270 = $3,
    WICBitmapTransformFlipHorizontal = $8,
    WICBitmapTransformFlipVertical = $10);

  WICBitmapLockFlags = (
    WICBitmapLockRead = $1,
    WICBitmapLockWrite = $2);

  WICBitmapDecoderCapabilities = (
    WICBitmapDecoderCapabilitySameEncoder = $1,
    WICBitmapDecoderCapabilityCanDecodeAllImages = $2,
    WICBitmapDecoderCapabilityCanDecodeSomeImages = $4,
    WICBitmapDecoderCapabilityCanEnumerateMetadata = $8,
    WICBitmapDecoderCapabilityCanDecodeThumbnail = $10);

  WICProgressOperation = (
    WICProgressOperationCopyPixels = $1,
    WICProgressOperationWritePixels = $2,
    WICProgressOperationAll = $FFFF);

  WICProgressNotification = (
    WICProgressNotificationBegin = $10000,
    WICProgressNotificationEnd = $20000,
    WICProgressNotificationFrequent = $40000,
    WICProgressNotificationAll = $FFFF0000);

  WICComponentSigning = (
    WICComponentSigned = $1,
    WICComponentUnsigned = $2,
    WICComponentSafe = $4,
    WICComponentDisabled = $80000000);

  WICTiffCompressionOption = (
    WICTiffCompressionDontCare = $0,
    WICTiffCompressionNone = $1,
    WICTiffCompressionCCITT3 = $2,
    WICTiffCompressionCCITT4 = $3,
    WICTiffCompressionLZW  = $4,
    WICTiffCompressionRLE  = $5,
    WICTiffCompressionZIP  = $6);

  WICNamedWhitePoint = (
    WICWhitePointDefault = $1,
    WICWhitePointDaylight  = $2,
    WICWhitePointCloudy  = $4,
    WICWhitePointShade = $8,
    WICWhitePointTungsten  = $10,
    WICWhitePointFluorescent = $20,
    WICWhitePointFlash = $40,
    WICWhitePointUnderwater  = $80,
    WICWhitePointCustom  = $100,
    WICWhitePointAutoWhiteBalance  = $200);

  WICRawCapabilities = (
    WICRawCapabilityNotSupported = $0,
    WICRawCapabilityGetSupported = $1,
    WICRawCapabilityFullySupported = $2);

  WICRawRotationCapabilities = (
    WICRawRotationCapabilityNotSupported = $0,
    WICRawRotationCapabilityGetSupported = $1,
    WICRawRotationCapabilityNinetyDegreesSupported = $2,
    WICRawRotationCapabilityFullySupported = $3);

  WICRawParameterSet = (
    WICAsShotParameterSet = $1,
    WICUserAdjustedParameterSet = $2,
    WICAutoAdjustedParameterSet = $3);

  WICRawRenderMode = (
    WICRawRenderModeDraft = $1,
    WICRawRenderModeNormal = $2,
    WICRawRenderModeBestQuality = $3);

  WICMetadataCreationOptions = (
    WICMetadataCreationDefault = $0,
    WICMetadataCreationFailUnknown = $10000,
    WICMetadataCreationMask = $FFFF0000);

  WICPersistOptions = (
    WICPersistOptionLittleEndian = $0,
    WICPersistOptionBigEndian = $1,
    WICPersistOptionStrictFormat = $2,
    WICPersistOptionNoCacheStream = $4,
    WICPersistOptionPreferUTF8 = $8,
    WICPersistOptionMask = $FFFF);

type
  WICRawCapabilitiesInfo = record
    cbSize: UINT;
    CodecMajorVersion: UINT;
    CodecMinorVersion: UINT;
    ExposureCompensationSupport: WICRawCapabilities;
    ContrastSupport: WICRawCapabilities;
    RGBWhitePointSupport: WICRawCapabilities;
    NamedWhitePointSupport: WICRawCapabilities;
    NamedWhitePointSupportMask: UINT;
    KelvinWhitePointSupport: WICRawCapabilities;
    GammaSupport: WICRawCapabilities;
    TintSupport: WICRawCapabilities;
    SaturationSupport: WICRawCapabilities;
    SharpnessSupport: WICRawCapabilities;
    NoiseReductionSupport: WICRawCapabilities;
    DestinationColorProfileSupport: WICRawCapabilities;
    ToneCurveSupport: WICRawCapabilities;
    RotationSupport: WICRawRotationCapabilities;
    RenderModeSupport: WICRawCapabilities;
  end;

type
  TProgressNotification = function(pvData: Pointer; uFrameNum: ULONG;
    operation: WICProgressOperation; dblProgress: double): HResult; Stdcall;

type
  IWICPalette = interface;
  IWICBitmapSource = interface;
  IWICFormatConverter = interface;
  IWICBitmapScaler = interface;
  IWICBitmapClipper = interface;
  IWICBitmapFlipRotator = interface;
  IWICBitmapLock = interface;
  IWICBitmap = interface;
  IWICColorContext = interface;
  IWICColorTransform = interface;
  IWICFastMetadataEncoder = interface;
  IWICStream = interface;
  IWICEnumMetadataItem = interface;
  IWICMetadataQueryReader = interface;
  IWICMetadataQueryWriter = interface;
  IWICBitmapEncoder = interface;
  IWICBitmapFrameEncode = interface;
  IWICBitmapDecoder = interface;
  IWICBitmapSourceTransform = interface;
  IWICBitmapFrameDecode = interface;
  IWICBitmapCodecProgressNotification = interface;
  IWICComponentInfo = interface;
  IWICFormatConverterInfo = interface;
  IWICBitmapCodecInfo = interface;
  IWICBitmapEncoderInfo = interface;
  IWICBitmapDecoderInfo = interface;
  IWICPixelFormatInfo = interface;
  IWICImagingFactory = interface;
  IWICDevelopRawNotificationCallback = interface;
  IWICDevelopRaw = interface;

{ IPropertyBag2 }

  IPropertyBag2 = interface(IPropertyBag)
  ['{22F55882-280B-11D0-A8A9-00A0C90C2004}']
    function Read2(cProperties: ULONG; pPropBag: PPROPBAG2;
      pErrLog: IErrorLog; out pvarValue: Variant; var phrError: HResult): HResult; stdcall;
    function Write2(cProperties: ULONG; pPropBag : PPROPBAG2; pvarValue: PVariant): HResult; stdcall;
    function CountProperties(out pcProperties: ULONG): HResult; stdcall;
    function GetPropertyInfo(iProperty: ULONG; cProperties: ULONG; out pPropBag: PPROPBAG2;
      out pcProperties: ULONG): HResult; stdcall;
    function LoadObject(pstrName: PWideChar; dwHint: DWORD;
      pUnkObject: IUnknown; pErrLog: IErrorLog): HResult; stdcall;
  end;

{ IWICPalette }

  IWICPalette = interface(IUnknown)
  ['{00000040-A8F2-4877-BA0A-FD2B6645FB94}']
    function InitializePredefined(ePaletteType: WICBitmapPaletteType; fAddTransparentColor: boolean): HResult; stdcall;
    function InitializeCustom(pColors: WICColor; cCount: UINT): HResult; stdcall;
    function InitializeFromBitmap(pISurface: IWICBitmapSource; cCount: UINT; fAddTransparentColor: boolean): HResult; stdcall;
    function InitializeFromPalette(pIPalette: IWICPalette): HResult; stdcall;
    function GetType(out pePaletteType: WICBitmapPaletteType): HResult; stdcall;
    function GetColorCount(out pcCount: UINT): HResult; stdcall;
    function GetColors(cCount: UINT; out pColors: WICColor; out pcActualColors: UINT): HResult; stdcall;
    function IsBlackWhite(out pfIsBlackWhite: boolean): HResult; stdcall;
    function IsGrayscale(out pfIsGrayscale: boolean): HResult; stdcall;
    function HasAlpha(out pfHasAlpha: boolean): HResult; stdcall;
  end;

{ IWICBitmapSource }

  IWICBitmapSource = interface(IUnknown)
  ['{00000120-A8F2-4877-BA0A-FD2B6645FB94}']
    function GetSize(out puiWidth, puiHeight: UINT): HResult; stdcall;
    function GetPixelFormat(out pPixelFormat: WICPixelFormatGUID): HResult; stdcall;
    function GetResolution(out pDpiX: Double; out pDpiY: Double): HResult; stdcall;
    function CopyPalette(pIPalette: IWICPalette): HResult; stdcall;
    function CopyPixels(prc: PWICRect; cPWideCharide, cbBufferSize: UINT; pbBuffer: PByte): HResult; stdcall;
  end;

{ IWICFormatConverter }

  IWICFormatConverter = interface(IWICBitmapSource)
  ['{00000301-A8F2-4877-BA0A-FD2B6645FB94}']
    function Initialize(pISource: IWICBitmapSource; const dstFormat: REFWICPixelFormatGUID; dither: WICBitmapDitherType;
      pIPalette: IWICPalette; alphaThresholdPercent: Double; paletteTranslate: WICBitmapPaletteType): HResult; stdcall;
    function CanConvert(const srcPixelFormat, dstPixelFormat: REFWICPixelFormatGUID; out pfCanConvert: boolean): HResult; stdcall;
  end;

{ IWICBitmapScaler }

  IWICBitmapScaler = interface(IWICBitmapSource)
  ['{00000302-A8F2-4877-BA0A-FD2B6645FB94}']
    function Initialize(pISource: IWICBitmapSource; uiWidth, uiHeight: UINT; mode: WICBitmapInterpolationMode): HResult; stdcall;
  end;

{ IWICBitmapClipper }

  IWICBitmapClipper = interface(IWICBitmapSource)
  ['{E4FBCF03-223D-4E81-9333-D635556DD1B5}']
    function Initialize(pISource: IWICBitmapSource; prc: WICRect): HResult; stdcall;
  end;

{ IWICBitmapFlipRotator }

  IWICBitmapFlipRotator = interface(IWICBitmapSource)
  ['{5009834F-2D6A-41CE-9E1B-17C5AFF7A782}']
    function Initialize(pISource: IWICBitmapSource; options: WICBitmapTransformOptions): HResult; stdcall;
  end;

{ IWICBitmapLock }

  IWICBitmapLock = interface(IUnknown)
  ['{00000123-A8F2-4877-BA0A-FD2B6645FB94}']
    function GetSize(out puiWidth, puiHeight: UINT): HResult; stdcall;
    function GetStride(out pcPWideCharide: UINT): HResult; stdcall;
    function GetDataPointer(out pcbBufferSize: UINT; out ppbData: PByte): HResult; stdcall;
    function GetPixelFormat(out pPixelFormat: WICPixelFormatGUID): HResult; stdcall;
  end;

{ IWICBitmap }

  IWICBitmap = interface(IWICBitmapSource)
  ['{00000121-A8F2-4877-BA0A-FD2B6645FB94}']
    function Lock(prcLock: PWICRect; flags: DWORD; out ppILock: IWICBitmapLock): HResult; stdcall;
    function SetPalette(pIPalette: IWICPalette): HResult; stdcall;
    function SetResolution(dpiX: Double; dpiY: Double): HResult; stdcall;
  end;

{ IWICColorContext }

  IWICColorContext = interface(IUnknown)
  ['{3C613A02-34B2-44EA-9A7C-45AEA9C6FD6D}']
    function InitializeFromFilename(wzFilename: PWideChar): HResult; stdcall;
    function InitializeFromMemory(pbBuffer: PByte; cbBufferSize: UINT): HResult; stdcall;
    function InitializeFromExifColorSpace(value: UINT): HResult; stdcall;
    function GetType(out pType: WICColorContextType): HResult; stdcall;
    function GetProfileBytes(cbBuffer: UINT; var pbBuffer: PByte; out pcbActual: UINT): HResult; stdcall;
    function GetExifColorSpace(out pValue: UINT): HResult; stdcall;
  end;

{ IWICColorTransform }

  IWICColorTransform = interface(IWICBitmapSource)
  ['{B66F034F-D0E2-40AB-B436-6DE39E321A94}']
    function Initialize(pIBitmapSource: IWICBitmapSource; pIContextSource: IWICColorContext;
      pIContextDest: IWICColorContext; const pixelFmtDest: REFWICPixelFormatGUID): HResult; stdcall;
  end;

{ IWICFastMetadataEncoder }

  IWICFastMetadataEncoder = interface(IUnknown)
  ['{B84E2C09-78C9-4AC4-8BD3-524AE1663A2F}']
    function GetMetadataQueryWriter(ppIMetadataQueryWriter: IWICMetadataQueryWriter): HResult; stdcall;
  end;

{ IWICStream }

  IWICStream = interface(IStream)
  ['{135FF860-22B7-4DDF-B0F6-218F4F299A43}']
    function InitializeFromIStream(pIStream: IStream): HResult; stdcall;
    function InitializeFromFilename(wzFileName: PWideChar; dwDesiredAccess: DWORD): HResult; stdcall;
    function InitializeFromMemory(pbBuffer: PByte; cbBufferSize: DWORD): HResult; stdcall;
    function InitializeFromIStreamRegion(pIStream: IStream; ulOffset: ULARGE_INTEGER; ulMaxSize: ULARGE_INTEGER): HResult; stdcall;
  end;

{ IWICEnumMetadataItem }

  IWICEnumMetadataItem = interface(IUnknown)
  ['{DC2BB46D-3F07-481E-8625-220C4AEDBB33}']
    function Next(var rgeltSchema: PROPVARIANT; var rgeltId: PROPVARIANT; var rgeltValue: PROPVARIANT; out pceltFetched: ULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppIEnumMetadataItem: IWICEnumMetadataItem): HResult; stdcall;
  end;

{ IWICMetadataQueryReader }

  IWICMetadataQueryReader = interface(IUnknown)
  ['{30989668-E1C9-4597-B395-458EEDB808DF}']
    function Clone(out pguidContainerFormat: TGUID): HResult; stdcall;
    function GetLocation(cchMaxLength: UINT; var wzNamespace: PWideChar; out pcchActualLength: UINT): HResult; stdcall;
    function GetMetadataByName(wzName: PWideChar; var pvarValue: PROPVARIANT): HResult; stdcall;
    function GetEnumerator(out ppIEnumString: IEnumString): HResult; stdcall;
  end;

{ IWICMetadataQueryWriter }

  IWICMetadataQueryWriter = interface(IWICMetadataQueryReader)
  ['{A721791A-0DEF-4D06-BD91-2118BF1DB10B}']
    function SetMetadataByName(wzName: PWideChar; pvarValue: PROPVARIANT): HResult; stdcall;
    function RemoveMetadataByName(wzName: PWideChar): HResult; stdcall;
  end;

{ IWICBitmapEncoder }

  IWICBitmapEncoder = interface(IUnknown)
  ['{00000103-A8F2-4877-BA0A-FD2B6645FB94}']
    function Initialize(pIStream: IStream; cacheOption: WICBitmapEncoderCacheOption): HResult; stdcall;
    function GetContainerFormat(out pguidContainerFormat: TGUID): HResult; stdcall;
    function GetEncoderInfo(out ppIEncoderInfo: IWICBitmapEncoderInfo): HResult; stdcall;
    function SetColorContexts(cCount: UINT; ppIColorContext: IWICColorContext): HResult; stdcall;
    function SetPalette(pIPalette: IWICPalette): HResult; stdcall;
    function SetThumbnail(pIThumbnail: IWICBitmapSource): HResult; stdcall;
    function SetPreview(pIPreview: IWICBitmapSource): HResult; stdcall;
    function CreateNewFrame(out ppIFrameEncode: IWICBitmapFrameEncode; var ppIEncoderOptions: IPropertyBag2): HResult; stdcall;
    function Commit: HResult; stdcall;
    function GetMetadataQueryWriter(out ppIMetadataQueryWriter: IWICMetadataQueryWriter): HResult; stdcall;
  end;

{ IWICBitmapFrameEncode }

  IWICBitmapFrameEncode = interface(IUnknown)
  ['{00000105-A8F2-4877-BA0A-FD2B6645FB94}']
    function Initialize(pIEncoderOptions: IPropertyBag2): HResult; stdcall;
    function SetSize(uiWidth, uiHeight: UINT): HResult; stdcall;
    function SetResolution(const dpiX, dpiY: Double): HResult; stdcall;
    function SetPixelFormat(var pPixelFormat: WICPixelFormatGUID): HResult; stdcall;
    function SetColorContexts(cCount: UINT; ppIColorContext: IWICColorContext): HResult; stdcall;
    function SetPalette(pIPalette: IWICPalette): HResult; stdcall;
    function SetThumbnail(pIThumbnail: IWICBitmapSource): HResult; stdcall;
    function WritePixels(lineCount, cPWideCharide, cbBufferSize: UINT; pbPixels: PByte): HResult; stdcall;
    function WriteSource(pIBitmapSource: IWICBitmapSource; prc: PWICRect): HResult; stdcall;
    function Commit: HResult; stdcall;
    function GetMetadataQueryWriter(ppIMetadataQueryWriter: IWICMetadataQueryWriter): HResult; stdcall;
  end;

{ IWICBitmapDecoder }

  IWICBitmapDecoder = interface(IUnknown)
  ['{9EDDE9E7-8DEE-47EA-99DF-E6FAF2ED44BF}']
    function QueryCapability(pIStream: IStream; out pdwCapability: DWORD): HResult; stdcall;
    function Initialize(pIStream: IStream; cacheOptions: WICDecodeOptions): HResult; stdcall;
    function GetContainerFormat(out pguidContainerFormat: TGUID): HResult; stdcall;
    function GetDecoderInfo(out ppIDecoderInfo: IWICBitmapDecoderInfo): HResult; stdcall;
    function CopyPalette(pIPalette: IWICPalette): HResult; stdcall;
    function GetMetadataQueryReader(out ppIMetadataQueryReader: IWICMetadataQueryReader): HResult; stdcall;
    function GetPreview(out ppIBitmapSource: IWICBitmapSource): HResult; stdcall;
    function GetColorContexts(cCount: UINT; var ppIColorContexts: IWICColorContext; out pcActualCount: UINT): HResult; stdcall;
    function GetThumbnail(out ppIThumbnail: IWICBitmapSource): HResult; stdcall;
    function GetFrameCount(out pCount: UINT): HResult; stdcall;
    function GetFrame(index: UINT; out ppIBitmapFrame: IWICBitmapFrameDecode): HResult; stdcall;
  end;

{ IWICBitmapSourceTransform }

  IWICBitmapSourceTransform = interface(IUnknown)
  ['{3B16811B-6A43-4EC9-B713-3D5A0C13B940}']
    function CopyPixels(prcSrc: WICRect; uiWidth, uiHeight: UINT; pguidDstFormat: WICPixelFormatGUID;
      dstTransform: WICBitmapTransformOptions; nStride: UINT; cbBufferSize: UINT; out pbBuffer: PByte): HResult; stdcall;
    function GetClosestSize(out puiWidth: UINT; out puiHeight: UINT): HResult; stdcall;
    function GetClosestPixelFormat(out pguidDstFormat: WICPixelFormatGUID): HResult; stdcall;
    function DoesSupportTransform(dstTransform: WICBitmapTransformOptions; out pfIsSupported: boolean): HResult; stdcall;
  end;

{ IWICBitmapFrameDecode }

  IWICBitmapFrameDecode = interface(IWICBitmapSource)
  ['{3B16811B-6A43-4EC9-A813-3D930C13B940}']
    function GetMetadataQueryReader(out ppIMetadataQueryReader: IWICMetadataQueryReader): HResult; stdcall;
    function GetColorContexts(cCount: UINT; var ppIColorContexts: IWICColorContext; out pcActualCount: UINT): HResult; stdcall;
    function GetThumbnail(ppIThumbnail: IWICBitmapSource): HResult; stdcall;
  end;

{ IWICBitmapCodecProgressNotification }

  IWICBitmapCodecProgressNotification = interface(IUnknown)
  ['{64C1024E-C3CF-4462-8078-88C2B11C46D9}']
    function RegisterProgressNotification( pfnProgressNotification: TProgressNotification; pvData: Pointer;
      dwProgressFlags: DWORD): HResult; stdcall;
  end;

{ IWICComponentInfo }

  IWICComponentInfo = interface(IUnknown)
  ['{23BC3F0A-698B-4357-886B-F24D50671334}']
    function GetComponentType(out pType: WICComponentType): HResult; stdcall;
    function GetCLSID(out pclsid: TCLSID): HResult; stdcall;
    function GetSigningStatus(out pStatus: DWORD): HResult; stdcall;
    function GetAuthor(cchAuthor: UINT; var wzAuthor: PWideChar; out pcchActual: UINT): HResult; stdcall;
    function GetVendorGUID(out pguidVendor: TGUID): HResult; stdcall;
    function GetVersion(cchVersion: UINT; wzVersion: PWideChar; out pcchActual: UINT): HResult; stdcall;
    function GetSpecVersion(cchSpecVersion: UINT; wzSpecVersion: PWideChar; out pcchActual: UINT): HResult; stdcall;
    function GetFriendlyName(cchFriendlyName: UINT; wzFriendlyName: PWideChar; out pcchActual: UINT): HResult; stdcall;
  end;

{ IWICFormatConverterInfo }

  IWICFormatConverterInfo = interface(IWICComponentInfo)
  ['{9F34FB65-13F4-4F15-BC57-3726B5E53D9F}']
    function GetPixelFormats(cFormats: UINT; var pPixelFormatGUIDs: WICPixelFormatGUID; out pcActual: UINT): HResult; stdcall;
    function CreateInstance(out ppIConverter: IWICFormatConverter): HResult; stdcall;
  end;

{ IWICBitmapCodecInfo }

  IWICBitmapCodecInfo = interface(IWICComponentInfo)
  ['{E87A44C4-B76E-4C47-8B09-298EB12A2714}']
    function GetContainerFormat(out pguidContainerFormat: TGUID): HResult; stdcall;
    function GetPixelFormats(cFormats: UINT; var pguidPixelFormats: TGUID; out pcActual: UINT): HResult; stdcall;
    function GetColorManagementVersion(cchColorManagementVersion: UINT; var wzColorManagementVersion: PWideChar;
      out pcActual: UINT): HResult; stdcall;
    function GetDeviceManufacturer(cchDeviceManufacturer: UINT; var wzDeviceManufacturer: PWideChar; out pcActual: UINT): HResult; stdcall;
    function GetDeviceModels(cchDeviceModels: UINT; var wzDeviceModels: PWideChar; out pcActual: UINT): HResult; stdcall;
    function GetMimeTypes(cchMimeTypes: UINT; var wzMimeTypes: PWideChar; out pcActual: UINT): HResult; stdcall;
    function GetFileExtensions(cchFileExtensions: UINT; var wzFileExtensions: PWideChar; out pcActual: UINT): HResult; stdcall;
    function DoesSupportAnimation(out pfSupportAnimation: boolean): HResult; stdcall;
    function DoesSupportChromakey(out pfSupportChromakey: boolean): HResult; stdcall;
    function DoesSupportLossless(out pfSupportLossless: boolean): HResult; stdcall;
    function DoesSupportMultiframe(out pfSupportMultiframe: boolean): HResult; stdcall;
    function MatchesMimeType(wzMimeType: PWideChar; out pfMatches: boolean): HResult; stdcall;
  end;

{ IWICBitmapEncoderInfo }

  IWICBitmapEncoderInfo = interface(IWICBitmapCodecInfo)
  ['{94C9B4EE-A09F-4F92-8A1E-4A9BCE7E76FB}']
    function CreateInstance(out ppIBitmapEncoder: IWICBitmapEncoder): HResult; stdcall;
  end;

{ IWICBitmapDecoderInfo }

  IWICBitmapDecoderInfo = interface(IWICBitmapCodecInfo)
  ['{D8CD007F-D08F-4191-9BFC-236EA7F0E4B5}']
    function GetPatterns(cbSizePatterns: UINT; var pPatterns: WICBitmapPattern; var pcPatterns, pcbPatternsActual: UINT): HResult; stdcall;
    function MatchesPattern(pIStream: IStream; out pfMatches: boolean): HResult; stdcall;
    function CreateInstance(out ppIBitmapDecoder: IWICBitmapDecoder): HResult; stdcall;
  end;

{ IWICPixelFormatInfo }

  IWICPixelFormatInfo = interface(IWICComponentInfo)
  ['{E8EDA601-3D48-431A-AB44-69059BE88BBE}']
    function GetFormatGUID(out pFormat: TGUID): HResult; stdcall;
    function GetColorContext(out ppIColorContext: IWICColorContext): HResult; stdcall;
    function GetBitsPerPixel(out puiBitsPerPixel: UINT): HResult; stdcall;
    function GetChannelCount(out puiChannelCount: UINT): HResult; stdcall;
    function GetChannelMask(uiChannelIndex, cbMaskBuffer: UINT; var pbMaskBuffer: PByte; out pcbActual: UINT): HResult; stdcall;
  end;

{ IWICImagingFactory }

  IWICImagingFactory = interface(IUnknown)
  ['{EC5EC8A9-C395-4314-9C77-54D7A935FF70}']
    function CreateDecoderFromFilename(wzFilename: PWideChar; pguidVendor: PGUID; dwDesiredAccess: DWORD;
      metadataOptions: WICDecodeOptions; out ppIDecoder: IWICBitmapDecoder): HResult; stdcall;
    function CreateDecoderFromStream(pIStream: IStream; pguidVendor: PGUID; metadataOptions: WICDecodeOptions;
      out ppIDecoder: IWICBitmapDecoder): HResult; stdcall;
    function CreateDecoderFromFileHandle(hFile: Pointer; pguidVendor: PGUID; metadataOptions: WICDecodeOptions;
      out ppIDecoder: IWICBitmapDecoder): HResult; stdcall;
    function CreateComponentInfo(const clsidComponent: TGUID; out ppIInfo: IWICComponentInfo): HResult; stdcall;
    function CreateDecoder(const guidContainerFormat: TGUID; pguidVendor: PGUID; out ppIDecoder: IWICBitmapDecoder): HResult; stdcall;
    function CreateEncoder(const guidContainerFormat: TGUID; pguidVendor: PGUID; out ppIEncoder: IWICBitmapEncoder): HResult; stdcall;
    function CreatePalette(out ppIPalette: IWICPalette): HResult; stdcall;
    function CreateFormatConverter(out ppIFormatConverter: IWICFormatConverter): HResult; stdcall;
    function CreateBitmapScaler(out ppIBitmapScaler: IWICBitmapScaler): HResult; stdcall;
    function CreateBitmapClipper(out ppIBitmapClipper: IWICBitmapClipper): HResult; stdcall;
    function CreateBitmapFlipRotator(out ppIBitmapFlipRotator: IWICBitmapFlipRotator): HResult; stdcall;
    function CreateStream(out ppIWICStream: IWICStream): HResult; stdcall;
    function CreateColorContext(out ppIWICColorContext: IWICColorContext): HResult; stdcall;
    function CreateColorTransformer(out ppIWICColorTransform: IWICColorTransform): HResult; stdcall;
    function CreateBitmap(uiWidth, uiHeight: UINT; const pixelFormat: REFWICPixelFormatGUID;
      option: WICBitmapCreateCacheOption; out ppIBitmap: IWICBitmap): HResult; stdcall;
    function CreateBitmapFromSource(pIBitmapSource: IWICBitmapSource; option: WICBitmapCreateCacheOption;
      out ppIBitmap: IWICBitmap): HResult; stdcall;
    function CreateBitmapFromSourceRect(pIBitmapSource: IWICBitmapSource; x, y, width, height: UINT;
      out ppIBitmap: IWICBitmap): HResult; stdcall;
    function CreateBitmapFromMemory(uiWidth, uiHeight: UINT; const pixelFormat: REFWICPixelFormatGUID; cPWideCharide,
      cbBufferSize: UINT; pbBuffer: PByte; out ppIBitmap: IWICBitmap): HResult; stdcall;
    function CreateBitmapFromHBITMAP(Bitmap: HBITMAP; hPalette: HPALETTE; options: WICBitmapAlphaChannelOption;
      out ppIBitmap: IWICBitmap): HResult; stdcall;
    function CreateBitmapFromHICON(Icon: HICON; out ppIBitmap: IWICBitmap): HResult; stdcall;
    function CreateComponentEnumerator(componentTypes, options: DWORD; out ppIEnumUnknown: IEnumUnknown): HResult; stdcall;
    function CreateFastMetadataEncoderFromDecoder(pIDecoder: IWICBitmapDecoder;
      out ppIFastEncoder: IWICFastMetadataEncoder): HResult; stdcall;
    function CreateFastMetadataEncoderFromFrameDecode(pIFrameDecoder: IWICBitmapFrameDecode;
      out ppIFastEncoder: IWICFastMetadataEncoder): HResult; stdcall;
    function CreateQueryWriter(const guidMetadataFormat, pguidVendor: TGUID; out ppIQueryWriter: IWICMetadataQueryWriter): HResult; stdcall;
    function CreateQueryWriterFromReader(pIQueryReader: IWICMetadataQueryReader; const pguidVendor: TGUID;
      out ppIQueryWriter: IWICMetadataQueryWriter): HResult; stdcall;
  end;

{ IWICDevelopRawNotificationCallback }

  IWICDevelopRawNotificationCallback = interface(IUnknown)
  ['{95C75A6E-3E8C-4EC2-85A8-AEBCC551E59B}']
    function Notify(NotificationMask: UINT): HResult; stdcall;
  end;

{ IWICDevelopRaw }

  IWICDevelopRaw = interface(IWICBitmapFrameDecode)
  ['{FBEC5E44-F7BE-4B65-B7F8-C0C81FEF026D}']
    function QueryRawCapabilitiesInfo(out pInfo: WICRawCapabilitiesInfo): HResult; stdcall;
    function LoadParameterSet(ParameterSet: WICRawParameterSet): HResult; stdcall;
    function GetCurrentParameterSet(out ppCurrentParameterSet: IPropertyBag2): HResult; stdcall;
    function SetExposureCompensation(ev: Double): HResult; stdcall;
    function GetExposureCompensation(out pEV: Double): HResult; stdcall;
    function SetWhitePointRGB(Red, Green, Blue: UINT): HResult; stdcall;
    function GetWhitePointRGB(out Red, Green, Blue: UINT): HResult; stdcall;
    function SetNamedWhitePoint(WhitePoint: WICNamedWhitePoint): HResult; stdcall;
    function GetNamedWhitePoint(out WhitePoint: WICNamedWhitePoint): HResult; stdcall;
    function SetWhitePointKelvin(WhitePointKelvin: UINT): HResult; stdcall;
    function GetWhitePointKelvin(out pWhitePointKelvin: UINT): HResult; stdcall;
    function GetKelvinRangeInfo(out pMinKelvinTemp, pMaxKelvinTemp, pKelvinTempStepValue: UINT): HResult; stdcall;
    function SetContrast(Contrast: Double): HResult; stdcall;
    function GetContrast(out pContrast: Double): HResult; stdcall;
    function SetGamma(Gamma: Double): HResult; stdcall;
    function GetGamma(out pGamma: Double): HResult; stdcall;
    function SetSharpness(Sharpness: Double): HResult; stdcall;
    function GetSharpness(out pSharpness: Double): HResult; stdcall;
    function SetSaturation(Saturation: Double): HResult; stdcall;
    function GSetSaturation(out pSaturation: Double): HResult; stdcall;
    function SetTint(Tint: Double): HResult; stdcall;
    function GetTint(out pTint: Double): HResult; stdcall;
    function SetNoiseReduction(NoiseReduction: Double): HResult; stdcall;
    function GetNoiseReduction(out pNoiseReduction: Double): HResult; stdcall;
    function SetDestinationColorContext(pColorContext: IWICColorContext): HResult; stdcall;
    function SetToneCurve(cbToneCurveSize: UINT; pToneCurve: WICRawToneCurve): HResult; stdcall;
    function GetToneCurve(cbToneCurveBufferSize: UINT; var pToneCurve: WICRawToneCurve; out pcbActualToneCurveBufferSize: UINT): HResult; stdcall;
    function SetRotation(Rotation: Double): HResult; stdcall;
    function GetRotation(out pRotation: Double): HResult; stdcall;
    function SetRenderMode(RenderMode: WICRawRenderMode): HResult; stdcall;
    function GetRenderMode(out pRenderMode: WICRawRenderMode): HResult; stdcall;
    function SetNotificationCallback(pCallback: IWICDevelopRawNotificationCallback): HResult; stdcall;
  end;

const
  GUID_MetadataFormatUnknown: TGUID = '{A45E592F-9078-4A7C-ADB5-4EDC4FD61B1F}';
  GUID_MetadataFormatIfd: TGUID = '{537396C6-2D8A-4BB6-9BF8-2F0A8E2A3ADF}';
  GUID_MetadataFormatSubIfd: TGUID = '{58A2E128-2DB9-4E57-BB14-5177891ED331}';
  GUID_MetadataFormatExif: TGUID = '{1C3C4F9D-B84A-467D-9493-36CFBD59EA57}';
  GUID_MetadataFormatGps: TGUID = '{7134AB8A-9351-44AD-AF62-448DB6B502EC}';
  GUID_MetadataFormatInterop: TGUID = '{ED686F8E-681F-4C8B-BD41-A8ADDBF6B3FC}';
  GUID_MetadataFormatApp0: TGUID = '{79007028-268D-45d6-A3C2-354E6A504BC9}';
  GUID_MetadataFormatApp1: TGUID = '{8FD3DFC3-F951-492B-817F-69C2E6D9A5B0}';
  GUID_MetadataFormatApp13: TGUID = '{326556A2-F502-4354-9CC0-8E3F48EAF6B5}';
  GUID_MetadataFormatIPTC: TGUID = '{4FAB0914-E129-4087-A1D1-BC812D45A7B5}';
  GUID_MetadataFormatIRB: TGUID = '{16100D66-8570-4BB9-B92D-FDA4B23ECE67}';
  GUID_MetadataFormat8BIMIPTC: TGUID = '{0010568c-0852-4e6a-b191-5c33ac5b0430}';
  GUID_MetadataFormatXMP: TGUID = '{BB5ACC38-F216-4CEC-A6C5-5F6E739763A9}';
  GUID_MetadataFormatChunktEXt: TGUID = '{568d8936-c0a9-4923-905d-df2b38238fbc}';
  GUID_MetadataFormatXMPStruct: TGUID = '{22383CF1-ED17-4E2E-AF17-D85B8F6B30D0}';
  GUID_MetadataFormatXMPBag: TGUID = '{833CCA5F-DCB7-4516-806F-6596AB26DCE4}';
  GUID_MetadataFormatXMPAlt: TGUID = '{7B08A675-91AA-481B-A798-4DA94908613B}';
  CLSID_WICApp0MetadataWriter: TGUID = '{F3C633A2-46C8-498e-8FBB-CC6F721BBCDE}';
  CLSID_WICApp0MetadataReader: TGUID = '{43324B33-A78F-480f-9111-9638AACCC832}';
  CLSID_WICApp1MetadataWriter: TGUID = '{ee366069-1832-420f-b381-0479ad066f19}';
  CLSID_WICApp1MetadataReader: TGUID = '{dde33513-774e-4bcd-ae79-02f4adfe62fc}';
  CLSID_WICApp13MetadataWriter: TGUID = '{7B19A919-A9D6-49E5-BD45-02C34E4E4CD5}';
  CLSID_WICApp13MetadataReader: TGUID = '{AA7E3C50-864C-4604-BC04-8B0B76E637F6}';
  CLSID_WICIfdMetadataWriter: TGUID = '{b1ebfc28-c9bd-47a2-8d33-b948769777a7}';
  CLSID_WICSubIfdMetadataReader: TGUID = '{50D42F09-ECD1-4B41-B65D-DA1FDAA75663}';
  CLSID_WICSubIfdMetadataWriter: TGUID = '{8ADE5386-8E9B-4F4C-ACF2-F0008706B238}';
  CLSID_WICExifMetadataReader: TGUID = '{d9403860-297f-4a49-bf9b-77898150a442}';
  CLSID_WICExifMetadataWriter: TGUID = '{c9a14cda-c339-460b-9078-d4debcfabe91}';
  CLSID_WICGpsMetadataReader: TGUID = '{3697790B-223B-484E-9925-C4869218F17A}';
  CLSID_WICGpsMetadataWriter: TGUID = '{CB8C13E4-62B5-4C96-A48B-6BA6ACE39C76}';
  CLSID_WICInteropMetadataReader: TGUID = '{B5C8B898-0074-459F-B700-860D4651EA14}';
  CLSID_WICInteropMetadataWriter: TGUID = '{122EC645-CD7E-44D8-B186-2C8C20C3B50F}';
  CLSID_WICThumbnailMetadataWriter: TGUID = '{d049b20c-5dd0-44fe-b0b3-8f92c8e6d080}';
  CLSID_WICIPTCMetadataWriter: TGUID = '{1249b20c-5dd0-44fe-b0b3-8f92c8e6d080}';
  CLSID_WICIRBMetadataReader: TGUID = '{D4DCD3D7-B4C2-47D9-A6BF-B89BA396A4A3}';
  CLSID_WICIRBMetadataWriter: TGUID = '{5C5C1935-0235-4434-80BC-251BC1EC39C6}';
  CLSID_WIC8BIMIPTCMetadataReader: TGUID = '{0010668c-0801-4da6-a4a4-826522b6d28f}';
  CLSID_WIC8BIMIPTCMetadataWriter: TGUID = '{00108226-ee41-44a2-9e9c-4be4d5b1d2cd}';
  CLSID_WICPngTextMetadataReader: TGUID = '{4b59afcc-b8c3-408a-b670-89e5fab6fda7}';
  CLSID_WICXMPMetadataReader: TGUID = '{72B624DF-AE11-4948-A65C-351EB0829419}';
  CLSID_WICXMPMetadataWriter: TGUID = '{1765E14E-1BD4-462E-B6B1-590BF1262AC6}';
  CLSID_WICXMPStructMetadataReader: TGUID = '{01B90D9A-8209-47F7-9C52-E1244BF50CED}';
  CLSID_WICXMPStructMetadataWriter: TGUID = '{22C21F93-7DDB-411C-9B17-C5B7BD064ABC}';
  CLSID_WICXMPBagMetadataReader: TGUID = '{E7E79A30-4F2C-4FAB-8D00-394F2D6BBEBE}';
  CLSID_WICXMPBagMetadataWriter: TGUID = '{ED822C8C-D6BE-4301-A631-0E1416BAD28F}';
  CLSID_WICXMPSeqMetadataReader: TGUID = '{7F12E753-FC71-43D7-A51D-92F35977ABB5}';
  CLSID_WICXMPSeqMetadataWriter: TGUID = '{6D68D1DE-D432-4B0F-923A-091183A9BDA7}';
  CLSID_WICXMPAltMetadataReader: TGUID = '{AA94DCC2-B8B0-4898-B835-000AABD74393}';
  CLSID_WICXMPAltMetadataWriter: TGUID = '{076C2A6C-F78F-4C46-A723-3583E70876EA}';
  IID_IWICMetadataBlockReader: TGUID = '{FEAA2A8D-B3F3-43E4-B25C-D1DE990A1AE1}';
  IID_IWICMetadataBlockWriter: TGUID = '{08FB9676-B444-41E8-8DBE-6A53A542BFF1}';
  IID_IWICMetadataReader: TGUID = '{9204FE99-D8FC-4FD5-A001-9536B067A899}';
  IID_IWICMetadataWriter: TGUID = '{F7836E16-3BE0-470B-86BB-160D0AECD7DE}';
  IID_IWICStreamProvider: TGUID = '{449494BC-B468-4927-96D7-BA90D31AB505}';
  IID_IWICPersistStream: TGUID = '{00675040-6908-45F8-86A3-49C7DFD6D9AD}';
  IID_IWICMetadataHandlerInfo: TGUID = '{ABA958BF-C672-44D1-8D61-CE6DF2E682C2}';
  IID_IWICMetadataReaderInfo: TGUID = '{EEBF1F5B-07C1-4447-A3AB-22ACAF78A804}';
  IID_IWICMetadataWriterInfo: TGUID = '{B22E3FBA-3925-4323-B5C1-9EBFC430F236}';
  IID_IWICComponentFactory: TGUID = '{412D0C3A-9650-44FA-AF5B-DD2A06C8E8FB}';

type
  WICMetadataPattern = record
    Position: ULARGE_INTEGER;
    Length: ULONG;
    Pattern: PByte;
    Mask: PByte;
    DataOffset: ULARGE_INTEGER;
  end;

  WICMetadataHeader = record
    Position: ULARGE_INTEGER;
    Length: ULONG;
    Header: PByte;
    DataOffset: ULARGE_INTEGER;
  end;

  IWICMetadataBlockReader = interface;
  IWICMetadataBlockWriter = interface;
  IWICMetadataReader = interface;
  IWICMetadataWriter = interface;
  IWICStreamProvider = interface;
  IWICPersistStream = interface;
  IWICMetadataHandlerInfo = interface;
  IWICMetadataReaderInfo = interface;
  IWICMetadataWriterInfo = interface;
  IWICComponentFactory = interface;

{ IWICMetadataBlockReader }

  IWICMetadataBlockReader = interface(IUnknown)
  ['{FEAA2A8D-B3F3-43E4-B25C-D1DE990A1AE1}']
    function GetContainerFormat(out pguidContainerFormat: TGUID): HResult; stdcall;
    function GetCount(out pcCount: UINT): HResult; stdcall;
    function GetReaderByIndex(nIndex: UINT; out ppIMetadataReader: IWICMetadataReader): HResult; stdcall;
    function GetEnumerator(out ppIEnumMetadata: IEnumerator): HResult; stdcall;
  end;

{ IWICMetadataBlockWriter }

  IWICMetadataBlockWriter = interface(IWICMetadataBlockReader)
  ['{08FB9676-B444-41E8-8DBE-6A53A542BFF1}']
    function InitializeFromBlockReader(out pIMDBlockReader: IWICMetadataBlockReader): HResult; stdcall;
    function GetWriterByIndex(nIndex: UINT; out ppIMetadataWriter: IWICMetadataWriter): HResult; stdcall;
    function AddWriter(pIMetadataWriter: IWICMetadataWriter): HResult; stdcall;
    function SetWriterByIndex(nIndex: UINT; pIMetadataWriter: IWICMetadataWriter): HResult; stdcall;
    function RemoveWriterByIndex(nIndex: UINT): HResult; stdcall;
  end;

{ IWICMetadataReader }

  IWICMetadataReader = interface(IUnknown)
  ['{9204FE99-D8FC-4FD5-A001-9536B067A899}']
    function GetMetadataFormat(out pguidMetadataFormat: TGUID): HResult; stdcall;
    function GetMetadataHandlerInfo(out ppIHandler: IWICMetadataHandlerInfo): HResult; stdcall;
    function GetCount(out pcCount: UINT): HResult; stdcall;
    function GetValueByIndex(nIndex: UINT; var pvarSchema, pvarId, pvarValue: Variant): HResult; stdcall;
    function GetValue(pvarSchema, pvarId: Variant; var pvarValue: Variant): HResult; stdcall;
    function GetEnumerator(out ppIEnumMetadata: IWICEnumMetadataItem): HResult; stdcall;
  end;

{ IWICMetadataWriter }

  IWICMetadataWriter = interface(IWICMetadataReader)
  ['{F7836E16-3BE0-470B-86BB-160D0AECD7DE}']
    function SetValue(pvarSchema, pvarId, pvarValue: Variant): HResult; stdcall;
    function SetValueByIndex(nIndex: UINT; pvarSchema, pvarId, pvarValue: Variant): HResult; stdcall;
    function RemoveValue(pvarSchema, pvarId: Variant): HResult; stdcall;
    function RemoveValueByIndex(nIndex: UINT): HResult; stdcall;
  end;

{ IWICStreamProvider }

  IWICStreamProvider = interface(IUnknown)
  ['{449494BC-B468-4927-96D7-BA90D31AB505}']
    function GetStream(out ppIStream: IStream): HResult; stdcall;
    function GetPersistOptions(out pdwPersistOptions: DWORD): HResult; stdcall;
    function GetPreferredVendorGUID(out pguidPreferredVendor: TGUID): HResult; stdcall;
    function RefreshStream: HResult; stdcall;
  end;

{ IWICPersistStream }

  IWICPersistStream = interface(IPersistStream)
  ['{00675040-6908-45F8-86A3-49C7DFD6D9AD}']
    function LoadEx(pIStream: IStream; pguidPreferredVendor: TGUID; dwPersistOptions: DWORD): HResult; stdcall;
    function SaveEx(pIStream: IStream; dwPersistOptions: DWORD; fClearDirty: boolean): HResult; stdcall;
  end;

{ IWICMetadataHandlerInfo }

  IWICMetadataHandlerInfo = interface(IWICComponentInfo)
  ['{ABA958BF-C672-44D1-8D61-CE6DF2E682C2}']
    function GetMetadataFormat(out pguidMetadataFormat: TGUID): HResult; stdcall;
    function GetContainerFormats(cContainerFormats: UINT; var pguidContainerFormats: TGUID; out pcchActual: UINT): HResult; stdcall;
    function GetDeviceManufacturer(cchDeviceManufacturer: UINT; var wzDeviceManufacturer: PWideChar; out pcchActual: UINT): HResult; stdcall;
    function GetDeviceModels(cchDeviceModels: UINT; var wzDeviceModels: PWideChar; out pcchActual: UINT): HResult; stdcall;
    function DoesRequireFullStreamout(pfRequiresFullStream: boolean): HResult; stdcall;
    function DoesSupportPadding(pfSupportsPadding: boolean): HResult; stdcall;
    function DoesRequireFixedSize(pfFixedSize: boolean): HResult; stdcall;
  end;

{ IWICMetadataReaderInfo }

  IWICMetadataReaderInfo = interface(IWICMetadataHandlerInfo)
  ['{EEBF1F5B-07C1-4447-A3AB-22ACAF78A804}']
    function GetPatterns(guidContainerFormat: TGUID; cbSize: UINT; out pPattern: WICMetadataPattern; out pcCount, pcbActual: UINT): HResult; stdcall;
    function MatchesPattern(guidContainerFormat: TGUID; pIStream: IStream; out pfMatches: boolean): HResult; stdcall;
    function CreateInstance(out ppIReader: IWICMetadataReader): HResult; stdcall;
  end;

{ IWICMetadataWriterInfo }

  IWICMetadataWriterInfo = interface(IWICMetadataHandlerInfo)
  ['{B22E3FBA-3925-4323-B5C1-9EBFC430F236}']
    function GetHeader(guidContainerFormat: TGUID; cbSize: UINT; out pHeader: WICMetadataHeader; out pcbActual: UINT): HResult; stdcall;
    function CreateInstance(out ppIWriter: IWICMetadataWriter): HResult; stdcall;
  end;

{ IWICComponentFactory }

  IWICComponentFactory = interface(IWICImagingFactory)
  ['{412D0C3A-9650-44FA-AF5B-DD2A06C8E8FB}']
    function CreateMetadataReader(guidMetadataFormat: TGUID; pguidVendor: PGUID; dwOptions: DWORD; pIStream: IStream;
      out ppIReader: IWICMetadataReader): HResult; stdcall;
    function CreateMetadataReaderFromContainer(guidContainerFormat: TGUID; pguidVendor: PGUID; dwOptions: DWORD; pIStream: IStream;
      out ppIReader: IWICMetadataReader): HResult; stdcall;
    function CreateMetadataWriter(guidMetadataFormat: TGUID; pguidVendor: PGUID; dwMetadataOptions: DWORD;
      out ppIWriter: IWICMetadataWriter): HResult; stdcall;
    function CreateMetadataWriterFromReader(pIReader: IWICMetadataReader; pguidVendor: PGUID;
      out ppIWriter: IWICMetadataWriter): HResult; stdcall;
    function CreateQueryReaderFromBlockReader(pIBlockReader: IWICMetadataBlockReader;
      out ppIQueryReader: IWICMetadataQueryReader): HResult; stdcall;
    function CreateQueryWriterFromBlockWriter(pIBlockWriter: IWICMetadataBlockWriter;
      out ppIQueryWriter: IWICMetadataQueryWriter): HResult; stdcall;
    function CreateEncoderPropertyBag(ppropOptions: TPROPBAG2; cCount: UINT; out ppIPropertyBag: IPropertyBag2): HResult; stdcall;
  end;

var
  WICMatchMetadataContent: function(guidContainerFormat: TGUID;
    pguidVendor: PGUID; pIStream: IStream; out pguidMetadataFormat: TGUID): HResult; stdcall;
  WICSerializeMetadataContent: function(guidContainerFormat: TGUID;
    pIWriter: IWICMetadataWriter; dwPersistOptions: DWORD; pIStream: IStream): HResult; stdcall;
  WICGetMetadataContentSize: function(guidContainerFormat: TGUID;
    pIWriter: IWICMetadataWriter; out pcbSize: ULARGE_INTEGER): HResult; stdcall;
  WICConvertBitmapSource: function(const dstFormat: REFWICPixelFormatGUID;
    pISrc: IWICBitmapSource; ppIDst: IWICBitmapSource): HResult; stdcall;
  WICCreateBitmapFromSection: function(width, height: UINT;
    const pixelFormat: REFWICPixelFormatGUID; hSection: THANDLE; stride, offset: UINT;
    out ppIBitmap: IWICBitmap): HResult; stdcall;
  WICMapGuidToShortName: function(guid: TGUID; cchName: UINT; var wzName: PWideChar;
    out pcchActual: UINT): HResult; stdcall;
  WICMapShortNameToGuid: function(wzName: PWideChar; out pguid: TGUID): HResult; stdcall;
  WICMapSchemaToName: function(guidMetadataFormat: TGUID; pwzSchema: LPWSTR;
    cchName: UINT; var wzName: PWideChar; out pcchActual: UINT): HResult; stdcall;

function WinCodecsInit(ThrowExceptions: Boolean = False): Boolean;
{$endif}

implementation

{$ifdef windows}
var
  Loaded: Boolean;
  Initialized: Boolean;
  FailedModuleName: string;
  FailedProcName: string;

function WinCodecsInit(ThrowExceptions: Boolean = False): Boolean;
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

const
  windowscodecs = 'windowscodecs.dll';
begin
  ThrowExceptions := ThrowExceptions and (@LibraryGetProc <> nil);
  if Loaded then
  begin
    CheckExceptions;
    Exit(Initialized);
  end;
  Loaded := True;
  if Initialized then
    Exit(True);
  Result := False;
  FailedModuleName := windowscodecs;
  FailedProcName := '';
  Module := LibraryLoad(FailedModuleName);
  if Module = ModuleNil then
  begin
    CheckExceptions;
    Exit;
  end;
  Result :=
    TryLoad('WICMatchMetadataContent', @WICMatchMetadataContent) and
    TryLoad('WICSerializeMetadataContent', @WICSerializeMetadataContent) and
    TryLoad('WICGetMetadataContentSize', @WICGetMetadataContentSize) and
    TryLoad('WICConvertBitmapSource', @WICConvertBitmapSource) and
    TryLoad('WICCreateBitmapFromSection', @WICCreateBitmapFromSection) and
    TryLoad('WICMapGuidToShortName', @WICMapGuidToShortName) and
    TryLoad('WICMapShortNameToGuid', @WICMapShortNameToGuid) and
    TryLoad('WICMapSchemaToName', @WICMapSchemaToName);
  if not Result then
    Exit;
  FailedModuleName := '';
  FailedProcName := '';;
  Initialized := True;
end;
{$endif}

end.

