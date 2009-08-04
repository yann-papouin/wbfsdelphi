mkdir lib

impdef.exe -a libeay32_bcc.def libeay32.dll
implib.exe -a lib\libeay32_bcc.lib libeay32_bcc.def

impdef.exe -a libssl32_bcc.def libssl32.dll
implib.exe -a lib\libssl32_bcc.lib libssl32_bcc.def
pause