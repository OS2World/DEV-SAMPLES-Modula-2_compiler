/* testing an M2-built REXX DLL */

SAY 'testing an M2-built DLL, task started...'

CALL RxFuncAdd 'RxDelay',    'MYRXDLL', 'RXDELAY'
CALL RxFuncAdd 'RxClipRange','MYRXDLL', 'RXCLIPRANGE'
CALL RxFuncAdd 'RxUpperCase','MYRXDLL', 'RXUPPERCASE'

say 'Starting "CALL RxDelay 1000"'
CALL RxDelay 1000

say "RxClipRange( -135, -100, 0 ) =" RxClipRange(-135,-100,0)
say "RxClipRange(  -33, -100, 0 ) =" RxClipRange( -33,-100,0)
say "RxClipRange(   25, -100, 0 ) =" RxClipRange(  25,-100,0)

say 'RxUpperCase( "Hello" ) =' RxUpperCase( "Hello" )

CALL RxFuncDrop 'RxDelay'
CALL RxFuncDrop 'RxClipRange'
CALL RxFuncDrop 'RxUpperCase'

EXIT
