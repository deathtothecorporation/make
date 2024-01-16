  /-  *vapor
  |%
++  tape-to-ux
  |=  t=tape
  (scan t zero-ux)
::
++  zero-ux
  ;~(pfix (jest '0x') hex)
::  custom pave
++  pave
  |=  =path
  ^-  pith
  %+  turn  path
  |=  i=@ta
  (fall (rush i spot:stip) ;;(@tas i))
::    +lane
::
::  wire with slip path component
::
::  .port: identifier
++  lane
  |=  [port=$@(@tas path) slip=(unit slip)]
  ^-  path
  ?~  slip  (welp /make ?@(port /(scot %tas port) port))
  =+  [oath=p deed=q]:u.slip
  %+  welp
    ?@  port  /make/(scot %tas port)
    (welp /make port)
  ?~  deed  /(scot %ux oath)
  /(scot %ux oath)/(scot %ux u.deed)
--
