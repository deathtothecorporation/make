/-  vap=vapor, spider
/+  *strandio, tio=ethio
=,  strand=strand:spider
=>
  ::
  |%
  ++  mine
    |=  a=addy:vap      
    ;:  welp
      "https://api.opensea.io/api/v1/assets?"
      "format=json&owner="
      (weld "0x" ((x-co:co 32) a))
    ==
  --
::
^-  thread:spider
|=  vaz=vase
=/  m  (strand ,vase)
^-  form:m
::
=/  have=(unit a=addy:vap)
  !<((unit addy:vap) vaz)
=+  fail=(pure:m !>(get-stuf+~))
?~  have  fail
::
;<  oep=json  bind:m  (fetch-json (mine u.have))
?.  ?=(%o -.oep)  fail
(pure:m !>(get-stuf+`oep))
  
