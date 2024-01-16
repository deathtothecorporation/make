/-  *vapor
/+  verb, dbug, default-agent, nav=naive, v=vapor
::
|%
+$  wares  (link deed pack)
+$  masks  (map path slip)
::
+$  versioned-state  $%(state-0)
::
+$  state-0  [%0 =wares =masks]
::
+$  card  card:agent:gall
--
::
%+  verb  &
%-  agent:dbug
=|  state-0
=*  state  -
::
^-  agent:gall
::
=<
  |_  =bowl:gall
  +*  this  .
      def  ~(. (default-agent this %|) bowl)
      eng   ~(. +> [bowl ~])
  ++  on-init
    ^-  (quip card _this)
    ~>  %bout.[0 '%vapor-ware +on-init']
    =^  cards  state  abet:init:eng
    [cards this]
  ::
  ++  on-save
    ^-  vase
    ~>  %bout.[0 '%vapor-ware +on-save']
    !>(state)
  ::
  ++  on-load
    |=  ole=vase
    ~>  %bout.[0 '%vapor-ware +on-load']
    ^-  (quip card _this)
    =^  cards  state  abet:(load:eng ole)
    [cards this]
  ::
  ++  on-poke
    |=  cag=cage
    ~>  %bout.[0 '%vapor-ware +on-poke']
    ^-  (quip card _this)
    =^  cards  state  abet:(poke:eng cag)
    [cards this]
  ::
  ++  on-peek
    |=  pat=path
    ~>  %bout.[0 '%vapor-ware +on-peek']
    ^-  (unit (unit cage))
    (peek:eng (pave pat))
  ::
  ++  on-agent
    |=  [wir=wire sig=sign:agent:gall]
    ~>  %bout.[0 '%vapor-ware +on-agent']
    ^-  (quip card _this)
    =^  cards  state  abet:(dude:eng (pave wir) sig)
    [cards this]
  ::
  ++  on-arvo
    |=  [wir=wire sig=sign-arvo]
    ~>  %bout.[0 '%vapor-ware +on-arvo']
    ^-  (quip card _this)
    =^  cards  state  abet:(arvo:eng wir sig)
    [cards this]
  ::
  ++  on-watch
    |=  pat=path
    ~>  %bout.[0 '%vapor-ware +on-watch']
    ^-  (quip card _this)
    =^  cards  state  abet:(peer:eng pat)
    [cards this]
  ::
  ++  on-fail
    ~>  %bout.[0 '%vapor-ware +on-fail']
    on-fail:def
  ::
  ++  on-leave
    ~>  %bout.[0 '%vapor-ware +on-leave']
    on-leave:def
  --
|_  [bol=bowl:gall dek=(list card)]
+*  dat  .
    paz  /(scot %p our.bol)/azimuth/(scot %da now.bol)
++  emit  |=(=card dat(dek [card dek]))
++  emil  |=(lac=(list card) dat(dek (welp lac dek)))
++  show  |=(cag=cage (emit %give %fact [/web-ui]~ cag))
++  mine
  .^  (unit point:nav)
    %gx
    (welp paz /point/(scot %p our.bol)/noun)
  ==
::
++  weve
  |=  [pat=path ted=@tas cag=cage]
  ^+(dat (emit %pass pat %arvo %k %fard %vapor ted cag))
::
++  subs
  |=  [=oath =deed =ship]
  ^+  dat
  =/  =path  /vapor/(scot %ux oath)/(scot %ux deed)
  %-  emit
  [%pass [(scot %p ship) path] %agent [ship %make] %watch path]
::
++  bind
  |=  =path
  ^+  dat
  =-  (emit %pass /eyre/connect %arvo %e -)
  [%connect [[~ (welp /apps/vapor/ware path)] dap.bol]]
::
++  abet
  ^-  (quip card _state)
  [(flop dek) state]
::
++  init
  ^+  dat
  dat
::
++  load
  |=  vaz=vase
  ^+  dat
  ?>  ?=([%0 *] q.vaz)
  dat(state !<(state-0 vaz))
::
++  arvo
  |=  [=pith sig=sign-arvo]
  ^+  dat
  ?+    pith  ~|(vapor-make-panic-bad-arvo/[pith sig] !!)
      [%stuf ~]
    ~_  'VAPOR: OpenSea data gathering failure.'
    ?>  ?=([%khan %arow *] sig)
    ?.  ?=(%& -.p.+.sig)
      %.  dat
      (slog 'VAPOR: OpenSea data thread failed.' tang.p.p.+.sig)
    (show vapor-make-update++.p.p.+.sig)
  ::
      [%eyre %connect ~]
    ~_  'VAPOR: ware error, failure to bind upload.'
    ?>(?=([%eyre %bound %& *] sig) dat)
  ::
  ==
::
++  peer
  |=  =pith
  ^+  dat
  ?+    pith  ~|(vapor-wear-panic-bad-watch/pith !!)
    [%http-response *]  dat
  ::
      [%web-ui ~]
    ?>  =(our.bol src.bol)
    =/  mine=(unit point:nav)  mine
    =.  dat
      ?~  mine  dat
      %^  weve  /stuf  %vapor-get-mine
      noun+!>(`(unit addy)``address.owner.own.u.mine)
    =~  (show vapor-ware-wares+!>(`_wares`wares))
        (show vapor-ware-masks+!>(`_masks`masks))
    ==
  ==
::
++  peek
  |=  =pith
  ^-  (unit (unit cage))
  ?+    pith  !!
    ::  XX: include scry that checks wex
      [%x %wears ~]
    ``vapor-ware-wares+!>(`_wares`wares)
      [%x %wears [%ux bo=@ux] [%ux it=@ux] ~]
      =,  pith
    ``vapor-ware-ware+!>((~(got bi wares) bo it))
      [%x %masks ~]
    ``vapor-ware-masks+!>(`_masks`masks)
      [%x %owned [%ux bo=@ux] [%ux it=@ux] ~]
    =,  pith
    =+  ware=(~(got bi wares) bo it)
    =/  wire=path
      /(scot %p host.ware)/(scot %ux bo)/(scot %ux it)
    =-  ``vapor-ware-owns+!>([bo it -])
    (~(has by wex.bol) wire host.ware %make)
      [%x %dbug %state ~]
    =-  ``noun+!>(-)
    state
    :: %_    state
    ::     wares
    ::   %-  ~(run by wares)
    ::   |=  m=(map deed pack)
    ::   %-  ~(run by m)
    ::   |=  p=pack
    ::   %_    p
    ::       mars
    ::     (~(run by mars.p) |=(mi=mime mi(q.q 1.337)))
    ::       state
    ::     ?~(state.d `s/'EMPTY' `s/'STATE-HELD')
    ::   ==
    :: ==
  ==
::
++  dude
  |=  [=pith sig=sign:agent:gall]
  ^+  dat
  ?+    pith  ~|(vapor-ware-panic-bad-dude/[pith sig] !!)
      [%state ~]
    ~_  'VAPOR: ware error, ware host ignored state'
    ?>  ?=(%poke-ack -.sig)
    ?~  p.sig  dat
    ~|(vapor-ware-panic-state-save/u.p.sig !!)
      [[%p host=@p] %vapor [%ux bo=@ux] [%ux it=@ux] ~]
    =,  pith
    ~_  'VAPOR: ware error, ware host watch failure'
    ?+    -.sig  ~|(vapor-ware-panic-bad-sig/[pith sig] !!)
      %kick  (subs bo it host)
    ::
        %watch-ack
      %.  dat
      ?~  p.sig  same
    ::  XX: delete vapor here
    (slog 'VAPOR: ware host watch failure' u.p.sig)
    ::
        %fact
      ?+    p.cage.sig  ~|(ware-panic-bad-cage/[pith sig] !!)
          %vapor-mars
        =+  hav=(~(got bi wares) bo it)
        =+  !<(mars=_mars.hav q.cage.sig)
        dat(wares (~(put bi wares) bo it hav(mars mars)))
      ::
          %vapor-make-pack
        =+  !<(=pack q.cage.sig)
        =.  wares.dat
          (~(put bi wares) bo it pack)
        dat
      ::
          %vapor-ware-update
        =+  !<(upd=updates:ware q.cage.sig)
        ?.  ?=(%get-stat -.upd)  !!
        =+  hav=(~(got bi wares) bo it)
        =+  ;;(mars=_mars.hav p.upd)
        dat(wares (~(put bi wares) bo it hav(mars mars)))
      ==
    ==
  ==
::
++  poke
  |=  [mar=mark vaz=vase]
  ^+  dat
  ?+    mar  ~|(vapor-ware-panic-bad-mark/mar !!)
      %handle-http-request
    =+  !<([id=@ta req=inbound-request:eyre] vaz)
    =+  we=(we-abed:webs id req)
    =^  cards  state  we-abet:we-work:(we-abed:webs id req)
    (emil cards)
  ::
      %vapor-ware-arks
    =+  !<([=oath =deed =pack] vaz)
    =/  pace=(axal tech)  (~(got by mars.pack) [oath deed])
    =/  move=(axal tech)  (~(dip of pace) /land)
    =+  dis=~(tap in ~(key by dir.move))
    ::  render path
    =+  ^=  rend
      |-  ^-  (list @t)
      ?~  dis  ~
      [i.dis $(dis t.dis)]
    =-  (subs:(bind:(show(state -) mar vaz) rend) oath deed src.bol)
    %=    state
        masks
      ?~  rend  masks
      (~(put by masks) rend [oath deed])
    ::
        wares
      (~(put bi wares) [oath deed [urth.pack mars.pack src.bol]])
    ==
  ::
      %vapor-ware-action
    =+  act=!<(actions:ware vaz)
    ?>  ?=(%get-stuf -.act)
    =/  mine=(unit point:nav)  mine
    ?~  mine  dat
    %^  weve  /stuf  %vapor-get-mine
    noun+!>(`(unit addy)``address.owner.own.u.mine)
  ==
::  +webs: handles handle-http-requests
++  webs
  |_  $:  eid=@ta
          caz=(list card)
          inb=inbound-request:eyre
          pay=(unit simple-payload:http)
          mas=mars
      ==
  +*  we  .
  ++  chop
    |=  p=path
    (crip (rash (spat p) ;~(pfix fas (star prn))))
  ++  we-emit  |=(c=card we(caz [c caz]))
  ++  we-emil  |=(lac=(list card) we(caz (welp lac caz)))
  ++  we-abed
    |=([id=@ta ib=inbound-request:eyre] we(eid id, inb ib))
  ++  we-abet
    ^-  (quip card _state)
    :_  state
    %-  flop  %+  welp  caz
    (give-simple-payload:app:ser eid (need pay))
  ++  we-send
    |=  [=ship =oath =deed j=(unit json)]
    =-  (we-emit %pass /state %agent [ship %vapor-make] %poke -)
    vapor-make-update+!>(`updates:make`new-stat+[[oath deed] j])
  ++  we-fail  we(pay `[404+~ ~])
  ++  we-dont  we(pay `[500+~ ~])
  ++  we-work
    ^+  we
    ?.  authenticated.inb
      %=    we
          pay
        =-  `[[302 [location+-]~] ~]
        (rap 3 '/~/login?redirect=' '.' url.request.inb ~)
      ==
    =*  headers  header-list.request.inb
    =/  reqline  (parse-request-line:ser url.request.inb)
    =/  =pith  (pave site.reqline)
    ?>  ?=([%apps %vapor %ware [%p host=@p] [%ux oath=@ux] [%ux deed=@ux] rest=*] pith)
    =,  pith
    =/  [=^oath =^deed]
      (~(got by masks) (pout [[%p host] [%ux oath] [%ux deed] ~]))
    =/  =slip  [oath deed]
    =/  rest=path  (pout rest)
    =/  =pack  (~(got bi wares) oath deed)
    =/  pace=(axal tech)  (~(got by mars.pack) slip)
    ?+    method.request.inb  we-dont
        %'POST'
      =+  head=(malt headers)
      =+  type=(~(got by head) 'content-type')
      ?.  =('application/json' type)  we-dont
      ?~  body.request.inb            we-dont
      ?~  jon=(de-json:html q.u.body.request.inb)  we-dont
      =/  =tech  (need (~(get of pace) /land/mime/index))
      ?>  ?=(%land -.tech)
      =/  =mime  (fall q.+.tech *mime)
      =/  ur=(urth json)
        :_  %local
        [%& u.jon]
      %.  [host oath deed jon]
      %=    we-send
        wares  (~(put bi wares) oath deed pack(urth ur))
      ::
          pay
        `[200^[content-type+(chop p.mime)]~ `q.mime]
      ==
    ::
        %'GET'
      ?+    rest
          ?~  tech=(~(get of pace) rest)  we-fail
          ?.  ?=(%land -.u.tech)  we-fail
          ?~  q.+.u.tech  we-fail
          =/  =mime  (need q.+.u.tech)
          we(pay `[200^[content-type+(chop p.mime)]~ `q.mime])
      ::
          ~
        we(pay `[301^[location+(cat 3 url.request.inb '/')]~ ~])
      ::
          [%$ ~]
        ?~  tech=(~(get of pace) /index)  we-fail
        ?.  ?=(%land -.u.tech)  we-fail
        ?~  q.+.u.tech  we-fail
        =/  =mime  (need q.+.u.tech)
        we(pay `[200^[content-type+(chop p.mime)]~ `q.mime])
      ::
          [%state ~]
        %=    we
            pay
          =-  `[200^[content-type+'application/json']~ `-]
          %-  json-to-octs:ser
          ?-  -.p.urth.pack
            %|  *json
            %&  p.p.urth.pack
          ==
        ==
      ==
    ==
  --
--
