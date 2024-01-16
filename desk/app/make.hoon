/-  *vapor, chat, scan=scanner
/+  w=work, dbug, default-agent, v=vapor, c=vapor-chain
::
|%
+$  pipe  $+(pipe card:agent:gall)
+$  card  $+(agent-card ^card)
+$  bowl  $+(bowl bowl:gall)
+$  app-state  $+(app-state versioned-state)
--
::
%+  verb:w  |
%-  agent:dbug
=|  [app-state log=[verb=_& state=_| type=$+(state-type state-0)]]
=*  state  -.-
=*  log  log.-
::
^-  agent:gall
::
=<
  |_  =bowl
  ::
  +*  this  .
      def  ~(. (default-agent this %|) bowl)
      eng   ~(. +> [bowl ~])
  ::
  ++  on-init
    ^-  (quip card _this)
    ~>  %bout.[0 '%vapor-make +on-init']
    =^  cards  state  abet:init:eng
    [cards this]
  ::
  ++  on-save
    ^-  vase
    ~>  %bout.[0 '%vapor-make +on-save']
    !>(state)
  ::
  ++  on-load
    |=  ole=vase
    ~>  %bout.[0 '%vapor-make +on-load']
    ^-  (quip card _this)
    =^  cards  state  abet:(load:eng ole)
    [cards this]
  ::
  ++  on-poke
    |=  cag=cage
    ~>  %bout.[0 '%vapor-make +on-poke']
    ^-  (quip card _this)
    ~&  >>>  [%log-level log.this]
    =/  mar  p.cag
    =/  =vase  q.cag
    ?:  ?=(%work mar)
      ?-  !<(?(%loud %state) vase)
        %loud   `this(verb.log !verb.log)
        %state  `this(state.log !state.log)
      ==
    =^  cards  state  abet:(poke:eng cag)
    [cards this]
  ::
  ++  on-peek
    |=  pat=path
    ~>  %bout.[0 '%vapor-make +on-peek']
    ^-  (unit (unit cage))
    (peek:eng (pave:v pat))
  ::
  ++  on-agent
    |=  [wir=wire sig=sign:agent:gall]
    ~>  %bout.[0 '%vapor-make +on-agent']
    ^-  (quip card _this)
    =^  cards  state  abet:(dude:eng (pave:v wir) sig)
    ~&  >>  sending+cards
    [cards this]
  ::
  ++  on-arvo
    |=  [wir=wire sig=sign-arvo]
    ~>  %bout.[0 '%vapor-make +on-arvo']
    ^-  (quip card _this)
    =^  cards  state  abet:(arvo:eng (pave:v wir) sig)
    [cards this]
  ::
  ++  on-watch
    |=  pat=path
    ~>  %bout.[0 '%vapor-make +on-watch']
    ^-  (quip card _this)
    =^  cards  state  abet:(peer:eng (pave:v pat))
    [cards this]
  ::
  ++  on-fail
    ~>  %bout.[0 '%vapor-make +on-fail']
    on-fail:def
  ::
  ++  on-leave
    ~>  %bout.[0 '%vapor-make +on-leave']
    on-leave:def
  --
|_  [bol=bowl dek=$+(dek (list card))]
+*  cor  .
    paz   /(scot %p our.bol)/azimuth/(scot %da now.bol)
    bark  bark:w(loud verb.log, state-print state.log, app %make)
++  emit  |=(car=card cor(dek [car dek]))
++  emil
  |=  lac=_dek
  %-  (bark state |.("emil count: {<(lent (welp lac dek))>}"))
  cor(dek (welp lac dek))
++  give  |=(=gift:agent:gall (emit %give gift))
++  abet  ^-((quip card _state) [(flop dek) state])
::  +mine: our azimuth point
++  mine
  ^-  (unit point:nav)
  .^  (unit point:nav)
    %gx
    (welp paz /point/(scot %p our.bol)/noun)
  ==
::    +show
::
::  send state updates to the ui
::
::  .cag: the state to show
++  show
  |=  cag=cage
  ^+  cor
  (emit %give %fact [/web-ui]~ cag)
::    +give
::
::  give a fact on the wire associated with the .slip
::
::  .slip: the contract/deed pair
::  .cage: the data to give
++  give-slip
  |=  [=slip =cage]
  =-  (give %fact - cage)
  [(lane:v %slip `slip)]~
::    +wive
::
::  start an inline thread
::
::  .path: the path a response will come in on
::  .shed: the thread to start
++  wive
  |=  [pat=path =shed:khan]
  ^+(cor (emit %pass pat %arvo %k %lard q.byk.bol shed))
::  start a thread
::
::  .path: the path a response will come in on
::  .ted: the thread to start
::  .cag: the data to send
++  weve
  |=  [pat=path ted=@tas cag=cage]
  ^+(cor (emit %pass pat %arvo %k %fard %vapor ted cag))
::    +stat
::
::  communicate urth state to subs
::
::  .slip: the contract/deed pair
::  .urth: the earth state to send
++  stat
  |=  [=slip =(urth json)]
  (give-slip slip vapor-ware-update+!>(get-stat+urth))
::    +arks
::
::  tell %ware about new vapors
::
::  .arks: new vapor with contract/deed addresses
::  .pods: ships that own it
++  arks
  |=  [=^arks =pods]
  %+  turn  ~(tap in pods)
  |=  =ship
  =-  [%pass /new %agent [ship %ware] %poke -]
  vapor-ware-arks+!>(arks)
::
++  init
  ^+  cor
  =/  mine=(unit point:nav)  mine
  ?~  mine  cor
  ?:  =(0x0 address.owner.own.u.mine)  cor
  cor(you address.owner.own.u.mine)
::
++  load
  |=  vaz=vase
  ^+  cor
  ?>  ?=([%0 *] q.vaz)
  cor(state !<(state-0 vaz))
::
++  peek
  |=  =pith
  ^-  (unit (unit cage))
  %-  (bark state |.("peek: {<pith>}"))
  |^
  ::
    ?+    pith  ~|(bad-peek/[pith] !!)
  ::
      [%x %jam %vapors ~]
    ``noun+!>((jam vapors))
  ::
      [%x %vapors ~]
    ``noun+!>(vapors)
  ::
      [%x %vapors [%ux bo=@ux] ~]
    =,  pith
    ``vapor-make-vapor+!>((~(got bi vapors) bo ~))
  ::
      [%x %vapors [%ux bo=@ux] [%ux it=@ux] ~]
    =,  pith
    ``vapor-make-vapor+!>((~(got bi vapors) bo `it))
  ::
      [%x %owners ~]
    ``noun+!>(owners)
  ::
      [%x %owners [%ux bo=@ux] ~]
    =,  pith
    ``noun+!>((~(get by owners) bo))
  ::
      [%x %owners [%ux bo=@ux] [%ux it=@ux] ~]
    =,  pith
    ``noun+!>((~(get bi owners) bo `it))
  ::
      [%x %dbug %state ~]
    =-  ``noun+!>(-)
    state
  ::
      [%x %mars [%ux bo=@ux] [%ux it=@ux] iota=*]
    =,  pith
    =/  =pack  (~(got bi vapors) bo `it)
    =/  pace=(axal tech)  (~(got by mars.pack) [bo `it])
    ?~  iota
      =/  tour=(list path)  ~(tap in ~(key by ~(tar of pace)))
      ``noun+!>(tour)
    =/  path  (pout iota)
    =?  path  ?=(^ (mole |.(=(%noun (rear path)))))  (snip path)
    =+  fil=fil:(~(dip of pace) path)
    ?~  fil  [~ ~]
    ~&  >>  fil
    ``noun+!>(fil)
    ::
    [%y %mars ~]
      (arc (lab deeds))
  ::
    [%y %mars [%ux bo=@ux] [%ux it=@ux] rest=?(~ *)]
      =,  pith
      =/  =pack  (~(got bi vapors) bo `it)
      =/  pace=(axal tech)  (~(got by mars.pack) [bo `it])
      =/  path  (pout rest)
      =?  path  ?=(^ (mole |.(=(%noun (rear path)))))  (snip path)
      ~&  path
      =+  (~(dip of pace) path)
      ?^  fil  ``noun+!>(`arch`[``@uvi`(shax (jam fil)) ~])
      %-  arc
     ~(tap in ~(key by dir))
  ::
    [%y %mars [%ux bo=@ux] rest=?(~ *)]
      =,  pith
      =/  =pack  (~(got bi vapors) bo ~)
      =/  pace=(axal tech)  (~(got by mars.pack) [bo ~])
      =/  path  (pout rest)
      =?  path  ?=(^ (mole |.(=(%noun (rear path)))))  (snip path)
      =+  (~(dip of pace) (pout rest))
      ?^  fil  ``noun+!>(`arch`[``@uvi`(shax (jam fil)) ~])
      %-  arc
      %+  weld  (lit (hosted bo))
      ~(tap in ~(key by dir))
  ::
     [%y %mars %tree ~]  ``noun+!>(`tang`(zing `(list tang)`(lap deeds)))
    ==
  ++  arc  |=  l=(list @ta)  ``noun+!>(`arch`~^(malt (turn l (late ~))))
  ++  alx  |=  l=(list deed)
    =-  ~&  >>  -  ``noun+!>(-)
    ^-  arch
    :-  ~
    %-  malt   (turn (murn l same) (cork |=(@ux (scot %ux +<)) (late ~)))
  ++  alf  |=  f=?           ``noun+!>(f)
  ++  ask  |=  u=(unit ?)  ?^(u (alf u.u) [~ ~])
  ++  lab
    |=  s=(set oath)
     (turn (sit s) |=(@ux (scot %ux +<)))
  ++  lap
    |=  s=(set oath)
    %-  zing
    %+  turn  (sit s)
    |=  b=oath
    =+  [p=b q=(hosted b)]
    =-  (turn (sit q) -)
    |=  i=deed
    =-  (turn - smyt)
    %+  tax  [p i]
    (nab p i)
  ++  las
    |=  s=(set oath)
    %-  zing
    %+  turn  (sit s)
    |=  b=oath
    =+  [p=b q=(hosted b)]
    =-  (turn (sit q) -)
    |=  i=deed
    %-  lat
    %+  tax  [p i]
    (nab p i)
  ++  sit  |*(s=(set) ~(tap in s))
  ++  sin  |*(s=(set) ~(has in s))
  ++  mit  |*(m=(axal tech) (sit ~(tar of m)))
  ++  man  |*(m=(axal tech) ~(has of m))
  ++  mad  |*([m=(axal tech) f=$-(* ?)] (sy (skim (mit m) f)))
  ++  nab  ~(got bi vapors)
  ++  hot
    |=  s=(set oath)
    ^-  (list deed)
    %-  zing
    %+  turn  (sit s)
    |=  b=oath
    (sit (hosted b))
  ++  lit
    |=  l=(set deed)
    ^-  (list @ta)
    (turn (murn (sit l) same) |=(@ux (scot %ux +<)))
  ++  lat
    |=  p=(list path)
    ^-  (list @ta)
    (turn p |=(pax=path (crip "{<pax>} ")))
  ++  tec
    |=  t=tech
    ^-  (list @ta)
    ?-  -.t
      %desks  ~(tap in +.t)
      %perm   ~
      %pipe   ~
      %slab  ~
      %land   ~
    ==
  ++  tax
    |=  [s=slip p=pack]
    ?~  m=(~(get by mars.p) s)  ~
    =|  out=(list path)
    =/  pax=path
      ?~  q.s  /(scot %ux p.s)
      /(scot %ux p.s)/(scot %ux u.q.s)
    =|  fat=(axal tech)
    =.  fat  `(axal tech)`u.m
    |-  ^+   out
    =?  out  ?=(^ fil.fat)  (snoc out pax)
    =/  dir  ~(tap by dir.fat)
    |-  ^+   out
    ?~  dir  out
    %=  $
      dir  t.dir
      out  ^$(pax (weld pax /[p.i.dir]), fat q.i.dir)
    ==
  ::  mars
  ++  deeds  ~(key by vapors)
  ++  hosted  ~(key bi vapors)
  ++  tenants  ~(key bi owners)
  --
::    +peer
::
::  handles +on-watch data
++  peer
  |=  =pith
  ^+  cor
  ?+    pith  ~|(vapor-make-panic-bad-watch/pith !!)
  ::
      [%make label=@ta * ~]
    %.  cor
    (bark state |.("peer seen on path {<pith>} with label {<label.pith>}"))
  ::
    [%http-response *]  cor
  ::
      [%make [%ux oath=@ux] [%ux deed=@ux] ~]
    =,  pith
    =/  =pack  (~(got bi vapors) oath `deed)
    =/  =pods  (~(got bi owners) oath `deed)
    ?>  (~(has in pods) src.bol)
    (emit %give %fact ~ vapor-make-pack+!>(pack))
  ==
::    +poke
::  handles +on-poke data
++  poke
  |=  [mar=mark vaz=vase]
  ^+  cor
  ?+    mar  ~|(vapor-make-panic-bad-mark/mar !!)
      %noun
    =+  in=(mule |.(!<(actions:make vaz)))
    ?:  ?=(%& -.in)  (poke %vapor-make-action vaz)
    =+  (road |.(;;([%fake flog=(list tape) slip=(unit slip)] !<(* vaz))))
    =?  slip  ?=(~ slip)  `[0xbeef ~]
    ?>  ?=(^ slip)
    =+  [oath=p deed=q]:u.slip
    =?  vapors  =(0xbeef oath)
      (~(put bi vapors) oath deed *pack)
    =/  all=(list [difs=effects:hunt:c kill=state:hunt:c])
      =|  kill=state:hunt:c
      %+  spun  flog
      |=  [log=tape acc=(list [effects:hunt:c state:hunt:c])]
      =^  difs=effects:hunt:c  kill
      %+  process:hunt:c   kill(oath oath, id deed)
      (fake-log:c (need (de:json:html (crip log))))
      :-  [difs kill]
      [[difs kill] acc]
    =/  dat  boc
    =<  cor
    |-  ^+  dat
    ?~  all  dat
    =+  i.all
    =/  loot  difs
    =/  to=(unit @ux)  to.kill
    =/  id=(unit @ux)  id.kill
    ::  %-    %+  bark
    ::      state
      ::  =;  log
      ::    |.(log)
      ::  "data: {<(turn loot |*($>(%pack diff:hunt:c) !<(?(@ux [@ud @ud] [@ud @p @ud] @t) data)))>} id: {<id>} to: {<to>}"
    =-  $(all t.all, cor cor:-, dat -)
    =<
      =.  vapors.cor  (~(put bi vapors) oath deed pak)
      boc
    %.  (fall to 0x0)
    =<  swap
    %.  loot
    =<  prep
    (abed:dat oath ?~(id deed id) 666 ~)
    ::
      %handle-http-request
    =+  !<([id=@ta req=inbound-request:eyre] vaz)
    :: =+  we=we-work:(we-abed:webs id req)
    =^  cards  state  we-abet:we-work:(we-abed:webs id req)
    (emil cards)
    :: =+  mars=we-have:we
    :: ?~  mars  (emil cards)
    :: (cher:(emil cards) (need mars))
    ::
      %vapor-make-update
    =+  upd=!<(updates:make vaz)
    ?>  ?=(%new-stat -.upd)
    =/  =pods  (~(got bi owners) p.upd q.upd)
    ?>  (~(has in pods) src.bol)
    =/  =pack  (~(got bi vapors) p.upd q.upd)
    ::
    =/  u=(urth json)
      :_  %local
      ?~  r.upd  [%| *goof]
      [%& u.r.upd]
    %.  [[p q]:upd u]
    %=    stat
        vapors
      (~(put bi vapors) p.upd q.upd pack(urth u))
    ==
    ::
      %vapor-make-action
    ?:  |(=('' addy) =('' node))
      ~|(make-need/[addy node] !!)
    =+  act=!<(actions:make vaz)
    ?-    -.act
    ::
      %set-addy  (show(you p.act) mar vaz)
      %set-node  (show(rpc p.act) mar vaz)
      %set-key   (show(key p.act) mar vaz)
    ::
        %del-vapor
      =.  vapors  (~(del bi vapors) p.act q.act)
      cor
    ::
        %clear
      %+  roll  ~(tap bi vapors)
      |=  $:  [=oath =deed *]
              out=_cor
          ==
      %-  emil:out
      %.  ~[%trans]
      =<  heel
      ~(. hund [oath deed ~ *@udblocknumber])
    ::
      ::  XX: remove in production?
        %kick
      %+  roll  ~(tap bi vapors)
      |=  $:  [=oath =deed =pack]
              out=_cor
          ==
      =/  =pods  (~(got bi owners) oath deed)
      %-  emil:out
      (arks [oath deed pack] pods)
      ::
        %show
      (show mar vaz)
      ::
        %add-tech
      =,  act
      =/  =slip  [p q]
      =/  =pack  (~(got bi vapors) p q)
      =/  pods=(unit pods)  (~(get bi owners) p q)
      =/  pace=(axal ^tech)  (~(got by mars.pack) slip)
      =.  mars.pack
        %+  ~(put by mars.pack)  slip
        (~(put of pace) /(scot %p our.bol)/[-.tech] tech)
      =.  vapors  (~(put bi vapors) p.slip q.slip pack)
      %.  [%vapor-make-action !>([%wrap slip])]
      =<  poke
      %-  emil:(show mar vaz)
      ?~  pods  ~
      (arks [p q pack] u.pods)
      ::
        %wrap
      ::  XX: restore in production:
      :: ~_  'vapor-make-panic-deed-already-registered'
      :: ?<  (~(has bi vapors) p.act q.act)
      =,  act
      =/  slip  [p q]
      =/  czek=(unit czek:make)
        `[rpc you p q]
      =-  (wive:(show mar vaz) -)
      :-  `path`(lane:v %wrap `slip)
      (block-number:~(. c rpc) %latest)
      ::
        %since
      =,  act
      =/  slip  [p q]
      =-  (wive:(show mar vaz) -)
      :-  `path`(lane:v %wrap `slip)
      (since-block:~(. c rpc) block)
    ==
  ==
::    +arvo
::
::  handles data coming in from arvo. the following paths are supported:
::
::  /make/navy: a new navy has been created
::  /eyre/connect: an eyre binding has been established
::  /make/wrap: a new wrap is being created. checks for a successful
::     result from scanner, updates the vapor namespace, and spawns 
++  arvo
  |=  [=pith sig=sign-arvo]
  ^+  cor
  ?+    pith  ~|(vapor-make-panic-bad-arvo/[pith sig] !!)
  ::
      [%make %owner ~]
    %.  cor
    (bark state |.("owner seen on path {<pith>}. sign: {<sig>}"))
  ::
      [%eyre %connect ~]
    ?>(?=([%eyre %bound %& *] sig) cor)
  ::
      [%make %navy *]
    %.  cor
    (bark state |.("navy seen on path {<pith>}"))
  ::
      [%make %wrap rest=*]
    ~_  (crip "make: failure: {<[rest.pith]>}")
    ?>  ?=(%khan -.sig)
    ?>  ?=(%arow +<.sig)
    =/  vow  ,.+>.sig
    =,  pith
    ~_  (crip "make: failure: {<[rest]>}")
    =+  ;;([[@ bo=@ux] it=?([[@ i=@ux] ~] ~)] rest)
    =/  =slip
      [bo ?^(it `i.it it)]
    =/  [=oath =deed]  slip
    =;    run
    ?-    -.vow
    ::
        %|
      %.  cor
      (bark state |.("thread failed {<+.vow>}"))
    ::
        %&
      (run !<(updates:make q.p.vow))
    ==
    |=  upda=updates:make
    ~_  (crip "make: failure: {<[upda rest]>}")
    ?>  ?=([%block ^] upda)
    =/  bloq=@udblocknumber  u.p.upda
    =/  hunter  ~(. hund [oath deed ~ bloq])
    =/  pak=pack
      =|  =pack
      %_    pack
        ::
          mars
        =/  old-pack=(unit ^pack)
          (~(get bi vapors) p.slip q.slip)
        =+  new-mars=(~(put by mars.pack) slip *(axal tech))
        =/  myth=?

          ?:  ?=(~ old-pack)  &
          !(~(has by mars.u.old-pack) slip)
        =.  mars.pack
          ?:  myth   new-mars
          ?~  old-pack  new-mars
          mars.u.old-pack
        mars.pack
        ::  =/  pace=(axal tech)  (~(got by mars.pack) slip)
        ::  =/  old=(unit tech)  (~(get of pace) tale)
        ::  %+  ~(put by mars.pack)  slip
        ::  ?~  old
        ::    (~(put of pace) tale [%land `e.u.r.result ~])
        ::  ?.  ?=(%land -.u.old)
        ::    (~(put of pace) tale [%land `e.u.r.result ~])
        ::  =+  new=u.old
        ::  (~(put of pace) tale new(p.+ `e.u.r.result))
        ::
        host  our.bol
      ==
    =.  vapors  (~(put bi vapors) oath deed pak)
    =/  game=(list card)
      =,  hunter
      (weld meat (chase ~[%base %trans]))
    %.  vapor-make-update+!>(upda)
    =<  show
    %.  game
    =<  emil:cor
    =<
      =/  =pods  (fall (~(get bi owners) oath deed) ~)
      (dist [pods (sift ~)])
    (abed:boc oath deed bloq ~)
  ==
::    +dude
::
::  handles data coming in from other agents. the following paths are
::  supported:
::
::  /make/notify            - after a dm
::  /make/hund              - scanner acks
::  /logs/[type]/make/hund  - scanner logs
::    parses a diff:scanner and updates the vapor namespace
::    with the results. the path contains the block number 
::    the watchdog began with. 
++  dude
  |=  [=pith sig=sign:agent:gall]
  ^+  cor
  ?+    pith  ~|(vapor-make-panic-bad-dude/[pith sig] !!)
  ::  notify - after a dm
        [%make %notify ~]  cor
  ::  oath/deed - poke-ack
      [%make %hund [%ux bo=@ux] [@ux it=@ux] [%ud bloq=@ud] ~]
    ~_  'VAPOR: make error, hund failure'
    ?>  ?=(%poke-ack -.sig)
    %.  cor
    ?~(p.sig same (slog 'VAPOR: hund failure' u.p.sig))
  ::  oath/~  - poke-ack
      [%make %hund [%ux bo=@ux] [%ud bloq=@ud] ~]
    ~_  'VAPOR: make error, hund failure'
    ?>  ?=(%poke-ack -.sig)
    %.  cor
    ?~(p.sig same (slog 'VAPOR: hund failure' u.p.sig))
  :: /logs/[type]/make/hund/[oath]/[bloq]
      [%logs type=@tas %make %hund [%ux oath=@ux] [%ud bloq=@ud] ~]
    =,  pith
    =/  s=slip  [oath ~]
    ~_  'VAPOR: make error, scanner failure'
    ?+    -.sig  ~|(vapor-make-panic-bad-sig/[pith sig] !!)
    ::  %kick
        %kick
      %-  (bark state |.("kicked: {<oath>}"))
      %-  emil:cor
      %.  ~[%base]
      =/  last  (block:scry:c (pout pith))
      %-  (bark state |.("last block: {<last>}"))
      ~(chase hund [p.s q.s ~ last])
    ::  %watch-ack
        %watch-ack
      %-  (bark state |.("watch-ack: {<oath>}"))
      %.  cor
      ?~  p.sig  same
      (slog 'VAPOR: scanner failure' u.p.sig)
    ::  %fact
        %fact
      %-  (bark state |.("{<p.cage.sig>}: {<oath>}"))
      ?>  ?=(%scanner-diff p.cage.sig)
      =+  !<(dif=diff:scan q.cage.sig)
      ?+    -.dif  cor
      ::  %history
          %history
        ~&  "history seen, cards: {<dek>}"
        cor
      ::  %logs
          %logs
        =,  hunt:c
        =/  logs=(list event)
          %-  flop
          %+  murn  loglist.dif
          |=  event-log:rpc:eth:c
          ?~  mined  ~
          ?:  removed.u.mined  ~
          %-  some
          :_  block-number.u.mined
          [address data topics]
        ~&  slip+s
        =|  kill=state
        =/  all=(list [difs=effects:hunt:c kill=state])
          %+  spun  logs
          |=  $:  event
                  acc=(list [effects state])
              ==
          =.  kill  kill(oath p.s, oaths (~(put in oaths.kill) p.s), boq `@udblocknumber`boq)
          =/  [difs=effects kill=state]
            (process kill log)
          :-  [difs kill]
          [[difs kill] acc]
        =/  dat  boc
        =<  cor
        |-  ^+  dat
        ?~  all  dat
        =+  i.all
        =/  loot  difs
        =-  $(all t.all, cor cor:-, dat -)
        =<
          =.  vapors.cor  (~(put bi vapors) oath deed pak)
          boc
        =/  to=(unit @ux)  to.kill
        =/  id=(unit @ux)  id.kill
        %-    %+  bark
            state:dat
          =;  msg
            |.(msg)
          """
          found a {<type.pith>}!
          to: {<to>}
          contract: {<oath.kill>}
          last block: {<boq.kill>}
          """
        %.  (fall to.kill 0x0)
        =<  swap
        %.  loot
        =<  prep
        (abed:dat oath.kill id.kill boq.kill to.kill)
        ==
    ==
  :: /logs/[type]/make/hund[oath]/[deed]/[bloq] 
      [%logs type=@tas %make %hund [%ux oath=@ux] [%ux deed=@ux] [%ud bloq=@ud] ~]
    ::  TODO support watching specific items again
    =,  pith
    %-  (bark state |.("saw logs for {<oath>}/{<deed>}, ignoring"))
    cor
  ==
::    +boc
::
::  oath core
::
++  boc
  =|  [pak=pack novo=(unit @ux) boat=(unit @p) buoy=(unit @ux) =effects:hunt:c kill=state:hunt:c]
  |_  [=oath =deed bloq=@udblocknumber]
  ++  boc  .
  ++  aboq
    ^+  boc
    =.  cor
      %-  emil
      ~(meat hund [oath deed ~ bloq])
    boc
  ++  abed
    |=  [=^oath =^deed =_bloq n=(unit @ux)]
    ^+  boc
    =?  novo  ?=(^ n)  n 
    =?  boat  ?=(^ novo)  
      =-  ?-  -.-  
          %&  p.-
          %|  boat
        ==
      %-  mule  |.
      %-  some
      %-  head
      ~(tap in (~(got bi owners) registry:bonds:c novo))
    ~&  >  novo
    ~&  >  boat
    %=  boc
      oath  oath
      deed  deed
      bloq  bloq
      pak   ?^  mack=(~(get bi vapors) oath deed)  u.mack 
            ?^  muck=(~(get bi vapors) oath ~)  u.muck
            pak
    ==
  ::    +bend
  ::
  ::  eyre binding for a slip
  ++  bend
    ^+  boc
    =.  cor
      =-  (emit %pass /eyre/connect %arvo %e -)
      [%connect [[~ (lane:v /apps/vapor/make `[oath deed])] dap.bol]]
    boc
  ::    +swap
  ::
  ::  handle ownership change
  ::
  ::  .addy: the new owner's eth address
  ++  swap
    |=  =addy
    ^+  boc
    =/  =slip  [oath deed]
    =/  heir=pods
      =/  all-owners  .^(owners:dic :(welp /gx paz /own/noun))
      %-  (bark state |.("checking ownership for {<addy>}"))
      %-  ~(rep by all-owners)
      |=  [[=owner:dic ships=(set ship)] all=(set ship)]
      ?.  =(address.owner addy)  all
      ?.  ?=(%own proxy.owner)
        %.  all
        (bark state |.("found proxy {<proxy.owner>} for {<addy>} with ships {<ships>}"))
      (~(uni in ships) all)
    =/  lord=(unit pods)  (~(get bi owners) oath deed)
    =/  tsar=?  |(=(onboarding:bonds:c oath) =(registry:bonds:c oath))
    ::
    %-  %+  bark  state
      |.("{<[%addy addy %is heir %ware [oath deed]]>}")
    =?   cor  &(?=(^ deed) ?=(^ lord) !tsar)
      %-  %+  bark  state
        |.("{<[%kicking owners]>}")
      (emit %give %kick [(lane:v %slip ~ oath deed)]~ ~)
    =?  owners  &(!tsar ?=(^ lord))
      ?~  (~(dif in heir) u.lord)  owners
      (~(put bi owners) oath deed heir)
    (dist:boc heir (sift ~))
  ::    +view
  ::  locate namespace within vapors
  ::
  ++  view 
    ^-  (axal tech)
    %+    fall
      (~(get by mars.pak) [oath deed])
    *(axal tech)
  ::    +rack
  ::  our hosted namespace
  ::  by contract only
  ::
  ++  rack
    ^-  (axal tech)
    %-  ~(dip of (~(got by mars:(~(got bi vapors) oath ~)) [oath ~]))
    /(scot %p our.bol)
  ::    +rail
  ::  hosted deed namespace
  ::  by contract and deed
  ::
  ++  rail
    ^-  (axal tech)
    %+  fall
      (~(get by mars.pak) [oath deed])
    (~(got by mars.pak) [oath ~])
  ::    +sift
  ::  locate all tech within a namespace
  ::
  ++  sift
    |=  filt=(unit $-(tech ?))
    ^-  gear
    %+  murn  ~(tap of rail)
    |=  [p=* q=tech]
    ?~  filt  `q
    ((flit u.filt) q)
  ::    +pick
  ::  locate a single tech within a namespace
  ::
  ++  pick
    |=  label=@tas
    ^-  (unit tech)
    %+  biff
      %.  label
      ~(get by dir:rack)
    |=(_rack fil)
  ::    +prep
  ::  handle post-hunt tech, possibly updating namespace
  ::
  ++  prep
    |=  =effects:hunt:c
    ^+  boc
    ~&  prep+novo.boc
    ?~  effects  boc
    =/  =diff:hunt:c  i.effects
    ?-    -.diff
    ::  %transfer
      %transfer  boc
    ::  %pill
      %pill  boc
    ::  %pack
        %pack
      =/  diff=$>(%pack diff:hunt:c)  diff
      =/  slab=(unit tech)  (pick %slab)
      =/  cast=vase
        ?.  ?=(^ slab) 
          %.  (road |.(data.diff))
          (bark state |.("no slab for diff (path: {<in>})"))
        ?>  ?=(%slab -.u.slab)
        =/  =cage  p.u.slab
        %-  road  |.((slam q.cage data.diff))
      ~&  >>>  -.cast
      =.  path.diff
        ?~  path.diff  path.diff
        ?+    p=u.path.diff  ~&  >>  "no change: {<path.diff>}"  path.diff
        ::  prepend ship alias to path
            [%transfersingle *]
          =+  !<([to=@ud id=@ud] data.diff)
          =/  puds=(unit pods)
            ?~  novo  ~&  >>  "empty novo"  ~
            %+  ~(get bi owners)
            registry:bonds:c
            %-  head
            ~(tap in (~(key bi owners) u.novo))
          ?~  puds  `p
          =/  =ship  (head ~(tap in u.puds)) 
          `(welp ~[i.p] /(scot %p ship))
            [%packagebought @ @ *]
          =+  !<([eoa=@ux ship=@p id=@ud] data.diff)
          `:(welp ~[i.p] ~[i.t.p] /(scot %p ship))
        ==
      ~&  >>  path.diff
      =/  dat
        ?~  path.diff  boc
        ?+    p=u.path.diff  ~&  >>  "unknown diff: {<path.diff>}"  boc
            [%packagebought %aliases @ *]
          =+  !<([eoa=@ux ship=@p id=@ux] data.diff)
          =/  puds=(unit pods)
            (~(get bi owners) registry:bonds:c `eoa)
          =/  =pods
            ?~  puds  (~(put in *pods) ship)
            %.  ship
            ~(put in u.puds)
          =/  pace=(axal tech)
            %.  /(scot %ux registry:bonds:c)/(scot %ux id)/pipe/accountcreated/aliases/(scot %ux id)
            ~(dip of rail)
          %=  boc
            :: owners  owners
              owners  
            =/  alias=(unit $>(%pipe tech)) 
              =;  res
                ?-    -.res
                  %&  p.res
                  %|  ~ 
                ==
              %-  mule  |.
              ;;  (unit $>(%pipe tech))
              fil:pace
            ?~   alias
              (~(put bi owners) registry:bonds:c (some id) pods)
            (~(put bi owners) !<(@ux q.p.u.alias) (some id) pods)
            ::
            :: boat  (some ship)
            :: buoy  (some id)
            :: novo  (some eoa)
          ==
          ::
            [%accountcreated %aliases @ *]
          =+  !<(nova=@ux data.diff)
          :: ~&  >>>  acc+novo.boc
          :: ~&  >>>  (pick %packagebought)
          :: =/  puds=(unit pods)
          ::   (~(get bi owners) registry:bonds:c `nova)
          :: =/  =pods
          ::   ?^  p=(both puds boat) 
          ::     (~(put in -.u.p) +.u.p)  
          ::   *pods
          :: =?  owners.boc  ?=(^ buoy)  
          ::   (~(put bi owners) nova buoy pods)
          boc
        ==
      =/  in=path  (weld /pipe (fall path.diff /strange/bloq/(scot %ud bloq)))
      =;  axe=(axal tech)
        =.  mars.pak  (~(put by mars.pak) [oath deed] axe)
        $(effects t.effects, boc dat(pak pak(mars mars.pak)), cor cor.boc)
      =/  last=path
        %+  weld  /pipe
        =+  `path`/bloq/last/(scot %ud bloq)
        %.  -
        %-  lead
        %+  scot  %tas
        ?~  path.diff  %strange
        (head u.path.diff)
      =/  pit=tech  [%pipe [%noun `vase`cast]]
      =-  ?^(path.diff (~(put of -) last pit) -)
      (~(put of (~(lop of rail) (snip last))) in pit)
    ==
  ::    +dist
  ::  handle distribution
  ::
  ::  .pods: the ships to distribute to
  ::  .gear: the tech to distribute
  ::
  ++  dist
    |=  [=pods =gear]
    ^+  boc
    |^
    %-  (bark state |.("namespace for {<[oath deed]>}: {<view>}"))
    |-  ^+  boc 
    ?~  gear  boc
    =/  =tech  i.gear
    ?+    -.tech  boc
    ::  %desks
        %desks
      =+  dusk=;;((set desk) +.tech)
      =;    wol
        =.  cor  wol
        $(gear t.gear, boc boc, cor cor) 
      %-  emil
      =,  clay
      %+  welp
      %-  zing
      %+  turn  ~(tap in dusk)
      |=  =desk
      =/  firm  (name desk)
      =/  perms  .^([r=dict w=dict] %cp /(scot %p our.bol)/[desk]/(scot %da now.bol))
      =/  =_who:*real  who.rul.r.perms
      ?^  crew=(~(get by q.who) firm)
        :~  :*  %pass  /make/navy/firm  %arvo  %c
            %cred  firm  pods
        ==  ==
      ::
      =/  visa=(set whom)
        %.  (lead %&)
        ~(run in p.who)
      =/  navy=(set whom)
        %.  (lead %|)
        ~(run in ~(key by q.who))
      =/  ayes=(set whom)  (~(uni in visa) navy)
      =+  (~(put in ayes) [%| firm])
      :~  :*  %pass  /make/navy/ayes  %arvo  %c
              [%perm desk / [%r `[%white -]]]
          ==
          [%pass /make/navy/crew %arvo %c %cred firm (~(put in pods) our.bol)]
      ==
      :: send DMs to all whitelisted ships
      %+  turn  ~(tap in pods)
      |=  =ship
      =/  m1
        """
        You have been whitelisted to the %tharsis desk!
        Your purchase includes both the client and server agents within the %tharsis desk.
        For further instructions and support, join the vaporware public group above.
        To install, run:
        """
      =/  links=(list block:chat)
        %+  welp
          :_  ~
          :-  %cite  [%group our.bol %vaporware]
        %+  turn  ~(tap in dusk)
        |=  =desk
        :-  %cite
        [%desk [our.bol desk] /]
      =/  install-message=inline:chat
        [%code '|install ~ligbel %tharsis']
      =/  content=content:chat
        story/[links [(crip m1) install-message ~]]
      =/  memo=memo:chat
        [~ our.bol now.bol content]
      =/  diff=diff:dm:chat
        [[our.bol now.bol] %add memo]
      :*  %pass  /make/notify  %agent  [our.bol %chat]  %poke
          %dm-action  !>([ship diff])
      ==
    ::  %land
        %land
      =;  run
        $(gear t.gear, cor run, boc boc)
      %-  emil  
      %+  turn  ~(tap in pods)
      |=  =ship
      =/  m1
        """
        you got a {<tech>}
        """
      =/  =content:chat
        story/[~ [(crip m1) ~]]
      =/  =memo:chat
        [~ our.bol now.bol content]
      =/  =diff:dm:chat
        [[our.bol now.bol] %add memo]
      :*  %pass  /make/notify  %agent  [our.bol %chat]  %poke
          %dm-action  !>([ship diff])  
      ==
    ::  %slab
        %slab
      =;  run
        $(gear t.gear, cor run, boc boc)
      %-  emil
      %+  turn  ~(tap in pods)
      |=  =ship
      =/  m1
        """
        you got a {<tech>}
        """
      =/  =content:chat
        story/[~ [(crip m1) ~]]
      =/  =memo:chat
        [~ our.bol now.bol content]
      =/  =diff:dm:chat
        [[our.bol now.bol] %add memo]
      :*  %pass  /make/notify  %agent  [our.bol %chat]  %poke
          %dm-action  !>([ship diff])  
      ==
    ::  %pipe
        %pipe
      =/  vent=(axal ^tech)
        (~(dip of view) /pipe)
      %-  (bark state |.("venting {<[oath deed]>}: {<vent>}"))
      =/  pace  (pat vent /accountcreated/aliases)
      =/  pats  (pat vent /packagebought/aliases)
      =/  puds=^pods
        %-  silt
        %+  murn  pats 
        |*  *
        ^-  (unit @p)
        ?~  +<  ~
        ?~  s=(slaw %p (rear +<))  ~
        =/  clan  (clan:title u.s)
        ~&  >>  s+u.s
        ?.  =(%duke clan)  ~
        [~ u.s]
      ~&  >>  pace+pace  
      ~&  >>  pats+pats 
      =/  alias=(unit $>(%pipe ^tech)) 
        ;;  (unit $>(%pipe ^tech))
        ?~  pace  
          ?~  ps=~(tap in puds)  ~
          `[%pipe [%noun !>(`@ux`(head ps))]]  
        (~(get of vent) i.pace)
      ~&  >>>  alias+alias
      ~&  >>>  puds+puds
      =;  run
        =.  dek.cor  dek.run
        =?  owners  ?=(^ alias)
        =+  !<(@ux q:`cage`p.u.alias)
        (~(put bi owners) - deed (fall (~(get bi owners) - deed) puds))  
        $(cor run, gear t.gear, boc boc(owners owners))
      =-  (give %fact - `cage`p.tech)
      %-  zing
      =-  %+  murn  -
        |=  (pole @ta)
        ^-  (unit (list path))
        ::  ~&  >>>  (pat vent /(scot %tas +<))
        ?+  +<  ~&([%strange-tech +<] `~[(lane:v %strange `[oath deed])])
        ::
          [%transfersingle ship=@ ~]  `~[(lane:v /transfersingle/[ship] ~)]
        ::
          [label=@ %owner *]  `~[(lane:v +< `[oath deed])]
        ::
          [%packagebought %aliases ship=@ ~]
             `~[(lane:v /packagebought/[ship] ~)]
          [%accountcreated %aliases id=@ ~]
             `~[(lane:v /accountcreated/[id] ~)]
          [label=@ %aliases rest=*]
            %-  some
            ^-  (list path)
            %+  turn  pace 
            |=  p=path
            ?~  s=((soft ship) (fall (slaw %p -.rest) **))  (lane:v p ~)
            (lane:v /[label]/(scot %p u.s) ~)
        ::  %bloq
        ::    only 
            [label=@ %bloq *]
          =/  =path  +<
          =/  last=_bloq
            =;    paz
              =+  tal=(head (flop paz))
              =+  res=(mule |.((slav %ud tal)))
              ?-    -.res
                %&  p.res
                %|  ~&  >>  tal  `@ud`tal
              ==
            -:(~(fit of vent) /bloq/last)
          ?.  (gth last bloq)  ~
          `~[(lane:v path `[oath deed])]
        ==
      ^-  (list path)
      %-  zing
      =-  (turn - :(corl (cury pat vent) path (late ~)))
      =-  ~(tap in -)
      ~(key by dir:vent)
    ::
    ==
    ::
    ++  pat
      |*  [fat=(axal) pax=path]
      =>  .(pax `path`pax)
      =.  fat  (~(dip of fat) pax)
      =|  out=(list path)
      |-  ^+   out
      =?  out  ?=(^ fil.fat)  (snoc out pax)
      =/  dir  ~(tap by dir.fat)
      |-  ^+   out
      ?~  dir  out
      %=  $
        dir  t.dir
        out  ^$(pax (weld pax /[p.i.dir]), fat q.i.dir)
      ==
    ++  puck
      |=  (unit tech) 
      +<
      :: ?^  fil  !<(tech u.fil)
      :: ~&  >>  "no file at {<dir>}"  ~
    ++  name
      |=  =desk
      ^-  @ta
      =/  hash  (shax (jam [oath deed]))
      (crip "make-{<desk>}-{<hash>}")
    --
  --
::    +hund
::
::  handles watchdogs
::
++  hund
  |_  $:  bon=oath
          tem=deed
          ady=(unit addy)
          boq=@udblocknumber
      ==
  ::
  +*  hu  .
      pat  (lane:v %hund `[bon tem])
      wir  (welp pat /(scot %ud boq))
      doe  [our.bol %scanner]
        raw
      ^-  (list card)
      =-  (abed:pokes:hunt:c -)
      [rpc boq bon bol %scanner]
  ::    +meat
  ::  
  ::  send out the hounds
  ::
  ++  meat  raw
  ::    +chase
  ::
  ::  who let the dogs out?
  ::
  ++  chase
    |=  trail=(list @tas)
    ^-  (list card)
    %-  (bark state |.("watchdogs on the loose at {<trail>}"))
    =;    cards
      %-  (bark state |.("watchdogs {<trail>} cards: {<cards>}"))
      cards
    %+  turn  trail
    |=(kind=@tas [%pass logs+[kind wir] %agent doe %watch logs+[kind pat]])
  ::    +heel
  ::
  ::  back to the pound
  ::
  ++  heel
    |=   trail=(list @tas)
    ^-  (list card)
    %-  (bark state |.("calling off the hounds"))
    %+  turn  trail
    |=  kind=@tas
    :*  %pass  wir  %agent  doe  %poke
        scanner-poke+!>(clear+[kind pat])
    ==
  --
::  +webs
::
::  handles http requests
::
++  webs
  |_  $:  eid=@tA
          caz=(list card)
          inb=inbound-request:eyre
          pay=(unit simple-payload:http)
          hav=(unit [oath deed pack])
      ==
  +*  we  .
  ++  fip
    =,  de-purl:html
    %+  cook
      |=(pork (weld q (drop p)))
    (cook deft (more fas smeg))
  ++  we-emit  |=(c=card we(caz [c caz]))
  ++  we-emil  |=(lac=(list card) we(caz (welp lac caz)))
  ++  we-have  hav
  ::
  ++  we-abed
    |=([id=@ta ib=inbound-request:eyre] we(eid id, inb ib))
  ::
  ++  we-abet
    ^-  (quip card _state)
    :_  state
    %-  flop  %+  welp  caz
    (give-simple-payload:app:ser eid (need pay))
  ::
  ++  we-fail  we(pay `[500+~ ~])
  ::
  ::    +we-work
  ::
  ::  /make/[oath]/[deed]/upload:
  ::    adds uploads to the slip's namespace under
  ::    /land/[name]/[content type]
  ++  we-work
    ^+  we
    =*  headers  header-list.request.inb
    =/  reqline  (parse-request-line:ser url.request.inb)
    =/  =pith  (pave:v site.reqline)
    ?+    pith  we-fail
        [%apps %vapor %make [%ux oath=@ux] [%ux deed=@ux] %upload name=@ rest=*]
      =,  pith
      =/  fact=card
        :^  %give  %fact  [/web-ui]~
        vapor-make-update+!>(add-mars+[[oath `deed] %&])
      =/  fail=card
        :^  %give  %fact  [/web-ui]~
        vapor-make-update+!>(add-mars+[[oath `deed] %|])
      ::  XX: no-op-ing on system files
      ?:  ?=(%46 (cut 3 [0 1] name))
        (we-emit(pay `[200^~ ~]) fact)
      ::
      =+  heads=(malt header-list.request.inb)
      %-  %+  bark  state
          =;  output  |.("{<output>}")
          :*  %name  name  %rest  rest  %head  heads  %got
            ?~  body.request.inb  %nothin
            p.u.body.request.inb
          ==
      ?~  muck=(~(get by heads) 'content-type')
        (we-emit(pay `[400^~ ~]) fail)
      ?~  body=body.request.inb
        (we-emit(pay `[400^~ ~]) fail)
      =/  fare=path  /land/[name]/[u.muck]
      =/  =slip  [oath `deed]
      =/  =pack  (~(got bi vapors) oath `deed)
      =/  pace=(axal tech)  (~(got by mars.pack) slip)
      =.  pace  (~(dip of pace) /(scot %p our.bol))
      =.  mars.pack
        %+  ~(put by mars.pack)  slip
        (~(put of pace) fare [%land (land ~ body)])
      %.  fact
      %=    we-emit
        pay     `[200^~ ~]
        hav     `[oath `deed pack]
        vapors  (~(put bi vapors) oath `deed pack)
      ==
    ==
  --
--
