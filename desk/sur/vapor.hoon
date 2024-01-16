/-  e-w=eth-watcher, dic=dice
/+  eth=ethereum, *mip, nav=naive, ser=server
|%
::    %vapor
::
+|  %state
+$  card  card:agent:gall
+$  perm  $+(card card)
+$  vapors
  $+  vapors
  (link deed pack)
+$  owners
  $+  owners
  (link deed pods)
+$  versioned-state  $%(state-0)
::
+$  state-0
  $:  %0
        you=addy
        rpc=node
        key=@t
        =vapors
        =owners
    ==
+|  %builders
::
::    +link
::
::  builds an association structure for oath-related
::  permissions.
::
::  .key:   thing associated with the oath
::  .value: the material
++  link
  |$  [key value]
  (mip oath key value)
::   +moss
::
::  holds a piece of earth or a problem.
::
::  .muk: the earth or the problem
++  moss
  |$  [muk]
  (each muk goof)
::   +urth
::
::  associates a tag to a particular $mud
::  we assume that it is unreliable.
::
::  .mud: the earth part
++  urth
  |$  [mud]
  $+  urth
  $:  p=(moss mud)
      =dirt
  ==
::    +mars
::
::  a representation of a dynamic piece of mars.
::
::  .space: label for a slice of namespace
::  .ice:   the type of data being distributed
++  mars
  |$  [space ice]
  $+  mars 
  (map space (axal ice))
::
+|  %urth
::
::  $node:  ethereum node rpc endpoint
+$  node      @t
::  $oath:  ethereum contract address
+$  oath      address:eth
::  $mud:   earth part
+$  dirt      ?(%os %ipfs %http %local %eth)
::  $addy:  ethereum address
+$  addy      address:eth
::  $slip:  contract and (optional) deed id
+$  slip      (pair oath deed)
::  $deed:  contract-related asset serialized in hex
+$  deed      (unit @ux)
::  $host:  distributing ship
+$  host      @p
::  %earf:  ethereum output
+$  earf      [e=json c=@t n=@t b=@udblocknumber]
::
+|  %mars
::
::  $pith: faceless typed path
::
+$  pith
  (pole iota)
::  $land:  rendering description
::
+$  land      $+(land (pair (unit json) (unit mime)))
::  $gear:  everything to be distributed
+$  gear      (list tech)
::  $tech:  distribution data
+$  tech
  $%  [%desks (set desk)]
      [%perm (list perm)]
      [%pipe p=(cask vase)]
      [%slab p=(cask vase)]
      [%land land]
  ==
::  $knob:  distribution types
+$  knob       ?(%desks %perm %pipe %slab %land)
::    $pack
::
::  .urth:  external data
::  .mars:  internal namespace
::  .gear:  distribution data
::  .host:  distributing ship
+$  pack
  $+  pack
  $:  =(urth json)
      =(mars slip tech)
      =host
  ==
::  $pods:  owned ships
+$  pods      (set @p)
::  $arks:  ownership data
+$  arks      [oath deed pack]
::
++  make
  |%
  +$  actions
    $%  [%set-addy p=@ux]
        [%set-node p=@t]
        [%set-key p=@t]
        [%del-vapor slip]
        [%clear ~]
      ::
        [%wrap slip]
        [%since slip block=@udblocknumber]
      ::
        [%add-tech slip =tech]
      ::
        [%kick ~]
        [%show ~]
    ==
  +$  updates
    $~  [%block ~]
    $%
        [%add-mars slip r=?]
        [%new-stat slip r=(unit json)]
        [%new-slip slip r=(set @p)]
        [%block p=(unit @udblocknumber)]
    ::
      $:  %add-earf
          slip
          r=(unit earf)
      ==
    ==
  +$  czek  [=node =addy =slip]
  --
++  ware
  |%
  +$  actions
    $%  [%wrap slip r=@p]
        [%get-stuf ~]
    ==
  +$  updates
    $%  [%get-stat p=(mars)]
        [%get-stuf p=(urth)]
    ==
  --
--
::
