extensions [rnd csv]
;; This is an adaptation of the  Relative Agreement Model of opinion dynamics (Deffuant et al. 2002)
;; as implemented by Meadows and Cliff (2012) with extensions that enable the exploration of
;; the effect of network structure.
;; Code for the generation of networks is borrowed from the Small World  and
;; Preferential Attachment Models of the Netlogo Models Libary (Wilensky, 1999)
;; 1/29/16
;; David Adelberg and Dr. Spiro Maroulis
;;LUCA
;;

turtles-own [
  o_openness
  o_insttrust
  o_demtrust
  ;;LUCA
  ;;
  ;;
  s_age_class
  s_gender
  s_country_area
  ;;LUCA
  ;;
  mu
  theta
  ;;
  uncertainty
]
globals [init_data global_gender global_age_class global_country_area global_s]

links-own
[
  rewired?
]



to setup
  ca
  set-default-shape turtles "person"



  set init_data csv:from-file "file_init.csv"
  set init_data but-first init_data                                                                       ;; tranne il primo
  set global_s csv:from-file "file_global.csv"
  set global_gender []                                                                                    ;; i sessi con le loro probabilità, calcolate dalla survey
  set global_age_class []                                                                                 ;; le classi di età con le loro probabilità, calcolate dalla survey
  set global_country_area []                                                                             ;; le aree geografiche con le loro probabilità, calcolate dalla survey
  let c 0
  repeat 3 [
    let raw_global item c global_s
    if (item 0 raw_global) = "global_gender" [
      let j 1
      let l 2

      repeat l [
        set global_gender lput (list (item j raw_global) (item (j + 1) raw_global)) global_gender
        show raw_global
        show global_gender
        set j (j + 2)

      ]
    ]
    if (item 0 raw_global) = "global_age_class" [
      let j 1
      let l 3
      repeat l [
        set global_age_class lput (list (item j raw_global) (item (j + 1) raw_global)) global_age_class
        set j (j + 2)
      ]
    ]
    if (item 0 raw_global) = "global_country_area" [
      let j 1
      let l 5
      repeat l [
        set global_country_area lput (list (item j raw_global) (item (j + 1) raw_global)) global_country_area
        set j (j + 2)
      ]
    ]

   set c c + 1
  ]
  crt number-of-people [
    setxy random-xcor random-ycor
    set shape "person"
    set uncertainty 0

    set s_gender first rnd:weighted-one-of-list global_gender [ [p] -> last p ]                                             ;; setta il sesso e la sua distribuzione globale per segmentazione
    set s_age_class first rnd:weighted-one-of-list global_age_class [ [p] -> last p ]                                       ;; setta la classe di età e la sua distribuzione globale per segmentazione
    set s_country_area first rnd:weighted-one-of-list global_country_area [ [p] -> last p ]                                 ;; setta l'area geografica e la sua distribuzione globale per segmentazione
    let riga filter [ i -> (item 0 i) = s_gender and (item 1 i) = s_age_class  and (item 2 i) = s_country_area] init_data   ;; carico il file di inizializzazione, con i valori delle opinioni e delle variabili di segmentazione
    set riga (item 0 riga)                                                                                                  ;; è una lista di liste, seleziono il primo item

    set o_openness random-normal (item 3 riga) ( (item 4 riga) * 0.5 )                                                      ;; non voglio il valore esatto ma un valore distribuito normalmente con quella media e quella varianza dal file di inizializzazione
    set o_insttrust random-normal (item 5 riga) ( (item 6 riga) * 0.5 )
    set o_demtrust random-normal (item 7 riga) ( (item 8 riga) * 0.5 )

    if o_openness < 0 [ set o_openness 0 ]                                                                                  ;; le opinioni devono essere nella scala 0-10
    if o_insttrust < 0 [ set o_insttrust 0 ]
    if o_demtrust < 0 [ set o_demtrust 0 ]

    if o_openness > 10 [ set o_openness 10 ]
    if o_insttrust > 10 [ set o_insttrust 10 ]
    if o_demtrust > 10 [ set o_demtrust 10 ]

   ; ask turtles [
   ;   if gnd = 4444 age_class=555  country=32
    ;  dalla tabella cvs carica opness trust (mean e dev std) in rnd normale]
  ]

  reset-ticks
end


to go
  ;; LUCA due utilizzi
  ;; 1) inserisco opinioni dalla survey e vedo come evolvono (inserendo fattori esogeni)
  ;; 2) opinioni a caso, vedere se lla fine corrispondono a quelle della survey. Abbiamo realizzato un modello, e il modello è il codice
  ;;
  repeat number-of-people [
    let p1 one-of turtles
    let p2 one-of other turtles
    interact p1 p2
  ]
  ; update-globals
  tick
end

;; Initializes the turtles
;to setup-turtles
;  clear-turtles
;
;  ifelse network-type = "small world" [
;    ;; adapted from Small World model
;    create-turtles number-of-people [
;      init-turtle self
;    ]
;
;    let success? false
;    while [not success?] [
;      ;; we need to find initial values for lattice
;      wire-them
;
;      ;;calculate average path length and clustering coefficient for the lattice
;      set success? do-calculations
;      layout-circle (sort turtles) max-pxcor - 1
;    ]
;
;    ;; setting the values for the initial lattice
;    set clustering-coefficient-of-lattice clustering-coefficient
;    set average-path-length-of-lattice average-path-length
;    set number-rewired 0
;    rewire-all
;  ]
;
;  [ifelse network-type = "fully connected" [
;      create-turtles number-of-people [
;        init-turtle self
;      ]
;      ask turtles [create-links-with other turtles]
;      layout-circle (sort turtles) max-pxcor - 1
;    ]
;
;  [if network-type = "preferential attachment" [
;    ;; adapted from Preferential Attachment model
;    make-node nobody
;    make-node turtle 0
;    let m 1
;    while [m < number-of-people - 1] [
;      make-node find-partner
;      set m m + 1
;      layout
;    ]
;    ask turtles [init-turtle self]
;  ]
;  ]
;  ]
;
;  let r do-calculations
;end
;
;to update-globals
;  set y calculate-y
;end
;
;;; The following procedures are adapted from the Meadows and Cliff (2012)
;
;to init-globals
;  set min-moderate-opinion (min-opinion + extreme-distance)
;  set max-moderate-opinion (max-opinion - extreme-distance)
;  set infinity 99999
;end
;
;to init-turtle [p]
;  let n-extremists (proportion-of-extremists *  number-of-people )
;  let half-n-extreme int (0.5 + (n-extremists / 2.0))
;  let n-moderate number-of-people - n-extremists
;  ask p [
;    set color gray + 2
;    ifelse who < half-n-extreme [
;      set opinion (max-opinion - (random-float extreme-distance))
;      set uncertainty uncertainty-of-extremists
;      set p-type "extreme"
;    ]
;    [ifelse who < n-moderate + half-n-extreme [
;        set opinion (min-moderate-opinion + random-float (max-moderate-opinion - min-moderate-opinion))
;        set uncertainty uncertainty-of-moderates
;        set p-type "moderate"
;    ]
;    [
;      set opinion (min-opinion + random-float (extreme-distance))
;      set uncertainty uncertainty-of-extremists
;      set p-type "extreme"
;    ]
;    ]
;    if p-type = "extreme" [
;      set color red
;
;      ]
;    if p-type = "moderate" [set color white]
;  ]
;end
;
;
;
;;; calculates the relative agreement between two turtles
;;; arguments:
;;;     p1    - the first turtle
;;;     p2    - the second turtle
;;; reports:
;;;     agree - the relative agreement between the two turtles
;to-report relative-agreement [p1 p2]
;  let the-overlap overlap p1 p2
;  let agree ((the-overlap / ([uncertainty] of p1)) - 1)
;  report agree
;end
;
;;; calculates the overlap between two turtless' bounds
;;; arguments:
;;;     p1      - the first turtle
;;;     p2      - the second turtle
;;; reports:
;;;    the-overlap - the overlap between the bounds

;
;;; calculates the upper and lower bounds on a turtle's opinion
;;; arguments:
;;;    p      - the turtle
;;; reports:
;;;    bounds - a list containing the lower and upper bounds on the turtle's opinion.

;

;; Makes p1 and p2 interact according to the relative agreement model
to interact [u v]

  let the-overlap overlap u v

  if (the-overlap > [uncertainty] of u) [

    let mu_w [mu] of u
    let theta_w [theta] of u
    ;; LUCA aggiungere la modifica di mu_w e theta_w sulla base del tabellone

    if abs([o_openness] of u - [o_openness] of v) <= theta_w [
      ask u [set o_openness (o_openness + mu_w * ([o_openness] of v - o_openness))]
      ask v [set o_openness (o_openness + mu_w * ([o_openness] of u - o_openness))]
    ]
    if abs([o_insttrust] of u - [o_insttrust] of v) <= theta_w [
      ask u [set o_insttrust (o_insttrust + mu_w * ([o_insttrust] of v - o_insttrust))]
      ask v [set o_insttrust (o_insttrust + mu_w * ([o_insttrust] of u - o_insttrust))]
    ]
    if abs([o_demtrust] of u - [o_demtrust] of v) <= theta_w [
      ask u [set o_demtrust (o_demtrust + mu_w * ([o_demtrust] of v - o_demtrust))]
      ask v [set o_demtrust (o_demtrust + mu_w * ([o_demtrust] of u - o_demtrust))]
    ]



  ]
end

to-report overlap [p1 p2]
  let p1-bounds (turtle-bounds p1)
  let p2-bounds (turtle-bounds p2)
  let lower (max list (item 0 p1-bounds) (item 0 p2-bounds))
  let upper (min list (item 1 p1-bounds) (item 1 p2-bounds))
  let the-overlap (upper - lower)
  report the-overlap
end

to-report turtle-bounds [p]
  let p-lower (([o_openness] of p) - ([uncertainty] of p))
  let p-upper (([o_openness] of p) + ([uncertainty] of p))
  let bounds list p-lower p-upper
  report bounds
end

;
;;; Calculates the y-value after a run
;to-report calculate-y
;  let moderates turtles with [p-type = "moderate"]
;  let moderate-count      count moderates
;  let p-prime-plus-count  count moderates with [opinion > max-moderate-opinion]
;  let p-prime-minus-count count moderates with [opinion < min-moderate-opinion]
;  let p-prime-plus p-prime-plus-count / moderate-count
;  let p-prime-minus p-prime-minus-count / moderate-count
;  set y sum map square list p-prime-plus p-prime-minus
;  report y
;end
;
;;; Squares num
;to-report square [num]
;  report num * num
;end
;
;;; The following procedures are from the Preferential Attachment Model
;;; in the NetLogo Models Library (Wilensky, 2005)
;
;to-report find-partner
;  report [one-of both-ends] of one-of links
;end
;
;;; used for creating a new node
;to make-node [old-node]
;  create-turtles 1
;  [
;    if old-node != nobody
;      [ create-link-with old-node
;        ;; position the new node near its partner
;        move-to old-node
;        fd 8
;      ]
;  ]
;end
;
;to layout
;  ;; the number 3 here is arbitrary; more repetitions slows down the
;  ;; model, but too few gives poor layouts
;  repeat 3 [
;    ;; the more turtles we have to fit into the same amount of space,
;    ;; the smaller the inputs to layout-spring we'll need to use
;    let factor sqrt count turtles
;    ;; numbers here are arbitrarily chosen for pleasing appearance
;    layout-spring turtles links (1 / factor) (20 / factor) (20 / factor)
;  ]
;  adjust-positions
;end
;
;to adjust-positions
;  ;; don't bump the edges of the world
;  let x-offset max [xcor] of turtles + min [xcor] of turtles
;  let y-offset max [ycor] of turtles + min [ycor] of turtles
;  ;; big jumps look funny, so only adjust a little each time
;  set x-offset limit-magnitude x-offset 0.1
;  set y-offset limit-magnitude y-offset 0.1
;  ask turtles [ setxy (xcor - x-offset / 2) (ycor - y-offset / 2) ]
;end
;
;to-report limit-magnitude [number limit]
;  if number > limit [ report limit ]
;  if number < (- limit) [ report (- limit) ]
;  report number
;end
;
;;; END
;
;;; The following procedures from the Small Worlds model
;;; in the Netlogo Models Library (Wilensky, 2005)
;
;to rewire-all
;
;  ;; make sure num-turtles is setup correctly; if not run setup first
;  if count turtles != number-of-people [
;    setup
;  ]
;
;  ;; record which button was pushed
;  set rewire-one? false
;  set rewire-all? true
;
;  ;; set up a variable to see if the network is connected
;  let success? false
;
;  ;; if we end up with a disconnected network, we keep trying, because the APL distance
;  ;; isn't meaningful for a disconnected network.
;  while [not success?] [
;    ;; kill the old lattice, reset neighbors, and create new lattice
;    ask links [ die ]
;    wire-them
;    set number-rewired 0
;
;    ask links [
;
;      ;; whether to rewire it or not?
;      if (random-float 1) < rewiring-probability
;      [
;        ;; "a" remains the same
;        let node1 end1
;        ;; if "a" is not connected to everybody
;        if [ count link-neighbors ] of end1 < (count turtles - 1)
;        [
;          ;; find a node distinct from node1 and not already a neighbor of node1
;          let node2 one-of turtles with [ (self != node1) and (not link-neighbor? node1) ]
;          ;; wire the new edge
;          ask node1 [ create-link-with node2 [ set color cyan  set rewired? true ] ]
;
;          set number-rewired number-rewired + 1  ;; counter for number of rewirings
;          set rewired? true
;        ]
;      ]
;      ;; remove the old edge
;      if (rewired?)
;      [
;        die
;      ]
;    ]
;
;    ;; check to see if the new network is connected and calculate path length and clustering
;    ;; coefficient at the same time
;    set success? do-calculations
;  ]
;end
;
;;; do-calculations reports true if the network is connected,
;;;   and reports false if the network is disconnected.
;;; (In the disconnected case, the average path length does not make sense,
;;;   or perhaps may be considered infinite)
;to-report do-calculations
;
;  ;; set up a variable so we can report if the network is disconnected
;  let connected? true
;
;
;  ;; find the path lengths in the network
;  find-path-lengths
;  let num-connected-pairs sum [length remove infinity (remove 0 distance-from-other-turtles)] of turtles
;
;  ;; In a connected network on number-of-people nodes, we should have N(N-1) measurements of distances between pairs,
;  ;; and none of those distances should be infinity.
;  ;; If there were any "infinity" length paths between nodes, then the network is disconnected.
;  ;; In that case, calculating the average-path-length doesn't really make sense.
;  ifelse ( num-connected-pairs != (count turtles * (count turtles - 1) ))
;  [
;      set average-path-length infinity
;      ;; report that the network is not connected
;      set connected? false
;  ]
;  [
;    set average-path-length (sum [sum distance-from-other-turtles] of turtles) / (num-connected-pairs)
;  ]
;  ;; find the clustering coefficient and add to the aggregate for all iterations
;  find-clustering-coefficient
;
;  ;; report whether the network is connected or not
;  report connected?
;end
;
;to-report in-neighborhood? [ hood ]
;  report ( member? end1 hood and member? end2 hood )
;end
;
;
;to find-clustering-coefficient
;  ifelse all? turtles [count link-neighbors <= 1]
;  [
;    ;; it is undefined
;    ;; what should this be?
;    set clustering-coefficient 0
;  ]
;  [
;    let total 0
;    ask turtles with [ count link-neighbors <= 1]
;      [ set node-clustering-coefficient "undefined" ]
;    ask turtles with [ count link-neighbors > 1]
;    [
;      let hood link-neighbors
;      set node-clustering-coefficient (2 * count links with [ in-neighborhood? hood ] /
;                                         ((count hood) * (count hood - 1)) )
;      ;; find the sum for the value at turtles
;      set total total + node-clustering-coefficient
;    ]
;    ;; take the average
;    set clustering-coefficient total / count turtles with [count link-neighbors > 1]
;  ]
;end
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Path length computations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;; Implements the Floyd Warshall algorithm for All Pairs Shortest Paths
;;; It is a dynamic programming algorithm which builds bigger solutions
;;; from the solutions of smaller subproblems using memoization that
;;; is storing the results.
;;; It keeps finding incrementally if there is shorter path through
;;; the kth node.
;;; Since it iterates over all turtles through k,
;;; so at the end we get the shortest possible path for each i and j.
;
;to find-path-lengths
;  ;; reset the distance list
;  ask turtles
;  [
;    set distance-from-other-turtles []
;  ]
;
;  let i 0
;  let j 0
;  let k 0
;  let node1 one-of turtles
;  let node2 one-of turtles
;  let node-count count turtles
;  ;; initialize the distance lists
;  while [i < node-count]
;  [
;    set j 0
;    while [j < node-count]
;    [
;      set node1 turtle i
;      set node2 turtle j
;      ;; zero from a node to itself
;      ifelse i = j
;      [
;        ask node1 [
;          set distance-from-other-turtles lput 0 distance-from-other-turtles
;        ]
;      ]
;      [
;        ;; 1 from a node to it's neighbor
;        ifelse [ link-neighbor? node1 ] of node2
;        [
;          ask node1 [
;            set distance-from-other-turtles lput 1 distance-from-other-turtles
;          ]
;        ]
;        ;; infinite to everyone else
;        [
;          ask node1 [
;            set distance-from-other-turtles lput infinity distance-from-other-turtles
;          ]
;        ]
;      ]
;      set j j + 1
;    ]
;    set i i + 1
;  ]
;  set i 0
;  set j 0
;  let dummy 0
;  while [k < node-count]
;  [
;    set i 0
;    while [i < node-count]
;    [
;      set j 0
;      while [j < node-count]
;      [
;        ;; alternate path length through kth node
;        set dummy ( (item k [distance-from-other-turtles] of turtle i) +
;                    (item j [distance-from-other-turtles] of turtle k))
;        ;; is the alternate path shorter?
;        if dummy < (item j [distance-from-other-turtles] of turtle i)
;        [
;          ask turtle i [
;            set distance-from-other-turtles replace-item j distance-from-other-turtles dummy
;          ]
;        ]
;        set j j + 1
;      ]
;      set i i + 1
;    ]
;    set k k + 1
;  ]
;end
;
;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Edge Operations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;
;;; creates a new lattice
;to wire-them
;  ;; iterate over the turtles
;  let m 0
;  while [m < count turtles]
;  [
;    ;; make edges with the next two neighbors
;    ;; this makes a lattice with average degree of 4
;    make-edge turtle m
;              turtle ((m + 1) mod count turtles)
;    make-edge turtle m
;              turtle ((m + 2) mod count turtles)
;    set m m + 1
;  ]
;end
;
;;; connects the two turtles
;to make-edge [node1 node2]
;  ask node1 [ create-link-with node2  [
;    set rewired? false
;  ] ]
;end
;
;;; END
@#$#@#$#@
GRAPHICS-WINDOW
450
349
729
629
-1
-1
2.98
1
10
1
1
1
0
0
0
1
-45
45
-45
45
1
1
1
ticks
60.0

BUTTON
14
15
77
48
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
89
15
174
48
go-once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
10
94
195
127
uncertainty-of-moderates
uncertainty-of-moderates
0.2
2
1.5
0.1
1
NIL
HORIZONTAL

SLIDER
10
135
193
168
proportion-of-extremists
proportion-of-extremists
0.025
0.3
0.2
0.025
1
NIL
HORIZONTAL

SLIDER
11
181
200
214
number-of-people
number-of-people
20
100
100.0
1
1
NIL
HORIZONTAL

SLIDER
229
94
401
127
min-opinion
min-opinion
-1.5
-0.5
-0.5
0.5
1
NIL
HORIZONTAL

SLIDER
228
138
400
171
max-opinion
max-opinion
0.5
1.5
1.0
0.5
1
NIL
HORIZONTAL

SLIDER
10
230
200
263
adjustment-rate
adjustment-rate
0.1
0.4
0.2
0.05
1
NIL
HORIZONTAL

SLIDER
228
275
401
308
uncertainty-of-extremists
uncertainty-of-extremists
0.025
0.25
0.125
0.025
1
NIL
HORIZONTAL

SLIDER
10
279
197
312
extreme-distance
extreme-distance
0.1
0.5
0.2
0.05
1
NIL
HORIZONTAL

SLIDER
230
185
401
218
min-moderate-opinion
min-moderate-opinion
-4.0
4.0
-0.3
0.005
1
NIL
HORIZONTAL

SLIDER
229
231
400
264
max-moderate-opinion
max-moderate-opinion
-4.0
4.0
0.8
0.005
1
NIL
HORIZONTAL

SLIDER
434
94
606
127
rewiring-probability
rewiring-probability
0
1
0.1
0.01
1
NIL
HORIZONTAL

CHOOSER
437
139
617
184
network-type
network-type
"fully connected" "small world" "preferential attachment"
2

BUTTON
188
15
288
48
go-forever
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
432
206
750
249
Convergence Indicator = 0.0 => Central Convergence Convergence Indicator = 0.5 => Bipolar Convergence\nConvergence Indicator = 1.0 => Single Extreme Convergence
11
0.0
1

TEXTBOX
535
332
685
350
Network Structure\n
11
0.0
1

CHOOSER
432
16
596
61
modality
modality
"random_opinions" "fromsurvey_opinions"
0

@#$#@#$#@
## WHAT IS IT?

This is an adaptation of the  Relative Agreement model of opinion dynamics (Deffuant et al. 2002), which models how extreme, minority views in a population can take hold in a population of individuals who are influenced by each others' opinions. This particular adaptation extends the Meadows and Cliff (2012) implementation of the Relative Agreement model in a manner that enables the exploration of the effect of the network structure among the agents.

## HOW IT WORKS

Agents are initialized with heterogeneous opinions expressed as a point on a continuum from -1 to 1. Each agent also has a certain level of confidence in their opinion, which is expressed by a bounded interval around the opinion -- i.e., the narrower the interval, the higher the confidence.  "Extremist" agents are defined as those with values less than -0.8 or greater than 0.8. Extremists have a high degree of confidence in their opinion. The remaining agents are classified as "moderate." Moderate agents have less confidence in their opinion than extremist agents. As the model runs, agents randomly interact, updating their opinion based on what they learn about their interaction partner's opinion. The amount an opinion is updated depends on the degree to which their confidence intervals overlap. 

The "opinions" plot shows the changes in the agents' opinions over time. There are three classes of outcomes that characterize the final beliefs of the population of agents: central convergence, bipolar convergence, and single extreme convergence. This information is quantified by a "convergence indicator" (as described in Cliff and Meadows 2012). When this indicator is approximately 0, central convergence has occurred; when the indicator is approximately 0.5, bipolar convergence has occurred; and when the indicator is approximately 1.0, single extreme convergence has occurred. A typical (though not only) exercise with the model is to understand under what conditions the extremists' views come to dominate the population (single extreme convergence).

## HOW TO USE IT
This model implements three initial network structures among agents, which bias the likelihood of interaction: fully connected graph (equivalent to the random mixing assuming by the original model), small world network, and preferential attachment (scale-free) network.

The NETWORK-TYPE drop-down box allows you to select the desired network.  If a small world network is selected, it will be created based on the value of the REWIRING-PROBABILITY slider. You can visualize the network structure using the graph titled "Network Structure." Calculated clustering coefficient and average path length are displayed in the CLUSTERING-COEFFICIENT and AVERAGE-PATH-LENGTH monitors respectively.

You can change the number of people in the network using the NUMBER-OF-PEOPLE slider. The PROPORTION-OF-EXTREMISTS slider determines the proportion of these people who have extreme opinions (extremists are displayed in red). All agents have opinions between the values of the MIN-OPINION and MAX-OPINION sliders, but extremists' opinions are within EXTREME-DISTANCE of these two extremes. Moderates' opinions are between the values of the MIN-MODERATE-OPINION and MAX-MODERATE-OPINION sliders, which automatically update based off the value of EXTREME-DISTANCE (moderates are displayed in white). You can adjust moderates' and extremists' uncertainty using the UNCERTAINTY-OF-MODERATES and UNCERTAINTY-OF-EXTREMISTS sliders respectively. You can also modify the ADJUSTMENT-RATE slider, which controls the extent to which interactions cause opinions to change.

After adjusting the parameters, press the SETUP button to create and initialize the people. Click the GO-ONCE button to perform one iteration; click the GO-FOREVER button to perform iterations in a loop. You can view output in the plot titled "opinions."

## CREDITS AND REFERENCES

Deffuant, G., Amblard, F., Weisbuch, G. and Faure, T. (2002). How can extremism prevail? A study based on the relative agreement interaction model. Journal of Artificial Societies and Social Simulation 5(4): 1 http://jasss.soc.surrey.ac.uk/5/4/1.html.

Meadows, Michael and Cliff, Dave (2012) 'Reexamining the Relative Agreement Model of Opinion Dynamics' Journal of Artificial Societies and Social Simulation 15 (4) 4 <http://jasss.soc.surrey.ac.uk/15/4/4.html>.

Wilensky, U. (2005). NetLogo Preferential Attachment model. http://ccl.northwestern.edu/netlogo/models/PreferentialAttachment. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Wilensky, U. (2005). NetLogo Small Worlds model. http://ccl.northwestern.edu/netlogo/models/SmallWorlds. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.


		
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
