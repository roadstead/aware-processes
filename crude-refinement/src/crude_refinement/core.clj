(ns crude-refinement.core)

(def steady-state-refinery
  "The yield of one barrel of crude oil from a refinery in steady-state
  equilibrium.

  see also (https://www.eia.gov/dnav/pet/pet_pnp_pct_dc_nus_pct_m.htm)[Refinery yield]"
  {:input {:units :imp-gallons
           :crude 42}
   :yield {:units     :imp-gallons
           :gasoline  {:dist 0.45}
           :diesel    {:dist 0.25}
           :kerosene  {:dist 0.09}
           :hgl       {:dist 0.04}
           :residuals {:dist 0.02}
           :other     {:dist 0.13}}})

(defn barrels-per-product
  [product]
  (let [brl steady-state-refinery
        brl-vol (-> brl :input :crude)
        yield (-> brl :yield product :dist)]
    (->> yield
         (* brl-vol)
         (/ 1))))

(defn round2
  "Round a double to the given precision (number of significant digits)"
  [d precision]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn distill-fraction
  "This is an approximation of fractional distillation based on a
  fictional distribution found in this blog post: 

  [What’s In A Crude Oil Barrel?](https://www.breakthroughfuel.com/blog/crude-oil-barrel/)
  "
  [brls d]
  (-> brls
      (* d)
      (round2 2)))

(defn refine-crude-oil [brls]
  (let [vol-crude (-> steady-state-refinery :input :crude)]
    (letfn [(refine-product [prod]
              (let [vol (* vol-crude (prod :dist))]
                (distill-fraction brls vol)))]
      (-> steady-state-refinery
          (update-in [:input :crude] (fn [vol] (distill-fraction brls vol)))
          (update-in [:yield :gasoline] refine-product)
          (update-in [:yield :diesel] refine-product)
          (update-in [:yield :kerosene] refine-product)
          (update-in [:yield :hgl] refine-product)
          (update-in [:yield :residuals] refine-product)
          (update-in [:yield :other] refine-product)))))

(comment 
  "How many barrels are required to produce a volume of the distillate,
based on the steady-state description of a refinery in equilibrium in
terms of barrels of oil.

volume-of-dist = bbl-vol * dist
bbl-vol = v / dist")

(defn get-petroleum-product-2
  "Takes a refinery product and desired volume and based on the
  distribution of a barrels output, calculates the number of barrels
  required, and returns the raw materials required."
  [product volume]
  (let [dist (-> steady-state-refinery :yield product :dist)
        crude (-> steady-state-refinery :input :crude)
        bbl-vol (-> volume (/ dist) float)
        bbls (/ bbl-vol crude)]
    (refine-crude-oil bbls)))

(defn pump-gas [vol]
  (get-petroleum-product-2 :gasoline vol))

(comment
  (let [us-2022-gasoline (* (Math/pow 10 9) 135.06)]
    (pump-gas us-2022-gasoline))

  (def num-licensed-drivers-us 2.328E8)
  (def combustion-vehicles-us 3.00E8)
  (def fuel-economy-avg-2020-mpg 25.4)
  (def daily-travel-miles-us 25.9)

  (def us-2020-gallons (-> combustion-vehicles-us
                           (* daily-travel-miles-us)
                           (/ fuel-economy-avg-2020-mpg)
                           (* 365)))
  )

(comment "see [Barrels of oil consumed](https://www.epa.gov/energy/greenhouse-gases-equivalencies-calculator-calculations-and-references#:~:text=The%20average%20carbon%20dioxide%20coefficient,gallon%20barrel%20(EPA%202022).)")
  
(def g-co2-gal-gasoline 
  "For reference, to obtain the number of grams of CO2 emitted per
  gallon of gasoline combusted, the heat content of the fuel per
  gallon can be multiplied by the kg CO2 per heat content of the
  fuel."
  8.887)
(def g-co2-gal-deisel 
  "For reference, to obtain the number of grams of CO2 emitted per
  gallon of diesel combusted, the heat content of the fuel per gallon
  can be multiplied by the kg CO2 per heat content of the fuel."
  10.180)
(def avg-pax-mpg-2020 22.9)
(def avg-vmt-2019 11520)
(def co2-mt-gallon (* (Math/pow 10 -3) 8.89))
(def co2-total-gg 
  "In 2019, the ratio of carbon dioxide emissions to total greenhouse
  gas emissions (including carbon dioxide, methane, and nitrous oxide,
  all expressed as carbon dioxide equivalents) for passenger vehicles
  was 0.993 (EPA 2022)."
  0.993)
(def annual-mt-gg-pax
  (-> (/ avg-vmt-2019 avg-pax-mpg-2020)
      (* co2-mt-gallon)
      (/ co2-total-gg)))
(def avg-cc-natural-gas 
  "The average carbon coefficient of pipeline natural gas burned in 2020
  is 14.43 kg carbon per mmbtu (EPA 2022). The fraction oxidized to
  CO2 is assumed to be 100 percent (IPCC 2006)."
  14.43)
(def avg-mmbtu-barrel-oil
  "The average heat content of crude oil is 5.80 mmbtu per barrel (EPA
  2022)"
  5.80)
(def avg-cc-crude-oil 
  "The average carbon coefficient of crude oil is 20.33 kg carbon per
  mmbtu (EPA 2022)."
  22.33)
(def co2-c-mol
  "ratio of the molecular weight of carbon dioxide to that of
  carbon (44/12)."
  44/12)
(defn co2-therm
  "Carbon dioxide emissions per therm are determined by converting
  million British thermal units (mmbtu) to therms, then multiplying
  the carbon coefficient times the fraction oxidized times the ratio
  of the molecular weight of carbon dioxide to carbon (44/12)."
  [cc]
  (let []
    (-> 0.1 ;;mmbtu/therm
        (* cc)
        (* co2-c-mol))))
  
(def mt-co2-barrel
  "Carbon dioxide emissions per barrel of crude oil are determined by
multiplying heat content times the carbon coefficient times the
fraction oxidized times the ratio of the molecular weight of carbon
dioxide to that of carbon (44/12)."
  (-> avg-mmbtu-barrel-oil
      (* avg-cc-crude-oil)
      (* co2-c-mol)
      (/ 1000) ;;mt/kg
      ))
