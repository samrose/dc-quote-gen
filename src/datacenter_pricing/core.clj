(ns datacenter-pricing.core
  (:require [clara.rules :refer [defrule defquery mk-session fire-rules insert insert! insert-unconditional! query]]
            [clara.rules.accumulators :as acc])
  (:gen-class))

(defrecord RackRequest [type power-kw network-speed ip-blocks cross-connects service-level term compliance remote-hands-hours credit-check])
(defrecord PricingResult [component cost recurring?])

(defquery get-results
  []
  [?result <- PricingResult])

(defquery get-recurring-total
  []
  [?total <- (acc/sum :cost) :from [PricingResult (= true recurring?) (not= component "Term Discount")]])

(def pricing-data
  {:base_infrastructure
   {:rack_space
    {:per_u_monthly 85.00
     :quarter_rack_monthly 950.00
     :half_rack_monthly 1750.00
     :full_rack_monthly 3200.00}
    :power
    {:per_kw_monthly 350.00
     :setup_fee 250.00
     :redundant_power_multiplier 1.5}
    :network
    {:1gbps_monthly 300.00
     :10gbps_monthly 1200.00
     :ip_block_24 50.00
     :cross_connect_setup 500.00
     :cross_connect_monthly 100.00}
    :cooling
    {:included_with_power true
     :additional_cooling_per_kw 150.00}}
   :service_levels {:basic {:monthly_fee 0.00
                           :response_time_hours 24
                           :sla_uptime 99.9
                           :included_remote_hands_hours 0}
                    :business {:monthly_fee 500.00
                              :response_time_hours 4
                              :sla_uptime 99.95
                              :included_remote_hands_hours 5}
                    :enterprise {:monthly_fee 2000.00
                               :response_time_hours 1
                               :sla_uptime 99.99
                               :included_remote_hands_hours 20}}
   :remote_hands {:business_hours_rate 150.00
                 :after_hours_rate 250.00
                 :minimum_increment_hours 0.5}
   :contract_terms {:setup_fees {:quarter_rack 500.00
                                :half_rack 750.00
                                :full_rack 1000.00}
                    :term_discounts {:12_month 0.00
                                   :24_month 0.05
                                   :36_month 0.10}
                    :security_deposit {:standard 1000.00
                                     :with_credit_check 0.00}}
   :compliance_addons {:sox 500.00
                      :hipaa 750.00
                      :pci 1000.00
                      :audit_assistance_hourly 250.00}})

(defrule calculate-rack-cost
  [RackRequest (= ?type type)]
  =>
  (println "Processing rack cost for type:" ?type)
  (let [cost (get-in pricing-data [:base_infrastructure :rack_space (keyword (str ?type "_monthly"))])
        setup-fee (get-in pricing-data [:contract_terms :setup_fees (keyword ?type)])]
    (insert! (->PricingResult "Rack Monthly" cost true))
    (insert! (->PricingResult "Rack Setup" setup-fee false))))

(defrule calculate-power-cost
  "Calculate power costs"
  [RackRequest (= ?power-kw power-kw)]
  =>
  (let [monthly-cost (* ?power-kw (get-in pricing-data [:base_infrastructure :power :per_kw_monthly]))
        setup-fee (get-in pricing-data [:base_infrastructure :power :setup_fee])]
    (insert! (->PricingResult "Power Monthly" monthly-cost true))
    (insert! (->PricingResult "Power Setup" setup-fee false))))

(defrule calculate-network-cost
  [RackRequest (= ?speed network-speed) (= ?blocks ip-blocks) (= ?connects cross-connects)]
  =>
  (let [speed-key (keyword (str ?speed "gbps_monthly"))
        monthly-cost (+ (get-in pricing-data [:base_infrastructure :network speed-key])
                       (* ?blocks (get-in pricing-data [:base_infrastructure :network :ip_block_24]))
                       (* ?connects (get-in pricing-data [:base_infrastructure :network :cross_connect_monthly])))
        setup-cost (* ?connects (get-in pricing-data [:base_infrastructure :network :cross_connect_setup]))]
    (insert! (->PricingResult "Network Monthly" monthly-cost true))
    (insert! (->PricingResult "Network Setup" setup-cost false))))

(defrule calculate-service-level-cost
  [RackRequest (= ?level service-level)]
  =>
  (let [monthly-fee (get-in pricing-data [:service_levels (keyword ?level) :monthly_fee])]
    (insert! (->PricingResult "Service Level Monthly" monthly-fee true))))

(defrule calculate-remote-hands-cost
  [RackRequest (= ?hours remote-hands-hours) (= ?level service-level)]
  =>
  (let [included-hours (get-in pricing-data [:service_levels (keyword ?level) :included_remote_hands_hours])
        billable-hours (max 0 (- ?hours included-hours))
        hourly-rate (get-in pricing-data [:remote_hands :business_hours_rate])
        cost (* billable-hours hourly-rate)]
    (when (pos? billable-hours)
      (insert! (->PricingResult "Remote Hands" cost false)))))

(defrule calculate-compliance-costs
  [RackRequest (= ?compliance compliance)]
  =>
  (doseq [requirement ?compliance]
    (when-let [cost (get-in pricing-data [:compliance_addons (keyword requirement)])]
      (insert! (->PricingResult (str "Compliance - " (name requirement)) cost true)))))

(defrule calculate-security-deposit
  [RackRequest (= ?credit-check credit-check)]
  =>
  (let [deposit (if ?credit-check
                 (get-in pricing-data [:contract_terms :security_deposit :with_credit_check])
                 (get-in pricing-data [:contract_terms :security_deposit :standard]))]
    (insert! (->PricingResult "Security Deposit" deposit false))))

(defrule apply-term-discount
  [RackRequest (= ?term term)]
  [:not [PricingResult (= component "Term Discount")]]
  [?total <- (acc/sum :cost) :from [PricingResult (= true recurring?) (not= component "Term Discount")]]
  =>
  (let [discount-rate (get-in pricing-data [:contract_terms :term_discounts (keyword (str ?term "_month"))])]
    (when (and (number? ?total) (pos? ?total) (number? discount-rate))
      (let [discount-amount (* ?total discount-rate)]
        (println "Applying one-time term discount of:" (- discount-amount))
        (insert-unconditional! (->PricingResult "Term Discount" (- discount-amount) true))))))

(def rules [calculate-rack-cost
            calculate-power-cost
            calculate-network-cost
            calculate-service-level-cost
            calculate-remote-hands-cost
            calculate-compliance-costs
            calculate-security-deposit
            apply-term-discount
            get-results])

(defn calculate-quote [request]
  (println "Starting quote calculation...")
  (let [session (-> (mk-session rules :cache false)
                    (insert request)
                    (fire-rules))
        results (query session get-results)
        valid-results (remove nil? (map :?result results))]
    (println "Got results:" (count valid-results))
    {:monthly (reduce + 0 (map :cost (filter :recurring? valid-results)))
     :one-time (reduce + 0 (map :cost (remove :recurring? valid-results)))
     :details (map #(select-keys % [:component :cost :recurring?]) valid-results)}))

(defn -main [& args]
  (println "Starting main...")
  (let [request (->RackRequest 
                 "quarter_rack"    ; type
                 2.5              ; power-kw
                 1                ; network-speed (Gbps)
                 1                ; ip-blocks
                 2                ; cross-connects
                 "business"       ; service-level
                 24               ; term (months)
                 ["sox" "pci"]    ; compliance
                 10               ; remote-hands-hours
                 true)]          ; credit-check
    (println "Created request...")
    (let [result (calculate-quote request)]
      (println "\nDatacenter Pricing Quote:")
      (println "------------------------")
      (println "\nMonthly Recurring Costs:" (:monthly result))
      (println "One-Time Costs:" (:one-time result))
      (println "\nDetailed Breakdown:")
      (doseq [item (:details result)]
        (println (format "%-20s $%10.2f  (%s)"
                        (:component item)
                        (:cost item)
                        (if (:recurring? item) "Monthly" "One-Time")))))))