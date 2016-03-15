(ns ni-apd.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io])
  (:gen-class))

;; Things to calculate distance from lat/lon positions.

(defn- deg2rad [deg]
  (/ (* deg (. Math PI)) 180))

(defn- rad2deg [rad]
  (/ (* rad 180) (. Math PI)))

(defn get-distance [lat1 lon1 lat2 lon2]
  (+ (* (Math/sin (deg2rad lat1)) (Math/sin (deg2rad lat2)))
     (* (Math/cos (deg2rad lat1))
        (Math/cos (deg2rad lat2))
        (Math/cos (deg2rad (- lon1 lon2))))
     ))

(defn calc-distance [lat1 lon1 lat2 lon2]
  (int (* 60 1.1515 (rad2deg (Math/acos (get-distance lat1 lon1 lat2 lon2))))))


;; Things that will load files for us.
(defn load-csv [filename]
  (with-open [in-file (io/reader (.toString (io/resource filename)))]
    (doall
     (csv/read-csv in-file))))

;; Static data like field names.

(def airport-fields [:airportid :name :city :country :iata :icao :lat :lon :alt :timezone :dst :tz])

(def route-fields [:airline :airlineid :source-airport :source-airport-id :dest-airport :dest-airport-id :codeshare :stops :equip])

(def apd-threshold 2000) ;; 2000 miles thresold for direct long haul flights for NI APD.

;; Short or long haul flight?

(defn is-long-haul? [distance]
  (> distance apd-threshold))

;; Things that do the actual work.

(defn load-airports [filename]
  (->> (load-csv filename)
       (map #(zipmap airport-fields %))))

(defn load-routes [filename]
  (->> (load-csv filename)
       (map #(zipmap route-fields %))))

(defn get-airport [iata-code airports]
  (first (filter #(= iata-code (:iata %)) airports)))

(defn find-routes [departure-airport]
  (let [airports (load-airports "airports.csv")
        routes (load-routes "routes.csv")
        dept-airport (get-airport departure-airport airports)
        matching-routes (filter #(= (:iata dept-airport) (:source-airport %)) routes)]
    (->> matching-routes
         (map (fn [route] (try
                            (let [dest-airport (get-airport (:dest-airport route) airports)
                                  distance (calc-distance (Double/parseDouble (:lat dept-airport))
                                                          (Double/parseDouble (:lon dept-airport))
                                                          (Double/parseDouble (:lat dest-airport))
                                                          (Double/parseDouble (:lon dest-airport)))
                                  long-haul (is-long-haul? distance)]
                              {:dept (:name dept-airport)
                               :dept-iata (:iata dept-airport)
                               :dept-lat (:lat dept-airport)
                               :dept-lon (:lon dept-airport)
                               :dest (:name dest-airport)
                               :dest-iata (:iata dest-airport)
                               :dest-lat (:lat dest-airport)
                               :dest-lon (:lon dest-airport)
                               :distance distance
                               :long-haul long-haul
                               :airline (:airline route)})
                            (catch Exception e )))))))

(defn output-csv [departure-airport]
  (mapv (fn [row] (vector (:dept row)
                          (:dept-iata row)
                          (:dept-lat row)
                          (:dept-lon row)
                          (:dest row)
                          (:dest-iata row)
                          (:dest-lat row)
                          (:dest-lon row)
                          (:distance row)
                          (:long-haul row)
                          (:airline row))) (find-routes departure-airport)))

(defn get-longhaul-average [departure-airport]
  (let [routes (find-routes departure-airport)]
    (double (* 100 (/ (count (filter #(= true (:long-haul %)) routes)) (count routes))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
