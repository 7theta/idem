;;   Copyright (c) 7theta. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;;   which can be found in the LICENSE file at the root of this
;;   distribution.
;;
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any others, from this software.

(ns idem.geo.maxmind
  (:require [utilis.map :refer [compact]]
            [utilis.exception :refer [with-exception->value]]
            [clojure.java.io :as io]
            [integrant.core :as ig])
  (:import  [java.net InetAddress]
            [com.maxmind.db CHMCache]
            [com.maxmind.geoip2 DatabaseReader DatabaseReader$Builder]
            [com.maxmind.geoip2.model CityResponse]
            [com.maxmind.geoip2.record Location City Subdivision Country
             Continent Traits]))

;;; Public

(declare ip-locator)

(defmethod ig/init-key ::ip-locator [_ {:keys [db-file]}]
  (ip-locator db-file))

(defn ip-locator
  "Creates an instance of a locator that uses a Maxmind database to
  resolve the location of an IP Address. The locator works with both
  the Lite and the Commercial versions of the Maxmind database
  in binary format.

  Maxmind databases can be obtained from:
    https://dev.maxmind.com/geoip/geoip2/downloadable/"
  [db-file]
  (-> db-file io/file
      DatabaseReader$Builder.
      (.withCache (CHMCache.))
      .build))

(declare parse-response)

(defn locate
  "Returns the geo-location for the supplied v4 'ip' address. If a
  location cannot be found, the optional 'default' value is returned. If
  a 'default' is not provided, a nil is returned.

  Note that only fields present in the database are returned and the
  responses will differ between the Lite and the Commercial versions
  of the database."
  ([locator ip] (locate locator ip nil))
  ([locator ip default]
   (if-let [reader ^DatabaseReader locator]
     (with-exception->value [Exception default]
       (->> ip InetAddress/getByName
            (.city reader)
            parse-response))
     (throw (IllegalArgumentException. "Locator is in an invalid state.")))))

;;; Implementation

(defn- parse-response
  [^CityResponse response]
  (compact
   (let [^Location location (.getLocation response)
         parse-country (fn [^Country country]
                         {:name (.getName country)
                          :iso-code (.getIsoCode country)})]
     {:geo {:lat (.getLatitude location)
            :lon (.getLongitude location)
            :accuracy {:radius (.getAccuracyRadius location)}}
      :time-zone {:name (.getTimeZone location)}
      :city (let [^City city (.getCity response)]
              {:name (.getName city)})
      :postal {:code (.getCode (.getPostal response))}
      :sub-division (let [^Subdivision region (.getMostSpecificSubdivision response)]
                      {:name (.getName region)
                       :iso-code (.getIsoCode region)})
      :country (parse-country (.getCountry response))
      :continent (let [^Continent continent (.getContinent response)]
                   {:name (.getName continent)
                    :iso-code (.getCode continent)})
      :represented-country (let [country (.getRepresentedCountry response)]
                             (assoc (parse-country country)
                                    :type (.getType country)))
      :registered-country (parse-country (.getCountry response))
      :traits (let [^Traits traits (.getTraits response)]
                {:asn (.getAutonomousSystemNumber traits)
                 :aso (.getAutonomousSystemOrganization traits)
                 :isp (.getIsp traits)
                 :domain (.getDomain traits)
                 :anonymous-proxy (.isAnonymousProxy traits)
                 :satalite-provider (.isSatelliteProvider traits)
                 :organization (.getOrganization traits)
                 :user-type (.getUserType traits)})})))
