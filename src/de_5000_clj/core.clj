(ns de-5000-clj.core
  (:require [serial.core :as c]
            [serial.util :as u]))

;; pecket header
(def byte-0 0)
(def byte-1 13)

;; packet footer
(def byte-15 13)
(def byte-16 10)

(declare port)

(defonce packet (atom (into [] (repeat 17 0))))

(defn handler [on-packet]
  (fn [input]
    (let [v              (.read input)
          updated-packet (swap! packet
                                (fn [p]
                                  (into []
                                        (drop 1 (conj p v)))))]

      (if (and (= (get updated-packet 0)
                  byte-0)
               (= (get updated-packet 1)
                  byte-1)
               (= (get updated-packet 15)
                  byte-15)
               (= (get updated-packet 16)
                  byte-16))
        (on-packet updated-packet)))))

(defn start [tty-path & {:keys [on-packet]
                         :or {on-packet println}}]
  (let [port (c/open tty-path ;;"/dev/ttyUSB0"
                     :baud-rate 9200
                     :databits  8
                     :parity c/PARITY_NONE
                     :stopbits  c/STOPBITS_1)]
    (c/listen! port (handler on-packet))))

(defn stop [port]
  (c/unlisten! port))

(defn get-flags [flags]
  ;; * 0x02: flags
  ;; *         bit 0 = hold enabled
  ;; *         bit 1 = reference shown (in delta mode)
  ;; *         bit 2 = delta mode
  ;; *         bit 3 = calibration mode
  ;; *         bit 4 = sorting mode
  ;; *         bit 5 = LCR mode
  ;; *         bit 6 = auto mode
  ;; *         bit 7 = parallel measurement (vs. serial)
  {:hold?        (= 1 (bit-and 2r00000001 flags))
   :reference?   (= 1 (bit-and 2r00000010 flags))
   :delta?       (= 1 (bit-and 2r00000100 flags))
   :calibration? (= 1 (bit-and 2r00001000 flags))
   :sorting?     (= 1 (bit-and 2r00010000 flags))
   :lcr?         (= 1 (bit-and 2r00100000 flags))
   :auto?        (= 1 (bit-and 2r01000000 flags))
   :parallel?    (= 1 (bit-and 2r10000000 flags))})

(defn get-config [config]
  ;; *         bit 0-4 = ??? (0x10)
  ;; *         bit 5-7 = test frequency
  ;; *                     0 = 100 Hz
  ;; *                     1 = 120 Hz
  ;; *                     2 = 1 kHz
  ;; *                     3 = 10 kHz
  ;; *                     4 = 100 kHz
  ;; *                     5 = 0 Hz (DC)
  {:frequency (case (bit-and config 2r11100000)
                0 100
                32 120
                64 1000
                96 10000
                128 100000
                0)})

(defn get-tolerance [tolerance]
  ;; * 0x04: tolerance (sorting mode)
  ;; *         0 = not set
  ;; *         3 = +-0.25%
  ;; *         4 = +-0.5%
  ;; *         5 = +-1%
  ;; *         6 = +-2%
  ;; *         7 = +-5%
  ;; *         8 = +-10%
  ;; *         9 = +-20%
  ;; *        10 = -20+80%
  {:percentage "not implemented"})

(defn get-measurement-details [msb lsb info status]
  {:msb  msb
   :lsb  lsb
   :info {:decimal-point-multiplier (bit-and info 2r00000111)
          ;; *           bit 3-7 = unit
          ;; *                       0 = no unit
          ;; *                       1 = Ohm
          ;; *                       2 = kOhm
          ;; *                       3 = MOhm
          ;; *                       5 = uH
          ;; *                       6 = mH
          ;; *                       7 = H
          ;; *                       8 = kH
          ;; *                       9 = pF
          ;; *                       10 = nF
          ;; *                       11 = uF
          ;; *                       12 = mF
          ;; *                       13 = %
          ;; *                       14 = degree

          :unit   (let [v (bit-shift-right info 3)]
                    (case v
                      0  :no-unit
                      1  :ohm
                      2  :kohm
                      3  :Mohm
                      5  :uH
                      6  :mH
                      7  :H
                      8  :kH
                      9  :pF
                      10 :nF
                      11 :uF
                      12 :mF
                      13 :percent
                      14 :degree
                      :error))
          :status (let [v (bit-and status 2r00001111)]
                    (case v
                      0  :normal
                      1  :blank
                      2  :lines
                      3  :outside
                      7  :pass
                      8  :fail
                      9  :open
                      10 :shorted
                      :unknown))}})

(defn get-primary-measurement [method
                               msb
                               lsb
                               info
                               status]
  ;; *   0x05: measured quantity
  ;; *           1 = inductance
  ;; *           2 = capacitance
  ;; *           3 = resistance
  ;; *           4 = DC resistance
  (merge {:method (case method
                    1 :inductance
                    2 :capacitance
                    3 :ac-resistance
                    4 :dc-resistance)}
         (get-measurement-details msb lsb info status)))


(defn get-secondary-measurement [method
                                 msb
                                 lsb
                                 info
                                 status]
  ;; *   0x05: measured quantity
  ;; *           1 = inductance
  ;; *           2 = capacitance
  ;; *           3 = resistance
  ;; *           4 = DC resistance
  (merge {:method (case method
                    0 :none
                    1 :dissipation-factor
                    2 :quality-factor
                    3 :esr
                    4 :phas3-angle)}
         (get-measurement-details msb lsb info status)))

(defn packet-handler
  [[_
    _
    flags
    config
    tolerance
    measurement-method
    measurement-msb
    measurement-lsb
    measurement-info
    measurement-status
    secondary-measurement-method
    secondary-measurement-msb
    secondary-measurement-lsb
    secondary-measurement-info
    secondary-measurement-status
    _
    _]]
  {:flags                 (get-flags flags)
   :config                (get-config config)
   :tolerance             (get-tolerance tolerance)
   :primary-measurement   (get-primary-measurement measurement-method
                                                   measurement-msb
                                                   measurement-lsb
                                                   measurement-info
                                                   measurement-status)
   :secondary-measurement (get-secondary-measurement secondary-measurement-method
                                                     secondary-measurement-msb
                                                     secondary-measurement-lsb
                                                     secondary-measurement-info
                                                     secondary-measurement-status)})
