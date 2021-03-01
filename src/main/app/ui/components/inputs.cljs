(ns app.ui.components.inputs
  (:require
   ["react-native" :refer [View TextInput Text Animated TouchableOpacity]]
   ["react-native-check-box" :default CheckBox]
   [keechma.next.helix.core :refer [with-keechma use-meta-sub dispatch]]
   [app.lib :refer [$ defnc convert-style]]
   [helix.dom :as d]
   [helix.hooks :as hooks]
   [oops.core :refer [ocall oget oset!]]
   [keechma.next.controllers.router :as router]
   [app.tailwind :refer [tw]]
   ["react" :as react]
   [app.rn.animated :as animated]
   ["react-dom" :as rdom]
   [app.ui.svgs :refer [Svg]]
   [app.utils.os-utils :refer [is-ios]]
   [keechma.next.controllers.form :as form]
   [app.ui.components.picker :refer [PickerComponent]]
   [app.validators :refer [get-validator-message]]))


(defn get-element-props
  [default-props props]
  (let [element-props (into {} (filter (fn [[k v]] (simple-keyword? k)) props))]
    (reduce-kv
     (fn [m k v]
       (let [prev-v (get k m)
             val (cond (and (fn? prev-v) (fn? v))
                       (fn [& args] (apply prev-v args) (apply v args))
                       (and (= :class k) (:class m)) (flatten [v (:class m)])
                       :else v)]
         (assoc m k val)))
     default-props
     element-props)))

;; ERRORS
(defnc ErrorsRenderer [{:keechma.form/keys [controller]
                        :input/keys        [attr]
                        :as                props}]
  (let [errors-getter (hooks/use-callback [attr] #(form/get-errors-in % attr))
        errors (use-meta-sub props controller errors-getter)]
    (when-let [errors' (get-in errors [:$errors$ :failed])]
      ($ View {:style {:color "red" :width "100%" :font-size 20}}
         (map-indexed (fn [i e] ($ Text {:key i} (get-validator-message e)))
                      errors')))))

(def Errors (with-keechma ErrorsRenderer))

;; TEXT
(defnc TextInputRenderer [{:keechma.form/keys [controller]
                           :input/keys        [attr]
                           :as                props}]
  (let [element-props (get-element-props {} props)(ns app.ui.components.inputs
                                                    (:require
                                                     ["react-native" :refer [View TextInput Text Animated TouchableOpacity]]
                                                     ["react-native-check-box" :default CheckBox]
                                                     [keechma.next.helix.core :refer [with-keechma use-meta-sub dispatch]]
                                                     [app.lib :refer [$ defnc convert-style]]
                                                     [helix.dom :as d]
                                                     [helix.hooks :as hooks]
                                                     [oops.core :refer [ocall oget oset!]]
                                                     [keechma.next.controllers.router :as router]
                                                     [app.tailwind :refer [tw]]
                                                     ["react" :as react]
                                                     [app.rn.animated :as animated]
                                                     ["react-dom" :as rdom]
                                                     [app.ui.svgs :refer [Svg]]
                                                     [app.utils.os-utils :refer [is-ios]]
                                                     [keechma.next.controllers.form :as form]
                                                     [app.ui.components.picker :refer [PickerComponent]]
                                                     [app.validators :refer [get-validator-message]]))

(defn get-element-props
  [default-props props]
  (let [element-props (into {} (filter (fn [[k v]] (simple-keyword? k)) props))]
    (reduce-kv
     (fn [m k v]
       (let [prev-v (get k m)
             val (cond (and (fn? prev-v) (fn? v))
                       (fn [& args] (apply prev-v args) (apply v args))
                       (and (= :class k) (:class m)) (flatten [v (:class m)])
                       :else v)]
         (assoc m k val)))
     default-props
     element-props)))

;; ERRORS
(defnc ErrorsRenderer [{:keechma.form/keys [controller]
                        :input/keys        [attr]
                        :as                props}]
  (let [errors-getter (hooks/use-callback [attr] #(form/get-errors-in % attr))
        errors (use-meta-sub props controller errors-getter)]
    (when-let [errors' (get-in errors [:$errors$ :failed])]
      ($ View {:style [(tw :text-red :w-full :font-bold :text-sm)]}
         (map-indexed (fn [i e] ($ Text {:key i
                                         :style [(tw :text-red)]} (get-validator-message e)))
                      errors')))))

(def Errors (with-keechma ErrorsRenderer))


;; TEXT
(defnc TextInputRenderer [{:keechma.form/keys [controller]
                           :input/keys        [attr]
                           :as                props}]
  (let [element-props (get-element-props {} props)
        value-getter (hooks/use-callback [attr] #(form/get-data-in % attr))
        value (use-meta-sub props controller value-getter)
        [focused? set-focused] (hooks/use-state false)]))

(def InputText (with-keechma TextInputRenderer))


;; PASSWORD
(defnc PasswordInputRenderer [{:keechma.form/keys [controller]
                               :input/keys        [attr]
                               :as                props}]
  (let [element-props (get-element-props {} props)
        icon-end (:icon-end props)
        value-getter (hooks/use-callback [attr] #(form/get-data-in % attr))
        value (use-meta-sub props controller value-getter)
        [password? set-password] (hooks/use-state true)
        [focused? set-focused] (hooks/use-state false)]))

(def InputPassword (with-keechma PasswordInputRenderer))


;; CHECKBOX
(defnc CheckboxRenderer [{:keechma.form/keys [controller]
                          :input/keys        [attr]
                          :as                props}]
  (let [element-props (get-element-props {} props)
        value-getter (hooks/use-callback [attr] #(form/get-data-in % attr))
        value' (use-meta-sub props controller value-getter)
        value (if (nil? value') false value')]))


;; SELECT
(defnc SelectRenderer [{:keechma.form/keys [controller]
                        :input/keys        [attr on-change]
                        :as                props}]
  (let [element-props (get-element-props {} props)
        value-getter (hooks/use-callback [attr] #(form/get-data-in % attr))
        value (use-meta-sub props controller value-getter)]))

(def Select (with-keechma SelectRenderer))

;;__________________________________________________________________________

(def Checkbox (with-keechma CheckboxRenderer))

(defmulti input (fn [props] (:input/type props)))
(defmethod input :text [props] ($ InputText {& props}))
(defmethod input :password [props] ($ InputPassword {& props}))
(defmethod input :checkbox [props] ($ Checkbox {& props}))
(defmethod input :select [props] ($ Select {& props}))


(defmulti wrapped-input (fn [props] (:input/type props)))
(defmethod wrapped-input :default [props] (input props))

(def AnimatedView (oget Animated :View))

(defmethod wrapped-input :text [props]
  ($ AnimatedView {:style [(tw :w-full)
                           {:animation/top (:top props)
                            :animation/opacity (:opacity props)}]}
     (input props)
     ($ Errors {& props})))

(defmethod wrapped-input :password [props]
  ($ AnimatedView {:style [(tw :w-full)
                           {:animation/top (:top props)
                            :animation/opacity (:opacity props)}]}
     (input props)
     ($ Errors {& props})))

(defmethod wrapped-input :checkbox [props]
  ($ AnimatedView {:style [(tw :w-full)
                           {:animation/top (:top props)
                            :animation/opacity (:opacity props)}]}
     (input props)
     ($ Errors {& props})))

(defmethod wrapped-input :select [props]
  ($ AnimatedView {:style [(tw :w-full)
                           {:animation/top (:top props)
                            :animation/opacity (:opacity props)}]}
     (input props)
     ($ Errors {& props})))
        value-getter (hooks/use-callback [attr] #(form/get-data-in % attr))
        value (use-meta-sub props controller value-getter)]))

(def InputText (with-keechma TextInputRenderer))

(defmulti input (fn [props] (:input/type props)))
(defmethod input :text     [props] ($ InputText {& props}))

(defmulti wrapped-input (fn [props] (:input/type props)))
(defmethod wrapped-input :default [props] (input props))

(defmethod wrapped-input :text [props]
  ($ View {:style [(tw :w-full)]}
     (input props)
     ($ Errors {& props})))