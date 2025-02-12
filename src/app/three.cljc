(ns app.three
  #?(:cljs (:require-macros [app.three :refer [gen_factory]]))
  (:require
   [hyperfiddle.electric :as e]
   #?(:cljs ["three" :as three])
   [contrib.missionary-contrib :as mx]
   [missionary.core :as m]
   [hyperfiddle.electric-dom2 :as dom])
   (:import  (hyperfiddle.electric Pending)))


#?(:clj (defn flatten-props
          ([m] (flatten-props m []))
          ([m p]
           (if (map? m)
             (mapcat
              (fn [[k v]]
                (flatten-props v (conj p k))) m)
             [[p m]]))))


#?(:cljs (defn interop-js
           ([^js cls]                                  #?(:cljs (new cls)))
           ([^js cls a#]                               #?(:cljs (new cls a#)))
           ([^js cls a# b#]                            #?(:cljs (new cls a# b#)))
           ([^js cls a# b# c#]                         #?(:cljs (new cls  a# b# c#)))
           ([^js cls a# b# c# d#]                      #?(:cljs (new cls a# b# c# d#)))
           ([^js cls a# b# c# d# e#]                   #?(:cljs (new cls a# b# c# d# e#)))
           ([^js cls a# b# c# d# e# f#]                #?(:cljs (new cls a# b# c# d# e# f#)))
           ([^js cls a# b# c# d# e# f# g#]             #?(:cljs (new cls a# b# c# d# e# f# g#)))
           ([^js cls a# b# c# d# e# f# g# h#]          #?(:cljs (new cls a# b# c# d# e# f# g# h#)))
           ([^js cls a# b# c# d# e# f# g# h# i#]       #?(:cljs (new cls a# b# c# d# e# f# g# h# i#)))
           ([^js cls a# b# c# d# e# f# g# h# i# j#]    #?(:cljs (new cls a# b# c# d# e# f# g# h# i# j#)))
           ([^js cls a# b# c# d# e# f# g# h# i# j# k#] #?(:cljs (new cls a# b# c# d# e# f# g# h# i# j# k#)))))


(e/def three_obj)
(e/def rerender-flag)
(e/def view-port-ratio)


(defmacro mark-render! []
  `(reset! rerender-flag true))


#?(:cljs(defn -setlistener [^js obj val]
          (set! (.-listeners  obj) val)))
(defmacro bare-obj [cls  unmount-fns body]
  (let [args  (first body)
        body-args (rest body)
        s (symbol cls)]
    `(do
       (let [obj# (apply interop-js ~s ~args)]
         (-setlistener  obj# (atom {}))
         (binding [three_obj obj#]
           (e/on-unmount #(mark-render!))
           ~@body-args
           ~@unmount-fns
           obj#)))))


#?(:cljs (defn -dispose [^js obj]
           (.dispose obj)))


(defmacro disposable-obj [cls body]
  `(do
     (bare-obj ~cls
               [(e/on-unmount #(-dispose three_obj))]
               ~body)))



#?(:cljs (defn -removeFromParent [^js obj]
           (.removeFromParent obj)))

(defmacro scene-obj [cls body]
  `(let [obj# (disposable-obj ~cls ~body)]

     (e/on-unmount #(-removeFromParent  obj#))
     (.add three_obj obj#)
     (mark-render!)

     obj#))


(defmacro set-prop-fn [path]
  `(fn [val#]
     (set! (.. three_obj ~@path) val#)
     (mark-render!)))


(defmacro unmount-prop [fn]
  `(new (m/observe (fn [!#] (!# nil) ~fn))))


(defmacro props [m]
  `(do ~@(map (fn [[k v]]
                (let [path (map #(symbol (str "-" (name %))) k)]
                  `(let [org-val# (.. three_obj ~@path)]
                     ((set-prop-fn ~path) ~v)
                     (unmount-prop #((set-prop-fn ~path) org-val#))))) (sort-by first (flatten-props m)))))


(defn invoke-setter [rerender-flag obj setfn & args]
  (apply setfn obj args)
  (reset! rerender-flag true))


(defmacro setter [undo setfn & args]
  `(do
     (invoke-setter rerender-flag three_obj ~setfn ~@args)
     (e/on-unmount #(~undo three_obj))
     three_obj))


(defmacro gen_factory [mname kw macro]
  (let [full-macro (symbol (resolve macro))]
    `(do
       (defmacro ~mname [& body#]
         (list  '~full-macro ~kw body#)))
    ))


(comment
  (macroexpand '(gen_factory WebGLRenderer :three/WebGLRenderer disposable-obj)))

(gen_factory WebGLRenderer :three/WebGLRenderer disposable-obj)
(gen_factory PerspectiveCamera :three/PerspectiveCamera disposable-obj)
(gen_factory Scene :three/Scene disposable-obj)
(gen_factory Mesh :three/Mesh scene-obj)
(gen_factory Group :three/Group scene-obj)


;geometries
(gen_factory BoxGeometry :three/BoxGeometry disposable-obj)
(gen_factory CapsuleGeometry :three/CapsuleGeometry disposable-obj)
(gen_factory CircleGeometry :three/CircleGeometry disposable-obj)
(gen_factory ConeGeometry :three/ConeGeometry disposable-obj)
(gen_factory CylinderGeometry :three/CylinderGeometry disposable-obj)
(gen_factory DodecahedronGeometry :three/DodecahedronGeometry disposable-obj)
(gen_factory EdgesGeometry :three/EdgesGeometry disposable-obj)
(gen_factory ExtrudeGeometry :three/ExtrudeGeometry disposable-obj)
(gen_factory PlaneGeometry :three/PlaneGeometry disposable-obj)



;lights

(gen_factory AmbientLight :three/AmbientLight scene-obj)
(gen_factory AmbientLightProbe :three/AmbientLightProbe scene-obj)
(gen_factory DirectionalLight :three/DirectionalLight scene-obj)
(gen_factory HemisphereLight :three/HemisphereLight scene-obj)
(gen_factory HemisphereLightProbe :three/HemisphereLightProbe scene-obj)
(gen_factory PointLight :three/PointLight scene-obj)
(gen_factory RectAreaLightHelper :three/PRectAreaLightHelper scene-obj)
(gen_factory SpotLight :three/SpotLight scene-obj)


;materials
(gen_factory MeshLambertMaterial :three/MeshLambertMaterial disposable-obj)
(gen_factory MeshBasicMaterial :three/MeshBasicMaterial disposable-obj)
(gen_factory MeshStandardMaterial :three/MeshStandardMaterial disposable-obj)


#?(:cljs (defn set_camera [^js camera pos target]
           (let [{px :x py :y pz :z} pos
                 {tx :x ty :y tz :z} target]
             (set! (.. camera -position -x) px)
             (set! (.. camera -position -y) py)
             (set! (.. camera -position -z) pz)
             (.lookAt camera tx ty tz)
             (.updateProjectionMatrix camera))))

#?(:cljs (defn reset_camera [^js camera]
           (set! (.-position camera) (three/Vector3. 0 0 0))
           (.lookAt camera 0 0 0)
           (.updateProjectionMatrix camera)))

(defn -control-render [mat s]
  (reset! s true))

(e/defn cam_mat [controls]
  (if (= "visible" e/dom-visibility-state)
    (new (m/sample #(do
                      (.update controls)
                      [(vec (.. controls -object -position))  (vec (.. controls -object -quaternion))]) e/<clock))
    (throw (Pending.)))) ;


(defmacro control []
  `(when (= "visible" e/dom-visibility-state)
    (let [mat# (new cam_mat three_obj)]
      (-control-render mat# rerender-flag))))


(gen_factory OrbitControls :orbitcontrols/OrbitControls disposable-obj)

#?(:cljs (defn -render [^js renderer ^js scene ^js camera tick !rerender]
           (when @!rerender
             (print "render")
             (reset! !rerender false)
             (.updateProjectionMatrix camera)
             (.render renderer scene camera))))

(defn -size [rect] [(.-width rect) (.-height rect)])

(defn size> [node]
  #?(:cljs (->> (m/observe (fn [!]
                             (! (-> node .getBoundingClientRect))
                             (let [resize-observer (js/ResizeObserver. (fn [[nd] _] (! (-> nd .-target .getBoundingClientRect))))]
                               (.observe resize-observer node)
                               #(.disconnect resize-observer))))
                (m/relieve {}))))


#?(:cljs (defn node-resized [flag]
           (fn [^js renderer w h]
             (.setSize renderer w h)
             (reset! flag true))))


#?(:cljs (defn dom-listener [^js obj typ f]
           (swap! (.-listeners obj) update typ #(if (nil? %) #{f} (conj % f)))
           #(swap! (.-listeners obj)   update typ (fn [x] (disj x f)))))

#?(:cljs (defn listen> ; we intend to replace this in UI5 workstream
           ([^js node event-type] (listen> node event-type identity))
           ([^js node event-type keep-fn!]
            (m/relieve {}
                       (m/observe (fn [!]
                                    (dom-listener node event-type #(when-some [v (keep-fn! %)]
                                                                     (! v)))))))))

(defmacro on!
  "Call the `callback` clojure function on event.
   (on! \"click\" (fn [event] ...)) "
  ([event-name callback] `(on! three_obj ~event-name ~callback))
  ([dom-node event-name callback]
   `(new (->> (listen> ~dom-node ~event-name ~callback)
              (m/reductions {} nil)))))

(defmacro on
  "Run the given electric function on event.
  (on \"click\" (e/fn [event] ...))"
  ;; TODO add support of event options (see `event*`)
  ;(^:deprecated [typ]  `(new Event ~typ false)) ; use `on!` for local side effects
  ([typ F] `(on three_obj ~typ ~F))
  ([node typ F] `(binding [three_obj ~node]
                   (let [[state# v#] (e/for-event-pending-switch [e# (listen> ~node ~typ)] (new ~F e#))]
                     (case state#
                       (::e/init ::e/ok) v# ; could be `nil`, for backward compat we keep it
                       (::e/pending) (throw (Pending.))
                       (::e/failed)  (throw v#))))))


(defn -intersected [x y scene camera]
  #?(:cljs
     (let [pointer (three/Vector2.)
           caster (three/Raycaster.)]
       (set! (.-x pointer) x)
       (set! (.-y pointer) y)
       (.setFromCamera caster pointer camera)
       (when-let [i (first (seq (.intersectObjects caster (.-children scene) true)))]
         (js->clj i)))))

(defn -pointer [rect [x y]]
  (let [[width height] (-size rect)
        dx (- (inc x) (.-left rect))
        dy (- (inc y) (.-top rect))
        px (dec (* 2 (/ dx width)))
        py (- (dec (* 2 (/ dy height))))]
    [px py]))

(defn -call-event-stack [{obj :obj e :e data :data } typ]
  (let [listeners (deref (.. obj -listeners))]
    (dorun (map #(% {:obj obj :e e :data data}) (listeners typ)))
     (when-let [parent (.-parent obj)]
      (-call-event-stack {:obj parent :e e :data data} typ))))

(defn -on-event [e rect scene camera typ]
  (let [[px py] (-pointer rect [(.-pageX e) (.-pageY e)])
        obj (-intersected px py scene camera)]
    (when obj
      (-call-event-stack {:obj (obj "object") :e e :data obj} typ))))

(defn intersected [v x y scene camera]
  #?(:cljs
     (let [intersection  (-intersected x y scene camera)
           i-obj (get intersection "object")
           last-intersection @v
           l-obj (get last-intersection "object")]
       (do
         (vreset! v intersection)
         (if intersection
           (if (== i-obj l-obj)
             {"pointermove" intersection}
             (if last-intersection
               {"pointermove" intersection "pointerout" last-intersection "pointerenter" intersection }
               {"pointermove" intersection "pointerenter" intersection}))
           (if last-intersection
             {"pointerout" last-intersection}
             {}))))))

(defn -on-event2 []
  (let [v (volatile! nil)]
    (fn [e rect scene camera typ]
      (let [[px py] (-pointer rect [(.-pageX e) (.-pageY e)])
            obj (intersected v px py scene camera)]
        (dorun  (map (fn [[k v]]
                       (-call-event-stack {:obj (v "object") :e e :data v} k)) obj)
                )))))

#?(:cljs (e/defn init-callbacksystem [^js rect ^js scene ^js camera]
           (let [f (-on-event2)]
             (dom/on! "pointermove" #(f % rect scene camera "pointermove")))
           (dom/on! "click" #(-on-event % rect scene camera "click"))))


(e/defn Hovered? "Returns whether this DOM `node` is hovered over."
  []
  (->> (mx/mix
        (listen> three_obj "pointerenter" (constantly true))
        (listen> three_obj "pointerout" (constantly false)))
       (m/reductions {} false)
       (m/relieve {})
       new))


#?(:cljs (defn -setPixelRatio [^js obj]
           (.setPixelRatio obj (.-devicePixelRatio js/window))))

(defmacro canvas [renderer camera scene]
  `(let [!rerender# (atom true)]
     (binding [rerender-flag !rerender#]
       (let [renderer# ~renderer
             node# (.-domElement renderer#)
             rect# (new (size>  dom/node))
             [width# height#] (-size rect#)]
         (.appendChild dom/node node#)
         (binding [view-port-ratio (/ width# height#)]
             (let [camera# ~camera
                   scene# ~scene
                   tick# (e/client e/system-time-ms)]
               (new init-callbacksystem rect# scene# camera#)
               (binding [dom/node node#]
                 ((node-resized rerender-flag) renderer# width# height#)
                 (-setPixelRatio renderer#)
                 (-render renderer# scene# camera# tick# !rerender#)
                 (e/on-unmount #(do
                                  (some-> (.-parentNode node#) (.removeChild node#)))))))))))


(comment
  (macroexpand `(three_js))
  )
(comment
  (use 'clojure.walk)

  (defn a []
    (let [e (fn [] 42)]
      (e)))
  (macroexpand  `(canvas a b c))
  (macroexpand-all `(props {:a 1 :b {:c 3}}))

  (namespace :three/foo)

  (macroexpand-all  `(th2/Scene (th2/Mesh (th2/BoxGeometry)
                                          (th2/MeshBasicMaterial
                                           (th2/props {:color  {:r 0.5 :g 0.0 :b (/ state 10)}})))))

  (macroexpand-all
   `(PerspectiveCamera 75 1 0.1 1000
                       (props {:position (vec3 0 0 2)})))


  1)