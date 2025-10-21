(use jw32/_util)
(use jw32/_combaseapi)
(use jw32/_uiautomation)

(use ../src/uia)
(use ../src/util)


(def dummy-elem-proto
  @{:AddRef  (fn [self] (put self :_ref (+ (in self :_ref) 1)))
    :Release (fn [self] (put self :_ref (- (in self :_ref) 1)))
    :get_CachedControlType (fn [self] (in self :_ctype))
    :get_CachedIsOffscreen (fn [self] FALSE)
    :get_CachedNativeWindowHandle (fn [self] (in self :_hwnd))})

(defn dummy-elem [hwnd ctype &opt parent sibling]
  (table/setproto @{:_ref 0 :_hwnd hwnd :_ctype ctype :_parent parent :_sibling sibling} dummy-elem-proto))


(defn test-with-uia []
  (var release-called-with nil)
  (var body-called-with nil)
  (var ctor-returned nil)
  (def dummy-uia-ctor
    (fn [op]
      (set ctor-returned
           (case op
             :fail
             nil

             :succeed
             @{:Release (fn [x] (set release-called-with x))}

             :error
             (error "dummy error")))
      ctor-returned))

  (with-uia [dummy-uia (dummy-uia-ctor :succeed)]
    (set body-called-with dummy-uia))

  (assert (= release-called-with ctor-returned))
  (assert (= body-called-with ctor-returned))

  (set release-called-with 'place-holder)
  (set body-called-with 'place-holder)

  (with-uia [dummy-uia (dummy-uia-ctor :fail)]
    (set body-called-with dummy-uia))

  (assert (= release-called-with 'place-holder))
  (assert (nil? body-called-with))

  (set release-called-with 'place-holder)
  (set body-called-with 'place-holder)

  (try
    (with-uia [dummy-uia (dummy-uia-ctor :error)]
      (set body-called-with dummy-uia))
    ((err fib)
     (assert (= err "dummy error"))))

  (assert (= release-called-with 'place-holder))
  (assert (= body-called-with 'place-holder))

  (try
    (with-uia [dummy-uia (dummy-uia-ctor :succeed)]
      (set body-called-with dummy-uia)
      (error "dummy error from body"))
    ((err fib)
     (assert (= err "dummy error from body"))))

  (assert (= release-called-with ctor-returned))
  (assert (= body-called-with ctor-returned)))


(defn test-uia-manager-get-parent-window []
  (def hwnd1 (ffi/malloc 1))
  (def hwnd2 (ffi/malloc 1))
  (def hwnd3 (ffi/malloc 1))
  (def hwnd4 (ffi/malloc 1))
  (def hwnd5 (ffi/malloc 1))

  (def dummy-root
    (dummy-elem hwnd1 UIA_PaneControlTypeId nil))

  (def dummy-parent
    (dummy-elem hwnd2 UIA_WindowControlTypeId
      dummy-root))

  (def dummy-elem1
    (dummy-elem hwnd3 UIA_PaneControlTypeId
      dummy-parent))

  (def dummy-elem2
    (dummy-elem hwnd4 UIA_PaneControlTypeId
      dummy-elem1))

  (def dummy-elem3
    (dummy-elem hwnd5 UIA_PaneControlTypeId
      dummy-elem2))

  (def dummy-walker
    @{:Release (fn [self] 0)
      :GetParentElementBuildCache (fn [_self elem _cr]
                                    (def p (in elem :_parent))
                                    (when p
                                      (:AddRef p))
                                    p)})

  (CoInitializeEx nil COINIT_MULTITHREADED)
  (def uia-man (uia-manager))

  (put uia-man
       :get-root
       (fn [_self]
         (:AddRef dummy-root)
         dummy-root))
  (:Release (in uia-man :control-view-walker))
  (put uia-man :control-view-walker dummy-walker)

  (:AddRef dummy-elem3)
  (assert (= 1 (in dummy-elem3 :_ref)))

  (def p (:get-parent-window uia-man dummy-elem3 false))
  (assert (= p dummy-parent))
  (assert (= 1 (in p :_ref)))
  (assert (= 0 (in dummy-root :_ref)))
  (assert (= 0 (in dummy-elem1 :_ref)))
  (assert (= 0 (in dummy-elem2 :_ref)))
  (assert (= 1 (in dummy-elem3 :_ref)))

  (:destroy uia-man)
  (CoUninitialize)
  (ffi/free hwnd1)
  (ffi/free hwnd2)
  (ffi/free hwnd3)
  (ffi/free hwnd4)
  (ffi/free hwnd5))


(defn test-uia-manager-enumerate-children []
  (def dummy-sib-4
    (dummy-elem :hwnd4 nil nil nil))
  (def dummy-sib-3
    (dummy-elem :hwnd3 nil nil dummy-sib-4))
  (def dummy-sib-2
    (dummy-elem :hwnd2 nil nil dummy-sib-3))
  (def dummy-sib-1
    (dummy-elem :hwnd1 nil nil dummy-sib-2))

  (def dummy-walker
    @{:_ref 0
      :AddRef  (fn [self] (put self :_ref (+ (in self :_ref) 1)))
      :Release (fn [self] (put self :_ref (- (in self :_ref) 1)))
      :GetFirstChildElement  (fn [_self _elem]
                               (:AddRef dummy-sib-1)
                               dummy-sib-1)
      :GetNextSiblingElement (fn [_self elem]
                               (def sib (in elem :_sibling))
                               (when sib
                                 (:AddRef sib))
                               sib)})

  (CoInitializeEx nil COINIT_MULTITHREADED)
  (def uia-man (uia-manager))

  (def seen @{})

  (:enumerate-children
     uia-man
     :dummy-elem
     (fn [c]
       (put seen (in c :_hwnd) true)
       true)
     dummy-walker)

  (assert (= 0 (in dummy-walker :_ref)))
  (assert (= 0 (in dummy-sib-1 :_ref)))
  (assert (= 0 (in dummy-sib-2 :_ref)))
  (assert (= 0 (in dummy-sib-3 :_ref)))
  (assert (= 0 (in dummy-sib-4 :_ref)))

  (assert (= 4 (length seen)))
  (assert (has-key? seen :hwnd1))
  (assert (has-key? seen :hwnd2))
  (assert (has-key? seen :hwnd3))
  (assert (has-key? seen :hwnd4))

  (:destroy uia-man)
  (CoUninitialize))


(defn test-uia-manager-walk-tree []
  (def dummy-parent
    (dummy-elem :hwnd0 nil nil nil))

  (def dummy-sib-3
    (dummy-elem :hwnd3 nil dummy-parent nil))
  (def dummy-sib-2
    (dummy-elem :hwnd2 nil dummy-parent dummy-sib-3))
  (def dummy-sib-1
    (dummy-elem :hwnd1 nil dummy-parent dummy-sib-2))

  (def dummy-sib-2-1
    (dummy-elem :hwnd2-1 nil dummy-sib-2 nil))

  (put dummy-sib-2 :_first-child dummy-sib-2-1)
  (put dummy-parent :_first-child dummy-sib-1)

  (def dummy-walker
    @{:_ref 0
      :AddRef  (fn [self] (put self :_ref (+ (in self :_ref) 1)))
      :Release (fn [self] (put self :_ref (- (in self :_ref) 1)))
      :GetFirstChildElement  (fn [_self elem]
                               (when-let [fc (in elem :_first-child)]
                                 (:AddRef fc)
                                 fc))
      :GetNextSiblingElement (fn [_self elem]
                               (def sib (in elem :_sibling))
                               (when sib
                                 (:AddRef sib))
                               sib)})

  (CoInitializeEx nil COINIT_MULTITHREADED)
  (def uia-man (uia-manager))

  (def seen @[])

  (:walk-tree
     uia-man
     dummy-parent
     (fn [e lv walk-children]
       (array/push seen [(in e :_hwnd) lv])
       (walk-children)
       true)
     nil
     dummy-walker)

  (assert (= 0 (in dummy-walker :_ref)))
  (assert (= 0 (in dummy-parent :_ref)))
  (assert (= 0 (in dummy-sib-1 :_ref)))
  (assert (= 0 (in dummy-sib-2 :_ref)))
  (assert (= 0 (in dummy-sib-2-1 :_ref)))
  (assert (= 0 (in dummy-sib-3 :_ref)))

  (assert (deep= seen @[[:hwnd0 0] [:hwnd1 1] [:hwnd2 1] [:hwnd2-1 2] [:hwnd3 1]]))

  (:destroy uia-man)
  (CoUninitialize))


(defn main [&]
  (test-with-uia)
  (test-uia-manager-get-parent-window)
  (test-uia-manager-enumerate-children)
  (test-uia-manager-walk-tree))
