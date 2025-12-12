(use jw32/_combaseapi)
(use ../src/vd)


(def dummy-guids
  ["{4BD7A195-CA18-4FE6-963E-E64EF5E91A82}"
   "{4D2376AD-1F5D-4F13-82D8-11E295C36B25}"
   "{CCDFE45E-A948-460B-A150-997EEF14575F}"
   "{C26FF0F7-040D-4586-A51F-6C6685785351}"])


(defn make-dummy-vdm [vd-list]
  (def vdm (virtual-desktop-manager))
  (put vdm :get-all-desktops (fn [&] vd-list)))


(defn test-vdm-get-desktop-guid-from-name []
  # All desktops using default names
  (var vdm
       (make-dummy-vdm
        [[1 (dummy-guids 0) nil]
         [2 (dummy-guids 1) nil]
         [3 (dummy-guids 2) nil]
         [4 (dummy-guids 3) nil]]))

  (defer (:destroy vdm)
    (assert (= (:get-desktop-guid-from-name vdm [:default 1]) (dummy-guids 0)))
    (assert (= (:get-desktop-guid-from-name vdm [:default 2]) (dummy-guids 1)))
    (assert (nil? (:get-desktop-guid-from-name vdm [:default 5])))
    (assert (= (:get-desktop-guid-from-name vdm "Desktop 1") (dummy-guids 0)))
    (assert (= (:get-desktop-guid-from-name vdm "Desktop 2") (dummy-guids 1)))
    (assert (= (:get-desktop-guid-from-name vdm "デスクトプ 1") (dummy-guids 0)))
    (assert (= (:get-desktop-guid-from-name vdm "デスクトプ 2") (dummy-guids 1)))
    (assert (nil? (:get-desktop-guid-from-name vdm "Desktop 5")))
    (assert (nil? (:get-desktop-guid-from-name vdm "Some Name"))))

  # All desktops using user-defined names
  (set vdm
       (make-dummy-vdm
        [[1 (dummy-guids 0) "Name One"]
         [2 (dummy-guids 1) "Name Two"]
         [3 (dummy-guids 2) "Name Three"]
         [4 (dummy-guids 3) "Name Four"]]))

  (defer (:destroy vdm)
    (assert (= (:get-desktop-guid-from-name vdm "Name One") (dummy-guids 0)))
    (assert (= (:get-desktop-guid-from-name vdm "Name Two") (dummy-guids 1)))
    (assert (nil? (:get-desktop-guid-from-name vdm "Name Five")))
    (assert (nil? (:get-desktop-guid-from-name vdm [:default 1])))
    (assert (nil? (:get-desktop-guid-from-name vdm "Desktop 1")))
    (assert (nil? (:get-desktop-guid-from-name vdm "Name 1"))))

  # Mixing user-defined and default names
  (set vdm
       (make-dummy-vdm
        [[1 (dummy-guids 0) "Name One"]
         [2 (dummy-guids 1) nil]
         [3 (dummy-guids 2) "Name Three"]
         [4 (dummy-guids 3) nil]]))

  (defer (:destroy vdm)
    (assert (= (:get-desktop-guid-from-name vdm "Name One") (dummy-guids 0)))
    (assert (nil? (:get-desktop-guid-from-name vdm "Name Two")))
    (assert (= (:get-desktop-guid-from-name vdm "Name Three") (dummy-guids 2)))
    (assert (nil? (:get-desktop-guid-from-name vdm "Name Four")))

    (assert (nil? (:get-desktop-guid-from-name vdm [:default 1])))
    (assert (= (:get-desktop-guid-from-name vdm [:default 2]) (dummy-guids 1)))
    (assert (nil? (:get-desktop-guid-from-name vdm [:default 3])))
    (assert (= (:get-desktop-guid-from-name vdm [:default 4]) (dummy-guids 3)))

    (assert (nil? (:get-desktop-guid-from-name vdm "Desktop 1")))
    (assert (= (:get-desktop-guid-from-name vdm "Desktop 2") (dummy-guids 1)))
    (assert (nil? (:get-desktop-guid-from-name vdm "Desktop 3")))
    (assert (= (:get-desktop-guid-from-name vdm "Desktop 4") (dummy-guids 3)))

    (assert (nil? (:get-desktop-guid-from-name vdm "デスクトプ 1")))
    (assert (= (:get-desktop-guid-from-name vdm "デスクトプ 2") (dummy-guids 1)))
    (assert (nil? (:get-desktop-guid-from-name vdm "デスクトプ 3")))
    (assert (= (:get-desktop-guid-from-name vdm "デスクトプ 4") (dummy-guids 3)))

    (assert (nil? (:get-desktop-guid-from-name vdm "Name Five")))
    (assert (nil? (:get-desktop-guid-from-name vdm [:default 5])))
    (assert (nil? (:get-desktop-guid-from-name vdm "Desktop 5")))
    (assert (nil? (:get-desktop-guid-from-name vdm "デスクトプ 5"))))

  # Virtual desktops disabled
  (set vdm (make-dummy-vdm []))

  (defer (:destroy vdm)
    (assert (empty? (:get-all-desktops vdm)))
    (assert (= :default (:get-desktop-name vdm (dummy-guids 0))))
    (assert (= :default (:get-desktop-name vdm (dummy-guids 1))))
    (assert (= :default (:get-desktop-name vdm :default)))
    (assert (= :default (:get-desktop-guid-from-name vdm "Desktop 1")))
    (assert (= :default (:get-desktop-guid-from-name vdm "Desktop 2")))
    (assert (= :default (:get-desktop-guid-from-name vdm "デスクトプ 1")))
    (assert (= :default (:get-desktop-guid-from-name vdm "Dummy Name")))))


(defn main [&]
  (CoInitializeEx nil COINIT_MULTITHREADED)

  (test-vdm-get-desktop-guid-from-name)

  (CoUninitialize))
