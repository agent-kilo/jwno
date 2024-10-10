# Helper for generating table of contents for site.

(defn relative-url
  "Express a url relative to (dyn :url). The given url should
  be absolute from the site root, like /index.html."
  [url]
  (def b-parts (string/split "/" (dyn :url)))
  (array/pop b-parts)
  (def a-parts (string/split "/" url))
  (while (and (not= 0 (length a-parts))
              (not= 0 (length b-parts))
              (= (a-parts 0) (b-parts 0)))
    (array/remove a-parts 0)
    (array/remove b-parts 0))
  (string (string/repeat "../" (length b-parts)) (string/join a-parts "/")))


(defn render-toc
  "Render the table of contents for a page."
  [node]
  (def url (if (node :pages) (string (node :url) "/index.html") (node :url)))
  {:tag "li"
   "class" (if (node :pages) "caret")
   :content [{:tag "span" 
              "class" (if (= (dyn :url) url) "selected")
              :content {:tag "a" "href" (relative-url url) :content (or (node :nav-title) (node :title))}}
             (if-let [pages (node :pages)]
               {:tag "ul"
                :content (map render-toc pages)})]})


(var total-order nil)


(defn visit
  [node]
  (if-let [ps (node :pages)]
    (do 
      (if (node :index) (array/push total-order (node :index)))
      (each page ps (visit page)))
    (array/push total-order node)))


(defn get-total-order
  "Lazily get total order. This must be lazy because the sitemap is not
  built until after all templates have been parsed."
  []
  (unless total-order
    (set total-order @[])
    (visit (dyn :sitemap)))
  total-order)


(defn findrel
  "Find page offset from url in the total order"
  [url offset]
  (def order (get-total-order))
  (def len (length order))
  (def my-index (find-index (fn [x] (= url (x :url))) order))
  (def index (+ my-index offset))
  (when (and (>= index 0)
             (< index len))
    (in order index)))
