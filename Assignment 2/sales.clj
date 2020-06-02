; Sales Order Application

;;; Variable declaration

; Cust HashMaps
(def custNameMap (sorted-map))
(def custMap (sorted-map))
; Prod HashMaps
(def prodNameMap (sorted-map))
(def prodMap (sorted-map))
; Sales HashMaps
(def saleMap (sorted-map))

;;; Util. functions declaration

; load fine and split with new line
(defn loadFile [file] 
   (clojure.string/split-lines (slurp file))
)

; split line with '|' delimeter
(defn splitDataFromLine [line]
   (clojure.string/split line #"\|")
)

; Convert String to Float
(defn stringToFloat [str]
   (let [n (read-string str)]
      (if (number? n) (float n) nil)
   )
)

; Calculate net price
(defn cartCalculator [cart]
   (if (not (empty? cart))
      (do
         (def mapValue (val (first cart)))
         (def pid (:pid mapValue))
         (def icount (:icount mapValue))
         (def pprice (:price ((keyword pid) prodMap)))
         (def netPrice (* (stringToFloat icount) (stringToFloat pprice)))
         (float (+ netPrice (cartCalculator (rest cart))))
      )
   ; return 0 if the cart is empty
   0)
)

; Calculate sales for product
(defn productCalculator [cart]
   (if (not (empty? cart))
      (do
         (def mapValue (val (first cart)))
         (def icount (:icount mapValue))
         (+ (Integer/parseInt icount) (productCalculator (rest cart)))
      )
   ; return 0 if the cart is empty
   0)
)

;;; Loading files into hasmaps

; crate hasmap from cust.txt
(defn readCust [collection]
  (if (not (empty? collection))
      (do
         (def user (splitDataFromLine  (first collection)))
         (def uid  (get user 0))
         (def uname (get user 1))
         (def uaddress (get user 2))
         (def uphone (get user 3))
         ; add into Cust Hash map
         (def custNameMap(merge custNameMap { (keyword uid) uname}))
         ; add into Cust Hash map
         (def custMap(merge custMap { (keyword uid) {:name uname, :address uaddress, :phone uphone}}))
         (readCust (rest collection))
      )
   )
)

; crate hasmap from prod.txt
(defn readProd [collection]
  (if (not (empty? collection))
      (do
         (def product (splitDataFromLine  (first collection)))
         (def pid  (get product 0))
         (def pname (get product 1))
         (def pprice (get product 2))

         ; add into Prod Hash map
         (def prodNameMap(merge prodNameMap { (keyword pid) pname}))
         ; add into Prod Hash map
         (def prodMap(merge prodMap { (keyword pid) {:name pname, :price pprice}}))
         (readProd (rest collection))
      )
   )
)

; crate hasmap from sales.txt
(defn readSale [collection]
  (if (not (empty? collection))
      (do
         ;<salesID, custID, prodID, itemCount>
         (def sale (splitDataFromLine (first collection)))
         (def sid (get sale 0))
         (def cid (get sale 1))
         (def pid (get sale 2))
         (def icount (get sale 3))

         ; add into Sale Hash map
         (def saleMap(merge saleMap { (keyword sid) {:cid cid, :pid pid, :icount icount}}))
         (readSale (rest collection))
      )
   )
)

;;; Pretty print functions for hashmaps

; print Cust
(defn printCust [collection]
  (if (not (empty? collection))
      ; then
      (do
         (def uid  (name (key (first collection))))
         (def user (val (first collection)))
         ; 1: ["John Smith" "123 Here Street" "456-4567"]
         (println (clojure.string/trim (str uid \: [(get user :name) (get user :address)(get user :phone)])))
         (printCust (rest collection))
      )
      ; else
      (print "\n")
   )
)

; print Prod
(defn printProd [collection]
  (if (not (empty? collection))
      ; then
      (do
         (def pid  (name (key (first collection))))
         (def prod (val (first collection)))
         ; 1: ["John Smith" "123 Here Street" "456-4567"]
         (println (clojure.string/trim (str pid \: [(get prod :name) (get prod :price)])))
         (printProd (rest collection))
      )
      ; else
      (print "\n")
   )
)

; print Sale
(defn printSale [collection]
  (if (not (empty? collection))
      ; then
      (do
         (def sid  (name (key (first collection))))
         (def sale (val (first collection)))
         ; 1: ["John Smith" "shoes" "3"]
         (println (clojure.string/trim (str sid \: [(get custNameMap (keyword (get sale :cid))) (get prodNameMap (keyword (get sale :pid))) (get sale :icount)])))
         (printSale (rest collection))
      )
      ; else
      (print "\n")
   )
)

;; Addtional function

; Calculate sales for customer
(defn calculateSales []
   (do
      (println "Enter customer name: ")
      (def customerName (read-line))
      (def user (vec (filter #(= customerName (str (second %))) custNameMap)))
      (if (= (count user) 0)
         (println "\nUser doesn't exist\n")
         (do
            (def uid (name (get (get user 0) 0)))
            ; <salesID, custID, prodID, itemCount>
            (def shoppingCart (vec (filter  #(= (:cid (second %)) uid) saleMap)))
            (println "\n"customerName": $" (cartCalculator shoppingCart)"\n")
         )
      )
   )
)

; Calculate sales for product
(defn calculateProductSales []
   (do
      (println "Enter product name: ")
      (def productName (read-line))
      (def product (vec (filter #(= productName (str (second %))) prodNameMap)))
      (if (= (count product) 0)
         (println "\nProduct doesn't exist\n")
         (do
            (def pid (name (get (get product 0) 0)))
            ; <salesID, custID, prodID, itemCount>
            (def productCounter (vec (filter  #(= (:pid (second %)) pid) saleMap)))
            (println "\n"productName": " (productCalculator productCounter)"\n")
         )
      )
   )
)


;;; loading files

(readCust (loadFile "cust.txt"))
(readProd (loadFile "prod.txt"))
(readSale (loadFile "sales.txt"))

; 1. Display Customer Table
(defn displayCustomerTable []
   (printCust custMap)
)

; 2. Display Product Table
(defn displayProductTable []
   (printProd prodMap)
)

; 3. Display Sales Table
(defn displaySalesTable []
   (printSale saleMap)
)

; 4. Total Sales For Customer
(defn totalSalesForCustomer []
   (calculateSales)

)

; 5. Total Count For Product
(defn totalCountForProduct []
   (calculateProductSales)
)

; 6. Exit
(defn exit []
   (println "\nGood Bye\n")
   (System/exit 0)
)

;; Displays Menu
(defn Menu []
   (println "*** Sales Menu ***")
   (println "------------------")
   (println "1. Display Customer Table")
   (println "2. Display Product Table")
   (println "3. Display Sales Table")
   (println "4. Total Sales for Customer")
   (println "5. Total Count for Product")
   (println "6. Exit")
   (println "Enter an option?")
   
   (try
      (def option (Integer/parseInt (read-line)))
   (catch NumberFormatException e (def option 0)))
   (print "\n")
   (cond
      (= option 1) (do
         (displayCustomerTable)
         (Menu)
      )
      (= option 2) (do
         (displayProductTable)
         (Menu)
      )
      (= option 3) (do
         (displaySalesTable)
         (Menu)
      )
      (= option 4) (do
         (totalSalesForCustomer)
         (Menu)
      )
      (= option 5) (do
         (totalCountForProduct)
         (Menu)
      )
      (= option 6) (exit)
      :else (do
         (println "\nInvalid Input\n")
         (Menu)
      )
   )
)

(Menu)