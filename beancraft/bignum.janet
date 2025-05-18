# I need a simple bignum implementation to use for bins and beans.

# We are going to represent numbers essentially in base 256 as byte arrays.
# We'll put the least significant bit first.

(defn bignum/new []
  (buffer/new-filled 1))

(defn zero [] (bignum/new))


(defn bignum/inc
  "Increment the bignum."
  [x]
  (defn inc-at [loc]
    (let [val (get x loc 0)]
      # If we see 255, we have to increment the next position
      (if (= val 255)
        (do
          (set (x loc) 0)
          (inc-at (inc loc)))
        (set (x loc) (inc val)))))
  (inc-at 0)
  x)

(defn bignum/dec
  "Decrement the bignum."
  [x]
  (def n (length x))
  (defn dec-at [loc]
    (let [val (get x loc)]
      # If we see a zero, we have to wrap around.
      (case val
        0 (if (< loc (dec n))
            (do
              (set (x loc) 255)
              (dec-at (inc loc)))
            x)
        1 (do
            (set (x loc) 0)
            # if there is another slot
            (if (< loc (dec n))
              # go and decrement that
              (dec-at (inc loc))
              # otherwise trim the buffer
              (when (> n 1)
                (buffer/popn x 1))))
        # otherwise, decrement and return
        (do
          (set (x loc) (dec val))
          x))))
  (dec-at 0))

(defn bignum/trim [x]
  (var trailing-zeros 0)
  (def n (length x))
  (for i 0 (length x)
    (if (> (get x i) 0)
       (set trailing-zeros 0)
       (++ trailing-zeros)))
  (buffer/popn x (min (dec n) trailing-zeros)))

(defn bignum/from-num [num]
  (let [x (buffer)]
    (buffer/push-uint64 x :le num)
    (bignum/trim x)
    x))

(defn bignum/= [x y]
  (deep= x y))

(defn bignum/digits [x]
  (seq [i :range [0 (length x)]]
    (get x i)))

(defn bignum/-add2
  "Add two bignums together"
  [x y]
  (def n (max (length x) (length y)))
  (def z (buffer/new-filled n 0))
  (var carry 0)
  (var tmp 0)
  (for i 0 n
    (set tmp (+ (get x i 0) (get y i 0) carry))
    (set (z i) (% tmp 256))
    (set carry (div tmp 256)))
  (when (> carry 0)
    (buffer/push-byte z carry))
  z)
     
(defn bignum/add
  "Add bignums together"
  [& xs]
  (reduce2 bignum/-add2 xs))

(defn bignum/-sub2
  [x y]
  (def n (max (length x) (length y)))
  (def z (buffer/new-filled n 0))
  (var carry 0)
  (var tmp 0)
  (for i 0 n
    (set tmp (- (get x i 0) (get y i 0) carry))
    (set (z i) (mod tmp 256))
    (set carry (if (< tmp 0) 1 0)))
  (case carry
    0 (bignum/trim z)
    1 (zero)))

(defn bignum/sub
  "Subtract bignums"
  [& xs]
  (reduce2 bignum/-sub2 xs))

(comment
  (bignum/digits (bignum/trim (zero))))

