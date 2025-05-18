(use ../beancraft/bignum)

(assert 
  (bignum/= (zero) (bignum/dec (zero))))

(assert
  (let [x (bignum/inc (zero))
        y (bignum/dec (bignum/inc (bignum/inc (zero))))]
    (bignum/= x y)))

(assert
  (let [x (zero)]
    (for i 0 10
      (bignum/inc x))
    (bignum/= x (bignum/from-num 10))))

(assert
  (let [x (zero)]
    (for i 0 8675
      (bignum/inc x))
    (bignum/= x (bignum/from-num 8675))))

(assert
  (let [x (zero)]
    (for i 0 8675
      (bignum/inc x))
    (for i 0 8675
      (bignum/dec x))
    (bignum/= x (zero))))

(assert
  (let [xn 1231
        yn 9452
        zn (+ xn yn)
        x (bignum/from-num xn)
        y (bignum/from-num yn)
        z (bignum/add x y)]
    (bignum/= z (bignum/from-num zn))))


(assert
  (let [xn 9452
        yn 1231
        zn (- xn yn)
        x (bignum/from-num xn)
        y (bignum/from-num yn)
        z (bignum/sub x y)]
    (bignum/= z (bignum/from-num zn))))


(assert
  (let [xn 1231
        yn 9452
        x (bignum/from-num xn)
        y (bignum/from-num yn)
        z (bignum/sub x y)]
    (bignum/= z (zero))))


