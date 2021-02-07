(if #t (let ([x Int 7] [y Lambda (lambda ([z Int]) (primapp * 6 z))]) (y x)) (primapp - 5 3))
