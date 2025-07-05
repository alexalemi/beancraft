#!/usr/bin/env janet

(use ./beancraft/parse)

(pp (parse `use "div" Dividend=20 Divisor=4`))