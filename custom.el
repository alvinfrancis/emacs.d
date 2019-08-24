(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :weight normal :height 110 :width normal :family "Monoid"))))
 '(elscreen-tab-background-face ((t (:inherit default))))
 '(elscreen-tab-current-screen-face ((t (:height 0.8))))
 '(elscreen-tab-other-screen-face ((t (:inherit linum))))
 '(fringe ((t (:inherit default :background nil))))
 '(linum ((t (:inherit default :box nil :height 0.8 :background nil))))
 '(magit-item-highlight ((t nil)))
 '(mode-line-inactive ((t (:inherit default :background nil))))
 '(org-block-background ((t (:background "#070707"))))
 '(org-block-begin-line ((t (:background "#002D43"))))
 '(org-block-end-line ((t (:background "#002D43"))))
 '(powerline-active1 ((t (:inherit default :background nil))))
 '(powerline-active2 ((t (:inherit default :background nil))))
 '(powerline-inactive1 ((t (:inherit default :background nil))))
 '(powerline-inactive2 ((t (:inherit default :background nil)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#F0F4FC" "#99324B" "#4F894C" "#9A7500" "#3B6EA8" "#97365B" "#398EAC" "#485163"])
 '(compilation-message-face (quote default))
 '(css-indent-offset 2)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "9d9fda57c476672acd8c6efeb9dc801abea906634575ad2c7688d055878e69d6" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "8e4efc4bed89c4e67167fdabff77102abeb0b1c203953de1e6ab4d2e3a02939a" "2af26301bded15f5f9111d3a161b6bfb3f4b93ec34ffa95e42815396da9cb560" "a7e7804313dbf827a441c86a8109ef5b64b03011383322cbdbf646eb02692f76" "10e3d04d524c42b71496e6c2e770c8e18b153fcfcc838947094dad8e5aa02cef" "2e973a84accb627f603f309fc13938daa227055d2a6528c80a9b7a9e2dd8da6b" "4182c491b5cc235ba5f27d3c1804fc9f11f51bf56fb6d961f94788be034179ad" "f67652440b66223b66a4d3e9c0ddeddbf4a6560182fa38693bdc4d940ce43a2e" "44c566df0e1dfddc60621711155b1be4665dd3520b290cb354f8270ca57f8788" "0eef522d30756a80b28333f05c7eed5721f2ba9b3eaaff244ea4c6f6a1b8ac62" "3eb2b5607b41ad8a6da75fe04d5f92a46d1b9a95a202e3f5369e2cdefb7aac5c" "8d3c5e9ba9dcd05020ccebb3cc615e40e7623b267b69314bdb70fe473dd9c7a8" "2d16a5d1921feb826a6a9b344837c1ab3910f9636022fa6dc1577948694b7d84" "0f0022c8091326c9894b707df2ae58dd51527b0cf7abcb0a310fb1e7bda78cd2" "8d737627879eff1bbc7e3ef1e9adc657207d9bf74f9abb6e0e53a6541c5f2e88" "668f0b70e9e371a625c37c83b017799a6a58c3fa535022e086c289647a4473a9" "5310b88333fc64c0cb34a27f42fa55ce371438a55f02ac7a4b93519d148bd03d" "16fd69242d5383a431bc49ed3b567dbce148a4991242baa11ee6367ca93705e2" "d29231b2550e0d30b7d0d7fc54a7fb2aa7f47d1b110ee625c1a56b30fea3be0f" "666c783d1534051189c9bca391037fc5a11dbc5d51dbe80e8148d66bfa4e9fdb" "6bde11b304427c7821b72a06a60e8d079b8f7ae10b407d8af37ed5e5d59b1324" "3a651bfd6708cd2995c6f9b50146e890e06419d445980a7cdc095af245899aa7" "f63adec7dee4f849b29fea17b7abc9d9b70cae91fe70ac510ca9408b5bdb7ab5" "227e2c160b0df776257e1411de60a9a181f890cfdf9c1f45535fc83c9b34406b" "34c6da8c18dcbe10d34e3cf0ceab80ed016552cb40cee1b906a42fd53342aba3" "ead76c417365064889c6552678e62a3982f9c6b359888dd7b2ba62efb9422b96" "a4bd55761752bddac75bad0a78f8c52081a1effb33b69354e30a64869b5a40b9" "fb3e623e6c6e98f45aea182e56808a11d4c255490e49387a508bfc42251e15d0" "ad1c2abad40e11d22156fe3987fd9b74b9e1c822264a07dacb24e0b3133aaed1" "9d91458c4ad7c74cf946bd97ad085c0f6a40c370ac0a1cbeb2e3879f15b40553" "0f0db69b7a75a7466ef2c093e127a3fe3213ce79b87c95d39ed1eccd6fe69f74" "945fe66fbc30a7cbe0ed3e970195a7ee79ee34f49a86bc96d02662ab449b8134" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" "10e231624707d46f7b2059cc9280c332f7c7a530ebc17dba7e506df34c5332c4" "ef479623c75026d8ba1de98a8cb74198f6f3eedc6fca509990ac2559ba830675" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "6254372d3ffe543979f21c4a4179cd819b808e5dd0f1787e2a2a647f5759c1d1" "afbb40954f67924d3153f27b6d3399df221b2050f2a72eb2cfa8d29ca783c5a8" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "4eb982b248bf818a72877ecb126a2f95d71eea24680022789b14c3dec7629c1b" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" "c006bc787154c31d5c75e93a54657b4421e0b1a62516644bd25d954239bc9933" "ad24ea739f229477ea348af968634cb7a0748c9015110a777c8effeddfa920f5" "ffe39e540469ef05808ab4b75055cc81266875fa4a0d9e89c2fec1da7a6354f3" "33bb2c9b6e965f9c3366c57f8d08a94152954d4e2124dc621953f5a8d7e9ca41" "12722541c8998f056b761bf63a92216aaf4610e4eb1afe7991842a31fa28b6d8" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "764e3a6472a3a4821d929cdbd786e759fab6ef6c2081884fca45f1e1e3077d1d" "fc58296b4831a327aa1b11939b32755a7bd4a0679665809968132ef38d2dd4d8" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "acb039d6f2c41b3bd852b448351b2979f44ef488026c95dd5228d2f6da57f574" "36b22a0d7e56042cda5c6a3aa589adfcde0660f3a048feaa54c2797cfb090510" "e4bc8563d7651b2fed20402fe37b7ab7cb72869f92a3e705907aaecc706117b5" "ddfb81ce0b4bbd47433866401713fcca73db8eb0d50bd323bad0f052155c995c" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "5ea20171762b3f9682fbf507ee4b4018ce7b6cc65415fa99799a125f112b2cdb" "30a8a5a9099e000f5d4dbfb2d6706e0a94d56620320ce1071eede5481f77d312" "8d6fb24169d94df45422617a1dfabf15ca42a97d594d28b3584dc6db711e0e0b" "49eea2857afb24808915643b1b5bd093eefb35424c758f502e98a03d0d3df4b1" "08efabe5a8f3827508634a3ceed33fa06b9daeef9c70a24218b70494acdf7855" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "7bde52fdac7ac54d00f3d4c559f2f7aa899311655e7eb20ec5491f3b5c533fe8" "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" "1a85b8ade3d7cf76897b338ff3b20409cb5a5fbed4e45c6f38c98eee7b025ad4" "0e7da2c7c64fb5d4764250ffa4b8b33c0946577108d1d6444f1020d0dabba784" "aea04a6cf90e3ad60e0aeb199d3e2ad06ab20d71150d5870dcb7117b6c554b0a" "a5cd126cd6dcf911a3f57c7e5d8ec652763cadcdcd317150295483454167227e" "a0ffdee5e0d310b62da5223f76e70d7b2ad122943e0bb90ec604e7394c1ec77e" "d1c2ea65f9163ea61908deba09f321640ed8733346bb1cb388eabf0310ccaf89" "fa942713c74b5ad27893e72ed8dccf791c9d39e5e7336e52d76e7125bfa51d4c" "57f8801351e8b7677923c9fe547f7e19f38c99b80d68c34da6fa9b94dc6d3297" "bd115791a5ac6058164193164fd1245ac9dc97207783eae036f0bfc9ad9670e0" "b6f7795c2fbf75baf3419c60ef7625154c046fc2b10e3fdd188e5757e08ac0ec" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "77d704908bf206929237af1089f020781963dcc671489005fb8153c1745f689a" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "53e29ea3d0251198924328fd943d6ead860e9f47af8d22f0b764d11168455a8e" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "aa392ee7deac22c0c0b71b396d4969ba0849b538bb1790ef31b115b4e620c0b5" "9c26d896b2668f212f39f5b0206c5e3f0ac301611ced8a6f74afe4ee9c7e6311" "36d0f600074e9299fb7b6a316161d99faa16a6551ddeda50980ae293e653e7b4" default)))
 '(fci-rule-color "#AEBACF")
 '(fringe-mode nil nil (fringe))
 '(highlight-changes-colors ("#FD5FF0" "#AE81FF"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (("#49483E" . 0)
    ("#67930F" . 20)
    ("#349B8D" . 30)
    ("#21889B" . 50)
    ("#968B26" . 60)
    ("#A45E0A" . 70)
    ("#A41F99" . 85)
    ("#49483E" . 100)))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(jdee-db-active-breakpoint-face-colors (cons "#0d0f11" "#41728e"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#0d0f11" "#b5bd68"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#0d0f11" "#5a5b5a"))
 '(js-indent-level 2)
 '(js2-strict-trailing-comma-warning nil)
 '(linum-format " %d ")
 '(linum-relative-current-symbol "")
 '(magit-diff-use-overlays nil)
 '(nrepl-force-ssh-for-remote-hosts nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-ellipsis "  ")
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t t)
 '(org-fontify-whole-heading-line t)
 '(org-plantuml-jar-path "/Users/alvin/bin/plantuml.jar")
 '(package-selected-packages
   (quote
    (vterm-toggle vterm theme-magic ns-auto-titlebar ensime mmm-mode edit-indirect jenkins graphql-mode 2048-game zone-tunnels multifiles org-presie rjsx-mode markdown-preview-eww markdown-preview-mode epresent org-present git-blamed smart-tabs-mode httpcode midje-mode csv-mode groovy-mode zone-nyan nyan-mode log4j-mode diffview markdown-mode plantuml-mode github-modern-theme say-what-im-doing tabbar format-sql sqlplus threads-modeline eldoc-eval openwith spaceline solarized-theme zenburn-theme zoutline yaml-mode web-mode vimrc-mode vdiff use-package trident-mode ssh spark spacegray-theme seti-theme scala-mode restclient request ranger rainbow-mode rainbow-delimiters powerline paredit org-page org-journal ob-ipython nlinum-relative neotree material-theme magit lively linum-relative latex-preview-pane key-chord jsx-mode ivy inf-clojure highlight helm-swoop helm-projectile helm-ls-git helm-descbinds helm-ag gruvbox-theme google-maps go-eldoc github-theme git-timemachine git-gutter-fringe git-gutter+ flycheck-package flatui-theme flatland-theme exec-path-from-shell evil-visualstar evil-surround evil-multiedit evil-indent-textobject evil-easymotion evil-anzu esup epoch-view elscreen elfeed edn doom-themes csharp-mode company-go cl-format centered-cursor-mode buttercup birds-of-paradise-plus-theme auctex atom-one-dark-theme alert adaptive-wrap ace-jump-mode)))
 '(pdf-view-midnight-colors (quote ("#6a737d" . "#fffbdd")))
 '(plantuml-jar-path "/Users/alvin/bin/plantuml.jar")
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(powerline-color1 "#3d3d68")
 '(powerline-color2 "#292945")
 '(powerline-height 10)
 '(safe-local-variable-values
   (quote
    ((cider-clojure-cli-parameters . "-A:dev -e '(require (quote cider-nrepl.main)) (cider-nrepl.main/init %s)'")
     (cider-clojure-cli-parameters . "-A:test:dev -e '(require (quote cider-nrepl.main)) (cider-nrepl.main/init %s)'")
     (eval progn
           (put-clojure-indent
            (quote tgen/let)
            1)
           (add-to-list
            (quote clojure-align-binding-forms)
            "tgen/let"))
     (eval progn
           (put-clojure-indent
            (quote fact)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote facts)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote future-fact)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote future-facts)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote provided)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote checker)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote defcache)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote every-checker)
            (quote
             (nil))))
     (eval progn
           (put-clojure-indent
            (quote fact)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote facts)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote future-fact)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote future-facts)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote provided)
            (quote
             (nil)))
           (put-clojure-indent
            (quote checker)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote defcache)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote every-checker)
            (quote
             (nil))))
     (eval progn
           (setq-local cider-boot-parameters "dev"))
     (eval progn
           (put-clojure-indent
            (quote fact)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote facts)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote future-fact)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote future-facts)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote provided)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote checker)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote defcache)
            (quote
             (:defn
              (1)))))
     (eval progn
           (put-clojure-indent
            (quote fact)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote facts)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote future-fact)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote future-facts)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote provided)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote checker)
            (quote
             (:defn
              (1)))))
     (eval progn
           (put-clojure-indent
            (quote fact)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote facts)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote future-fact)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote future-facts)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote provided)
            (quote
             (:defn
              (1)))))
     (eval progn
           (c-set-offset
            (quote case-label)
            (quote +))
           (push
            (quote
             (c-mode . "k&r"))
            c-default-style)
           (push
            (quote
             (c++-mode . "k&r"))
            c-default-style))
     (put-clojure-indent
      (quote provided)
      (quote
       (:defn
        (1))))
     (put-clojure-indent
      (quote dwm-resource)
      (quote
       (:defn
        (1))))
     (org-confirm-babel-evaluate)
     (bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")
     (checkdoc-package-keywords-flag)
     (eval progn
           (put-clojure-indent
            (quote mdl/textfield)
            (quote
             (:defn
              (1))))
           (put-clojure-indent
            (quote mdl/layout)
            (quote
             (:defn
              (1))))))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(syslog-debug-face
   (quote
    ((t :background unspecified :foreground "#A1EFE4" :weight bold))))
 '(syslog-error-face
   (quote
    ((t :background unspecified :foreground "#F92672" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#A6E22E"))))
 '(syslog-info-face
   (quote
    ((t :background unspecified :foreground "#66D9EF" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#E6DB74"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#FD5FF0"))))
 '(syslog-warn-face
   (quote
    ((t :background unspecified :foreground "#FD971F" :weight bold))))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background "#E5E9F0")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (list
    (cons 20 "#4F894C")
    (cons 40 "#688232")
    (cons 60 "#817b19")
    (cons 80 "#9A7500")
    (cons 100 "#a0640c")
    (cons 120 "#a65419")
    (cons 140 "#AC4426")
    (cons 160 "#a53f37")
    (cons 180 "#9e3a49")
    (cons 200 "#97365B")
    (cons 220 "#973455")
    (cons 240 "#983350")
    (cons 260 "#99324B")
    (cons 280 "#a0566f")
    (cons 300 "#a87b93")
    (cons 320 "#b0a0b6")
    (cons 340 "#AEBACF")
    (cons 360 "#AEBACF")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
